#include <stdio.h>
#include <string.h>
#include <getopt.h>
#include <rpc/rpc.h>
#include <sys/socket.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <time.h>
#include "nfs-common.h"

void usage(char *prg) {
  fprintf(stderr, "Usage: %s [ -q ] [ -v nfsvers ] [ -t test_duration ] [ -u uid ] [ -g gid ] [ -b blocksize ] [ -p udp|tcp ] [ -i label ] host:/export/path/to/file_to_read\n", prg);
  exit(1);
}

int main(int argc, char **argv) {
  struct file_handle *rootfh, *fh;
  CLIENT *clnt;
  AUTH *auth;
  unsigned long long total=0;
  int reads=0;
  struct timeval starttime, now, elapsed;
  double kbps;
  char opt;
  char myhostname[255];

  /* Parameters */
  int vers=2;
  int duration=60;
  int label=0;
  int uid=geteuid(), gid=getegid();
  int blocksize=4096;
  char *host=NULL;
  char *x;
  char *testpath=NULL; 
  char *exportname=NULL;
  int quiet = 0;
  char *proto="udp";
  
  while ((opt=getopt(argc, argv, "i:v:t:u:g:b:qp:"))!=-1) {
    switch (opt) {
    case 'v':
      vers=atoi(optarg);
      if (vers != 2 && vers != 3) {
	fprintf(stderr, "%s: NFS V%d not supported yet\n", argv[0], vers);
	exit(1);
      }
      break;
    case 't':
      duration=atoi(optarg);
      if (duration < 1) {
	fprintf(stderr, "%s: Duration must be greater than zero.\n", argv[0]);
	exit(1);
      }
      break;
    case 'q':
      quiet = 1;
      break;
    case 'i': 
      label=atoi(optarg);
      break;
    case 'u': 
      uid=atoi(optarg);
      break;
    case 'g':
      gid=atoi(optarg);
      break;
    case 'b':
      blocksize=atoi(optarg);
      break;
    case 'p':
      if (strcmp(optarg, "udp") !=0 && strcmp(optarg,"tcp") != 0) {
	fprintf(stderr, "Invalid protocol: '%s'. Must be udp or tcp.\n", optarg);
	exit(1);
      }
      proto=strdup(optarg);
      break;
    default:
      usage(argv[0]);
    }
  }

  if (optind >= argc) 
    usage(argv[0]);

  x=strchr(argv[optind], ':');
  if (!x) 
    usage(argv[0]);
 
  *x=0;

  host=argv[optind];
  
  exportname=x+1;

  if (!strlen(exportname))
    usage(argv[0]);

  x=strchr(*exportname == '/' ? exportname+1 : exportname , '/');
  if (!x) 
    usage(argv[0]);
  
  *x=0;
  
  testpath=x+1;
  if (!strlen(testpath))
    usage(argv[0]);

  if (!strcmp(proto,"udp") && blocksize > 8192) {
    fprintf(stderr, "Max NFS blocksize over UDP is 8192\n");
    exit(1);
  }

  printf("(\n");
  printf(":export-name \"%s\"\n", exportname);
  printf(":testpath \"%s\"\n", testpath);
  printf(":iteration %d\n", label);
  printf(":nfs-version %d\n", vers);
  printf(":blocksize %d ;; in bytes\n", blocksize);
  printf(":transport :%s\n", proto);

  gethostname(myhostname, sizeof(myhostname));

  auth=authunix_create(myhostname, uid, gid, 0, NULL);

  rootfh=get_export_fh(vers, host, exportname, auth);

  clnt=clnt_create_with_retry(host, NFS_PROGRAM, vers, proto);
  if (!clnt) {
    clnt_pcreateerror("clnt_create failed[3]");
    exit(1);
  }

  clnt->cl_auth=auth;

  fh=lookup_path(clnt, rootfh, testpath);

#if 0
  /* Ahmon and I used this to debug the problem in spr43071 and the
   * same problem seen by another customer, where when a file is
   * deleted we get this error.  It required this hackery because most
   * NFS clients don't cause the error because when they notice
   * there's a problem, they probe the file and find it's been
   * deleted.
   */
  {
      printf("pause to delete file on NFS server host; restart server:");
      getchar();
      nfs_read(clnt, fh, blocksize);
      exit(1);
  }
#endif

  gettimeofday(&starttime, NULL);
  
  while (1) {
    int count;

    gettimeofday(&now, NULL);
    
    timeval_subtract(&elapsed, &now, &starttime);
    
    if (elapsed.tv_sec >= duration)
      break;

    count=nfs_read(clnt, fh, blocksize);
    total+=count;
    reads++;
  }

#ifdef __APPLE__
  printf(":duration %ld.%06d  ;; seconds\n", elapsed.tv_sec, elapsed.tv_usec);
#else
  printf(":duration %ld.%06ld  ;; seconds\n", elapsed.tv_sec, elapsed.tv_usec);
#endif

  printf(":reads %d\n", reads);
  printf(":read-bytes %llu\n", total);
  kbps=(double)total/timeval_to_seconds(&elapsed)/(double)1024;
  printf(":rate %f ;; KB/second\n", kbps);
  printf(")\n");
  clnt_destroy(clnt);
  
  return 0;
}
