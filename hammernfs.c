#include <stdio.h>
#include <getopt.h>
#include <rpc/rpc.h>
#include <sys/socket.h>
#include <rpcsvc/mount.h>
#include <rpcsvc/nfs_prot.h>

void print_fh(unsigned char *fh, int vers) {
  int i;
  
  switch(vers) {
  case 2:
    for(i=0; i<FHSIZE; i++) {
      printf("%02x", fh[i]);
    }
    break;
  default:
    printf("Printing file handles for NFS v%d not supported yet\n",
	   vers);
    exit(1);
  }
}

void usage(char *prg) {
  fprintf(stderr, "Usage: %s [ -v nfsvers ] [ -t test_duration ] [ -h hostname ] [ -u uid ] [ -g gid ] [ -b blocksize ] -e export -f file_to_read\n", prg);
  exit(1);
}

int main(int argc, char **argv) {
  struct sockaddr_in addr;
  fhstatus fhstatus;
  CLIENT *clnt;
  diropargs doa;
  diropres *dor;
  AUTH *auth;
  readargs ra;
  readres *rres;
  unsigned long long total=0;
  time_t starttime, now;
  double kbps;
  char opt;

  /* Parameters */
  int vers=2;
  int duration=60;
  int uid=geteuid(), gid=getegid();
  int blocksize=4096;
  char *host="localhost";
  char *testfile=NULL; 
  char *exportname=NULL;
  
  while ((opt=getopt(argc, argv, "v:t:h:e:f:u:g:b:"))!=-1) {
    switch (opt) {
    case 'v':
      vers=atoi(optarg);
      if (vers != 2) {
	fprintf(stderr, "%s: NFS V%d not supported yet\n", argv[0]);
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
    case 'h':
      host=strdup(optarg);
      break;
    case 'e':
      exportname=strdup(optarg);
      break;
    case 'f':
      testfile=strdup(optarg);
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
    default:
      usage(argv[0]);
      exit(1);
    }
  }

  if (!exportname) {
    fprintf(stderr, "%s: Export name must be specified.\n", argv[0]);
    exit(1);
  }

  if (!testfile) {
    fprintf(stderr, "%s: Test filename name must be specified (relative to export).\n", argv[0]);
    exit(1);
  }

  auth=authunix_create("localhost", uid, gid, 0, NULL);

  callrpc(host, MOUNTPROG, vers == 2 ? 1 : vers, MOUNTPROC_MNT,
	  xdr_string, &exportname, xdr_fhstatus, &fhstatus);
  
  if (fhstatus.fhs_status != 0) {
    printf("mount failed\n");
    exit(1);
  }

  printf("Root file handle: ");
  print_fh(fhstatus.fhstatus_u.fhs_fhandle, vers);
  printf("\n");
  
  clnt=clnt_create(host, 100003, vers, "udp");
  if (!clnt) {
    printf("clnt_create failed\n");
    exit(1);
  }

  clnt->cl_auth=auth;

  memcpy(doa.dir.data, fhstatus.fhstatus_u.fhs_fhandle, FHSIZE);
  doa.name=testfile;

  dor=nfsproc_lookup_2(&doa, clnt);

  if (!dor) {
    printf("lookup failed\n");
    exit(1);
  }

  if (dor->status != 0) {
    printf("lookup failed.  Status: %d\n",
	   dor->status);
    exit(1);
  }

  printf("handle: ");
  print_fh(dor->diropres_u.diropres.file.data, 2);
  printf("\n");
  
  ra.file=dor->diropres_u.diropres.file;
  ra.offset=0;
  ra.count=blocksize;
  ra.totalcount=blocksize; /* unused */

  time(&starttime);
  
  while (1) {
    time(&now);
    
    if (now-starttime >= duration) 
      break;
    
    rres=nfsproc_read_2(&ra, clnt);
    
    if (rres->status != 0) {
      printf("read failed: status %d\n", rres->status);
      exit(1);
    }
    
    if (clnt_freeres(clnt, xdr_readres, &rres) != 1) {
      printf("clnt_freeres failed\n");
      exit(1);
    }
    
    total+=rres->readres_u.reply.data.data_len;
  }

  printf("Ran for %d seconds\n", now-starttime);
  
  printf("Read %u bytes\n", total);
  kbps=(double)total/(double)(now-starttime)/(double)1024;
  printf("%f KB/second\n", kbps);
  
  clnt_destroy(clnt);
  
  return 0;
}
