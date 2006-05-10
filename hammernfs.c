#include <stdio.h>
#include <string.h>
#include <getopt.h>
#include <rpc/rpc.h>
#include <sys/socket.h>
#include "hammernfs-libs/mount.h"
#include "hammernfs-libs/nfs.h"

struct file_handle {
  int vers;
  int len;
  char data[FHSIZE3];
};

void print_fh(struct file_handle *fh) {
  int i;
  
  for(i=0; i<fh->len; i++) {
    printf("%02x", fh->data[i]);
  }
}

void usage(char *prg) {
  fprintf(stderr, "Usage: %s [ -q ] [ -v nfsvers ] [ -t test_duration ] [ -u uid ] [ -g gid ] [ -b blocksize ] host:/export/path/to/file_to_read\n", prg);
  exit(1);
}

struct file_handle *get_export_fh3(char *host, char *export, AUTH *auth) {
  mountres3 *mountres;
  CLIENT *clnt;
  struct file_handle *fh;

  clnt=clnt_create(host, MOUNTPROG, 3, "udp");
  if (!clnt) {
    printf("clnt_create failed\n");
    exit(1);
  }
  
  clnt->cl_auth=auth;
  
  mountres=mountproc3_mnt_3(&export, clnt);
  
  if(mountres->fhs_status != MNT3_OK) {
    printf("mount failed: status: %d\n", mountres->fhs_status);
    exit(1);
  }

  fh=malloc(sizeof(struct file_handle));
  if (!fh) {
    perror("malloc");
    exit(1);
  }

  fh->vers=3;
  fh->len=mountres->mountres3_u.mountinfo.fhandle.fhandle3_len;
  memcpy(fh->data, mountres->mountres3_u.mountinfo.fhandle.fhandle3_val,
	 fh->len);
  
  if (clnt_freeres(clnt, xdr_mountres3, mountres) != 1) {
    printf("clnt_freeres failed\n");
    exit(1);
  }
  
  clnt_destroy(clnt);
  
  return fh;
}

struct file_handle *get_export_fh2(char *host, char *export, AUTH *auth) {
  CLIENT *clnt;
  fhstatus *fhstatus;
  struct file_handle *fh;

  clnt=clnt_create(host, MOUNTPROG, 1, "udp");
  if (!clnt) {
    printf("clnt_create failed\n");
    exit(1);
  }
  clnt->cl_auth=auth;
  
  fhstatus=mountproc_mnt_1(&export, clnt);

  if (fhstatus->fhs_status != 0) {
    printf("mount failed: status ~d\n", fhstatus->fhs_status);
    exit(1);
  }

  fh=malloc(sizeof(struct file_handle));
  if (!fh) {
    perror("malloc");
    exit(1);
  }

  fh->vers=2;
  fh->len=FHSIZE;
  memcpy(fh->data, fhstatus->fhstatus_u.fhs_fhandle, FHSIZE);
  
  if (clnt_freeres(clnt, xdr_fhstatus, fhstatus) != 1) {
    printf("clnt_freeres failed\n");
    exit(1);
  }

  clnt_destroy(clnt);

  return fh;
  
}

struct file_handle *get_export_fh(int vers, char *host, char *export, 
				  AUTH *auth) {
  switch(vers) {
  case 2:
    return get_export_fh2(host, export, auth);
    break;
  case 3:
    return get_export_fh3(host, export, auth);
    break;
  }
}
      
struct file_handle *lookup2(CLIENT *clnt, struct file_handle *base, 
			    char *name) {
  diropres *res;
  struct file_handle *fh;
  diropargs arg;
  
  memcpy(&arg.dir.data, base->data, NFS_FHSIZE); 
  arg.name=name;
  
  res=nfsproc_lookup_2(&arg, clnt);
  
  if (res->status != NFS_OK) {
    printf("lookup of name %s failed with status: %d\n",
	   name, res->status);
    exit(1);
  }
  
  fh=malloc(sizeof(*fh));
  if (!fh) {
    perror("malloc");
    exit(1);
  }
  fh->vers=2;
  fh->len=NFS_FHSIZE;
  memcpy(fh->data, &res->diropres_u.diropres.file, NFS_FHSIZE);

  if (clnt_freeres(clnt, xdr_diropres, res) != 1) {
    printf("clnt_freeres failed\n");
    exit(1);
  }

  return fh;

}

struct file_handle *lookup3(CLIENT *clnt, struct file_handle *base, 
			    char *name) {
  LOOKUP3res *res;
  struct file_handle *fh;
  LOOKUP3args arg;
  
  arg.what.dir.data.data_len=base->len;
  arg.what.dir.data.data_val=base->data;
  arg.what.name=name;
  
  res=nfsproc3_lookup_3(&arg, clnt);
  
  if (res->status != NFS_OK) {
    printf("lookup of name %s failed with status: %d\n",
	   name, res->status);
    exit(1);
  }
  
  fh=malloc(sizeof(*fh));
  if (!fh) {
    perror("malloc");
    exit(1);
  }


  fh->vers=3;
  fh->len=res->LOOKUP3res_u.resok.object.data.data_len;
  memcpy(fh->data, res->LOOKUP3res_u.resok.object.data.data_val, fh->len);

  if (clnt_freeres(clnt, xdr_LOOKUP3res, res) != 1) {
    printf("clnt_freeres failed\n");
    exit(1);
  }

  return fh;

}

struct file_handle *lookup(CLIENT *clnt, struct file_handle *base, 
			   char *name) {
  switch (base->vers) {
  case 2:
    return lookup2(clnt, base, name);
  case 3:
    return lookup3(clnt, base, name);
  }
}

struct file_handle *lookup_path(CLIENT *clnt, struct file_handle *base,
				char *path) {
  char comp[1024];
  char *slash=strchr(path, '/');
  struct file_handle *fh;

  if (!slash)
    return lookup(clnt, base, path);

  strncpy(comp, path, slash-path);

  base=lookup(clnt, base, comp);
  /* need to free 'base' after we get the result we want */

  fh=lookup_path(clnt, base, slash+1);
  
  free(base);
  
  return fh;

}

int nfs_read2(CLIENT *clnt, struct file_handle *fh, int count) {
  readargs arg;
  readres *res;

  memcpy(arg.file.data, fh->data, fh->len);
  arg.offset=0;
  arg.count=count;
  arg.totalcount=count;
  res=nfsproc_read_2(&arg, clnt);

  if (res->status != NFS_OK) {
    printf("nfs read failed, status: %d\n", res->status);
    exit(1);
  }

  count=res->readres_u.reply.data.data_len;
  
  if (clnt_freeres(clnt, xdr_readres, res) != 1) {
    printf("clnt_freeres failed\n");
    exit(1);
  }

  return count;
}

int nfs_read3(CLIENT *clnt, struct file_handle *fh, int count) {
  READ3args arg;
  READ3res *res;

  arg.file.data.data_len=fh->len;
  arg.file.data.data_val=fh->data;
  arg.offset=0;
  arg.count=count;
  res=nfsproc3_read_3(&arg, clnt);
  if (!res) {
    clnt_perror(clnt, "Failed to decode result from readv3 call");
    exit(1);
  }

  if (res->status != NFS_OK) {
    printf("nfs read failed, status: %d\n", res->status);
    exit(1);
  }
  
  count=res->READ3res_u.resok.count;
  
  if (clnt_freeres(clnt, xdr_READ3res, res) != 1) {
    printf("clnt_freeres failed\n");
    exit(1);
  }

  return count;
}

int nfs_read(CLIENT *clnt, struct file_handle *fh, int count) {
  switch(fh->vers) {
  case 2:
    return nfs_read2(clnt, fh, count);
  case 3:
    return nfs_read3(clnt, fh, count);
  }
}

int main(int argc, char **argv) {
  struct sockaddr_in addr;
  struct file_handle *rootfh, *fh;
  CLIENT *clnt;
  diropargs doa;
  diropres *dor;
  AUTH *auth;
  unsigned long long total=0;
  time_t starttime, now;
  double kbps;
  char opt;
  char myhostname[255];

  /* Parameters */
  int vers=2;
  int duration=60;
  int uid=geteuid(), gid=getegid();
  int blocksize=4096;
  char *host=NULL;
  char *x;
  char *testpath=NULL; 
  char *exportname=NULL;
  int quiet = 0;
  
  while ((opt=getopt(argc, argv, "v:t:h:e:f:u:g:b:q"))!=-1) {
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

  printf("export:   %s\n", exportname);
  printf("testpath: %s\n", testpath);

  gethostname(myhostname, sizeof(myhostname));

  auth=authunix_create(myhostname, uid, gid, 0, NULL);

  rootfh=get_export_fh(vers, host, exportname, auth);

  clnt=clnt_create(host, NFS_PROGRAM, vers, "udp");
  if (!clnt) {
    printf("clnt_create failed\n");
    exit(1);
  }

  clnt->cl_auth=auth;

  fh=lookup_path(clnt, rootfh, testpath);

  time(&starttime);
  
  while (1) {
    int count;

    time(&now);
    
    if (now-starttime >= duration) 
      break;

    count=nfs_read(clnt, fh, blocksize);
    total+=count;
  }

  printf("Ran for %d seconds\n", now-starttime);
  
  printf("Read %u bytes\n", total);
  kbps=(double)total/(double)(now-starttime)/(double)1024;
  printf("%f KB/second\n", kbps);
  
  clnt_destroy(clnt);
  
  return 0;
}
