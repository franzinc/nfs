/*
 * This is for testing connection reset errors.  We cause
 * the reset to see how the server behaves.
 */
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
  fprintf(stderr, "Usage: %s [ -q ] [ -v nfsvers ] [ -t test_duration ] [ -u uid ] [ -g gid ] [ -b blocksize ] [ -p udp|tcp ] [ -i label ] host:/export/path/to/file_to_read\n", prg);
  exit(1);
}

/* Attempts to work around Windows + RDP disconnect strangeness */
CLIENT *clnt_create_with_retry(char *host, unsigned long program, 
			    unsigned long version, char *proto) {
  CLIENT *clnt;
  int tries;

  for (tries=0;tries<10;tries++) {
    clnt=clnt_create(host, program, version, proto);
    if (clnt) {
      if (tries) 
	printf("%s: Try #%d: clnt_create succeeded.\n", __func__, tries+1);	
      
      return clnt;
    }
    
    if (rpc_createerr.cf_stat == RPC_SYSTEMERROR && rpc_createerr.cf_error.re_errno == EADDRINUSE) {
      printf("%s: Try #%d: %s\n", __func__, tries+1, clnt_spcreateerror("clnt_create"));
      continue;
    }
    
    /* Some other failure that we don't handle */
    return NULL;
  }

  return NULL;
}
      
    


struct file_handle *get_export_fh3(char *host, char *export, AUTH *auth) {
  mountres3 *mountres;
  CLIENT *clnt;
  struct file_handle *fh;

  clnt=clnt_create_with_retry(host, MOUNTPROG, 3, "udp");
  if (!clnt) {
    clnt_pcreateerror("clnt_create failed[1]");
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
  
  if (clnt_freeres(clnt, (xdrproc_t)xdr_mountres3, (char *)mountres) != 1) {
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

  clnt=clnt_create_with_retry(host, MOUNTPROG, 1, "udp");
  if (!clnt) {
    clnt_pcreateerror("clnt_create failed[2]");
    exit(1);
  }
  clnt->cl_auth=auth;
  
  fhstatus=mountproc_mnt_1(&export, clnt);

  if (!fhstatus) {
    printf("mountproc_mnt_1 returned NULL\n");
    exit(1);
  }

  if (fhstatus->fhs_status != 0) {
    printf("mount failed: status %d\n", fhstatus->fhs_status);
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
  
  if (clnt_freeres(clnt, (xdrproc_t)xdr_fhstatus, (char *)fhstatus) != 1) {
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
  default:
    printf("%s: Unsupported protocol version: %d\n", __func__, vers);
    exit(1);
    return NULL; // Satisfy compiler
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
    printf("%s: lookup of name %s failed with status: %d (%s)\n",
	   __func__, name, res->status, strerror(res->status));
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

  if (clnt_freeres(clnt, (xdrproc_t)xdr_diropres, (char *)res) != 1) {
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
    printf("%s: lookup of name %s failed with status: %d (%s)\n",
	   __func__, name, res->status, strerror(errno));
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

  if (clnt_freeres(clnt, (xdrproc_t)xdr_LOOKUP3res, (char *)res) != 1) {
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
  default:
    printf("%s: Unsupported protocol version: %d\n", __func__, base->vers);
    exit(1);
    return NULL; // Satisfy compiler
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
  
  if (clnt_freeres(clnt, (xdrproc_t)xdr_readres, (char *)res) != 1) {
    printf("clnt_freeres failed\n");
    exit(1);
  }

  return count;
}

/* timeout is 0 since we don't wait for a response */
static struct timeval TIMEOUT = { 0, 0 };

READ3res *
x_nfsproc3_read_3(READ3args *argp, CLIENT *clnt)
{
	static READ3res clnt_res;

	memset((char *)&clnt_res, 0, sizeof(clnt_res));
	if (clnt_call (clnt, NFSPROC3_READ,
		(xdrproc_t) xdr_READ3args, (caddr_t) argp,
		(xdrproc_t) xdr_READ3res, (caddr_t) &clnt_res,
		TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&clnt_res);
}

void nfs_read3(CLIENT *clnt, struct file_handle *fh, int count) {
  READ3args arg;
  READ3res *res;
  int n;

  arg.file.data.data_len=fh->len;
  arg.file.data.data_val=fh->data;
  arg.offset=0;
  arg.count=count;

  /* takes several tries to trigger the problem */
  for (n=0;n<20;n++) {
      x_nfsproc3_read_3(&arg, clnt);
  }
}

double timeval_to_seconds(struct timeval *tv) {
  return tv->tv_sec + tv->tv_usec / (double)1000000;
}

/* Ref: http://www.gnu.org/software/libc/manual/html_node/Elapsed-Time.html */
/* result=x-y */
/* Subtract the `struct timeval' values X and Y,
   storing the result in RESULT.
   Return 1 if the difference is negative, otherwise 0. */

int
timeval_subtract (result, x, y)
     struct timeval *result, *x, *y;
{
  /* Perform the carry for the later subtraction by updating y. */
  if (x->tv_usec < y->tv_usec) {
    int nsec = (y->tv_usec - x->tv_usec) / 1000000 + 1;
    y->tv_usec -= 1000000 * nsec;
    y->tv_sec += nsec;
  }
  if (x->tv_usec - y->tv_usec > 1000000) {
    int nsec = (x->tv_usec - y->tv_usec) / 1000000;
    y->tv_usec += 1000000 * nsec;
    y->tv_sec -= nsec;
  }
  
  /* Compute the time remaining to wait.
     tv_usec is certainly positive. */
  result->tv_sec = x->tv_sec - y->tv_sec;
  result->tv_usec = x->tv_usec - y->tv_usec;
  
  /* Return 1 if result is negative. */
  return x->tv_sec < y->tv_sec;
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

  gettimeofday(&starttime, NULL);

  nfs_read3(clnt, fh, 4096);
  clnt_destroy(clnt);
  return 0;
}
