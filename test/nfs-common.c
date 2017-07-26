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
#include <assert.h>
#include "nfs-common.h"

void print_fh(struct file_handle *fh) {
  int i;
  
  for(i=0; i<fh->len; i++) {
    printf("%02x", fh->data[i]);
  }
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

    printf("%s: clnt_create failed.  rpc_createerr.cf_stat: %d, rpc_createerr.cf_error.re_errno: %d\n",
	   __func__, rpc_createerr.cf_stat, rpc_createerr.cf_error.re_errno);
    
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
  
  if (clnt_freeres(clnt, (xdrproc_t)xdr_READ3res, (char *)res) != 1) {
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
  default:
    printf("%s: Unsupported protocol version: %d\n", __func__, fh->vers);
    exit(1);
    return 0; // Satisfy compiler
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

int timeval_subtract (struct timeval *result, struct timeval *x, struct timeval *y) {
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

/* string is the input.  It is expected to be
 * a string in host:/path format.
 * host and path are outputs.  They will be populated with
 * string pointers.
 *
 * Returns 1 if successful, 0 otherwise (in which case
 *  complaint will be updated to point to an error string).
 */
int split_host_and_path(char *string, char **host, char **path, char **complaint) {
  char *colon;

  *host=strdup(string);
  *path=NULL;
  *complaint=NULL;

  assert(*host);

  colon=strchr(*host, ':');
  if (colon == NULL) {
    *complaint="Expected a string in host:/path format, but found no colon";
    return 0;
  }

  *colon=0;
  *path=colon+1; // FIXME: Consider strduping this so that the host and path can be free'd separately.
  
  if (!strlen(*path)) {
    *complaint="Expected a string in host:/path format, but path is blank";
    return 0;
  }
  
  return 1;
}
