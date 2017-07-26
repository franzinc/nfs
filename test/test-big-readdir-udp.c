/*
 * Test for rfe15117
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
#include <assert.h>
#include "nfs-common.h"

/*
struct READDIR3args {
	nfs_fh3 dir;
	cookie3 cookie;
	cookieverf3 cookieverf;
	count3 count;
};
*/

void test_readdir3 (CLIENT *clnt, struct file_handle *fh) {
  READDIR3args args;

  memset(&args, 0, sizeof(args));

  args.dir.data.data_len=fh->len;
  args.dir.data.data_val=fh->data;

  args.count=8192; // works

  /* These don't work.  We end up getting a RPC_CANTDECODERES status
     (Can't decode result) back. The NFS server is returning a correct
     response, though.  And the Linux NFS client doesn't have any
     trouble with the results.  I think this issue is a size limit
     built into the sunrpc library.  Anyway, that problem basically
     makes this test program incapable of testing rfe15117, but I'm
     leaving this code in place for reference.
  */

  //args.count=65535;
  //args.count=16000; 
  
  READDIR3res res;
  memset(&res, 0, sizeof(res));
  
  struct timeval TIMEOUT = { 25, 0 };

  enum clnt_stat stat = clnt_call (clnt, NFSPROC3_READDIR,
				   (xdrproc_t) xdr_READDIR3args, (caddr_t) &args,
				   (xdrproc_t) xdr_READDIR3res, (caddr_t) &res,
				   TIMEOUT);
  if (stat != RPC_SUCCESS) {
    clnt_perror(clnt, "readdir3");
    exit (1);
  }
  
  printf("stat: %d\n", stat);
}

int main(int argc, char **argv) {
  char myhostname[255], *host, *path, *complaint;
  int uid = 0;
  int gid = 0;
  int vers = 3;
  CLIENT *clnt;
  AUTH *auth;
  struct file_handle *rootfh, *fh;  

  if (argc != 2) {
    printf("Usage: %s host:/path/to/directory-with-many-files\n", argv[0]);
    exit(1);
  }

  if (!split_host_and_path(argv[1], &host, &path, &complaint)) {
    printf("%s\n", complaint);
    exit(1);
  }

  if (gethostname(myhostname, sizeof(myhostname)) != 0) {
    perror("gethostname");
    exit(1);
  }
  
  auth=authunix_create(myhostname, uid, gid, 0, NULL);
  assert(auth);

  rootfh=get_export_fh(vers, host, path, auth);

  clnt=clnt_create_with_retry(host, NFS_PROGRAM, vers, "udp");
  if (!clnt) {
    clnt_pcreateerror("clnt_create failed");
    exit(1);
  }

  clnt->cl_auth=auth;

  test_readdir3(clnt, rootfh);

  clnt_destroy(clnt);

  return 0;
}
