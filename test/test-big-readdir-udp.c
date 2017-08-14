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
  int vers = 3;
  CLIENT *clnt;
  struct file_handle *rootfh;

  if (argc != 2) {
    printf("Usage: %s host:/path/to/directory-with-many-files\n", argv[0]);
    exit(1);
  }

  setup_client(argv[1], vers, "udp", &clnt, &rootfh);

  test_readdir3(clnt, rootfh);

  clnt_destroy(clnt);

  return 0;
}
