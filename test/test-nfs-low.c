/*
 * Various tests which make direct RPC calls 
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

struct timeval TIMEOUT = { 25, 0 };

// Test bug24843 
void test_bogus_volume_guid(CLIENT *clnt, struct file_handle *fh) {
  printf("%s\n", __func__);

#if 0
  printf("    Orig file handle: ");
  print_fh(fh);
  printf("\n");
#endif
  
  struct file_handle *mod_fh = copy_file_handle(fh);
  /* Modify the volume guid portion of the copied file handle
     to trigger the bug.  The volume guid begins at offset 4 */
  mod_fh->data[4]++;

#if 0
  printf("Modified file handle: ");
  print_fh(mod_fh);
  printf("\n");
#endif

  FSSTAT3args args;
  args.fsroot.data.data_len=mod_fh->len;
  args.fsroot.data.data_val=mod_fh->data;

  FSSTAT3res res;
  memset(&res, 0, sizeof(res));
    
  enum clnt_stat stat = clnt_call (clnt, NFSPROC3_FSSTAT,
				   (xdrproc_t) xdr_FSSTAT3args, (caddr_t) &args,
				   (xdrproc_t) xdr_FSSTAT3res, (caddr_t) &res,
				   TIMEOUT);
  if (stat != RPC_SUCCESS) {
    clnt_perror(clnt, "fsstat3");
    exit (1);
  }
  
  switch (res.status) {
  case NFS3ERR_STALE:
    /* Since we passed in a deliberately bogus file handle, we expect to
       get a stale file handle response. */
    printf("PASS: Got stale nfs file handle response as expected.\n");
    free(mod_fh);
    return;
  case NFS3_OK:
    printf("WTF?! fsstat call succeeded unexpectedly.\n");
    exit(1);
  default:
    printf("FAIL: Got unexpected status: %d\n", res.status);
    exit(1);
  }
}


int main(int argc, char **argv) {
  CLIENT *clnt;
  struct file_handle *rootfh;

  if (argc != 2) {
    printf("Usage: %s host:/export\n", argv[0]);
    exit(1);
  }

  /* Prepare an NFSv3/UDP client */
  setup_client(argv[1], 3, "udp", &clnt, &rootfh);

  test_bogus_volume_guid(clnt, rootfh);

  clnt_destroy(clnt);

  return 0;
}
