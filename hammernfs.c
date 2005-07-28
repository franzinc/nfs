#include <stdio.h>
#include <rpc/rpc.h>
#include <sys/socket.h>
#include <rpcsvc/mount.h>
#include <rpcsvc/nfs_prot.h>

#define TESTFILE "setup.log.full"

char *EXPORTNAME="/export";

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



void nfs_call() {
}


int main(int argc, char **argv) {
  int vers;
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
  char *host=argv[1];

  if (argc != 3) {
    printf("Usage: %s host protocolversion\n", argv[0]);
    exit(1);
  }

  vers=atoi(argv[2]);
  
  if (vers != 2 && vers != 3) {
    printf("protocol version must be 2 or 3\n");
    exit(1);
  }

  if (vers == 3) {
    printf("version 3 not supported yet\n");
    exit(1);
  }

  auth=authunix_create("localhost", 0, 0, 0, NULL);

  callrpc(host, MOUNTPROG, vers == 2 ? 1 : vers, MOUNTPROC_MNT,
	  xdr_string, &EXPORTNAME, xdr_fhstatus, &fhstatus);
  
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
  doa.name=TESTFILE;

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
  ra.count=4096;
  ra.totalcount=4096; /* unused */

  time(&starttime);
  
  while (1) {
    time(&now);
    
    if (now-starttime >= 60) 
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
