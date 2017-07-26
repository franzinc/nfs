#include "hammernfs-libs/mount.h"
#include "hammernfs-libs/nfs.h"

struct file_handle {
  int vers;
  int len;
  char data[FHSIZE3];
};

CLIENT *clnt_create_with_retry(char *host, unsigned long program, 
			       unsigned long version, char *proto);

struct file_handle *get_export_fh(int vers, char *host, char *export, 
				  AUTH *auth);

struct file_handle *lookup(CLIENT *clnt, struct file_handle *base, 
			   char *name);

struct file_handle *lookup_path(CLIENT *clnt, struct file_handle *base,
				char *path);

int nfs_read(CLIENT *clnt, struct file_handle *fh, int count);

double timeval_to_seconds(struct timeval *tv);

int timeval_subtract (struct timeval *result, struct timeval *x, struct timeval *y);

int split_host_and_path(char *string, char **host, char **path, char **complaint);

