/* $Header: /repo/cvs.copy/nfs/testnfs.c,v 1.1 2005/06/01 17:27:52 dancy Exp $ */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/statfs.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/vfs.h>
#include <malloc.h>
#include <utime.h>
#include <dirent.h>
#ifdef sun
#define Solaris
#include <sys/statvfs.h>
#endif

/* nfs calls:
  getattr (tested during rename testing)
  setattr  (tested)
  lookup (tested as a side effect of others.. no direct access)
  read (tested)
  write (tested)
  create (tested)
  remove (tested)
  rename (tested)
  mkdir (tested)
  rmdir (tested)
  readdir (tested)
  statfs (tested)
*/

/* non-goal:  Testing the nfs client... so there is no test for
   deleting or renaming an open file .. both of which amount to a
   behind-the-scenes rename call to the nfs server.  */

/* stuff not tested:
   file permission bits (since they're server controlled).
   file ownership info (server controlled)
*/

/* Things that I might want to try:
   Creating files with backslash or colon */

/* NFS client caching will likely interfere with some tests 
   (e.g., the repeat-create or repeat-remove tests.. they
   don't really result in repeated NFS calls) */

#define BIGTESTFILE "acl62.bz2"

#define TESTFILES 1000

#ifdef Solaris
#define statfs statvfs
#endif

void test_statfs(char *nfsdir) {
  struct statfs sfs;

  printf("testing statfs\n");
  
  /* test statfs */
  if (statfs(nfsdir, &sfs)) {
	  printf("statfs(%s) failed: %s\n", nfsdir, strerror(errno));
	  exit(1);
  }
  
  if (sfs.f_blocks < sfs.f_bfree ||
      sfs.f_blocks < sfs.f_bavail) {
    printf("Weird! statfs says that total filesystem blocks is less than free blocks.\n");
    exit(1);
  }
  
}


void test_read(char *nfsdir) {
  char filename[1024], buf[64*1024];
  int infd, outfd, got, wrote, res;

  printf("testing read\n");

  sprintf(filename, "%s/%s", nfsdir, BIGTESTFILE);

  infd=open(filename, O_RDONLY);
  if (infd < 0) {
    printf("Failed to open %s for reading: %s\n", 
	   filename, strerror(errno));
    exit(1);
  }

  outfd=open("/tmp/nfsreadfile", O_WRONLY|O_CREAT, 0666);
  if (outfd < 0) {
    perror("Failed to open /tmp/nfsreadfile for writing");
    exit(1);
  }

  while ((got=read(infd, buf, sizeof(buf))) > 0) {
    wrote=write(outfd, buf, got);
    if (wrote < 0) {
      perror("Failed to writo to /tmp/nfsreadfile");
      exit(1);
    }
    if (wrote != got) {
      printf("Incomplete write to /tmp/nfsreadfile.  Tried to write %d but only wrote %d\n", got, wrote);
      exit(1);
    }
  }

  /* EOF or error */
  if (got < 0) {
    printf("Error while reading %s: %s\n", filename,
	   strerror(errno));
    exit(1);
  }

  /* EOF */
  close(infd);
  close(outfd);

  /* Compare the file we saved to a known good sample */
  
  sprintf(buf, "diff %s /tmp/nfsreadfile", filename);
  res=system(buf);
  
  if (res != 0) {
    printf("File read from nfs server differs from expected!\n");
    exit(1);
  }
  
  /* cleanup */
  unlink("/tmp/nfsreadfile");

}

void test_mkdir(char *workdir) {
	printf("testing mkdir\n");
	

  /* test mkdir */
  if (mkdir(workdir, 0777)) {
    printf("mkdir(%s) failed: %s\n", workdir, strerror(errno));
    exit(1);
  }
  
  /* test mkdir failure */
  if (!mkdir(workdir, 0777)) {
    printf("Got no error when doing repeat mkdir(%s).\n", workdir);
    exit(1);
  }
  
  if (errno != EEXIST) {
    printf("Got '%s' when doing repeat mkdir(%s).\nexpected: %s\n",
	   strerror(errno), workdir, strerror(EEXIST));
    exit(1);
  }
  
}  

void test_create(char *workdir) {
  int fd;
  char filename[1024];

  printf("testing create\n");
  
  /* test create */
  sprintf(filename, "%s/file1", workdir);
  fd=open(filename, O_WRONLY|O_CREAT, 0666); 
  if (fd < 0) {
    printf("open(%s, O_WRONLY|O_CREAT, 0666) failed: %s\n",
	   filename, strerror(errno));
    exit(1);
  }
  
  close(fd);
  
}

void test_remove(char *workdir) {
  char filename[1024];
  
  printf("testing remove\n");
  
  sprintf(filename, "%s/file1", workdir);
  
  /* test remove */
  if (unlink(filename)) {
    printf("unlink(%s) failed: %s\n", filename, strerror(errno));
    exit(1);
  }
  
  /* Second attempt should fail w/ ENOENT */
  if (!unlink(filename)) {
    printf("Got no error when doing repeat unlink(%s)\n", filename);
    exit(1);
  }
  
  if (errno != ENOENT) {
    printf("Got '%s' when doing repeat unlink(%s).\nexpected: %s\n",
	   strerror(errno), workdir, strerror(ENOENT));
    exit(1);
  }
}
  

void test_rmdir(char *workdir) {
	printf("testing rmdir\n");

  /* tests rmdir */
  if (rmdir(workdir)) {
    printf("rmdir(%s) failed: %s\n", workdir, strerror(errno));
    exit(1);
  }
  
  /* Second attempt should fail w/ ENOENT */
  if (!rmdir(workdir)) {
    printf("Got no error when doing repeat rmdir(%s).\n", workdir);
    exit(1);
  }
  if (errno != ENOENT) {
    printf("Got '%s' when doing repeat rmdir(%s).\nexpected: %s\n",
	   strerror(errno), workdir, strerror(ENOENT));
    exit(1);
  }
}

void test_rename(char *workdir) {
	char file1[1024], file2[1024];
	struct stat sb1, sb2;

	printf("testing rename\n");

	sprintf(file1, "%s/file1", workdir);
	sprintf(file2, "%s/file2", workdir);
	
	/* test stat first.. since we need it to test rename */
	if (stat(file1, &sb1)) {
		printf("stat(%s) failed: %s\n",
		       file1, strerror(errno));
		exit(1);
	}
	
	/* Not much to look at, really..  how about making sure that
	   the modification time of the file is not in the future.
	   allow ten seconds of slack */
	if (sb1.st_mtime > time(NULL) + 10) {
		printf("Modification time of %s is in the future!\n",
		       file1);
		exit(1);
	}
	
	if (rename(file1, file2)) {
		printf("rename(%s,%s) failed: %s\n",
		       file1, file2, strerror(errno));
		exit(1);
	}

	/* try repeat */
	if (!rename(file1, file2)) {
		printf("repeat rename(%s,%s) worked.. but it shouldn't have\n",
		       file1, file2);
		exit(1);
	}

	/* inode number of file2 should be the same as it was for file1 */
	if (stat(file2, &sb2)) {
		printf("stat(%s) failed: %s\n",
		       file2, strerror(errno));
		exit(1);
	}
	
	if (sb1.st_ino != sb2.st_ino) {
		printf("inode number changed during rename operation.\n");
		exit(1);
	}
	
	/* rename back */
	if (rename(file2, file1)) {
		printf("rename(%s,%s) failed: %s\n",
		       file1, file2, strerror(errno));
		exit(1);
	}

}		


int test_readdir(char *workdir) {
	int i, fd, direntnum;
	char filename[1024], seen[TESTFILES];
	DIR *dirp;
	struct dirent *de;
	
	printf("Making a bunch of directory entries...\n");
	/* make a bunch of test files */
	
	for(i=0; i<TESTFILES; i++) {
		sprintf(filename, "%s/dirent%d", workdir, i);
		fd=open(filename, O_WRONLY|O_CREAT, 0666);
		if (fd < 0) {
			printf("Failed to open %s for writing: %s\n",
			       filename, strerror(errno));
			exit(1);
		}
		close(fd);
	}

	printf("Testing readdir\n");
	memset(seen, 0, sizeof(seen));
	
	dirp=opendir(workdir);
	if (!dirp) {
		printf("failed to opendir(%s): %s\n",
		       workdir, strerror(errno));
		exit(1);
	}
	
	/* tests modifying the directory while readdiring it */
	while (de=readdir(dirp)) {
		if (sscanf(de->d_name, "dirent%d", &direntnum)==1) {
			seen[direntnum]=1;
			sprintf(filename, "%s/%s", workdir, de->d_name);
			if (unlink(filename)) {
				printf("unlink(%s) failed: %s\n",
				       filename, strerror(errno));
				exit(1);
			}
			
		}
	}
#if 0
	if (errno) {
		perror("readdir");
		exit(1);
	}
#endif
	closedir(dirp);

	/* Make sure we saw all the expected files */
	for (i=0; i<TESTFILES; i++) {
		if (!seen[i]) {
			printf("File dirent%d wasn't seen in the directory listing.\n", i);
			exit(1);
		}
	}
	
}


void test_setattr(char *workdir) {
	char filename[1024];
	struct stat sb;
	struct utimbuf ut;

	printf("Testing setattr\n");

	sprintf(filename, "%s/file1", workdir);
	
	ut.actime=time(NULL)-60;
	ut.modtime=ut.actime-60;

	if (utime(filename, &ut)) {
		printf("utime(%s,struct) failed: %s\n",
		       filename, strerror(errno));
		exit(1);
	}
	
	if (stat(filename, &sb)) {
		printf("stat(%s) failed: %s\n",
		       filename, strerror(errno));
		exit(1);
	}
	
	if (sb.st_atime != ut.actime) {
		printf("stat atime differs from utime'd atime\n");
		exit(1);
	}
	if (sb.st_mtime != ut.modtime) {
		printf("stat mtime differs from utime'd mtime\n");
		exit(1);
	}

	/* truncate uses setattr too */
	if (truncate(filename, 10*1024)) {
		printf("truncate(%s, 10*1024) failed: %s\n",
		       filename, strerror(errno));
		exit(1);
	}
	
	if (stat(filename, &sb)) {
		printf("stat(%s) failed: %s\n",
		       filename, strerror(errno));
		exit(1);
	}
	
	
	if (sb.st_size != 10*1024) {
		printf("new file size does not match what it should be.\n");
		exit(1);
	}
	
}
	

void test_write(char *workdir, char *nfshost, char *hosttemp, 
		char *workdirbasename) {
	char filename[1024], outfile[1024], buffer[64*1024];
	int infd, outfd, written, got, res;

	printf("testing write\n");

	sprintf(filename, "/tmp/%s", BIGTESTFILE);
	
	sprintf(outfile, "%s/%s.written", workdir, BIGTESTFILE);

	infd=open(filename, O_RDONLY);
	if (infd < 0) {
		printf("open(%s) failed: %s\n", filename,
		       strerror(errno));
		exit(1);
	}
	
	outfd=open(outfile, O_WRONLY|O_CREAT, 0666);
	if (outfd < 0) {
		printf("open(%s) for writing failed: %s\n", outfile,
		       strerror(errno));
		exit(1);
	}
	
	while ((got=read(infd, buffer, sizeof(buffer))) > 0) {
		written=write(outfd, buffer, got);
		if (written < 0) {
			printf("Error writing to %s: %s\n",
			       outfile, strerror(errno));
			exit(1);
		}
		if (written != got) {
			printf("Expected to write %d but only wrote %d\n",
			       got, written);
			exit(1);
		}
	}
	
	if (got < 0) {
		printf("Error while reading %s: %s\n",
		       filename, strerror(errno));
		exit(1);
	}
	
	close(infd);
	
	if (close(outfd)) {
		printf("error closing %s: %s\n", outfile,
		       strerror(errno));
		exit(1);
	}
	
	    
	sprintf(buffer, "on %s /usr/bin/diff %s/%s %s/%s/%s.written",
		nfshost, hosttemp, BIGTESTFILE, hosttemp, 
		workdirbasename, BIGTESTFILE);
	
	res=system(buffer);
	
	if (res) {
		printf("Non-zero return code from remote diff.\n");
		exit(1);
	}
	
	if (unlink(outfile)) {
		printf("unlink(%s): %s\n", outfile, strerror(errno));
		exit(1);
	}
	
}
	    
	    

	
	


int main(int argc, char **argv) {
  struct stat sb;
  char workdir[1024], filename[1024], *p, *p2;
  char workdirbasename[1024];
  char *nfsdir, *nfshost, *hosttemp;
  int fd, res, i, got, skipread=0, skipwrite=0, argi=1;
  
  argc--;

  while (argc && argv[argi][0] == '-') {
	  if (!strcmp(argv[argi], "-skipread"))
		  skipread=1;
	  if (!strcmp(argv[argi], "-skipwrite"))
		  skipwrite=1;
	  argi++;
	  argc--;
  }


  if (argc != 3) {
    printf("Usage: %s [-skipread] [-skipwrite] nfshost host-temp-dir nfs-mounted-temp-dir\n", argv[0]);
    exit(1);
  }
  
  nfshost=argv[argi++];
  argc--;

  hosttemp=argv[argi++];
  argc--;
  
  nfsdir=argv[argi];
  argc--;
  

  test_statfs(nfsdir);

  /* Start with a fresh work area */
  sprintf(workdirbasename, "tmp%d", getpid());
  sprintf(workdir, "%s/%s", nfsdir, workdirbasename);
  printf("workdir is %s\n", workdir);

  test_mkdir(workdir);

  test_create(workdir);

  test_setattr(workdir);

  if (!skipread) 
	  test_read(nfsdir);

  if (!skipwrite)
	  test_write(workdir, nfshost, hosttemp, workdirbasename);

  test_rename(workdir);
  
  test_readdir(workdir);
  
  test_remove(workdir);
  
  test_rmdir(workdir);

  return 0;

}
