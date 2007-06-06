/* -*- mode: c-mode; c-basic-offset: 4; -*- */
/* $Header: /repo/cvs.copy/nfs/testnfs.c,v 1.9 2007/06/06 19:24:48 dancy Exp $ */

/* To build on 'blade', use /opt/SUNWspro/bin/cc */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef __FreeBSD__
#include <sys/param.h>
#include <sys/mount.h>
#endif

#if !defined(__hpux) && !defined(__FreeBSD__)
#include <sys/statfs.h>
#endif

#ifndef __FreeBSD__
#include <malloc.h>
#endif

#ifdef __hpux
#include <sys/vfs.h>
#endif

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <utime.h>
#include <dirent.h>
#include <string.h>
#include <time.h>
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

#define DEFAULT_TESTFILE "nfstestfile"
#define DEFAULT_HOSTTEMP "/tmp"
#define DEFAULT_LOCALTEMP "."
#define DEFAULT_TESTFILES 1000

char *testfile=DEFAULT_TESTFILE;
char *localtemp=DEFAULT_LOCALTEMP;
int testfiles=DEFAULT_TESTFILES;

#ifdef Solaris
#define statfs statvfs
#endif


void my_stat(char *filename, struct stat *sb) {
    if (stat(filename, sb)) {
	printf("stat(%s) failed: %s\n",
	       filename, strerror(errno));
	exit(1);
    }
    /* printf("%s: inum %d\n", filename, sb->st_ino); */
}

void verify_no_exist(char *filename) {
    struct stat sb;
    
    if (!stat(filename, &sb)) {
	printf("%s exists when it shouldn't\n");
	exit(1);
    }
}
  


void verify_inum(char *filename, struct stat *sb1) {
    struct stat sb2;

    my_stat(filename, &sb2);
	
    if (sb1->st_ino != sb2.st_ino) {
	printf("inode number of %s changed during rename operation.\n"
	       "Expected %d but got %d\n", 
	       filename, sb1->st_ino, sb2.st_ino);
	       
	exit(1);
    }
}

void my_rename(char *file1, char *file2) {
    if (rename(file1, file2)) {
	printf("rename(%s,%s) failed: %s\n",
	       file1, file2, strerror(errno));
	exit(1);
    }
}

void my_mkdir(char *path) {
    if (mkdir(path, 0777)) {
	printf("mkdir(%s) failed: %s\n", path, strerror(errno));
	exit(1);
    }
}

void my_rmdir(char *path) {
    if (rmdir(path)) {
	printf("rmdir(%s) failed: %s\n", path, strerror(errno));
	exit(1);
    }
}

void my_unlink(char *path) {
    if (unlink(path)) {
	printf("unlink(%s) failed: %s\n", path, strerror(errno));
	exit(1);
    }
}



void my_create(char *filename) {
    int fd;

    fd=open(filename, O_WRONLY|O_CREAT, 0666); 
    if (fd < 0) {
	printf("open(%s, O_WRONLY|O_CREAT, 0666) failed: %s\n",
	       filename, strerror(errno));
	exit(1);
    }
  
    close(fd);
}

void my_link(char *src, char *dest) {
    if (link(src, dest)) {
	printf("link(%s, %s) failed: %s\n", src, dest, strerror(errno));
	exit(1);
    }
}


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
    char infilename[1024], outfilename[1024], buf[64*1024];
    int infd, outfd, got, wrote, res;
  
    printf("testing read\n");
  
    snprintf(infilename, sizeof(infilename), "%s/%s", nfsdir, testfile);
    snprintf(outfilename, sizeof(outfilename), "%s/%s.read",
	     localtemp, testfile);

    infd=open(infilename, O_RDONLY);
    if (infd < 0) {
	printf("Failed to open %s for reading: %s\n", 
	       infilename, strerror(errno));
	exit(1);
    }

    outfd=open(outfilename, O_WRONLY|O_CREAT, 0666);
    if (outfd < 0) {
	printf("Failed to open %s for writing: %s\n",
	       outfilename, strerror(errno));
	exit(1);
    }

    while ((got=read(infd, buf, sizeof(buf))) > 0) {
	wrote=write(outfd, buf, got);
	if (wrote < 0) {
	    printf("Failed to write to %s: %s\n", outfilename, strerror(errno));
	    exit(1);
	}
	if (wrote != got) {
	    printf("Incomplete write to %s.  Tried to write %d but only wrote %d\n",
		   outfilename, got, wrote);
	    exit(1);
	}
    }

    /* EOF or error */
    if (got < 0) {
	printf("Error while reading %s: %s\n", infilename,
	       strerror(errno));
	exit(1);
    }

    /* EOF */
    close(infd);
    close(outfd);

    /* Compare the file we saved to a known good sample */
  
    sprintf(buf, "diff %s %s/%s", outfilename, localtemp, testfile);
    res=system(buf);
  
    if (res != 0) {
	printf("File read from nfs server differs from expected!\n");
	exit(1);
    }
  
    /* cleanup */
    unlink(outfilename);

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
    char file1[1024], file2[1024], link[1024],
	subdir[1024], newdir[1024];
    struct stat sb1, sb2;
    int fd;
    
    printf("testing rename\n");

    sprintf(file1, "%s/file1", workdir);
    sprintf(file2, "%s/file2", workdir);
	
    /* test stat first.. since we need it to test rename */
    my_stat(file1, &sb1);
	
    /* Not much to look at, really..  how about making sure that
       the modification time of the file is not in the future.
       allow ten seconds of slack */
    if (sb1.st_mtime > time(NULL) + 10) {
	printf("Modification time of %s is in the future!\n",
	       file1);
	exit(1);
    }
	
    my_rename(file1, file2);

    /* try repeat */
    if (!rename(file1, file2)) {
	printf("repeat rename(%s,%s) worked.. but it shouldn't have\n",
	       file1, file2);
	exit(1);
    }

	
    /* inode number of file2 should be the same as it was for file1 */
    verify_inum(file2, &sb1);
	
	
    /* rename back */
    my_rename(file2, file1);
	
    verify_inum(file1, &sb1);

    /* Make a subdir, which will be renamed */
    sprintf(subdir, "%s/rename_this_dir", workdir);
	
    my_mkdir(subdir);
    my_stat(subdir, &sb2);
	
    /* Test moving a file into a subdir */
    sprintf(file2, "%s/file1", subdir);
    my_rename(file1, file2);
    verify_inum(file2, &sb1);

    /* Add in a hard link to spice things up */
    sprintf(link, "%s/link", subdir);
    my_link(file2, link);

    sprintf(newdir, "%s/newdirname", workdir);
    my_rename(subdir, newdir);
    verify_inum(newdir, &sb2);
    sprintf(file2, "%s/file1", newdir);
    verify_inum(file2, &sb1);
    sprintf(link, "%s/newdirname/link", workdir);
    verify_inum(link, &sb1);

    /* Test moving up in the tree */
    my_rename(file2,file1);
    verify_inum(file1, &sb1);

    my_unlink(link);

    my_rmdir(newdir);
	
}		


void test_readdir(char *workdir) {
    int i, fd, direntnum;
    char filename[1024];
    DIR *dirp;
    struct dirent *de;
    char *seen=malloc(testfiles);
	
    printf("Making a bunch of directory entries...\n");
    /* make a bunch of test files */
	
    for(i=0; i<testfiles; i++) {
	snprintf(filename, sizeof(filename), "%s/dirent%d", 
		 workdir, i);
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
    errno=0;
    while ((de=readdir(dirp))) {
	/* printf("%s\n", de->d_name); */
	if (!strncmp(de->d_name, "rent", 4)) {
	    printf("readdir is returning truncated directory entries.  This is a C library or building bug.  Aborting.\n");
	    exit(1);
	}

	if (sscanf(de->d_name, "dirent%d", &direntnum)==1) {
	    seen[direntnum]=1;
	    sprintf(filename, "%s/%s", workdir, de->d_name);
	    if (unlink(filename)) {
		printf("unlink(%s) failed: %s\n",
		       filename, strerror(errno));
		exit(1);
	    }
			
	}
	errno=0;
    }
#if 1
    if (errno) {
	perror("readdir");
	exit(1);
    }
#endif
    closedir(dirp);

    /* Make sure we saw all the expected files */
    for (i=0; i<testfiles; i++) {
	if (!seen[i]) {
	    printf("File dirent%d wasn't seen in the directory listing.\n", i);
	    exit(1);
	}
    }

    free(seen);
	
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
	

/* hard links are also tested a bunch in test_rename */
void test_link(char *workdir) {
    char file1[1024], file2[1024], file3[1024];
    struct stat sb;
    
    printf("Testing link\n");

    sprintf(file1, "%s/original", workdir);
    sprintf(file2, "%s/link", workdir);

    my_create(file1);
    my_stat(file1, &sb);
    my_link(file1, file2);
    verify_inum(file2, &sb);
    my_unlink(file2);
    verify_inum(file1, &sb);

    /* Test deletion of the original file after a hard link has been
       created */
    my_link(file1, file2);
    my_unlink(file1);
    verify_inum(file2, &sb);
    /* Make sure the original file really isn't accessible */
    verify_no_exist(file1);
    my_unlink(file2);
    verify_no_exist(file2);
    
    /* Try renaming a hard link */
    my_create(file1);
    my_stat(file1, &sb);
    my_link(file1, file2);
    sprintf(file3, "%s/file3", workdir);
    my_rename(file2, file3);
    verify_inum(file3, &sb);
    verify_no_exist(file2);
    
    my_unlink(file3);
    verify_no_exist(file3);
    
    my_unlink(file1);

}


void test_write(char *workdir, char *nfshost, char *hosttemp, 
		char *workdirbasename) {
    char infilename[1024], outfilename[1024], buffer[64*1024];
    int infd, outfd, written, got, res;
  
    printf("testing write\n");
  
    snprintf(infilename, sizeof(infilename), "%s/%s",
	     localtemp, testfile);
  
    snprintf(outfilename, sizeof(outfilename), "%s/%s.written", 
	     workdir, testfile);
  
    infd=open(infilename, O_RDONLY);
    if (infd < 0) {
	printf("open(%s) failed: %s\n", infilename,
	       strerror(errno));
	exit(1);
    }
  
    outfd=open(outfilename, O_WRONLY|O_CREAT, 0666);
    if (outfd < 0) {
	printf("open(%s) for writing failed: %s\n", outfilename,
	       strerror(errno));
	exit(1);
    }
  
    while ((got=read(infd, buffer, sizeof(buffer))) > 0) {
	written=write(outfd, buffer, got);
	if (written < 0) {
	    printf("Error writing to %s: %s\n",
		   outfilename, strerror(errno));
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
	       infilename, strerror(errno));
	exit(1);
    }
  
    close(infd);
  
    if (close(outfd)) {
	printf("error closing %s: %s\n", outfilename,
	       strerror(errno));
	exit(1);
    }
  
  
    snprintf(buffer, sizeof(buffer), 
	     "on %s /usr/bin/diff %s/%s %s/%s/%s.written",
	     nfshost, 
	     hosttemp, testfile, 
	     hosttemp, workdirbasename, testfile);
  
    res=system(buffer);
  
    if (res) {
	printf("Non-zero return code from remote diff.\n");
	exit(1);
    }
  
    if (unlink(outfilename)) {
	printf("unlink(%s): %s\n", outfilename, strerror(errno));
	exit(1);
    }
  
}

void usage(char *prg) {
    printf("Usage: %s\n", prg);
    printf("\t[-o skipread] [-o skipwrite] [-o skipdir ] [-t tmpdir-on-nfshost]\n");
    printf("\t[-c num-files-to-create-in-dir-test ]\n");
    printf("\t[-l local-test-dir] [-f test-filename] nfshost mountpoint\n");
    printf("\n");
    printf("    tmpdir-on-nfshost defaults to '%s'\n", DEFAULT_HOSTTEMP);
    printf("    local-test-dir defaults to the current working directory.\n");
    printf("    test-filename defaults to '%s'\n", DEFAULT_TESTFILE);
    printf("    -c defaults to 1000\n");
    exit(1);
}


int main(int argc, char **argv) {
    char workdir[1024];
    char workdirbasename[1024];
    char *nfsdir, *nfshost, *hosttemp=DEFAULT_HOSTTEMP;
    int c;
    int skipread=0, skipwrite=0, skipdir=0;

    while (1) {
	c=getopt(argc, argv, "o:t:l:f:c:");
	printf("c is %d\n", c);
	if (c == -1)
	    break;

	switch(c) {
	case '?':
	    usage(argv[0]);
	    /* notreached */
	case 'o':
	    if (!strcmp(optarg, "skipread")) {
		skipread=1;
	    } else if (!strcmp(optarg, "skipwrite")) {
		skipwrite=1;
	    } else if (!strcmp(optarg, "skipdir")) {
		skipdir=1;
	    } else {
		printf("Unrecognized -o option: %s\n", optarg);
		usage(argv[0]);
	    }
	    break;
	case 't':
	    hosttemp=strdup(optarg);
	    break;
	case 'l':
	    localtemp=strdup(optarg);
	    break;
	case 'f':
	    testfile=strdup(optarg);
	    break;
	case 'c':
	    testfiles=atoi(optarg);
	    break;
	}
    }
  
    if (argc-optind != 2) {
	printf("Invalid number of required arguments\n");
	usage(argv[0]);
    }

    nfshost=argv[optind++];
    nfsdir=argv[optind++];

    /* Begin */

    test_statfs(nfsdir);

    /* Start with a fresh work area */
    sprintf(workdirbasename, "tmp%d", getpid());
    sprintf(workdir, "%s/%s", nfsdir, workdirbasename);
    printf("workdir is %s\n", workdir);

    test_mkdir(workdir);

    test_create(workdir);

    test_setattr(workdir);

    test_link(workdir);

    if (!skipread) 
	test_read(nfsdir);

    if (!skipwrite)
	test_write(workdir, nfshost, hosttemp, workdirbasename);

    test_rename(workdir);

    if (!skipdir)
	test_readdir(workdir);
  
    test_remove(workdir);
  
    test_rmdir(workdir);

    return 0;

}
