/*
** genfiles :: generate test files for performance testing
**
** usage: genfiles.exe count
**
** `count' is the number of files to create in the current directory.
** They are named "file%d" where %d is a sequence number from 0 to count-1.
*/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

void generate(char *filename)
{
    int fd, n;
    const char *contents = "nothing to see here, move along.\n";
    if ((fd = open(filename, O_CREAT|O_RDWR, 0644)) < 0) {
	fprintf(stderr, "Could not create %s: %s\n", filename,
	    strerror(errno));
	exit(1);
    }
    if ((n = write(fd, contents, strlen(contents))) < 0) {
	fprintf(stderr, "write failed: %d: %s\n", n, strerror(errno));
	exit(1);
    }
    close(fd);
}

int main(int argc, char **argv)
{
    int i;
    int max;
    char filename[1024];

    if (argc != 2) {
	fprintf(stderr, "usage: %s count\n", argv[0]);
	exit(1);
    }

    max = atoi(argv[1]);

    for (i = 0; i < max; i++) {
	sprintf(filename, "file%d", i);
	generate(filename);
    }
    exit(0);
}
