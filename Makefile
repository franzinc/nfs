# $Id: Makefile,v 1.5 2002/07/24 19:25:25 layer Exp $
# This makefile assumes that cygwin has been installed.

LISPEXE = "/c/Program Files/acl62/mlisp"
LISPDXL = mlisp

default: compile

### used on Windows:
compile: FORCE
	rm -fr nfs
	@echo '(compile-file-if-needed "ntservice/ntservice.cl")' >> b.tmp
	@echo '(load "loadem.cl")' >> b.tmp
	@echo '(buildit)' >> b.tmp
	@echo '(exit 0)' >> b.tmp
	$(LISPEXE) +s b.tmp -I $(LISPDXL)

### used on UNIX:
zip: FORCE
	rm -f nfs.zip
	zip nfs.zip *.cl ChangeLog nfs.cfg.sample *.txt

clean: FORCE
	rm -rf *.out *.fasl */*.fasl *.zip *.tmp nfs

install: FORCE
	rm -fr /c/nfs.old
	-mv /c/nfs /c/nfs.old
	mkdir /c/nfs
	cp -rp nfs/*.* /c/nfs

FORCE:
