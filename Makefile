# $Id: Makefile,v 1.4 2002/02/12 22:20:50 layer Exp $
# This makefile assumes that cygwin has been installed.

LISPEXE = /d/acl62b/src/cl/src/lisp
LISPDXL = dcl
### these would be a good choice for the standard installation of ACL 6.1:
#LISPEXE = "/c/program files/acl61/mlisp"
#LISPDXL = mlisp

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
