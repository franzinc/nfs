# $Id: Makefile,v 1.7 2002/09/20 22:08:31 layer Exp $
# This makefile assumes that cygwin has been installed (ie, it assumes
# GNU make).

LISPEXE = "/c/Program Files/acl62/mlisp"

default: compile

### used on Windows:
compile: FORCE
	rm -fr nfs
	@echo '(compile-file-if-needed "ntservice/ntservice.cl")' >> b.tmp
	@echo '(load "loadem.cl")' >> b.tmp
	@echo '(buildit)' >> b.tmp
	@echo '(exit 0)' >> b.tmp
	$(LISPEXE) +s b.tmp
	@rm -f b.tmp

version = $(shell grep nfsd-version nfs.cl | sed -e 's,.*"\([0-9.]*\)".*,\1,')
source_files = *.cl ChangeLog nfs.cfg.sample *.txt

dist: FORCE
	rm -f nfs-$(version).zip
	rm -fr nfs-$(version)
	cp -rp nfs nfs-$(version)
	/c/winzip/wzzip.exe -ex -rP -yb nfs-$(version).zip nfs-$(version)
	rm -fr nfs-$(version)
	rm -f nfs-$(version)-src.zip
	rm -fr nfs-$(version)-src
	mkdir nfs-$(version)-src
	cp -p $(source_files) nfs-$(version)-src
	/c/winzip/wzzip.exe -ex -rP -yb nfs-$(version)-src.zip \
		nfs-$(version)-src
	rm -fr nfs-$(version)-src

clean: FORCE
	rm -rf *.out *.fasl */*.fasl *.zip *.tmp nfs *~ .*~

# Assumes cygwin mounted c:\ on /c
install: FORCE
	rm -fr /c/nfs.old
	-mv /c/nfs /c/nfs.old
	mkdir /c/nfs
	cp -rp nfs/*.* /c/nfs

FORCE:
