# $Id: Makefile,v 1.11 2003/03/19 16:54:16 dancy Exp $
# This makefile assumes that cygwin has been installed (ie, it assumes
# GNU make).

LISPEXE = "/c/Program Files/acl62/mlisp"

default: compile

### used on Windows:
compile: FORCE
	rm -fr nfs
	@echo '(setq excl::*break-on-warnings* t)' >> b.tmp
	@echo '(compile-file-if-needed "ntservice/ntservice.cl")' >> b.tmp
	@echo '(load "loadem.cl")' >> b.tmp
	@echo '(buildit)' >> b.tmp
	@echo '(exit 0)' >> b.tmp
	$(LISPEXE) +s b.tmp
	@rm -f b.tmp
	if test -f nfs.cfg; then cp -p nfs.cfg nfs; fi

version = $(shell grep nfsd-version nfs.cl | sed -e 's,.*"\([0-9.]*\)".*,\1,')
nfs_source_files = Makefile ChangeLog *.cl *.txt nfs.cfg.sample nfs.ico
ntservice_source_files = ntservice/ChangeLog ntservice/*.cl ntservice/*.txt

binzip = dists/nfs-$(version).zip
srczip = dists/nfs-$(version)-src.zip

dist: FORCE
### make binary dist:
	rm -f $(binzip)
	rm -fr nfs-$(version)
	cp -rp nfs nfs-$(version)
	rm -f nfs-$(version)/nfs.cfg
	cp -p nfs.cfg.sample nfs-$(version)
	cp -p readme.txt nfs-$(version)
	cp -p binary-license.txt nfs-$(version)
	zip -r $(binzip) nfs-$(version)
	rm -fr nfs-$(version)
### make source dist:
	rm -f $(srczip)
	rm -fr nfs-$(version)-src
	mkdir nfs-$(version)-src
	cp -p $(nfs_source_files) nfs-$(version)-src
	mkdir nfs-$(version)-src/ntservice
	cp -p $(ntservice_source_files) nfs-$(version)-src/ntservice
	zip -r $(srczip) nfs-$(version)-src
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
