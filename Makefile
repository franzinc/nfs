# $Id: Makefile,v 1.19 2004/02/03 20:58:12 dancy Exp $
# This makefile assumes that cygwin has been installed (ie, it assumes
# GNU make).

LISPEXE = "/c/Program Files/acl62/mlisp"
#LISPEXE = "/c/Program Files/acl70pb/mlisp8"
#LISPEXE = "/c/Program Files/acl70pb/mlisp"
MAKENSIS = "/c/Program Files/NSIS/makensis.exe"
version = $(shell grep nfsd-version nfs.cl | sed -e 's,.*"\([0-9.]*\)".*,\1,')


default: build

demo: build-demo

build: build-prologue build-normal-cmd build-epilogue

build-demo: build-prologue build-demo-cmd build-epilogue

build-prologue:
	rm -fr nfs
	@echo '(setq excl::*break-on-warnings* t)' >> b.tmp
	@echo '(load "loadem.cl")' >> b.tmp

build-normal-cmd:
	@echo '(buildit)' >> b.tmp

build-demo-cmd:
	@echo '(buildit :demo 30)' >> b.tmp

build-epilogue:
	@echo '(exit 0)' >> b.tmp
	$(LISPEXE) +B +cn +s b.tmp
	@rm -f b.tmp
	if test -f nfs.cfg; then cp -p nfs.cfg nfs; fi
	cd configure && $(MAKE)

installer-common:
	rm -f nfs/nfs.cfg
	rm -fr nfs/configure
	cp -pr configure/configure nfs

installer: installer-common
	$(MAKENSIS) /V1 /DVERSION=$(version) nfs.nsi

installer-demo: installer-common
	$(MAKENSIS) /V1 /DVERSION=$(version)-demo nfs.nsi	

dists: dist dist-demo

dist: build installer

dist-demo: build-demo installer-demo
# Alias
demo-dist: dist-demo


nfs_bin_files = nfs.cfg.sample readme.txt binary-license.txt access-control.txt
nfs_source_files = Makefile ChangeLog *.cl *.txt nfs.cfg.sample nfs.ico
ntservice_source_files = ntservice/ChangeLog ntservice/*.cl ntservice/*.txt

binzip = dists/nfs-$(version).zip
srczip = dists/nfs-$(version)-src.zip

clean: FORCE
	rm -rf *.out *.fasl */*.fasl *.zip *.tmp nfs *~ .*~ ntservice/testapp

real-clean: clean
	rm -fr configure/configure

# Assumes cygwin mounted c:\ on /c
install: FORCE
	rm -fr /c/nfs.old
	-mv /c/nfs /c/nfs.old
	mkdir /c/nfs
	cp -rp nfs/*.* /c/nfs

FORCE:
