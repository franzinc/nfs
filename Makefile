# $Id: Makefile,v 1.38 2004/11/02 23:17:19 layer Exp $
# This makefile assumes that cygwin has been installed (ie, it assumes
# GNU make).

### IMPORTANT: the nightly builds the product need to use a local copy and
###	       and not one in /c/Program Files/...  only change the ../aclxxx
###	       when the build machine's copy is updated.
LISPDIR := $(shell if test -d ../acl70pf; then echo ../acl70pf; else echo '"/c/Program Files/acl70"'; fi)
LISPEXE=$(LISPDIR)/mlisp

MAKENSIS = "/c/Program Files/NSIS/makensis.exe"
version = $(shell grep 'defvar .nfsd-version' nfs.cl | sed -e 's,.*"\([0-9.]*\)".*,\1,')

default: build

all: clean dist demo-dist

demo: build-demo

build: build-prologue build-normal-cmd build-epilogue

build-demo: build-prologue build-demo-cmd build-epilogue

build-prologue:
	rm -fr nfs
# Hack.. since the configure program shares some fasls
# with the nfs server.. but they're using incompatible
# lisps.
	rm -f *.fasl
	rm -f b.tmp
	@echo '(setq excl::*break-on-warnings* t)' >> b.tmp
	@echo '(load "loadem.cl")' >> b.tmp

build-normal-cmd:
	@echo '(buildit)' >> b.tmp

build-demo-cmd:
	@echo '(buildit :demo 30)' >> b.tmp

build-epilogue:
	@echo '(exit 0)' >> b.tmp
	$(LISPEXE) +B +cn +s b.tmp -batch
	@rm -f b.tmp
	if test -f nfs.cfg; then cp -p nfs.cfg nfs; fi
	#$(MAKE) -C configure 'LISPDIR=$(LISPDIR)'
	$(MAKE) -C configure

# Forcibly rebuild the configure program
configure: FORCE
	@rm -fr configure/configure
	$(MAKE) -C configure 'LISPDIR=$(LISPDIR)'

installer-common:
	rm -f nfs/nfs.cfg
	rm -fr nfs/configure
	cp -pr configure/configure nfs

installer: installer-common
	$(MAKENSIS) /V1 /DVERSION=$(version) nfs.nsi

installer-demo: installer-common
	$(MAKENSIS) /V1 /DVERSION=$(version)-demo nfs.nsi	

# Each build runs in a separate make because there are some
# shared dependencies.. and make will merge them .. and we don't
# want that.
dists: FORCE
	$(MAKE) dist
	$(MAKE) dist-demo

dist: build installer

dist-demo: build-demo installer-demo
# Alias
demo-dist: dist-demo

update_cobweb: FORCE
	cp -p dists/setup-nfs-2.0.exe //cobweb/nfsforwindows/prod/downloadables

update_demo_cobweb: FORCE
	scp -p dists/setup-nfs-2.0-demo.exe \
		layer@cobweb:/www/nfsforwindows/prod/downloadables

## the following rule is run nightly on hobart to produce a new demo
## version that expires 30 days into the future.  See nightly.bat.
nightly: dist-demo update_demo_cobweb

nfs_source_files = Makefile ChangeLog *.cl *.txt \
		nfs.cfg.default nfs.ico nfs.nsi
ntservice_source_files = ntservice/ChangeLog ntservice/*.cl ntservice/*.txt
configure_source_files = configure/Makefile configure/*.cl \
			configure/*.txt configure/configure.lpr \
			configure/*.bil 

srczip = dists/nfs-$(version)-src.zip

srcdist: FORCE
	rm -f $(srczip)
	rm -fr nfs-$(version)-src
	mkdir nfs-$(version)-src \
	 nfs-$(version)-src/ntservice \
	 nfs-$(version)-src/configure
	cp -p $(nfs_source_files) nfs-$(version)-src
	cp -p $(ntservice_source_files) nfs-$(version)-src/ntservice
	cp -p $(configure_source_files) nfs-$(version)-src/configure
	zip -r $(srczip) nfs-$(version)-src
	rm -fr nfs-$(version)-src

clean: FORCE
	rm -rf *.out *.fasl */*.fasl *.zip *.tmp nfs *~ .*~ ntservice/testapp
	$(MAKE) -C configure clean

# Assumes cygwin mounted c:\ on /c
install: FORCE
	rm -fr /c/nfs.old
	-mv /c/nfs /c/nfs.old
	mkdir /c/nfs
	cp -rp nfs/*.* /c/nfs

FORCE:
