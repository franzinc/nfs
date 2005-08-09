# $Id: Makefile,v 1.49 2005/08/09 17:22:29 dancy Exp $
# This makefile assumes that cygwin has been installed (ie, it assumes
# GNU make).

### IMPORTANT: the nightly builds the product need to use a local copy and
###	       and not one in /c/Program Files/...  only change the ../aclxxx
###	       when the build machine's copy is updated.
LISPDIR := $(shell if test -f ../acl70/mlisp; then echo ../acl70; else echo '"/c/Program Files/acl70"'; fi)
LISPEXE=$(LISPDIR)/mlisp

MAKENSIS = "/c/Program Files/NSIS/makensis.exe"
version = $(shell grep 'defvar .nfsd-version' nfs.cl | sed -e 's,.*"\([a-z0-9.]*\)".*,\1,')

default: build

# use `dists' because ``dist dist-demo'' does not work.. see comment below
# near `dists' for why.
all: clean dists

prereqs: FORCE
	@if test ! -d date; then cvs co -r acl70 date; fi
	@if test ! -d demoware; then cvs co demoware; fi

build: FORCE
	@$(MAKE) $(MFLAGS) do_build

build-demo: FORCE
	@$(MAKE) $(MFLAGS) DEMOWARE=xxx do_build

do_build: prereqs FORCE
# make sure the demo and non-demo versions do not share fasls:
	rm -fr nfs *.fasl b.tmp
	echo '(setq excl::*break-on-warnings* t)' >> b.tmp
ifdef DEMOWARE
	echo '(push :nfs-demo *features*)' >> b.tmp
endif
	echo '(load "loadem.cl")' >> b.tmp
	echo '(buildit)' >> b.tmp
	echo '(exit 0)' >> b.tmp
	$(LISPEXE) +B +cn +s b.tmp -batch
	@rm -f b.tmp
	if test -f nfs.cfg; then cp -p nfs.cfg nfs; fi
	$(MAKE) -C configure

# Forcibly rebuild the configure program
configure: FORCE
	@rm -fr configure/configure
	$(MAKE) -C configure 'LISPDIR=$(LISPDIR)'

installer-common: FORCE
	rm -f nfs/nfs.cfg
	rm -fr nfs/configure
	cp -pr configure/configure nfs

installer: installer-common
	$(MAKENSIS) /V1 /DVERSION=$(version) nfs.nsi

installer-demo: installer-common
	$(MAKENSIS) /V1 /DNFSDEMO=true /DVERSION=$(version)-demo nfs.nsi

# Each build runs in a separate make because there are some
# shared dependencies.. and make will merge them .. and we don't
# want that.  Specifically, ``installer-common'' will be run once
# instead of twice.
dists: FORCE
	$(MAKE) $(MFLAGS) dist
	$(MAKE) $(MFLAGS) dist-demo

dist: build installer

dist-demo: build-demo installer-demo
# Alias
demo-dist: dist-demo

update_cobweb: FORCE
	scp -p dists/setup-nfs-3.0.exe \
	       layer@cobweb:/www/nfsforwindows/prod/downloadables

update_demo_cobweb: FORCE
	scp -p dists/setup-nfs-3.0-demo.exe \
	       layer@cobweb:/www/nfsforwindows/prod/downloadables

## the following rule is run nightly on hobart to produce a new demo
## version that expires 30 days into the future.  See nightly.bat.
nightly: dist-demo update_demo_cobweb

###############################################################################
## source distribution
###############################################################################

nfs_source_files = Makefile ChangeLog *.cl *.txt \
		nfs.cfg.default nfs.ico nfs.nsi
configure_source_files = configure/Makefile configure/*.cl \
			configure/*.txt configure/configure.lpr \
			configure/*.bil 

srczip = dists/nfs-$(version)-src.zip

srcdist: FORCE
	rm -f $(srczip)
	rm -fr nfs-$(version)-src
	mkdir nfs-$(version)-src \
	 nfs-$(version)-src/configure
	cp -p $(nfs_source_files) nfs-$(version)-src
	cp -p $(configure_source_files) nfs-$(version)-src/configure
	zip -r $(srczip) nfs-$(version)-src
	rm -fr nfs-$(version)-src

###############################################################################

# Assumes cygwin mounted c:\ on /c
install: FORCE
	rm -fr /c/nfs.old
	-mv /c/nfs /c/nfs.old
	mkdir /c/nfs
	cp -rp nfs/*.* /c/nfs

###############################################################################
# testing

hammernfs: hammernfs.c hammernfs-libs/mount_clnt.c \
			hammernfs-libs/nfs_prot_clnt.c
	cc -o hammernfs hammernfs.c hammernfs-libs/mount_xdr.c \
					hammernfs-libs/nfs_prot_clnt.c \
					hammernfs-libs/nfs_prot_xdr.c \
			-lrpc 

hammernfs-libs: 

hammernfs-libs-dir:
	mkdir -p hammernfs-libs

hammernfs-libs/mount.x: hammernfs-libs-dir
	ln -sdf /usr/include/rpcsvc/mount.x hammernfs-libs/mount.x

hammernfs-libs/nfs_prot.x: hammernfs-libs-dir
	ln -sdf /usr/include/rpcsvc/nfs_prot.x hammernfs-libs/nfs_prot.x

hammernfs-libs/mount_clnt.c: hammernfs-libs/mount.x
	(cd hammernfs-libs && rpcgen mount.x)

hammernfs-libs/nfs_prot_clnt.c: hammernfs-libs/nfs_prot.x
	(cd hammernfs-libs && rpcgen nfs_prot.x)

###############################################################################
# misc

clean: FORCE
	rm -rf *.out *.fasl */*.fasl *.zip *.tmp nfs *~ .*~
	$(MAKE) -C configure clean

FORCE:
