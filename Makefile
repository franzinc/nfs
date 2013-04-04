# This makefile assumes that cygwin has been installed (ie, it assumes
# GNU make).

DO_MAKEFILE_LOCAL := $(shell if test -f Makefile.local; then echo yes; fi)

ifeq ($(DO_MAKEFILE_LOCAL),yes)
include Makefile.local
endif

ifndef LISPDIR
LISPDIR = /c/acl90.patched
endif

LISPEXE=$(LISPDIR)/mlisp

ifndef MAKENSIS
MAKENSIS = "/c/Program Files (x86)/NSIS/makensis.exe"
endif

version := $(shell grep 'defvar .nfsd-version' nfs-common.cl | sed -e 's,.*"\([a-z0-9.]*\)".*,\1,')

default: build

# use `dists' because ``dist dist-demo'' does not work.. see comment below
# near `dists' for why.
all: clean dists

GIT_REPO_BASE=$(shell dirname `git remote show origin | grep URL | awk '{print $$2}'`)

MODULES = .:master date:acl90 demoware:master

prereqs: FORCE
	@sh verify_modules.sh $(MODULES)

tag_name = nfs$(version)$(release_suffix)

tag: FORCE
ifndef release_suffix
	$(error release_suffix is not defined.)
endif
	./git.sh tag -a -m $(tag_name) $(FORCE) $(tag_name) HEAD
	@echo NOTE: do this to push the tag:
	@echo ./git.sh push origin $(tag_name)

build: check_cpp
	@$(MAKE) $(MFLAGS) do_build

check_cpp: FORCE
	@if ! which cpp > /dev/null 2>&1; then \
	    echo Error: cpp not installed.; \
	    exit 1; \
	fi

build-demo: FORCE
	@$(MAKE) $(MFLAGS) DEMOWARE=xxx do_build

do_build: prereqs rpc FORCE
# make sure the demo and non-demo versions do not share fasls:
	rm -fr nfs *.fasl b.tmp build.out
	echo '(dribble "build.out")' >> b.tmp
	echo '(setq excl::*break-on-warnings* t)' >> b.tmp
ifdef DEMOWARE
	echo '(push :nfs-demo *features*)' >> b.tmp
endif
	echo '(load "loadem.cl")' >> b.tmp
	echo '(buildit)' >> b.tmp
	echo '(dribble)' >> b.tmp
	echo '(exit 0)' >> b.tmp
	$(LISPEXE) +B +cn +s b.tmp -batch
	@rm -f b.tmp
	if test -f nfs.cfg; then cp -p nfs.cfg nfs; fi
	$(MAKE) -C configure

rpc: FORCE
	echo '(load (compile-file-if-needed "rpcgen"))' > b.tmp
	echo '(dolist (file (list "sunrpc.x" "portmap.x" "mount.x" "nlm.x" "nsm.x")) (write-line file) (rpcgen file))' >> b.tmp
	echo '(rpcgen "nfs.x" :out-base "gen-nfs")' >> b.tmp
	echo '(exit 0)' >> b.tmp
	$(LISPEXE) +B +cn +s b.tmp -batch
	rm b.tmp

# Forcibly rebuild the configure program
configure: FORCE
	@rm -fr configure/configure
	$(MAKE) -C configure 'LISPDIR=$(LISPDIR)'

installer-common: FORCE
	rm -f nfs/nfs.cfg
	rm -fr nfs/configure
	cp -pr configure/configure nfs
	mkdir -p dists

installer: installer-common
	$(MAKENSIS) /V1 /DVERSION=$(version) /DVERSION2=$(version) nfs.nsi

installer-demo: installer-common
	$(MAKENSIS) /V1 /DNFSDEMO=true \
		/DVERSION="$(version) Demo" \
		/DVERSION2=$(version)-demo \
		nfs.nsi

# Each build runs in a separate make because there are some
# shared dependencies.. and make will merge them .. and we don't
# want that.  Specifically, ``installer-common'' will be run once
# instead of twice.
#
# `clean' added to make sure that configure is really rebuilt.  There
# was evidence in June of 2011 that this wasn't happening.  -Kevin/Elliott
dists: clean
ifndef release_suffix
	$(error release_suffix is not defined.)
endif
	$(MAKE) $(MFLAGS) dist
	$(MAKE) $(MFLAGS) dist-demo
	$(MAKE) $(MFLAGS) tag

dist: build installer

dist-demo: build-demo installer-demo
# Alias
demo-dist: dist-demo

###############################################################################
# testing

HAMMERNFS_LIBS = $(shell uname | grep -q CYGWIN && echo -lrpc)

hammernfs: hammernfs.c hammernfs-libs/mount_clnt.c hammernfs-libs/nfs_clnt.c
	cc -O -o hammernfs \
			hammernfs.c 	\
			hammernfs-libs/mount_xdr.c \
			hammernfs-libs/mount_clnt.c \
			hammernfs-libs/nfs_clnt.c \
			hammernfs-libs/nfs_xdr.c \
			hammernfs-libs/compat.c \
			$(HAMMERNFS_LIBS)

###############################################################################
# misc

clean: FORCE
	rm -rf *.out *.fasl */*.fasl *.zip *.tmp nfs *~ .*~
	rm -f gen-nfs-*.cl mount-*.cl sunrpc-common.cl nlm-*.cl nsm-*.cl 
	rm -f portmap-*.cl
	$(MAKE) -C configure clean

FORCE:

