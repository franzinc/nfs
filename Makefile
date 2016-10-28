###############################################################################
# NFS makefile (requires Cygwin and GNU make)
#
# Rules of note:
#
# - make release candidate 2:
#   $ make all release_suffix=rc2
#
# - make demo and non-demo versions:
#   $ make clean dist dist-demo LISPDIR=/c/acl90.patched
#
# - remove tag to build without changing version #:
#   $ make delete_tag
#
###############################################################################

# chosen because it seems to work on thor and for spr42738
ACL_BUILD_ACLMALLOC_HEAP_START = 0x8ab0000

DO_MAKEFILE_LOCAL := $(shell if test -f Makefile.local; then echo yes; fi)

ifeq ($(DO_MAKEFILE_LOCAL),yes)
include Makefile.local
endif

LISPDIR ?= /c/acl100.patched
LISPEXE = $(LISPDIR)/mlisp
MAKENSIS ?= "/cygdrive/c/Program Files (x86)/NSIS/makensis.exe"

version := $(shell grep 'defvar .nfsd-version' nfs-common.cl | sed -e 's,.*"\([a-z0-9.]*\)".*,\1,')
major-version := $(shell echo $(version) | sed -e 's/\(.*\)\.[0-9]*/\1/')

default: build

# use `dists' because `dist dist-demo' does not work.. see comment below
# near `dists' for why.
all: clean dists

MODULES = date:master demoware:master

prereqs: FORCE
	@bin/verify_modules.sh $(MODULES)

ifdef release_suffix
tag_name = nfs-$(version)-$(release_suffix)
else
tag_name = nfs-$(version)
endif

check_tag_name: FORCE
	@if git tag | grep -q '^$(tag_name)$$'; then \
	    echo ERROR: git tag $(tag_name) already exists; \
	    exit 1; \
	else \
	    echo '**** TAG: $(tag_name)'; \
	fi

commit-id.cl: FORCE
	echo -n '(defvar *nfsd-commit-id* "' > commit-id.cl
	echo -n `git log -n1 --pretty=format:%H HEAD` >> commit-id.cl
	echo -n '")' >> commit-id.cl

tag: FORCE
	git.sh tag -a -m $(tag_name) $(FORCE) $(tag_name) HEAD
	@echo NOTE: do this to push the tag:
	@echo git.sh push origin $(tag_name)

delete_tag: FORCE
	git.sh tag -d $(tag_name)
	git.sh push origin :refs/tags/$(tag_name)

build: check_cpp
	@$(MAKE) $(MFLAGS) do_build

check_cpp: FORCE
	@if ! which cpp > /dev/null 2>&1; then \
	    echo Error: cpp not installed.; \
	    exit 1; \
	fi

build-demo: FORCE
	@$(MAKE) $(MFLAGS) DEMOWARE=xxx do_build

do_build: prereqs rpc commit-id.cl FORCE
# make sure the demo and non-demo versions do not share fasls:
	rm -fr nfs *.fasl b.tmp build.out
	echo '(dribble "build.out")' >> b.tmp
	echo '(setq excl::*break-on-warnings* t)' >> b.tmp
ifdef DEMOWARE
	echo '(push :nfs-demo *features*)' >> b.tmp
endif
	echo '(load "load.cl")' >> b.tmp
	echo '(buildit)' >> b.tmp
	echo '(dribble)' >> b.tmp
	echo '(exit 0)' >> b.tmp
	env ACL_BUILD_ACLMALLOC_HEAP_START=$(ACL_BUILD_ACLMALLOC_HEAP_START) \
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

EXE     = dists/setup-nfs-$(version).exe
DEMOEXE = dists/setup-nfs-$(version)-demo.exe

installer: installer-common
	$(MAKENSIS) /V1 /DVERSION=$(version) /DVERSION2=$(version) nfs.nsi
	sha256sum $(EXE) > $(EXE).sha256sum

installer-demo: installer-common
	$(MAKENSIS) /V1 /DNFSDEMO=true \
		/DVERSION="$(version) Demo" \
		/DVERSION2=$(version)-demo \
		nfs.nsi
	sha256sum $(DEMOEXE) > $(DEMOEXE).sha256sum

# Each build runs in a separate make because there are some
# shared dependencies.. and make will merge them .. and we don't
# want that.  Specifically, ``installer-common'' will be run once
# instead of twice.
#
# `clean' added to make sure that configure is really rebuilt.  There
# was evidence in June of 2011 that this wasn't happening.  -Kevin/Elliott
dists: clean check_tag_name
	@if grep -q '^(pushnew :nfs-' load.cl && ! grep -q 'nfsd-version.*beta' nfs-common.cl; then \
	    echo ERROR: debugging features enabled for production build; \
	    exit 1; \
	fi
	$(MAKE) $(MFLAGS) dist
	$(MAKE) $(MFLAGS) dist-demo
	$(MAKE) $(MFLAGS) tag

dist: build installer

dist-demo: build-demo installer-demo
# Alias
demo-dist: dist-demo

DEST = ../nfs-outgoing/$(tag_name)

publish: FORCE
	mkdir -p $(DEST)
	cp -p $(EXE)               $(DEST)
	cp -p $(EXE).sha256sum     $(DEST)
	cp -p $(DEMOEXE)           $(DEST)
	cp -p $(DEMOEXE).sha256sum $(DEST)

###############################################################################
# testing

# Needs the tirpc Cygwin package on Windows

exe := $(shell test -d c:/ && echo .exe)

hammer_deps = \
	test/hammernfs-libs/mount_xdr.c \
	test/hammernfs-libs/mount_clnt.c \
        test/hammernfs-libs/nfs_clnt.c \
	test/hammernfs-libs/nfs_xdr.c

# $@ is the target and $< is the first dependency.
define build_test_program
	cc -O -o $@ \
	  $(shell uname | grep -q CYGWIN && echo -I/usr/include/tirpc) \
	  $< \
	  $(hammer_deps) \
	  $(shell uname | grep -q CYGWIN && echo -ltirpc)
endef

hammernfs$(exe): test/hammernfs.c $(hammer_deps)
	$(build_test_program)

test-conn-reset$(exe): test/test-conn-reset.c $(hammer_deps)
	$(build_test_program)

perftest: FORCE
	test/performance.sh test/performance.log.$(version)
	$(LISPEXE) -L test/performance.cl

testnfs: test/testnfs.c
	cc -O -o testnfs test/testnfs.c

###############################################################################
# misc

echo_version: FORCE
	@echo $(version)

clean: FORCE
	rm -rf *.out *.fasl */*.fasl *.zip *.tmp nfs *~ .*~
	rm -f gen-nfs-*.cl mount-*.cl sunrpc-common.cl nlm-*.cl nsm-*.cl 
	rm -f portmap-*.cl hammernfs$(exe) test-conn-reset$(exe) 
	$(MAKE) -C configure clean

tags: FORCE
	find . -name "*.[ch]" | xargs etags

FORCE:
