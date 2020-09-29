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
#   $ make delete_tag release_suffix=rc1
#
###############################################################################

# chosen because it seems to work on thor and for spr42738
#ACL_BUILD_ACLMALLOC_HEAP_START = 0x8ab0000

DO_MAKEFILE_LOCAL := $(shell if test -f Makefile.local; then echo yes; fi)

ifeq ($(DO_MAKEFILE_LOCAL),yes)
include Makefile.local
endif

NEWSDKDIR = /c/Program Files (x86)/Microsoft SDKs/Windows/v7.1A/bin
NEWSDK = $(shell if test -d "$(NEWSDKDIR)"; then echo yes; else echo no; fi)
ifeq ($(NEWSDK),yes)

SIGNTOOL = ficodesign

# Add the directory containing "signtool.exe" to PATH so scm-bin/ficodesign can find it
PATH := $(PATH):$(NEWSDKDIR)

CERT = c:/src/scm/acl10.1.32/src/cl/release-keys/windows/code-signing-certificate-2019-11-05.p12
CERTOK = $(shell if test -f "$(CERT)"; then echo yes; else echo no; fi)
ifeq ($(CERTOK),yes)
SIGNTOOL += /v /f $(CERT)
endif

endif

# The variable NFSWIDTH specifies a 64bit version;
VCREDIST ?= $(shell if test "$(NFSWIDTH)" = "64"; then echo x64; else echo x86; fi)
defaultlisp = $(shell if test "$(NFSWIDTH)" = "64"; then echo .64.patched; else echo .patched; fi)
LISPDIR ?= /c/acl10.1$(defaultlisp)
LISPEXE = $(LISPDIR)/mlisp
MAKENSIS ?= "/cygdrive/c/Program Files (x86)/NSIS/makensis.exe"

# The variable VER_SUFFIX specifies a modifier appended to the 
# name of the installer and to the name of the installed application.

version := $(shell ./anfs-version)

default: build

# use `dists' because `dist dist-demo' does not work.. see comment below
# near `dists' for why.
all:
	$(MAKE) $(MFLAGS) clean dists NFSWIDTH=64 NFSLISPBSW=yes VER_SUFFIX=-64
	$(MAKE) $(MFLAGS) clean dists
	$(MAKE) $(MFLAGS) tag

MODULES = demoware:master

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
	$(MAKE) $(MFLAGS) do_build

check_cpp: FORCE
	@if ! which cpp > /dev/null 2>&1; then \
	    echo Error: cpp not installed.; \
	    exit 1; \
	fi

build-demo: FORCE
	$(MAKE) $(MFLAGS) DEMOWARE=xxx do_build

ifdef ACL_BUILD_ACLMALLOC_HEAP_START
env = env ACL_BUILD_ACLMALLOC_HEAP_START=$(ACL_BUILD_ACLMALLOC_HEAP_START)
else
env = 
endif

do_build: prereqs commit-id.cl
# make sure the demo and non-demo versions do not share fasls:
	rm -fr nfs *.fasl b.tmp build.out
	echo '(dribble "build.out")' >> b.tmp
	echo '(setq excl::*break-on-warnings* t)' >> b.tmp
ifdef DEMOWARE
	echo '(push :nfs-demo *features*)' >> b.tmp
endif
# The variable NFSLISPBSW specifies byte swap in Lisp instead of machine instr.
ifdef NFSLISPBSW
	echo '(push :nfs-lisp-bsw *features*)' >> b.tmp
endif
	echo '(load "load.cl")' >> b.tmp
	echo '(buildit)' >> b.tmp
	echo '(dribble)' >> b.tmp
	echo '(exit 0)' >> b.tmp
	$(env) $(LISPEXE) +B +cn +s b.tmp -batch
	rm -f b.tmp
	if test -f nfs.cfg; then cp -p nfs.cfg nfs; fi
	rm -fr nfs/system-dlls
	if test ! -f nfs/vcredist_$(VCREDIST).exe; then \
	    cp -p "$(LISPDIR)/vcredist_$(VCREDIST).exe" nfs/; \
	fi
	$(MAKE) -C configure 'LISPDIR=$(LISPDIR)'

# Forcibly rebuild the configure program
configure: FORCE
	rm -fr configure/configure
	$(MAKE) -C configure 'LISPDIR=$(LISPDIR)'

installer-common: FORCE
	rm -f nfs/nfs.cfg
	rm -fr nfs/configure
	cp -pr configure/configure nfs
	mkdir -p dists

EXE     = dists/setup-nfs-$(version)$(VER_SUFFIX).exe
DEMOEXE = dists/setup-nfs-$(version)$(VER_SUFFIX)-demo.exe

ifeq ($(NFSWIDTH),64)
INSTALLDIR = AllegroNFS64
else
INSTALLDIR = AllegroNFS
endif

installer: installer-common
	$(MAKENSIS) /V1 /DINSTALLDIR=$(INSTALLDIR) /DVERSION=$(version)$(VER_SUFFIX) /DVERSION2=$(version)$(VER_SUFFIX) nfs.nsi
ifdef SIGNTOOL
ifneq ($(CERTOK),yes)
	@echo CERT is not setup properly; exit 1
endif
	$(SIGNTOOL) $(EXE)
endif
	sha256sum $(EXE) > $(EXE).sha256sum

installer-demo: installer-common
	$(MAKENSIS) /V1 /DNFSDEMO=true \
		/DVERSION="$(version) Demo" \
		/DVERSION2=$(version)$(VER_SUFFIX)-demo \
		nfs.nsi
ifdef SIGNTOOL
	$(SIGNTOOL) $(DEMOEXE)
endif
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
	test/hammernfs-libs/nfs_xdr.c \
	test/nfs-common.c

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

test-big-readdir-udp$(exe): test/test-big-readdir-udp.c $(hammer_deps)
	$(build_test_program)

test-nfs-low$(exe): test/test-nfs-low.c $(hammer_deps)
	$(build_test_program)

perftest: FORCE
	test/performance.sh test/performance.log.$(version)
	$(LISPEXE) -L test/performance.cl

testnfs: test/testnfs.c
	cc -O -o testnfs test/testnfs.c

results: FORCE
	@prev=; for v in $$(cd results; echo *); do \
	    if [ "$$prev" ]; then \
		test/results.cl $$prev $$v; \
	    fi; \
	    prev=$$v; \
	done

# The following 3 variables must be set externally to specify
# the current test machine configuration.
LOCAL_TEST_DIR ?= /home/tmp/layer/nfs.test
REMOTE_TEST_DIR ?= /c/tmp/nfs.test
TEST_HOST ?= thunder
TEST_NFSPATH = /net/$(TEST_HOST)/nfs.test

###### times for various iterations:
#   1 iteration  takes ~75 seconds.
# 240 iterations takes ~5.5 hours.
# 600 iterations takes ~13 hours.
STRESS_ITERATIONS ?= 600

# Each test gets progressively longer
runtests: testnfs test-nfs-low
	date
	./test-nfs-low $(TEST_HOST):/c
#	test/misc-tests.sh $(TEST_HOST) /net/$(TEST_HOST)/c
#   The above test does not always work because of Win10 permission issues.
	./test/misc-tests.sh $(TEST_HOST) $(TEST_NFSPATH) $(REMOTE_TEST_DIR)
	./testnfs -l $(LOCAL_TEST_DIR) -t $(REMOTE_TEST_DIR) \
		$(TEST_HOST) $(TEST_NFSPATH)
	./test/bigfile-test.sh $(LOCAL_TEST_DIR) $(TEST_NFSPATH) 
	./test/stress-test.sh $(LOCAL_TEST_DIR) $(TEST_NFSPATH) \
		$(STRESS_ITERATIONS)
	date

###############################################################################
# misc

echo_version: FORCE
	@echo $(version)

clean: FORCE
	rm -rf *.out *.fasl */*.fasl *.zip *.tmp nfs *~ .*~
	rm -f gen-nfs-*.cl mount-*.cl sunrpc-common.cl nlm-*.cl nsm-*.cl 
	rm -f portmap-*.cl hammernfs$(exe) test-conn-reset$(exe) test-big-readdir-udp$(exe)
	$(MAKE) -C configure clean

tags: FORCE
	find . -name "*.cl" | xargs etags

FORCE:
