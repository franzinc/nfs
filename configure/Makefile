# $Id: Makefile,v 1.15 2007/05/09 22:12:08 dancy Exp $

## The `stable' directory is one that is only updated after rigorous
## testing, to make sure the base is as stable as possible.
STABLE := $(shell if test -d "/c/Program Files/acl80-stable"; then echo yes; else echo no; fi)
ifeq ($(STABLE),yes)
LISPDIR = "/c/Program Files/acl80-stable"
else
LISPDIR = "/c/Program Files/acl80"
endif
ALLEGRO=$(LISPDIR)/allegro

# Worst directory name ever
MT="/c/Program Files/Microsoft Platform SDK for Windows Server 2003 R2/Bin/mt.exe"

build: configure/configure.exe configure.exe.manifest
	$(MT) -manifest configure.exe.manifest -outputresource:configure/configure.exe

configure/configure.exe:
	$(ALLEGRO) -L buildit.cl

clean:
	rm -fr *.fasl configure *.build-dribble 
