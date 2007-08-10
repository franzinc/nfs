# $Id: Makefile,v 1.16 2007/08/10 00:32:10 dancy Exp $

## The `stable' directory is one that is only updated after rigorous
## testing, to make sure the base is as stable as possible.
STABLE := $(shell if test -d "/c/Program Files/acl81-stable"; then echo yes; else echo no; fi)
ifeq ($(STABLE),yes)
LISPDIR = "/c/Program Files/acl81-stable"
else
LISPDIR = "/c/Program Files/acl81"
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
