# $Id: Makefile,v 1.14 2007/01/25 22:22:06 layer Exp $

## The `stable' directory is one that is only updated after rigorous
## testing, to make sure the base is as stable as possible.
STABLE := $(shell if test -d "/c/Program Files/acl80-stable"; then echo yes; else echo no; fi)
ifeq ($(STABLE),yes)
LISPDIR = "/c/Program Files/acl80-stable"
else
LISPDIR = "/c/Program Files/acl80"
endif
ALLEGRO=$(LISPDIR)/allegro

build: configure/configure.exe

configure/configure.exe:
	$(ALLEGRO) -L buildit.cl

clean:
	rm -fr *.fasl configure *.build-dribble 
