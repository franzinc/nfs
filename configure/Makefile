# $Id: Makefile,v 1.13 2006/01/30 18:49:11 layer Exp $

### IMPORTANT: the nightly builds the product need to use a local copy and
###	       and not one in /c/Program Files/...  only change the ../aclxxx
###	       when the build machine's copy is updated.
LISPDIR = "/c/Program Files/acl80"
ALLEGRO=$(LISPDIR)/allegro

build: configure/configure.exe

configure/configure.exe:
	$(ALLEGRO) -L buildit.cl

clean:
	rm -fr *.fasl configure *.build-dribble 
