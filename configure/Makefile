# $Id: Makefile,v 1.12 2005/11/28 21:56:25 layer Exp $

### IMPORTANT: the nightly builds the product need to use a local copy and
###	       and not one in /c/Program Files/...  only change the ../aclxxx
###	       when the build machine's copy is updated.
LISPDIR := $(shell if test -f ../../acl80b/allegro; then echo ../../acl80b; else echo '"/c/Program Files/acl80"'; fi)
ALLEGRO=$(LISPDIR)/allegro

build: configure/configure.exe

configure/configure.exe:
	$(ALLEGRO) -L buildit.cl

clean:
	rm -fr *.fasl configure *.build-dribble 
