# $Id: Makefile,v 1.11 2005/08/03 20:56:34 dancy Exp $

### IMPORTANT: the nightly builds the product need to use a local copy and
###	       and not one in /c/Program Files/...  only change the ../aclxxx
###	       when the build machine's copy is updated.
LISPDIR := $(shell if test -f ../../acl70/allegro; then echo ../../acl70; else echo '"/c/Program Files/acl70"'; fi)
ALLEGRO=$(LISPDIR)/allegro

build: configure/configure.exe

configure/configure.exe:
	$(ALLEGRO) -L buildit.cl

clean:
	rm -fr *.fasl configure *.build-dribble 
