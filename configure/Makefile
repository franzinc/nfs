# $Id: Makefile,v 1.8 2004/11/02 23:17:19 layer Exp $

### IMPORTANT: the nightly builds the product need to use a local copy and
###	       and not one in /c/Program Files/...  only change the ../aclxxx
###	       when the build machine's copy is updated.
LISPDIR := $(shell if test -d ../../acl70pf; then echo ../../acl70pf; else echo '"/c/Program Files/acl70"'; fi)
ALLEGRO=$(LISPDIR)/allegro

build: configure/configure.exe

configure/configure.exe:
	# Hack.. to avoid incompatible fasl warning on shared files
	rm -f ../*.fasl
	$(ALLEGRO) -L buildit.cl

clean:
	rm -fr *.fasl configure *.build-dribble 
