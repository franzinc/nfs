# $Id: Makefile,v 1.5 2004/02/20 23:14:28 layer Exp $

LISPDIR=$(shell if test -d ../../acl70pb; then echo ../../acl70pb; else echo '"/c/Program Files/acl70pb"'; fi)
ALLEGRO=$(LISPDIR)/allegro

build: configure/configure.exe

configure/configure.exe:
	# Hack.. to avoid incompatible fasl warning on shared files
	rm -f ../*.fasl
	$(ALLEGRO) -L buildit.cl

clean:
	rm -fr *.fasl configure *.build-dribble 
