# $Id: Makefile,v 1.5.2.1 2004/09/16 20:34:08 layer Exp $

LISPDIR=$(shell if test -d ../../acl70pf; then echo ../../acl70pf; else echo '"/c/Program Files/acl70pf"'; fi)
ALLEGRO=$(LISPDIR)/allegro

build: configure/configure.exe

configure/configure.exe:
	# Hack.. to avoid incompatible fasl warning on shared files
	rm -f ../*.fasl
	$(ALLEGRO) -L buildit.cl

clean:
	rm -fr *.fasl configure *.build-dribble 
