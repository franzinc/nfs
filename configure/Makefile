# $Id: Makefile,v 1.6 2004/09/16 17:40:17 dancy Exp $

LISPDIR=$(shell if test -d ../../acl70pb; then echo ../../acl70pb; else echo '"/c/Program Files/acl70pf"'; fi)
ALLEGRO=$(LISPDIR)/allegro

build: configure/configure.exe

configure/configure.exe:
	# Hack.. to avoid incompatible fasl warning on shared files
	rm -f ../*.fasl
	$(ALLEGRO) -L buildit.cl

clean:
	rm -fr *.fasl configure *.build-dribble 
