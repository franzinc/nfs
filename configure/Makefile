ifndef LISPDIR
LISPDIR="/c/Program Files/acl62"
endif
ALLEGRO=$(LISPDIR)/allegro

build: configure/configure.exe

configure/configure.exe:
	$(ALLEGRO) -L buildit.cl

clean:
	rm -fr *.fasl configure *.build-dribble 
