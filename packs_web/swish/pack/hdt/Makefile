# Build HDT library for SWI-Prolog

HDTHOME=hdt-cpp/hdt-lib
SOBJ=	$(PACKSODIR)/hdt4pl.$(SOEXT)
CFLAGS+=-I$(HDTHOME)/include -g
LIBS=	-L$(HDTHOME) -lhdt
OBJ=	c/hdt4pl.o
LD=g++

all:	$(SOBJ)

$(SOBJ): $(OBJ)
	mkdir -p $(PACKSODIR)
	$(LD) $(ARCH) $(LDSOFLAGS) -o $@ $< $(LIBS) $(SWISOLIB) -lserd-0

c/hdt4pl.o: c/hdt4pl.cpp $(HDTHOME)/libhdt.a
	$(CC) $(ARCH) $(CFLAGS) -c -o $@ c/hdt4pl.cpp

$(HDTHOME)/.make-senitel:
	[ ! -f $(HDTHOME)/Makefile ] || (cd $(HDTHOME) && git reset --hard)
	git submodule update --init
	sed -i 's/^FLAGS=-O3/FLAGS=-fPIC -O3/' $(HDTHOME)/Makefile
	touch $@

$(HDTHOME)/libhdt.a: $(HDTHOME)/.make-senitel
	$(MAKE) -C $(HDTHOME) all

check::
install::
clean:
	rm -f $(OBJ) $(HDTHOME)/.make-senitel
	[ ! -f $(HDTHOME)/Makefile ] || (cd $(HDTHOME) && git reset --hard)
	[ ! -f $(HDTHOME)/Makefile ] || $(MAKE) -C $(HDTHOME) clean

distclean: clean
	rm -f $(SOBJ)
