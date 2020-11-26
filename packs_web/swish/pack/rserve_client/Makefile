CPPFLAGS=-std=c++11 -fPIC
SOBJ=   $(PACKSODIR)/rserve.$(SOEXT)
CFLAGS+=-I$(HDTHOME)/include
SWICPPFLAGS=-std=c++11
COFLAGS=-O2 -gdwarf-2 -g3
LIBS=-ldl -lcrypt -lm
LD=g++

CXXCLIENT=Rserve/src/client/cxx
CXXDEPS= $(CXXCLIENT)/configure
RCONN=$(CXXCLIENT)/Rconnection.o
RCONNH=$(CXXCLIENT)/Rconnection.h
RCONNIN=$(CXXCLIENT)/Rconnection.cc $(RCONNH)
RSINCLUDE=-IRserve/src -IRserve/src/include -I$(CXXCLIENT)

OBJ=cc/rserve.o $(RCONN)

all:	$(SOBJ)

$(SOBJ): $(OBJ)
	mkdir -p $(PACKSODIR)
	$(LD) $(ARCH) $(LDSOFLAGS) -o $@ $(OBJ) $(LIBS) $(SWISOLIB)

cc/rserve.o: cc/rserve.cc $(CXXCLIENT)/Makefile $(RCONNH)
	$(CC) $(ARCH) $(CFLAGS) $(COFLAGS) $(SWICPPFLAGS) $(RSINCLUDE) -c -o $@ $<

clean:
	rm -f cc/rserve.o $(RCONN) *~

distclean: clean
	rm -f $(SOBJ)

check::
install::

$(CXXCLIENT)/configure.ac:
	@if [ -d .git ]; then \
	  git submodule update --init; \
	else \
	  git clone -b janw https://github.com/JanWielemaker/Rserve; \
	fi

$(CXXCLIENT)/configure: $(CXXCLIENT)/configure.ac
	cd $(CXXCLIENT) && autoheader && autoconf

$(CXXCLIENT)/Makefile: $(CXXCLIENT)/configure $(CXXCLIENT)/Makefile.in
	cd $(CXXCLIENT) && ./configure

$(RCONN): $(CXXCLIENT)/Makefile $(RCONNIN)
	$(CXX) -c $(CPPFLAGS) $(COFLAGS) $(RSINCLUDE) -o $@ $(CXXCLIENT)/Rconnection.cc
