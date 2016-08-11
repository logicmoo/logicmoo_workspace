LIBDIR=lib/$(SWIARCH)

#
#
CC=gcc
CFLAGSMPI= $(CFLAGS) $(CFLAGS)/Yap -shared -fPIC -O3 -fomit-frame-pointer -Wall -Wstrict-prototypes -Wmissing-prototypes -g -O2 

LDFLAGS= $(LDSOFLAGS)
#
#
# You shouldn't need to change what follows.
#
INSTALL=/usr/bin/install -c
INSTALL_DATA=${INSTALL} -m 644
INSTALL_PROGRAM=${INSTALL}
SHELL=/bin/sh
RANLIB=ranlib
CWD=$(PWD)
MPILDF=
MPICF=
#

OBJS=matrix.o
SOBJS=matrix.so

#in some systems we just create a single object, in others we need to
# create a libray

all: $(SOBJS)

matrix.o: matrix.c 
	$(CC)  $(CFLAGSMPI) -c matrix.c -o matrix.o 

matrix.so: $(OBJS)
	$(CC) -shared -export-dynamic $(LDFLAGS) -o matrix.so $(OBJS) -Wl,-R,$(LIBDIR)

check:
install: all 
	mkdir -p  $(LIBDIR)
	cp  matrix.so $(LIBDIR)

clean:
	rm -f *.o *~ $(OBJS) $(SOBJS) *.BAK


