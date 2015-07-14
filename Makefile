LIBDIR=lib/$(SWIARCH)

#
#
CC=gcc
MPI_CC=mpicc
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

OBJS=pl_mpi.o hash.o prologterms2c.o
SOBJS=pl_mpi.so

#in some systems we just create a single object, in others we need to
# create a libray

all: $(SOBJS)

pl_mpi.o: pl_mpi.c 
	$(MPI_CC)  $(CFLAGSMPI) -c pl_mpi.c -o pl_mpi.o 

prologterms2c.o: prologterms2c.c prologterms2c.h
	$(MPI_CC) -c $(CFLAGSMPI) prologterms2c.c -o prologterms2c.o

hash.o: hash.c hash.h
	$(MPI_CC) -c $(CFLAGSMPI) hash.c -o hash.o

pl_mpi.so: $(OBJS)
	$(MPI_CC) -shared -export-dynamic $(LDFLAGS) -o pl_mpi.so $(OBJS) -Wl,-R,$(LIBDIR)

check:
install: all 
	mkdir -p  $(LIBDIR)
	cp  pl_mpi.so $(LIBDIR)

clean:
	rm -f *.o *~ $(OBJS) $(SOBJS) *.BAK


