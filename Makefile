#
# default base directory for YAP installation
# (EROOT for architecture-dependent files)
#
#
prefix = /home/rzf/usr/local_yap6.3rw-lam
exec_prefix = ${prefix}
ROOTDIR = $(prefix)
EROOTDIR = ${prefix}
abs_top_builddir = /home/rzf/yap-6.3rwnew/archmpi
#
# where the binary should be
#
BINDIR = $(EROOTDIR)/bin
#
# where YAP should look for libraries
#
LIBDIR=${exec_prefix}/lib
YAPLIBDIR=${exec_prefix}/lib/Yap
#
#
CC=gcc
MPI_CC=/usr/local/mpi/mpich2-1.4.1-gcc/bin/mpicc
CFLAGS= -shared -fPIC -O3 -fomit-frame-pointer -Wall -Wstrict-prototypes -Wmissing-prototypes -g -O2 $(YAP_EXTRAS) $(DEFS) -I$(srcdir) -I/home/rzf/usr/local/lib/swipl-7.1.28/include -I/home/rzf/usr/local/lib/swipl-7.1.28/include/Yap -I/home/rzf/yap-6.3rwnew/archmpi
#
#
# You shouldn't need to change what follows.
#
INSTALL=/usr/bin/install -c
INSTALL_DATA=${INSTALL} -m 644
INSTALL_PROGRAM=${INSTALL}
SHELL=/bin/sh
RANLIB=ranlib
srcdir=.
SO=so
CWD=$(PWD)
MPILDF=
MPICF=
#

OBJS=pl_mpi.o hash.o prologterms2c.o
SOBJS=pl_mpi.so

#in some systems we just create a single object, in others we need to
# create a libray

all: $(SOBJS)

pl_mpi.o: $(srcdir)/pl_mpi.c 
	$(MPI_CC)  $(CFLAGS) $(MPICF)  -c $(srcdir)/pl_mpi.c -o pl_mpi.o 

prologterms2c.o: $(srcdir)/prologterms2c.c $(srcdir)/prologterms2c.h
	$(MPI_CC) -c $(CFLAGS) $(srcdir)/prologterms2c.c -o prologterms2c.o

hash.o: $(srcdir)/hash.c $(srcdir)/hash.h
	$(MPI_CC) -c $(CFLAGS) $(srcdir)/hash.c -o hash.o

pl_mpi.so: $(OBJS)
	$(MPI_CC) -shared -export-dynamic $(LDFLAGS) -o pl_mpi.so $(OBJS)  $(MPILDF)  -Wl,-R,$(YAPLIBDIR) -Wl,-R,$(LIBDIR)

install: all  install-examples
	if test x"$(SOBJS)" != "x"; then $(INSTALL_PROGRAM) $(SOBJS) $(DESTDIR)$(YAPLIBDIR); fi

clean:
	rm -f *.o *~ $(OBJS) $(SOBJS) *.BAK

no:
	@echo "YAP LAM/MPI module not compiled."

install-examples:

distclean: clean
	rm -f Makefile

FULL_PATH_C_SOURCES=  $(srcdir)/pl_mpi.c $(srcdir)/prologterms2c.c  $(srcdir)/hash.c 
FULL_PATH_PL_SOURCES= 
FULL_PATH_HEADERS=$(srcdir)/hash.h $(srcdir)/prologterms2c.h

TAGS: $(C_SOURCES) $(PL_SOURCES) $(HEADERS)
	etags --append ../../TAGS $(FULL_PATH_C_SOURCES)  $(FULL_PATH_PL_SOURCES) $(FULL_PATH_HEADERS) 
