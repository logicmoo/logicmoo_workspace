SHELL := /bin/bash
LIBDIR=lib/$(SWIARCH)/  
#
#
CC=gcc
CURRDIR=$(shell pwd)

CFLAGSPHIL= -Wall -Wextra -g -O3 $(CFLAGS)  

all:  phil.$(SOEXT)

phil.$(SOEXT): phil.o
	
	if test $(SWIARCH) == x64-win64 -o $(SWIARCH) == i386-win32 ; then \
  $(CC) phil.o -static-libgcc -static-libstdc++  $(LDSOFLAGS) -lswipl -o phil.$(SOEXT) ;\
   else  \
     if [[ $(SWIARCH) ==  *darwin* ]] ;  then \
  $(CC) phil.o  $(LDSOFLAGS) -lswipl -o phil.$(SOEXT) ;\
  else  \
  $(CC) -export-dynamic phil.o $(LDSOFLAGS) -o phil.$(SOEXT) ;\
  fi \
  fi


phil.o : phil.c
	$(CC) -c $(CFLAGSPHIL) phil.c -o phil.o


distclean: clean
	rm Makefile

clean:
	rm -f *.o phil.$(SOEXT)

check:
	@echo "no check"

install: all
	mkdir -p $(LIBDIR)
	cp phil.$(SOEXT) $(LIBDIR)

installcheck:
	swipl -g test -t halt prolog/phil test/test.pl
