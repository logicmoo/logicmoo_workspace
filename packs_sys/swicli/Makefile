UNAME := $(shell uname)
ifeq ($(UNAME),$(filter $(UNAME),Linux Darwin SunOS FreeBSD GNU/kFreeBSD NetBSD OpenBSD GNU))
ifeq ($(UNAME),$(filter $(UNAME),Darwin))
OS=darwin
else
ifeq ($(UNAME),$(filter $(UNAME),SunOS))
OS=solaris
else
ifeq ($(UNAME),$(filter $(UNAME),FreeBSD GNU/kFreeBSD NetBSD OpenBSD))
OS=bsd
else
OS=linux
endif
endif
endif
else
OS=windows
endif

HARDWARE_NAME := $(shell uname -m)

ifneq ($(findstring 64, $(HARDWARE_NAME)),)
BITS=""
else
BITS=""
endif


SHELL := /bin/bash
ifndef SWIARCH
SWIARCH=$(shell uname -m)-$(OS)
endif
LIBDIR=lib/$(SWIARCH)
#
#
CC=swipl-ld
INC1=src/swicli
CURRDIR=$(shell pwd)
#INC2=$(shell while read one two three; \
#do TEMP=$two; \
#done <<< `whereis swipl`; \
#readlink -f $TEMP; \
)/include #da completare

CFLAGS=$(shell pkg-config --cflags --libs mono-2)

INC2=$(SWIHOME)/include
#INC2=`echo /usr/lib/swi*`/include/
INCDIRS= -I$(INC1) -I$(INC2)

LDFLAGS= $(CFLAGS) -fPIC -DBP_FREE -O3 -fomit-frame-pointer -Wall -g -O2 $(INCDIRS) 

ifndef SOEXT
SOEXT=so
endif

SWICLI_SO=$(LIBDIR)/swicli.$(SOEXT)


all: prepare compile
	
prepare:
	@echo $(shell ./make-linux.sh prepare)

compile: $(SWICLI_SO)
	@echo $(shell ./make-linux.sh compile)

$(SWICLI_SO):
	$(CC) -shared -Wno-unused-result src/swicli/swicli.c $(LDFLAGS) $(MONO_FLAGS) -o $(SWICLI_SO)

distclean: clean
	@echo rm Makefile.bak

clean: prepare
	rm -f $(SWICLI_SO) \
	@echo $(shell ./make-linux.sh clean)

check:
	@echo "the check is in your mouth" \
	@echo $(shell ./make-linux.sh check)

install:
	@echo install $(SWICLI_SO) $(LIBDIR) \
	@echo $(shell ./make-linux.sh install) 

uninstall:
	@echo Uninstall $(SWICLI_SO) $(LIBDIR) \
	@echo $(shell ./make-linux.sh uninstall)


