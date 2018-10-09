SHELL := /bin/bash
LIBDIR=/usr/lib/swi-prolog/include/ # The directory holding SWI-Prolog.h file
CFLAGS=-Wall -Wextra -g -O3 -fPIC
CC=gcc
CURRDIR=$(shell pwd)

phil.so: phil.o
	$(CC)  phil.o -shared -o phil.so

phil.o: phil.c
	$(CC) -c $(CFLAGS) -I $(LIBDIR) phil.c -o phil.o

