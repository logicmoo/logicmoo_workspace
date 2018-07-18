LIBDIR=lib/$(SWIARCH)/

all: matrixNative.so doc

matrixNative.so: matrixNative.c matrixNative.h
	gcc -O2 -shared -o matrixNative.so matrixNative.c

doc: prolog/matrix.pl
	swipl prolog/doc.pl

check:
	@echo "no check"

install: all
	mkdir -p $(LIBDIR)
	cp matrixNative.$(SOEXT) $(LIBDIR)

