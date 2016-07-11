#COFLAGS=-gdwarf-2 -g3
CPPFLAGS=-std=c++11 $(CFLAGS) $(LDSOFLAGS)
PLPATHS=-p library=prolog -p foreign="$(PACKSODIR)"

all:	$(PACKSODIR)/rocksdb4pl.$(SOEXT)

$(PACKSODIR)/rocksdb4pl.$(SOEXT): cpp/rocksdb4pl.cpp
	mkdir -p $(PACKSODIR)
	gcc $(CPPFLAGS) -shared -o $@ cpp/rocksdb4pl.cpp -lrocksdb $(SWISOLIB)

install::

check::
	swipl $(PLPATHS) -g test_rocksdb,halt -t 'halt(1)' test/test_rocksdb.pl

distclean: clean
	rm -f $(PACKSODIR)/rocksdb.$(SOEXT)

clean:
	rm -f *~

