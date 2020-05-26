#COFLAGS=-gdwarf-2 -g3
CPPFLAGS=-std=c++11 $(CFLAGS) $(LDSOFLAGS)
LIBROCKSDB=rocksdb/librocksdb.a
ROCKSENV=ROCKSDB_DISABLE_JEMALLOC=1 ROCKSDB_DISABLE_TCMALLOC=1
ROCKSCFLAGS=EXTRA_CXXFLAGS=-fPIC EXTRA_CFLAGS=-fPIC USE_RTTI=1 DEBUG_LEVEL=0
PLPATHS=-p library=prolog -p foreign="$(PACKSODIR)"

# sets PLATFORM_LDFLAGS
-include rocksdb/make_config.mk

all:	plugin

rocksdb/INSTALL.md:
	git submodule update --init rocksdb
rocksdb/librocksdb.a: rocksdb/INSTALL.md
	$(ROCKSENV) make -C rocksdb static_lib $(ROCKSCFLAGS)

plugin:	$(LIBROCKSDB)
	$(MAKE) shared_object

shared_object: $(PACKSODIR)/rocksdb4pl.$(SOEXT)

$(PACKSODIR)/rocksdb4pl.$(SOEXT): cpp/rocksdb4pl.cpp $(LIBROCKSDB) Makefile
	mkdir -p $(PACKSODIR)
	gcc $(CPPFLAGS) -shared -o $@ cpp/rocksdb4pl.cpp $(LIBROCKSDB) $(PLATFORM_LDFLAGS) $(SWISOLIB)

install::

check::
	swipl $(PLPATHS) -g test_rocksdb -t halt test/test_rocksdb.pl

distclean: clean
	git -C rocksdb clean -xfd
	rm -f $(PACKSODIR)/rocksdb4pl.$(SOEXT)

clean:
	rm -f *~

