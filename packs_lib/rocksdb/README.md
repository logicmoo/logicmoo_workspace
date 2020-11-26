# SWI-Prolog interface for RocksDB

This is a SWI-Prolog pack that   provides library(rocksdb), a binding to
[RocksDB](http://rocksdb.org/).

## Installation

The installation requires a recent C++   compiler. RocksDB can work with
several compression libraries. Most  systems   have  zlib installed, but
others may provide better  performance  or   less  resource  usage.  See
[INSTALL.md](https://github.com/facebook/rocksdb/blob/master/INSTALL.md).
Once these are in place, a simple

    ?- pack_install(rocksdb).

Should do the trick. Note that this clones RocksDB and builds it the way
we need the library. This requires   significant  disk space (1.4Gb) and
takes long (several minutes on a modern machine).

#### Why are you not using the pre-build librocksdb?

There are a  number  of  issues   with  several  pre-built  versions  of
librocksdb:

  - Shared objects are often linked to jemalloc or tcmalloc. This
    prevents lazy loading of the library, causing either problems
    loading or running the embedded rocksdb.
  - Various libraries are compiled without RTTI (RunTime Type Info),
    which breaks subclassing RocksDB classes.
  - Static library is by default compiled without -fPIC and thus not
    usable.

As is, the most reliable way around  is   to  include RocksDB, so we can
control the version and build it the way  that best fits our needs: as a
static library with RTTI and -fPIC.


### Manual installation

If the above fails

  - Clone this prepository in the `pack` directory of your installation
    or clone it elsewhere and link it.
  - Run `?- pack_rebuild(rocksdb).` to rebuild it.  On failure, adjust
    `Makefile` to suit your installation and re-run the pack_rebuild/1
    command.

### Status

The wrapper provides most functionality of  RocksDB. It does not provide
access to the many options that can   be set to configure RocksDB. These
will be added on demand. Please  create issues for missing functionality
(or bugs).


