# SWI-Prolog interface for RocksDB

This is a SWI-Prolog pack that   provides library(rocksdb), a binding to
[RocksDB](http://rocksdb.org/).

## Installation

Make sure you have `rocksdb`, including   the  development libraries and
headers installed on your system in  a   location  where `gcc` looks for
them.  On Debian based systems, this is done using

    sudo apt-get install librocksdb-dev

Make sure to have  a  sufficiently   recent  version  of SWI-Prolog. The
required extensions for the  C++  interface   are  present  from version
`V7.3.24-32-g0ecc515`.    This    implies    the    git    version    of
[swipl-devel](https://github.com/SWI-Prolog/swipl-devel) pulled on  July
11, 2016 or development version 7.3.25 or later.

If all this is in place, run

    ?- pack_install(rocksdb).

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


