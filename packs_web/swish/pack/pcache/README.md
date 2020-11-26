# Persistent answer caching

This repo defines a  SWI-Prolog  library   `cache_rocks`,  which  uses a
RocksDB interface to provide persistent caching  of answers. The answers
are associated with a _deep  hash_   of  the  involved predicates, which
means that they are invalidated when  one   of  the  predicates that are
reachable from the call tree starting with the predicate associated with
the goal changes.

## Status

Experimental.

## Requirements

  - SWI-Prolog 7.5.14 or later

  - Install the rocksdb binding from the pack:

       ?- pack_install(rocksdb).

    Note that this requires rocksdb installed on your system.
