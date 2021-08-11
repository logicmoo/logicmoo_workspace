# Issues with tcmalloc

[tcmalloc](https://github.com/google/tcmalloc) is a memory allocation
library provided by Google that works well with long running highly
concurrent programs in SWI-Prolog, notably reducing the required
memory resources.

## Consequences

If SWI-Prolog is not compiled with tcmalloc support

  1. it does not support malloc_property/1, set_malloc/1 and `heapused`,
     `memory` and `core` keys of statistics/2.
  2. The total memory footprint can be significantly higher than when
     using the system's default memory allocator.  This depends on the
     OS and SWI-Prolog workload though.

## Solutions

  - If a binary distribution starts, but check_installation/0 reports
    tcmalloc as missing, you should contact the packager of the
    distribution and point at this page.
  - If you built from __source__, make sure the tcmalloc library
    is installed.  The package is typically called something including
    _perftools_, i.e., `libgoogle-perftools-dev` (Debian),
    `gperftools-devel` (Redhat), `gperftools` (Macports). It must
    provide the library `libtcmalloc_minimal`.  The header files are
    not required.

