---+ SWI-Prolog semweb package version 3

Starting at Nov 16, 2012, the GIT version of pl-devel.git is updated to
version 3 of the RDF package.  Highlights:

  - Clean transaction semantics
  - Concurrent update and querying using the familiar Prolog
    _|logical update semantics|_
  - Concurrent modifications, including loading of RDF from
    multiple sources and restoring multiple named graphs from
    the persistent database.
  - Restoring and loading of RDF can be several times faster
    (depending on hardware, OS and data).
  - Memory consumption is approximately 30% higher.  Can be
    tweaked down a little more at the cost of some performance
    reduction.  See [[3.12 Memory management considerations][http://www.swi-prolog.org/download/doc/semweb.html#sec:3.12]]

Installation and basic functionality has been tested on Linux (Ubuntu
12.04 and 12.10), Windows and MacOS 10.7 (Lion).

This version works with the current
[[ClioPatria][http://cliopatria.swi-prolog.org]].  ClioPatria still
works with the old version as well.

*Documentation* is uploaded to http://www.swi-prolog.org/download/doc/
(both PDF and HTML).
[[Section 13][http://www.swi-prolog.org/download/doc/semweb.html#sec:13]]
documents the (few and minor) implications for upgrading applications.

*|Version 2|* has been given the GIT branch =version2=.

