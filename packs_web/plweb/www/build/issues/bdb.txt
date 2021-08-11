# Issues with library(bdb)

## Consequences

The library(bdb) provides an interface to the BerkeleyDB embedded database.  SWI-Prolog has no dependencies on this library, so if you do not
intend to use this library there is no reason to take action.

## Solutions

Building the BerkeleyDB interface library requires the database development
libraries and headers.  Packages:

  - Debian based Linux systems: =|sudo apt-get install libdb-dev|=
  - Windows: see =README.mingw=
  - MacOS using Macports: =|sudo port install db60|=