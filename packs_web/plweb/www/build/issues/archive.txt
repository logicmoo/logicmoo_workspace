# Issues with library(archive)

## Consequences

The library(archive) is used to access encoded and archived data (compressed,
data, zip archives, tar archives, etc).  It is internally used by

  - pack_install/1 for installing [add-ons](</pack/list>)

## Solutions

  - For binary installations, contact the packager and point to this page.
  - For source installations, make sure that the development libraries for
    [libarchive](http://www.libarchive.org/) are installed and
    [re-build](<RebuildAfterDevLib.txt>).
