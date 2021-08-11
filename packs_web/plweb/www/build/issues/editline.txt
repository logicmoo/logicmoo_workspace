# Issues with library(editline)

The library(editline) is based on [BSD
libedit](https://www.cs.utah.edu/~bigler/code/libedit.html), providing
command line editing for the terminal on Unix-like systems.

## Consequences

The library(editline) is an alternative to library(readline), providing
the same functionality based on the GNU readline library. The
library(editline) provides some advantages:

  - BSD license doesn't affect the licensing of SWI-Prolog
  - Also provides editing for secondary consoles (interactor/0)
  - Can be programmed in Prolog

The main disadvantage is that GNU readline is a much more powerful
editor.

## Solutions

Install the development package for libedit.  This is available on
many platforms.

  $ Debian/Ubuntu :
  =|$ apt-get install libedit-dev|=

  $ Red Hat/Fedora :
  =|$ yum install libedit-devel|= or =|$ dnf install libedit-devel|=

After installing the development library, [re-install the
system](<RebuildAfterDevLib.html>)
