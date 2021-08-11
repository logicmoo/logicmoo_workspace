# Issues with library(readline)

The library(readline) is based on [GNU
readline](https://tiswww.case.edu/php/chet/readline/rltop.html),
providing command line editing for the terminal on Unix-like systems.

## Consequences

The library(readline) is an alternative to library(editline), providing
the same functionality based on the BSD libedit library. The
library(editline) provides some advantages:

  - BSD license doesn't affect the licensing of SWI-Prolog
  - Also provides editong for secondary consoles (interactor/0)
  - Can be programmed in Prolog

The main advantage of library(readline) is that GNU readline is a much
more powerful editor.

## Solutions

Install the development package for libreadline.  This is available on
many platforms.

  $ Debian/Ubuntu :
  =|$ apt-get install libreadline-dev|=

After installing the development library, [re-install the
system](<RebuildAfterDevLib.html>)
