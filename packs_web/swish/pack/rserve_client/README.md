# Rserve client for SWI-Prolog/SWISH

## Rserve R-package:

  - https://rforge.net/Rserve/doc.html
  - https://github.com/s-u/Rserve
  - Installation of Rserve:
    - Default version (1.7.3):
      - install.packages("Rserve")
    - Latest (1.8.5, has lots of enhancements)
      - install.packages("Rserve",,"http://rforge.net/",type="source")

## Compiling this package

This library is configured as a  SWI-Prolog   pack.  It can be installed
using the command below. The installation requires `git`, `autoconf` and
the common C++ build tools.

    ?- pack_install('rserve_client').

This is tested on Ubuntu (14.04 and 16.04).  It performs these steps:

  - Clone my fork of Rserve (some extensions to the C++ client)
  - Configure and build the C++ client library
  - Build the SWI-Prolog interface `rserve.so`

## Using this package

This  package  is  primarily  intended  for    accessing   R  in  server
environments such as [SWISH](http://swish.swi-prolog.org).  We created a
[Docker container](https://github.com/JanWielemaker/rserve-sandbox) that
runs  Rserve  in  a  sandbox.  The  container  exposes  Rserve  using  a
Unix-domain socket at the following address:

    /home/rserve/socket

With SWISH and is interface  installed   in  adjacent directories, i.e.,
below the same parent, R may be linked to SWISH doing

    :- use_module(lib/r_swish).

Now, R is not safe. You should either run Rserve in a tight OS container
and load `library(r/r_sandbox)` or run SWISH   in  authenticated mode by
loading `lib/authenticate.pl.`

## Libraries provided

User libraries

  - library(r/r_call)
  Defines basic user API to R
  - library(r/r_data)
  Utilities to create and fetch R data frames

Implementation libraries

  - library(r/r_expand_dot)
  Allow for dots in atoms and functors without quotes.
  - library(r/r_grammar)
  R Parser utilities (lexer) that support R quasi quotations
  - library(r/r_term)
  DCG non-terminal to translate a term into an R command string
  - library(r/r_sandbox)
  Declare the R API sandbox-safe
  - library(r/r_serve)
  Low-level level communication library

## Status

This is just a proof of context. Obviously missing functionality:

  - Cover more _Prolog term --> R_ translations, following Real.
  - Support OOB (Out Of Band) communication introduced in recent
    versions of Rserve to deal with R I/O.

## Related projects

This           interface           is             inspired            by
[Real](http://stoics.org.uk/~nicos/sware/real/)        by         [Nicos
Angelopoulos](http://stoics.org.uk/~nicos/).  Main differences:

  - _Real_ is _embedded_ in SWI-Prolog.  This is more productive for
    local deployment as the communication is faster and R has access
    to its default environment.  Thus, R can open graphical windows
    and can read and write files.  _Real_ is also much more mature,
    notably in the supporting a much larger part of the R syntax
    from Prolog.

  - _Rserve_ runs typically using a different user in a different
    environment.  The R environment cannot easily communicate with
    your local development environment.  When used in a (web)
    server environment this comes with several advantages.  We can
    seriously sandbox the R executable, each query in SWISH gets
    its own R instance and information can thus nog leak between
    queries and users.
