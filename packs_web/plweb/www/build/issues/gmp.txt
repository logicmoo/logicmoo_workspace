# Issues with gmp

## Consequences

If SWI-Prolog is not compiled with GMP support, it does not support

  1. Unbounded integers
  2. Rational numbers
  3. Various random number and cryptographic features

Affected libraries

  - library(clpb).
  - library(clpfd).
  - library(http/http_openid)
  - library(simplex).

## Solutions

  - If a binary distribution starts, but check_installation/0 reports GMP as missing,
    you should contact the packager of the distribution and point at this page.
  - The binary installations for MacOS and Windows provided by us bundle the GMP
    library.  If there are issues, report them.
  - If you built from *source*, make sure the GMP development libraries are installed.
    Perform a fully new build afterwards and re-check.
    
