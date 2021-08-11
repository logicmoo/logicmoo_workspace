# Issues with library(pce)

## Consequences

The library(pce) provides graphics to SWI-Prolog. It isn't needed to run
Prolog, but you need it for:

  - Almost all development tools: editor, source navigator,
    cross-referencer, profiler, graphical debugger, etc.
  - The SPARQL implementation of ClioPatria uses it for regular
    expressions in SPARQL.

## Solutions

  $ Linux packages :
  Quite a few Linux packages for SWI-Prolog have a separate package
  for xpce because it requires X11.  For example:

    - Debian based:

      ==
      apt-get install swi-prolog-x
      ==

  $ Sources on Unix systems :
  Often the jpeg or Xpm development libraries are missing.  See
  [here](<RebuildAfterDevLib.txt>).

  $ MacOS :
  On the Mac, the latest version of XQuartz is generally required.
  See https://xquartz.macosforge.org/landing/  If you build from
  source, make sure you have libjpeg and libXpm.

  $ Windows :
  There are no known problems on this platform wrt. library(pce).
