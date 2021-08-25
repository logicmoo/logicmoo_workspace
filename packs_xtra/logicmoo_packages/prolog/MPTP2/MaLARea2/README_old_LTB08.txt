MaLARea for CASC LTB, version 0.3

Run it as ./RunLTB with two arguments - see the SYNOPSIS in RunLTB.pl 
for their description.

Requirements:

- Linux
- sufficiently large /dev/shm (2G is good, it should be more than the 
  size of all MIZ files with expanded includes)
- following commands should be the appropriate executables:
  - swipl  ... starts SWI Prolog
  - sha1sum ... the Linux sha1sum checksum
  - cat, grep, sed, perl, tee, etc.

This is a complex beast, strange things can happen - please report them.

LICENCE:

The TheoryLearner.pl and the scripts are GPL, the files in the bin/ directory 
are distributed under whatever licence they allow (usually GPL, don't know
for TPTP tools, SNoW has a special licence).

Josef Urban

