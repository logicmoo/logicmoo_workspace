The Causal Calculator (CCalc) version 2.0

-------------
Installation:
-------------

To install CCalc, simply unpack the contents of the distribution tarfile to the
directory of your choice.  CCalc is written in Prolog, so it does not need to
be compiled for the target system, but the satisfiability solvers called by
CCalc do.

SAT solver executables are provided for Linux and SunOS; to test whether these
will work properly on your system, you can run 'solver_test'. To do so, start
Prolog, include both the files 'ccalc.pl' and 'solver_test.pl', and execute the
procedure 'solver_test'.  CCalc will test the solvers and some of the
potentially nonportable system calls and report any problems it finds.

If you need to recompile a SAT solver, you can obtain the source code or
executables from the locations listed below under "Obtaining SAT solvers."  
You may need to rename the executable you download or compile; CCalc expects
each solver have the same name as its corresponding argument to the
'set(solver,_)' command.  (For example, when mChaff is compiled, the executable
is called Chaff2; it must be renamed to "mchaff" to be used with CCalc.)  You
must then copy it to the directory [CCalc]/solvers/[OS], where [CCalc] is the
base directory of your CCalc installation and [OS] is the name of your
operating system as reported by the Unix function 'uname'.

A single installation of CCalc may be shared by multiple operating systems on a
network by including for each OS a subdirectory of the 'solvers' directory
containing solvers compiled for that OS.  CCalc will call 'uname' to determine
the OS in use and use the appropriate set of solvers.

The file 'options.std' in the CCalc installation directory contains default
user options.  This file is included whenever CCalc is loaded or when the
command 'reset_parameters' is executed.  You may wish to edit this file to set
options appropriate for your users.  For more information about setting
options, please use 'help(options).' within CCalc, or see the tutorial on
the Web site listed below.


---------------------
Obtaining SAT solvers
---------------------

The satisfiability solvers which can be used by CCalc are:

GRASP, Feb/2000 version, by João Marques Silva
http://sat.inesc.pt/grasp/

mChaff, spelt3 version, by Matthew W. Moskewicz; and zChaff, by Lintao Zhang
both at http://www.princeton.edu/~chaff/software.html

relsat, versions 2.02 and 1.1.2 (the latter called 'relsat_old' within CCalc),
by Roberto Bayardo
the former available from http://www.bayardo.org/resources.html
and the latter available by e-mailing ccalc-help@cs.utexas.edu.

SATO, version 3.2.1, by Hantao Zhang
http://www.cs.uiowa.edu/~hzhang/sato.html
(SATO version 3.1.2 is also included with CCalc as 'sato_old', but source code
is no longer available for that version.)

Satz, version 215.2, by Chu Min Li
http://www.laria.u-picardie.fr/~cli/EnglishPage.html

Satz-rand, version 4.9, by Henry Kautz (based on Chu Min Li's Satz)
http://www.cs.washington.edu/homes/kautz/satz-rand/

WalkSAT, version 41, by Bart Selman and Henry Kautz
http://www.cs.washington.edu/homes/kautz/walksat/


CCalc can also use James M. Crawford's "compact" utility to reduce the size of
the propositional theory before passing it to the SAT solver.  The program was
modified (to produce additional output) for use with CCalc; the modified source
code is included in [CCalc]/solvers/compact.tar.gz.  The original version of
compact is available as part of the distribution for the SAT checker "tableau"
on Crawford's Web page at: http://www.cirl.uoregon.edu/crawford/crawford.html


----------------------
Additional information
----------------------

For more information about CCalc, please:

- see the online help utility, accessed by the command 'help.' within CCalc;

- visit the CCalc Web site at http://www.cs.utexas.edu/users/tag/ccalc, which
  includes a tutorial for using CCalc;

- or email ccalc-help@cs.utexas.edu.

Thanks for using CCalc!
