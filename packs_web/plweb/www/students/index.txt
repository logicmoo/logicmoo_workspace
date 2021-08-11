# Using SWI-Prolog for studying Prolog

The vast majority of downloads for SWI-Prolog concerns students using it
for their Prolog language course. This wiki page is intended to collect
tricks and tips for using SWI-Prolog for teaching.

## Development environment

Options (FIXME: elaborate)

  $ Native (swipl-win + PceEmacs) :
  $ Using [SWI-Prolog Editor](http://lakk.bildung.hessen.de/netzwerk/faecher/informatik/swiprolog/indexe.html) (Windows only) :
  $ Using [Eclipse PDT pluggin](http://sewiki.iai.uni-bonn.de/research/pdt/) :
  $ Using [GNU-Emacs ediprolog pluggin](https://www.metalevel.at/ediprolog/) :

## Customization

If you use a centrally installed copy of SWI-Prolog, you can customize
it by editing =swipl.rc= and/or =swipl-win.rc=, which can be found in the
root directory of the installation. Students may add these to their
personal initialization file, which can be installed and edited using
the built-in editor by first running emacs/0 as below and then using
the menu *Edit/Prolog preferences*

  ==
  ?- emacs.
  ==


Below are some useful settings:

  $ :- set_prolog_flag(occurs_check, error). :
  This flag prevents users from creating infinite (cyclic) trees.  For
  example, the code below will now raise an error instead of failing
  silently.

    ==
	...,
	A = A+1
    ==

  $ :- set_prolog_stack(local, limit(4000000)). :
  By limiting the local (environment) stack to 4 Mb, unbounded recursion
  will quickly terminate with a resource error.

  $ :- load_files(library(clpfd), [silent(true)]). :
  This makes library(clpfd) available to your students, which allows
  for transparent monotonic arithmetic.  With this, students can learn
  to appreciate declarative programming to perform more
  realistic tasks.
