# Issues with command_line_editing

## Consequences

Command line editing allows reusing editing commands using the keyboard arrows.

## Solutions

  $ Linux :
  Make sure the development version of =libreadline= is installed.  See
  [here](</build/issues/RebuildAfterDevLib.txt>)

  $ MacOS :
  As Linux, but be aware that the =build= script sets the environment to
  prefer to GNU version of libreadline over the system one.  The system
  one has the same name and basic interface, but lacks features that are
  required to link SWI-Prolog.

  Note that the graphical app for MacOS has its own line editing.

  $ Windows :
  The graphical app (=swipl-win.exe=) has its own line editing.  The console 
  version (=swipl.exe=) uses the command line editing facilities of the Windows
  console.
