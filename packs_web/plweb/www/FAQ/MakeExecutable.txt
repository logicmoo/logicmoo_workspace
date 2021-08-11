---+ How do I make an executable?

For local use you generally do not want to make an executable. Simply
create a script as described in PrologScript.txt is easier and starts
only a little slower in most cases. Sometimes you do want to make a true
executable. Some cases are:

    * I want to deliver to a machine that doesn't have Prolog installed
    and doesn't want to install it. 

    * I need a .exe file, for example to use as a CGI script or because
    I want to associate it with a filetype in Windows. 

A SWI-Prolog executable is a single file that consists of a native
system executable with a saved-state attached to it. States are created
using qsave_program/2 as described in the SWI-Prolog reference manual.

Unfortunately things start to get platform specific quickly. Here are
some cases:

    * WinExe.txt -- Make an executable of Prolog for MS-Windows
    * PceWinExe.txt -- Make an executable of an XPCE graphical
    application for MS-Windows
    * UnixExe.txt -- Make executable in Linux/Unix systems 
