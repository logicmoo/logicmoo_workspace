---+ Cut/copy/paste from the plwin.exe console?

The console of plwin.exe (the main executable for Windows) is modelled
after the Unix/X11 application xterm and follows the conventions
thereof. This means that:

    * Copy is implied by selection (no need for Control-C)
    * Paste is achieved using middle-button click (or Control-V,
    as this doesn't conflict). 

Control-C is defined to raise an interrupt to stop your running Prolog
application.

The plwin.exe console has a menu holding the copy and paste operations
as well as access to many other common operations. 

@see MakeLog.txt on how to make a transcript of the terminal session.


