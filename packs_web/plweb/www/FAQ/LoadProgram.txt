---+ How do I load a program?

Start writing a Prolog program in a file with extension =|.pl|=. If your
program consists of multiple files, it is common practice to add a file
=|load.pl|= that contains file-loading directives to load the various parts
of the program:

==
/*  File:    load.pl
    Purpose: Load my program
*/

:- [ rules,
     inference,
     goals
   ].
==

On *Unix*, it is normal to start Prolog in the same directory as the
program resides and run the following commands.

==
% swipl
<banner>
?- [load].
<consult messages ...>
?-
==

Alternatively, you may wish to use

==
% swipl -s load.pl
<banner>
?-
==

On *Windows*, the =|.pl|= extension is associated with =|swipl-win.exe|= and most
comfortable way is to *|double-click|* the =|.pl|= file you want to load in the
explorer. This will start SWI-Prolog, which changes directory to the
directory holding the file and then loads the clicked file.
