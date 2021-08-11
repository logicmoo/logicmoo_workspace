---+ Can I make an executable?

Creating an executable is discussed in the reference manual (see the
predicate qsave_program/2) and the *|-c|* commandline option. For local
usage, there is often no reason to make a real executable.

---++ Unix

SWI-Prolog supports PrologScript, which allows you to create an
executable program very easily on Unix systems:

==
#!/usr/bin/pl -q -g main -s

main :-
	current_prolog_flag(argv, Argv),
	append(_, [--|Av], Argv), !,
	main(Argv).

main(Argv) :-
	...
==

More information on using PrologScript as well as more compilation
issues can be found in the [[Reference Manual][</pldoc/man?section=compilation>]].

---++ Windows

If you are using Windows, you can write a .bat file or you can create a
shortcut to the =|.pl|= file to load. For example:

==
/* File: run.pl Purpose: Load and run my program */

:- [load].
:- go. 
==
