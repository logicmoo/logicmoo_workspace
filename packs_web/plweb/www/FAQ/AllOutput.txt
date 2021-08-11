# Help: I want the whole answer

Both the toplevel query/answer loop as the debugger abbreviate long
complex terms. They do this to avoid endless pages of output. In fact,
they write using write_term/3 which takes an option-list as argument.
The option list for answers printed by the Prolog toplevel is in the
prolog-flag =toplevel_print_options= and the one for the debugger is in
=debugger_print_options=. Initially both have the value given below:

==
?- current_prolog_flag(answer_write_options, X).

X = [quoted(true), portray(true), max_depth(10),
     spacing(next_argument)].
==

The option max_depth(10) says anything nested more then 10 levels should
be written as =|...|=.

## What to do?

If the system prints an answer that is abbreviated and you want to see
it all, type *w* and the system will use plain write/1 for printing the
answer: (the user pressed *w* at the place the diagram says
=|[write]|=). Note the *|; true|*. This is used to introduce
_|non-determinism|_ that makes Prolog wait after the answer. If the
answer is _deterministic_, Prolog prints it with the default settings
and prompts for the next command.

==
?- atom_chars(goodbye_prolog, X) ; true.

X = [g, o, o, d, b, y, e, '_', p|...] [write]

X = [g, o, o, d, b, y, e, '_', p, r, o, l, o, g]
==

Use the *p* command to get the default behaviour.

The debugger also has the commands *w* and *p* to switch between
writeq/1 (quoted write without portray) and print/1 (use the
=toplevel_print_options= options).

## Changing default?

Add a set_prolog_flag/2 directive in your prolog personal initialisation
file (see PlInitialisation.txt) to change the default the above mentioned
prolog flags.  E.g. to disable abbreviation, use:

==
?- set_prolog_flag(answer_write_options,
		   [ quoted(true),
	             portray(true),
		     spacing(next_argument)
		   ]).
==

@see portray_text/1 for displaying lists of character codes as strings.
