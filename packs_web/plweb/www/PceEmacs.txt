---+ The SWI-Prolog builtin editor: PceEmacs

			[[emacs.gif]]

PceEmacs showing a coloured Prolog file, the summary description of the
current goal and a just-set break-point. Breakpoints are set at a
specific call in a specific clause (opposite to traditional spy-points
that trap the tracer on any call to a specified predicate). As execution
reaches the break-point, the source-level debugger is activated.

PceEmacs is started using ?- emacs. or ?- emacs(File). It can be made
the default editor for the edit/0 and edit/1 commands by setting the
Prolog flag =editor=. For example by adding the following to =|.plrc|=
(=|pl.ini|= on MS-Windows):

==
:- set_prolog_flag(editor, pce_emacs).
==
