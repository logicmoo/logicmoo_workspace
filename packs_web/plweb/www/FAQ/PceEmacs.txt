---+ Using PceEmacs

The XPCE GUI tool for Prolog comes with a built-in emacs clone written
in Prolog. Its Prolog mode does proper indentation, full syntax
checking by calling the SWI-Prolog parser, warning for singleton
variables and finding predicate definitions based on the
source-information from the Prolog database. Syntax colouring of clause
heads and goals is based on information from the cross-referencer.

Both syntax checking and singleton warnings are generated for the clause
in which the caret appears after every keystroke.  Global analysis of the
file is done after a shot idle time or after typing *|C-l|* (control+L).

@see	BindEditor.txt for selecting PceEmacs as default editor.
@see	../PceEmacs.txt for screenshot
@see	../gxref.txt for a graphical cross-referencer
