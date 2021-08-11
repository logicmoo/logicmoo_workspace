---+ SWI-Prolog XREF --- Cross-Referencing Tool

The SWI-Prolog Cross-Referencer can be used to examine the currently
loaded program, creating a hierarchical file overview with icons
indicating which files have suspicious code, a dependency view as shown
below and detailed info per file as shown below that. To analyse a
program, load it into Prolog and, simply run gxref/0:

==
?- gxref.
==

---++ XREF - Dependency

			[[xrefchatdep.gif]]

File-dependency view. Views can be created for the whole project, per
directory or selectively by adding files to the view.

---++ XREF - File info

			[[xrefchatfile.gif]]

Provide information per file on its dependency. Using the command
'Header', the system can create module headers with appropriate import
and export headers. 
