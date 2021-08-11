---+ Select an editor

The simplest way to tell edit/1 to use a specific editor is using the
prolog flag editor, which can be set in the system or personal
initialisation file. Two values have a special meaning. If the value
starts with a $, it is taken as the name of an environment variable,
e.g., =|$EDITOR|=, thus following the Unix convention to use this
variable for selecting an editor. The value =pce_emacs= (default if XPCE
is available) causes the internal Emacs clone to be used. Some examples:

|:- set_prolog_flag(editor, pce_emacs). | Use PceEmacs         |
|:- set_prolog_flag(editor, wordpad).   | Windows: use wordpad |

If the editor is not known to SWI-Prolog, it will open the file, but not
locate the editor to source-locations. To tell Prolog how to open a file
and jump to a specified line, add a clause to prolog_edit:edit_command/2 . See
the example below. See library/edit.pl for details.

==
:- multifile
	prolog_edit:edit_command/2.

edit_command(myedit, '%e --line=%d "%f"').
==

---++ Editing the personal initialisation file

On Windows, using plwin.exe use *|Settings/User init file...|* from the
menu. On first use it will ask you whether it should install the default
profile. Confirm this and edit the result.

On systems using XPCE you can achieve the same from the SWI-Prolog help
window opened with the =|?- help.|= command.

For non-windows users, see PlInitialisation.txt for how Prolog locates
its customisation files.
