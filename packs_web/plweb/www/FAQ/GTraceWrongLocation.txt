---+ Graphical tracer does not show the correct location of calls

Sometimes, the graphical tracer (enabled with guitracer/0 or started
using gtrace/0) highlights a call (the green area) incorrectly. If it
shows a completely different goal than you expected you may be wrong in
your expectations or there may be a bug in the debugger.

If the green area doesn't exactly highlight a goal, it is possible that
you are using a POSIX (Unix, Linux, MacOSX) platform and the file has
been transferred from a DOS (Windows) platform without updating the
file to the POSIX newline conventions.  I.e. your file contains CR/LF
between lines while POSIX systems expect a single LF character.

The built-in editor can tell you this. If you are in the tracer, type
^X2 (that is *|Control-X|* followed by *2*). This opens a full editor
window. Then use the mouse to select *|File/Properties|*. This opens a
window that shows (among others) the encoding of the file. This field
says either =|NL=posix|= (ok) or =|NL=dos|= (wrong). In the latter case,
use a tool to convert the file. Most Unix platforms ship with =dos2unix=
for this purpose.
