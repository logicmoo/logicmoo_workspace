---+ SWI-Prolog --- Source-level debugger

SWI-Prolog and XPCE concentrate on program development by offering an
environment that is especially suitable for (rapid) prototyping and
debugging. This is supported by the fast incremental SWI-Prolog
compiler, the debugger that is capable of debugging compiled code and
the built-in commandline editor that supports Prolog specific
completion.

Tracing Prolog programs using a traditional 4-port debugger on a
terminal is difficult. Large Prolog terms make the trace hard to read,
difficult to understand determinism and backtracking, and hard to
examine the running clauses. That is why SWI-Prolog provides a
source-level debugger.

---++ The graphical tracer

The XPCE-based graphical tracer is the cornerstone of the IDE
environment and probably the most useful tool for experts as well as for
beginners. It provides three simultaneous views on the status of the
environment:

    $ Source code :
    The current location in the source code is displayed in a window
    displaying the actual source code or, if the clause is asserted, in
    a window displaying the decompiled predicate. Colours are used to
    indicate the status, green meaning normal forward calling, red
    failure, yellow redo and purple exception.

    $ Bindings :
    The bindings window displays the binding of the local variables of
    the selected frame. Variables are indicated by their true name. A
    concise display, clearly indicating which variables share the same
    value and removing unbound variables, is provided. Values can be
    examined by double-clicking.

    $ Stack :
    The stack-view not only provides the call-stack, but also the
    choice-point chain. The latter is notably useful to detect
    (undesired) non-determinism.

			[[guitracer.gif]]

---++ The XPCE/SWI-Prolog GUI tracer in action

In this picture, the *|top-left|* window indicates the binding. *B* is
not listed as this variable is unbound. The *|top-right|* window
displays the call-stack. The icon indicates the called predicate is a
normal user-defined predicate. The [[det.gif]] icon indicates the call
is deterministic, while the [[ndet.gif]] icon indicates the frame has
choice-points left. The user can click on any frame to switch both
source and bindings window to the clicked frame.  The trace was started
using

==
?- gtrace, test_chat.
==

---++ Starting the source-level debugger

The source-level debugger is controlled by gtrace/0 or gspy/1. These
predicates enable the debugger using guitracer/0 and then call trace/0
or spy/1. Like trace/0, gtrace/0 can be called from anywhere in a
program to start debugging at a specific location.  In the unlikely
event that you want to switch back to the traditional tracer in the
same session, call noguitracer/0.

==
?- gspy(dubious/1).
% The graphical front-end will be used for subsequent tracing
Spy point on dubious/1
true.

[debug] ?- go.
==

---++ Debugging threads

The source-level debugger is also the tool of choice for debugging
threads. This is supported by means of tspy/1, tdebug/0 and tnodebug/0.
See also [[the web-server debugging
hints][<howto/http/Developing.html>]]
