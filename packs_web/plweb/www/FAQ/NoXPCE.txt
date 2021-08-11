---+ ERROR: (file:line): Undefined procedure: pce_begin_class/2

If you load a Prolog file containing XPCE classes using plain Prolog,
you sometimes get and sometimes don't get this error. What is the
problem?

The :- pce_begin_class directive is handled using term_expansion/2 if
XPCE is loaded. Unlike predicates, term_expansion rules are not
autoloaded. Thus, if you reference an XPCE predicate first or otherwise
force XPCE to load, all is fine, but if the first XPCE reference is a
new class it fails.

---+ What to do?

Load XPCE explicitly by starting a file or module that uses it with the
load-command:

==
:- use_module(library(pce)). 

:- pce_begin_class(my_window, window).

...

:- pce_end_class.
==
