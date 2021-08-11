---+ ERROR: No permission to modify static_procedure Name/Arity

This message happens is there are clauses for Name/Arity in your program
and you try to modify this predicate using assert/1, retract/1 or a
related predicate.

---++ What is wrong?

Normally, *compiled* code results in *static* predicates. These cannot
be modified at runtime. Knowing code is static helps the compiler. If
you want to make _dynamic_ code, i.e., code that can be modified at
runtime, use the dynamic/1 directive. Normally you place this directive
near the start of a file or just above the code:

==
:- dynamic arc/2.

arc(a, b).
arc(a, c).
...
==

Generally it is not a very good idea to load dynamic data as program
data. If some rule depends partly on static data and partly on dynamic
define them as separate predicates and write a predicate combining the
two types of data properly:

==
:- dynamic
	user_arc/2.

arc(A, B) :-
	user_arc(A, B).
arc(A, B) :-
	predefined_arc(A, B).

predefined_arc(a, b).
...
==

Though assert/1 on a not-yet-existent predicate creates the predicate as
dynamic, it is good practice to declare all dynamic code using the
dynamic/1 directive.

---++ Abolish is evil

The predicate abolish/1 deletes all traces of a predicate from the
program: its clauses, its attributes (dynamic, multifile, meta) and its
source-information. Normally you just want to delete the clauses, so you
use retractall/1, e.g.:

==
	...,
	retractall(arc(,)),
==

See also ReadDynamicFromFile.txt
