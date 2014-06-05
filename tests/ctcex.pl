:- module(ctcex, [a/2, q/0]).

:- use_module(library(swi/assertions)).
:- use_module(library(swi/ctchecks)).
:- use_module(library(swi/nativeprops)).
:- use_module(library(swi/basicprops)).

:- set_prolog_flag(check_assertions, [defined,
				      is_prop,
				      ctcheck
				     ]). % Now tighten the screws

:- pred a(int, list).

a(1,[]).
a(a, b).

q :-
    a(1,b),
    b(2,3).

b(2, 3) :- c.
