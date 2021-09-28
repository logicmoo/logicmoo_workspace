:- begin_tests(metaprops).

:- use_module(library(assertions)).
:- use_module(library(metaprops)).
:- use_module(library(typeprops)).

test(compat) :-
    \+ compat(list(num,[_A, 1/2])).

:- end_tests(metaprops).
