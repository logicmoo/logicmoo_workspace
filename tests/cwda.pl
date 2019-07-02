:- module(cwda, [a/0]).

:- use_module(cwdb).

a :-
    assertz(b(1)).

:- public x/0.

x :-
    p(a).
