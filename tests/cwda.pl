:- module(cwda, [a/0]).

:- use_module(cwdb).

a :-
    assertz(b(1)).
