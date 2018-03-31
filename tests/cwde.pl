:- module(cwde, [p/1, q/1, r/2, s/2]).

:- meta_predicate a(0), b(0).

a(X) :- call(X).

b(X) :- call(X).

p(B) :-
    ( a(Y)
    ; b(Y)
    ),
    clause(Y, B).

q(B) :-
    a(Y),
    clause(Y, B).

r(Y, B) :-
    ( a(Y)
    ; b(Y)
    ),
    clause(Y, B).

s(Z, B) :-
    Z=Y,
    clause(Y, B).
