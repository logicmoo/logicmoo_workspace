:- module(cwdc, []).

:- meta_predicate p(0 ), q(0 ).

r(X) :- p(X).
r(X) :- q(X).

p(X) :- call(X).
q(X) :- call(X).

s(X, Y) :-
    r(X),
    clause(X, Y).

t(X, Y) :-
    ( p(X)
    ; q(X)
    ),
    clause(X, Y).
