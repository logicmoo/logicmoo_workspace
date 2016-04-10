:- module(p1, [p0/0, p1/1]).

:- use_module(library(assertions)).

:- use_module(q1).

:- meta_predicate p1(0).

:- pred p1(0).

p1(Call) :-
    call(Call).

p0 :-
    p1(q1),
    p1(q2).
