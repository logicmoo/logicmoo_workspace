:- module(p1,
          [ e1/2,
            e2/2,
            e3/2
          ]).

:- use_module(library(neck)).

q1(1, a).
q1(2, b).
q1(3, c).

e1(A, F) :-
    q1(A, B),
    upcase_atom(B, U),
    neck,
    p2(U, C),
    p3(C, D),
    p2(D, E),
    p3(E, F).
e1(A, F) :-
    q1(A, B),
    neck,
    !,
    p2(B, C),
    p3(C, D),
    p2(D, E),
    p3(E, F).
e1(A, F) :-
    q1(A, B),
    neck,
    p2(B, C),
    p3(C, D),
    !,
    p2(D, E),
    p3(E, F).
e1(A, D) :-
    q1(B, A),
    neck,
    p2(B, C),
    p3(C, D).

e2(A, C) :-
    q1(A, B),
    neck,
    p2(B, C).

e3(A, B) :-
    q1(A, B),
    neck.

e4(A, A) :-
    neck.

p2(A, A).

p3(A, A).
