:- module(p1, [p0/0, p1/1, p2/1]).

:- use_module(library(assertions)).
:- use_module(library(typeprops)).
:- use_module(q1).

:- meta_predicate p1(0).

:- pred p1(0).

p1(Call) :-
    call(Call).

p0 :-
    p1(q1),
    p1(q2).

:- pred q(+int,int,-int).
:- pred q(+atm,atm,-atm).

p2(D) :-
    q(A,B,C),
    A=B,
    q(B,C,D),
    q(B,C,D),
    q(X,Y,Z),
    q(X,Y,Z).

q(1,1,1).
