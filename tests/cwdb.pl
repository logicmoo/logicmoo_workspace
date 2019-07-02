:- module(cwdb, [b/1, p/1, q/1, r/1]).

:- dynamic b/1.

p(_) :-
    q(a).

q(a).
q(b).

r(c).
r(d).
