:- table d/1,e/1.

entry(d(_)).

% Two mutually recursive predicates:
d(X) :- e(Y), Y < 10000, X is Y + 1.
d(0).

e(X) :- d(Y), Y < 10000, X is Y + 1.
e(0).
