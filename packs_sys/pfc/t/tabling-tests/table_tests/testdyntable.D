:- table(p/2).

p(X,Y) :- e(X,Y).
p(X,Y) :- p(X,Z),e(Z,Y).

e(1,2).
e(2,3).
e(3,4).
e(1,4).
e(4,2).
