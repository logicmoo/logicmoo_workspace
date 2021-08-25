%%% Test 2

q(0..20). 
p(X,Y) :- p(Z,X), q(Y),q(X),q(Z), Y = X + 1, q(Z).
p(0,1):-not p(0,2).
p(0,2):-not p(0,1).

h(X,Y) :- p(X,Y), q(X), q(Y).
h(X,Y) :- p(X,Z),h(Z,Y),q(Y), q(Z), q(X).
r(X,Y) :- h(X,Y), not p(X,Y), q(X), q(Y), q(Z).


%hide.

