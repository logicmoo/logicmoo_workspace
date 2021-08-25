%%% Test 1. A well founded program
%%% 

q(1..512). %%% enlarge upper bound freely!
p(X,Y) :- q(Y),q(X),Y = X + 1.
h(X,Y) :- p(X,Y).
h(X,Y) :- p(X,Z),h(Z,Y),q(Y).
r(X,Y) :- h(X,Y), not p(X,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%hide.
 

