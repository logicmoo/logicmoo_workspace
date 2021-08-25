%%% Modifications of p2.nlp of ASPERIX. 
%%% Added some "domain" predicate in bodies
%%% to allow lparse execution.

p(1..20).

aa :- not bb.
bb :- not aa.

pa(X) :- aa, p(X).
pb(X) :- bb, p(X).

a(X,Y) :- pa(X), pa(Y), p(X), p(Y), not b(X,Y).
b(X,Y) :- pb(X), pb(Y), p(X), p(Y), not c(X,Y).
c(X,Y) :- a(X,Y), p(X), p(Y), X<Y.

:- aa.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
