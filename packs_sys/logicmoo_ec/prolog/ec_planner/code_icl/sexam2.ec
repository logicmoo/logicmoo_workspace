% Simple example for the independent choice logic
% Copyright 1997, David Poole, all rights reserved.

na <- ~ a.
nna <- ~ na.
a <- a1 & b1 & c1 & e1.
a <- a2 & b1 & d1 & f1.
a <- b2 & c2.

p <- g & ~ a.
np <- ~ p.
f <- a & ~ np.
tr <- (~ np) ; na & ng ; (~ na).
u <- ~ np.
u <-  na & ng.
u <- ~ na.
g <- a1 & b1.
g <- a2 & c1.
ng <- ~ g.
random([a1:0.3,a2:0.7]).
random([b1:0.4,b2:0.6]).
random([c1:0.2,c2:0.8]).
random([d1:0.1,d2:0.9]).
random([e1:0.1,e2:0.9]).
random([f1:0.1,f2:0.9]).
controllable([s,t]).

% Are these rules all disjoint?
% Try these. What should they return?
%| ?- explain(a,[]).
%| ?- explain(na,[]).
%| ?- check(a,[]).
%| ?- check(na,[]).
