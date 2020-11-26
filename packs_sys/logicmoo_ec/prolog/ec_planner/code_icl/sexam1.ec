% Simple example for the independent choice logic
% Copyright 1997, David Poole, all rights reserved.

z <- a.
x <- ~ w.
w <- ~ a.
a <- s & b & h.
a <- t & f & h.
a <- t & q & h & b.
a <- q & e.
q <- h.
q <- b & e.
h <- b & f.
h <- c & e.
h <- g & b.
random([b:0.2,c:0.7,i:0.1]).
random([e:0.6,f:0.3,g:0.1]).
controllable([s,t]).

% Are these rules all disjoint?
% Try these. What should they return?
%| ?- explain(z,[t]).
%| ?- check(z,[t]).
%| ?- explain(z,[s]).
%| ?- check(z,[s]).
%| ?- explain(w,[s]).
