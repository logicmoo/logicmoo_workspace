% CILog code for the electrical environment.
% This is the code discussed in Section 3.2 of Computational Intelligence.
% Copyright (c) Poole, Mackworth and Goebel and Oxford University Press, 1998

% lit(L) is true if light L is lit.
lit(L) <-
   light(L) &
   ok(L) &
   live(L).

% live(W) is true if W is live (i.e., current will flow through it if grounded)
live(W) <-
   connected_to(W,W1) &
   live(W1).

live(outside).

% connected_to(W0,W1) is true if W0 is connnected to W1 such that current will
% flow from W1 to W0.

connected_to(l1,w0).
connected_to(w0,w1) <- up(s2).
connected_to(w0,w2) <- down(s2).
connected_to(w1,w3) <- up(s1).
connected_to(w2,w3) <- down(s1).
connected_to(l2,w4).
connected_to(w4,w3) <- up(s3).
connected_to(p1,w3).
connected_to(w3,w5) <- ok(cb1).
connected_to(p2,w6).
connected_to(w6,w5) <- ok(cb2).
connected_to(w5,outside).

% light(L) is true if L is a light
light(l1).
light(l2).

% up(S) is true if switch S is up
% down(S) is true if switch S is down

up(s2).
down(s1).
up(s3).

% ok(X) is true if circuit breaker or light X is working (not blown)
ok(l1).
ok(l2).
ok(cb1).
ok(cb2).

% EXAMPLE QUERIES
% ask up(X).
% ask connected_to(w0,W).
% ask connected_to(w1,W).
% ask connected_to(Y,w3).
% ask lit(L).
% ask live(L).
