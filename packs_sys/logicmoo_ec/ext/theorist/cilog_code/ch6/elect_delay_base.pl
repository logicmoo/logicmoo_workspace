% OBJECT-LEVEL DEFINITION OF THE ELECTRICAL DOMAIN, WITH DELAYABLES
% CILog code. Copyright David Poole, 1997.

% Here "<=" is the object-level "if" and is a meta-level predicate.
% "&" is the object-level conjunction and is a meta-level function symbol.

% lit(L) is true if light L is lit.
lit(L) <=
   light(L) &
   ok(L) &
   live(L).

% live(W) is true if W is live (i.e., current will flow through it if grounded)
live(W) <=
   connectedto(W,W1) &
   live(W1).

live(outside) <= true.

% connectedto(W0,W1) is true if W0 is connnected to W1 such that current will
% flow from W1 to W0.

connectedto(l1,w0) <= true.
connectedto(w0,w1) <= up(s2) & ok(s2).
connectedto(w0,w2) <= down(s2) & ok(s2).
connectedto(w1,w3) <= up(s1) & ok(s1).
connectedto(w2,w3) <= down(s1) & ok(s1).
connectedto(l2,w4) <= true.
connectedto(w4,w3) <= up(s3) & ok(s3).
connectedto(p1,w3) <= true.
connectedto(w3,w5) <= ok(cb1).
connectedto(p2,w6) <= true.
connectedto(w6,w5) <= ok(cb2).
connectedto(w5,outside) <= ok(outside_connection).

% light(L) is true if L is a light
light(l1) <= true.
light(l2) <= true.

% up(S) is true if switch S is up
% down(S) is true if switch S is down

up(s2) <= true.
down(s1) <= true.
up(s3) <= true.

% ok(G) is true if G is working
% ok is delayable
delay(ok(X)).

% Example Queries:
% ask dprove(live(w6),[],D).
% ask dprove(live(p1),[],D).
% ask dprove(lit(l2),[],D).
% ask dprove(lit(l2)&live(p1),[],D).
