% DELAYING META-INTERPRETER
% CILog code. Copyright David Poole, 1997.

% dprove(G,D0,D1) is true if G can be proven from the delayables in D1-D0
%   where D0 is a tail of D1.

%% Intuitively, D0 are the assumptions made before G, and D1 are the
%% assumptions made after G. D1 consists of D0 and the assumptions
%% needed to prove G.

dprove(true,D,D).
dprove((A & B),D1,D3) <-
   dprove(A,D1,D2) &
   dprove(B,D2,D3).
dprove(G,D,[G|D]) <-
   delay(G).
dprove(H,D1,D2) <-
   (H <= B) &
   dprove(B,D1,D2).

