% DEPTH-BOUNDED META-INTERPRETER
% CILog code. Copyright David Poole, 1997.

% bprove(G,D) is true if G can be proven with depth no more than D.
% This assumes that D is bound when called.

bprove(true,D).
bprove((A & B),D) <-
   bprove(A,D)&
   bprove(B,D).
bprove(H,D) <-
   D >= 0 &
   D1 is D-1 &
   (H <= B) &
   bprove(B,D1).
