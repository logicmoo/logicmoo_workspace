%  Messy Blocks World
%  
%  This is a version that has a move for each object at each state. If the
%  agent doesn't move it, nature will.
%
%  We select a subset of action, nature selects others.  After each action,
%  we need to check the world to see what nature actually did.
%  

% First a couple of auxiliary relations
% 
% The above and below relations
:- expects_dialect(icl).

% U is above v
not_above(U,V,S) <- on(U,table,S) & V \= table.
not_above(U,V,S) <- U \= V & block(W) & on(U,W,S) & W \= V & not_above(W,V,S).

% U is below v
below(U,V,S) <- above(V,U,S).
not_below(U,V,S) <- not_above(V,U,S).

%---------------%
%  The Actions  %
%---------------%

% It may be weird to put priors on these, but the priors represent what happens
%  if we don't intervene.

random([puton(X,Z,S):0.0,
              careful(X,Z,S):0.0,
              clamp(X,S):0.1,
              nothing(X,S):0.9]).


% puton(X,Z,S) means X is out onto Z is state S
% careful(X,Z,S) means X is out onto Z is state S
% clamp(X,S) means that we are holding X to make sure it doesn't move
% nothing(X,S) means that we are doing nothing to X

%------------------%
%  puton(X,Z,S)  %
%------------------%

% preconds true
%
% If the preconditions are true, then one of these happen.

random([puton_success(X,S): 0.8,
        puton_topple(X,S): 0.05,
        puton_drop(X,S): 0.15]).

% preconds false
%
% If the preconditions are false, then either nothing happens, or the
% whole tower that x is in topples.  We should really discriminate
% precondition failures much better, but for now, this will do.  (i.e.,
% messup could mean that X is not clear or z is not clear or X is below
% z or X is already on z, etc.).

random([puton_same(X,S): 0.8,
        puton_messup(X,S): 0.2]).

% preconditions

puton_preconds(X,Z,S) <- clear(X,S) & clear(Z,S) & X \= Z & X\= table.

not_puton_preconds(table,_,_) <- true.
not_puton_preconds(X,X,_) <- X \= table.
not_puton_preconds(X,Z,S) <-
   block(W) & on(W,X,S) & X\= table.
not_puton_preconds(X,Z,S) <-
   clear(X,S) & block(W) & on(W,Z,S) & X\= table.
%-------- Effects

% preconds true

% It so happens that Y always becomes clear as long as puton is
% possible and attempted.

clear(Y,S+1) <- block(Z) & block(X) & puton(X,Z,S) & on(X,Y,S) 
       & puton_preconds(X,Z,S) & Y\= table.

clear(table,_) <- true.

% It also happens that X always becomes clear as long as puton is
% possible and attempted.

% clear(X,S+1) <- block(Z) & puton(X,Z,S) & puton_preconds(X,Z,S).


% preconds true, success
%
% Success means the normal thing happens

on(X,Z,S+1) <- puton(X,Z,S) & puton_success(X,S)
        & puton_preconds(X,Z,S).

% preconds true, topple
%
% Topple means that the target tower (Z) is toppled (as well as X).
% N.B. this interacts with the nothing relation.

on(W,table,S+1) <- block(X) & block(Z) & puton(X,Z,S) & puton_topple(X,S)
        & puton_preconds(X,Z,S) & ~ clamp(Z,S)
        & below_unclamped(W,Z,S).
on(Z,table,S+1) <- block(X) & puton(X,Z,S) & puton_topple(X,S)
        & puton_preconds(X,Z,S)  & ~ clamp(Z,S) & Z\= table. 
% & on(Z,W,S) & W \=table.
on(X,table,S+1) <- block(Z) & puton(X,Z,S) & puton_topple(X,S)
        & puton_preconds(X,Z,S).
clear(W,S+1) <- block(X) & block(Z) & puton(X,Z,S) & puton_topple(X,S)
        & puton_preconds(X,Z,S)
        & below(W,Z,S).
wrongon(U,V,S+1) <- block(X) & block(Z) & puton(X,Z,S) & puton_topple(X,S)
        & puton_preconds(X,Z,S)
        & on(U,V,S)
        & V \= (table)
        & U \= X
        & not_below(U,Z,S).
%clear(U,S+1) <- puton(X,Z,S) & puton_topple(X,S)
%        & puton_preconds(X,Z,S)
%        & clear(U,S) & U \= table & U\= X
%        & not_below(U,Z,S).
below_unclamped(U,V,S) <- on(V,U,S) & ~ clamp(U,S) & U \= table.
below_unclamped(U,V,S) <- U \= (table) & block(W) & on(V,W,S) & ~ clamp(W,S) 
        & below_unclamped(U,W,S).

% preconds true, drop
%
% Drop means that we drop the block.  That's it.

on(X,table,S+1) <- block(Z) & puton(X,Z,S) & puton_drop(X,S)
        & puton_preconds(X,Z,S).
%clear(Z,S+1) <- block(X) & puton(X,Z,S) & puton_drop(X,S)
%        & puton_preconds(X,Z,S).
% preconds false, same
%
% Same means nothing changes.

on(X,Y,S+1) <-  block(Z) & puton(X,Z,S) & puton_same(X,S)
        & on(X,Y,S)
        & not_puton_preconds(X,Z,S).
clear(X,S+1) <-  block(Z) & puton(X,Z,S) & puton_same(X,S)
        & clear(X,S) & X \= (table)
        & not_puton_preconds(X,Z,S).

% These are the axioms for clear
clear(U,S+1) <- clear(U,S) & 
    ~(( block(Y) & puton(Y,U,S) 
         & puton_preconds(Y,U,S) & puton_success(Y,S))).


% preconds false, messup
%
% messup means everything in X's tower ends up on the table.
% This also interacts with "nothing" (unless clamp is true).

on(X,table,S+1) <- block(Z) & puton(X,Z,S) & puton_messup(X,S)
        & X \= (table)
        & not_puton_preconds(X,Z,S).
clear(X,S+1) <- block(Z) & puton(X,Z,S) & puton_messup(X,S)
        & X \= (table)
        & not_puton_preconds(X,Z,S).
on(W,table,S+1) <- block(X) & block(Z) & puton(X,Z,S) & puton_messup(X,S)
        & (above(W,X,S) ; below(W,X,S))
        & W \= (table)
        & not_puton_preconds(X,Z,S).
clear(W,S+1) <- block(X) & block(Z) & puton(X,Z,S) & puton_messup(X,S)
        & (above(W,X,S) ; below(W,X,S))
        & W \= (table)
        & not_puton_preconds(X,Z,S).
on(U,V,S+1) <-  block(X) & block(Z) & puton(X,Z,S) & puton_messup(X,S)
        & on(U,V,S)
        & not_above(U,X,S) 
        & not_below(U,X,S)
        & not_puton_preconds(X,Z,S).
clear(U,S+1) <-  block(X) & block(Z) & puton(X,Z,S) & puton_messup(X,S)
        & not_puton_preconds(X,Z,S)& U \= (table)
        & clear(U,S).


%--------------------%
%  careful(X,Z,S)  %
%--------------------%


%  The axioms for careful are exactly the same as for puton except that
%  the priors over the possible outcomes are different.

%--------------------%
%  clamp(X,S)        %
%--------------------%

on(X,Y,S+1) <- on(X,Y,S) & clamp(X,S).

%--------------------%
%  nothing(X,S)        %
%--------------------%

on(X,Y,S+1) <- nothing(X,S) 
               & on(X,Y,S)  
               & above_dont_topple(X,S).   
%               & protected_above_from_messup(X,S) 
%               & protected_below_from_messup(X,S)

above_dont_topple(X,S) <- clear(X,S) & 
   block(Y) & puton(Y,X,S) & puton_success(Y,S).
above_dont_topple(X,S) <- clear(X,S) & 
   block(Y) & puton(Y,X,S) & puton_drop(Y,S).
above_dont_topple(X,S) <- clear(X,S) &
   ~ (block(Y) & puton(Y,X,S)).
above_dont_topple(X,S) <- block(Z) & on(Z,X,S) & block(W) & puton(Z,W,S) 
   & puton_preconds(Z,W,S)
   & ~ (block(Y) & puton(Y,X,S)).
above_dont_topple(X,S) <-
   block(Y) & on(Y,X,S) &
   clamp(Y,S).
above_dont_topple(X,S) <-
   block(Y) & on(Y,X,S) &
   nothing(Y,S) &
   above_dont_topple(Y,S).

   
%  Initial state:
%
%                 b
%                 c      e
%          a      d      f
%      ----------------------

on(a,table,0) <- true.
clear(a,0) <- true.
on(b,c,0) <- true.
on(c,d,0) <- true.
on(d,table,0) <- true.
clear(b,0) <- true.
on(e,f,0) <- true.
on(f,table,0) <- true.
clear(e,0) <- true.

block(_) <- true.
% block(a) <- true.
% block(b) <- true.
% block(c) <- true.
% block(d) <- true.
% block(e) <- true.
% block(f) <- true.

%  Goal state
%
%  We happen to have a goal state for this domain.

% explain(on(b,a,(0+1)+1),[],[puton(b,a,0),puton(c,b,0+1)]).
% explain(on(b,table,(0+1)+1),[],[puton(b,a,0),puton(c,b,0+1)]).
% explain(on(b,X,(0+1)+1),[],[puton(b,a,0),puton(c,b,0+1)]).
% explain(~((on(b,table,(0+1)+1) ; on(b,a,(0+1)+1) )), [], [puton(b,a,0),puton(c,b,0+1)]).
% explain(clear(b,(0+1)),[],[puton(b,a,0),puton(c,b,0+1)]).
% explain(on(a,X,0+1+1),[],[puton(b,a,0),puton(c,b,0+1)]).
% explain(on(b,X,0+1),[],[puton(b,a,0)]).
% explain(on(d,X,0+1+1),[],[puton(b,a,0),puton(c,b,0+1)]).
% explain(~ on(d,table,0+1+1),[],[puton(b,a,0),puton(c,b,0+1)]).
% explain(on(e,X,0+1+1),[],[puton(b,a,0),puton(c,b,0+1)]).
% explain(~ on(e,f,0+1+1),[],[puton(b,a,0),puton(c,b,0+1)]).
% explain(on(f,X,0+1+1),[],[puton(b,a,0),puton(c,b,0+1)]).
% explain(~ on(f,table,0+1+1),[],[puton(b,a,0),puton(c,b,0+1)]).


goal(S) <- on(d,c,S)
        & on(c,b,S)
        & on(b,a,S)
        & on(a,table,S)
        & clear(d,S).

%  Utility

utility(U) <- goal(S) & U is 10-S.


% & ~ (goal(t) & t < s)
