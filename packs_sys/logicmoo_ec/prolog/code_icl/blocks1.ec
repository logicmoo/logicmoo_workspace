%  Messy Blocks World
%  
%  We select an action, nature selects the outcome.  After each action,
%  we need to check the world to see what nature actually did.
%  
%  What we probably want to end up with: carelessly stack the first
%  few, then carefully stack the last ones.
%  
%  Possible "problem": "messup" outcome (toppling original tower) is
%  probably a fortuitous outcome, so that's what the planner should
%  plan.  Actually, may be that's fine -- first present it without
%  messup, and then with messup and argue that what it does is the
%  right thing in the first place.
%  
%  How many actions do we need???
%  
%  There is no checking against puton(table, ...) and careful(table, ...)
%  and ontable(table, ...).  (Easy enough to add).


% First a couple of auxiliary relations
% 
% The above and below relations
:- expects_dialect(icl).

:- op(1060, xfy, '&').
:- op(900,fy, ~).
:- op(700,xfx, \=).
:- op(1150, xfx, <- ).
:- op(0, fx, (table) ).

% U is above v
above(U,V,S) <- on(U,V,S) & V \= table.
above(U,V,S) <- V \= table & on(U,W,S) & above(W,V,S).

not_above(U,V,S) <- on(U,table,S) & V \= table.
not_above(U,V,S) <- on(U,W,S) & W \= V & not_above(W,V,S).

% U is below v
below(U,V,S) <- above(V,U,S).
not_below(U,V,S) <- not_above(V,U,S).

%---------------%
%  The Actions  %
%---------------%

% I still think it's weird to put priors on these.

controllable([puton(X,Z,S),
              careful(X,Z,S),
              nothing(X,S)]).

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
% z or X is already on z, etc..

random([puton_same(X,S): 0.8,
        puton_messup(X,S): 0.2,
	puton_messup(S): 0.2]).

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

clear(Y,S+1) <- puton(X,Z,S) & on(X,Y,S) & puton_preconds(X,Z,S) & Y\= table.

clear(table,_) <- true.

% preconds true, success
%
% Success means the normal thing happens

on(X,Z,S+1) <- puton(X,Z,S) & puton_success(X,S)
        & puton_preconds(X,Z,S).
on(U,V,S+1) <- puton(X,Z,S) & puton_success(X,S)
        & puton_preconds(X,Z,S)
        & on(U,V,S) & U \= X.
clear(U,S+1) <- puton(X,Z,S) & puton_success(X,S)
        & puton_preconds(X,Z,S)
        & clear(U,S) & U \= table.

% preconds true, topple
%
% Topple means that the target tower (Z) is toppled (as well as X).

on(W,table,S+1) <- puton(X,Z,S) & puton_topple(X,S)
        & puton_preconds(X,Z,S)
        & below(W,Z,S).
on(X,table,S+1) <- puton(X,Z,S) & puton_topple(X,S)
        & puton_preconds(X,Z,S).
%on(Z,table,S+1) <- puton(X,Z,S) & puton_topple(X,S)
%        & puton_preconds(X,Z,S)& Z \= table.
clear(W,S+1) <- puton(X,Z,S) & puton_topple(X,S)
        & puton_preconds(X,Z,S)
        & below(W,Z,S).
on(U,V,S+1) <- puton(X,Z,S) & puton_topple(X,S)
        & puton_preconds(X,Z,S)
        & on(U,V,S)
        & U \= X
        & not_below(U,Z,S).
clear(U,S+1) <- puton(X,Z,S) & puton_topple(X,S)
        & puton_preconds(X,Z,S)
        & clear(U,S) & U \= table
        & not_below(U,Z,S).

% preconds true, drop
%
% Drop means that we drop the block.  That's it.

on(X,table,S+1) <- puton(X,Z,S) & puton_drop(X,S)
        & puton_preconds(X,Z,S).
on(U,V,S+1) <- puton(X,Z,S) & puton_drop(X,S)
        & puton_preconds(X,Z,S)
        & on(U,V,S)
        & U \= X.
clear(U,S+1) <- puton(X,Z,S) & puton_drop(X,S)
        & puton_preconds(X,Z,S) & U \= table
        & clear(U,S).
% preconds false, same
%
% Same means nothing changes.

on(X,Y,S+1) <-  puton(X,Z,S) & puton_same(X,S)
        & on(X,Y,S)
        & not_puton_preconds(X,Z,S).
clear(X,S+1) <-  puton(X,Z,S) & puton_same(X,S)
        & clear(X,S) & X \= table
        & not_puton_preconds(X,Z,S).

% preconds false, messup
%
% messup means everything in X's tower ends up on the table.

on(X,table,S+1) <- puton(X,Z,S) & puton_messup(X,S)
        & X \= table
        & not_puton_preconds(X,Z,S).
clear(X,S+1) <- puton(X,Z,S) & puton_messup(X,S)
        & X \= table
        & not_puton_preconds(X,Z,S).
on(W,table,S+1) <- puton(X,Z,S) & puton_messup(X,S)
        & (above(W,X,S) ; below(W,X,S))
        & W \= table
        & not_puton_preconds(X,Z,S).
clear(W,S+1) <- puton(X,Z,S) & puton_messup(X,S)
        & (above(W,X,S) ; below(W,X,S))
        & W \= table
        & not_puton_preconds(X,Z,S).
on(U,V,S+1) <-  puton(X,Z,S) & puton_messup(S)
        & on(U,V,S)
        & not_above(U,X,S) 
        & not_below(U,X,S)
        & not_puton_preconds(X,Z,S).
clear(U,S+1) <-  puton(X,Z,S) & puton_messup(S)
        & not_puton_preconds(X,Z,S)& U \= table
        & clear(U,S).


%--------------------%
%  careful(X,Y,Z,S)  %
%--------------------%


%  The axioms for careful are exactly the same as for puton except that
%  the priors over the possible outcomes are different.



%  Initial state:
%
%                 b
%                 c
%          a      d
%      ----------------------

on(a,table,0) <- true.
clear(a,0) <- true.
on(b,c,0) <- true.
on(c,d,0) <- true.
on(d,table,0) <- true.
clear(b,0) <- true.

block(a) <- true.
block(b) <- true.
block(c) <- true.
block(d) <- true.

%  Goal state
%
%  We happen to have a goal state for this domain.

% explain(on(b,a,(0+1)+1),[puton(b,a,0),puton(c,b,0+1)]).
% explain(on(b,X,(0+1)+1),[puton(b,a,0),puton(c,b,0+1)]).
% explain(on(b,a,0+1),[puton(b,a,0)]).

goal(S) <- on(d,c,S)
        & on(c,b,S)
        & on(b,a,S)
        & on(a,table,S)
        & clear(d,S).

%  Utility

utility(U) <- goal(S) & U is 10-S.


% & ~ (goal(t) & t < s)
