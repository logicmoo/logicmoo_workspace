% Pesky Blocks world
%
% We assume the following relations:
%
%    p(Xs,S) is true if predicate p is true of Xs in state S
%
%    poss(A,S) is true if action A is possible in state S
%
%    clips(P,A,S) is true if action A prevents proposition P from persisting in
%                 state S.
%
%    a(Xs,S) is true if action a is carried out on Xs in state S
:- expects_dialect(icl).


% The following are the actions that can occur. The priors represent what 
%  happens if we don't intervene.

random([puton(X,Z,S):0.0,
              careful(X,Z,S):0.0,
              clamp(X,S):0.1,
              nothing(X,S):0.9]).


% Some derived relations
%

% clear(X,S) is true if there is nothing on X in state S, or
%  X is the table.

clear(X,S) <- ~ somethingon(X,S) & X\= table.
clear(table,S) <- true.

somethingon(X,S) <- on(Z,X,S).

%------------------%
%  puton(X,Z,S)  %
%------------------%

% preconds true
%
% If the preconditions are true, then one of these happen.

random([puton_success(X,S): 0.8,
        puton_topple(X,S): 0.05,
        puton_drop(X,S): 0.15]).

% preconditions are true

preconds(puton(X,Z),S) <- 
     clear(X,S) & 
     clear(Z,S) & 
     X \= Z & 
     X \= table.

on(X,Z,S+1) <- puton(X,Z,S) & puton_success(X,S)
        & preconds(puton(X,Z),S).

on(W,table,S+1) <- puton(X,Z,S) 
        & puton_topple(X,S)
        & preconds(puton(X,Z),S)
        & below_unclamped(W,Z,S).

on(X,table,S+1) <- puton(X,Z,S) & puton_topple(X,S)
        & preconds(puton(X,Z),S).

% below_unclamped(U,V,S) is true if U is V or is below V and is not
%   clamped and there are no clamped blocks inbetween.
below_unclamped(U,U,S) <- 
        ~ clamp(U,S) & U \= table.
below_unclamped(U,V,S) <- 
        on(V,W,S) 
        % dmiles changed to next line: & ~ clamped(W,S)
	& ~ clamp(W,S)
        & below_unclamped(U,W,S).

% preconds true, drop
%
% Drop means that we drop the block.  That's it.

on(X,table,S+1) <- puton(X,Z,S) & puton_drop(X,S)
        & preconds(puton(X,Z),S).

clipped(on(X,Y),S) <-
   puton(X,Z,S)
   & preconds(puton(X,Z),S).
clipped(on(A,B),S) <-
   puton(X,Z,S)
   & preconds(puton(X,Z),S)
   & puton_topple(X,S)
   & below_unclamped(A,Z,S).




% Frame Axiom
% relation on holds in the next state if it held in the previous 
% state and there was no action to clip it. 

holds_at(on(X,Y),S+1) <-
   holds_at(on(X,Y),S) &
   ~ clipped(on(X,Y),S).

% preconds false
%
% If the preconditions are false, then either nothing happens, or the
% whole tower above X is toppled, and those below X are toppled unless held.

random([puton_same(X,S): 0.8,
        puton_messup(X,S): 0.2]).

clipped(on(A,B),S) <-
   puton(X,Z,S)
   & ~ preconds(puton(X,Z),S)
   & puton_messup(X,S)
   & (below_unclamped(A,X,S) 
%     ; A=X
     ; above(on(A,B),X,S) ).

above(on(A,B),B,S) <-
   on(A,B,S).

above(on(A,B),X,S) <-
   on(Y,X,S) &
   above(on(A,B),Y,S).

on(A,table,S+1) <-
   puton(X,Z,S)
   & ~ preconds(puton(X,Z),S)
   & puton_messup(X,S)
   & (below_unclamped(A,X,S) 
%     ; A=Z
     ; above(on(A,B),X,S) ).

%  Initial state:
%
%                 b
%                 c      e
%          a      d      f
%      ----------------------

on(a,table,0) <- true.
on(b,c,0) <- true.
on(c,d,0) <- true.
on(d,table,0) <- true.
on(e,f,0) <- true.
on(f,table,0) <- true.
			      
explain(on(b,a,0+1+1),[],[puton(b,a,0),puton(c,b,0+1)]).
% explain(on(b,table,0+1+1),[],[puton(b,a,0),puton(c,b,0+1)]).
% explain(on(b,X,0+1+1),[],[puton(b,a,0),puton(c,b,0+1)]).
% explain(~((on(b,table,0+1+1) ; on(b,a,0+1+1) )), [], [puton(b,a,0),puton(c,b,0+1)]).
% explain(clear(b,0+1),[],[puton(b,a,0),puton(c,b,0+1)]).
% explain(on(a,X,0+1+1),[],[puton(b,a,0),puton(c,b,0+1)]).
explain(on(b,X,0+1),[],[puton(b,a,0)]).
% explain(~((on(b,a,0+1);on(b,table,0+1))),[],[puton(b,a,0)]).
% explain(on(d,X,0+1+1),[],[puton(b,a,0),puton(c,b,0+1)]).
% explain(~ on(d,table,0+1+1),[],[puton(b,a,0),puton(c,b,0+1)]).
% explain(on(e,X,0+1+1),[],[puton(b,a,0),puton(c,b,0+1)]).
% explain(~ on(e,f,0+1+1),[],[puton(b,a,0),puton(c,b,0+1)]).
% explain(on(f,X,0+1+1),[],[puton(b,a,0),puton(c,b,0+1)]).
% explain(~ on(f,table,0+1+1),[],[puton(b,a,0),puton(c,b,0+1)]).
explain(on(X,table,0+1),[],[puton(c,a,0)]).
