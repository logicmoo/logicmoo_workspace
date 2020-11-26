% Pesky Blocks world
%
% We assume the following relations:
%
%    holds_at(P,S) is true if P is true in state S
%
%    poss(A,S) is true if action A is possible in state S
%
%    clips(P,A,S) is true if action A prevents proposition P from persisting in
%     state S.
%
%    happens(A,S) is true if action A is carried out in state S

:- cls.
:- expects_dialect(icl).

:- clear.

% The following are the actions that can occur. The priors represent what 
%  happens if we don't intervene.

random([happens(puton(X,Z),S):0.0,
        happens(careful(X,Z),S):0.0,
        happens(clamp(X),S):0.1,
        happens(nothing(X),S):0.9]).


% Some derived relations
%

% holds_at(clear(X),S) is true if there is nothing on X in state S, or
%  X is the table.

holds_at(clear(X),S) <- ~ holds_at(somethingon(X),S) & X\= (table).
holds_at(clear(table),_) <- true.

holds_at(somethingon(X),S) <- holds_at(on(Z,X),S).

%------------------%
%  puton(X,Z,S)  %
%------------------%

% preconds true
%
% If the preconditions are true, then one of these happen.

random([success(puton(X,Z),S): 0.8,
        topple(puton(X,Z),S): 0.05,
        holds_at(puton_drop(X,Z),S): 0.15]).

% preconditions are true

preconds(puton(X,Z),S) <- 
     holds_at(clear(X),S) & 
     holds_at(clear(Z),S) & 
     X \= Z & 
     X \= (table).


initiates(puton(X,Z),on(X,Z)) <- true.

/*
holds_at(on(X,Z),S+1) 
 <- happens(puton(X,Z),S) 
  & success(puton(X,Z),S)
  & preconds(puton(X,Z),S).
*/
holds_at(F,S+1) 
 <- initiates(A,F)
  & happens(A,S) 
  & success(A,S)
  & preconds(A,S).

holds_at(on(W,table),S+1) 
 <- happens(puton(X,Z),S) 
  & topple(puton(X,Z),S)
  & preconds(puton(X,Z),S)
  & holds_at(below_unclamped(W,Z),S).

holds_at(on(X,table),S+1) 
<- happens(puton(X,Z),S) 
  & topple(puton(X,Z),S)
  & preconds(puton(X,Z),S).

% holds_at(below_unclamped(U,V),S) is true if U is V or is below V and is not
%   clamped and there are no clamped blocks inbetween.
holds_at(below_unclamped(U,U),S) <- 
  ~ happens(clamp(U),S) & U \= table.
holds_at(below_unclamped(U,V),S) 
  <-  holds_at(on(V,W),S) 
  & ~ holds_at(clamped(W),S)
  & holds_at(below_unclamped(U,W),S).

% preconds true, drop
%
% Drop means that we drop the block.  That's it.

holds_at(on(X,table),S+1) 
  <- happens(puton(X,Z),S) 
  & holds_at(puton_drop(X,Z),S)
  & preconds(puton(X,Z),S).

/*
clipped(on(X,Y),S) <-
   happens(puton(X,Z),S)
   & preconds(puton(X,Z),S).
*/

clipped(F,S) <- 
  releases(F,A,S)
  & happens(A,S)
  & preconds(A,S).

releases(puton(X,_Z),on(X,_Y),_S) <- true.

/*
clipped(on(A,B),S) <-
   happens(puton(X,Z),S)
   & preconds(puton(X,Z),S)
   & topple(puton(X,Z),S)
   & holds_at(below_unclamped(A,Z),S).
*/

releases(puton(X,Z),on(A,B),S)
  <- topple(puton(X,Z),S)
  & holds_at(below_unclamped(A,Z),S).


% Frame Axiom
% relation on holds_at in the next state if it held in the previous 
% state and there was no action to clip it. 
/*
holds_at(on(X,Y),S+1) <-
   holds_at(on(X,Y),S) &
   ~ clipped(on(X,Y),S).
*/

holds_at(F,S+1) 
  <- fluent(F) 
   & holds_at(F,S) 
   & ~(clipped(F,S)).

fluent(on(_,_)) <- true.

% preconds false
%
% If the preconditions are false, then either nothing happens, or the
% whole tower above X is toppled, and those below X are toppled unless held.

random([holds_at(puton_same(X),S): 0.8,
        messup(puton(X,Z),S): 0.2]).

clipped(on(A,B),S) <-
   happens(puton(X,Z),S)
   & ~ preconds(puton(X,Z),S)
   & messup(puton(X,Z),S)
   & (holds_at(below_unclamped(A,X),S) 
%     ; A=X
     ; holds_at(transitive(on(A,B),X),S) ).

holds_at(on(A,table),S+1) <-
   happens(puton(X,Z),S)
   & ~ preconds(puton(X,Z),S)
   & messup(puton(X,Z),S)
   & (holds_at(below_unclamped(A,X),S) 
%     ; A=Z
     ; holds_at(transitive(on(A,B),X),S) ).

transitive_decl(on(Y,X),Y,X) <- true.

holds_at(transitive(F,V),S) <-
   transitive_decl(F,V),
   holds_at(F,S).

holds_at(transitive(FAB,X),S) <-
   transitive_decl(FAB,_,_),
   transitive_decl(FYX,Y,X),
   holds_at(FYX,S) &
   holds_at(transitive(FAB,Y),S).

/*
holds_at(transitive(on(A,B),B),S) <-
   holds_at(on(A,B),S).

holds_at(transitive(on(A,B),X),S) <-
   holds_at(on(Y,X),S) &
   holds_at(transitive(on(A,B),Y),S).
*/

%  Initial state:
%
%     b
%     c      e
%    a      d      f
%      ----------------------

holds_at(on(a,table),0) <- true.
holds_at(on(b,c),0) <- true.
holds_at(on(c,d),0) <- true.
holds_at(on(d,table),0) <- true.
holds_at(on(e,f),0) <- true.
holds_at(on(f,table),0) <- true.

explain(holds_at(on(b,a),0+1+1),    [],[happens(puton(b,a),0),happens(puton(c,b),0+1)]).
explain(holds_at(on(b,table),0+1+1),[],[happens(puton(b,a),0),happens(puton(c,b),0+1)]).
explain(holds_at(on(b,X),0+1+1),    [],[happens(puton(b,a),0),happens(puton(c,b),0+1)]).
explain(~((holds_at(on(b,table),0+1+1) ; holds_at(on(b,a),0+1+1) )), [], [happens(puton(b,a),0),happens(puton(c,b),0+1)]).
explain(holds_at(clear(b),0+1),[],[happens(puton(b,a),0),happens(puton(c,b),0+1)]).
explain(holds_at(on(a,X),0+1+1),[],[happens(puton(b,a),0),happens(puton(c,b),0+1)]).
explain(holds_at(on(b,X),0+1),[],[happens(puton(b,a),0)]).
explain(~((holds_at(on(b,a),0+1);holds_at(on(b,table),0+1))),[],[happens(puton(b,a),0)]).
explain(holds_at(on(d,X),0+1+1),[],[happens(puton(b,a),0),happens(puton(c,b),0+1)]).
explain(~ holds_at(on(d,table),0+1+1),[],[happens(puton(b,a),0),happens(puton(c,b),0+1)]).
explain(holds_at(on(e,X),0+1+1),[],[happens(puton(b,a),0),happens(puton(c,b),0+1)]).
explain(~ holds_at(on(e,f),0+1+1),[],[happens(puton(b,a),0),happens(puton(c,b),0+1)]).
explain(holds_at(on(f,X),0+1+1),[],[happens(puton(b,a),0),happens(puton(c,b),0+1)]).
explain(~ holds_at(on(f,table),0+1+1),[],[happens(puton(b,a),0),happens(puton(c,b),0+1)]).
explain(holds_at(on(X,table),0+1),[],[happens(puton(c,a),0)]).
