% Computational Intelligence: a logical approach. 
% Prolog Code.
% DELIVERY ROBOT DOMAIN IN THE SITUATION CALCULUS
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% `<-' is the object-level `if' - it is an infix meta-level predicate
:- op(1150, xfx, <- ).

% `&' is the object level conjunction.
% It is an infix meta-level binary function symbol:
:- op(950,xfy, &).

% `~' is the object level negation as failure.
% It is an prefix meta-level binary function symbol:
:- op(900,fy, ~).

% `\=' is the object level not equal.
% It is an infix meta-level binary function symbol:
:- op(700,xfx, \=).


% INITIAL SITUATION

sitting_at(rob,o109,s_0) <- true.
sitting_at(parcel,lng,s_0) <- true.
sitting_at(k1,mail,s_0) <- true.
%locked(door1,s_0) <- true.

% DERIVED RELATIONS

at(Obj,Pos,S) <-
   sitting_at(Obj,Pos,S).
at(Obj,Pos,S) <-
   carrying(Ag,Obj,S) &
   at(Ag,Pos,S).

adjacent(o109,o103,_) <- true.
adjacent(o103,o109,_) <- true.
adjacent(o109,lng,_) <- true.
adjacent(lng,o109,_) <- true.
adjacent(o109,o111,_) <- true.
adjacent(o111,o109,_) <- true.
adjacent(o103,mail,_) <- true.
adjacent(mail,o103,_) <- true.
adjacent(lab2,o109,_) <- true.
adjacent(P_1,P_2,S) <-
   blocks(Door,P_1,P_2)&
   unlocked(Door,S).

% STATIC RELATIONS
blocks(door1,o103,lab2) <- true.
opens(k1,door1) <- true.
autonomous(rob) <- true.

% ACTION PRECONDITIONS
poss(move(Ag,Pos,Pos_1),S)  <- 
   autonomous(Ag) &
   adjacent(Pos,Pos_1,S) &
   sitting_at(Ag,Pos,S).
poss(pickup(Ag,Obj),S) <-
   autonomous(Ag) &
   Ag \= Obj &
   at(Ag,Pos,S)&
   sitting_at(Obj,Pos,S).
poss(putdown(Ag,Obj),S) <-
   carrying(Ag,Obj,S).
poss(unlock(Ag,Door),S) <-
   autonomous(Ag) &
   blocks(Door,P_1,_)&
   at(Ag,P_1,S)&
   opens(Key,Door)&
   carrying(Ag,Key,S).

% PRIMITIVE PREDICATE DEFINITIONS

sitting_at(Obj,Pos,do(move(Obj,Pos_0,Pos),S))  <-
   poss(move(Obj,Pos_0,Pos),S).

sitting_at(Obj,Pos,do(putdown(Ag,Obj),S))  <-
   poss(putdown(Ag,Obj),S)&
   at(Ag,Pos,S).

sitting_at(Obj,Pos,do(A,S) )  <- 
   poss(A,S) & 
   sitting_at(Obj,Pos,S) & 
   ~ ismove(A,Obj,Pos) &
   ~ ispickup(A,Obj).
ismove(move(Obj,Pos,_),Obj,Pos) <- true.
ispickup(pickup(_,Obj),Obj) <- true.

carrying(Ag,Obj,do(pickup(Ag,Obj),S)) <-
   poss(pickup(Ag,Obj),S).

carrying(Ag,Obj,do(A,S)) <-
   carrying(Ag,Obj,S) &
   poss(A,S) &
   A \= putdown(Ag,Obj).

%locked(Door,do(A,S)) <-
%   locked(Door,S) &
%   poss(A,S) &
%   A \= unlock(Door).

unlocked(Door,do(unlock(Ag,Door),S)) <-
   poss(unlock(Ag,Door),S).
unlocked(Door,do(A,S)) <-
   unlocked(Door,S) &
   poss(A,S).

% TRY the following queries:
% bprove(at(parcel,o111,S),12).
% bprove(carrying(rob,k1,S),10).
% bprove(at(rob,lab2,S),17).
% bprove(at(rob,lab2,S),15). % warning: this takes a long time
% bprove(at(rob,lab2,S),20).
