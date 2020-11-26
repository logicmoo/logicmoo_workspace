% DELIVERY ROBOT DOMAIN IN THE SITUATION CALCULUS

:- expects_dialect(cilog).

% INITIAL SITUATION

sitting_at(rob,o109,init).
sitting_at(parcel,storage,init).
sitting_at(k1,mail,init).
color(parcel,blue,init).
%locked(door1,init).

% DERIVED RELATIONS

at(Obj,Pos,S) <-
   sitting_at(Obj,Pos,S).
at(Obj,Pos,S) <-
   carrying(Ag,Obj,S) &
   at(Ag,Pos,S).


adjacent(o109,o103,_).
adjacent(o103,o109,_).
adjacent(o109,storage,_).
adjacent(storage,o109,_).
%adjacent(o109,o111,_).
%adjacent(o111,o109,_).
adjacent(o103,mail,_).
adjacent(mail,o103,_).
adjacent(lab2,o109,_).
adjacent(P_1,P_2,S) <-
   blocks(Door,P_1,P_2)&
   unlocked(Door,S).

% STATIC RELATIONS
blocks(door1,o103,lab2).
opens(k1,door1).
autonomous(rob).

% ACTION PRECONDITIONS
poss(move(Ag,Pos,Pos_1),S)  <- 
   autonomous(Ag) &
   adjacent(Pos,Pos_1,S) &
   sitting_at(Ag,Pos,S).
poss(pickup(Ag,Obj),S) <-
   autonomous(Ag) &
   at(Ag,Pos,S)&
   sitting_at(Obj,Pos,S)&
   Ag \= Obj .
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
ismove(move(Obj,Pos,_),Obj,Pos).
ispickup(pickup(_,Obj),Obj).

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
% bound 10.
:- trace.
askable sitting_at(parcel,o109,S).
% bound 8.
askable carrying(rob,k1,S).
% bound 16.
askable sitting_at(rob,lab2,S).
% bound 15.
askable sitting_at(rob,lab2,S).
      % warning: this takes a *very* *very* long time
askable sitting_at(parcel,lab2,S).
askable sitting_at(parcel,lab2,do(putdown(rob,parcel),do(move(rob, o103, lab2), do(unlock(rob, door1), do(move(rob, mail, o103), do(pickup(rob, k1), do(move(rob, o103, mail), do(move(rob, o109, o103), do(move(rob, storage, o109), do(pickup(rob, parcel), do(move(rob, o109, storage), init))))))))))).


