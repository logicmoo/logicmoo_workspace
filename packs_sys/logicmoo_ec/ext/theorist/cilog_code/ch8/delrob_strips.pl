%DEFINITION OF DELIVERY ROBOT WORLD IN STRIPS NOTATION

:- expects_dialect(cilog).

% ACTIONS
% move(Ag,Pos,Pos_1) is the action of Ag moving from Pos to Pos_1

preconditions(move(Ag,Pos,Pos_1),
    [autonomous(Ag), adjacent(Pos,Pos_1), sitting_at(Ag,Pos)]).
achieves(move(Ag,Pos,Pos_1),sitting_at(Ag,Pos_1)).
deletes(move(Ag,Pos,Pos_1),sitting_at(Ag,Pos)).

% pickup(Ag,Obj,Pos) is the action of agent Ag picking up Obj.
preconditions(pickup(Ag,Obj),
    [autonomous(Ag), Ag \= Obj, location(Pos),
                       sitting_at(Obj,Pos), at(Ag,Pos) ]).
achieves(pickup(Ag,Obj), carrying(Ag,Obj)).
deletes(pickup(Ag,Obj), sitting_at(Obj,Pos)).

% putdown(Ag,Obj,Pos)
preconditions(putdown(Ag,Obj,Pos), 
    [autonomous(Ag),  Ag \= Obj, at(Ag,Pos), carrying(Ag,Obj)]).
achieves(putdown(Ag,Obj,Pos),sitting_at(Obj,Pos)).
deletes(putdown(Ag,Obj,Pos),carrying(Ag,Obj)).

% unlock(Ag,Door)
preconditions(unlock(Ag,Door),
    [autonomous(Ag), blocks(Door,P_1,_), opens(Key,Door),  
        carrying(Ag,Key), at(Ag,P_1)]).
achieves(unlock(Ag,Door),unlocked(Door)).

% PRIMITIVE RELATIONS
primitive(carrying(_,_)).
primitive(sitting_at(_,_)).
primitive(unlocked(_)).
primitive(autonomous(_)).

% DERIVED RELATIONS

clause(at(Obj,Pos),[sitting_at(Obj,Pos)] ).
clause(at(Obj,Pos),[autonomous(Ag), Ag \= Obj, carrying(Ag,Obj), at(Ag,Pos)]).

clause(location(o109),[]).
clause(location(o103),[]).
clause(location(storage),[]).
clause(location(o111),[]).
clause(location(mail),[]).
clause(location(lab2),[]).


clause(adjacent(o109,o103),[]).
clause(adjacent(o103,o109),[]).
clause(adjacent(o109,storage),[]).
clause(adjacent(storage,o109),[]).
clause(adjacent(o109,o111),[]).
clause(adjacent(o111,o109),[]).
clause(adjacent(o103,mail),[]).
clause(adjacent(mail,o103),[]).
clause(adjacent(lab2,o109),[]).
clause(adjacent(P_1,P_2), [blocks(Door,P_1,P_2), unlocked(Door)]).
clause(blocks(door1,o103,lab2),[]).
clause(opens(k1,door1),[]).
clause(autonomous(rob),[]).

% INITIAL SITUATION
holds(sitting_at(rob,o109),init).
holds(sitting_at(parcel,storage),init).
holds(sitting_at(k1,mail),init).

achieves(init,X) <-
   holds(X,init).

