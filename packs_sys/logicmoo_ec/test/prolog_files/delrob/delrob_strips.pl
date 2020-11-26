% DEFINITION OF DELIVERY ROBOT WORLD IN STRIPS NOTATION
:- use_module(library(planner_api)).

%:- planner_add_workspace(ws1).
% ACTIONS
% move(Ag,Pos,Pos_1) is the action of Ag moving from Pos to Pos_1

:- planner_add_action(_WS,
   move(Ag,Pos,Pos_1),
   [precondition:[autonomous(Ag), adjacent(Pos,Pos_1), sitting_at(Ag,Pos)],
          effect:[sitting_at(Ag,Pos_1),not(sitting_at(Ag,Pos))]]).

% pickup(Ag,Obj,Pos) is the action of agent Ag picking up Obj.
:- planner_add_action(_WS,pickup(Ag,Obj),
   [precondition:[autonomous(Ag), Ag \= Obj, location(Pos),
                       sitting_at(Obj,Pos), at(Ag,Pos) ],
          effect:[pickup(Ag,Obj), carrying(Ag,Obj), not(sitting_at(Obj,Pos))]]).

% putdown(Ag,Obj,Pos)
:- planner_add_action(_WS,putdown(Ag,Obj,Pos), 
   [precondition:[autonomous(Ag),  Ag \= Obj, at(Ag,Pos), carrying(Ag,Obj)],
          effect:[sitting_at(Obj,Pos),not(carrying(Ag,Obj))]]).

% unlock(Ag,Door)
:- planner_add_action(_WS,unlock(Ag,Door),
   [precondition:[
        autonomous(Ag), blocks(Door,P_1,_), opens(Key,Door),  
        carrying(Ag,Key), at(Ag,P_1)],
   effect:[unlocked(Door)]]).

% PRIMITIVE RELATIONS
:- planner_add_predicate(_WS,carrying(_,_)).
:- planner_add_predicate(_WS,sitting_at(_,_)).
:- planner_add_predicate(_WS,unlocked(_)).

% DERIVED RELATIONS

:- planner_add_derived(_WS,at(Obj,Pos),[sitting_at(Obj,Pos)] ).
:- planner_add_derived(_WS,at(Obj,Pos),[autonomous(Ag), Ag \= Obj, carrying(Ag,Obj), at(Ag,Pos)]).

:- planner_add_derived(_WS,location(o109),[]).
:- planner_add_derived(_WS,location(o103),[]).
:- planner_add_derived(_WS,location(storage),[]).
:- planner_add_derived(_WS,location(o111),[]).
:- planner_add_derived(_WS,location(mail),[]).
:- planner_add_derived(_WS,location(lab2),[]).


:- planner_add_derived(_WS,adjacent(o109,o103),[]).
:- planner_add_derived(_WS,adjacent(o103,o109),[]).
:- planner_add_derived(_WS,adjacent(o109,storage),[]).
:- planner_add_derived(_WS,adjacent(storage,o109),[]).
:- planner_add_derived(_WS,adjacent(o109,o111),[]).
:- planner_add_derived(_WS,adjacent(o111,o109),[]).
:- planner_add_derived(_WS,adjacent(o103,mail),[]).
:- planner_add_derived(_WS,adjacent(mail,o103),[]).
:- planner_add_derived(_WS,adjacent(lab2,o109),[]).
:- planner_add_derived(_WS,adjacent(P_1,P_2), [blocks(Door,P_1,P_2), unlocked(Door)]).
:- planner_add_derived(_WS,blocks(door1,o103,lab2),[]).
:- planner_add_derived(_WS,opens(k1,door1),[]).
:- planner_add_derived(_WS,autonomous(rob),[]).

% INITIAL SITUATION
:- planner_add_init(_WS,sitting_rob_at(o109)).
:- planner_add_init(_WS,sitting_parcel_at(storage)).
:- planner_add_init(_WS,sitting_at(k1,mail)).

