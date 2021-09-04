/* * module *
% climb.pl
% June 18, 1996
% John Eikenberry
%
% This file defines the agents action of climbing. 
% Comments below document the basic idea.
%
% Dec 13, 2035
% Douglas Miles
*/
% :-swi_module(user). 
%:-swi_module(modClimb, []).

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

can_move_into(_LOC,XXYY):-var(XXYY),!,fail.
can_move_into(_LOC,XXYY):-not(mudAtLoc(_,XXYY)),!.
can_move_into(_LOC,XXYY):-ground(XXYY).

%:-start_rtrace.
%:-trace.
vtActionTemplate(actClimb(vtDirection)).
%:- quietly.
%:-stop_rtrace.


baseKB:agent_call_command(Agent,actClimb(Dir)):- once(actClimb(Agent,Dir)).

% Climb - If there is nothing there to climb, move to location plus take some damage and loose charge 
actClimb(Agent,Dir) :-	
	mudAtLoc(Agent,LOC),
	from_dir_target(LOC,Dir,XXYY),
	can_move_into(LOC,XXYY),
	in_world_move(_,Agent,Dir),
	call_update_stats(Agent,trip),
	call_update_charge(Agent,actClimb).

% Object is too high to climb, or it is another agent. 
actClimb(Agent,Dir) :-	
	\+ climbable(Agent,Dir),
	call_update_stats(Agent,pulled),
	call_update_charge(Agent,actClimb).

% Successful climb
actClimb(Agent,Dir) :-	
	in_world_move(_,Agent,Dir),
	call_update_charge(Agent,actClimb).

% Test to see if agent can climb the object
climbable(Agent,Dir) :-
	mudAtLoc(Agent,LOC),
	from_dir_target(LOC,Dir,XXYY),
	mudAtLoc(Obj,XXYY),
	props(Obj,mudHeight(ObjHt)), % If object is an agent, it will fail at this point
	mudHeightOnObj(Agent,AgHt),
	mudAtLoc(Obj2,LOC), prop_or(Obj2,mudHeight,0,Obj2Ht),
	ObjHt =< (AgHt + Obj2Ht),
	ObjHt > 1.

%Record keeping
update_charge(Agent,actClimb) :- call(padd(Agent,mudEnergy(+ -5))).

prologBuiltin(padd/2).
update_stats(Agent,trip) :-  padd(Agent,mudHealth(+ -3)).

update_stats(Agent,pulled) :- call(padd(Agent,mudHealth(+ -2))),
	(add_cmdfailure(Agent,pulled)).


:- include(prologmud(mud_footer)).
