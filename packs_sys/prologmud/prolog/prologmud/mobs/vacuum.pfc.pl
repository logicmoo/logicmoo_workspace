/* * module * 
% This is a *very* simple example of an agent for
% the vacuum cleaner example world.
%
% Maintainers: Douglas Miles/Annie Ogborn/Kino Coursey
% Contact: $Author: dmiles $@users.sourceforge.net ;
*/

% Declare the module name and the exported (public) predicates.
:-swi_module(mobVacuum,[]).

% Predicates asserted during run.
% :- dynamic memory/2.

:- include(prologmud(mud_header)).
% :- register_module_type (planning).

% Possible agent actions.

tCol(mobVacuum).
tCol(mobVacuum).

world_agent_plan(_World,Agent,Act):-
   isa(Agent,mobVacuum),
   vacuum_idea(Agent,Act).

vacuum_idea(Agent,actTake(tPowerOutlet)) :-
	mudEnergy(Agent,Charge),
	Charge < 490,
	mudNearBody(Agent,What),
	member(tPowerOutlet,What).
vacuum_idea(Agent,actTake(tDirt)) :-
	mudNearBody(Agent,What),
	member(tDirt,What).
vacuum_idea(Agent,actMove(Dir)) :-
	mudEnergy(Agent,Charge),
	Charge < 200,
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,tPowerOutlet,Dir),
	number_to_dir(N,Dir,vHere),
	nth1(N,List,What),
	(What == [];
	    What == [tDirt];
	    What == [tPowerOutlet]).
vacuum_idea(Agent,actClimb(Dir)) :-
	mudEnergy(Agent,Charge),
	Charge < 200,
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,tPowerOutlet,Dir),
	number_to_dir(N,Dir,vHere),
	nth1(N,List,What),
	(What == [tLowBox];
	    What == [tLowWall]).
vacuum_idea(Agent,actMove(Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,tDirt,Dir),
	number_to_dir(N,Dir,vHere),
	nth1(N,List,What),
	(What == [];
	What == [tDirt];
	What == [tPowerOutlet]).
vacuum_idea(Agent,actClimb(Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,tDirt,Dir),
	number_to_dir(N,Dir,vHere),
	nth1(N,List,What),
	(What == [tLowBox];
	    What == [tLowWall]).

vacuum_idea(Agent,Act) :- move_or_sit_memory_idea(Agent,Act,[tPowerOutlet]).





:- include(prologmud(mud_footer)).
