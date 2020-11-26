/* * module * 
% This is a *very* simple example of an agent for
% the predator example world.
%
% eg.predator.pl
% July 8, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
*/

% Declare the module name and the exported (public) predicates.
:-swi_module(mobPredator,[]).

% Predicates asserted during run.
% :- dynamic memory/2.

% Possible agent actions.
:- include(prologmud(mud_header)).
% :- register_module_type (planning).


tCol(mobPredator).
world_agent_plan(_World,Agent,Act):-
   isa(Agent,mobPredator),
   predator_idea(Agent,Act).

predator_idea(Agent,actEat(Corpse)) :-
	mudEnergy(Agent,Charge),
	Charge < 100,
	mudPossess(Agent, List),                
	obj_memb(Corpse,List),
        isa(Corpse,tCorpse).
predator_idea(Agent,actTake(What)) :-
	mudNearBody(Agent,What),
	isa(What,tCorpse).
predator_idea(Agent,actMove(Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,iCorpseFn(_),Dir).
predator_idea(Agent,actAttack(Dir)) :-
	mudNearReach(Agent,List),
	list_object_dir_near(List,mobPrey,Dir).

% find something near and itnersting and go to it.. or find a dirrection and go that way.. or sit 

predator_idea(Agent,actMove(Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,mobPrey,Dir).

predator_idea(Agent,Act) :- 
      move_or_sit_memory_idea(Agent,Act,[tNut]).


:- include(prologmud(mud_footer)).
