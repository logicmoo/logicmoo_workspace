/* * module * 
% This is simple example pddl for the maze world.
%
%
% monster.pl
% July 11, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
% Declare the module name and the exported (public) predicates.
*/



% Declare the module name and the exported (public) predicates.
:-swi_module(modulePddlAgent,[]).

:- include(prologmud(mud_header)).
% :- register_module_type (planning).

ttAgentType(mobPddlAgent).

pddl_vette_idea(Agent,Act,Act):-var(Act),!,dmsg(pddl_vette_idea(Agent,Act)).
pddl_vette_idea(_,actSit,actSit):-!.
pddl_vette_idea(Agent,Act,Act):-dmsg(pddl_vette_idea(Agent,Act)).


% ==>mudLabelTypeProps('oP',mobPddlAgent,[isa(tAgent)]).
typeHasGlyph(mobPddlAgent,"oP").

world_agent_plan(_World,Agent,ActV):-
   tAgent(Agent),
   % isa(Agent,mobPddlAgent),
   pddl_idea(Agent,Act),
   pddl_vette_idea(Agent,Act,ActV).

% Possible agent actions.
pddl_idea(Agent,actEat(Elixer)) :-
	mudHealth(Agent,Damage),
	Damage < 15,
   mudPossess(Agent,List),
   obj_memb(Elixer,List),
   isa(Elixer,tElixer).

pddl_idea(Agent,actEat(tFood)) :-
	mudEnergy(Agent,Charge),
	Charge < 150,
   mudPossess(Agent,List),
   obj_memb(Food,List),
   isa(Food,tFood).

pddl_idea(Agent,actTake(Good)) :-
	mudNearBody(Agent,What),
        obj_memb(Good,What),
	isa_any(Good,[tGold,tElixer,tTreasure]).  

pddl_idea(Agent,actTake(Good)) :-
	mudNearBody(Agent,What),
        obj_memb(Good,What),
	isa_any(Good,[tFood,tUsefull,tItem]).

pddl_idea(Agent,actMove(1,Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,tTreasure,Dir).

pddl_idea(Agent,actMove(3,Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,mobMonster,OppDir),
	reverse_dir(OppDir,Dir),
	number_to_dir(N,Dir,vHere),
        nth1(N,List,What),
	What == [].

pddl_idea(Agent,actMove(1,Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,tUsefull,Dir).

pddl_idea(Agent,actMove(1,Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,tAgent,Dir).

pddl_idea(Agent,actMove(5,Dir)) :-
	mudMemory(Agent,aDirectionsFn([Dir|_])),
	number_to_dir(Num,Dir,vHere),
	mudNearReach(Agent,List),
	nth1(Num,List,What),
	What == [].

pddl_idea(Agent,actAttack(Dir)) :-
	mudNearReach(Agent,List),
	list_object_dir_near(List,mobMonster,Dir).

pddl_idea(Agent,actLook) :-
        req1(mudMemory(Agent,aDirectionsFn(Old))),
	del(mudMemory(Agent,aDirectionsFn(Old))),
	random_permutation(Old,New),
	ain(mudMemory(Agent,aDirectionsFn(New))).


:- include(prologmud(mud_footer)).
