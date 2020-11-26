/* * module * 
% This is isOptional (simple) monster to prowl the maze world.
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
:-swi_module(mobMonster,[]).

% Predicates asserted during run.
% :- dynamic memory/2. 
% :- dynamic  agent_list/1.

:- include(prologmud(mud_header)).
% Possible agent actions.
% :- register_module_type (planning).

tCol(mobMonster).

world_agent_plan(_World,Agent,Act):-
   isa(Agent,mobMonster),
   monster_idea(Agent,Act).
   
monster_idea(Agent,actEat(Food)) :-
	mudEnergy(Agent,Charge),
	Charge < 100,
        mudPossess(Agent, Food),
        isa_any(Food,[tFood,tCorpse]).
monster_idea(Agent,actTake(Food)) :-
	mudNearBody(Agent,What),
	isa_any(Food,[tFood,tCorpse]),
	obj_memb(Food,What).
monster_idea(Agent,actMove(1,Dir)) :-
	mudGetPrecepts(Agent,List),
	isa_any(Food,[tFood,tCorpse]),
	list_object_dir_sensed(_,List,Food,Dir).
monster_idea(Agent,actAttack(Dir)) :-
	mudNearReach(Agent,List),
	list_agents(Agents),
	isa_any(NearAgt,Agents),
	list_object_dir_near(List,NearAgt,Dir).
monster_idea(Agent,actMove(1,Dir)) :-
	mudGetPrecepts(Agent,List),
	list_agents(Agents),
	isa_any(NearAgt,Agents),
	list_object_dir_sensed(_,List,NearAgt,Dir).

monster_idea(Agent,Act) :- move_or_sit_memory_idea(Agent,Act,[tCorpse]).

tCol(tClothing).
prologHybrid(wearsClothing/2).

%:-wdmsg(wearsClothing/2).
%:-prolog.
%:-visible(+all),leash(+all),trace.
:-ain(meta_argtypes(wearsClothing(tObj,tClothing))).
%:-quietly.
%:- prolog.

% TODO fingure out why term_expansion is not working
%:-visible(+all),leash(-all),trace.
%instTypeProps(Instance,mobMonster,[mudDescription(txtFormatFn("Very screy looking monster named ~w",[Instance])),wearsClothing(tToughHide),mudPossess(tToughHide)]).
%:-quietly.
%:- prolog.


:- include(prologmud(mud_footer)).
