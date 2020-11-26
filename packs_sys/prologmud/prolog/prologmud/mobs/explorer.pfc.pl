/* * module * 
% This is simple example explorer for the maze world.
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
:-swi_module(mobExplorer,[]).

%:- dmsg( dumpST).

%:- backtrace(200).
 
:- include(prologmud(mud_header)).
% :- register_module_type (planning).

tCol(mobExplorer).

vette_idea(Agent,Act,Act):-var(Act),!,dmsg(vette_idea(Agent,Act)).
vette_idea(_,actSit,actSit):-!.
vette_idea(Agent,Act,Act):-dmsg(vette_idea(Agent,Act)).


prologHybrid(mudLabelTypeProps/3).

% :- show_pred_info(mudLabelTypeProps/3).


:- sanity(arity(mudLabelTypeProps,3)).

prologHybrid(typeHasGlyph(tCol,ftString)).
% mudLabelTypeProps('Px',mobExplorer,[]).
typeHasGlyph(mobExplorer,"Px").

world_agent_plan(_World,Agent,ActV):-
   tAgent(Agent),
  % isa(Agent,explorer),
   explorer_idea(Agent,Act),
   vette_idea(Agent,Act,ActV).

==> genls(tElixer,tDrinkAble).

% Possible agent actions.
explorer_idea(Agent,actEat(Elixer)) :-
	mudHealth(Agent,Damage),
	Damage < 15,
   mudPossess(Agent,List),
   obj_memb(Elixer,List),
   isa(Elixer,tElixer).

explorer_idea(Agent,actEat(tFood)) :-
	mudEnergy(Agent,Charge),
	Charge < 150,
   mudPossess(Agent,List),
   obj_memb(Food,List),
   isa(Food,tFood).

explorer_idea(Agent,actTake(Good)) :-
	mudNearBody(Agent,What),
        obj_memb(Good,What),
	isa_any(Good,[tGold,tElixer,tTreasure]).  

explorer_idea(Agent,actTake(Good)) :-
	mudNearBody(Agent,What),
        obj_memb(Good,What),
	isa_any(Good,[tFood,tUsefull,tItem]).

explorer_idea(Agent,actMove(1,Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,tTreasure,Dir).

explorer_idea(Agent,actMove(3,Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,mobMonster,OppDir),
	reverse_dir(OppDir,Dir),
	number_to_dir(N,Dir,vHere),
        nth1(N,List,What),
	What == [].

explorer_idea(Agent,actMove(1,Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,tUsefull,Dir).

explorer_idea(Agent,actMove(1,Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,tAgent,Dir).

explorer_idea(Agent,actMove(5,Dir)) :-
	mudMemory(Agent,aDirectionsFn([Dir|_])),
	number_to_dir(Num,Dir,vHere),
	mudNearReach(Agent,List),
	nth1(Num,List,What),
	What == [].

explorer_idea(Agent,actAttack(Dir)) :-
	mudNearReach(Agent,List),
	list_object_dir_near(List,mobMonster,Dir).

explorer_idea(Agent,actLook) :-
        req1(mudMemory(Agent,aDirectionsFn(Old))),
	del(mudMemory(Agent,aDirectionsFn(Old))),
	random_permutation(Old,New),
	ain(mudMemory(Agent,aDirectionsFn(New))).

:- set_prolog_flag(runtime_debug,3).

:- include(prologmud(mud_footer)).
