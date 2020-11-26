/* * module * 
% This is a *very* simple example of an agent meant to be 
% used as prey (dead prey turns into food) in simple simulations.
%
% prey.pl
%
% July 8, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
*/

:- include(prologmud(mud_header)).

% Declare the module name and the exported (public) predicates.
:-swi_module(mobPrey,[]).

:- include(prologmud(mud_header)).
% :- register_module_type (planning).
% :- register_module_type (mtCommand).

ttAgentType(mobPrey).
resultIsa(aPreyFn(ftInt),mobPrey).

% Predicates asserted during run.
% :- dynamic memory/2. 
%:- dynamic agent_list/1.

world_agent_plan(_World,Self,Act):-
   isa(Self,mobPrey),
   prey_idea(Self,Act).
   
% Possible agent actions.
prey_idea(Self,actMove(Dir)) :-
	mudGetPrecepts(Self,List),
	list_agents(Agents),
	obj_memb(NearAgnt,Agents),
	list_object_dir_sensed(_,List,NearAgnt,OppDir),
	reverse_dir(OppDir,Dir),
	number_to_dir(Num,Dir,vHere),
	nth1(Num,List,What),
	What == [].
prey_idea(Self,actTake(tNut)) :-
	mudNearBody(Self,What),
	member(tNut,What).
prey_idea(Self,actEat(tNut)) :-
	mudEnergy(Self,Charge),
	Charge < 120,
	mudPossess(Self,tNut).
prey_idea(Self,actMove(Dir)) :-
	mudGetPrecepts(Self,List),
	list_object_dir_sensed(_,List,tNut,Dir).
prey_idea(_Agent,_) :-
	actSpawnPrey.

prey_idea(Agent,Act) :- move_or_sit_memory_idea(Agent,Act,[tNut]).



% spawn new prey
% maybe(N) == N chance of each agent spawning a new agent each turn

vtActionTemplate(actSpawn(tCol)).

agent_command(_Agent,actSpawn(mobPrey)):-actSpawnPrey.

actSpawnPrey :-
	% maybe(10),
	spawn_prey(1),
	!,
	fail.

% Doesn't actually create a new prey, but revives a dead one.
% This allows for absolute control of the max number of prey 
% which is important for performance considerations
% (too many prey slows the simulation to a crawl)
% 10 slows my P5-100 down a lot... 
spawn_prey(10) :-
	!.
spawn_prey(N) :-
       Prey = aPreyFn(N),
       assert_isa(Prey,mobPrey),
       put_in_world(Prey),!.

       /*
       get_instance_default_props(Prey,Traits),
	\+ mudAgentTurnnum(Prey,_),
         req1(predInstMax(Prey,mudEnergy,NRG)),
         req1(predInstMax(Prey,mudHealth,Dam)),
         clr(mudEnergy(Prey,_)),
         clr(mudHealth(Prey,_)),
         ain(mudEnergy(Prey,NRG)),
         ain(mudHealth(Prey,Dam)),
         ain(mudAgentTurnnum(Prey,1)),
         clr(mudPossess(Prey,_)),
	set_stats(Prey,Traits),
	
        % add_missing_instance_defaults(Prey),
	!.
        */

spawn_prey(N) :-
	Ntemp is N + 1,
	spawn_prey(Ntemp).


:- include(prologmud(mud_footer)).
