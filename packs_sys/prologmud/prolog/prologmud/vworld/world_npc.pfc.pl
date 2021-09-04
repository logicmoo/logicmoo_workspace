/*
:-swi_module(toploop_npc, [
          move_or_sit_memory_idea/3,
          npc_tick/0,
          join_npcs_long_running/0,npc_tick_tock/0,npc_tick_tock_time/1,
          command_actTick/1,
          npc_controller/2,   
          %warnOnError/1,
          get_world_agent_plan/3,
          tick_controller/2]).
*/

/* * module> 
% Uses timers to make sure all Agents get a chance to do their things
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- meta_predicate warnOnError(0).
:- meta_predicate agent_call_safely(?,?,?).


% :- include(prologmud(mud_header)).
% :- file_begin(mudcode).
:- dynamic(npc_tick_tock_time/1).
npc_tick_tock_time(60).

npc_tick_tock:-
   npc_tick_tock_time(Time),sleep(Time),
   npc_tick.

npc_tick:-
   join_npcs_long_running, 
   findall(What-Who,npc_controller(What,Who),List),!,
   my_random_member(What-Who,List),!,
   ignore(in_thread_and_join(on_x_debug(tick_controller(What,Who)))).

join_npcs_long_running.

% skip manually controled agents
npc_controller(simple_world_agent_plan,Who):- no_repeats(tAgent(Who)), \+ has_tty(Who).

tick_controller(simple_world_agent_plan,Who):- command_actTick(Who).

%:-ggtrace.

move_or_sit_memory_idea(Agent,actMove(Dir),[Outlet]) :- 
	mudMemory(Agent,aDirectionsFn([Dir|_])),
	number_to_dir(Num,Dir,vHere),
	mudNearReach(Agent,List),
	nth1(Num,List,What),
	(What == [];
	What == [Outlet]).
move_or_sit_memory_idea(Agent,actSit,_) :-
        req1(mudMemory(Agent,aDirectionsFn(Old))),
	del(mudMemory(Agent,aDirectionsFn(Old))),
	random_permutation(Old,New),
	ain(mudMemory(Agent,aDirectionsFn(New))).


command_actTick(Who):- (side_effect_prone),
   ignore(current_agent(Who)),
   must(nonvar(Who)),
   with_agent(Who,
     must_det_l((
      show_failure(current_agent(Who)),
      command_actIdea(Who,IdeaS),
      my_random_member(Idea,IdeaS),!,
      do_agent_call_plan_command(Who,Idea)))).


 
get_world_agent_plan(W,Who,Idea):-no_repeats(with_agent(Who,call_no_cuts(world_agent_plan(W,Who,Idea)))).

do_agent_call_plan_command(A,C):- t_l:agent_current_action(A,CC),dmsg(too_busy(CC,agent_call_plan_command(A,C))),!.
do_agent_call_plan_command(A,C):-   
   with_agent(A,locally_tl(agent_current_action(A,C), do_agent_action(A,C))).


command_actIdea(Who,IdeaSO):- (var(Who)->current_agent(Who);true),
  side_effect_prone,
  findall(Idea,
        (get_world_agent_plan(current,Who,Idea),
             dmsg(get_world_agent_plan(current,Who,Idea))),IdeaS),
  (IdeaS=[_,_|_]->delete_eq(IdeaS,actLook,IdeaSO);IdeaSO=IdeaS),
  (IdeaSO==[]->dmsg(noidea(actIdea(Who)));true).

baseKB:action_info(actNpcTimer(ftInt),"sets how often to let NPCs run").

baseKB:action_info(actTock,"Makes All NPCs do something brilliant").
baseKB:action_info(actTick(tAgent),"Makes some agent do something brilliant").
baseKB:action_info(actTick,"Makes *your* agent do something brilliant").

baseKB:action_info(actIdea(isOptional(tAgent,isSelfAgent)),"Makes some agent (or self) think of something brilliant").
baseKB:action_info(actProlog(ftCallable),"Call a ftCallable").

baseKB:agent_text_command(Agent,["prolog",X],Agent,actProlog(X)):-ignore(X=ftCallable).
baseKB:agent_text_command(Agent,["prolog"],Agent,actProlog(prolog_repl)).
% baseKB:agent_text_command(Agent,["tlocals"],Agent,actProlog(tlocals)).

:-export(warnOnError/1).
:-module_transparent(warnOnError/1).
warnOnError(X):-catch(X,E,dmsg(error(E:X))).

baseKB:agent_call_command(_Agent,actProlog(prolog_repl)) :- (side_effect_prone),true, prolog_repl,!.
baseKB:agent_call_command(Agent,actProlog(C)) :- (side_effect_prone),true,nonvar(C),agent_call_safely(Agent,C).

:-export(agent_call_safely/2).
agent_call_safely(_Agnt,C):- any_to_callable(C,X,Vars), !, gensym(result_count_,RC),flag(RC,_,0),!,agent_call_safely(RC,X,Vars),flag(RC,CC,CC),fmt(result_count(CC)).
agent_call_safely(RC,X,[]) :- !, call_u(quietly((warnOnError(doall(((X,flag(RC,CC,CC+1),fmt(cmdresult(X,true))))))))).
agent_call_safely(RC,X,Vars) :-  call_u(quietly((warnOnError(doall(((X,flag(RC,CC,CC+1),fmt(cmdresult(X,Vars))))))))).

atom_to_term_safe(A,T,O):-catch(atom_to_term(A,T,O),_,fail)->T\==end_of_file.

any_to_callable(C,X,Vs):- atom(C),!,atom_to_term_safe(C,XX,Vs1),!,any_to_callable0(XX,X,Vs2),term_variables((Vs1,Vs2),Vs),!.
any_to_callable(T,X,Vs):- catch(text_to_string(T,S),_,fail),string_to_atom(S,C),atom_to_term_safe(C,XX,Vs1),!,any_to_callable0(XX,X,Vs2),term_variables((Vs1,Vs2),Vs),!.
any_to_callable(C,X,Vs):- any_to_callable0(C,X,Vs).
any_to_callable0(C,X,Vs):- expand_goal(C,X),term_variables((C,X),Vs),!.
% any_to_callable(C,X,Vs):-force_expand(expand_goal(C,X)),term_variables((C,X),Vs),!.

baseKB:agent_call_command(_Agent,actNpcTimer(Time)):-retractall(npc_tick_tock_time(_)),asserta(npc_tick_tock_time(Time)).
baseKB:agent_call_command(Who,actTick) :-  on_x_debug(command_actTick(Who)).
baseKB:agent_call_command(_Agent,actIdea(Who)) :-  must(command_actIdea(Who,Idea)),fmt(result_actIdea(Who,Idea)).
baseKB:agent_call_command(_Agent,actTock) :- (side_effect_prone), npc_tick.
baseKB:agent_call_command(_Agent,actTick(Other)) :-(side_effect_prone), baseKB:agent_call_command(Other,actTick).

:- include(prologmud(mud_footer)).
