/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */
/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
% LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
%
% Copyright (C) 2004 Marty White under the GNU GPL
% Sept 20, 1999 - Douglas Miles
% July 10, 1996 - John Eikenberry
%
% Logicmoo Project changes:
%
% Main file.
%
*/

:- '$set_source_module'(mu).

:- meta_predicate with_agent_console(*, 0).
/*
with_agent_console(Agent, Goal):-
 mu_global:console_host_io_history_unused(Id, Alias, InStream, OutStream, Host, Peer, Agent),
 nop(mu_global:console_host_io_history_unused(Id, Alias, InStream, OutStream, Host, Peer, Agent)),
 current_input(WasIn),
 InStream\==WasIn, !,
 setup_call_cleanup(set_input(InStream), with_agent_console(Agent, Goal), set_input(WasIn)).
*/
with_agent_console(Agent, Goal):-
 term_to_atom(Agent, Mutex),
 setup_call_cleanup(
  asserta(mu_global:current_agent_tl(Agent), E),
   with_mutex(get_advstate, with_mutex(Mutex, Goal)), erase(E)), !.

with_agents(_Pred1, []):-!.
with_agents( Pred1, [Agent|More]) :- !, with_agents(Pred1, Agent), !, with_agents(Pred1, More).
with_agents( Pred1, Agent):- with_agent_console(Agent, must(call(Pred1, Agent))).


run_perceptq(Agent) :-
 declared(perceptq(Agent, PerceptQ)),
 redeclare(perceptq(Agent, [])),
 invoke_percept_list(Agent, PerceptQ), !.
run_perceptq(_Agent) :- !.

/*
:- defn_state_setter(run_perceptq(+agent)).
run_perceptq(Agent, S0, S9) :-
 % get_advstate(S0),
 undecla re(perceptq(Agent, PerceptQ), S0, S1), PerceptQ \==[],
 decl are(perceptq(Agent, []), S1, S2),
 %set_advstate(S9),
 invoke_percept_list(Agent, PerceptQ, S2, S9), !.
run_perceptq(_Agent, S0, S0).

*/

each_live_agent(NewGoal, S0, S2) :-
 get_live_agents(List, S0),
 apply_mapl_state(NewGoal, List, S0, S2).

each_sensing_thing(Sense, NewGoal, S0, S2) :-
 must_mw1((get_sensing_objects(Sense, List, S0),
  List\==[],
  %dbug1(each_sensing_thing(Sense)=(List=NewGoal)),
 apply_mapl_state(NewGoal, List, S0, S2))).

each_agent(Precond, NewGoal, S0, S2) :-
 get_some_agents(Precond, List, S0),
 apply_mapl_state(NewGoal, List, S0, S2).



% -----------------------------------------------------------------------------
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_agent_model')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Protocol:
% Agent: request(Action, Action_Id)
% Simulation: respond(Action_Id, LogicalResponse/Percept, EnglishResponse)
% Action(Verb, ...)
% failure(Reason)
% moved(agent, how, obj, from, prep)

% -----------------------------------------------------------------------------
% The state of an Agent is stored in its memory.
% Agent memory is stored as a list in reverse chronological order, implicitly
% ordering and timestamping everything.
% Types of memories:
% inst(A)  - identity of agent (?)
% timestamp(T) - agent may add a new timestamp whenever a sequence point
%      is desired.
% [percept]  - received perceptions.
% model([...]) - Agent's internal model of the world.
%      Model is a collection of timestampped relations.
% current_goals(Agent, [...]) - states the agent would like to achieve, or
%      acts the agent would like to be able to do.
% plan(S, O, B, L) - plans for achieving goals.
% affect(...)  - Agent's current affect.
% Multiple plans, goals, models, affects, etc. may be stored, for introspection
% about previous internal states.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_agent_goal')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- defn_mem_setter(add_goal(agent, cmd)).
add_goal(Agent, Goal, Mem0, Mem2) :- is_list(Goal), !,
 apply_mapl_state(add_goal(Agent), Goal, Mem0, Mem2).
add_goal(Agent, Goal, Mem0, Mem2) :-
 dbug(planner, 'adding ~w goal ~w~n', [Agent, Goal]),
 thought_check(Agent, current_goals(Agent, OldGoals), Mem0),
 append([Goal], OldGoals, NewGoals),
 replace_thought(Agent, current_goals(Agent, NewGoals), Mem0, Mem2).

add_goals(_Agent, Goals, Mem0, Mem2) :-
 thought_check(Agent, current_goals(Agent, OldGoals), Mem0),
 append(Goals, OldGoals, NewGoals),
 replace_thought(Agent, current_goals(Agent, NewGoals), Mem0, Mem2).

add_intent( Agent, attempts(Doer, Action), Mem0, Mem2):-  Agent==Doer,
  add_intent( Agent, Action, Mem0, Mem2).
  
add_intent( Agent, Action, Mem0, Mem2):- 
  add_intent0( Agent, Action, Mem0, Mem2).

add_intent0( Agent, Auto, Mem0, Mem3) :- Auto = act3('auto',Agent,[]), !,
 %must_mw1(member(inst(Agent), Mem0)),
 autonomous_decide_action(Agent, Mem0, Mem3), !.

add_intent0( _Agent, Action, Mem0, Mem2) :-
 thought_check(Agent, intent(Agent, OldToDo), Mem0),
 append(OldToDo, [Action], NewToDo),
 replace_thought(Agent, intent(Agent, NewToDo), Mem0, Mem2), !.

add_intent_all(_Agent, [], Mem0, Mem0).
add_intent_all(Agent, [Action|Rest], Mem0, Mem2) :-
 add_intent( Agent, Action, Mem0, Mem1),
 add_intent_all(Agent, Rest, Mem1, Mem2), !.



% -----------------------------------------------------------------------------
% invoke_introspect(Agent, Query, Answer, Memory)

:- defn_state_getter( invoke_introspect(agent, action, result)).

invoke_introspect(Agent, attempts(Other,Command), Answer, M0) :- Other == Agent, !,
  invoke_introspect(Agent, Command, Answer, M0).

invoke_introspect(Agent, path(There), Answer, M0) :- !,
   declared(h(spatial, _, _, There), M0),
   declared(h(spatial, _, Agent, Here), M0),
  invoke_introspect(Agent, path(Here, There), Answer, M0).

invoke_introspect(Agent, path(Here, There), Answer, M0) :-
 agent_thought_model(Agent, ModelData, M0),
 find_path(Agent, Here, There, Route, ModelData), !,
 get_structure_label(ModelData, Name),
 Answer = msg(['Model is:', Name, 'Shortest path is:\n', Route]).

invoke_introspect(Agent, path(Here, There), Answer, ModelData) :-
 find_path(Agent, Here, There, Route, ModelData), !,
 get_structure_label(ModelData, Name),
 Answer = msg(['Model is:', Name, 'Shortest path was:', nl, list(Route)]).

invoke_introspect(Agent1, recall(Agent, WHQ, Target), Answer, M0) :-
 agent_thought_model(Agent, ModelData, M0),
 recall_whereis(M0, Agent1, WHQ, Target, Answer, ModelData).

invoke_introspect(Agent1, recall(Agent, Target), Answer, M0) :- !,
  invoke_introspect(Agent1, recall(Agent, what, Target), Answer, M0).

recall_whereis(_S0, _Self, _WHQ, There, Answer, ModelData) :-
 findall(Data, (member(Data, ModelData), nonvar_subterm(There, Data)), Memories),
 Memories\==[],
 Answer = Memories.

recall_whereis(_S0, Agent, _WHQ, There, Answer, _ModelData) :-
 Answer = [subj(Agent), person('don\'t', 'doesn\'t'),
   'recall a "', There, '".'].

get_agent_prompt(Agent, Prompt):-
  get_object_props(Agent, Mem), !,
  declared(prompt(Prompt), Mem).
get_agent_prompt(Agent, Prompt):- fail, % trace,
  must_or_rtrace(get_object_props(Agent, Mem)), !,
  declared(prompt(Prompt), Mem), !.

console_decide_action(Agent, Mem0, Mem1):-
 %thought(Agent, timestamp(T0), Mem0),
 %dbug1(read_pending_codes(In, Codes, Found, Missing)),
 % repeat,
 xnotrace((
 ttyflush,
 agent_to_input(Agent, In),
 must_mw1(is_stream(In)),
 setup_console,
 ensure_has_prompt(Agent),
 read_line_to_tokens(Agent, In, [], Words0),
 (Words0==[]->(Words=[wait], notrace(makep));Words=Words0))),
 eng2cmd(Agent, Words, Action, Mem0),
 !,
 if_tracing(dbug(telnet, 'Console TODO ~p~n', [Agent: Words->Action])),
 add_intent( Agent, Action, Mem0, Mem1), ttyflush, !.

makep:-!, mmake.
makep:- update_changed_files, !.
makep:-
 locally(set_prolog_flag(verbose_load, true),
 with_no_dmsg(make:((

  '$update_library_index', 
 findall(File, make:modified_file(File), Reload0),
 list_to_set(Reload0, Reload),
 ( prolog:make_hook(before, Reload)
 -> true
 ; true
 ),
 print_message(silent, make(reload(Reload))),
 must_maplist(reload_file, Reload),
 print_message(silent, make(done(Reload))),
 ( prolog:make_hook(after, Reload)
 -> true
 ; nop(list_undefined),
  nop(list_void_declarations)
 ))))).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
%:- dbug1(ensure_loaded('adv_agents')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
agent_somewhere( Agent, Here):- declared(h(spatial, in, Agent, Here), advstate),!.
agent_somewhere(_Agent, "somewhere").


decide_action(Agent) :-
 %pprint(decide_action(Agent)),
 %get_advstate(State),
 declared(memories(Agent, Mem0)),
 must_mw1(decide_action(Agent, Mem0, Mem2)),
 Mem0 \== Mem2, !,
 redeclare(memories(Agent, Mem2)).
decide_action(_Agent) :- !.

:- defn_mem_setter(run_perceptq(+agent)).
decide_action(Agent, Mem0, Mem2):-
  forget_satisfied_goals(Agent, Mem0, Mem1), !,
  decide_action(Agent, Mem1, Mem2).

decide_action(Agent, Mem0, Mem9):- fail,
  per_plugin(early_decide_action(Agent), Mem0, Mem1),
  Mem0\==Mem9, !,
  decide_action(Agent, Mem1, Mem9).

decide_action(Agent, Mem0, Mem0) :-
 thought_check(Agent, intent(Agent, [Action|_]), Mem0),
 agent_somewhere(Agent,Here),
 (trival_act(Action)->true;dbug1(planner(Agent, Here, Action))).

decide_action(Agent, Mem0, Mem1) :-
 %must_mw1(thought(Agent, timestamp(T0), Mem0)),
 ensure_has_prompt(Agent),
 retract(mu_global:console_tokens(Agent, Words)), !,
 must_mw1((eng2cmd(Agent, Words, Action, Mem0),
 if_tracing(dbug1(add_intent(Agent: Words->Action))),
 add_intent( Agent, Action, Mem0, Mem1))).

% Telnet client (Covered by the above)
decide_action(Agent, Mem0, Mem1) :-
 ensure_has_prompt(Agent),
 fail,
 xnotrace(declared(inherited(telnet), Mem0)), !,
 must_mw1(telnet_decide_action(Agent, Mem0, Mem1)).


% Stdin Client
decide_action(Agent, Mem0, Mem1) :-
 % fail,
 once(xnotrace((declared(inherited(console), Mem0), current_input(In), agent_to_input(Agent, AgentIn)))),
 AgentIn == In,
 ensure_has_prompt(Agent),
 ttyflush,
 (tracing->catch(wait_for_input_safe([In], Found, 20.0), _, (nortrace, xnotrace, break));
                 wait_for_input_safe([In], Found, 0.0)),
 Found \==[],
 console_decide_action(Agent, Mem0, Mem1), !.

decide_action(Agent, Mem0, Mem3) :-
 declared(inherited(autonomous), Mem0),
 maybe_autonomous_decide_goal_action(Agent, Mem0, Mem3).

decide_action(Agent, Mem0, Mem9):- fail,
  per_plugin(decide_action(Agent), Mem0, Mem9), !.

decide_action(_Agent, Mem, Mem) :-
 declared(inherited(memorizer), Mem), !. % recorders don't decide much.

decide_action(_Agent, Mem0, Mem0) :- !.

decide_action(Agent, Mem0, Mem0) :-
 set_last_action(Agent, attempts(Agent, act3('auto',Agent,[]))),
 nop(dbug(decide_action, 'decide_action(~w) FAILED!~n', [Agent])).


:- meta_predicate match_functor_or_arg(1, *).
match_functor_or_arg(Q, P):- compound(P), safe_functor(P, F, _), (call(Q, F)->true;(arg(1, P, E), call(Q, E))), !.


% --------

