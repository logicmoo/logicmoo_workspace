/*
% NomicMUD: A MUD server written in Prolog
%
% Some parts used Inform7, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
%
% July 10, 1996 - John Eikenberry
% Copyright (C) 2004 Marty White under the GNU GPL
%
% Dec 13, 2035 - Douglas Miles
%
%
% Logicmoo Project changes:
%
% Main file.
%
*/
:- '$set_source_module'(mu).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
%:- dbug1(ensure_loaded('adv_robot_floyd')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

random_noise(Agent, [cap(subj(Agent)), Msg]) :-
 random_member(Msg, [
 'hums quietly to themself.', 
 'inspects their inspection cover.', 
 'buffs their chestplate.', 
 'fidgets uncomfortably.'
 ]).

:- dynamic(mu_global:agent_last_action/3).

:- dynamic(mu_global:auto_pause/1).

mu_global:auto_pause(30).

invoke_autonomous_cycle(Agent):-   
  time_since_last_action(Agent, When), mu_global:auto_pause(Seconds), When > Seconds, !.
invoke_autonomous_cycle(Agent):-
 time_since_last_action(Other, When), !,
 Other \== Agent, When < 1, !,
 retractall(mu_global:agent_last_action(Other, _, _)),
 nop(dbug1(time_since_last_action_for(Other, When, Agent))),
 overwrote_prompt, !.


% If actions are queued, no further thinking required.
maybe_autonomous_decide_goal_action(Agent, Mem0, Mem2):-
  forget_satisfied_goals(Agent, Mem0, Mem1), !,
  maybe_autonomous_decide_goal_action(Agent, Mem1, Mem2).
% Is powered down
maybe_autonomous_decide_goal_action(Agent, Mem0, Mem0) :-
 get_advstate(State), getprop(Agent, (powered = f), State), !.
% is not yet time to do something
maybe_autonomous_decide_goal_action(Agent, Mem0, Mem0) :-
 notrace( \+ invoke_autonomous_cycle(Agent)), !.
% attempts to run the ( act3('auto',Agent,[])) command
maybe_autonomous_decide_goal_action(Agent, Mem0, Mem1) :-
 add_intent( Agent, ( act3('auto',Agent,[])), Mem0, Mem1).



% If cleanup old goals
autonomous_decide_action(Agent, Mem0, Mem2):-
  once(forget_satisfied_goals(Agent, Mem0, Mem1)), Mem0 \== Mem1, !,
  autonomous_decide_action(Agent, Mem1, Mem2).

% If actions are queued, no further thinking required.
autonomous_decide_action(Agent, Mem0, Mem0) :-
 thought_check(Agent, intent(Agent, [Action|_]), Mem0),
 agent_somewhere(Agent,Here),
 (trival_act(Action)->true;dbug1(autonomous(Agent, Here, Action))).

% notices bugs
autonomous_decide_action(Agent, Mem0, _) :-
 once((agent_thought_model(Agent, ModelData, Mem0),
 (\+ in_agent_model(Agent, h(spatial, _, Agent, _), ModelData) -> (pprint(Mem0, always), pprint(ModelData, always)) ; true),
 must_mw1(in_agent_model(Agent, h(spatial, _Prep, Agent, Here), ModelData)),
 nonvar(Here))),
 fail.

% If goals exist, attempts to solve them.
autonomous_decide_action(Agent, Mem0, Mem1) :-
 thought_check(Agent, current_goals(Agent, [_|_]), Mem0),
 action_invoke_goals(Agent, Mem0, Mem1), !.


autonomous_decide_action(Agent, Mem0, Mem1) :- 
  autonomous_decide_unexplored_object(Agent, Mem0, Mem1), !.

autonomous_decide_action(Agent, Mem0, Mem1) :-
 once((
% If no actions or goals, but there's an unexplored exit here, go that way.
 random_permutation([
 autonomous_create_new_goal(Agent, Mem0, Mem1),
 autonomous_decide_unexplored_object(Agent, Mem0, Mem1),
 autonomous_decide_unexplored_fn(exit, Agent, Mem0, Mem1),
 autonomous_decide_follow_player(Agent, Mem0, Mem1),
 autonomous_decide_silly_emoter_action(Agent, Mem0, Mem1)
 ], Premute),
 member(Try, Premute),
 call(Try))).

autonomous_decide_action(Agent, Mem0, Mem0) :-
 agent_somewhere(Agent,Here),
 (dbug(autonomous+verbose, '~w: Can\'t think of anything to do.~n', [Agent-Here])), fail.% trace.

autonomous_create_new_goal(_Agent, _Mem0, _Mem1) :- fail.

% An unexplored exit here, go that way.
autonomous_decide_unexplored_fn(Exit, Agent, Mem0, Mem1) :-
 agent_thought_model(Agent, ModelData, Mem0),
 in_agent_model(Agent, h(Spatial, _Prep, Agent, Here), Mem0),
 in_agent_model(Agent, h(Spatial, fn(exit, Dir), Here, '<mystery>'(Exit, _, _)), ModelData),
 add_intent(Agent, ( act3('go__dir',Agent,[ walk, Dir])), Mem0, Mem1).
% An unexplored exit here, go that way.
 /*
autonomous_decide_unexplored_fn(Exit, Agent, Mem0, Mem2) :-
 agent_thought_model(Agent, ModelData, Mem0),
 in_agent_model(Agent, h(Spatial, _Prep, Agent, Here), Mem0),
 in_agent_model(Agent, h(Spatial, fn(Exit, Dir), Here, '<mystery>'(Exit, _, _)), ModelData),
 in_agent_model(Agent, h(Spatial, fn(Exit, Dir), Here, There), ModelData),

 add_intent(Agent, ( act3('go__dir',Agent,[ walk, Dir])), Mem0, Mem2),!.
 % add_intent(Agent, ( act3('go__dir',Agent,[ walk, Prev])), Mem1, Mem2).
*/
% An unexplored object!
autonomous_decide_unexplored_object(Agent, Mem0, Mem2) :-
 agent_thought_model(Agent, ModelData, Mem0),
 g_h(spatial, touchable, Agent, Object, Mem0),
 Agent\= Object,
 can_sense(Agent, _, Object, Mem0),
 in_agent_model(Agent, h(spatial, _, '<mystery>'(closed, _, _), Object), ModelData),
 % in_agent_model(Agent, h(spatial, Prep, Object, Here), ModelData),
 % in_agent_model(Agent, h(spatial, Prep, Agent, Here), ModelData),
 % getprop(Object, can_be(open, t), Mem0), 
 add_intent( Agent, ( act3('open',Agent,[ Object])), Mem0, Mem1),
 add_intent( Agent, ( act3('examine',Agent,[ see, Object])), Mem1, Mem2), !.

autonomous_decide_unexplored_object(Agent, Mem0, Mem1) :-  fail,
 agent_thought_model(Agent, ModelData, Mem0),
 in_agent_model(Agent, h(spatial, A, '<mystery>'(W, B, C), Object), ModelData),
 add_intent( Agent, ( act3('make_true',Agent,[ ~(h(spatial, A, '<mystery>'(W, B, C), Object))])), Mem0, Mem1).


% Follow Player to adjacent rooms.
autonomous_decide_follow_player(Agent, Mem0, Mem1) :- % 1 is random(2),
 must_mw1((
 agent_thought_model(Agent, ModelData, Mem0),
 in_agent_model(Agent, h(spatial, _, Agent, Here), ModelData))),
 dif(Agent, Player), mu_current_agent(Player),
 in_agent_model(Agent, h(spatial, _, Player, There), ModelData),
 in_agent_model(Agent, h(spatial, fn(exit, Dir), Here, There), ModelData),
 add_intent(Agent, ( act3('go__dir',Agent,[ walk, Dir])), Mem0, Mem1).

autonomous_decide_silly_emoter_action(Agent, Mem0, Mem1) :-
 1 is random(5), % fail_feature,
 random_noise(Agent, Msg),
 add_intent( Agent, ( act3('emote',Agent,[ act, *, Msg])), Mem0, Mem1).


always_action( act3('go__dir',_,[ _, _])).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_agent_listen')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consider_text(Speaker, _EmoteType, Agent, Words, Mem0, Mem1):-
 eng2cmd(Agent, Words, Action, Mem0) ->
 consider_request(Speaker, Agent, Action, Mem0, Mem1).

% For now, agents will attempts to satisfy all commands.
consider_request(Requester, Agent, Action, _M0, _M1) :-
 dbug(autonomous, '~w: considering request from: ~w.~n', [Requester, Agent, Action]),
 fail.

consider_request(Requester, Agent, Query, M0, M1) :-
 invoke_introspect(Agent, Query, Answer, M0),
 %add_intent( Agent, print_(Answer), M0, M1).
 add_intent( Agent, ( act3('emote',Agent,[ say, Requester, Answer])), M0, M1).

consider_request(_Speaker, Agent, forget(Agent, goals), M0, M2) :-
 dbug(autonomous, '~w: forgetting goals.~n', [Agent]),
 update_agent_model_props(Agent, current_goals(Agent, []), M0, M2), !.

% Bring object back to Speaker.
consider_request(Speaker, Agent, fetch(Object), M0, M1) :-
 add_goal(Agent, h(spatial, held_by, Object, Speaker), M0, M1).
consider_request(_Speaker, Agent, ( act3('put',Agent,[ Thing, Relation, Where])), M0, M) :-
 add_goal(Agent, h(spatial, Relation, Thing, Where), M0, M).
consider_request(_Speaker, Agent, ( act3('take',Agent,[ Thing])), M0, M) :-
 add_goal(Agent, h(spatial, held_by, Thing, Agent), M0, M).
consider_request(_Speaker, Agent, ( act3('drop',Agent,[ Object])), M0, M1) :-
 add_goal(Agent, ~(h(spatial, held_by, Object, Agent)), M0, M1).

consider_request(_Speaker, Agent, AlwaysAction, M0, M1) :-
 always_action(AlwaysAction),
 dbug(autonomous, 'Queueing action ~w~n', [AlwaysAction]),
 add_intent( Agent, AlwaysAction, M0, M1).

consider_request(_Speaker, Agent, Action, M0, M1) :-
 dbug(autonomous, 'Finding goals for action: ~w~n', [Action]),
 initial_operators(Agent, Operators),
 findall(Effects,
   member(oper(Agent, Action, _Conds, Effects), Operators),
   [UnambiguousGoals]),
 dbug(autonomous, 'Request: ~w --> goals ~w.~n', [Action, UnambiguousGoals]),
 add_goals(UnambiguousGoals, M0, M1).

consider_request(_Speaker, Agent, Action, M0, M1) :-
 dbug(autonomous, 'Queueing action: ~w~n', [Action]),
 add_intent( Agent, Action, M0, M1).
consider_request(_Speaker, Agent, Action, M0, M0) :-
 dbug(autonomous, '~w: did not understand request: ~w~n', [Agent, Action]).


