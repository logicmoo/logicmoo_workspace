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

%:- nop(ensure_loaded('adv_chat80')).
%:- ensure_loaded(adv_main).
%:- endif.

%:- user:listing(adventure).


:- defn_state_getter(get_perceptq_objects(-list(inst))).
get_perceptq_objects(Objects, S0):-
 setof(O, member(perceptq(O, _), S0), Objects).

get_sensing_objects(Sense, Agents, S0):-
 get_objects((has_sense(Sense);inherited(memorizer)), Agents, S0).

:- defn_state_getter(get_live_agents(-list(agent))).
get_live_agents(LiveAgents, S0):-
 get_some_agents( \+ powered=f, LiveAgents, S0).

:- defn_state_getter(get_some_agents(conds, -list(agent))).
get_some_agents(Precond, LiveAgents, S0):-
 must_mw1((
  get_objects(
  (inherited(character), Precond), LiveAgents, S0),
  LiveAgents \== [])).



%sense_here0(_Sense, _In, _Here, _S0):-!.
sense_here0(Sense, _In, Here, S0):-
 getprop(Here, TooDark, S0),
 (sensory_problem_solution(Sense, TooDark, EmittingLight) ->
   related_with_prop(Sense, _Obj, Here, EmittingLight, S0) ; true).

can_sense_here(Agent, Sense, S0) :-
 from_loc(Agent, Here, S0),
 sense_here0(Sense, in, Here, S0), !.
% can_sense_here(_Agent, _Sense, _State) .

is_star(Star):- Star == '*'.
is_star('*'(Star)):- nonvar(Star).

can_sense(Agent, Sense, Thing, S0, S9):- can_sense(Agent, Sense, Thing, S0), S9=S0.
:- defn_state_getter(can_sense(agent, sense, thing)).
can_sense(_Agent, _See, Star, _State) :- is_star(Star), !.
can_sense(Agent, Sense, Thing, S0) :- Agent == Thing, !, can_sense_here(Agent, Sense, S0).
can_sense(_Agent, Sense, Here, S0) :- fail,
  getprop(Here, has_rel(fn(exit, _), t), S0),
  sense_here0(Sense, in, Here, S0), !.

can_sense(Agent, Sense, Thing, S0) :-
  can_sense_here(Agent, Sense, S0),
  from_loc(Agent, Here, S0),
  (Thing=Here;  open_traverse(Thing, Here, S0)).
/*can_sense(Agent, Sense, Thing, S0) :-
 % get_open_traverse(_Open, Sense, _Traverse, Sense),
 can_sense_here(Agent, Sense, S0),
 h(spatial, Sense, Agent, Here, S0),
 (Thing=Here; h(spatial, Sense, Thing, Here, S0)).
*/
can_sense(Agent, Sense, Thing, _State):- fail,
 dbug1(pretending_can_sense(Agent, Sense, Thing, Agent)), !.

as_single_event([Event], SEvent):- !, as_single_event(Event, SEvent).
as_single_event([E1, E2|More], single_event([E1, E2|More])).
% as_single_event([E1, E2|More], SEvent):- dumpST, break, !, as_single_event(Event, SEvent).
as_single_event(Event, Event).

stripped_term(MAction, Action):- strip_module(MAction, _, Action), MAction\==Action.

send_1percept(Agent, MAction, S0, S9):-  stripped_term(MAction, Action), !, send_1percept(Agent, Action, S0, S9).
send_1percept(Agent, Event, S0, S2) :- as_single_event(Event, SEvent) -> Event\==SEvent, !,
 send_1percept(Agent, SEvent, S0, S2).

send_1percept(Agent, Event, S0, S2) :-
  declared(perceptq(Agent, _Q), S0), !,
  queue_agent_percept(Agent, [Event], S0, S2).

send_1percept(Agent, Event, S0, S2) :-
  invoke_percept_list(Agent, [Event], S0, S2).

send_percept(Agent, Event, S0, S2) :-
  declared(perceptq(Agent, _Q), S0), !,
  queue_agent_percept(Agent, Event, S0, S2).
send_percept(Agent, Event, S0, S2) :-
  invoke_percept_list(Agent, Event, S0, S2).

:- defn_state_setter( invoke_percept_list(agent, list(event))).
invoke_percept_list(_Agent, [], S0, S0):-!.
% invoke_percept_list(Agent, Events, _S0, _S2) :- dmsg( invoke_percept_list(Agent, Events)), fail.
invoke_percept_list(Agent, Events, S0, S2) :-
  pre_redeclare(memories(Agent, Mem0), S0, S1),
  agent_clock_time_prev(Agent, timestamp(Stamp, _OldNow), Events),
  with_agent_console(Agent, process_percept_list(Agent, Events, Stamp, Mem0, Mem3)),
  redeclare(memories(Agent, Mem3), S1, S2).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_events')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- defn_state_setter(queue_agent_percept(agent, listok(event))).
% Manipulate one agents percepts
queue_agent_percept(Agent, Event, S0, S2) :-
 \+ is_list(Event), !,
 queue_agent_percept(Agent, [Event], S0, S2).
% Agent process event list now

queue_agent_percept(_Agent, [], S0, S0):-!.
%queue_agent_percept(Agent, Events, _S0, _S2) :- dmsg(queue_agent_percept(Agent, Events)), _Player_1\==Agent, dumpST, fail.
queue_agent_percept(Agent, Events, S0, S2) :-
 getprop(Agent, inherited(no_perceptq), S0), !,
 invoke_percept_list(Agent, Events, S0, S2).
queue_agent_percept(Agent, Events, S0, S2) :-
 agent_clock_time_now(Agent, timestamp(Stamp, OldNow), S0),
 append_toplevel_props(perceptq(Agent, [timestamp(Stamp, OldNow)|Events]), S0, S2).


:- defn_state_setter(queue_event(listok(event))).
queue_event(Event, S0, S2) :-
 each_sensing_thing(_All, queue_agent_percept(Event), S0, S2).


locally__agent_percept__(Agent, Event, Places, S0, S1) :- assertion(is_list(Places)),
 member(Where, Places), can_sense(Agent, _, Where, S0), !,
 queue_agent_percept(Agent, Event, S0, S1), !.
locally__agent_percept__(_Agent, _Event, _Places, S0, S0).


% Room-level simulation percepts
:- defn_state_setter(queue_local_event(listof(event), listof(place))).
queue_local_event(Event, Places) --> { \+ is_list(Places)},
  !, queue_local_event(Event, [Places]).
queue_local_event(Event, Places) -->
 each_sensing_thing(_All, locally__agent_percept__(Event, Places)).




is_sense(X):- sensory_verb(X, _).

sensory_verb(see, look).
sensory_verb(hear, listen).
sensory_verb(taste, taste).
sensory_verb(smell, smell).
sensory_verb(touch, feel).

sensible_pred(_Sense, _At).
sense_to_domain(_Sense, Spatially):-  spatial_domain(Spatially).
pred_to_domain(P, Spatially):- is_spatial_rel(P),!, spatial_domain(Spatially).
verb_to_domain(_Verb, Spatially):-  spatial_domain(Spatially).
domain_to_default_fn(_,exit).

:- defn_state_none(spatial_domain(-domain)).
spatial_domain(spatial).


action_sensory(Action, Sense):-
 compound(Action),
 Action=..[_Verb, Sensory|_],
 is_sense(Sensory), !,
 Sense=Sensory.
action_sensory(Action, Sense):-
 compound(Action),
 Action=..[Verb|_],
 verb_sensory(Verb, Sense).
action_sensory(Action, Sense):-
 verb_sensory(Action, Sense) *-> true; Sense=see.


% listen->hear
verb_sensory(goto, Sense):- is_sense(Sense).
verb_sensory(examine, Sense):- is_sense(Sense).
verb_sensory(wait, Sense):- is_sense(Sense).
verb_sensory(print_, Sense):- is_sense(Sense).
verb_sensory(Verb, Sense):- sensory_verb(Sense, Verb).
verb_sensory(look, see).
verb_sensory(say, hear).
verb_sensory(eat, taste).
verb_sensory(feel, touch).
verb_sensory(goto, see).
verb_sensory(Verb, Sense):- nonvar(Verb), is_sense(Verb), Sense=Verb.
verb_sensory(Verb, Sense):- requires_spatially(Verb, Verb2), Verb\=Verb2,
 verb_sensory(Verb2, Sense), \+ is_sense(Verb).
verb_sensory(Verb, Sense):- verb_alias(Verb, Verb2), Verb\=Verb2,
 verb_sensory(Verb2, Sense), \+ is_sense(Verb).



% sensory_model(Visual, TooDark, EmittingLight))
sensory_problem_solution(Sense, Dark = t, emitting(Sense, Light)):-
 problem_solution(Dark, Sense, Light).

problem_solution(dark, see, light).
problem_solution(stinky, smell, purity).
problem_solution(noisy, hear, quiet).


:- defn_mem_setter(percept_intent( agent, list(action))).
percept_intent( Agent, Actions, Mem0, Mem2):- add_intent_all(Agent, Actions, Mem0, Mem2), !.
%percept_intent( Agent, Actions, Mem0, Mem2):- apply_mapl_state(add_goal(Agent), Actions, Mem0, Mem2).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_agent_percepts')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Autonomous logical percept processing.
%process_percept_do_auto(Agent, with_msg(Percept, _Msg), Timestamp, M0, M2) :- !,
% process_percept_do_auto(Agent, Percept, Timestamp, M0, M2).

must_be_args_bound(Cmpd):- arg(_,Cmpd,E),var(E),!,dumpST,dbug1(not_args_bound(Cmpd)),trace_or_throw(not_args_bound(Cmpd)).

:- defn_mem_setter(process_percept_auto//3).
process_percept_do_auto(Agent, Percept, Stamp, M0, M0) :- must_be_args_bound(process_percept_do_auto(Agent, Percept, Stamp, M0)).
process_percept_do_auto(_Agent, msg(_), _Stamp, M0, M0) :- !.
process_percept_do_auto(_Agent, [], _Stamp, M0, M0) :- !.
process_percept_do_auto(Agent, [Percept|Tail], Stamp, M0, M9) :-
 process_percept_do_auto(Agent, Percept, Stamp, M0, M1),
 process_percept_do_auto(Agent, Tail, Stamp, M1, M9).

process_percept_do_auto(Agent, Percept, _Stamp, M0, M0) :- was_own_self(Agent, Percept), !.

% Auto examine room items
process_percept_do_auto(Agent, percept(Agent, Sense, Depth, child_list(_Spatially, _Here, _Prep, Objects)), _Stamp, Mem0, Mem2) :-
 agent_thought_model(Agent, _ModelData, Mem0), Depth > 1,
 % getprop(Agent, model_depth = ModelDepth, advstate),
 DepthLess is Depth - 1, 
 findall( act3('examine__D5',Agent,[ Sense, child, Obj, DepthLess]),
   ( member(Obj, Objects),
      Obj \== Agent), % ( \+ member(props(Obj, _), ModelData); true),
   Actions),
 percept_intent( Agent, Actions, Mem0, Mem2).
 

process_percept_do_auto(_Agent, _Percept, _Timestamp, M0, M0):-  \+ declared(inherited(autonomous), M0), !.

% Auto Answer
process_percept_do_auto(Agent, act3('emote',Speaker,[ EmoteType, Agent, Words]), _Stamp, Mem0, Mem1) :-
  consider_text(Speaker, EmoteType, Agent, Words, Mem0, Mem1).
process_percept_do_auto(Agent, act3('emote',Speaker,[ EmoteType, Star, WordsIn]), _Stamp, Mem0, Mem1) :- is_star(Star),
 addressing_whom(WordsIn, Whom, Words),
 Whom == Agent,
 consider_text(Speaker, EmoteType, Agent, Words, Mem0, Mem1).

% Auto take
process_percept_do_auto(Agent, unused_percept_props(Agent, Sense, Object, Depth, PropList), _Stamp, Mem0, Mem2) :-
  Depth > 1,
 (member(inherited(shiny), PropList)),
 Object \== Agent,
 dbug(autonomous, '~w: ~p~n', [Agent, unused_percept_props(Agent, Sense, Object, Depth, PropList)]),
 agent_thought_model(Agent, ModelData, Mem0),
 \+ h(spatial, descended, Object, Agent, ModelData), % Not holding it?
 add_intent_all([ act3('take',Agent,[ Object]), print_(Agent, 'My shiny precious!')], Mem0, Mem2).


process_percept_do_auto(_Agent, _Percept, _Stamp, M0, M0):- !.

addressing_whom(List, Agent, Words):- Words = [_|_], append(Words, [Agent], List).
addressing_whom(List, Agent, Words):- Words = [_|_], append(_, [Agent|Words], List).


%was_own_self(Agent, say(Agent, _)).
was_own_self(Agent, (act3('emote',Agent,[ _, _Targ, _]))).
was_own_self(Agent, attempts(Agent, act3('emote',Agent,[ _, _Targ, _]))).
was_own_self(Agent, act3('emote',Agent,[ _, _Targ, _])).
% was_own_self(Agent, Action):- action_doer(Action, Was), Was == Agent.

:- defn_mem_setter(process_percept_player//3).
% Ignore own speech.
process_percept_player(Agent, _Percept, _Stamp, Mem0, Mem0) :- \+ is_player(Agent), !.
process_percept_player(_, [], _Stamp, Mem0, Mem0) :- !.
process_percept_player(Agent, [Percept|Tail], Stamp, Mem0, Mem4) :- !,
 process_percept_player(Agent, Percept, Stamp, Mem0, Mem1),
 process_percept_player(Agent, Tail, Stamp, Mem1, Mem4).
process_percept_player(Agent, Percept, _Stamp, Mem0, Mem0) :- was_own_self(Agent, Percept), !.
process_percept_player(_Agent, percept(_, Know, _, _Percept), _Stamp, Mem0, Mem0) :- Know == know, !.
process_percept_player(Agent1, percept(Agent2, _, _, _), _Stamp, Mem0, Mem0) :- Agent1 \== Agent2, !.
%process_percept_player(Agent1, percept(Agent2, _, _, _), _Stamp, Mem0, Mem0) :- Agent1 \== Agent2, !.
%process_percept_player(Agent, Percept, _Stamp, Mem0, Mem0) :- sub_term(Sub, Percept), compound(Sub), Sub=depth(_KnowsD, DepthN), getprop(Agent, look_depth = LookDepth, advstate), DepthN > LookDepth, !.
process_percept_player(Agent, Percept, _Stamp, Mem0, Mem0) :-
 percept2txt(Agent, Percept, Text), !, 
 ignore((filtered_percept_text(Agent,Text),
   player_format(Agent, '~N~w~n', [Text]))), !.

process_percept_player(Agent, Percept, _Stamp, M0, M0) :- player_format(Agent, '~N~q~n', [Agent:Percept]).

filtered_percept_text(A,T):- E=pre_txt(A,T),(nb_current(filtered_percept_text,Was);Was=[]),!,
  ((nth0(N,Was,EE),EE=@=E)-> (!,N>6) ; nb_setval(filtered_percept_text,[E|Was])).

is_player(Agent):- \+ is_non_player(Agent).
is_non_player(Agent):- inst_of(Agent, floyd, _).


:- defn_mem_setter(process_percept_main//3).
% process_percept_main(Agent, PerceptsList, Stamp, OldModel, NewModel)
process_percept_main(_Agent, [], _Stamp, Mem0, Mem0) :- !.
process_percept_main(Agent, Percept, Stamp, Mem0, Mem2) :-
 % dbug(always, '~N1 percept ~q !~n', [percept(Percept)]),
 nb_setval(filtered_percept_text,[]),
 quietly(process_percept_player(Agent, Percept, Stamp, Mem0, Mem1)),
 process_percept_do_auto(Agent, Percept, Stamp, Mem1, Mem2), !.
process_percept_main(Agent, Percept, Stamp, Mem0, Mem0):-
 dbug(perceptq, '~q FAILED!~n', [bprocess_percept(Agent, Percept, Stamp)]), !.


:- defn_mem_setter(process_percept_list(agent, list(event), tstamp)).

process_percept_list(Agent, PerceptList, _Stamp, _, _):-
  episodic_mem(Agent, PerceptList), fail.

% process_percept_list(Agent, Percept, Stamp, _, _):- notrace((format('~N', []), prolog_pprint(p2(Agent, Percept, Stamp)), format('~N', []))), fail.

process_percept_list(Agent, Percept, Stamp, Mem0, Mem3) :-
 \+ is_list(Percept), !, process_percept_list(Agent, [Percept], Stamp, Mem0, Mem3).
% caller memorizes PerceptList
process_percept_list(_Agent, _, _Stamp, Mem, Mem) :-
 declared(inherited(memorize_perceptq), Mem),
 !.
process_percept_list(Agent, Percept, Stamp, Mem0, Mem3) :-
 must_mw1((
 append(Percept, Mem0, PerceptMem),
 each_update_model(Agent, Percept, Stamp, PerceptMem, Mem0, Mem2),
 process_percept_main(Agent, Percept, Stamp, Mem2, Mem3))), !.
process_percept_list(_Agent, Percept, _Stamp, Mem0, Mem0) :- dumpST,
 dbug(intent, 'process_percept_list(~w) FAILED!~n', [Percept]), !.



end_of_file.


