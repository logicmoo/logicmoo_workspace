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

:- op(1200, xfy, ('==>>')).
:- op(1200, xfy, ('::=')).
:- op(700, fx, ('~')).

:- ensure_loaded(adv_axiom).

:- dynamic(mu_global:agent_last_action/3).

time_since_last_action(Agent, When):-
 (mu_global:agent_last_action(Agent, _Action, Last), clock_time(T), When is T - Last) *-> true; clock_time(When).

set_last_action(Agent, Action):-
 clock_time(T),
 retractall(mu_global:agent_last_action(Agent, _, _)),
 assertz(mu_global:agent_last_action(Agent, Action, T)).



%! cmd_workarround( +VerbObj, -VerbObj2) is semidet.
%
% Cmd Workarround.
%
cmd_workarround(VerbObj, VerbObj2):-
 VerbObj=..VerbObjL,
 quietly(cmd_workarround_l(VerbObjL, VerbObjL2)),
 VerbObj2=..VerbObjL2.


cmd_workarround_l([Verb|ObjS], [Verb|ObjS2]):- append(ObjS2, ['.'], ObjS).
% look at screendoor -> look screendoor 
cmd_workarround_l([Verb, Relation|ObjS], [Verb|ObjS]):- is_ignorable(Relation), !.
% look at screendoor -> look_at screendoor 
cmd_workarround_l(ObjS, ObjS2):- fail,
 append(Left, [L, R, M|More], ObjS), atom(L), atom(R),
 current_atom(Atom), atomic_list_concat([L, RR], Atom, '_'), RR==R,
 append(Left, [Atom, M|More], ObjS2).
% attempts(Agent, act3('look',Agent,[ spatial])) at screen door
cmd_workarround_l( ObjS1, ObjS2):- append(L,[Verb1|R],ObjS1), verb_alias(Verb1, Verb2), append(L,[Verb2|R],ObjS2).

cmd_workarround_l([Verb, Agent,ObjS], WA ):- is_list(ObjS),flatten([Verb, Agent,ObjS],WA).
cmd_workarround_l([act3, Verb, Agent, ObjS], [attempts,Agent,act3(Verb,Agent,ObjS)]):- !.
% look listen, smell ...
cmd_workarround_l([Verb, Agent|ObjS], [attempts,Agent,act3('examine',Agent,[Sense|ObjS])]):-  sensory_verb(Sense, Verb),!.
cmd_workarround_l([Verb, Agent|ObjS], [attempts,Agent,act3(Verb,Agent,ObjS)]):-  Verb\==attempts.

is_ignorable(Var):- var(Var), !, fail.
is_ignorable(at). is_ignorable(in). is_ignorable(to). is_ignorable(the). is_ignorable(a). is_ignorable(spatial).

verb_alias(look, examine) :- fail.



% drop -> move -> touch
requires_verb(touch, touch).
requires_verb(move, touch).
requires_verb(drop, move).
requires_verb(eat, touch).
requires_verb(hit, touch).
requires_verb(put, drop).
requires_verb(give, drop).
requires_verb(take, move).
requires_verb(throw, drop).
requires_verb(open, touch).
requires_verb(close, touch).
requires_verb(lock, touch).
requires_verb(unlock, touch).
requires_verb(switch, touch).

requires_verb(walk, goto).

% feel <- taste <- smell <- look <- listen (by distance)
requires_verb(listen, examine).
% requires_verb(examine, examine).
requires_verb(look, examine).
% in order to smell it you have to at least be in sight distance
requires_verb(smell, look).
requires_verb(eat, taste).
requires_verb(taste, smell).
requires_verb(taste, feel).
requires_verb(feel, examine).
requires_verb(feel, touch).
requires_verb(X, Y):- ground(requires_verb(X, Y)), X=Y.

requires_verb(SpatialVerb1, SpatialVerb2):- compound(SpatialVerb1), compound(SpatialVerb2), !,
 SpatialVerb1=..[Verb1, Arg1|_],
 SpatialVerb2=..[Verb2, Arg2|_],
 requires_verb(Verb1, Verb2),
 requires_verb(Arg1, Arg2).

requires_verb(SpatialVerb, Verb2):- compound(SpatialVerb), safe_functor(SpatialVerb, Verb, _), !,
 requires_verb(Verb, Verb2).

requires_verb(Verb, SpatialVerb2):- compound(SpatialVerb2), safe_functor(SpatialVerb2, Verb2, _), !,
 requires_verb(Verb, Verb2).

% proper subset - C may not be a subset of itself.
proper_requires_spatially(A, B):- A==B, !, fail.
proper_requires_spatially(A, B) :- requires_verb(A, B).
proper_requires_spatially(A, C) :-
 requires_verb(A, B),
 requires_verb(B, C).


maybe_pause(Agent):- stdio_player(CP), (Agent==CP -> wait_for_input([user_input], _, 0) ; true).

invoke_command(Requester, attempts(Agent, Action)) ==>>  
 {Requester==Agent},
 invoke_command(Agent, Action), !.

invoke_command(Agent, Action) ==>>
 invoke_metacmd(Agent, Action),
  {overwrote_prompt(Agent)}, !.
invoke_command(Agent, Action) ==>>
  {set_last_action(Agent, Action)},
 agent_try_action(Agent, Action), !.
invoke_command(Agent, Action) :-
 event_failed(Agent, unknown_comand( Action)),
 player_format(Agent, 'ERRROR Failed or No Such Command: ~w~n', Action).

% --------

/*
new_do_intent( Agent):-
 get_advstate(S0),
 invoke_intents( Agent, S0, S9),
 set_advstate(S9), !.
*/
:- defn_state_setter( invoke_intents( +agent)).

invoke_intents( Agent) ==>>
 sg(declared(memories(Agent, Mem0))),
 {member( intent(Agent, []), Mem0)}, !.
invoke_intents( Agent, S0, S9) :-
 declared(memories(Agent, Mem0), S0, S1),
 thought_check(Agent, intent(Agent, OldToDo), Mem0), 
    append([Action], NewToDo, OldToDo), 
 replace_thought(Agent, intent(Agent, NewToDo), Mem0, Mem2),
 redeclare(memories(Agent, Mem2), S1, S2),
 set_last_action(Agent, Action),
 invoke_command(Agent, Action, S2, S9).
invoke_intents( _Agent, S0, S0).



%do_intent_while(Agent, S0, S9) :-
% declared(memories(Agent, Mem0), S0),
% th ought(Agent, intent(Agent, ToDo), Mem0),
% append([Action], NewToDo, OldToDo),



% ---- apply_aXioms( Action, S0, S9)
% where the states also contain Percepts.
% In Inform, actions work in the following order:
% game-wide preconditions
% Player preconditions
% objects-in-vicinity react_before conditions
% room before-conditions
% direct-object before-conditions
% verb
% objects-in-vicinity react_after conditions
% room after-conditions
% direct-object after-conditions
% game-wide after-conditions
% In TADS:
% "verification" methods perferm tests only

agent_try_action(Agent, Action, S0, S3) :-
 quietly_must((
 set_last_action(Agent, Action),
 pre_redeclare(memories(Agent, Mem0), S0, S1),
 memorize_attempting(Agent, Action, Mem0, Mem1),
 redeclare(memories(Agent, Mem1), S1, S2))),
 into_attempt(Agent, Action, AgentAction),
 once(show_failure(raise_aXiom_events( AgentAction, S2, S3));(S2=S3, dumpST)), !.

into_attempt(Agent, Action, attempts(Agent,Action)):- \+ functor(Action, attempts, 2), !.
into_attempt(_Agent, Action, AgentAction):- Action = AgentAction.

%memorize_attempting(_Agent, Action, Mem0, Mem0):- has_depth(Action), !.
memorize_attempting(Agent, Action, Mem0, Mem2):-
  copy_term(Action, ActionG),
  mw_numbervars(ActionG, 999, _),
  ( has_depth(Action)
    -> Mem0 = Mem1 ;
    (agent_clock_time_now(Agent, Timestamp, Mem0),
     memorize_prepending(Agent, Timestamp, Mem0, Mem1))),
  DOES = attempts(Agent, ActionG),
  memorize_prepending(Agent, DOES, Mem1, Mem2).




agent_clock_time_prev(Agent, timestamp(T0, OldNow), Mem0):-
 find_clock_time(Agent, T0, OldNow, Mem0), !.

agent_clock_time_now(Agent, timestamp(T1, Now), Mem0):-
  agent_clock_time_prev(Agent, timestamp(T0, _OldNow), Mem0),
  T1 is T0 + 1, clock_time(Now), !.

some_agent_clock_time(Agent, T0, OldNow, Mem):- nonvar(Mem),
  thought_check(Agent, timestamp(T0, OldNow), Mem), !.
 
find_clock_time(Agent, T0, OldNow, _UMem):-
   get_advstate(State), declared(percept(Agent, Mem), State),
   some_agent_clock_time(Agent, T0, OldNow, Mem).
find_clock_time(Agent, T0, OldNow, Mem0):- some_agent_clock_time(Agent, T0, OldNow, Mem0).
find_clock_time(Agent, T0, OldNow, _UMem):-
   get_advstate(State), declared(memories(Agent, Mem), State),
   some_agent_clock_time(Agent, T0, OldNow, Mem).
find_clock_time(Agent, T0, OldNow, _UMem):-
   get_advstate(State), declared(props(Agent, Mem), State),
   some_agent_clock_time(Agent, T0, OldNow, Mem).

   
has_depth(Action):- compound(Action), safe_functor(Action, _, A), arg(A, Action, E), compound(E), E=depth(_), !.

trival_act(V):- \+ callable(V), !, fail.
%trival_act(act3('examine__D5',_,[ _, _, _, _])).
%trival_act(act3('look',_,[])).
%trival_act(Action):- has_depth(Action).
trival_act(V):- \+ compound(V), !, fail.
trival_act(_):- !, fail.
trival_act( attempts(_, X)):- !, trival_act( X).
trival_act( act3('wait',_,[])).

exclude_failures(X,Y):- apply:exclude((=(failed(_))),X,Y).

satisfy_each_always(Context, Cond, Memory, NewMemory):-
  satisfy_each(Context, Cond, Memory, NewMemory0), 
  exclude_failures(NewMemory0,NewMemory),!.
satisfy_each_always(_Context, _Cond, Memory, NewMemory):- exclude_failures(Memory,NewMemory).

satisfy_each_cond(Ctxt, [], success(Ctxt)) ==>> !.
satisfy_each_cond(Context, [Cond|CondList], Out) ==>>
  dmust_tracing(satisfy_each(Context, Cond)), !,
  ((sg(member(failed(Why))) -> Out=failed(Why) ; satisfy_each_cond(Context, CondList, Out))), !.
% satisfy_each_cond(_Context, [Cond|_CondList], failed(Cond)) ==>> !.

satisfy_each(Ctx, MCond, S0, S9) :-  stripped_term(MCond, Cond), !, trace, satisfy_each(Ctx, Cond, S0, S9).
satisfy_each(Ctx, Cond, S0, S9) :- 
     exclude_failures(S0, S1), S0\==S1,!,
     satisfy_each2(Ctx, Cond, S1, S9),!.
satisfy_each(Ctx, Cond, S0, S9) :- 
     satisfy_each2(Ctx, Cond, S0, S9),!.

satisfy_each2(Ctx, MCond, S0, S9) :-  stripped_term(MCond, Cond), !, trace, satisfy_each2(Ctx, Cond, S0, S9).
satisfy_each2(Ctx, Cond) ==>> satisfy_each1(Ctx, Cond).
satisfy_each2(_, Cond) ==>> [failed(Cond)].

satisfy_each1(_Context, h(spatial, P, _X, _Y)) ==>> {P==takeable}, !.

satisfy_each1(Context, List) ==>> {is_list(List)}, !,
  satisfy_each_cond(Context, List, Out), !,
  {dmust_tracing(Out\=failed(_))}.

satisfy_each1(_Ctx, A \= B) ==>> {dif(A, B)}, !.

satisfy_each1(_Ctxt, exists(Ex)) ==>> !, {ground(Ex)}.


satisfy_each1(Context, believe(Beliver, Cond)) ==>>
   pre_redeclare(memories(Beliver, Memory)),
   {satisfy_each_always(Context, Cond, Memory, NewMemory)}, !,
   redeclare(memories(Beliver, NewMemory)).

satisfy_each1(Context, believe(_Beliver, Cond)) ==>> !, satisfy_each(Context, Cond), !.
   

satisfy_each1(postCond(_Action), event(Event), S0, S9) :-  raise_aXiom_events(Event, S0, S9).

satisfy_each1(Context, (C1, C2), S0, S9) :- !,
  satisfy_each(Context, C1, S0, S1),
  satisfy_each(Context, C2, S1, S9).
satisfy_each1(Context, (C1 ; C2), S0, S9) :- !,
  (satisfy_each1(Context, C1, S0, S9);
   satisfy_each(Context, C2, S0, S9)).

satisfy_each1(postCond(_Action), ~(Cond)) ==>> !, undeclare_always(Cond). % select_always//1
satisfy_each1(Context, foreach(Cond, Event), S0, S9) :- findall(Event, phrase(Cond, S0, _), TODO), satisfy_each(Context, TODO, S0, S9).
satisfy_each1(_, percept_local(Where, Event)) ==>> !, queue_local_event([Event], [Where]).
satisfy_each1(_, percept(Agent, Event)) ==>> !, send_1percept(Agent, Event).
satisfy_each1(Context, ~(Cond)) ==>> !, (( \+ satisfy_each1(Context, Cond)) ; [failed(Cond)] ).
satisfy_each1(_, call(Cond)) ==>> (declared(Cond)*-> true; apply_aXioms(Cond)).
satisfy_each1(_, Cond) ==>> declared(Cond), !.
satisfy_each1(postCond(_Action), Cond) ==>> must(({old_figment(Cond, _F, _A, Old)},
  undeclare_always(Old))), !, declare(Cond).


oper_splitk(Agent, Action, Preconds, Postconds):-
  oper_db(Agent, Action, PrecondsK, PostcondsK),
  split_k(Agent, PrecondsK, Preconds),
  split_k(Agent, PostcondsK, Postconds).

split_k(_Agent, [], []) :- !.

split_k(Agent, [b(P, [V|VS])|PrecondsK], Preconds):- !,
  split_k(Agent, [b(P, V), b(P, VS)|PrecondsK], Preconds).

split_k(Agent, [~(k(P, X, Y))|PrecondsK], [believe(Agent, ~(h(spatial, P, X, Y))), ~(h(spatial, P, X, Y))|Preconds]):- !,
  split_k(Agent, PrecondsK, Preconds).
split_k(Agent, [k(P, X, Y)|PrecondsK], [believe(Agent, h(spatial, P, X, Y)), h(spatial, P, X, Y)|Preconds]):- !,
  split_k(Agent, PrecondsK, Preconds).
split_k(Agent, [~(b(P, X, Y))|PrecondsK], [believe(Agent, ~(h(spatial, P, X, Y)))|Preconds]):- !,
  split_k(Agent, PrecondsK, Preconds).
split_k(Agent, [b(P, X, Y)|PrecondsK], [believe(Agent, h(spatial, P, X, Y))|Preconds]):- !,
  split_k(Agent, PrecondsK, Preconds).
split_k(Agent, [isa(X, Y)|PrecondsK], [getprop(X, inherited(Y))|Preconds]):-
  split_k(Agent, PrecondsK, Preconds).
split_k(Agent, [in(X, Y)|PrecondsK], [h(spatial, in, X, Y)|Preconds]):-
  split_k(Agent, PrecondsK, Preconds).
split_k(Agent, [Cond|PrecondsK], [Cond|Preconds]):-
  split_k(Agent, PrecondsK, Preconds).

api_invoke( Action) :- get_advstate(S), api_invoke( Action, S, E), set_advstate(E).

api_invoke( Action) ==>> apply_aXioms( Action).

% apply_act( Action , S0, S9) :- apply_aXioms( Action , S0, S9).

apply_aXioms( Action) ==>> aXiom(Action), !.
apply_aXioms( Act, S0, S9):- axiom_Recalc_e(Act, RECALC, S0, S1),!, apply_aXioms(RECALC, S1, S9), !.
apply_aXioms( Act, S0, S9) :- ((cmd_workarround(Act, NewAct) -> Act\==NewAct)), !, apply_aXioms( NewAct, S0, S9).
apply_aXioms( Action, S0, S0) :-
  notrace((dbug(general, failed_aXiom( Action)))), !, fail, \+ tracing.


% must_act( Action , S0, S9) :- raise_aXiom_events( Action , S0, S9).

raise_aXiom_events( Action , S0, S9) :-
  (apply_aXioms( Action, S0, S9)) *-> ! ; fail.
raise_aXiom_events( Action, S0, S1) :- debugging(apply_aXioms), !, rtrace(apply_aXioms( Action, S0, S1)), !.
raise_aXiom_events( Action) ==>>
 action_doer(Action, Agent),
 send_1percept(Agent, [failure(Action, unknown_to(Agent, Action))]).


act_required_posses('lock', 'key', $agent).
act_required_posses('unlock', 'key', $agent).

:- defn_state_none(verb_domain(action, -domain)).
%verb_domain(touch, spatial).
verb_domain(Verb, spatial):- requires_spatially(Verb, touch),!.
verb_domain(_, spatial):-!.

act_change_opposite(f, t):-!.
act_change_opposite(t, f):-!.
act_change_opposite(F, T):- act_change_opposite_0(T, F), !.
act_change_opposite(T, F):- act_change_opposite_0(T, F), !.

act_change_opposite_0('close', 'open').
act_change_opposite_0(Auto, _):- parsed_as_simple(Auto), !, fail.
act_change_opposite_0(UnOpen, Open):- (atom(Open) -> \+ atom_concat('un', _, Open) ; atom(UnOpen)),
    atom_concat('un', Open, UnOpen).

act_change_can(X,Y):- var(X),!, Y=X.
act_change_can(X,Y):- act_change_opposite_0(X,Y),!.
act_change_can(X,Y):-  Y=X.

act_change_state(Auto, _, _):- parsed_as_simple(Auto), !, fail.
act_change_state(Open, Opened, Value):- nonvar(Value), !, act_change_state(Open, Opened, Val), !, Value=Val.
act_change_state('lock', 'locked', t).
act_change_state('open', 'opened', t).
act_change_state( switch(on), 'powered', t).
act_change_state( switch(off), 'powered', f).
act_change_state( switch(Open), Opened, TF):- nonvar(Open), !, act_change_state(Open, Opened, TF).

act_change_state(Close, Opened, Value):-
   act_change_state_0(Close, UnOpened, Val), nonvar(Val), as_true(UnOpened, Opened, TF),
   (TF == f -> once(act_change_opposite(Val, Value)) ; Value = Val).

as_true(UnOpen, Open, f):- atom(UnOpen), var(Open), atom_concat('un', Open, UnOpen), !.
as_true( Opened, Opened, t).

act_change_state_0(Light, Lit, t):- control_changed(Light, Lit), !.
act_change_state_0(Unlock, Locked, FT):-
  act_change_opposite(Unlock, Lock),
  act_change_state(Lock, Locked, TF), act_change_opposite(TF, FT).


act_change_state_or_fallback(Open, Opened, TF):- act_change_state(Open, Opened, TF), !.
act_change_state_or_fallback(UnOpen, Opened, F):- act_change_opposite(Open, UnOpen), act_change_state(Open, Opened, T), act_change_opposite(T, F).

% act_prevented_by(Open, Opened, TF):- act_change_state(Open, Opened, TF).
act_prevented_by('open', 'locked', t).
act_prevented_by('close', 'locked', t).


event_failed(Agent, CUZ):- simplify_reason(CUZ, Msg),
  episodic_mem(Agent, event_failed(Agent, CUZ)),
  player_format(Agent, '~N~p~n', [Msg]).

:- meta_predicate maybe_when(0, 0).
maybe_when(If, Then):- If -> Then ; true.

:- meta_predicate unless_reason(*, '//', *, ?, ?).
unless_reason(_Agent, Then, _Msg) ==>> Then, !.
unless_reason(Agent, _Then, Msg) ==>> {event_failed(Agent, Msg)}, !, {fail}.

:- meta_predicate unless_reason(*, '//', *, '//', ?, ?).
unless_reason(_Agent, Unless, _Msg, Then) ==>> Unless, !, Then.
unless_reason(Agent, _Unless, Msg, _Then) ==>> {event_failed(Agent, Msg)}, !.

:- meta_predicate unless(*, '//', '//', ?, ?).
unless(_Agent, Required, Then) ==>> Required, !, Then.
unless(Agent, Required, _Then) ==>> {simplify_reason(Required, CUZ), event_failed(Agent, cant( cuz(\+ CUZ)))}, !.

:- meta_predicate required_reason(*, 0).
required_reason(_Agent, Required):- Required, !.
required_reason(Agent, Required):- simplify_reason(Required, CUZ), event_failed(Agent, cant( cuz(CUZ))), !, fail.

simplify_reason(_:Required, CUZ):- !, simplify_reason(Required, CUZ).
simplify_reason(Required, CUZ):- simplify_dbug(Required, CUZ).

reverse_dir(north, south, _):-!.
reverse_dir(south, north, _):-!.
reverse_dir(reverse(ExitName), ExitName, _) :- nonvar(ExitName), !.
reverse_dir(Dir, RDir, S0):- 
 spatial_domain(Spatially),
 h(Spatially, fn(exit, Dir), Here, There, S0),
 h(Spatially, fn(exit, RDir), There, Here, S0), !.

reverse_dir(Dir, RDir, S0):-
 h(Spatially, Dir, Here, There, S0),
 h(Spatially, RDir, There, Here, S0), !.

reverse_dir(Dir, reverse(Dir), _).


/*
creates:

add_agent_intent( Agent, Action):-
 get_advstate(S0),
 add_agent_intent( Agent, Action, S0, S9),
 get_advstate(S9).
*/
:- defn_state_setter(add_agent_intent( agent, action)).

add_agent_intent( Agent, Action, S0, S9) :-
  pre_redeclare(memories(Agent, Mem0), S0, S1),
  add_intent( Agent, Action, Mem0, Mem1),
  redeclare(memories(Agent, Mem1), S1, S9).

add_agent_goal(Agent, Action, S0, S9) :-
  pre_redeclare(memories(Agent, Mem0), S0, S1),
  add_goal(Agent, Action, Mem0, Mem1),
  redeclare(memories(Agent, Mem1), S1, S9).

add_intent_look(Agent) ==>>
  h(spatial, inside, Agent, _Somewhere),
  add_agent_intent( Agent, attempts(Agent, act3('look',Agent,[]))).

% assert_if_new(arginfo_for(
%                  ':-'(
%                     uses_default_doer(Action),
%                     (safe_functor(Action,Verb,_216776) ,
%                      verbatum_anon(Verb))),
%                  mfl4(Mfl4P_Num4_V, mu,
%                     /.../(episodic_memory,'adv_action.pl'), 482),
%                  [ uses_default_doer(Action),
%                    verbatum_anon(Verb),
%                    safe_functor(Action,Verb,_216776) ])).

uses_default_doer(Action):- safe_functor(Action, Verb, _), verbatum_anon(Verb).
uses_default_doer(Action):- \+ compound(Action).

:- defn_state_none(action_doer(action, -agent)).
action_doer(Action, Agent):- uses_default_doer(Action), !, must_mw1(mu_current_agent(Agent)), !.
action_doer(Action, Agent):- arg(_, Action, Agent), nonvar(Agent), is_x_instance(Agent), !.

action_doer(Action, Agent):- action_verb_agent_args(Action, _Verb, Agent, _Thing),!.

action_doer(Action, Agent):- mu_current_agent(Agent), !, must_mw1(Action == Agent).
action_doer(Action, Agent):- trace, throw(missing(action_doer(Action, Agent))).

is_funct3(Action):- \+ compound(Action), !, fail.
is_funct3(Action):- \+ compound_name_arity(Action,_,3), !, fail.
is_funct3(Action):- functor(Action, act3, 3).
is_funct3(Action):- functor(Action, event3, 3).

action_verb_agent_thing(Action, Verb, Agent, Thing):-
  action_verb_agent_args(Action, Verb, Agent, Args),
  (Args=[[Thing]]->true;Args=[Thing]->true;Thing=_), !.

action_verb_agent_args(Atom, Verb, Agent, _):- atom(Atom),!,Atom = Verb, mu_current_agent(Agent),!.
action_verb_agent_args(event3(Verb, [Agent|Args], _), Verb, Agent, Args):-!.
action_verb_agent_args(act3(Verb, Agent, Args), Verb, Agent, Args):-!.

action_verb_agent_args(MetaAction, Verb, Agent, Args):- 
  arg(_, MetaAction, Action), is_funct3(Action), !, 
  action_verb_agent_args(Action, Verb, Agent, Args).

action_verb_agent_args(MetaAction, Verb, Agent, Args):- 
  arg(_, MetaAction, Action), is_funct3(Action),
    action_verb_agent_args(Action, Verb, Agent, Args), !.

action_verb_agent_args(Action, Verb, Agent, Args):-
  univ_safe(Action, [Verb, Agent|Args]),
  act_change_state(Verb, _, _), !.

action_verb_agent_args(Action, Verb, Agent, Args):-
  get_functor_types(action, Action, Types),
  univ_safe(Action, [Verb|Rest]), !,
  ((Types = [agent|_]) -> Rest = [Agent|Args] ; Args=Rest).
%action_verb_agent_args(Action, Verb, Agent, Args):-
% notrace((compound(Action), Action=..[Verb, Agent|Args], \+ verbatum_anon(Verb))), !.
action_verb_agent_args(Action, Verb, Agent, Args):-
  univ_safe(Action, [Verb, Agent|Args]),
  \+ verbatum_anon(Verb),
  Args\==[].



/*
disgorge(Doer, Verb, Container, At, Here, Vicinity, Msg) :-
  findall(Inner, h(Spatially, child, Inner, Container), Contents),
  dbug(general, '~p contained ~p~n', [Container, Contents]),
  moveto(Doer, Verb, Contents, At, Here, Vicinity, Msg).
disgorge(Doer, Verb, _Container, _At, _Here, _Vicinity, _Msg).
*/
disgorge(Doer, Verb, Container, Prep, Here, Vicinity, Msg) ==>>
  verb_domain( Verb, Spatially),
  findall(Inner, h(Spatially, child, Inner, Container), Contents),
   {dbug(general, '~p childs: ~p~n', [Container, Contents])},
  moveto(Doer, Verb, Contents, Prep, Here, Vicinity, Msg).

:- defn_state_setter(moveto(agent, verb, listof(inst), domrel, dest, list(dest), msg)).
moveto(Doer, Verb, List, At, Dest, Vicinity, Msg) ==>> {is_list(List)}, !,
 apply_mapl_rest_state(moveto(Doer, Verb), List, [At, Dest, Vicinity, Msg]).

moveto(Doer, Verb, Object, At, Dest, Vicinity, Msg) ==>>
  undeclare(h(spatial, _, Object, From)),
  declare(h(spatial, At, Object, Dest)),
  queue_local_event([act3('move', Doer, [Verb, Object, From, At, Dest]), Msg], Vicinity).


event_props( act3(Verb,Agent,[ Thing, _Target, Prep, Here, Vicinity]),
 [getprop(Thing, breaks_into(NewBrokenType)),
 dbug(general, 'object ~p is breaks_into~n', [Thing]),
  verb_domain( Verb, Spatially),
  undeclare(h(Spatially, _, Thing, _)),
  declare(h(Spatially, Prep, NewBrokenType, Here)),
 queue_local_event([transformed(Thing, NewBrokenType)], Vicinity),
  disgorge(Agent, do_throw, Thing, Prep, Here, Vicinity, 'Something falls out.')]):- Verb = 'throw'.


setloc_silent(Prep, Object, Dest) ==>>
 undeclare(h(spatial, _, Object, _)),
 declare(h(spatial, Prep, Object, Dest)).

state_value(Opened=TF, Opened, TF):-!.
state_value(NamedValue, Named, Value):-
  univ_safe(NamedValue, NamedValueL),
  append(NamedL, [Value], NamedValueL),
  univ_safe(Named, NamedL).

expected_truth(G) :- ignore(dshow_failure(G)).

change_state(Agent, Action, Thing, OpenedTF, S0, S):-
 change_state_0(Agent, Action, Thing, OpenedTF, S0, S), !.
change_state(Agent, Action, Thing, OpenedTF, S0, S):-
 ignore(rtrace(change_state_0(Agent, Action, Thing, OpenedTF, S0, S))), !.

change_state_0(Agent, Action, Thing, OpenedTF, S0, S):-
 action_verb_agent_thing(Action, Open, _, _),
 % must_mw1
 ((

 (act_change_can(Open,OpenClose)-> expected_truth(getprop(Thing, can_be(OpenClose, t), S0));true),

 expected_truth(required_reason(Agent, \+ getprop(Thing, can_be(Open, f), S0))),

 (
   maybe_when(proper_requires_spatially(Open, touch), 
     required_reason(Agent, 
       will_need_touch(Agent, Thing, S0, _)))),
   required_reason(Agent, \+ getprop(Thing, OpenedTF, S0)),

 %getprop(Thing, can_be(open, S0),
 %\+ getprop(Thing, =(open, t), S0),  

 forall(act_prevented_by(Open, Locked, Prevented),
   required_reason(Agent, \+ getprop(Thing, =(Locked, Prevented), S0))),

 %delprop(Thing, =(Open, f), S0, S1),
 %setprop(Thing, =(Open, t), S0, S1),

 expected_truth(open_traverse(Agent, Here, S0)),

 apply_forall(
  (getprop(Thing, effect(Open, Term0), S0),
  adv_subst(equivalent, $self, Thing, Term0, Term1),
  adv_subst(equivalent, $agent, Agent, Term1, Term2),
  adv_subst(equivalent, $here, Here, Term2, Term)),
  call(Term), S0, S1),

 setprop(Thing, OpenedTF, S1, S2))),

 queue_local_event([msg([Agent, ensures, Thing, is, OpenedTF])], [Here, Thing], S2, S), !.

end_of_file.


Some things that we call the proto-concepts


** during = as a subpart of


  Realize existance of a STAG
  Realize an subpart of an STAG as a different STAG in which can be refered to
  Realize STAG can be a slot-role for another STAG
  Select STAG as a slot-role for another STAG
  Create a new instance of STAG of type
  Copy a new STAG from another STAG
  Combine two STAGs to create a new STAG




  



