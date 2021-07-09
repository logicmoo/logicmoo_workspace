/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
% LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
%
% Feb 20, 2020 - Andrew Dougherty
% Copyright (C) 2004 Marty White under the GNU GPL
% Sept 20, 1999 - Douglas Miles
% July 10, 1996 - John Eikenberry
%
% Logicmoo Project changes:
%
%
*/
:- '$set_source_module'(mu).

:- if(\+ pldoc_loading).

:- op(1200, xfy, ('==>>')).
:- op(1200, xfy, ('::=')).

:- dynamic (aXiom//1, eVent//2).
:- multifile (aXiom//1, eVent//2).
:- discontiguous aXiom//1.
:- discontiguous eVent//2.

:- dynamic (aXiom_p1//1, aXiom_p2//2).
:- multifile (aXiom_p1//1, aXiom_p2//2).
:- discontiguous aXiom_p1//1.
:- discontiguous aXiom_p2//1.

:- defn_state_setter(aXiom//1 ).
:- defn_state_setter(aXiom_p1//1 ).
:- defn_state_setter(aXiom_p2//1 ).

:- defn_state_setter(eVent//2 ).

aXiom(X):- 
 get_advstate(State),
 aXiom(X,State,_),!.

%:- defn_state_getter(will_need_touch(agent,inst)).
%:- defn_state_setter(will_need_touch(agent,inst)).

:- expand_term((will_need_touch(Agent, Thing) ==>>
  h(spatial, touchable, Agent, Thing)), Dmsg), dmsg(Dmsg).

% :- trace.
will_need_touch(Agent, Thing) ==>>
  can_sense(Agent, _, Thing),
  h(spatial, touchable, Agent, Thing).

:- listing(will_need_touch).


eVent(Agent, Event) ==>>
 send_1percept(Agent, Event),
 must_mw1(apply_aXioms(Event)).

aXiom(MAction, S0, S9):-  stripped_term(MAction, Action), !, trace, aXiom(Action, S0, S9).
aXiom(Action, S0, S9):- axiom_Recalc_e(Action, RECALC, S0, S9), !, apply_aXioms(RECALC, S0, S9).
aXiom(Action, _S0, _S9)::= xnotrace(( \+ trival_act(Action), dbug1(aXiom(Action)))), xnotrace(fail).
aXiom(Action) ==>> aXiom_p1(Action).
aXiom(Action) ==>> aXiom_p2(Action).
:- add_bt_meta_processing(aXiom_p1).

/*
aXiom_p1(true) ==>> [].
aXiom_p1((A, B)) ==>> !,
  aXiom_p1(A), aXiom_p1(B).
aXiom_p1((A;B)) ==>> !,
  aXiom_p1(A) ; aXiom_p1(B).
aXiom_p1((A->B;C)) ==>> !,
  (aXiom_p1(A) -> aXiom_p1(B) ; aXiom_p1(C)).
aXiom_p1((A->B)) ==>> !,
  (aXiom_p1(A) -> aXiom_p1(B)).
aXiom_p1((A*->B;C)) ==>> !,
  (aXiom_p1(A) *-> aXiom_p1(B) ; aXiom_p1(C)).
aXiom_p1((A*->B)) ==>> !,
  (aXiom_p1(A) *-> aXiom_p1(B)).
*/

aXiom_p2(Action) ==>>
 {implications(DoesEvent, Action, Preconds, Postconds), action_doer(Action, Agent) },
 /*dmust_tracing*/(satisfy_each(preCond(_1), Preconds)),
 (((sg(member(failed(Why))), send_1percept(Agent, failed(Action, Why))))
    ; (satisfy_each(postCond(_2), Postconds), send_1percept(Agent, (Action)))),
 {episodic_mem(Agent, implications(DoesEvent, Action, Preconds, Postconds))},
 {dbug1(used_implications(DoesEvent, Action, Preconds, Postconds))},
 !.

aXiom_p1( Action) ==>>
 ({oper_splitk(Agent, Action, Preconds, Postconds)},
  {dbug1(using_oper_splitk(Agent, Action, Preconds, Postconds))},
 /*dmust_tracing*/
 (satisfy_each(preCond(_1), Preconds)),
 (((sg(member(failed(Why))), send_1percept(Agent, failed(Action, Why))))
    ; (satisfy_each(postCond(_2), Postconds), send_1percept(Agent, success(Action))))),
  {dbug1(used_oper_splitk(Agent, Action, Preconds, Postconds))},!.

aXiom_p2(Action, S, E) ::=
  append_termlist(Action, [S, E], ActionSE),
  current_predicate(_, mu:ActionSE),
  adv_safe_to_call(ActionSE),
  !,
  call(Action, S, E).

aXiom_p2(Action, S, E) ::=
  current_predicate(_, mu:Action), !,
  adv_safe_to_call(Action),
  call(Action), S=E.

adv_safe_to_call(Action):- 
 (predicate_property(Action,imported_from(M));(clause(Action,_,Cl),clause_property(Cl,module(M)))), !, nop(M), fail.

 
  

aXiom_p1( Action) ==>>
 action_doer(Action, Agent),
 invoke_introspect(Agent, Action, Answer),
 send_1percept(Agent, [answer(Answer), Answer]), !.

aXiom_p1(print_(Agent, Msg)) ==>>
  h(spatial, descended, Agent, Here),
  queue_local_event(msg_from(Agent, Msg), [Here]).

aXiom_p1( attempts(Agent, act3('wait',Agent,[]))) ==>>
 from_loc(Agent, Here),
 queue_local_event(time_passes(Agent), Here).

:- defn_state_getter(eng2cmd(agent, english, action)).
aXiom_p1( attempts(Agent, act3('english',Agent,[ English]))) ==>>
 eng2cmd(Agent, English, Action),
 add_intent( Agent, Action).

aXiom_p1(intent_english(Agent, English)) ==>> !,
  {assertz(mu_global:console_tokens(Agent, English))}.


aXiom_p1( attempts(Agent, act3('talk',Agent,[ Object, Message]))) ==>>  % directed message
  can_sense(Agent, audio, Object),
  from_loc(Agent, Here),
  queue_local_event([did(Agent, act3('talk',Agent,[ Here, Object, Message]))], [Here]).

aXiom_p1(say(Agent, Message)) ==>>          % undirected message
  from_loc(Agent, Here),
  queue_local_event([did(Agent, act3('talk',Agent,[ Here, *, Message]))], [Here]).


aXiom_p1( attempts(Agent, act3('emote',Agent,[ EmoteType, Object, Message]))) ==>> !, % directed message
 from_loc(Agent, Here),
 queue_local_event([ act3('emote',Agent,[ EmoteType, Object, Message])], [ Here]).



aXiom_p1(terminates(h(Spatially, Prep, Object, Here))) ==>> !, % {fail},
 %ignore(sg(declared(h(spatial, Prep, Object, Here)))),
 undeclare(h(Spatially, Prep, Object, Here)).

 % ==============
%  WALK ON TABLE
% ==============
aXiom_p1( attempts(Agent, act3('go__prep_obj',Agent,[ Walk, At, Object]))) ==>>
  will_need_touch(Agent, Object),
  has_rel(At, Object),
  \+ is_closed(At, Object),
 eVent(Agent, event3('arrive', [At, Agent, Object], [ Walk, At])).


aXiom_p1(initiates(h(Spatially, Prep, Object, Dest))) ==>> !, % {fail},
 declare(h(Spatially, Prep, Object, Dest)).


aXiom_p1(status_msg(_Begin, _End)) ==>> [].



% ==============
%  WALK TABLE
% ==============
axiom_Recalc_e( attempts(Agent, act3('go__obj',Agent,[ Walk, Object])), RECALC) ==>>
  has_rel(At, Object),
 RECALC = ( attempts(Agent, act3('go__prep_obj',Agent,[ Walk, At, Object]))).

% ==============
%  GOTO PANTRY
% ==============
aXiom_p1( attempts(Agent, act3('go__loc',Agent,[ _Walk, There]))) ==>> % go some room
  has_rel(fn(exit, _), There),
  eVent(Agent, make_true(Agent, h(spatial, in, Agent, There))).

aXiom_p1(make_true(Doer, h(spatial, in, Agent, There))) ==>>
  {Doer==Agent},
  has_rel(fn(exit, _), There),
  from_loc(Agent, Here),
  agent_thought_model(Agent, ModelData),
  {find_path(Doer, Here, There, Route, ModelData)}, !,
 eVent(Agent, follow_plan(Agent, did(Agent, act3('go__loc',Agent,[ walk, There])), Route)).

aXiom_p1(make_true(Agent, FACT)) ==>>
  add_agent_goal(Agent, FACT).

aXiom_p1(add_intent( Agent, TODO)) ==>>
  add_agent_intent( Agent, TODO).

aXiom_p1(follow_plan(Agent, Name, [])) ==>> !,
  send_1percept(Agent, [success(followed_plan(Agent, Name))]).

aXiom_p1(follow_plan(Agent, Name, [Step|Route])) ==>>
  % %% update_flp(Agent, Name, Route, Step),
  eVent(Agent, follow_step(Agent, Name, Step)),
  eVent(Agent, follow_plan(Agent, Name, Route)).

aXiom_p1(follow_step(Agent, Name, Step)) ==>>
  {dbug1(follow_step(Agent, Name, Step))},
  raise_aXiom_events(Step).


%  sim(verb(args...), preconds, effects)
%    Agent is substituted for Agent.
%    preconds are in the implied context of a State.
%  In Inform, the following are implied context:
%    actor, action, noun, second
%  Need:
%    actor/agent, verb/action, direct-object/obj1, indirect-object/obj2,
%      preposition-introducing-obj2
%sim(put(Obj1, Obj2),
%    (  h(spatial, descended, Thing, Agent),
%      can_sense(Agent, Sense, Agent, Where),
%      has_rel(Relation, Where),
%      h(spatial, descended, Agent, Here)),
%    moveto(Agent, Put, Thing, Relation, Where, [Here],
%      [cap(subj(Agent)), person('put the', 'puts a'),
%        Thing, Relation, the, Where, '.'])).

axiom_Recalc_e( attempts(Agent, act3('take', Agent,[ Thing])), RECALC) ==>> !,
 RECALC = ( attempts(Agent, act3('put__via',Agent, [take, Thing, held_by, Agent]))).

axiom_Recalc_e( attempts(Agent, act3('drop',Agent,[ Thing])), RECALC) ==>> !,
  dshow_failure(h(spatial, At, Agent, Here)),
 RECALC = ( attempts(Agent, act3('put__via', Agent,[ drop, Thing, At, Here]))).

axiom_Recalc_e( attempts(Agent, act3('put', Agent,[ Thing1, PrepAt, Thing2])), RECALC) ==>>
 RECALC = ( attempts(Agent, act3('put__via', Agent,[ put, Thing1, PrepAt, Thing2]))).

axiom_Recalc_e( attempts(Agent, act3('give', Agent,[ Thing, Recipient])), RECALC) ==>>
 RECALC = ( attempts(Agent, act3('put__via', Agent,[ give, Thing, held_by, Recipient]))).


aXiom_p1( attempts(Agent, act3('put__via', Agent, [How, Thing1, Prep, Thing2]))) ==>> {fail}, !,
  dshow_failure(prep_to_rel(Thing2, Prep, At)),
  from_loc(Agent, Here),
  % dshow_failure((At \= in ; \+ is_closed(At, Thing2))),
  dshow_failure(has_rel(At, Thing2)),
  dshow_failure(will_need_touch(Agent, Thing2)), % what if "under" an "untouchable" thing?
  % OK, put it
  moveto(Agent, How, Thing1, At, Thing2, [Here],
    [cap(subj(Agent)), person(How, es(How)), Thing1, At, Thing2, '.']).


% do_throw ball up
axiom_Recalc_e( attempts(Agent, act3('throw_dir',Agent,[ Thing, ExitName])), RECALC) ==>>
  from_loc(Agent, Here),
 RECALC = ( attempts(Agent, act3('throw_prep_obj',Agent,[ Thing, ExitName, Here]))).

% throw ball at catcher
axiom_Recalc_e( attempts(Agent, act3('throw_at',Agent,[ Thing, Target])), RECALC) ==>>
 RECALC = ( attempts(Agent, act3('throw_prep_obj',Agent,[ Thing, at, Target]))).

% do_throw ball over homeplate
axiom_Recalc_e( attempts(Agent, act3('throw_prep_obj',Agent,[ Thing, Prep, Target])), RECALC) ==>>
  prep_to_rel(Target, Prep, Rel),
 RECALC = ( attempts(Agent, act3('put',Agent,['throw', Thing, Rel, Target]))).

axiom_Recalc_e( attempts(Agent, act3('throw',Agent,[ Thing, Prep, Target])), RECALC) ==>>
 (prep_to_rel(Target, Prep, Rel);Prep=Rel),
 RECALC = ( attempts(Agent, act3('put',Agent,['throw', Thing, Rel, Target]))).

% is throwing the ball...
aXiom_p1( attempts(Agent, act3('throw',Agent,[ Thing, At, Target]))) ==>>
  will_need_touch(Agent, Thing),
  can_sense(Agent, see, Target),
 eVent(Agent, act3('throw', Agent,[ Thing, At, Target])).

% has thrown the ball...
aXiom_p1( act3('throw', Agent,[ Thing, AtTarget, Target])) ==>>
  ignore((getprop(Thing, breaks_into(Broken)),
  dbug(general, 'object ~p is breaks_into~n', [Thing]),
  eVent(Agent, thing_transforms(Thing, Broken)))),
 eVent(Agent, disgorge(Agent, do_throw, Target, AtTarget, Target, [ Target], 'Something falls out.')).

aXiom_p1(thing_transforms(Thing, Broken))  ==>>
  undeclare(h(Spatial, At, Thing, Here)),
  declare(h(Spatial, At, Broken, Here)),
  queue_local_event([transformed(Thing, Broken)], Here).


aXiom_p1( attempts(Agent, act3('hit_with',Agent,[ Thing, With]))) ==>>
  from_loc(Agent, Here),
  invoke_hit(Agent, Thing, With, [Here]),
  send_1percept(Agent, [true, 'OK.']).

aXiom_p1( attempts(Agent, act3('hit',Agent,[ Thing]))) ==>>
  from_loc(Agent, Here),
  invoke_hit(Agent, Thing, Agent, [Here]),
  send_1percept(Agent, [true, 'OK.']).

invoke_hit(Doer, Target, _With, Vicinity) ==>>
 ignore(( % Only brittle items use this
  getprop(Target, breaks_into(Broken)),
  dbug(general, 'target ~p is breaks_into~n', [Target]),
  undeclare(h(Spatial, Prep, Target, Here)),
  queue_local_event([transformed(Target, Broken)], Vicinity),
  declare(h(Spatial, Prep, Broken, Here)),
  disgorge(Doer, hit, Target, Prep, Here, Vicinity, 'Something falls out.'))).


aXiom_p1( attempts(Agent, act3('dig',Agent,[ Hole, Where, Tool]))) ==>>
  {memberchk(Hole, [hole, trench, pit, ditch]),
  memberchk(Where, [garden]),
  memberchk(Tool, [shovel, spade])},
  open_traverse(Tool, Agent),
  from_loc(Agent, Where),
  \+  h(spatial, _At, Hole, Where),
  % OK, dig the hole.
  declare(h(spatial, in, Hole, Where)),
  setprop(Hole, default_rel = in),
  setprop(Hole, can_be(move, f)),
  setprop(Hole, can_be(take, f)),
  declare(h(spatial, in, dirt, Where)),
  queue_event(
    [ created(Hole, Where),
      [cap(subj(Agent)), person(dig, digs), 'a', Hole, 'in the', Where, '.']]).

aXiom_p1( attempts(Agent, act3('eat',Agent,[ Thing]))) ==>>
  (getprop(Thing, can_be(eat, t)) ->
  (undeclare(h(spatial, _1, Thing, _2)), send_1percept(Agent, [destroyed(Thing), 'Mmmm, good!'])) ;
  send_1percept(Agent, [failure(attempts(Agent, act3('eat',Agent,[ Thing]))), 'It''s inedible!'])).


aXiom_p1( attempts(Agent, act3('switch',Agent,[ OnOff, Thing]))) ==>>
  will_need_touch(Agent, Thing),
  getprop(Thing, can_be( switch(OnOff), t)),
  getprop(Thing, effect( switch(OnOff), Term0)),
  {adv_subst(equivalent, ($(self)), Thing, Term0, Term)},
  call(Term),
  send_1percept(Agent, [success(true, 'OK')]).
 
  
axiom_Recalc_e( attempts(Agent, act3('inventory',Who,[])), RECALC) ==>> 
  RECALC = ( attempts(Agent, act3('examine',Agent,[ Who]))).




% Agent looks
axiom_Recalc_e( attempts(Agent, act3('look',Agent,[])), RECALC) ==>>
  % Agent is At Here
  % from_loc(Agent, Here),
  h(spatial, At, Agent, Here),
  % Agent looks At Here
  RECALC = ( attempts(Agent, act3('examine__D3',Agent,[ see, At, Here]))).

axiom_Recalc_e( attempts(Agent, act3('examine',Agent,[ Sense])), RECALC) ==>> {is_sense(Sense)}, !, from_loc(Agent, Place), RECALC = ( attempts(Agent, act3('examine__D3',Agent,[ see, in, Place]))).
axiom_Recalc_e( attempts(Agent, act3('examine',Agent,[ Object])), RECALC) ==>> RECALC = ( attempts(Agent, act3('examine__D3',Agent,[ see, at, Object]))).
axiom_Recalc_e( attempts(Agent, act3('examine',Agent,[ Sense, Object])), RECALC) ==>> RECALC = ( attempts(Agent, act3('examine__D3',Agent,[ Sense, at, Object]))), !.
axiom_Recalc_e( attempts(Agent, act3('examine',Agent,[ Sense, Prep, Object])), RECALC) ==>> RECALC = ( attempts(Agent, act3('examine__D3', Agent, [ Sense, Prep, Object]))), !.

axiom_Recalc_e( attempts(Agent, act3('examine',Agent,[ Sense, Prep, Object, Depth])), RECALC) ==>> RECALC = ( attempts(Agent, act3('examine__D3',Agent,[ Sense, Prep, Object, Depth]))), !.


axiom_Recalc_e( attempts(Agent, act3('examine__D3',Agent,[ Sense, Prep, Object])), RECALC) ==>> RECALC = ( attempts(Agent, act3('examine__D5',Agent,[ Sense, Prep, Object, 3]))), !.
axiom_Recalc_e( attempts(Agent, act3('examine__D3',Agent,[ Sense, Prep, Object, Depth])), RECALC) ==>> RECALC = ( attempts(Agent, act3('examine__D5',Agent,[ Sense, Prep, Object, Depth]))), !.


% Here does not allow Sense?
aXiom_p1( attempts(Agent, act3('examine__D5',Agent,[ Sense, Prep, Object, Depth]))) ==>>
  \+ sg(can_sense_here(Agent, Sense)), !,
  raise_aXiom_events( failed( did(Agent, act3('examine',Agent,[ Sense, Prep, Object, Depth])), \+ can_sense_here(Agent, Sense))).
aXiom_p1( attempts(Agent, act3('examine__D5',Agent,[ Sense, Prep, Object, Depth]))) ==>>
  \+ sg(can_sense(Agent, Sense, Object)), !,
  raise_aXiom_events( failed( did(Agent, act3('examine',Agent,[ Sense, Prep, Object, Depth])), \+ can_sense(Agent, Sense, Object))).
aXiom_p1( attempts(Agent, act3('examine__D5',Agent,[ Sense, Prep, Object, Depth]))) 
   ==>> must_mw1(act_examine(Agent, Sense, Prep, Object, Depth)), !.


% used mainly to debug if things are locally accessable
aXiom_p1( attempts(Agent, act3('touch',Agent,[ Thing]))) ==>> !,
 unless_reason(Agent, will_need_touch(Agent, Thing),
                      cant( reach(Agent, Thing)),
                      send_1percept(Agent, [success( act3('touch',Agent,[ Thing]), 'Ok.')])).


aXiom_p1(change_state(Agent, Action, Thing, Prop)) ==>> !,
  change_state(Agent, Action, Thing, Prop).


aXiom_p1(Action, S0, S9) ::=
 action_verb_agent_thing(Action, Verb, Agent, Thing),
 nonvar(Verb), nonvar(Thing), nonvar(Agent),
 act_change_state_or_fallback(Verb, State, TF), !,
 eVent(Agent, change_state(Agent, Action, Thing, State=TF), S0, S9), !.



/*

aXiom_p1( switch(Open, Thing)) ==>>
 act_prevented_by(Open, TF),
 will_need_touch(Agent, Thing),
 %getprop(Thing, can_be(open),
 %\+ getprop(Thing, =(open, t)),
 Open = open, traverses(Sense, Open)
 %delprop(Thing, =(Open, f)),
 %setprop(Thing, =(open, t)),
 setprop(Thing, =(Open, TF)),
 h(spatial, Sense, Agent, Here),
 queue_local_event([setprop(Thing, =(Open, TF)), [Open, is, TF]], [Here, Thing]).

aXiom_p1( switch(OnOff, Thing)) ==>>
 will_need_touch(Agent, Thing),
 getprop(Thing, can_be(switch, t)),
 getprop(Thing, effect( switch(OnOff), Term0)),
 adv_subst(equivalent, $self, Thing, Term0, Term),
 call(Term),
 send_1percept(Agent, [true, 'OK']).
*/
% attempts

/*
aXiom_p1( Action) ==>> fail,
  action_doer(Action, Agent),
  copy_term(Action, ActionG),
  from_loc(Agent, Here, S0),
  % queue_local_event(spatial, [attempts(Agent, Action)], [Here], S0, S1),
  act( Action), !,
 queue_local_event([ did('emote',Agent, aXiom_p1, 
 '*'(Here), ActionG)], [ Here], S0, S9).
*/



:- endif.
