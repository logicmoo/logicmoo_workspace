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


:- dynamic (aXiom//1, eVent//2
   ).
:- multifile (aXiom//1, eVent//2
   ).
:- discontiguous aXiom//1.
:- discontiguous eVent//2.

:- defn_state_setter(aXiom//1
  ).
:- defn_state_setter(eVent//2
  ).

:- expand_term((will_touch(Agent, Thing) ==>>
  h(touchable, Agent, Thing)),_).
% :- trace.
will_touch(Agent, Thing) ==>>
  h(touchable, Agent, Thing).

eVent(Agent, Event) ==>>
 send_1percept(Agent, Event),
 aXiom(Event).


aXiom(Action, _S0, _S9)::= xnotrace(( \+ trival_act(Action), dbug1(aXiom(Action)))), xnotrace(fail).


aXiom( Action) ==>>
 action_doer(Action, Agent),
 do_introspect(Agent, Action, Answer),
 send_1percept(Agent, [answer(Answer), Answer]), !.

aXiom(print_(Agent, Msg)) ==>>
  h(descended, Agent, Here),
  queue_local_event(msg_from(Agent, Msg), [Here]).

aXiom(wait(Agent)) ==>>
 from_loc(Agent, Here),
 queue_local_event(time_passes(Agent), Here).

aXiom(Action) ==>>
 {implications(_DoesEvent, (Action), Preconds, Postconds), action_doer(Action, Agent) },
 /*dmust_tracing*/(satisfy_each(preCond(_1), Preconds)),
 (((sg(member(failed(Why))), send_1percept(Agent, failed(Action, Why))))
    ; (satisfy_each(postCond(_2), Postconds), send_1percept(Agent, (Action)))), !.

aXiom( Action) ==>>
 {oper_splitk(Agent, Action, Preconds, Postconds)},
 /*dmust_tracing*/
 (satisfy_each(preCond(_1), Preconds)),
 (((sg(member(failed(Why))), send_1percept(Agent, failed(Action, Why))))
    ; (satisfy_each(postCond(_2), Postconds), send_1percept(Agent, success(Action)))), !.


:- defn_state_getter(eng2cmd(agent, english, action)).
aXiom(do_english(Agent, English)) ==>>
 eng2cmd(Agent, English, Action),
 add_todo(Agent, Action).

aXiom(todo_english(Agent, English)) ==>> !,
  {assertz(mu_global:console_tokens(Agent, English))}.


aXiom(talk(Agent, Object, Message)) ==>>  % directed message
  can_sense(Agent, audio, Object),
  from_loc(Agent, Here),
  queue_local_event([talk(Agent, Here, Object, Message)], [Here]).

aXiom(say(Agent, Message)) ==>>          % undirected message
  from_loc(Agent, Here),
  queue_local_event([talk(Agent, Here, *, Message)], [Here]).


aXiom(emote(Agent, EmoteType, Object, Message)) ==>> !, % directed message
 from_loc(Agent, Here),
 queue_local_event([emoted(Agent, EmoteType, Object, Message)], [Here]).




% ==============
%  WALK WEST
% ==============

aXiom(go_dir(Agent, Walk, ExitName)) ==>> !,  %{fail}, % go n/s/e/w/u/d/in/out
  must_act(status_msg(vBegin, go_dir(Agent, Walk, ExitName))),
  % {break},
  must_mw1(from_loc(Agent, Here)),
  %must_mw1(h(exit(ExitName), Here, _There)),
  unless(Agent, h(exit(ExitName), Here, _There),
  (eVent(Agent, departing(Agent, in, Here, Walk, ExitName)),
   must_act(status_msg(vDone, go_dir(Agent, Walk, ExitName))))).

aXiom(departing(Agent, in, Here, Walk, ExitName)) ==>> !,% {fail},
  %member(At, [*, to, at, through, thru]),
  h(exit(ExitName), Here, There),
  eVent(Agent, terminates(h(_, Agent, Here))),
  queue_local_event( departing(Agent, Here, Walk, ExitName), [Here]),
   % queue_local_event( msg([cap(subj(Agent)), leaves, Here, ing(Walk), to, the, ExitName]), [Here]).
  sg(reverse_dir(ExitName, ExitNameR)),
  must_mw1(eVent(Agent, arriving(Agent, There, Walk, ExitNameR))).

aXiom(terminates(h(Prep, Object, Here))) ==>> !,% {fail},
 %ignore(sg(declared(h(Prep, Object, Here)))),
 undeclare(h(Prep, Object, Here)).

aXiom(arriving(Agent, Here, Walk, ReverseDir)) ==>>  !,% {fail},
  queue_local_event( arriving(Agent, Here, Walk, ReverseDir), [Here]),
  %sg(default_rel(PrepIn, Here)), {atom(PrepIn)},
  {PrepIn = in},
  % [cap(subj(Agent)), arrives, PrepIn, Here, ing(Walk), from, the, ReverseDir]
  must_mw1(eVent(Agent, initiates(h(PrepIn, Agent, Here)))),
  must_mw1(add_look(Agent)).

aXiom(initiates(h(Prep, Object, Dest))) ==>> !,% {fail},
 declare(h(Prep, Object, Dest)).


aXiom(status_msg(_Begin, _End)) ==>> [].


% ==============
%  WALK TABLE
% ==============
aXiom(goto_obj(Agent, Walk, Object)) ==>>
  has_rel(At, Object),
  eVent(Agent, goto_prep_obj(Agent, Walk, At, Object)).


% ==============
%  WALK ON TABLE
% ==============
aXiom(goto_prep_obj(Agent, Walk, At, Object)) ==>>
  will_touch(Agent, Object),
  has_rel(At, Object),
  \+ is_closed(At, Object),
  eVent(Agent, arriving(Agent, Walk, Object, At)).

aXiom(arriving(Agent, Walk, Object, At)) ==>>
  from_loc(Object, Here),
  moveto(Agent, Walk, Agent, At, Object, [Here],
    [subj(Agent), person(Walk, es(Walk)), At, the, Object, .]),
  add_look(Agent).

% ==============
%  GOTO PANTRY
% ==============
aXiom(goto_loc(Agent, _Walk, There)) ==>>           % go some room
  has_rel(exit(_), There),
  eVent(Agent, make_true(Agent, h(in, Agent, There))).

aXiom(make_true(Doer, h(in, Agent, There))) ==>>
  {Doer==Agent},
  has_rel(exit(_), There),
  from_loc(Agent, Here),
  agent_thought_model(Agent, ModelData),
  {find_path(Doer, Here, There, Route, ModelData)}, !,
  eVent(Agent, follow_plan(Agent, goto_loc(Agent, walk, There), Route)).

aXiom(make_true(Agent, FACT)) ==>>
  add_agent_goal(Agent, FACT).

aXiom(add_todo(Agent, TODO)) ==>>
  add_agent_todo(Agent, TODO).

aXiom(follow_plan(Agent, Name, [])) ==>> !,
  send_1percept(Agent, [success(followed_plan(Agent, Name))]).

aXiom(follow_plan(Agent, Name, [Step|Route])) ==>>
  % %% update_flp(Agent, Name, Route, Step),
  eVent(Agent, follow_step(Agent, Name, Step)),
  eVent(Agent, follow_plan(Agent, Name, Route)).

aXiom(follow_step(Agent, Name, Step)) ==>>
  {dbug1(follow_step(Agent, Name, Step))},
  must_act(Step).


%  sim(verb(args...), preconds, effects)
%    Agent is substituted for Agent.
%    preconds are in the implied context of a State.
%  In Inform, the following are implied context:
%    actor, action, noun, second
%  Need:
%    actor/agent, verb/action, direct-object/obj1, indirect-object/obj2,
%      preposition-introducing-obj2
%sim(put(Obj1, Obj2),
%    (  h(descended, Thing, Agent),
%      can_sense(Agent, Sense, Agent, Where),
%      has_rel(Relation, Where),
%      h(descended, Agent, Here)),
%    moveto(Agent, Put, Thing, Relation, Where, [Here],
%      [cap(subj(Agent)), person('put the', 'puts a'),
%        Thing, Relation, the, Where, '.'])).
aXiom(does_put(Agent, Put, Thing1, At, Thing2)) ==>>
  from_loc(Agent, Here),
  % moveto(Agent, Put, Thing1, held_by, Recipient, [Here], [cap(subj(Agent)), person([give, Recipient, the], 'gives you a'), Thing, '.'],
  moveto(Agent, Put, Thing1, At, Thing2, [Here],
    [cap(subj(Agent)), person(Put, es(Put)), Thing1, At, Thing2, '.']).

aXiom(take(Agent, Thing)) ==>> !,
  % [silent(subj(Agent)), person('Taken.', [cap(Doer), 'grabs the', Thing, '.'])]
  dshow_failure(will_touch(Agent, Thing)),
  eVent(Agent, does_put(Agent, take, Thing, held_by, Agent)).

aXiom(drop(Agent, Thing)) ==>> !,
  will_touch(Agent, Thing),
  h(At, Agent, Here),
  % has_rel(At, Here),
  eVent(Agent, does_put(Agent, drop, Thing, At, Here)).

aXiom(put(Agent, Thing1, Prep, Thing2)) ==>>
  has_rel(At, Thing2),
  prep_to_rel(Thing2, Prep, At),
  (At \= in ; \+ is_closed(At, Thing2)),
  will_touch(Agent, Thing2), % what if "under" an "untouchable" thing?
  % OK, put it
  must_act( does_put(Agent, put, Thing1, At, Thing2)).

aXiom(give(Agent, Thing, Recipient)) ==>>
  has_rel(held_by, Recipient),
  will_touch(Agent, Thing),
  will_touch(Recipient, Agent),
  % OK, give it
  must_act( does_put(Agent, give, Thing, held_by, Recipient)).

% throw ball up
aXiom(throw_dir(Agent, Thing, ExitName)) ==>>
  from_loc(Agent, Here),
  eVent(Agent, throw_prep_obj(Agent, Thing, ExitName, Here)).

% throw ball at catcher
aXiom(throw_at(Agent, Thing, Target)) ==>>
  eVent(Agent, throw_prep_obj(Agent, Thing, at, Target)).

% throw ball over homeplate
aXiom(throw_prep_obj(Agent, Thing, Prep, Target)) ==>>
  prep_to_rel(Target, Prep, Rel),
  eVent(Agent, throwing(Agent, Thing, Rel, Target)).

% is throwing the ball...
aXiom(throwing(Agent, Thing, At, Target)) ==>>
  will_touch(Agent, Thing),
  can_sense(Agent, see, Target),
  eVent(Agent, thrown(Agent, Thing, At, Target)).

% has thrown the ball...
aXiom(thrown(Agent, Thing, AtTarget, Target)) ==>>
  ignore((getprop(Thing, breaks_into(Broken)),
  dbug(general, 'object ~p is breaks_into~n', [Thing]),
  eVent(Agent, thing_transforms(Thing, Broken)))),
  eVent(Agent, disgorge(Agent, throw, Target, AtTarget, Target, [Target], 'Something falls out.')).

aXiom(thing_transforms(Thing, Broken))  ==>>
  undeclare(h(At, Thing, Here)),
  declare(h(At, Broken, Here)),
  queue_local_event([transformed(Thing, Broken)], Here).


aXiom(hit_with(Agent, Thing, With)) ==>>
  from_loc(Agent, Here),
  hit(Agent, Thing, With, [Here]),
  send_1percept(Agent, [true, 'OK.']).

aXiom(hit(Agent, Thing)) ==>>
  from_loc(Agent, Here),
  hit(Agent, Thing, Agent, [Here]),
  send_1percept(Agent, [true, 'OK.']).

hit(Doer, Target, _With, Vicinity) ==>>
 ignore(( % Only brittle items use this
  getprop(Target, breaks_into(Broken)),
  dbug(general, 'target ~p is breaks_into~n', [Target]),
  undeclare(h(Prep, Target, Here)),
  queue_local_event([transformed(Target, Broken)], Vicinity),
  declare(h(Prep, Broken, Here)),
  disgorge(Doer, hit, Target, Prep, Here, Vicinity, 'Something falls out.'))).


aXiom(dig(Agent, Hole, Where, Tool)) ==>>
  {memberchk(Hole, [hole, trench, pit, ditch]),
  memberchk(Where, [garden]),
  memberchk(Tool, [shovel, spade])},
  open_traverse(Tool, Agent),
  h(in, Agent, Where),
  \+  h(_At, Hole, Where),
  % OK, dig the hole.
  declare(h(in, Hole, Where)),
  setprop(Hole, default_rel = in),
  setprop(Hole, can_be(move, f)),
  setprop(Hole, can_be(take, f)),
  declare(h(in, dirt, Where)),
  queue_event(
    [ created(Hole, Where),
      [cap(subj(Agent)), person(dig, digs), 'a', Hole, 'in the', Where, '.']]).

aXiom(eat(Agent, Thing)) ==>>
  (getprop(Thing, can_be(eat, t)) ->
  (undeclare(h(_1, Thing, _2)), send_1percept(Agent, [destroyed(Thing), 'Mmmm, good!'])) ;
  send_1percept(Agent, [failure(eat(Thing)), 'It''s inedible!'])).


aXiom(switch(Agent, OnOff, Thing)) ==>>
  will_touch(Agent, Thing),
  getprop(Thing, can_be(switch(OnOff), t)),
  getprop(Thing, effect(switch(OnOff), Term0)),
  {adv_subst(equivalent, ($(self)), Thing, Term0, Term)},
  call(Term),
  send_1percept(Agent, [success(true, 'OK')]).

aXiom(inventory(Agent)) ==>>
  can_sense(Agent, see, Agent),
  must_act( does_inventory(Agent)).

aXiom(does_inventory(Agent)) ==>>
  eVent(Agent, examine(Agent, Agent)).
  %findall(What, h(child, What, Agent), Inventory),
  %send_1percept(Agent, [rel_to(held_by, Inventory)]).




% Agent looks
aXiom(look(Agent)) ==>>
  % Agent is At Here
  h(At, Agent, Here),
  % Agent looks At Here
  eVent(Agent, sub__examine(Agent, see, At, Here, 3)).

aXiom(examine(Agent, Sense)) ==>> {is_sense(Sense)}, !,
   from_loc(Agent, Place),
   eVent(Agent, sub__examine(Agent, see, in, Place, 3)).

aXiom(examine(Agent, Object)) ==>> eVent(Agent, sub__examine(Agent, see, at, Object, 3)).
aXiom(examine(Agent, Sense, Object)) ==>> eVent(Agent, sub__examine(Agent, Sense, at, Object, 3)), !.
aXiom(examine(Agent, Sense, Prep, Object)) ==>> eVent(Agent, sub__examine(Agent, Sense, Prep, Object, 3)), !.

% listen, smell ...
aXiom(Action) ==>>
 {xnotrace((Action=..[Verb, Agent|Args],
 sensory_verb(Sense, Verb)))}, !,
 {NewAction=..[examine, Agent, Sense|Args]},
 eVent(Agent, NewAction).

% Here does not allow Sense?
aXiom(sub__examine(Agent, Sense, Prep, Object, Depth)) ==>>
  \+ sg(can_sense_here(Agent, Sense)), !,
  must_act( failed(examine(Agent, Sense, Prep, Object, Depth), \+ can_sense_here(Agent, Sense))).
aXiom(sub__examine(Agent, Sense, Prep, Object, Depth)) ==>>
  \+ can_sense(Agent, Sense, Object), !,
  must_act( failed(examine(Agent, Sense, Prep, Object, Depth), \+ can_sense(Agent, Sense, Object))).
aXiom(sub__examine(Agent, Sense, Prep, Object, Depth)) ==>> must_mw1(act_examine(Agent, Sense, Prep, Object, Depth)), !.


% used mainly to debug if things are locally accessable
aXiom(touch(Agent, Thing)) ==>> !,
 unless_reason(Agent, will_touch(Agent, Thing),
   cant( reach(Agent, Thing))),
 send_1percept(Agent, [success(touch(Agent, Thing), 'Ok.')]).


aXiom(change_state(Agent, Action, Thing, Prop)) ==>> !,
  change_state(Agent, Action, Thing, Prop).
 

aXiom(Action, S0, S9) ::=
 action_verb_agent_thing(Action, Open, Agent, Thing),
 nonvar(Open), nonvar(Thing), nonvar(Agent),
 act_change_state_or_fallback(Open, Opened, TF), !,
 eVent(Agent, change_state(Agent, Action, Thing, Opened=TF), S0, S9), !.


aXiom(Action,S,E) ::= 
  current_predicate(_,mu:Action), !,
  call(Action,S,E).


:- add_bt_meta_processing(aXiom).

/*
aXiom(true) ==>> [].
aXiom((A, B)) ==>> !,
  aXiom(A), aXiom(B).
aXiom((A;B)) ==>> !,
  aXiom(A) ; aXiom(B).
aXiom((A->B;C)) ==>> !,
  (aXiom(A) -> aXiom(B) ; aXiom(C)).
aXiom((A->B)) ==>> !,
  (aXiom(A) -> aXiom(B)).
aXiom((A*->B;C)) ==>> !,
  (aXiom(A) *-> aXiom(B) ; aXiom(C)).
aXiom((A*->B)) ==>> !,
  (aXiom(A) *-> aXiom(B)).
*/


/*

aXiom(switch(Open, Thing)) ==>>
 act_prevented_by(Open, TF),
 will_touch(Agent, Thing),
 %getprop(Thing, can_be(open),
 %\+ getprop(Thing, =(open, t)),
 Open = open, traverses(Sense, Open)
 %delprop(Thing, =(Open, f)),
 %setprop(Thing, =(open, t)),
 setprop(Thing, =(Open, TF)),
 h(Sense, Agent, Here),
 queue_local_event([setprop(Thing, =(Open, TF)), [Open, is, TF]], [Here, Thing]).

aXiom(switch(OnOff, Thing)) ==>>
 will_touch(Agent, Thing),
 getprop(Thing, can_be(switch, t)),
 getprop(Thing, effect(switch(OnOff), Term0)),
 adv_subst(equivalent, $self, Thing, Term0, Term),
 call(Term),
 send_1percept(Agent, [true, 'OK']).
*/
% todo

/*
aXiom( Action) ==>> fail,
  action_doer(Action, Agent),
  copy_term(Action, ActionG),
  from_loc(Agent, Here, S0),
  % queue_local_event(spatial, [attempts(Agent, Action)], [Here], S0, S1),
  act( Action), !,
  queue_local_event([emoted(Agent, aXiom, '*'(Here), ActionG)], [Here], S0, S9).
*/








