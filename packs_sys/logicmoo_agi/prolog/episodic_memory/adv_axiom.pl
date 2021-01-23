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

:- if(\+ pldoc_loading).

:- op(1200, xfy, ('==>>')).
:- op(1200, xfy, ('::=')).

:- dynamic (aXiom//1, eVent//2).
:- multifile (aXiom//1, eVent//2).
:- discontiguous aXiom//1.
:- discontiguous eVent//2.

:- defn_state_setter(aXiom//1 ).

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

recalcAx( Event,S,E):- aXiom( Event,S,E).

eVent(Agent, Event) ==>>
 send_1percept(Agent, Event),
 must_mw1(aXiom(Event)).

aXiom(MAction, S0, S9):-  stripped_term(MAction, Action), !, trace, aXiom(Action, S0, S9).
aXiom(Action, S0, S9):- axiom_Recalc_e(Action, RECALC, S0, S9), !, aXiom(RECALC, S0, S9).
aXiom(Action, S0, S9):- axiom_Recalc(Action, S0, S9).

aXiom( Action) ==>>
 action_doer(Action, Agent),
 invoke_introspect(Agent, Action, Answer),
 send_1percept(Agent, [answer(Answer), Answer]), !.

aXiom(print_(Agent, Msg)) ==>>
  h(spatial, descended, Agent, Here),
  queue_local_event(msg_from(Agent, Msg), [Here]).

aXiom( try(Agent, act3('wait',Agent,[]))) ==>>
 from_loc(Agent, Here),
 queue_local_event(time_passes(Agent), Here).

aXiom(Action) ==>>
 {implications(DoesEvent, Action, Preconds, Postconds), action_doer(Action, Agent) },
 /*dmust_tracing*/(satisfy_each(preCond(_1), Preconds)),
 (((sg(member(failed(Why))), send_1percept(Agent, failed(Action, Why))))
    ; (satisfy_each(postCond(_2), Postconds), send_1percept(Agent, (Action)))),
 {episodic_mem(Agent, implications(DoesEvent, Action, Preconds, Postconds))},
 {dbug1(used_implications(DoesEvent, Action, Preconds, Postconds))},
 !.

aXiom( Action) ==>>
 ({oper_splitk(Agent, Action, Preconds, Postconds)},
  {dbug1(using_oper_splitk(Agent, Action, Preconds, Postconds))},
 /*dmust_tracing*/
 (satisfy_each(preCond(_1), Preconds)),
 (((sg(member(failed(Why))), send_1percept(Agent, failed(Action, Why))))
    ; (satisfy_each(postCond(_2), Postconds), send_1percept(Agent, success(Action))))),
  !.

aXiom(Action, _S0, _S9)::= xnotrace(( \+ trival_act(Action), dbug1(aXiom(Action)))), xnotrace(fail).

:- defn_state_getter(eng2cmd(agent, english, action)).
aXiom( try(Agent, act3('english',Agent,[ English]))) ==>>
 eng2cmd(Agent, English, Action),
 add_intent( Agent, Action).

aXiom(intent_english(Agent, English)) ==>> !,
  {assertz(mu_global:console_tokens(Agent, English))}.


aXiom( try(Agent, act3('talk',Agent,[ Object, Message]))) ==>>  % directed message
  can_sense(Agent, audio, Object),
  from_loc(Agent, Here),
  queue_local_event([did(Agent, act3('talk',Agent,[ Here, Object, Message]))], [Here]).

aXiom(say(Agent, Message)) ==>>          % undirected message
  from_loc(Agent, Here),
  queue_local_event([did(Agent, act3('talk',Agent,[ Here, *, Message]))], [Here]).


aXiom( try(Agent, act3('emote',Agent,[ EmoteType, Object, Message]))) ==>> !, % directed message
 from_loc(Agent, Here),
 queue_local_event([ act3('emote',Agent,[ EmoteType, Object, Message])], [ Here]).




% ==============
%  WALK WEST
% ==============

aXiom( try(Agent, act3('go__dir',Agent,[ Walk, ExitName]))) ==>> !, %{fail}, % go n/s/e/w/u/d/in/out
 raise_aXiom_events( status_msg( vBegin, did(Agent, act3('go__dir',Agent,[ Walk, ExitName])))),
  % {break},
  must_mw1(from_loc(Agent, Here)),
  %must_mw1(h(spatial,exit(ExitName), Here, _There)),
  unless(Agent, h(spatial,exit(ExitName), Here, _There),
 ( eVent(Agent, event3('depart', Agent,[ in, Here, Walk, ExitName])),
 raise_aXiom_events( status_msg( vDone, did(Agent, act3('go__dir',Agent,[ Walk, ExitName])))))).

aXiom( event3('depart', Agent,[ in, Here, Walk, ExitName])) ==>> !, % {fail},
  %member(At, [*, to, at, through, thru]),
  h(spatial,exit(ExitName), Here, There),
  eVent(Agent, terminates(h(spatial, _, Agent, Here))),
 queue_local_event( event3('depart', Agent,[ Here, Walk, ExitName]), [ Here]),
   % queue_local_event( msg([cap(subj(Agent)), leaves, Here, ing(Walk), to, the, ExitName]), [Here]).
  sg(reverse_dir(ExitName, ExitNameR)),
 must_mw1( eVent(Agent, event3('arrive',Agent,[ There, Walk, ExitNameR]))).

aXiom(terminates(h(Spatial, Prep, Object, Here))) ==>> !, % {fail},
 %ignore(sg(declared(h(spatial, Prep, Object, Here)))),
 undeclare(h(Spatial, Prep, Object, Here)).

 % ==============
%  WALK ON TABLE
% ==============
aXiom( try(Agent, act3('go__prep_obj',Agent,[ Walk, At, Object]))) ==>>
  will_need_touch(Agent, Object),
  has_rel(At, Object),
  \+ is_closed(At, Object),
 eVent(Agent, event3('arrive',Agent,[ Walk, Object, At])).

aXiom( event3('arrive',Agent,[ Walk, Object, At])) ==>>
  from_loc(Object, Here),
  moveto(Agent, Walk, Agent, At, Object, [Here],
    [subj(Agent), person(Walk, es(Walk)), At, the, Object, .]),
  add_intent_look(Agent), !.

/*
aXiom( event3('arrive',Agent,[ Here, Walk, ReverseDir])) ==>> !, % {fail},
 queue_local_event( event3('arrive',Agent,[ Here, Walk, ReverseDir]), [ Here]),
  %sg(default_rel(PrepIn, Here)), {atom(PrepIn)},
  {PrepIn = in},
  % [cap(subj(Agent)), arrives, PrepIn, Here, ing(Walk), from, the, ReverseDir]
  must_mw1(eVent(Agent, initiates(h(spatial, PrepIn, Agent, Here)))),
  must_mw1(add_intent_look(Agent)).
*/

aXiom(initiates(h(Spatial, Prep, Object, Dest))) ==>> !, % {fail},
 declare(h(Spatial, Prep, Object, Dest)).


aXiom(status_msg(_Begin, _End)) ==>> [].



% ==============
%  WALK TABLE
% ==============
axiom_Recalc( try(Agent, act3('go__obj',Agent,[ Walk, Object]))) ==>>
  has_rel(At, Object),
 recalcAx( try(Agent, act3('go__prep_obj',Agent,[ Walk, At, Object]))).




% ==============
%  GOTO PANTRY
% ==============
aXiom( try(Agent, act3('go__loc',Agent,[ _Walk, There]))) ==>> % go some room
  has_rel(exit(_), There),
  eVent(Agent, make_true(Agent, h(spatial, in, Agent, There))).

aXiom(make_true(Doer, h(spatial, in, Agent, There))) ==>>
  {Doer==Agent},
  has_rel(exit(_), There),
  from_loc(Agent, Here),
  agent_thought_model(Agent, ModelData),
  {find_path(Doer, Here, There, Route, ModelData)}, !,
 eVent(Agent, follow_plan(Agent, did(Agent, act3('go__loc',Agent,[ walk, There])), Route)).

aXiom(make_true(Agent, FACT)) ==>>
  add_agent_goal(Agent, FACT).

aXiom(add_intent( Agent, TODO)) ==>>
  add_agent_intent( Agent, TODO).

aXiom(follow_plan(Agent, Name, [])) ==>> !,
  send_1percept(Agent, [success(followed_plan(Agent, Name))]).

aXiom(follow_plan(Agent, Name, [Step|Route])) ==>>
  % %% update_flp(Agent, Name, Route, Step),
  eVent(Agent, follow_step(Agent, Name, Step)),
  eVent(Agent, follow_plan(Agent, Name, Route)).

aXiom(follow_step(Agent, Name, Step)) ==>>
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

aXiom( begaN(Agent, 'put', [ Put, Thing1, At, Thing2])) ==>>
  from_loc(Agent, Here),
  % moveto(Agent, Put, Thing1, held_by, Recipient, [Here], [cap(subj(Agent)), person([give, Recipient, the], 'gives you a'), Thing, '.'],
  moveto(Agent, Put, Thing1, At, Thing2, [Here],
    [cap(subj(Agent)), person(Put, es(Put)), Thing1, At, Thing2, '.']).

aXiom( try(Agent, act3('take',Agent,[ Thing]))) ==>> !,
  % [silent(subj(Agent)), person('Taken.', [cap(Doer), 'grabs the', Thing, '.'])]
  dshow_failure(will_need_touch(Agent, Thing)),
 eVent(Agent, begaN(Agent, 'put', [ take, Thing, held_by, Agent])).

axiom_Recalc_e( try(Agent, act3('drop',Agent,[ Thing])), RECALC) ==>> !,
  will_need_touch(Agent, Thing),
  h(spatial, At, Agent, Here),
  % has_rel(At, Here),
 RECALC = ( try(Agent, act3('put',Agent,[ drop, Thing, At, Here]))).

aXiom( try(Agent, act3('put',Agent,[ Thing1, Prep, Thing2]))) ==>>
  has_rel(At, Thing2),
  prep_to_rel(Thing2, Prep, At),
  (At \= in ; \+ is_closed(At, Thing2)),
  will_need_touch(Agent, Thing2), % what if "under" an "untouchable" thing?
  % OK, put it
 raise_aXiom_events( begaN(Agent, 'put', [ put, Thing1, At, Thing2])).

aXiom( try(Agent, act3('give',Agent,[ Thing, Recipient]))) ==>>
  has_rel(held_by, Recipient),
  will_need_touch(Agent, Thing),
  will_need_touch(Recipient, Agent),
  % OK, give it
 raise_aXiom_events( begaN(Agent, 'put', [ give, Thing, held_by, Recipient])).

% do_throw ball up
axiom_Recalc( try(Agent, act3('throw_dir',Agent,[ Thing, ExitName]))) ==>>
  from_loc(Agent, Here),
 recalcAx( try(Agent, act3('throw_prep_obj',Agent,[ Thing, ExitName, Here]))).

% throw ball at catcher
axiom_Recalc( try(Agent, act3('throw_at',Agent,[ Thing, Target]))) ==>>
 recalcAx( try(Agent, act3('throw_prep_obj',Agent,[ Thing, at, Target]))).

% do_throw ball over homeplate
axiom_Recalc( try(Agent, act3('throw_prep_obj',Agent,[ Thing, Prep, Target]))) ==>>
  prep_to_rel(Target, Prep, Rel),
 recalcAx( try(Agent, act3('put',Agent,['throw', Thing, Rel, Target]))).

axiom_Recalc( try(Agent, act3('throw',Agent,[ Thing, Prep, Target]))) ==>>
 (prep_to_rel(Target, Prep, Rel);Prep=Rel),
 recalcAx( try(Agent, act3('put',Agent,['throw', Thing, Rel, Target]))).

% is throwing the ball...
aXiom( try(Agent, act3('throw',Agent,[ Thing, At, Target]))) ==>>
  will_need_touch(Agent, Thing),
  can_sense(Agent, see, Target),
 eVent(Agent, act3('throw', Agent,[ Thing, At, Target])).

% has thrown the ball...
aXiom( act3('throw', Agent,[ Thing, AtTarget, Target])) ==>>
  ignore((getprop(Thing, breaks_into(Broken)),
  dbug(general, 'object ~p is breaks_into~n', [Thing]),
  eVent(Agent, thing_transforms(Thing, Broken)))),
 eVent(Agent, disgorge(Agent, do_throw, Target, AtTarget, Target, [ Target], 'Something falls out.')).

aXiom(thing_transforms(Thing, Broken))  ==>>
  undeclare(h(spatial, At, Thing, Here)),
  declare(h(spatial, At, Broken, Here)),
  queue_local_event([transformed(Thing, Broken)], Here).


aXiom( try(Agent, act3('hit_with',Agent,[ Thing, With]))) ==>>
  from_loc(Agent, Here),
  invoke_hit(Agent, Thing, With, [Here]),
  send_1percept(Agent, [true, 'OK.']).

aXiom( try(Agent, act3('hit',Agent,[ Thing]))) ==>>
  from_loc(Agent, Here),
  invoke_hit(Agent, Thing, Agent, [Here]),
  send_1percept(Agent, [true, 'OK.']).

invoke_hit(Doer, Target, _With, Vicinity) ==>>
 ignore(( % Only brittle items use this
  getprop(Target, breaks_into(Broken)),
  dbug(general, 'target ~p is breaks_into~n', [Target]),
  undeclare(h(spatial, Prep, Target, Here)),
  queue_local_event([transformed(Target, Broken)], Vicinity),
  declare(h(spatial, Prep, Broken, Here)),
  disgorge(Doer, hit, Target, Prep, Here, Vicinity, 'Something falls out.'))).


aXiom( try(Agent, act3('dig',Agent,[ Hole, Where, Tool]))) ==>>
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

aXiom( try(Agent, act3('eat',Agent,[ Thing]))) ==>>
  (getprop(Thing, can_be(eat, t)) ->
  (undeclare(h(spatial, _1, Thing, _2)), send_1percept(Agent, [destroyed(Thing), 'Mmmm, good!'])) ;
  send_1percept(Agent, [failure(try(Agent, act3('eat',Agent,[ Thing]))), 'It''s inedible!'])).


aXiom( try(Agent, act3('switch',Agent,[ OnOff, Thing]))) ==>>
  will_need_touch(Agent, Thing),
  getprop(Thing, can_be( switch(OnOff), t)),
  getprop(Thing, effect( switch(OnOff), Term0)),
  {adv_subst(equivalent, ($(self)), Thing, Term0, Term)},
  call(Term),
  send_1percept(Agent, [success(true, 'OK')]).
 
  
axiom_Recalc( try(Agent, act3('inventory',[])), RECALC) ==>> 
  can_sense(Agent, see, Agent),
  RECALC = ( try(Agent, act3('examine',Agent,[ Agent]))).



% Agent looks
axiom_Recalc( try(Agent, act3('look',Agent,[]))) ==>>
  % Agent is At Here
  h(spatial, At, Agent, Here),
  % Agent looks At Here
  recalcAx( try(Agent, act3('examine__D3',Agent,[ see, At, Here]))).

axiom_Recalc( try(Agent, act3('examine',Agent,[ Sense]))) ==>> {is_sense(Sense)}, !, from_loc(Agent, Place), recalcAx( try(Agent, act3('examine__D3',Agent,[ see, in, Place]))).
axiom_Recalc( try(Agent, act3('examine',Agent,[ Object]))) ==>> recalcAx( try(Agent, act3('examine__D3',Agent,[ see, at, Object]))).
axiom_Recalc( try(Agent, act3('examine',Agent,[ Sense, Object]))) ==>> recalcAx( try(Agent, act3('examine__D3',Agent,[ Sense, at, Object]))), !.
axiom_Recalc( try(Agent, act3('examine',Agent,[ Sense, Prep, Object]))) ==>> recalcAx( try(Agent, act3('examine__D3', Agent, [ Sense, Prep, Object]))), !.

axiom_Recalc( try(Agent, act3('examine',Agent,[ Sense, Prep, Object, Depth]))) ==>> recalcAx( try(Agent, act3('examine__D3',Agent,[ Sense, Prep, Object, Depth]))), !.


axiom_Recalc( try(Agent, act3('examine__D3',Agent,[ Sense, Prep, Object]))) ==>> recalcAx( try(Agent, act3('examine__D5',Agent,[ Sense, Prep, Object, 3]))), !.
axiom_Recalc( try(Agent, act3('examine__D3',Agent,[ Sense, Prep, Object, Depth]))) ==>> recalcAx( try(Agent, act3('examine__D5',Agent,[ Sense, Prep, Object, Depth]))), !.


% Here does not allow Sense?
aXiom( try(Agent, act3('examine__D5',Agent,[ Sense, Prep, Object, Depth]))) ==>>
  \+ sg(can_sense_here(Agent, Sense)), !,
  raise_aXiom_events( failed( did(Agent, act3('examine',Agent,[ Sense, Prep, Object, Depth])), \+ can_sense_here(Agent, Sense))).
aXiom( try(Agent, act3('examine__D5',Agent,[ Sense, Prep, Object, Depth]))) ==>>
  \+ sg(can_sense(Agent, Sense, Object)), !,
  raise_aXiom_events( failed( did(Agent, act3('examine',Agent,[ Sense, Prep, Object, Depth])), \+ can_sense(Agent, Sense, Object))).
aXiom( try(Agent, act3('examine__D5',Agent,[ Sense, Prep, Object, Depth]))) 
   ==>> must_mw1(act_examine(Agent, Sense, Prep, Object, Depth)), !.


% used mainly to debug if things are locally accessable
aXiom( try(Agent, act3('touch',Agent,[ Thing]))) ==>> !,
 unless_reason(Agent, will_need_touch(Agent, Thing),
                      cant( reach(Agent, Thing)),
                      send_1percept(Agent, [success( act3('touch',Agent,[ Thing]), 'Ok.')])).


aXiom(change_state(Agent, Action, Thing, Prop)) ==>> !,
  change_state(Agent, Action, Thing, Prop).


aXiom(Action, S0, S9) ::=
 action_verb_agent_thing(Action, Verb, Agent, Thing),
 nonvar(Verb), nonvar(Thing), nonvar(Agent),
 act_change_state_or_fallback(Verb, State, TF), !,
 eVent(Agent, change_state(Agent, Action, Thing, State=TF), S0, S9), !.


aXiom(Action, S, E) ::=
  append_termlist(Action, [S, E], ActionSE),
  current_predicate(_, mu:ActionSE), !,
  call(Action, S, E).

aXiom(Action, S, E) ::=
  current_predicate(_, mu:Action), !,
  call(Action), S=E.

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

aXiom( switch(Open, Thing)) ==>>
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

aXiom( switch(OnOff, Thing)) ==>>
 will_need_touch(Agent, Thing),
 getprop(Thing, can_be(switch, t)),
 getprop(Thing, effect( switch(OnOff), Term0)),
 adv_subst(equivalent, $self, Thing, Term0, Term),
 call(Term),
 send_1percept(Agent, [true, 'OK']).
*/
% try

/*
aXiom( Action) ==>> fail,
  action_doer(Action, Agent),
  copy_term(Action, ActionG),
  from_loc(Agent, Here, S0),
  % queue_local_event(spatial, [attempts(Agent, Action)], [Here], S0, S1),
  act( Action), !,
 queue_local_event([ did('emote',Agent, aXiom, 
 '*'(Here), ActionG)], [ Here], S0, S9).
*/



:- endif.
