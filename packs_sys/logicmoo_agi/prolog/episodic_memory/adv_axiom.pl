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
  h(touchable, Agent, Thing)), Dmsg), dmsg(Dmsg).

% :- trace.
will_need_touch(Agent, Thing) ==>>
  h(touchable, Agent, Thing).

:- listing(will_need_touch).

eVent(Agent, Event) ==>>
 send_1percept(Agent, Event),
 aXiom(Event).

aXiom(MAction, S0, S9):-  stripped_term(MAction, Action), !, trace, aXiom(Action, S0, S9).

aXiom(Action, _S0, _S9)::= xnotrace(( \+ trival_act(Action), dbug1(aXiom(Action)))), xnotrace(fail).


aXiom( Action) ==>>
 action_doer(Action, Agent),
 invoke_introspect(Agent, Action, Answer),
 send_1percept(Agent, [answer(Answer), Answer]), !.

aXiom(print_(Agent, Msg)) ==>>
  h(descended, Agent, Here),
  queue_local_event(msg_from(Agent, Msg), [Here]).

aXiom( intend(Agent, act3('wait',Agent,[]))) ==>>
 from_loc(Agent, Here),
 queue_local_event(time_passes(Agent), Here).

aXiom(Action) ==>>
 {implications(DoesEvent, Action, Preconds, Postconds), action_doer(Action, Agent) },
 /*dmust_tracing*/(satisfy_each(preCond(_1), Preconds)),
 (((sg(member(failed(Why))), send_1percept(Agent, failed(Action, Why))))
    ; (satisfy_each(postCond(_2), Postconds), send_1percept(Agent, (Action)))),
 {episodic_mem(Agent, implications(DoesEvent, Action, Preconds, Postconds))},
 !.

aXiom( Action) ==>>
 {oper_splitk(Agent, Action, Preconds, Postconds)},
 /*dmust_tracing*/
 (satisfy_each(preCond(_1), Preconds)),
 (((sg(member(failed(Why))), send_1percept(Agent, failed(Action, Why))))
    ; (satisfy_each(postCond(_2), Postconds), send_1percept(Agent, success(Action)))), !.


:- defn_state_getter(eng2cmd(agent, english, action)).
aXiom( intend(Agent, act3('english',Agent,[ English]))) ==>>
 eng2cmd(Agent, English, Action),
 add_intent( Agent, Action).

aXiom(intent_english(Agent, English)) ==>> !,
  {assertz(mu_global:console_tokens(Agent, English))}.


aXiom( intend(Agent, act3('talk',Agent,[ Object, Message]))) ==>>  % directed message
  can_sense(Agent, audio, Object),
  from_loc(Agent, Here),
  queue_local_event([intend(Agent, act3('talk',Agent,[ Here, Object, Message]))], [Here]).

aXiom(say(Agent, Message)) ==>>          % undirected message
  from_loc(Agent, Here),
  queue_local_event([intend(Agent, act3('talk',Agent,[ Here, *, Message]))], [Here]).


aXiom( intend(Agent, act3('emote',Agent,[ EmoteType, Object, Message]))) ==>> !, % directed message
 from_loc(Agent, Here),
 queue_local_event([ did_emote(Agent, EmoteType, Object, Message)], [ Here]).




% ==============
%  WALK WEST
% ==============

aXiom( intend(Agent, act3('go__dir',Agent,[ Walk, ExitName]))) ==>> !, %{fail}, % go n/s/e/w/u/d/in/out
 must_act( status_msg( vBegin, intend(Agent, act3('go__dir',Agent,[ Walk, ExitName])))),
  % {break},
  must_mw1(from_loc(Agent, Here)),
  %must_mw1(h(exit(ExitName), Here, _There)),
  unless(Agent, h(exit(ExitName), Here, _There),
 ( eVent(Agent, did_depart(Agent, in, Here, Walk, ExitName)),
 must_act( status_msg( vDone, intend(Agent, act3('go__dir',Agent,[ Walk, ExitName])))))).

aXiom( did_depart(Agent, in, Here, Walk, ExitName)) ==>> !, % {fail},
  %member(At, [*, to, at, through, thru]),
  h(exit(ExitName), Here, There),
  eVent(Agent, terminates(h(_, Agent, Here))),
 queue_local_event( did_depart(Agent, Here, Walk, ExitName), [ Here]),
   % queue_local_event( msg([cap(subj(Agent)), leaves, Here, ing(Walk), to, the, ExitName]), [Here]).
  sg(reverse_dir(ExitName, ExitNameR)),
 must_mw1( eVent(Agent, did_arrive(Agent, There, Walk, ExitNameR))).

aXiom(terminates(h(Prep, Object, Here))) ==>> !, % {fail},
 %ignore(sg(declared(h(Prep, Object, Here)))),
 undeclare(h(Prep, Object, Here)).

 % ==============
%  WALK ON TABLE
% ==============
aXiom( intend(Agent, act3('go__prep_obj',Agent,[ Walk, At, Object]))) ==>>
  will_need_touch(Agent, Object),
  has_rel(At, Object),
  \+ is_closed(At, Object),
 eVent(Agent, did_arrive(Agent, Walk, Object, At)).

aXiom( did_arrive(Agent, Walk, Object, At)) ==>>
  from_loc(Object, Here),
  moveto(Agent, Walk, Agent, At, Object, [Here],
    [subj(Agent), person(Walk, es(Walk)), At, the, Object, .]),
  add_intent_look(Agent), !.

/*
aXiom( did_arrive(Agent, Here, Walk, ReverseDir)) ==>> !, % {fail},
 queue_local_event( did_arrive(Agent, Here, Walk, ReverseDir), [ Here]),
  %sg(default_rel(PrepIn, Here)), {atom(PrepIn)},
  {PrepIn = in},
  % [cap(subj(Agent)), arrives, PrepIn, Here, ing(Walk), from, the, ReverseDir]
  must_mw1(eVent(Agent, initiates(h(PrepIn, Agent, Here)))),
  must_mw1(add_intent_look(Agent)).
*/

aXiom(initiates(h(Prep, Object, Dest))) ==>> !, % {fail},
 declare(h(Prep, Object, Dest)).


aXiom(status_msg(_Begin, _End)) ==>> [].


% ==============
%  WALK TABLE
% ==============
aXiom( intend(Agent, act3('go__obj',Agent,[ Walk, Object]))) ==>>
  has_rel(At, Object),
 eVent(Agent, intend(Agent, act3('go__prep_obj',Agent,[ Walk, At, Object]))).




% ==============
%  GOTO PANTRY
% ==============
aXiom( intend(Agent, act3('go__loc',Agent,[ _Walk, There]))) ==>> % go some room
  has_rel(exit(_), There),
  eVent(Agent, make_true(Agent, h(in, Agent, There))).

aXiom(make_true(Doer, h(in, Agent, There))) ==>>
  {Doer==Agent},
  has_rel(exit(_), There),
  from_loc(Agent, Here),
  agent_thought_model(Agent, ModelData),
  {find_path(Doer, Here, There, Route, ModelData)}, !,
 eVent(Agent, follow_plan(Agent, intend(Agent, act3('go__loc',Agent,[ walk, There])), Route)).

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
aXiom( begaN(Agent, 'put', [ Put, Thing1, At, Thing2])) ==>>
  from_loc(Agent, Here),
  % moveto(Agent, Put, Thing1, held_by, Recipient, [Here], [cap(subj(Agent)), person([give, Recipient, the], 'gives you a'), Thing, '.'],
  moveto(Agent, Put, Thing1, At, Thing2, [Here],
    [cap(subj(Agent)), person(Put, es(Put)), Thing1, At, Thing2, '.']).

aXiom( intend(Agent, act3('take',Agent,[ Thing]))) ==>> !,
  % [silent(subj(Agent)), person('Taken.', [cap(Doer), 'grabs the', Thing, '.'])]
  dshow_failure(will_need_touch(Agent, Thing)),
 eVent(Agent, begaN(Agent, 'put', [ take, Thing, held_by, Agent])).

aXiom( intend(Agent, act3('drop',Agent,[ Thing]))) ==>> !,
  will_need_touch(Agent, Thing),
  h(At, Agent, Here),
  % has_rel(At, Here),
 eVent(Agent, intend(Agent, act3('put',Agent,[ drop, Thing, At, Here]))).

aXiom( intend(Agent, act3('put',Agent,[ Thing1, Prep, Thing2]))) ==>>
  has_rel(At, Thing2),
  prep_to_rel(Thing2, Prep, At),
  (At \= in ; \+ is_closed(At, Thing2)),
  will_need_touch(Agent, Thing2), % what if "under" an "untouchable" thing?
  % OK, put it
 must_act( begaN(Agent, 'put', [ put, Thing1, At, Thing2])).

aXiom( intend(Agent, act3('give',Agent,[ Thing, Recipient]))) ==>>
  has_rel(held_by, Recipient),
  will_need_touch(Agent, Thing),
  will_need_touch(Recipient, Agent),
  % OK, give it
 must_act( begaN(Agent, 'put', [ give, Thing, held_by, Recipient])).



% do_throw ball up
aXiom( intend(Agent, act3('throw_dir',Agent,[ Thing, ExitName]))) ==>>
  from_loc(Agent, Here),
 eVent(Agent, intend(Agent, act3('throw_prep_obj',Agent,[ Thing, ExitName, Here]))).

% throw ball at catcher
aXiom( intend(Agent, act3('throw_at',Agent,[ Thing, Target]))) ==>>
 eVent(Agent, intend(Agent, act3('throw_prep_obj',Agent,[ Thing, at, Target]))).

% do_throw ball over homeplate
aXiom( intend(Agent, act3('throw_prep_obj',Agent,[ Thing, Prep, Target]))) ==>>
  prep_to_rel(Target, Prep, Rel),
 eVent(Agent, intend(Agent, act3('throw',Agent,[ Thing, Rel, Target]))).

% is throwing the ball...
aXiom( intend(Agent, act3('throw',Agent,[ Thing, At, Target]))) ==>>
  will_need_touch(Agent, Thing),
  can_sense(Agent, see, Target),
 eVent(Agent, did_throw(Agent, Thing, At, Target)).

% has thrown the ball...
aXiom( did_throw(Agent, Thing, AtTarget, Target)) ==>>
  ignore((getprop(Thing, breaks_into(Broken)),
  dbug(general, 'object ~p is breaks_into~n', [Thing]),
  eVent(Agent, thing_transforms(Thing, Broken)))),
 eVent(Agent, disgorge(Agent, do_throw, Target, AtTarget, Target, [ Target], 'Something falls out.')).

aXiom(thing_transforms(Thing, Broken))  ==>>
  undeclare(h(At, Thing, Here)),
  declare(h(At, Broken, Here)),
  queue_local_event([transformed(Thing, Broken)], Here).


aXiom( intend(Agent, act3('hit_with',Agent,[ Thing, With]))) ==>>
  from_loc(Agent, Here),
  intend(Agent, act3('hit',Agent,[ Thing, With, [Here]])),
  send_1percept(Agent, [true, 'OK.']).

aXiom( intend(Agent, act3('hit',Agent,[ Thing]))) ==>>
  from_loc(Agent, Here),
  invoke_hit(Agent, Thing, Agent, [Here]),
  send_1percept(Agent, [true, 'OK.']).

invoke_hit(Doer, Target, _With, Vicinity) ==>>
 ignore(( % Only brittle items use this
  getprop(Target, breaks_into(Broken)),
  dbug(general, 'target ~p is breaks_into~n', [Target]),
  undeclare(h(Prep, Target, Here)),
  queue_local_event([transformed(Target, Broken)], Vicinity),
  declare(h(Prep, Broken, Here)),
  disgorge(Doer, hit, Target, Prep, Here, Vicinity, 'Something falls out.'))).


aXiom( intend(Agent, act3('dig',Agent,[ Hole, Where, Tool]))) ==>>
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

aXiom( intend(Agent, act3('eat',Agent,[ Thing]))) ==>>
  (getprop(Thing, can_be(eat, t)) ->
  (undeclare(h(_1, Thing, _2)), send_1percept(Agent, [destroyed(Thing), 'Mmmm, good!'])) ;
  send_1percept(Agent, [failure(intend(Agent, act3('eat',Agent,[ Thing]))), 'It''s inedible!'])).


aXiom( intend(Agent, act3('switch',Agent,[ OnOff, Thing]))) ==>>
  will_need_touch(Agent, Thing),
  getprop(Thing, can_be( did('switch', OnOff), t)),
  getprop(Thing, effect( did('switch', OnOff), Term0)),
  {adv_subst(equivalent, ($(self)), Thing, Term0, Term)},
  call(Term),
  send_1percept(Agent, [success(true, 'OK')]).
 
  
aXiom( intend(Agent, act3('inventory',Agent,[]))) ==>>
  can_sense(Agent, see, Agent),
 must_act( begaN(Agent, 'inventory', [])).

aXiom( begaN( Agent, 'inventory', [])) ==>>
  eVent(Agent, intend(Agent, act3('examine',Agent,[ Agent]))).
  %findall(What, h(child, What, Agent), Inventory),
  %send_1percept(Agent, [rel_to(held_by, Inventory)]).




% Agent looks
aXiom( intend(Agent, act3('look',Agent,[]))) ==>>
  % Agent is At Here
  h(At, Agent, Here),
  % Agent looks At Here
  eVent(Agent, intend(Agent, act3('examine__D3',Agent,[ see, At, Here]))).

aXiom( intend(Agent, act3('examine',Agent,[ Sense]))) ==>> {is_sense(Sense)}, !, from_loc(Agent, Place), eVent(Agent, intend(Agent, act3('examine__D3',Agent,[ see, in, Place]))).
aXiom( intend(Agent, act3('examine',Agent,[ Object]))) ==>> eVent(Agent, intend(Agent, act3('examine__D3',Agent,[ see, at, Object]))).
aXiom( intend(Agent, act3('examine',Agent,[ Sense, Object]))) ==>> eVent(Agent, intend(Agent, act3('examine__D3',Agent,[ Sense, at, Object]))), !.
aXiom( intend(Agent, act3('examine',Agent,[ Sense, Prep, Object]))) ==>> eVent(Agent, intend(Agent, act3('examine__D3', Agent, [ Sense, Prep, Object]))), !.
aXiom( intend(Agent, act3('examine',Agent,[ Sense, Prep, Object, Depth]))) ==>> eVent(Agent, intend(Agent, act3('examine__D3',Agent,[ Sense, Prep, Object, Depth]))), !.

aXiom( intend(Agent, act3('examine__D3',Agent,[ Sense, Prep, Object]))) ==>> eVent(Agent, intend(Agent, act3('examine__D5',Agent,[ Sense, Prep, Object, 3]))), !.
aXiom( intend(Agent, act3('examine__D3',Agent,[ Sense, Prep, Object, Depth]))) ==>> eVent(Agent, intend(Agent, act3('examine__D5',Agent,[ Sense, Prep, Object, Depth]))), !.


% Here does not allow Sense?
aXiom(intend(Agent, act3('examine__D5',Agent,[ Sense, Prep, Object, Depth]))) ==>>
  \+ sg(can_sense_here(Agent, Sense)), !,
  must_act( failed( intend(Agent, act3('examine',Agent,[ Sense, Prep, Object, Depth])), \+ can_sense_here(Agent, Sense))).
aXiom(intend(Agent, act3('examine__D5',Agent,[ Sense, Prep, Object, Depth]))) ==>>
  \+ sg(can_sense(Agent, Sense, Object)), !,
  must_act( failed( intend(Agent, act3('examine',Agent,[ Sense, Prep, Object, Depth])), \+ can_sense(Agent, Sense, Object))).
aXiom(intend(Agent, act3('examine__D5',Agent,[ Sense, Prep, Object, Depth]))) 
   ==>> must_mw1(act_examine(Agent, Sense, Prep, Object, Depth)), !.


% used mainly to debug if things are locally accessable
aXiom( intend(Agent, act3('touch',Agent,[ Thing]))) ==>> !,
 unless_reason(Agent, will_need_touch(Agent, Thing),
                      cant( reach(Agent, Thing)),
                      send_1percept(Agent, [success( intend(Agent, act3('touch',Agent,[ Thing])), 'Ok.')])).


aXiom(change_state(Agent, Action, Thing, Prop)) ==>> !,
  change_state(Agent, Action, Thing, Prop).


aXiom(Action, S0, S9) ::=
 action_verb_agent_thing(Action, Open, Agent, Thing),
 nonvar(Open), nonvar(Thing), nonvar(Agent),
 act_change_state_or_fallback(Open, Opened, TF), !,
 eVent(Agent, change_state(Agent, Action, Thing, Opened=TF), S0, S9), !.


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

aXiom( did('switch', Open, Thing)) ==>>
 act_prevented_by(Open, TF),
 will_need_touch(Agent, Thing),
 %getprop(Thing, can_be(open),
 %\+ getprop(Thing, =(open, t)),
 Open = open, traverses(Sense, Open)
 %delprop(Thing, =(Open, f)),
 %setprop(Thing, =(open, t)),
 setprop(Thing, =(Open, TF)),
 h(Sense, Agent, Here),
 queue_local_event([setprop(Thing, =(Open, TF)), [Open, is, TF]], [Here, Thing]).

aXiom( did('switch', OnOff, Thing)) ==>>
 will_need_touch(Agent, Thing),
 getprop(Thing, can_be(switch, t)),
 getprop(Thing, effect( did('switch', OnOff), Term0)),
 adv_subst(equivalent, $self, Thing, Term0, Term),
 call(Term),
 send_1percept(Agent, [true, 'OK']).
*/
% intent

/*
aXiom( Action) ==>> fail,
  action_doer(Action, Agent),
  copy_term(Action, ActionG),
  from_loc(Agent, Here, S0),
  % queue_local_event(spatial, [attempts(Agent, Action)], [Here], S0, S1),
  act( Action), !,
 queue_local_event([ did_emote(Agent, aXiom, '*'(Here), ActionG)], [ Here], S0, S9).
*/



:- endif.
