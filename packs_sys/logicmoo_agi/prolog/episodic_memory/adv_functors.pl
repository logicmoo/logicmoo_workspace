/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
% LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty"s Prolog Adventure Prototype
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

% Some Inform properties:
% light - rooms that have light in them
% can(eat) - can be eaten
% static - can"t be taken or moved
% scenery - assumed to be in the room description (implies static)
% concealed - obscured, not listed, not part of "all", but there
% found_in - lists places where scenery objects are seen
% absent - hides object entirely
% clothing - can be worn
% worn - is being worn
% container
% (opened = t) - container is open (must_mw be opened) to be used. there is no "closed").
% can(open) - can be opened and closed
% capacity(N) - number of objects a container or supporter can hold
% state(locked) - cannot be opened
% can(lock), with_key
% enterable
% supporter
% article - specifies indefinite article ("a", "le")
% cant_go
% daemon - called each turn, if it is enabled for this object
% description
% inside_description
% invent - code for inventory listing of that object
% list_together - way to handle "5 fish"
% plural - pluralized-name =  if different from singular
% when_closed - description when closed
% when_open - description when (opened = t)
% when_on, when_off - like when_closed, etc.
% Some TADS properties:
% thedesc
% pluraldesc
% is_indistinguishable
% is_visible(vantage)
% touchable($agent, actor)
% valid(verb) - is object seeable, touchable, etc.
% verification(verb) - is verb logical for this object
% Parser disambiguation:
% eliminate objs not visalbe, touchable, etc.
% check preconditions for acting on a candidate object


is_type_functor(Type, Logic):-
  strip_module(Logic, M, Term),
  is_m_type_functor(M, Type, Term).

is_m_type_functor(_, Type, Logic):- var(Logic), !, type_functor(Type, Logic).
is_m_type_functor(_, _, =(Name, _Value)):- var(Name), !, fail.
is_m_type_functor(_, Type, Term):-
  compound(Term),
 \+ is_list(Term),
  safe_functor(Term, F, A), !,
  is_type_functor(Type, F, A).


is_type_functor(Type, F, A):- var(A), !,
   type_functor(Type, Skel),
   safe_functor(Skel, F, A).
is_type_functor(Type, F, A):-
   safe_functor(Skel, F, A),
   type_functor(Type, Skel).



%type_functor(state, holds_at(state, time)).
%type_functor(action, get_advstate_db(list(state))).


type_functor(dest, spatially(in, inst)).
type_functor(dest, spatially(at, inst)).
type_functor(dest, spatially(on, inst)).
type_functor(dest, of(up, $here)).
type_functor(dest, of(west, $here)).


type_functor(memory, current_goals(agent, list(goals))).
type_functor(memory, goals_skipped(agent, list(goals))).
type_functor(memory, goals_satisfied(agent, list(goals))).
type_functor(memory, intent(agent, list(action))).
%type_functor(memory, model(list(state_with_stamps))).
type_functor(event, timestamp(ordinal, timept)).



type_functor(action, say(text)).  % undirected message
%type_functor(action, touchable(agent, instance)).
type_functor(state, h(domain, domrel, inst, inst)).
type_functor(state, memories(inst, list(event))).
type_functor(state, perceptq(inst, list(event))).
type_functor(state, props(inst, list(nv))).
type_functor(state, type_props(type, list(nv))).

type_functor(event, attempts(agent, action)).
type_functor(event, unused_percept_props(agent, sense, inst, depth, list(nv))).
type_functor(event, time_passes(agent)).
type_functor(event, act3('emote', agent,[ emotype, dest, statement])).

type_functor(action,X):- type_functor(maction,X).

type_functor(maction, rez(type)).
type_functor(maction, derez(inst)).
type_functor(maction, properties(inst)).
type_functor(maction, setprop(inst, nv)).

type_functor(maction, recall(agent, prop, inst2)).
type_functor(maction, print_(agent, msg)). % for debug and agent feedback
type_functor(maction, auto(agent)).
type_functor(maction, inspect(agent, getprop(inst, nv))).
type_functor(maction, attempts(agent, act3)).

type_functor(action,X):- type_functor(act3,X).

type_functor(act3, emote(agent, emotype, dest, statement)).

type_functor(act3, examine__D5(agent, sense, preprel, inst, depth)).
type_functor(act3, examine(agent, optional(sense, see), optional(prep, at), optional(inst, $(here)), optional(depth, 3))).
% type_functor(act3, look(agent, optional(prep, at), optional(inst, $(here)))).
type_functor(act3, inventory(agent)).
type_functor(act3, look(agent)).

type_functor(act3, consume(agent, optional(consume__type, eat), inst)).
type_functor(act3, eat(agent, inst)).
type_functor(act3, drink(agent, inst)).

type_functor(act3, toggle(agent, nv, inst)).
type_functor(act3, open(agent, inst)).
type_functor(act3, close(agent, inst)).
type_functor(act3, switch(agent, tfstate, inst)).

type_functor(act3, wait(agent)).

type_functor(act3, touch(agent, inst)).
type_functor(act3, hit(agent, inst, with)).
type_functor(act3, throw(agent, inst, dest)).
type_functor(act3, dig(agent, holetype, prep, dest, inst)).

type_functor(act3, drop(agent, inst)).
type_functor(act3, give(agent, inst, agnt2)).
type_functor(act3, put(agent, inst, dest)).
type_functor(act3, take(agent, inst)).

type_functor(act3,Mact):- type_functor(mact3,Mact).
type_functor(mact3,go__dir(agent, movetype, dir)).
type_functor(mact3,go__loc(agent, movetype, dest)).
type_functor(mact3,go__obj(agent, movetype, obj)).
type_functor(mact3,go__prep_obj(agent, movetype, domrel, obj)).


% Access ot planner ops
%type_functor(prolog, oper_db(agent, action, list(nv), list(nv)).
% Data representing planner midway state
%type_functor(prolog, oper_in_step(agent, action, list(nv)).

type_functor(event, move(agent, how, inst, from, prop, to)).

type_functor(event, h(spatial, held_by,agent, list(inst))).
type_functor(event, destroyed(inst)).
type_functor(event, did(action)).
type_functor(event, percept(agent, sense, depth, props)).
type_functor(event, percept(agent, fn_list(domain, fn, in, dest, list(exit)))). % paths noticable
type_functor(event, percept(agent, child_list(domain, dest, domrel, list(inst)))).
type_functor(event, failed(action, msg)). % some action has failed
type_functor(event, transformed(inst, inst2)). % inst1 got derezed and rerezed as inst2

% DATA
type_functor(nv_of_any, propOf(term, term)).

type_functor(nv, adjs(list(text))).
type_functor(nv, traits(list(text))).
type_functor(nv, nominals(list(text))).
type_functor(nv, nouns(list(text))).
type_functor(nv, sp(speech_part, list(text))).
type_functor(nv, '<mystery>'(reason, preprel, inst2)).
type_functor(nv, can_beyeah(actverb, tf)).
type_functor(nv, knows_verbs(actverb, tf)).  % can use these actions
type_functor(nv, cant_go(inst, dir, text)). % region prevents dir
type_functor(nv, class_desc(list(text))). % class description
type_functor(nv, co(list(nv))).  % item is created
type_functor(nv, desc(sv(text))).
type_functor(nv, prefix(sv(text))).
type_functor(nv, door_to(inst)).
type_functor(nv, effect(verb_targeted, script)). %
type_functor(nv, has_rel(domrel, tf)).
type_functor(nv, has_sense(sense)).
type_functor(nv, can_be(verb, tf)).
type_functor(nv, initial(sv(text))).

type_functor(nv, has_sense(sense)).
type_functor(nv, isnt(type)). % stops inheritance into some type
type_functor(nv, inherit(type, tf)).
type_functor(nv, inherited(type)).
type_functor(nv, inst(sv(term))).
type_functor(nv, oper(action, preconds, postconds)).
type_functor(nv, emitting(sense, type)).
type_functor(nv, domrel=value).
type_functor(nv, breaks_into = type).
type_functor(nv, default_rel=type).
type_functor(nv, name = (sv(text))).


type_functor(Sen,Atom):- atom(Sen),atom_concat(is_,Sen,Sense), current_predicate(Sense/1),!,call(Sense,Atom),atom(Atom).

:- multifile(user:argname_hook/4).
:- dynamic(user:argname_hook/4).

user:argname_hook(F,A,N,T):- 
  ground(v(F,A)),A>0,
  F \== (=),
  type_functor(_,P),
  functor(P,F,A),
  arg(N,P,T),
  nonvar(T).


remember_arity(T, P):- safe_functor(P, F, A), asserta(type_functor_arity(T, F, A)).
:- forall(type_functor(T, P), remember_arity(T, P)).


get_functor_types(T, Functor, Types):-
  safe_functor(Functor, F, _),
  type_functor_arity(T, F, A),
  length(Types, A),
  P=..[F|Types],
  type_functor(T, P).


current_mfa(M, F, A, P):-
  current_predicate(M:F/A),
  safe_functor(P, F, A),
  \+ predicate_property(M:P, imported_from(_)).

:- dynamic(new_type_functor/3).

scan_missing_functors:- scan_missing_functors(mu),
  % listing(new_type_functor/3),
  forall(new_type_functor(_, _, P),
        (numbervars(P), format('~N~p.~n', [type_functor(unk, P)]), nop(must_or_rtrace(dmsg(type_functor(unk, P)))))).

scan_missing_functors(M):- forall(current_mfa(M, _, _, P), scan_predicate(M, P)).

scan_predicate(M, P):- \+ predicate_property(M:P, number_of_clauses(_)), !.
scan_predicate(M, P):- forall(clause(M:P, Body), (scan_functors(P), scan_functors(Body))).

scan_functors(Body):- \+ compound(Body), !.
scan_functors(Body):- compound_name_arity(Body, _, 0), !.
scan_functors(Body):- safe_functor(Body, F, A), \+ \+ remember_functor(Body, F, A),
  Body=..[_|Args], must_maplist(scan_functors, Args), !.


remember_functor(_P, F, A):- is_type_functor(_, F, A), !.
%remember_functor( P, F, A):- retract(new_type_functor(F, A, P)), !, asserta(new_type_functor(F, A, P)), !.
remember_functor( P, F, A):- new_type_functor(F, A, PF), !, ignore(((P=PF, retract(new_type_functor(F, A, P)), !, asserta(new_type_functor(F, A, P))))).
remember_functor( P, _, _):- predicate_property(P, defined), !.
remember_functor( P, F, A):- asserta(new_type_functor(F, A, P)), !, dmsg(new_type_functor(P)).

:- export(scan_missing_functors/0).

