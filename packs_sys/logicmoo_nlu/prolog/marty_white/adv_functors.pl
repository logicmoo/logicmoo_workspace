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


is_type_functor(Type, F, A):-
   safe_functor(Skel, F, A),
   type_functor(Type, Skel).

%functor_arity_state(F, A):- is_spatial_rel(F).

%functor_arity_state(F, A):- is_spatial_rel(F).


%type_functor(state, holds_at(state, time)).
%type_functor(action, get_advstate_db(list(state))).


type_functor(dest, spatially(in, inst)).
type_functor(dest, spatially(at, inst)).
type_functor(dest, spatially(on, inst)).
type_functor(dest, of(up, $here)).
type_functor(dest, of(west, $here)).


type_functor(memory, goals(list(goals))).
type_functor(memory, goals_skipped(list(goals))).
type_functor(memory, goals_satisfied(list(goals))).
type_functor(memory, todo(list(action))).
%type_functor(memory, model(list(state_with_stamps))).
type_functor(event, timestamp(ordinal, timept)).


%type_functor(state_with_stamps, holds_at(h(domrel, inst, inst), timept)).

type_functor(state, type_props(type, list(nv))).
type_functor(state, props(inst, list(nv))).
type_functor(state, memories(inst, list(event))).
type_functor(state, perceptq(inst, list(event))).
type_functor(state, h(domrel, inst, inst)).


type_functor(action, inventory(agent)).
type_functor(action, look(agent)).
type_functor(action, examine(agent, optional(sense, see), optional(inst, here), optional(depth, 1))).
type_functor(event, percept_props(agent, sense, inst, depth, list(nv))).

type_functor(event, time_passes(agent)).
type_functor(event, attempts(agent, action)).

type_functor(action, dig(agent, holetype, prep, dest, inst)).

type_functor(action, eat(agent, inst)).
type_functor(action, hit(agent, inst, with)).

type_functor(action, rez(type)).
type_functor(action, derez(inst)).

type_functor(action, switch(agent, tfstate, tf, inst)).
type_functor(action, touch(agent, inst)).



%type_functor(action, touchable(agent, instance)).


%type_functor(action, say(Message)).  % undirected message
type_functor(action, emote(agent, emotype, dest, statement)).
type_functor(event, emoted(agent, emotype, dest, statement)).


type_functor(action, auto(agent)).

type_functor(action, wait(agent)).
type_functor(event, time_passes(agent)).


type_functor(action, recall(agent, prop, inst2)).
type_functor(action, properties(inst)).
type_functor(action, inspect(agent, getprop(inst, nv))).
type_functor(action, setprop(inst, nv)).
type_functor(action, print_(agent, msg)). % for debug and agent feedback

type_functor(action, sub__examine(agent, sense, preprel, inst, depth)).

type_functor(action, give(agent, inst, agnt2)).
type_functor(action, take(agent, inst)).
type_functor(action, drop(agent, inst)).

type_functor(action, go_dir(agent, movetype, dir)).
type_functor(action, goto_obj(agent, movetype, obj)).
type_functor(action, goto_prep_obj(agent, movetype, domrel, obj)).

type_functor(action, goto_loc(agent, movetype, dest)).

type_functor(action, throw(agent, inst, dest)).
type_functor(action, put(agent, inst, dest)).

% Access ot planner ops
%type_functor(prolog, oper_db(agent, action, list(nv), list(nv)).
% Data representing planner midway state
%type_functor(prolog, oper_in_step(agent, action, list(nv)).



type_functor(event, moved(agent, how, inst, from, prop, to)).

type_functor(event, carrying(agent, list(inst))).
type_functor(event, destroyed(inst)).
type_functor(event, did(action)).
type_functor(event, percept(agent, sense, depth, props)).
type_functor(event, percept(agent, exit_list(in, dest, list(exit)))). % paths noticable
type_functor(event, percept(agent, child_list(sense, dest, domrel, depth, list(inst)))).
type_functor(event, failed(action, msg)). % some action has failed
type_functor(event, transformed(inst, inst2)). % inst1 got derezed and rerezed as inst2

% DATA
type_functor(nv_of_any, propOf(term, term)).

type_functor(nv, adjs(list(text))).
type_functor(nv, nominals(list(text))).
type_functor(nv, nouns(list(text))).

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
type_functor(nv, breaks_into = type).
type_functor(nv, has_rel(domrel, tf)).
type_functor(nv, has_sense(sense)).
type_functor(nv, can_be(verb, tf)).
type_functor(nv, initial(sv(text))).

type_functor(nv, has_sense(sense)).
type_functor(nv, isnt(type)). % stops inheritance into some type
type_functor(nv, inherit(type, tf)).
type_functor(nv, inherited(type)).
type_functor(nv, default_rel=type).
type_functor(nv, inst(sv(term))).
type_functor(nv, name = (sv(text))).
type_functor(nv, oper(action, preconds, postconds)).
type_functor(nv, emitting(sense, type)).
% type_functor(nv, domrel=value).

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


/*
type_functor(unk, exists(A)).
type_functor(unk, the(cup)).
type_functor(unk, tell(A, B, [please, give, C, the(D)])).
type_functor(unk, k(takeable, A, B)).
type_functor(unk, put(A, B, C, D)).
type_functor(unk, moves(A, B, C, take, held_by, B, D)).
type_functor(unk, exit(A, B, C)).
type_functor(unk, departing(A, B, C, D, E)).
type_functor(unk, percept_local(A, departing(B, C, A, D, E))).
type_functor(unk, b(A, [percept_local(B, departing(C, D, B, E, F)), in(A, G)])).
type_functor(unk, arriving(A, B, C, D, E)).
type_functor(unk, exit(west)).
type_functor(unk, percept(A, {|i7||<state> the C has $VAR 1 to D |})).
type_functor(unk, prop(A, {|i7||<nv> inherits perceptq |})).
type_functor(unk, handle_events(A)).
type_functor(unk, looky(A)).
type_functor(unk, b(exit(A), B, C)).
type_functor(unk, do_nothing(A)).
type_functor(unk, reverse(on)).
type_functor(unk, A+2).
type_functor(unk, no_copy(t)).
type_functor(unk, mem(A)).
type_functor(unk, with-using:(A/B)).
type_functor(unk, sv(text)).
type_functor(unk, child_list(sense, dest, domrel, depth, list(inst))).
type_functor(unk, exit_list(in, dest, list(exit))).
type_functor(unk, optional(sense, see)).
type_functor(unk, plan([], A, B, C)).
type_functor(unk, link(A)).
type_functor(unk, alias(user_error)).
type_functor(unk, agent_conn(A, B, C, adventure_client_process(D, C, E, F, G, H))).
type_functor(unk, get_object_props(A)).
type_functor(unk, cuz(\+A)).
type_functor(unk, info(A)).
type_functor(unk, name(['Telnet:', A])).
type_functor(unk, subj(actor(A))).
type_functor(unk, parseFrame(e2l)).
type_functor(unk, console_io_player(A, B, C)).
type_functor(unk, console_io_conn_history(A, B, C, D, E, F, G)).
type_functor(unk, peer_agent(A, B)).
type_functor(unk, peer_character(A, B)).
type_functor(unk, agent_character(A, B)).
type_functor(unk, has_rel(in)).
type_functor(unk, knows_verbs(A)).
type_functor(unk, can(A, B)).
type_functor(unk, can(examine)).
type_functor(unk, nc(A)).
type_functor(unk, isa(A)).
type_functor(unk, inherit(nomicmu_plugin)).
type_functor(unk, current_subject(vSpeaker)).
type_functor(unk, disgorge(A, throw, B, C, B, [B], 'Something falls out.')).
type_functor(unk, queue_local_event([{|i7||<event> A turns into B. |}], C)).
type_functor(unk, declare({|i7||<state> the B has $VAR 0 to C |})).
type_functor(unk, undeclare({|i7||<state> the B has $VAR 0 to C |})).
type_functor(unk, breaks_into(A)).
type_functor(unk, thrown(A, B, C, D, E, F)).
type_functor(unk, recall(A, B)).
type_functor(unk, list(dest)).
type_functor(unk, path(A, B)).
type_functor(unk, path(A)).
type_functor(unk, i7_syntax(+list, +list, -list)).
type_functor(unk, adv_serve_client(A, B, C, D, E, F)).
type_functor(unk, close_on_abort(false)).
type_functor(unk, close_on_exec(false)).
type_functor(unk, failed(A)).
type_functor(unk, postCond(A)).
type_functor(unk, event(moving_in_dir(A, B, C, D, E, D, F))).
type_functor(unk, believe(A, ~{|i7||<state> the C has $VAR 1 to D |})).
type_functor(unk, updateprop_(A)).
type_functor(unk, moving_in_dir(A, B, C, D, E, D, F)).
type_functor(unk, delprop_always_(A)).
type_functor(unk, to_upper(A)).
type_functor(unk, oper(A, do_nothing(A), [], [])).
type_functor(unk, step(start, oper(A, do_nothing(A), [], []))).
type_functor(unk, add_todo(A, {|i7||<action> A does goto loc walk pantry |})).
type_functor(unk, im(A)).
type_functor(unk, log2eng(A, B, C)).
type_functor(unk, add_goal(agent, {|i7||<action> A does take crate |})).
type_functor(unk, switch(on)).
type_functor(unk, enters(A, B, C)).
type_functor(unk, to(place(A))).
type_functor(unk, via(exit(A))).
type_functor(unk, from(place(A))).
type_functor(unk, does(A)).
type_functor(unk, actor(A)).

type_functor(unk, cap(subj(actor(A)))).
type_functor(unk, s(A)).
type_functor(unk, a(rock)).
type_functor(unk, to_upper(A)).
type_functor(unk, txt(A)).

type_functor(unk, msg([cap(subj(actor(A))), does(B), from(place(C)), via(exit(D)), E, to(place(F))])).
type_functor(unk, leaves(A, B, C)).
type_functor(unk, notice(A, leaves(B, A, C))).
type_functor(unk, reverse_dir(A, B)).
type_functor(unk, timeout(planner)).
type_functor(unk, parse_cmd(A, B)).
type_functor(unk, mu_create_object(A, B)).
type_functor(unk, instance(A)).
type_functor(unk, goal(A, B\=C)).
type_functor(unk, causes(start, ~A, B)).
type_functor(unk, completeFn(A)).
type_functor(unk, before(eat, (random100=<30, die("It was poisoned!");"yuck!"))).
type_functor(unk, create_1obj(A, B, C, D)).
type_functor(unk, frame(A)).
type_functor(unk, failed_update_model(A, B, C)).
type_functor(unk, dbug1(failed_update_model(A, B, C), model)).
type_functor(unk, unused_update_model(A, {|i7||<event> Time passes for B |}, C, D)).
type_functor(unk, failure(eat(A))).
type_functor(unk, success(true, 'OK')).
type_functor(unk, failure(A, unknown_to(B, A))).
type_functor(unk, child_list(A, B, C, D)).
type_functor(unk, percept(A, B, child_list(C, D, E, F))).
type_functor(unk, updateprop(A)).
type_functor(unk, child_list(A, B, {|i7||<nv> a mystery because "D E" , is C |})).
type_functor(unk, wearing(A, B)).
type_functor(unk, preCond(A)).
type_functor(unk, listok(event)).
type_functor(unk, queue_agent_percept(agent, listok(event))).
type_functor(unk, queue_event(listok(event))).
type_functor(unk, percept_todo(list(action))).
type_functor(unk, process_percept_list(agent, list(event), tstamp)).
type_functor(unk, do_metacmd(agent, action)).
type_functor(unk, listof(inst)).
type_functor(unk, moveto(agent, verb, listof(inst), domrel, dest, list(dest), msg)).
type_functor(unk, delprop_always(thing, nv)).
type_functor(unk, updateprop(thing, nv)).
type_functor(unk, wants_quit(A, B, C)).
type_functor(unk, (fail(dmust_tracing):-throw(A))).
type_functor(unk, agent_discon(A)).
type_functor(unk, was_dcg(A, B)).
type_functor(unk, (A-->was_dcg(B, C))).
type_functor(unk, agent_thought_model(A)).
type_functor(unk, before(A)).
type_functor(unk, agent_last_action(A, B, C)).
type_functor(unk, unknown_to(A, B)).
type_functor(unk, parse2object(A)).
type_functor(unk, parse_for_kind(agent, A)).
type_functor(unk, member(failed(A))).
type_functor(unk, satisfy_each(A, B)).
type_functor(unk, success(followed_plan(A, B))).
type_functor(unk, assert_text(A, B)).
type_functor(unk, assert_text(A)).
type_functor(unk, type(A, B)).
type_functor(unk, unknown_push_to_state(A)).
type_functor(unk, parse_for_kind(state, A, B)).
type_functor(unk, bprocess_percept(A, B, C)).
type_functor(unk, (:-push_to_state(A))).
type_functor(unk, eof_action(eof_code)).
type_functor(unk, newline(detect)).
type_functor(unk, encoding(A)).
type_functor(unk, tty(true)).
type_functor(unk, show_ranges(A, B, user_output)).
type_functor(unk, online_manual_stream(A)).
type_functor(unk, pager_stream(A)).
type_functor(unk, show_help_hook(A, B)).
type_functor(unk, write_ranges_to_file(A, B)).
type_functor(unk, number_of_clauses(A)).
type_functor(unk, percept(A, B, C, D)).
type_functor(unk, cmd_help(A, B)).
type_functor(unk, console_tokens(A, B)).
type_functor(unk, plan(A, B, C)).
type_functor(unk, step(A)).
type_functor(unk, failed_aXiom(A)).
type_functor(unk, error_srv_catch(A, B)).
type_functor(unk, depth(3)).
type_functor(unk, file_name(A)).
type_functor(unk, file_no(2)).
type_functor(unk, max_errors(-1)).
type_functor(unk, case_sensitive_attributes(false)).
type_functor(unk, case_preserving_attributes(true)).
type_functor(unk, space(remove)).
type_functor(unk, system_entities(true)).
type_functor(unk, cdata(string)).
type_functor(unk, attribute_value(string)).
type_functor(unk, dialect(html5)).
type_functor(unk, v(ask, say, tell, talk)).
type_functor(unk, num(A, B, C)).
type_functor(unk, an(A)).
type_functor(unk, dotlists(true)).
type_functor(unk, cycles(true)).
type_functor(unk, variable_names(A)).
type_functor(unk, var_prefix(false)).
type_functor(unk, syntax_errors(error)).
type_functor(unk, -all).
type_functor(unk, cmdFrame(A)).
type_functor(unk, add(A)).
type_functor(unk, history(user_input, add(A))).
type_functor(unk, pretending_can_sense(A, B, C, A)).
type_functor(unk, diff(A, B)).
type_functor(unk, happens(A, B)).
type_functor(unk, happens(A, B, B)).
type_functor(unk, initiates(wake_up(A), awake(A), B)).
type_functor(unk, inits_or_rels(A, B, C)).
type_functor(unk, releases(A, B, C)).
type_functor(unk, terminates(fall_asleep(A), awake(A), B)).
type_functor(unk, terms_or_rels(A, B, C)).
type_functor(unk, >(0)).
type_functor(unk, portray_goal(mu:adv_pretty_print_goal)).
type_functor(unk, in_logic2english(A)).
type_functor(unk, spacing(next_argument)).
type_functor(unk, attributes(portray)).
type_functor(unk, max_depth(10)).
type_functor(unk, declipped(0, A, B)).
type_functor(unk, clipped(0, A, B)).
type_functor(unk, compile_eng(A)).
type_functor(unk, current_agent_tl(A)).
type_functor(unk, queue_agent_percept(A)).
type_functor(unk, fail(dmust_tracing)).
type_functor(unk, to_lower(~)).
type_functor(unk, neg(awake(A))).
type_functor(unk, dif(A)).
type_functor(unk, input_log(A)).
type_functor(unk, unused(A)).
type_functor(unk, =(0)).
type_functor(unk, e(A:B, C)).
type_functor(unk, cleanup_elements(A)).
type_functor(unk, *(A)).
type_functor(unk, structure_label(istate)).
type_functor(unk, print_("Hit brklamp")).
type_functor(unk, after(take, (initial, "You pick the mushroom, neatly cleaving its thin stalk."))).
type_functor(unk, die("It was poisoned!")).
type_functor(unk, eachOf([mushroom, fungus, toadstool])).
type_functor(unk, '$error'("required config var")).

type_functor(unk, delprop($self, emitting(see, light))).
type_functor(unk, single_event([A, B|C])).
type_functor(unk, need_redraw(A)).
type_functor(unk, v(~, post, pre)).
type_functor(unk, sg(A)).
type_functor(unk, current_state(A)).
type_functor(unk, i_o(A, B)).
type_functor(unk, person(A)).
type_functor(unk, agent(A)).
type_functor(unk, structure_type(A)).
type_functor(unk, assign_var_name(A)).
type_functor(unk, occurs(A, B)).
type_functor(unk, textString(A, B)).
type_functor(unk, done_by(A, B)).
type_functor(unk, missing(action_doer(A, B))).
type_functor(unk, check4bugs_failed(A)).
type_functor(unk, check4bugs_found_bug(A, B)).
type_functor(unk, prompt(A)).
type_functor(unk, logic2english(A)).
type_functor(unk, agent_io(A, agent_to_output(A, B))).
type_functor(unk, logic2eng(A)).
type_functor(unk, es(A)).
type_functor(unk, anglify(A, A)).
type_functor(unk, hidden(class_desc(A))).
type_functor(unk, inherited(A, f)).
type_functor(unk, tense(A, past)).
type_functor(unk, can_be(A)).
type_functor(unk, cantdothat(A)).
type_functor(unk, move(A, B)).
type_functor(unk, mustdrop(A)).
type_functor(unk, moibeus_relation(A, B)).
type_functor(unk, self_relation(A)).
type_functor(unk, mustgetout(A)).
type_functor(unk, alreadyhave(A)).
type_functor(unk, manipulate(self)).
type_functor(unk, reach(A, B)).
type_functor(unk, cant(reach(A, B))).
type_functor(unk, ly(A)).
type_functor(unk, sense(A, B, C, D)).
type_functor(unk, round((A-B*60)*10)).
type_functor(unk, (A-B*60)*10).
type_functor(unk, round(A)//60).
type_functor(unk, ago(A)).
type_functor(unk, do_props(A, [])).
type_functor(unk, notices(A, B, C)).
type_functor(unk, notices(A, B, C, D)).
type_functor(unk, extra_verbose_eng(percept(A, see, B, {|i7||<state> the C D shaped. |}))).
type_functor(unk, quoted([A, B])).
type_functor(unk, '...verbose...'(A)).
type_functor(unk, '$VAR'(A)).
type_functor(unk, loc(A, B, C, D)).
type_functor(unk, duplicated_object(A, B, C)).
type_functor(unk, open_list(A)).
type_functor(unk, locally__agent_percept__(A, B)).
type_functor(unk, new_type_functor(A)).
type_functor(unk, time_since_last_action_for(A, B, C)).
type_functor(unk, fileno(0)).
type_functor(unk, save(A)).
type_functor(unk, wants(A, undo)).
type_functor(unk, inspect(A, B, C)).
type_functor(unk, @>=(props(A, B))).
type_functor(unk, include_functor([props, h])).
type_functor(unk, console_controls_agent(A, B)).
type_functor(unk, possess(A)).
type_functor(unk, english(A)).
type_functor(unk, wishes(A, quit)).
type_functor(unk, quit(A)).
type_functor(unk, decide_action(A)).
type_functor(unk, early_decide_action(A)).
type_functor(unk, xfn(A, B)).
type_functor(unk, adv(all)).
type_functor(unk, adv_skip(all)).
type_functor(unk, module_class([user, system, library, test, development])).
type_functor(unk, simplify_dbug(A)).
type_functor(unk, ...(A)).
type_functor(unk, moveto(A, B)).
type_functor(unk, t(A, B, C)).
type_functor(unk, zexistsLeftOver(A, B)).
type_functor(unk, zexistsLeftOverText(A, B, C)).
type_functor(unk, done(A)).
type_functor(unk, reload(A)).
type_functor(unk, make(reload(A))).
type_functor(unk, make_hook(before, A)).
type_functor(unk, modified_file(A)).
type_functor(unk, fetch(A)).
type_functor(unk, forget(goals)).
type_functor(unk, msg_from(A, B)).
type_functor(unk, change_state(A, B, C, D, E)).
type_functor(unk, will_touch(A, B)).
type_functor(unk, act_examine(A, B, C, D, E)).
type_functor(unk, examine(A, B, C, D, E)).
type_functor(unk, can_sense_here(A, B)).
type_functor(unk, examine(A, see, B)).
type_functor(unk, examine(A, A)).
type_functor(unk, does_inventory(A)).
type_functor(unk, switch(A, B, C)).
type_functor(unk, eat(A)).
type_functor(unk, created(A, B)).
type_functor(unk, dig(A, B, C, D)).
type_functor(unk, hit(A, B)).
type_functor(unk, hit_with(A, B, C)).
type_functor(unk, thing_transforms(A, B)).
type_functor(unk, eVent(A, thing_transforms(B, C))).
type_functor(unk, thrown(A, B, C, D)).
type_functor(unk, throwing(A, B, C, D)).
type_functor(unk, throw_prep_obj(A, B, at, C)).
type_functor(unk, throw_at(A, B, C)).
type_functor(unk, throw_dir(A, B, C)).
type_functor(unk, does_put(A, take, B, held_by, A)).
type_functor(unk, follow_step(A, B, C)).
type_functor(unk, follow_plan(A, {|i7||<action> A does goto loc walk B |}, [])).
type_functor(unk, followed_plan(A, B)).
type_functor(unk, make_true(A, ~h(B, C, {|i7||<nv> a mystery because "D E" , is closed |}))).
type_functor(unk, arriving(A, B, C, D)).
type_functor(unk, status_msg(A, B)).
type_functor(unk, talk(A, B, *, C)).
type_functor(unk, say(A, B)).
type_functor(unk, talk(A, B, C)).
type_functor(unk, todo_english(A, B)).
type_functor(unk, do_english(A, B)).
type_functor(unk, answer(A)).
type_functor(unk, aXiom(A)).
type_functor(unk, status(A, B)).
type_functor(unk, opposite_values(A, B)).
type_functor(unk, symetrically(opposite_values(A, B))).
type_functor(unk, can_reach(A, B)).
type_functor(unk, cntrls(A, B)).
type_functor(unk, $verb).
type_functor(unk, position(A, B)).
type_functor(unk, \[does-done_by:tAnimate, some-objectActedOn, with-using:bpart]).
type_functor(unk, wantsToDo(A, B, C)).
type_functor(unk, feelsAbout(A, B, C)).
type_functor(unk, part_of(A, B)).
type_functor(unk, post(part_of(A, B))).
type_functor(unk, pre(exists(A))).
type_functor(unk, surfaceOf(A)).
type_functor(unk, or({|i7||<state> the B has $VAR 0 to C |}, {|i7||<state> the C has $VAR 0 to B |})).
type_functor(unk, pre(isa(A, tKnife), cntrls(B, A), can_reach(A, C))).
type_functor(unk, post(~cntrls(A, B), cntrls(C, B))).
type_functor(unk, pre(cntrls(A, B), ~cntrls(C, B))).
type_functor(unk, normally(isa(A, tBodyPart), cntrls(B, A), can_reach(A, C))).
type_functor(unk, v(request, tell)).
type_functor(unk, silent(A)).
type_functor(unk, person(are, is)).
type_functor(unk, postponed(holds_at(A, B))).
type_functor(unk, output(current_output)).
type_functor(unk, initially(neg(awake(A)))).
type_functor(unk, numbervars(A, B)).
type_functor(unk, f(A, 1, [B])).
type_functor(unk, lbl(A)).
type_functor(unk, relative_to(A)).
type_functor(unk, file_type(directory)).
type_functor(unk, access(read)).
type_functor(unk, file_errors(fail)).
type_functor(unk, awake(A)).
type_functor(unk, opened(cont1)).
type_functor(unk, 'TypeFn'(A)).
type_functor(unk, wake_up(A)).
type_functor(unk, fall_asleep(A)).
type_functor(unk, fg(cyan)).
type_functor(unk, imported_from(A)).
type_functor(unk, codes(A)).
type_functor(unk, open(A, B)).
type_functor(unk, make_atomic(A)).
type_functor(unk, current(name, A)).
type_functor(unk, flp_update(0, 0, [current(name, A), current(plan, B), current(step, C)], D)).
type_functor(unk, query_agent(flp, '127.0.0.1', flp_update(0, 0, [current(name, A), current(plan, B), current(step, C)], D), E)).
type_functor(unk, nomic_config_dir(A)).
type_functor(unk, delprop_(A)).
type_functor(unk, setprop_(A)).
type_functor(unk, has_setup_setup_console(A)).
type_functor(unk, library(prolog_history)).
*/
