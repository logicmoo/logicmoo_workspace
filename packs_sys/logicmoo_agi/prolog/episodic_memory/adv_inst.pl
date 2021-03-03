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
% :- ensure_loaded(adv_naming).

create_new_unlocated(Type, Inst, S0, S2):-
 inst_sep(Sep),
 create_new_suffixed_unlocated(Sep, Type, Inst, S0, S2), !.

inst_sep('_X').
current_world_adv_suffix('i1').

inst_of(x(C, N), C, N):- (nonvar(C);nonvar(N)), !.
inst_of(x(C, N), C, N):- !.

is_x_instance(Obj):- var(Obj), !, break.
is_x_instance(x(_, _)).

is_non_instance(Obj):- var(Obj), !, fail.
is_non_instance(Obj):- is_x_instance(Obj), !, fail.
is_non_instance(_):-!.

into_inst_name(_Suffix, Obj, Inst):- is_x_instance(Obj), !, Inst=Obj.
into_inst_name(Suffix, Obj, Inst):- atom(Obj), !, Inst = x(Obj, Suffix).
into_inst_name(Suffix, Obj, Inst):- ignore(arg(2, Obj, Suffix)), !, Inst=Obj.
/*

into_inst_name(Suffix, Type, Inst):- instatom_codes(Suffix, Codes), last(Codes, Code),
 code_type(Code, digit), !, atom_concat(Type, Suffix, Inst).
into_inst_name(Suffix, Type, Inst):- atom_concat(Type, Suffix, Inst0), gensym(Inst0, Inst).
*/

create_new_suffixed_unlocated(Suffix, Type, Inst, S0, S2):-
 into_inst_name(Suffix , Type, Inst),
 declare_inst_type(Inst, Type, S0, S2).

declare_inst_type(Inst, Type, S0, S2):-
  assertion(nonvar(Inst)),
  assertion(nonvar(Type)),
  object_props_or(Inst, PropList1, [], S0),
  (member(sp(adjs, _), PropList1)-> PropList1=PropList;  [sp(nouns, [Type])|PropList1]=PropList),
  list_to_set([shape=Type, traits([Type]), inherit(Type, t)|PropList], Set),
  %undeclare_alw ays(props(Inst, _), S0, S1),
  redeclare(props(Inst, Set), S0, S2).

init_objects :-
  with_mutex(get_advstate,
    init_objects_from_mutex).

init_objects_from_mutex :-
 get_advstate(S0),
 retractall(advstate_db(_)),
 must_mw1(get_objects(true, OldObjectList, S0)),
 create_missing_instances(S0, S1), !,
 set_advstate(S1),
 must_mw1(call((get_objects(true, ObjectList, S1), ObjectList\==[]))),
 dbug1(oldObjectList = OldObjectList),
 subtract(ObjectList, OldObjectList, NewObjectList),
 dbug1(newObjectList = NewObjectList),
 apply_mapl_state(mu_create_object(), ObjectList, S1, S2),
 set_advstate(S2).


create_missing_instances(S0, S2):-
 current_world_adv_suffix(Suffix),
 Info = S0,
 TODO = S0,
 create_instances(Suffix, Info, TODO, S0, S2), !.

may_contain_insts(h).
% may_contain_insts(props).
% may_contain_insts(holds_at).


create_instances(Suffix, Info, [Prop|TODO], S0, S3):-
 Prop =.. [F, Type, Pred | Objs],
 may_contain_insts(F), 
 member(Obj, Objs), is_non_instance(Obj), !,
 must_mw1((select_from(Prop, S0, S1))),
 must_mw1((create_objs(Objs, NewObjs, Suffix, Info, S1, S2),
 NewProp =.. [F, Type, Pred | NewObjs],
 create_instances(Suffix, Info, TODO, [NewProp|S2], S3))).

create_instances(Suffix, Info, [_|TODO], S0, S2):-
 create_instances(Suffix, Info, TODO, S0, S2).
create_instances(_Suffix, _Info, [], S0, S0).


create_objs([Obj|Objs], [NewObj|NewObjs], Suffix, Info, S0, S2):-
 dmsg(create_1obj(Obj, Suffix)),
 must_mw1(create_1obj(Suffix, Info, Obj, NewObj, S0, S1)),
 create_objs(Objs, NewObjs, Suffix, Info, S1, S2).
create_objs([], [], _Suffix, _Info, S0, S0).



%mu_create_object(Agent, S0, S2) :- declared(perceptq(Agent, []), S0), !,
% dbug1(existingAgent=Agent),
% S2=S0.

mu_create_object(Object, S0, S0) :- declared(props(Object, PropList), S0), member(co(_), PropList), !.
mu_create_object(Object, S0, S9) :-
 object_props_or(Object, PropList, [], S0), !,
 dbug1(mu_create_object(Object, PropList)),
 %  undeclare_al ways(props(Object, _), S0, S2),
 redeclare(props(Object, [co(PropList)]), S0, S3),
 create_objprop(creation, Object, PropList, S3, S4),
 create_objprop(instance, Object, PropList, S4, S9).
/*
visit_existing(_Object, [], S0, S0) :-!.
visit_existing(Object, [Prop|List], S0, S2):- !,
 visit_existing(Object, List, S0, S1),
 visit_existing(Object, Prop, S1, S2).

%visit_existing(Object, Prop, S1, S2):- must_mw1(create_objprop(Why, Object, Prop, S1, S2)).

visit_existing(Object, Prop, S1, S2):- Prop=inherit(_, t), !, must_mw1(create_objprop(Why, Object, Prop, S1, S2)).
visit_existing(Object, Prop, S0, S2):- must_mw1(updateprop_from_create(Object, Prop, S0, S2)).
*/

create_objprop(_Why, _Object, [], S0, S0):- !.
create_objprop(Why, Object, [Prop|List], S0, S2):- !,
 create_objprop(Why, Object, List, S0, S1),
 create_objprop(Why, Object, Prop, S1, S2).

create_objprop(Why, Object, Prop, S0, S1):- /*notrace*/((correct_props(Object, Prop, PropList), [Prop]\==PropList, !)),
  create_objprop(Why, Object, PropList, S0, S1).

 % As events happen, percepts are entered in the percept queue of each agent.
 % Each agent empties their percept queue as they see fit.
create_objprop(_Why, Object, inherit(perceptq, t), S0, S0):- declared(perceptq(Object, _), S0), !.
create_objprop(_Why, Object, inherit(perceptq, t), S0, S1):- !,
 redeclare(perceptq(Object, []), S0, S1).

 % Most agents store memories of percepts, world model, goals, etc.
create_objprop(_Why, Object, inherit(memorizer, t), S0, S0):- declared(memories(Object, _), S0), !.
create_objprop(_Why, Self, inherit(memorizer, t), S0, S2):- !, clock_time(Now),
 redeclare(memories(Self, [
  propOf(memories, Self),
 structure_label(mem(Self)),
 timestamp(0, Now),
 current_goals(Agent, []),
 goals_skipped(Agent, []),
 goals_satisfied(Agent, []),
 % model([]),
  intent(Agent, [ act3('look',Self,[])]),
 inst(Self)]), S0, S2).


create_objprop(_Why, Object, inherit(Other, f), S0, S0):- getprop(Object, isnt(Other), S0), !.
create_objprop(_Why, Object, inherit(Other, f)) -->
   updateprop_from_create(Object, isnt(Other)),
   delprop_always(Object, inherited(Other)),
   delprop_always(Object, inherit(Other, t)),
   updateprop_from_create(Object, inherit(Other, f)).

create_objprop(Why, Object, inherit(Other, t), S0, S2):- getprop(Object, inherit(Other, f), S0), !,
 updateprop_from_create(Object, inherit(Other, t), S0, S1), create_objprop(Why, Object, inherit(Other, t), S1, S2).
create_objprop(_Why, Object, inherit(Other, t), S0, S0):- getprop(Object, inherited(Other), S0), !.
create_objprop(_Why, Object, inherit(Other, t), S0, S0):- getprop(Object, isnt(Other), S0), !.
create_objprop(_Why, Object, inherit(Other, t), S0, S0):- Other==Object, !.
create_objprop(_Why, _Object, inherit(Other, t), S0, S0):- direct_props(Other, PropList, S0), member(no_copy(t), PropList), !.

create_objprop(Why, Object, inherit(Other, t), S0, S9):-
 direct_props_or(Other, PropList0, [], S0),
 adv_subst(equivalent, $class, Other, PropList0, PropList1),
 (member(sp(adjs, _), PropList1)-> PropList1=PropList;  [sp(nouns, Other)|PropList1]=PropList),
 copy_term(PropList, PropListC), !,
 % must_mw1(updateprop_from_create(Object, inherit(Other, t), S5, S9)), !,
 %must_mw1(updateprop_from_create(Object, visited(Other), S0, S1)),
 must_mw1(updateprop_from_create(Object, inherited(Other), S0, S2)),
 select_always( inherited(Other), PropListC, PropListD),
 must_mw1(create_objprop(Why, Object, PropListD, S2, S9)),
 %must_mw1(setprop_from_create(Object, inherited(Other), S3, S9)),
 !.

%create_objprop(Why, Object, inherit(Other, t), S0, S0):- getprop(Object, inherited(Other), S0), !.

create_objprop(Why, Object, Prop, S0, S2):-
 adv_subst(equivalent, $self, Object, Prop, NewProp), Prop\==NewProp, !,
 create_objprop(Why, Object, NewProp, S0, S2).

create_objprop(_Why, Object, Prop, S0, S2):-
  must_mw1(updateprop_from_create(Object, Prop, S0, S2)).


create_1obj(Suffix, _Info, the(Type), Inst, S0, S2):- !,
 must_mw1(create_new_suffixed_unlocated(Suffix, Type, Inst, S0, S2)),
 nop((pprint(S2),!,break)).

create_1obj(Suffix, _Info, a(Type), Inst, S0, S2):-  !, atom_concat(Suffix, '_A', NewSuffix),
 must_mw1(create_new_suffixed_unlocated(NewSuffix, Type, Inst, S0, S2)).
create_1obj(Suffix, _Info, s(Type), Inst, S0, S2):- trace, !, atom_concat(Suffix, '_S', NewSuffix),
 must_mw1(create_new_suffixed_unlocated(NewSuffix, Type, Inst, S0, S2)).

create_1obj(Suffix, Info, the(Type), Inst, S0, S2):- find_recent(Suffix, Type, Inst, S0, S2)->true;create_1obj(Suffix, Info, Type, Inst, S0, S2).
create_1obj(_Suffix, _Info, I, I, S0, S0):- is_x_instance(I), !.

create_1obj(Suffix, Info, Type, Inst, S0, S2):- 
  atom(Type), !, create_1obj(Suffix, Info, the(Type), Inst, S0, S2).
create_1obj(_Suffix, _Info, I, I, S0, S0):- assertion(atom(I)), !.

find_recent(_Suffix, Type, Inst, S0, S0):- declared(props(Inst, PropList), S0), declared(instance(Type), PropList).


