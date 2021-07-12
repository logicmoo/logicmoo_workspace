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



% get_all_props(Object, AllProps, S0):- findall(Prop, getprop(Object, Prop, S0), AllProps).
:- defn_state_getter(getprop(thing, nv)).
getprop(Object, Prop, S0) :- quietly((correct_prop(Prop, PropList), getprop0(Object, PropList, S0))).

getprop0(Object, Prop, S0):-
  ((as_first_arg(Object, Prop, Element), declared(Element, S0))
     *-> true ; getprop1(Object, [], Object, Prop, S0)).

getprop1(Orig, AlreadyUsed, Object, Prop, S0) :-
 direct_props(Object, PropList, S0),
 ( declared(Prop, PropList)*-> true ;
 inherited_prop1(Orig, AlreadyUsed, Object, Prop, PropList, S0)).

inherited_prop1(Orig, AlreadyUsed, _Object, Prop, PropList, S0):-
 member(inherit(Delegate, t), PropList),
 \+ member(inherit(Delegate, t), AlreadyUsed),
 \+ member(inherit(Delegate, f), PropList),
 \+ member(inherited(Delegate), AlreadyUsed),
 append(AlreadyUsed, PropList, AllPropList),
 \+ member(isnt(Delegate), AllPropList),
 getprop1(Orig, AllPropList, Delegate, Prop, S0).

inherited_prop1(_Orig, AlreadyUsed, _Object, Prop, PropList, _S0):-
 member(link(Delegate), PropList),
 \+ member(link(Delegate), AlreadyUsed),
 nb_current(Delegate, NewProps),
 member(Prop, NewProps).


direct_props(Object, PropList, State):-
 (var(State)->get_advstate(State); true),
 direct_props2(Object, PropList, State).

direct_props2(Object, PropList, State):-
 (declared(props(Object, PropList), State)
 *-> true
 ; ( declared(type_props(Object, PropList), State)
 *-> true
  ; extra_decl(Object, PropList))).

direct_props_or(Object, PropList, Default, S0) :-
 direct_props(Object, PropList, S0)*->true; PropList=Default.

object_props_or(Object, PropList, Default, S0) :-
 declared(props(Object, PropList), S0)*->true; PropList=Default.

 :- meta_predicate each_prop(3, ?, ?, ?).
each_prop(_, [], S0, S0) :-!.
each_prop(Pred, [Prop|List], S0, S2) :- !,
  each_prop(Pred, Prop, S0, S1),
  each_prop(Pred, List, S1, S2).
each_prop(Pred, Prop, S0, S1):- assertion(compound(Prop)), call(Pred, Prop, S0, S1), !.


% Remove Prop.  @TODO @BUG may not undo side-effects
:- defn_state_setter(delprop(thing, nv)).
delprop(Object, Prop, S0, S2) :- /*notrace*/(must_mw1((correct_props(Object, Prop, PropList), each_prop(delprop_(Object), PropList, S0, S2)))), !.
delprop_(Object, Prop, S0, S2) :-
 must_mw1(declared(props(Object, PropList), S0, S1)),
 select_from(Prop, PropList, NewPropList),
 redeclare(props(Object, NewPropList), S1, S2).

% Remove Prop Always. @TODO @BUG may not undo side-effects
:- defn_state_setter(delprop_always(thing, nv)).
delprop_always(Object, Prop, S0, S2) :- /*notrace*/(must_mw1((correct_props(Object, Prop, PropList), each_prop(delprop_always_(Object), PropList, S0, S2)))), !.
delprop_always_(Object, Prop, S0, S2) :-  delprop_(Object, Prop, S0, S2), !.
delprop_always_(_Object, _Prop, S0, S0).

% Replace or create Prop.
:- defn_state_setter(setprop(thing, nv)).
setprop(Object, Prop, S0, S2):- create_objprop(setprop, Object, Prop, S0, S2), !.

:- defn_state_setter(setprop_from_create(thing, nv)).
setprop_from_create(Object, Prop, S0, S2) :-
 /*notrace*/((correct_props(Object, Prop, PropList),
    each_prop(setprop_(Object), PropList, S0, S2))).

setprop_(Object, Prop, S0, S2) :-
  assertion(is_list(S0)),
  \+ declared(props(Object, _), S0),
  redeclare(props(Object, []), S0, S1), !,
  setprop_(Object, Prop, S1, S2).
setprop_(Object, [P|PropS], S0, S2) :- !, setprop_(Object, P, S0, S1), setprop_(Object, PropS, S1, S2).
setprop_(Object, Prop, S0, S2) :-
 direct_props_or(Object, PropList, [], S0),
 %undeclare_always(props(Object, _), S0, S1),
 S0=S1,
 old_figment(Prop, F, A, Old),
 (select_from(Old, PropList, PropList2) ->
 (upmerge_prop(F, A, Old, Prop, Merged) ->
  ((Old==Merged, fail) -> S2=S0; % no update
  (append([Merged], PropList2, PropList3), redeclare(props(Object, PropList3), S1, S2)));
      append([Prop], PropList, PropList3), redeclare(props(Object, PropList3), S1, S2));
 (    append([Prop], PropList, PropList3), redeclare(props(Object, PropList3), S1, S2))).


old_figment(h(Spatially, fn(Exit, D),X,_), h, 4, h(Spatially, fn(Exit, D),X,_)):- nonvar(X),!.
old_figment(h(Spatially,_,X,_), h, 4, h(Spatially,_,X,_)):- nonvar(X),!.
old_figment(Prop, F, A, Old):-
 (var(A)-> safe_functor(Prop, F, A); true),
 duplicate_term(Prop, Old),
 assertion(integer(A)),
 nb_setarg(A, Old, _), !.

% Update or create Prop.
:- defn_state_setter(updateprop(thing, nv)).
updateprop(Object, Prop, S0, S2):- create_objprop(updateprop, Object, Prop, S0, S2).

:- defn_state_setter(updateprop_from_create(thing, nv)).
updateprop_from_create(Object, Prop, S0, S2) :- /*notrace*/((correct_props(Object, Prop, PropList),
  must(each_prop(updateprop_(Object), PropList, S0, S2)))).

updateprop_(Object, Prop, S0, S2) :-
  assertion(is_list(S0)),
  \+ declared(props(Object, _), S0),
  redeclare(props(Object, []), S0, S1), !,
  updateprop_(Object, Prop, S1, S2).

updateprop_(Object, Prop, S0, S2) :-
 assertion(compound(Prop)),
 direct_props_or(Object, PropList, [], S0),
 (member(Prop, PropList)
 -> S0=S2;  % no update
 (S0=S1, % undeclare_always(props(Object, _), S0, S1),
 updateprop_1(Object, Prop, PropList, S1, S2))).

updateprop_1(Object, Prop, PropList, S0, S2) :-
 old_figment(Prop, F, A, Old),
 (select_from(Old, PropList, PropList2) ->
 (upmerge_prop(F, A, Old, Prop, Merged) ->
     ((Old==Merged, fail) -> redeclare(props(Object, PropList), S0, S2) ; % no update
       (append([Merged], PropList2, PropList3), redeclare(props(Object, PropList3), S0, S2)));
           append([Prop], PropList, PropList3), redeclare(props(Object, PropList3), S0, S2));
          (append([Prop], PropList, PropList3), redeclare(props(Object, PropList3), S0, S2))).



upmerge_prop(_, _, Before, After, Result):- Before==After, !, Result=Before.
upmerge_prop(F, N, Before, After, Result):- arg(N, Before, B), arg(N, After, A), !,
 merge_value(F, N, B, A, R), duplicate_term(After, Result), nb_setarg(N, Result, R).

collector_prop(nouns).
collector_prop(traits).
collector_prop(nominals).
collector_prop(adjs).
collector_prop(desc).
collector_prop(color).

single_valued_prop(name).
single_valued_prop(desc).
single_valued_prop(prefix).
single_valued_prop(sp).

single_valued_prop(mass).
single_valued_prop(volume).

is_spatial_rel(in).
is_spatial_rel(worn_by).
is_spatial_rel(held_by).
is_spatial_rel(on).
is_spatial_rel(R):- is_spatial_rel_fn(R).
is_spatial_rel_fn(exit).
is_spatial_rel_fn(attached).





filter_spec(true, _):- !.
filter_spec( \+ Spec, PropList):- !,
 \+ filter_spec(Spec, PropList).
filter_spec((Spec1;Spec2), PropList):- !, (filter_spec(Spec1, PropList);filter_spec(Spec2, PropList)).
filter_spec((Spec1, Spec2), PropList):- !, filter_spec(Spec1, PropList), filter_spec(Spec2, PropList).
filter_spec( Spec, PropList):- declared(Spec, PropList).


% extra_decl(Object, PropList):- get_advstate(State), direct_props(Object, PropList, State).

% Entire state of simulation & agents is held in one list, so it can be easy
% to roll back. The state of the simulation consists of:
% object properties
% object relations
% percept queues for agents
% memories for agents (actually logically distinct from the simulation)
% Note that the simulation does not maintain any history.
% TODO: change state into a term:
% ss(Objects, Relationships, PerceptQueues, AgentMinds)
% TODO:
% store initial state as clauses which are collected up and put into a list,
% like the operators are, to provide proper prolog variable management.

get_object_props(Agent, Mem):-
   get_advstate(State),
   declared(props(Agent, Mem), State).

:- defn_state_getter(get_object_props(agent, model)).
get_object_props(Obj, ObjectProps, M0):- var(M0), get_advstate(M0), !, get_object_props(Obj, ObjectProps, M0).
get_object_props(Obj, ObjectProps, M0):- is_list(M0), get_object_props_list(Obj, ObjectProps, M0), !.
get_object_props(Obj, ObjectProps, M0):- declared_link(get_object_props(Obj), ObjectProps, M0).

get_object_props_list(Obj, ObjectProps, M0):- memberchk(propOf(_, Obj), M0), ObjectProps = M0, !.
get_object_props_list(Obj, ObjectProps, M0):- member(props(Obj, ObjectProps), M0), !.
% get_object_props_list(Obj, ObjectProps, M0):- member(type_props(Obj, ObjectProps), M0), !.


get_objects(Spec, Set, State):-
 quietly((must_input_state(State),
  get_objects_(Spec, List, State, im(State)), !,
  list_to_set(List, Set))).
%get_objects(_Spec, [_Player_1, floyd_X1], _State):-!.

get_objects_(_Spec, [], [], im(_)) :- !.
get_objects_(Spec, OutList, [Store|StateList], im(S0)):-
 (( stores_props(Store, Object, PropList) -> filter_spec(Spec, PropList))
 -> OutList = [Object|MidList]
 ; OutList = MidList), !,
 get_objects_(Spec, MidList, StateList, im(S0)).

stores_props(perceptq(Agent, PropList), Agent, PropList).
%stores_props(type_props(Agent, PropList), Agent, PropList).
stores_props(memories(Agent, PropList), Agent, PropList).
stores_props(props(Object, PropList), Object, PropList).



push_to_state(Info):- push_2_state(Info), !.
push_to_state(Info):- must_or_rtrace(push_2_state(Info)).


%push_2_state(State):- push_to_obj(world, State).
push_2_state(StateInfo):- end_of_list == StateInfo, !.
%push_2_state(sp(Adjs, TypeS)):- is_list(TypeS), must_maplist([E]>>push_2_state(sp(Adjs, E)), TypeS).
%push_2_state(sp(Adjs, Atom)):- push_2_state(type_props(Atom, [inherit(Adjs)])), push_2_state(inherit(Atom)).
push_2_state(StateInfo):- is_codelist(StateInfo), any_to_string(StateInfo, SStateInfo), !, push_2_state(SStateInfo).
push_2_state(StateInfo):- is_charlist(StateInfo), any_to_string(StateInfo, SStateInfo), !, push_2_state(SStateInfo).
push_2_state(StateInfo):- string(StateInfo), parse_kind(state, StateInfo, Logic), push_2_state(Logic).
push_2_state(StateInfo):- is_list(StateInfo), !, must_maplist(push_2_state, StateInfo).
push_2_state(StateInfo):- \+ compound(StateInfo), trace_or_throw(unknown_push_to_state(StateInfo)), !.
push_2_state(type(Type, Conj)):-  !, push_2_state(props(type(Type), Conj)).
push_2_state(props(type(Type), Conj)):- !, props_to_list(Conj, List), push_2_state(type_props(Type, List)).
push_2_state(props(Obj, Conj)):-  props_to_list(Conj, List) -> Conj\== List, !, push_2_state(props(Obj, List)).
push_2_state(type_props(Obj, Conj0)):-
  (adv_subst(equivalent, $class, Obj, Conj0, Conj)-> Conj0\==Conj), !, push_2_state(type_props(Obj, Conj)).
push_2_state(type_props(Obj, Conj)):-
  (props_to_list(Conj, List) -> Conj\== List), !, push_2_state(type_props(Obj, List)).
push_2_state(call(StateInfo)):- !, call(StateInfo),!.
push_2_state(THOLDS):- compound_name_arguments(THOLDS,'t',[T|HOLDS]),append_termlist(T,HOLDS,StateInfo),!,push_2_state(StateInfo).
push_2_state(StateInfo):- StateInfo=..[F, Obj, E1, E2|More], functor_arity_state(F, 2), !,
  StateInfoNew=..[F, Obj, [E1, E2|More]], !, push_2_state(StateInfoNew).
push_2_state(StateInfo):- props_to_list(StateInfo, StateInfo2)->StateInfo2\=[StateInfo], !, push_2_state(StateInfo2).

push_2_state(assert_text(Text)):- trace, must(eng2log(istate, Text, Translation, [])), push_2_state(Translation).
push_2_state(assert_text(Where, Text)):-  trace, !, must(eng2log(Where, Text, Translation, [])), push_2_state(Translation).

push_2_state(StateInfo):- is_state_info(StateInfo), !, get_state_context(Ctx), must_or_rtrace(declare(StateInfo, Ctx, _)), 
  must_or_rtrace(update_running(StateInfo)).
push_2_state(StateInfo):- wdmsg(warn(push_2_state(StateInfo))), trace, forall(arg(_, StateInfo, Sub), push_2_state(Sub)).

correct_props(_Obj, PropsIn, PropsOut):- props_to_list(PropsIn, PropsOut), !.

check_atom(Atom):- assertion(atom(Atom)).

props_to_list(Nil, []):- assertion(\+ var(Nil)), Nil==[], !.
props_to_list(end_of_list, []):- !.
props_to_list(Before, AfterL):- (correct_prop(Before, After) -> Before\==After, listify(After, AfterL)), !.
props_to_list(NC, [nc(NC)]):- \+ compound(NC), !.
props_to_list(oper(_, _, _), []):- !.
props_to_list([A|B], ABL):- !,
   props_to_list(A, AL),
   props_to_list(B, BL),
   append(AL, BL, ABL).
props_to_list((A, B), ABL):- !,
   props_to_list(A, AL),
   props_to_list(B, BL),
   append(AL, BL, ABL).
props_to_list(Other, [Other]).


make_class_desc_sp(adjs, Atom, Desc):- string_concat("normally ", Atom, Desc).
make_class_desc_sp(nouns, Atom, Desc):- string_concat("refered to as a ", Atom, Desc).
make_class_desc_sp(traits, Atom, Desc):- string_concat("related to a ", Atom, Desc).

pos_to_sp(adjs).
pos_to_sp(nouns).
%pos_to_sp(traits).

negated_boolean(Last, _NegLast):- \+ atomic(Last), !, fail.
%negated_boolean(nil, t).
negated_boolean(Yes, No):- true_2_false(Yes, No), !.
negated_boolean(No, Yes):- true_2_false(Yes, No), !.

true_2_false(t, f).
true_2_false(1, 0).
true_2_false(true, false).
true_2_false(y, n).
true_2_false(yes, no).

negate_prop(UnNegated, Negated):- \+ compound_gt(UnNegated, 0), !, (negated_boolean(UnNegated, Negated)->true;UnNegated=Negated).
negate_prop(UnNegated, Negated):-
  functor(UnNegated, F, A), arg(A, UnNegated, Last),
  negated_boolean(Last, NegLast), !,
  UnNegated=..[F|Args],
  append(Left, [_], Args),
  append(Left, [NegLast], NewArgs),
  Negated=..[F|NewArgs], !.

:- op(700, fx, ('~')).

correct_prop(NC, NO):- var(NC), !, NC = NO.
correct_prop(NC, nc(NC)):- var(NC), throw(correct_prop(NC, nc(NC))), !.
correct_prop(        (Type), inherit(Type, t)):- atom(Type).
correct_prop(~inherit(Type), inherit(Type, f)):- atom(Type), !.
correct_prop( inherit(Type), inherit(Type, t)):- check_atom(Type), !.
correct_prop(     isa(Type), inherit(Type, t)):- check_atom(Type), !.
correct_prop(    isnt(Type), inherit(Type, f)):- check_atom(Type), !.
correct_prop(NC, nc(NC)):- \+ compound(NC), !.
correct_prop(       ~(Type), Negated):- correct_prop( Type, UnNegated), negate_prop(UnNegated, Negated), !.
correct_prop(       ~(Type), inherit(Type, f)):- atom(Type), !.
correct_prop(AdjsInfo, sp(Adjs, Info)):- pos_to_sp(Adjs), compound_name_arguments(AdjsInfo, Adjs, [Info]).

correct_prop(sp(Adjs, TypeS), Out):- is_list(TypeS), must_maplist(correct_some(Adjs), TypeS, Out).
correct_prop(sp(Adjs, Atom), Out):-  check_atom(Atom), 
  nop(push_to_state(type_props(Atom, [traits(Atom), sp=Adjs]))), !,
  % make_class_desc_sp(Adjs, Atom, ClassDesc), push_to_state(type_props(Atom, [class_desc([ClassDesc])])),
  must(correct_prop(inherit(Atom), Out)).

correct_prop(h(F, X, Y), h(Spatially, F, X, Y)):- must(pred_to_domain(F, Spatially)), !.
correct_prop(HPRED, h(Spatially, FS, X, Y)):- HPRED=..[F, S, X, Y], pred_to_domain(F, Spatially), !, FS=fn(F, S).
correct_prop(HPRED, h(Spatially, F, X, Y)):- HPRED=..[F, X, Y], pred_to_domain(F, Spatially), !.
correct_prop(          SV, N=V):- SV=..[N, V], single_valued_prop(N), !.

correct_prop( (can(Verb)), can_be(Verb, t)):- nop(check_atom(Verb)).
correct_prop(~(can(Verb)), can_be(Verb, f)):- nop(check_atom(Verb)).
correct_prop( (can(Verb, TF)), can_be(Verb, TF)):- nop(check_atom(Verb)).
correct_prop( (knows_verbs(Verb)), knows_verbs(Verb, t)):- nop(check_atom(Verb)).
correct_prop(~(knows_verbs(Verb)), knows_verbs(Verb, f)):- nop(check_atom(Verb)).
correct_prop( (has_rel(Verb)), has_rel(Verb, t)):- nop(check_atom(Verb)).
correct_prop(~(has_rel(Verb)), has_rel(Verb, f)):- nop(check_atom(Verb)).
correct_prop(  Other, Other).

correct_some(Adjs, E, O):- check_atom(Adjs), must(correct_prop(sp(Adjs, E), O)).


merge_value(F, N, B, A, RO):- collector_prop(F), \+ is_list(B), !, merge_value(F, N, [B], A, RO).
merge_value(F, N, B, A, RO):- collector_prop(F), \+ is_list(A), !, merge_value(F, N, B, [A], RO).
merge_value(F, _, _, A, R):- single_valued_prop(F), !, A=R.

merge_value(=, 2, _, V, R):- !, R = V.

merge_value(_, _, _, t, R):- !, R = t.
merge_value(_, _, _, f, R):- !, R = f.
merge_value(_, _, _, [], R):- !, R = [].
merge_value(_, _, _, A, R):- number(A), !, A=R.

merge_value(_F, 1, B, A, R):- B == A, !, R = A.

merge_value(_F, 1, B, A, RO):- (is_list(B);is_list(A)), flatten([A, B], R), !, list_to_set(R, RO).

merge_value(_, 1, _, A, R):- number(A), !, A=R.
merge_value(_, 1, _, _, _):- !, fail.
merge_value(_F, _, _B, A, R):- R = A.

:- fixup_exports.
