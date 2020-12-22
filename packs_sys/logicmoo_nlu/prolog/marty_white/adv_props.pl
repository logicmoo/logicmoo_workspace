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
delprop(Object, Prop, S0, S2) :- /*notrace*/(must_mw1((correct_props(Object, Prop, PropList), each_prop(delprop_(Object), PropList, S0, S2)))),!.
delprop_(Object, Prop, S0, S2) :-
 must_mw1(declared(props(Object, PropList), S0, S1)),
 select_from(Prop, PropList, NewPropList),
 replace_declare(props(Object, NewPropList), S1, S2).

% Remove Prop Always. @TODO @BUG may not undo side-effects 
:- defn_state_setter(delprop_always(thing, nv)).
delprop_always(Object, Prop, S0, S2) :- /*notrace*/(must_mw1((correct_props(Object, Prop, PropList), each_prop(delprop_always_(Object), PropList, S0, S2)))),!.
delprop_always_(Object, Prop, S0, S2) :-  delprop_(Object, Prop, S0, S2), !.
delprop_always_(_Object, _Prop, S0, S0).

% Replace or create Prop.
:- defn_state_setter(setprop(thing, nv)).
setprop(Object, Prop, S0, S2):- create_objprop(setprop, Object, Prop, S0, S2),!.

:- defn_state_setter(setprop_from_create(thing, nv)).
setprop_from_create(Object, Prop, S0, S2) :- 
 /*notrace*/((correct_props(Object, Prop, PropList), 
    each_prop(setprop_(Object), PropList, S0, S2))).

setprop_(Object, Prop, S0, S2) :- 
  assertion(is_list(S0)),
  \+ declared(props(Object,_), S0),
  replace_declare(props(Object,[]), S0, S1), !,
  setprop_(Object, Prop, S1, S2).
setprop_(Object, [P|PropS], S0, S2) :- !, setprop_(Object, P, S0, S1), setprop_(Object, PropS, S1, S2).
setprop_(Object, Prop, S0, S2) :-
 direct_props_or(Object, PropList, [], S0),
 %undeclare_always(props(Object, _), S0, S1),
 S0=S1,
 old_figment(Prop,F,A,Old),
 (select_from(Old, PropList, PropList2) ->
 (upmerge_prop(F, A, Old, Prop, Merged) ->
  ((Old==Merged, fail) -> S2=S0; % no update
  (append([Merged], PropList2, PropList3), replace_declare(props(Object, PropList3), S1, S2)));
      append([Prop], PropList, PropList3), replace_declare(props(Object, PropList3), S1, S2));
 (    append([Prop], PropList, PropList3), replace_declare(props(Object, PropList3), S1, S2))).


old_figment(Prop,F, A, Old):- 
 (var(A)-> safe_functor(Prop, F, A); true),
 duplicate_term(Prop, Old),
 assertion(integer(A)),
 nb_setarg(A, Old, _),!.

% Update or create Prop.
:- defn_state_setter(updateprop(thing, nv)).
updateprop(Object, Prop, S0, S2):- create_objprop(updateprop, Object, Prop, S0, S2).

:- defn_state_setter(updateprop_from_create(thing, nv)).
updateprop_from_create(Object, Prop, S0, S2) :- /*notrace*/((correct_props(Object, Prop, PropList), 
  must(each_prop(updateprop_(Object), PropList, S0, S2)))).

updateprop_(Object, Prop, S0, S2) :- 
  assertion(is_list(S0)),
  \+ declared(props(Object,_), S0),
  replace_declare(props(Object,[]), S0, S1), !,
  updateprop_(Object, Prop, S1, S2).

updateprop_(Object, Prop, S0, S2) :-
 assertion(compound(Prop)),
 direct_props_or(Object, PropList, [], S0),
 (member(Prop, PropList)
 -> S0=S2;  % no update
 (S0=S1,% undeclare_always(props(Object, _), S0, S1),
 updateprop_1(Object, Prop, PropList, S1, S2))).

updateprop_1(Object, Prop, PropList, S0, S2) :-
 old_figment(Prop,F,A,Old),
 (select_from(Old, PropList, PropList2) ->
 (upmerge_prop(F, A, Old, Prop, Merged) ->
     ((Old==Merged, fail) -> replace_declare(props(Object, PropList), S0, S2) ; % no update
       (append([Merged], PropList2, PropList3), replace_declare(props(Object, PropList3), S0, S2)));
 append([Prop], PropList, PropList3), replace_declare(props(Object, PropList3), S0, S2));
 (append([Prop], PropList, PropList3), replace_declare(props(Object, PropList3), S0, S2))).



upmerge_prop(_, _, Before, After, Result):- Before==After, !, Result=Before.
upmerge_prop(F, N, Before, After, Result):- arg(N, Before, B), arg(N, After, A), !,
 merge_value(F, N, B, A, R), duplicate_term(After, Result), nb_setarg(N, Result, R).

text_prop(nouns).
text_prop(nominals).
text_prop(adjs).
text_prop(desc).

single_valued_prop(name).
single_valued_prop(desc).
single_valued_prop(prefix).
single_valued_prop(mass).
single_valued_prop(volume).
single_valued_prop(sp).


