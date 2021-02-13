/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Jan 19, 2038 @ 03:14:07
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

must_be_same(X, Y):- must_mw1(X==Y).
model_prepend(X, Y, Z):- append(X, Y, Z).
model_remove_if(X, Y, Z):- select(X, Y, Z).
model_remove_always(X, Y, Z):- select_always(X, Y, Z).

is_not_stored_at_all(none).

maybe_store(Percept, M0, M0):- safe_functor(Percept, F, _), is_not_stored_at_all(F), !.
%maybe_store(percept_props(Whom, see, What, Depth, _List), M0, M1):- maybe_store(percept_props(Whom, see, WhatDepth, ), M0, M1), !.
maybe_store(Percept, M0, M1):- model_prepend([Percept], M0, M1).

%:- ensure_loaded(adv_main).

% TODO: change agent storage into a term:
% mind(DoerName, DoerType, History, ModelData, Goals /*, ToDo*/)

model_recent(X, List, List):-
  old_figment(X, _F, _A, XNew),
  member(XNew, List), !,
  XNew=X.


% -------- Model updating predicates (here M stands for ModelData)

% Fundamental predicate that actually modifies the list:
remove_old_parents(_Spatial, '<mystery>'(_, _, _), _Timestamp, M0, M0) :- !.
remove_old_parents(_Spatial, [], _Timestamp, M0, M0) :- !.
remove_old_parents(Spatially, [Orphan|Tail], Timestamp, M0, M2) :-
 must_mw1(make_orphan(Spatially, Orphan, Timestamp, M0, M1)),
 remove_old_parents(Spatially, Tail, Timestamp, M1, M2).
 
make_orphan(Spatially, Orphan, _Timestamp, M0, M2) :-
 model_remove_if(h(Spatially, OldPrep, Orphan, OldParent), M0, M1),
 maybe_store(h(Spatially, OldPrep, Orphan, '<mystery>'(removed, OldPrep, OldParent)), M1, M2).
make_orphan(_Spatially, _Orphan, _Timestamp, M0, M0).

remove_child(Spatially, Child, _Timestamp, M0, M2) :-
 model_remove_if(h(Spatially, _OldPrep, Child, _OldParent), M0, M2), !.
remove_child(_Spatially, _Child, _Timestamp, M0, M0).


orphan_old_children(Spatially, Prep, Parent, RetainedChildren, Timestamp, M0, M2) :-
 in_model(h(Spatially, Prep, OldChild, Parent), M0),
 \+ member(OldChild,RetainedChildren), !,
 make_orphan(Spatially, OldChild, Timestamp, M0, M1), 
 orphan_old_children(Spatially, Prep, Parent, RetainedChildren, Timestamp, M1, M2).
orphan_old_children(_Spatial, _Prep, _Parent, _RetainedChildren, _Timestamp, M0, M0).


update_children(Spatially, Prep, Parent, '<mystery>'(How, WhatPrep, Parent2), _Timestamp, M0, M1):- !,
 must_mw1(( \+ in_model(h(Spatially, Prep,  _, Parent), M0) ->
   maybe_store(h(Spatially, Prep, '<mystery>'(How, WhatPrep, Parent2), Parent), M0, M1) ; M0 = M1)).
update_children(Spatially, Prep, Parent, [], Timestamp, M0, M1):- 
  must_mw1(orphan_old_children(Spatially, Prep, Parent, [], Timestamp, M0, M1)),!.
update_children(Spatially, PrepNew, Parent, [Item|Tail], Timestamp, M0, M2) :-
  must_mw1(orphan_old_children(Spatially, PrepNew, Parent, [Item|Tail], Timestamp, M0, M1)),
  must_mw1(add_children(Spatially, PrepNew, Parent, [Item|Tail], Timestamp, M1, M2)),!.

add_children(_Spatial, _PrepNew, _NewParent, [], _Timestamp, M0, M0):- !.
add_children(Spatially, PrepNew, NewParent, [Item|Tail], Timestamp, M0, M2) :-
 dshow_call(must_mw1(add_child(Spatially, PrepNew, NewParent, Item, Timestamp, M0, M1))),
 add_children(Spatially, PrepNew, NewParent, Tail, Timestamp, M1, M2),
 must_mw1(member(h(Spatially, PrepNew, Item, NewParent), M2)),!.

add_child(Spatially, Prep, Parent, Child, Timestamp, M0, M2):- 
  remove_child(Spatially, Child,  Timestamp, M0, M1),
  maybe_store(h(Spatially, Prep, Child, Parent), M1, M2).


% If dynamic topology needs storing, use
%  h(Spatially, fn(Fn, E), Here, [There1|ThereTail], Timestamp)

realize_model_fn(Fn, Spatially, Value, From, _Timestamp, M0, M2) :-
 model_recent(h(Spatially, fn(Fn, Value), From, _To), M0, M2), !.
realize_model_fn(Fn, Spatially, Value, From, _Timestamp, M0, M1) :-
 maybe_store(h(Spatially, fn(Fn, Value), From, '<mystery>'(Fn, Value, From)), M0, M1), !.

update_model_fn(Fn, Spatially, Value, From, To, _Timestamp, M0, M2) :-
 model_recent((h(Spatially, fn(Fn, Value), From, _To)), M0, M1),
 maybe_store(h(Spatially, fn(Fn, Value), From, To), M1, M2).


% Model exits from Here.
update_model_fns(_Fn,_Spatial, [], _From, _T, M, M).
update_model_fns(Fn, Spatially, [Value|Tail], From, Timestamp, M0, M2) :-
 realize_model_fn(Fn, Spatially, Value, From, Timestamp, M0, M1),
 update_model_fns(Fn, Spatially, Tail, From, Timestamp, M1, M2).


dumpST_break:- dumpST, break.

update_model(Knower, event3('arrive',Doer,[ In, Here, Verb, ExitNameReversed]), Timestamp, Mem, M0, M2) :-
   verb_to_domain(Verb, Spatially),
   domain_to_default_fn(Spatially, Fn),
   \+ in_model(h(Spatially, fn(Fn, ExitNameReversed), Here, _There), M0),
   realize_model_fn(Fn, Spatially, ExitNameReversed, Here, Timestamp, M0, M1),
 update_model(Knower, event3('arrive',Doer,[ In, Here, Verb, ExitNameReversed]), Timestamp, Mem, M1, M2).

% Match only the most recent Figment in Memory.
%last_thought(Figment, Memory) :- % or member1(F, M), or memberchk(Term, List)
% copy_term(Figment, FreshFigment),
% model_prepend(RecentMemory, [Figment|_Tail], Memory),
% \+ member(FreshFigment, RecentMemory).

update_model(Knower, event3('arrive',Doer,[ At, Here, Verb, ExitNameReversed]), Timestamp, Mem, M0, M2) :- Knower == Doer,
  must_mw1(verb_to_domain(Verb, Spatially)),
  % According to model, where was I?
  must_mw1(in_model(h(_Was, Doer, There), M0)),
  % TODO: Handle goto(Doer, walk, on, table)
  % reverse_dir(ExitNameReversed, ExitName, advstate),
  % How did I get Here?
 append(RecentMem, [ attempts(Doer, ( act3('go__dir',Doer,[ _, ExitName])))| OlderMem], Mem),
    % find figment
 \+ member( attempts(Doer,  act3('go__dir',Doer,[ _, _])), RecentMem), % guarrantee recentness
  memberchk(timestamp(_T1, _OldNow), OlderMem), % get associated stamp
  %player_format(Doer, '~p moved: goto(Doer, walk, ~p, ~p) from ~p leads to ~p~n', 
  %       [Doer, AtGo, Dest, There, Here]),
  domain_to_default_fn(Spatially, Fn),  
  update_model_fn(Fn, Spatially, ExitName, There, Here, Timestamp, M0, M11), % Model the path.
  update_model_fn(Fn, Spatially, ExitNameReversed, Here, There, Timestamp, M11, M1),
  add_child(Spatially, At, Here, Doer, Timestamp, M1, M2), !. % And update location.

update_model(Knower, event3('arrive',Doer,[ In, Here, Verb, ExitNameReversed]), Timestamp, Mem, M0, M2) :-
   must_mw1(verb_to_domain(Verb, Spatially)),
   \+ in_model(h(Spatially, In, Doer, Here), M0),
   add_child(Spatially,  In, Here, Doer, Timestamp, M0, M1),
 update_model(Knower, event3('arrive',Doer,[ In, Here, Verb, ExitNameReversed]), Timestamp, Mem, M1, M2).

update_model(_Knower, event3('moved', _Doer, [ How, Parent, _From, At, To]), Timestamp, _Mem, M0, M1) :-
  must_mw1(verb_to_domain(How, Spatially)),
  add_child(Spatially, At, To, Parent, Timestamp, M0, M1).

/*
update_model(Knower, Event, Timestamp, Memory, M0, M2) :- fail,
  implications(event , Event, Preconds, Postconds),
    (satisfy_each(preCond(_), Preconds, M0, _)  ->
      satisfy_each(postCond(_), Postconds, M0, M1) -> M0\=@= M1), !,
    update_model(Knower, Event, Timestamp, Memory, M1, M2).
*/

update_model(_Knower, h(Spatially, Prep, Child, Parent), Timestamp, _Memory, M0, M1) :- !,
  add_child(Spatially, Prep, Parent, Child, Timestamp, M0, M1).

%update_model(_Knower, percept_props(_Doer, _Sense, Parent, _Depth, PropList), _Stamp, _Mem, M0, M2) :-
% apply_mapl_rest_state(updateprop_from_create(Parent), PropList, [], M0, M2).

update_model(_Knower, props(Parent, PropList), _Stamp, _Mem, M0, M2) :-
  apply_mapl_rest_state(updateprop(Parent), PropList, [], M0, M2).

% Wrong Doer !
update_model(Knower, percept(Doer2, _, _, _Info), _Timestamp, _Mem, M0, M0):- Knower \=@= Doer2, dmsg(Knower \=@= Doer2), !.

% Model exits from Here.
update_model(Knower, percept(Doer, _, _, fn_list(Spatially, Exit, in, Here, Functions)), Timestamp, _Mem, M0, M4) :- must_be_same(Knower, Doer),
  update_model_fns(Exit, Spatially, Functions, Here, Timestamp, M0, M4).

% Model objects seen Here ... this replaces the above code
update_model(Knower, percept(Doer, _Sense, _Depth, child_list(Spatially, Parent, At, Children)), Timestamp, _Mem, M0, M2) :- must_be_same(Knower, Doer),
   must_mw1(update_children(Spatially,  At, Parent, Children, Timestamp, M0, M2)), !.

% Copy objects props Here
update_model(Knower, percept(Doer, _, _, props(Parent, PropList)), _Stamp, _Mem, M0, M2) :- must_be_same(Knower, Doer),
  apply_mapl_rest_state(updateprop_from_create(Parent), PropList, [], M0, M2).


update_model(Knower, percept(Doer, _Sense, _Depth, []), _Stamp, _Mem, M0, M0) :-  must_be_same(Knower, Doer),!.
update_model(Knower, percept(Doer, Sense, Depth, [H|List]), Stamp, Mem, M0, M2) :-  must_be_same(Knower, Doer),
  update_model(Knower, percept(Doer, Sense, Depth, H), Stamp, Mem, M0, M1),
  update_model(Knower, percept(Doer, Sense, Depth, List), Stamp, Mem, M1, M2).

update_model(_Knower, [], _Timestamp, _Memory, M, M).
update_model( Knower, [Percept|Tail], Timestamp, Memory, M0, M2) :-
 update_model(Knower, Percept, Timestamp, Memory, M0, M1),
 update_model(Knower, Tail, Timestamp, Memory, M1, M2), !.
update_model(_Knower, failure(_, _), _Timestamp, _Mem, M0, M0) :- !.
update_model(_Knower, success(_, _), _Timestamp, _Mem, M0, M0) :- !.
update_model(_Knower, failure(_), _Timestamp, _Mem, M0, M0) :- !.
update_model(_Knower, event3('emote',_,[ _, _, _]), _Timestamp, _Mem, M0, M0) :- !.
update_model(_Knower, act3('emote',_,[ _, _, _]), _Timestamp, _Mem, M0, M0) :- !.
update_model(_Knower, msg(_), _Timestamp, _Mem, M0, M0) :- !.

update_model(Knower, time_passes(Target), Timestamp, _Memory, M, M):-
 nop(dbug1(unused_update_model(Knower, time_passes(Target), Timestamp, M))).

update_model(Knower, Percept, Timestamp, _Memory, M, M):-
 nop(dbug1(failed_update_model(Knower, Percept, Timestamp), model)).


each_update_model(_Knower, [], _Timestamp, _Memory, M, M):- !.
each_update_model( Knower, [Percept|Tail], Timestamp, Memory, M0, M3) :-
 must_mw1(maybe_store(Percept, M0, M1)),
 must_mw1(apply_to_goal(update_model(Knower, Percept, Timestamp, Memory), M1, M2)),
 must_mw1(each_update_model( Knower, Tail, Timestamp, Memory, M2, M3)).


