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

%:- ensure_loaded(adv_main).

% TODO: change agent storage into a term:
% mind(AgentName, AgentType, History, ModelData, Goals /*, ToDo*/)

% -------- Model updating predicates (here M stands for ModelData)

% Fundamental predicate that actually modifies the list:
update_relation( NewHow, Item, NewParent, Timestamp, M0, M2) :-
 remove_old_info( NewHow, Item, NewParent, Timestamp, M0, M1),
 memorize(h(NewHow, Item, NewParent), M1, M2).

remove_old_info( _NewHow, '<mystery>'(_, _, _), _NewParent, _Timestamp, M0, M0) :- !.
remove_old_info( _NewHow, Item, _NewParent, _Timestamp, M0, M2) :-
 forget_always(h(_OldHow, Item, _OldWhere), M0, M1),
 forget_always(h(_OldHow2, Item, _OldWhere2), M1, M2).


remove_children(_At, '<mystery>'(_, _, _), _Object, _Timestamp, M0, M0):- !.
remove_children( At, _, Object, Timestamp, M0, M2):-
  forget((h(At, _, Object)), M0, M1), !,
  remove_children( At, _, Object, Timestamp, M1, M2).
remove_children( _At, _, _Object, _Timestamp, M0, M0).

% Batch-update relations.

update_relations(Prep, '<mystery>'(How, What, Object2), Object, Timestamp, M0, M1):-
  \+ in_model((h(What, _Child, Object2)), M0),
  % \+ in_model((h(What, Object2, _Parent), _), M0),
  update_relation( Prep, '<mystery>'(How, What, Object2), Object, Timestamp, M0, M1).

update_relations(_NewHow, '<mystery>'(_, _, _), _NewParent, _Timestamp, M, M).
update_relations(_NewHow, [], _NewParent, _Timestamp, M, M).
update_relations( NewHow, [Item|Tail], NewParent, Timestamp, M0, M2) :-
 update_relation( NewHow, Item, NewParent, Timestamp, M0, M1),
 update_relations( NewHow, Tail, NewParent, Timestamp, M1, M2).


% If dynamic topology needs remembering, use
%  h(exit(E), Here, [There1|ThereTail], Timestamp)
realize_model_exit(At, From, _Timestamp, M0, M2) :-
 forget((h(exit(At), From, To)), M0, M1),
 append([(h(exit(At), From, To))], M1, M2).
realize_model_exit(At, From, _Timestamp, M0, M1) :-
 append([(h(exit(At), From, '<mystery>'(exit, At, From)))], M0, M1).

update_model_exit(At, From, To, _Timestamp, M0, M2) :-
 select_always((h(exit(At), From, _To)), M0, M1),
 append([(h(exit(At), From, To))], M1, M2).


% Model exits from Here.
update_model_exits([], _From, _T, M, M).
update_model_exits([Exit|Tail], From, Timestamp, M0, M2) :-
 realize_model_exit(Exit, From, Timestamp, M0, M1),
 update_model_exits(Tail, From, Timestamp, M1, M2).


dumpST_break:- dumpST, break.

update_model(Knower, arriving(Agent, In, Here, Walk, ExitNameReversed), Timestamp, Mem, M0, M2) :-
   \+ in_model(h(exit(ExitNameReversed), Here, _There), M0),
   realize_model_exit(ExitNameReversed, Here, Timestamp, M0, M1),
   update_model(Knower, arriving(Agent, In, Here, Walk, ExitNameReversed), Timestamp, Mem, M1, M2).

% Match only the most recent Figment in Memory.
%last_thought(Figment, Memory) :- % or member1(F, M), or memberchk(Term, List)
% copy_term(Figment, FreshFigment),
% append(RecentMemory, [Figment|_Tail], Memory),
% \+ member(FreshFigment, RecentMemory).

update_model(Knower, arriving(Agent, At, Here, _, ExitNameReversed), Timestamp, Mem, M0, M2) :-  Knower == Agent,
  % According to model, where was I?
  must_mw(in_model(h(_Was, Agent, There), M0)),
  % TODO: Handle goto(Agent, walk, on, table)
  % reverse_dir(ExitNameReversed, ExitName, advstate),
  % How did I get Here?
  append(RecentMem, [attempts(Agent, go_dir(Agent, _, ExitName))|OlderMem], Mem),
    % find figment
  \+ member(attempts(Agent, go_dir(Agent, _, _)), RecentMem), % guarrantee recentness
  memberchk(timestamp(_T1, _OldNow), OlderMem), % get associated stamp
  %player_format(Agent, '~p moved: goto(Agent, walk, ~p, ~p) from ~p leads to ~p~n',
  %       [Agent, AtGo, Dest, There, Here]),
  update_model_exit(ExitName, There, Here, Timestamp, M0, M11), % Model the path.
  update_model_exit(ExitNameReversed, Here, There, Timestamp, M11, M1),
  update_relation(At, Agent, Here, Timestamp, M1, M2), !. % And update location.

update_model(Knower, arriving(Agent, In, Here, Walk, ExitNameReversed), Timestamp, Mem, M0, M2) :-
   \+ in_model(h(In, Agent, Here), M0),
   update_relations( In, [Agent], Here, Timestamp, M0, M1),
   update_model(Knower, arriving(Agent, In, Here, Walk, ExitNameReversed), Timestamp, Mem, M1, M2).

update_model(_Agent, moved(_Doer, _How, Object, _From, At, To), Timestamp, _Mem, M0, M1) :-
  update_relation(At, Object, To, Timestamp, M0, M1).

update_model(Agent, Event, Timestamp, Memory, M0, M2) :- fail,
  implications(event , ( Event), Preconds, Postconds),
    (satisfy_each(preCond(_), Preconds, M0, _)  ->
      satisfy_each(postCond(_), Postconds, M0, M1) -> M0\=@= M1), !,
    update_model(Agent, Event, Timestamp, Memory, M1, M2).


update_model(Agent, carrying(Agent, Objects), Timestamp, _Memory, M0, M1) :-
 update_relations( held_by, Objects, Agent, Timestamp, M0, M1).
update_model(Agent, wearing(Agent, Objects), Timestamp, _Memory, M0, M1) :-
 update_relations( worn_by, Objects, Agent, Timestamp, M0, M1).


update_model(Agent, percept_props(Agent, _Sense, Object, _Depth, PropList), _Stamp, _Mem, M0, M2) :-
 apply_mapl_rest_state(updateprop_from_create(Object), PropList, [], M0, M2).

update_model(_Agent, props(Object, PropList), _Stamp, _Mem, M0, M2) :-
  apply_mapl_rest_state(updateprop(Object), PropList, [], M0, M2).

% Wrong Agent !
update_model(Agent, percept(Agent2, _, _, _Info), _Timestamp, _Mem, M0, M0):- Agent \=@= Agent2, !.
% Model exits from Here.
update_model(Agent, percept(Agent, _, _, exit_list(in, Here, ExitRelations)), Timestamp, _Mem, M0, M4) :-
  update_model_exits(ExitRelations, Here, Timestamp, M0, M4).
% Model objects seen Here.. This is no longer used right?
update_model(Agent, percept(Agent, _Sense, child_list(_Depth, There, Prep, Objects)), Timestamp, _Mem, M0, M3):- 
   dumpST_break, !,
   update_relations(Prep, Objects, There, Timestamp, M0, M3).
% Model objects seen Here ... this replaces the above code
update_model(Agent, percept(Agent, _Sense, _Depth, child_list(Object, At, Children)), Timestamp, _Mem, M0, M2) :-
 must_mw1((remove_children( At, Children, Object, Timestamp, M0, M1),
   update_relations( At, Children, Object, Timestamp, M1, M2))).
% Copy objects props Here
update_model(Agent, percept(Agent, _Sense, _Depth, props(Object, PropList)), _Stamp, _Mem, M0, M2) :-
  apply_mapl_rest_state(updateprop_from_create(Object), PropList, [], M0, M2).

update_model(_Agent, [], _Timestamp, _Memory, M, M).
update_model(Agent, [Percept|Tail], Timestamp, Memory, M0, M2) :-
 update_model(Agent, Percept, Timestamp, Memory, M0, M1),
 update_model( Agent, Tail, Timestamp, Memory, M1, M2), !.
update_model(_Agent, failure(_, _), _Timestamp, _Mem, M0, M0) :- !.
update_model(_Agent, success(_, _), _Timestamp, _Mem, M0, M0) :- !.
update_model(_Agent, failure(_), _Timestamp, _Mem, M0, M0) :- !.
update_model(_Agent, emoted(_, _, _, _), _Timestamp, _Mem, M0, M0) :- !.
update_model(_Agent, emote(_, _, _, _), _Timestamp, _Mem, M0, M0) :- !.
update_model(_Agent, msg(_), _Timestamp, _Mem, M0, M0) :- !.

update_model(Agent, time_passes(Target), Timestamp, _Memory, M, M):-
 nop(dbug1(unused_update_model(Agent, time_passes(Target), Timestamp, M))).

update_model(Agent, Percept, Timestamp, _Memory, M, M):-
 nop(dbug1(failed_update_model(Agent, Percept, Timestamp), model)).


well_remembered(none).

maybe_remember(Percept, M0, M0):- safe_functor(Percept, F, _), well_remembered(F), !.
%maybe_remember(percept_props(Whom, see, What, Depth, _List), M0, M1):- maybe_remember(percept_props(Whom, see, WhatDepth, ), M0, M1), !.
maybe_remember(Percept, M0, M1):- append([Percept], M0, M1).

each_update_model(_Agent, [], _Timestamp, _Memory, M, M).
each_update_model( Agent, [Percept|Tail], Timestamp, Memory, M0, M3) :-
 maybe_remember(Percept, M0, M1),
 map_apply_findall(update_model(Agent, Percept, Timestamp, Memory), M1, M2),
 each_update_model( Agent, Tail, Timestamp, Memory, M2, M3).


