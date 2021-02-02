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

/*
nearby_objs(Agent, Here, Nearby, S0):-
 ignore(g_h(Spatially, At, Agent, Here, S0)),
 findall_set(What,
   (g_h(Spatially, At, What, Here, S0),
    sub_objs(descended, Here, What, S0)),
   Nearby).
*/

sub_objs(At, Here, What, S0):-
  g_h(Spatially, At, What, Here, S0),
 \+ ((g_h(Spatially, inside, What, Container, S0),
   Container\==Here, g_h(Spatially, descended, Container, Here, S0))).

add_exit_percepts(Spatially, Sense, Agent, PrepFrom, Depth, Object, S2, S3):-
   sense_object_exitnames(Spatially, Sense, Depth, PrepFrom, Object, SensedExits, S2),
   % sense_object_exitnames(Spatially, Sense, Depth, PrepFrom, Object, SensedExits, S2),
   maybe_send_sense((SensedExits\==[]), Agent, Sense, Depth, SensedExits, S2, S3).

sense_object_exitnames(_Spatial, _Sense, _Depth, child, _Object, [], _S0) :- !.
sense_object_exitnames(Spatially, _Sense, _Depth, in, Object, Exits, S0) :-
  Info = h(Spatially,exit(Dir), Object,'<mystery>'(exit,Dir,Object)),  
  findall(Info, g_h(Spatially,exit(Dir), Object, _, S0), Exits), Exits\==[], !.
sense_object_exitnames(Spatially, _Sense, _Depth, In, Object, [Info], _S0) :-
  Dir = escape(In), !,
  Info = h(Spatially,exit(Dir),Object,'<mystery>'(exit,Dir,Object)).
 

/*
prep_object_exitnames(_Sense, _Depth, in, Object, Exits, S0) :-
  findall(h(Spatially,exit(Direction), Object, _), g_h(Spatially,exit(Direction), Object, _, S0), Exits), Exits\==[], !.
prep_object_exitnames(_Sense, _Depth, InOnUnderAt, Object, [h(Spatially,exit(escape), Object, _)], _S0) :- member(InOnUnderAt, [in, on, under, at]), !.
prep_object_exitnames(_Sense, _Depth, Direction, _Object, [h(Spatially,exit(reverse(Direction)), Object, _)], _S0) :- !.
%prep_object_exitnames(_Sense, _Depth, Other, _Object, [reverse(Other)], _S0).
*/

is_prop_accessable(_, N, _):- N == 5, !.
is_prop_accessable(_, N, _):- N == 4, admin, !.
is_prop_accessable(Sense, N, Prop):- is_prop_accessable_at(Sense, NL, Prop), !, N >= NL.
% is_prop_accessable(_, 1, _):- !.

is_prop_accessable_at(_, _, P):- \+ callable(P), !, fail.

% stared at
is_prop_accessable_at(see, 3, desc).
is_prop_accessable_at(see, 3, volume_capacity).
is_prop_accessable_at(see, 3, volume).
% groped
is_prop_accessable_at(touch, 3, locked).

% looked
is_prop_accessable_at(see, 2, shiny).
is_prop_accessable_at(see, 2, opened).
is_prop_accessable_at(see, 2, worn_on).
is_prop_accessable_at(_, 2, has_rel).
is_prop_accessable_at(see, 2, emitting).
% felt
is_prop_accessable_at(touch, 2, shape).
is_prop_accessable_at(touch, 2, volume).

% glanced
is_prop_accessable_at(see, 1, in). % has_rel
is_prop_accessable_at(see, 1, on). % has_rel
is_prop_accessable_at(see, 1, shape).
% bumped
is_prop_accessable_at(touch, 1, texture).

% parsing
is_prop_accessable_at(know, 1, name).
is_prop_accessable_at(know, 1, adjs).
is_prop_accessable_at(know, 1, nouns).
is_prop_accessable_at(know, 1, traits).
is_prop_accessable_at(know, 1, default_rel).

% dunno where to put eatable
is_prop_accessable_at(know, 2, eat).

% debugging
is_prop_accessable_at(know, 3, inherit).
is_prop_accessable_at(know, 3, isnt).
is_prop_accessable_at(know, 3, inherited).

is_prop_accessable_at(know, 4, held_by).
is_prop_accessable_at(know, 4, class_desc).
is_prop_accessable_at(know, 4, has_sense).
is_prop_accessable_at(know, 4, knows_verbs).
is_prop_accessable_at(know, 4, can_be).

is_prop_accessable_at(see, 5, co(_)).

% action = try it to find out
is_prop_accessable_at(action, 3, move).
is_prop_accessable_at(action, 5, effect).
is_prop_accessable_at(action, 5, after).
is_prop_accessable_at(action, 5, before).
is_prop_accessable_at(action, 5, breaks_into).
is_prop_accessable_at(action, 5, oper).
is_prop_accessable_at(action, 5, cant_go).
is_prop_accessable_at(_, N, P):- var(N), compound(P), safe_functor(P, F, _), is_prop_accessable_at(action, 5, F), !, N = 5.


is_prop_accessable_at(_, _, P):- \+ compound(P), !, fail.
is_prop_accessable_at(S, N, F = _):- !, is_prop_accessable_at(S, N, F).
is_prop_accessable_at(S, N, P):- safe_functor(P, F, _), is_prop_accessable_at(S, N, F).
is_prop_accessable_at(S, N, P) :- arg(1, P, F), is_prop_accessable_at(S, N, F).

object_props(Object, Sense, PropDepth, PropList, S0):-
 findall(P, (getprop(Object, P, S0), is_prop_accessable(Sense, PropDepth, P)), PropListL),
 list_to_set(PropListL, PropList), !.

:- meta_predicate(maybe_send_sense(0, *, *, *, *, *, *)).
maybe_send_sense(IF, Agent, Sense, Depth, Data, S0, S1):-
 call(IF) ->
   send_sense(Agent, Sense, Depth, Data, S0, S1)
  ; S0 = S1.

send_sense(Agent, Sense, Depth, Data, S0, S1):-
   queue_agent_percept(Agent, percept(Agent, Sense, Depth, Data), S0, S1).

act_examine(Agent, Sense, PrepFrom, Object, Depth, SA, S3):-
 add_prop_percepts(know, Agent, PrepFrom, Depth, Object, SA, S0),
 add_prop_percepts(Sense, Agent, PrepFrom, Depth, Object, S0, S1),
 add_child_percepts(Sense, Agent, PrepFrom, Depth, Object, S1, S2),
 add_exit_percepts(spatial, Sense, Agent, PrepFrom, Depth, Object, S2, S3), !.


add_prop_percepts(Sense, Agent, _PrepFrom, Depth, Object, S1, S2):-
   object_props(Object, Sense, Depth, KPropList, S1),
   maybe_send_sense((KPropList\==[]), Agent, Sense, Depth, props(Object, KPropList), S1, S2),!.

findall_set2(T, G, L):-findall(T, G, S), list_to_set(S, L).

get_relation_list(Object, RelationSet, S1) :-
  findall_set2(At,
     ((getprop(Object, has_rel(At, t), S1);
      (declared(h(spatial, At, _, Object), S1))),
     At\=exit(_)), RelationSet).

% add_child_percepts(_Sense, _Agent, _PrepIn, Depth, _Object, S1, S1):- Depth > 2, !.
add_child_percepts(Sense, Agent, PrepFrom, Depth, Object, S1, S2):-
 get_relation_list(Object, RelationSet, S1),
 (member(PrepFrom, RelationSet) -> UseRelationSet = [PrepFrom] ; UseRelationSet= RelationSet),
 % dmsg(get_relation_list(Object, RelationSet)),
 findall(percept(Agent, Sense, Depth, h(_Spatial, At, Object, Children)),
     ((member(At, UseRelationSet),
       child_percepts(Agent, Sense, Object, At, Depth, Children, S1))), PreceptS),
 queue_agent_percept(Agent, PreceptS, S1, S2).

% add_child_percepts(_Sense, _Agent, _PrepIn, Depth, _Object, S1, S1):- Depth > 2, !.
add_child_percepts_new(Sense, Agent, PrepFrom, Depth, Object, S1, S2):-
 get_relation_list(Object, RelationSet, S1),
 (member(PrepFrom, RelationSet) -> UseRelationSet = [PrepFrom] ; UseRelationSet= RelationSet),
 % dmsg(get_relation_list(Object, RelationSet)),
 findall(percept(Agent, Sense, Depth, Children),
     ((member(At, UseRelationSet),
       child_percepts_h(Agent, Sense, Object, At, Depth, Children, S1))), PreceptS),
 queue_agent_percept(Agent, PreceptS, S1, S2).

child_percepts_h(Agent, Sense, Object, At, Depth, Children, S0):-  At == at,
 getprop(Object, default_rel = Default, S0), Default\==At, !,
 child_percepts_h(Agent, Sense, Object, Default, Depth, Children, S0).
child_percepts_h(_Agent, _All, Object, At, _Depth, '<mystery>'(closed, At, Object), S1):- is_closed(At, Object, S1), !.
/*act_examine(Agent, Sense, At, Here, Depth, S0, S9):-  At == at,
 getprop(Object, default_rel = Default, S0), Default\==At, !,
 act_examine(Agent, Sense, Default, Here, Depth, S0, S9).
*/
child_percepts_h(Agent, Sense, Object, At, _Depth, Children, S1):- fail, !,
 findall_set2(h(Spatially, At, What, Object),
  (g_h(Spatially, At, What, Object, S1),
   nop(once(can_sense(Agent, Sense, What, S1)))),
   Children).

child_percepts_h(Agent, Sense, Object, At, _Depth, Children, S1):-
 findall_set2(h(Spatially, At, Object, What),
  (g_h(Spatially, At, What, Object, S1),
   nop(once(can_sense(Agent, Sense, What, S1)))),
   Children).


child_percepts(Agent, Sense, Object, At, Depth, Children, S0):-  At == at,
 getprop(Object, default_rel = Default, S0), Default\==At, !,
 child_percepts(Agent, Sense, Object, Default, Depth, Children, S0).
child_percepts(_Agent, _All, Object, At, _Depth, '<mystery>'(closed, At, Object), S1):- is_closed(At, Object, S1), !.
/*act_examine(Agent, Sense, At, Here, Depth, S0, S9):-  At == at,
 getprop(Object, default_rel = Default, S0), Default\==At, !,
 act_examine(Agent, Sense, Default, Here, Depth, S0, S9).
*/
child_percepts(Agent, Sense, Object, At, _Depth, Children, S1):- fail, !,
 findall_set2(h(Spatially, At, What, Object),
  (g_h(Spatially, At, What, Object, S1),
   nop(once(can_sense(Agent, Sense, What, S1)))),
   Children).

child_percepts(Agent, Sense, Object, At, _Depth, Children, S1):-
 findall_set2(What,
  (g_h(_Spatial, At, What, Object, S1),
   nop(once(can_sense(Agent, Sense, What, S1)))),
   Children).

