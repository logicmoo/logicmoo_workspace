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


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_relation')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- defn_state_none(never_equal(sense, inst, agent)).
never_equal(Sense, Thing, Agent):- nop(never_equal(Sense, Thing, Agent)), !.
never_equal(Sense, Thing, Agent):-
  never_equal(Sense, Thing), never_equal(Sense, Agent).
never_equal(Sense, Thing):-
 xnotrace((freeze(Thing, (must_mw1(Thing\==Sense))), freeze(Sense, (must_mw1(Thing\==Sense))))).



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_relation')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- defn_state_getter(related_with_prop(domrel, inst, place, prop)).
related_with_prop(At, Object, Place, Prop, S0) :-
  g_h(Spatial, At, Object, Place, S0),
  getprop(Object, Prop, S0).

% getprop(Object, can_be(open, S0),
% \+ getprop(Object, =(open, t), S0).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_relation')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% -----------------------------------------------------------------------------

subrelation(child, at).
subrelation(in, child).
subrelation(on, child).
subrelation(worn_by, child).
subrelation(held_by, child).
subrelation(Sub, child_not_in):- dif(Sub, in), subrelation(Sub, child).
%subrelation(under, in).
%subrelation(reverse(on), child).

same_rel(Rel, Prep):- Rel==Prep.
same_rel(Rel, Prep):- \+ ground((Rel, Prep)), !, fail.
same_rel(Rel, Prep):- compound(Prep), !, arg(_, Prep, E), same_rel(Rel, E).
same_rel(Rel, Prep):- compound(Rel), !, arg(_, Rel, E), same_rel(E, Prep).
same_rel(Rel, Prep):- subrelation(Rel, Prep).

:- defn_state_getter(prep_to_rel(target, preprel, -domrel)).
prep_to_rel(Target, Prep, Rel, S0):- has_rel(Rel, Target, S0), same_rel(Rel, Prep), !.
prep_to_rel(Target, Prep, Rel, S0):- in_model(h(Spatial, Rel, Target, _), S0), same_rel(Rel, Prep), !.
prep_to_rel(Target, Prep, Rel, S0):- atom(Prep), prep_to_rel(Target,exit(Prep), Rel, S0), !.
prep_to_rel(Target, _Prep, Rel, S0):- default_rel(Rel, Target, S0).

:- defn_state_getter(has_rel(domrel, inst)).
has_rel(At, X, S0) :- hrel(At, X, S0).
has_rel(At, X, S0) :- sub_hrel(At, X, S0).
% default_rel(in, _, _S0) :- !.

:- defn_state_getter(default_rel(domrel, inst)).
default_rel(At, X, S0) :- hrel(At, X, S0), !.
default_rel(At, X, S0) :- sub_hrel(At, X, S0).
default_rel(in, _, _S0) :- !.

hrel(At, X, S0) :-
  getprop(X, default_rel = (At), S0).
hrel(At, X, S0) :-
  getprop(X, has_rel(At, TF), S0), TF \== f.

sub_hrel(At, X, S0) :-
  getprop(X, default_rel = (Specific), S0),
  subrelation(Specific, At).
sub_hrel(At, X, S0) :-
  getprop(X, has_rel(Specific, TF), S0), TF \== f,
  subrelation(Specific, At).


:- defn_state_getter(h(Spatial, domrel, source, target)).

h(Spatial, At, X, Y, S0):- g_h(Spatial, At, X, Y, S0).

:- defn_state_getter(g_h(Spatial, domrel, source, target)).


g_h(Spatial, At, X, Y, S0) :- in_model( h(Spatial, At, X, Y), S0).

g_h(Spatial, child, X, Y, S0) :- subrelation(At, child), g_h(Spatial, At, X, Y, S0).

g_h(Spatial, descended, X, Z, S0) :-
  g_h(Spatial, child, X, Z, S0).
g_h(Spatial, descended, X, Z, S0) :-
  g_h(Spatial, child, Y, Z, S0),
  g_h(Spatial, descended, X, Y, S0).

g_h(Spatial, open_traverse, X1, Z1, S0):- nonvar(X1), nonvar(Z1),
 ((X1=X,Z1=Z);(X1=Z,Z1=X)),
   ((g_h(Spatial, inside, Z, C, S0), is_closed(in, C, S0), % cant reach what is inside of something closed unless...
                          \+ g_h(Spatial, inside, X, C, S0))), !, fail.
g_h(Spatial, open_traverse, X, Z, S0):-
  %g_h(Spatial, touchable, X, Z, S0),
  g_h(Spatial, descended, X, Z, S0),
  \+ (g_h(Spatial, inside, X, Z, S0), is_closed(in, Z, S0)).

g_h(Spatial, in_scope, X, Z, S0):-
  g_h(Spatial, child, X, Y, S0),
  g_h(Spatial, descended, Z, Y, S0).
g_h(Spatial, in_scope, X, Z, S0):-
  g_h(Spatial, descended, X, Z, S0).

g_h(Spatial, touchable, X, Z, S0):-
  g_h(Spatial, in_scope, X, Z, S0),
  \+ ((g_h(Spatial, inside, Z, C, S0), is_closed(in, C, S0), % cant reach what is inside of something closed unless...
                          \+ g_h(Spatial, inside, X, C, S0))). % ... we are inside of that something as well

g_h(Spatial, takeable, X, Z, S0):-
  g_h(Spatial, touchable, X, Z, S0),
  X \= Z, % cant take self
  \+ getprop(Z, can_be(move, f)),
  \+ getprop(Z, can_be(take, f)),
  \+ g_h(Spatial, inside, X, Z, S0), % cant take outer object
  \+ g_h(Spatial, held_by, Z, X, S0). % cant take what already have


g_h(Spatial, inside, X, Z, S0) :- g_h(Spatial, in, X, Z, S0).
g_h(Spatial, inside, X, Z, S0) :- g_h(Spatial, in, Y, Z, S0),
          g_h(Spatial, descended, X, Y, S0).

g_h(Spatial,exit(Out), Inner, Outer, S0) :- in_out(In, Out),
  g_h(Spatial, child, Inner, Outer, S0),
  has_rel(In, Inner, S0),
  has_rel(child, Outer, S0),
  \+ is_closed(In, Inner, S0), !.
g_h(Spatial,exit(Off), Inner, Outer, S0) :- on_off(On, Off),
  g_h(Spatial, child, Inner, Outer, S0),
  has_rel(On, Inner, S0),
  has_rel(child, Outer, S0), !.
g_h(Spatial,exit(Escape), Inner, Outer, S0) :- escape_rel(Escape),
  g_h(Spatial, child, Inner, Outer, S0),
  has_rel(child, Inner, S0),
  has_rel(child, Outer, S0), !.


in_out(in, out).
on_off(on, off).
escape_rel(escape).

:- defn_state_getter(is_closed(prep, inst)).

:- defn_state_getter(in_state(domrel, inst)).
in_state(~(Opened), Object, S0) :- ground(Opened), !,
 getprop(Object, Opened=f, S0).
in_state(Opened, Object, S0) :-
 getprop(Object, Opened=t, S0).

:- defn_state_getter(is_closed(domrel, inst)).
is_closed(At, Object, S0) :-
 in_state(~(opened), Object, S0) -> getprop(Object, default_rel = At, S0).
%  getprop(Object, openable, S0),
%  \+ getprop(Object, open, S0).


:- defn_state_getter(from_loc(inst, place)).

from_loc(Thing, Here, S0):-
   g_h(Spatial, child, Thing, Here, S0), !.
from_loc(Thing, Here, S0):-
   g_h(Spatial, open_traverse, Thing, Here, S0), !.
from_loc(Thing, Here, S0):-
   g_h(Spatial, _, Thing, Here, S0), !.

:- defn_state_getter(open_traverse(inst, here)).

open_traverse(Thing, Here, S0):-
   g_h(Spatial, open_traverse, Thing, Here, S0).
open_traverse(Thing, Here, S0):-
   g_h(Spatial, open_traverse, Here, Thing, S0).





% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_action')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- defn_state_getter(applied_direction(start, source, prep, domrel, target)).
applied_direction(Start, Here, Dir, Relation, End, S0):-
 g_h(Spatial, _Relation, Start, Here, S0),
 g_h(Spatial,exit(Dir), Here, End, S0),
 has_rel(Relation, End, S0).



