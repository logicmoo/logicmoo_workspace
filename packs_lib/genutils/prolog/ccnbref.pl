:- module(ccnbref, [run_nb_ref/1, nbref_new/2, with_nbref/2, nbref_new/3,
                    nbref_app/2, nbref_get/2, nbref_add_with/3]).
/** <module> Context providing allocation and release of nb_ mutable variables
*/

:- use_module(library(delimcc), [p_reset/3, p_shift/2]).

:- meta_predicate nbref_app(+,2), nbref_add_with(+,3,+), run_nb_ref(0), with_nbref(-,0).

%% run_nb_ref(+Goal:pred) is det.
%  Runs Goal in a delimited context supporting nbref_new/2 for allocating
%  a fresh name for a nonbacktrackable mutable variable.
run_nb_ref(Goal) :- with_nbref(E, run(Goal, E)).

run(Goal, E) :- p_reset(nbref, Goal, Status), cont(Status, E).
cont(susp(X-Ref,Cont), E) :- nbref_new(E, X, Ref), run(Cont, E).
cont(done, _).

%% nbref_new(+V:A, -R:nbref(A)) is det.
%  Create a new nb_ variable initialised with value V. Must be run in the
%  context of run_nb_ref/1. 
nbref_new(X, Ref) :- p_shift(nbref, X-Ref).

%% with_nbref(-E:nbenv, +Goal:pred) is det.
%  Runs Goal. While Goal is active, E is bound to a value which can be used
%  with nbref_new/3 to allocate a fresh name for a nonbacktrackable mutable
%  variable. E remains bound after final exit, but should not be used.
with_nbref(E, Goal) :- setup_call_cleanup(setup(E), Goal, cleanup(E)).

setup(E) :- gensym(nbref,ID), atom_concat(ID,'.',E), nb_setval(E, 0).
cleanup(E) :- nb_getval(E, N), nb_delete(E), forall(between(1,N,I), delete(E,I)).
delete(E,I) :- atomic_concat(E,I,Ref), nb_delete(Ref).

%% nbref_new(+E:nbenv, +V:A, -R:nbref(A)) is det.
nbref_new(E, Value, Ref) :-
   nb_getval(E, I), J is I+1, atomic_concat(E, J, Ref),
   nb_setval(Ref, Value), nb_setval(E,J).

%% nbref_app(+R:ref(A), +P:pred(+A,-A)) is det.
nbref_app(Ref, P) :- nb_getval(Ref, X1), call(P,X1,X2), nb_setval(Ref, X2).

%% nbref_get(+R:ref(A), -V:A) is det.
nbref_get(Ref, X) :- nb_getval(Ref, X).

%% nbref_add_with(+R:ref(A), +P:pred(+B,+A,-A), +X:B) is det.
%  Helper for more efficient updating of non-backtrackable mutable data
%  structures. X is copied using duplicate_term/2, and P is expected to
%  insert it into the data structure without trailing any more data, such
%  that the variable can be updated using nb_linkval/2 instead of 
%  nb_setval/2 (which would copy the whole data structure).
nbref_add_with(Ref,Pred,X) :-
   duplicate_term(X,X1),
   nb_getval(Ref, Y1), call(Pred,X1,Y1,Y2),
   nb_linkval(Ref, Y2).
