:- module(assert_db_local, [
 l_goal_expansion/3,
 l_retractall_hp/2,
 l_retractall/2,
 l_retractall/1,
 l_retract_hp/2,
 l_retract/2,
 l_retract/1,
 into_hp/2,
 check_l/1,
 l_call/1,
 l_call/2,
 l_install/0,
 l_reset/0,
 last_cons/2,
 l_assertz_hp/2,
 l_assertz/2,
 l_assertz/1,
 l_assert/2,
 l_assert/1,
 l_asserta_hp/2,
 l_asserta/2,
 l_asserta/1,
 l_erase/2,
 l_clause/4,
 ref_of/3,
 l_clause/3,
 l_erase/1,
 l_show/1,
 l_listing/1,
 l_listing/0,
 is_l_dynamic/2,
 make_l_dynamic/3,
 l_dynamic/1,
 l_install/0]).

:- set_module(class(library)).
% :- use_module(library(logicmoo_common)).

/*  Logicmoo Debug Tools
% ===================================================================
% File 'logicmoo_util_varnames.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_varnames.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/

:- module_transparent((
 l_goal_expansion/3,
 l_retractall_hp/2,
 l_retractall/2,
 l_retractall/1,
 l_retract_hp/2,
 l_retract/2,
 l_retract/1,
 into_hp/2,
 l_call/1,
 l_call/2,
 % l_install/0,
 l_reset/0,
 last_cons/2,
 l_assertz_hp/2,
 l_assertz/2,
 l_assertz/1,
 l_assert/2,
 l_assert/1,
 l_asserta_hp/2,
 l_asserta/2,
 l_asserta/1,
 l_erase/2,
 l_clause/4,
 ref_of/3,
 l_clause/3,
 check_l/1,
 l_call/1,
 l_erase/1,
 l_show/1,
 l_listing/1,
 l_listing/0,
 is_l_dynamic/2,
 make_l_dynamic/3,
 l_dynamic/1)).

:- dynamic(tmp:is_dynamic_l/1).
:- dynamic(t_l:is_l_file/1).


l_dynamic((A, B)):- !, l_dynamic(A), l_dynamic(B).
l_dynamic(F/A):- functor(P, F, A), make_l_dynamic(P, F, A).
make_l_dynamic(P, _, _):- tmp:is_dynamic_l(P), !.
% only fresh preds (not defined elsewhere)
% l_dynamic(P, F, A):-  predicate_property(P, defined), !, dynamic(F/A).
make_l_dynamic(P, F, A):-
   prolog_load_context(module, M),
   assert(tmp:is_dynamic_l(P)), !,
   assert((M:(P:- l_call(P)))),
   compile_predicates([M:F/A]).


is_l_dynamic((P:-_), DB):- !, is_l_dynamic(P, DB).
is_l_dynamic(P, DB):- tmp:is_dynamic_l(P), !, nb_current('$db', DB), !.
is_l_dynamic(P, DB):- \+ predicate_property(P, defined),
  functor(P, F, A), make_l_dynamic(P, F, A), !, nb_current('$db', DB), !.


l_listing:-
  listing(tmp:is_dynamic_l/1),
  nb_current('$db', DB), l_listing(DB).

l_listing(val(H, T, _)):-
  format('~N% Head~n', []),
  maplist(l_show, H),
  format('~N% Tail~n', []),
  maplist(l_show, T).

l_show(H:-B):- B==true, !, format('~N ~p.~n', [H]).
l_show(H):- format('~N ~p.~n', [H]).




l_erase(Ref):- nb_current('$db', DB), l_erase(DB, Ref).
l_clause(H, B, Ref):- nb_current('$db', DB), l_clause(DB, H, B, Ref).

ref_of(List, E, OneLeft):- List=[H|T], (H=E -> OneLeft=List ; ref_of(T, E, OneLeft)).

l_clause(val(List, _, _), H, B, Ref):- H=..[F|HL], HP=..[F,B|HL], ref_of(List, HP, Ref).

into_hp((H:-B), HP):- H=..[F|HL], HP=..[F,B|HL].
into_hp(  H   , HP):- H=..[F|HL], HP=..[F,true|HL].

l_asserta(P):- is_l_dynamic(P, DB), !, l_asserta(DB, P), check_l(DB).
l_asserta(P):- asserta(P).
l_asserta(DB, P) :- into_hp(P, HP), l_asserta_hp(DB, HP).
l_asserta_hp(DB, P):-
  nb_setarg(3, DB, P), arg(3, DB, CopyP),
  arg(1, DB, Front), nb_linkarg(1, DB, [CopyP|Front]).

l_assert(P):- l_assertz(P).
l_assert(DB, H):- l_assertz(DB, (H:-true)).


l_assertz(P):- is_l_dynamic(P, DB), !, l_assertz(DB, P), check_l(DB).
l_assertz(P):- assertz(P).
l_assertz(DB, P) :- into_hp(P, HP), l_assertz_hp(DB, HP).
l_assertz_hp(DB, P):-
  arg(2, DB, WasEnd),
  l_assertz_hp_3(DB, WasEnd, P), !.

l_assertz_hp_3(_, WasEnd, P):- arg(1, WasEnd, empty), !, nb_setarg(1, WasEnd, P).
l_assertz_hp_3(DB, WasEnd, P):-
  nb_setarg(3, DB, [P]), arg(3, DB, NewEnd),
  % NewEnd now has duplicated term
  nb_linkarg(2, WasEnd, NewEnd),
  nb_linkarg(2, DB, NewEnd).



last_cons(In, Last):- In=[_|List],
  ( ( \+ List=[_|_] ; var(List))
    -> Last=In
    ;  last_cons(List, Last)).

l_reset:-
  nb_setval('$db', val(_, _, [empty])),
  nb_current('$db', DB),
  arg(3, DB, End),
  nb_linkarg(1, DB, End),
  nb_linkarg(2, DB, End),
  check_l(DB).

l_install:- prolog_load_context(source, F), asserta(t_l:is_l_file(F)).

l_call(val(List, _, _), H):- member((H:-Body), List), call(Body).
l_call(P):- nb_current('$db', DB), l_call(DB, P).

l_retract(P):- is_l_dynamic(P, DB), !, l_retract(DB, P).
l_retract(P):- retract(P).
l_retract(DB, P0):- into_hp(P0, P), l_retract_hp(DB, P).
l_retract_hp(DB, P):-
  arg(1, DB, List),
  notrace(ref_of(List, P, Ref)),
  check_l(DB),
  l_erase(DB, Ref).

l_erase(DB, Ref):- Ref=[_|T], T= [H1|T1],
   nb_linkarg(2, Ref, T1), nb_linkarg(1, Ref, H1),
   (T1 \==[] -> true ; nb_linkarg(2, DB, Ref)), !, check_l(DB).
l_erase(DB, Ref):- DB=val(_, Ref, _), nb_linkarg(1, Ref, empty), !, check_l(DB).

check_l(_).
% check_l(val(List, End, _)):- notrace((last_cons(List, Last), (same_term(End, Last)->true;throw(not_same_term(End, Last))))).

l_retractall(P):- is_l_dynamic(P, DB), !, l_retractall(DB, P).
l_retractall(P):- retractall(P).
l_retractall(DB, P0 :- _ ):- !, into_hp(P0:- _, P), l_retractall_hp(DB, P).
l_retractall(DB, P0      ):-    into_hp(P0:- _, P), l_retractall_hp(DB, P).
l_retractall_hp(DB, P):- \+ \+ l_retract_hp(DB, P), !, l_retractall_hp(DB, P).
l_retractall_hp(_, _).

l_goal_expansion(_, _, _):- prolog_load_context(source, F), \+ t_l:is_l_file(F), !, fail.
l_goal_expansion(_, I, O):- do_l_goal_expansion(I, O).

do_l_goal_expansion(dynamic(P), l_dynamic(P)).
do_l_goal_expansion(assert(P), l_assert(P)).
do_l_goal_expansion(assertz(P), l_assertz(P)).
do_l_goal_expansion(asserta(P), l_asserta(P)).
do_l_goal_expansion(retract(P), l_retract(P)).
do_l_goal_expansion(retractall(P), l_retractall(P)).
do_l_goal_expansion(erase(P), l_erase(P)).
do_l_goal_expansion(clause(H, B), l_clause(H, B, _)).
do_l_goal_expansion(clause(H, B, R), l_clause(H, B, R)).

:- if(current_predicate(fixup_exports/0)).
:- fixup_exports.
:- else.
l_module:-
 prolog_load_context(source, S), prolog_load_context(module, M),
 forall(source_file(M:H, S),
 (functor(H, F, A),
 format('~N ~q/~q, ~n', [F, A]))).
:- endif.

:- l_reset.

:- system:import(l_goal_expansion/3).
:- '$toplevel':import(l_goal_expansion/3).
:- multifile(system:goal_expansion/2).
:- module_transparent(system:goal_expansion/2).
system:goal_expansion(I, O):-
  prolog_load_context(module, M),
  l_goal_expansion(M, I, O).

