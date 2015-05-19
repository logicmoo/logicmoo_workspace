:- module(rtchecks_eval,
	  [rtchecks_eval/1,
	   generate_rtchecks/3]).

:- use_module(library(implementation_module)).
:- use_module(library(qualify_meta_goal)).
:- use_module(rtchecks(rtchecks_gen)).
:- use_module(rtchecks(rtchecks_basic)).

:- expects_dialect(swi).
:- meta_predicate rtchecks_eval(0).
rtchecks_eval(Goal) :-
    generate_rtchecks(_, Goal, RTChecks),
    call(RTChecks).

:- meta_predicate generate_rtchecks(?, 0, -).
generate_rtchecks(Loc, CM:Goal, CM:RTChecks) :-
    functor(Goal, F, A),
    functor(Head, F, A),
    implementation_module(CM:Goal, M),
    ( proc_ppassertion(Goal, _, [], Loc, RTChecks)
    ->true
    ; collect_assertions(Head, M, rtcheck, Assertions),
      Assertions \= [], !,
      generate_rtchecks(Assertions, Head, M, Loc, G1, G2, G3, CM:Head),
      qualify_meta_goal(CM:Goal, Head),
      (M \= CM -> G0 = G1, G2=G3 ; G0 = G3),
      lists_to_lits(G0, RTChecks)
    ).
generate_rtchecks(_, Goal, Goal).
