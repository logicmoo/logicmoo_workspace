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
    generate_rtchecks(_, Goal, RTChecksInfo),
    call_rtchecks(RTChecksInfo, Goal).

:- meta_predicate generate_rtchecks(?, 0, -).
generate_rtchecks(Loc, CM:Goal, RTChecksInfo) :-
    functor(Goal, F, A),
    functor(Head, F, A),
    implementation_module(CM:Goal, M),
    ( proc_ppassertion(Goal, _, [], Loc, RTChecks)
    ->RTChecksInfo = CM:RTChecks
    ; collect_assertions(Head, M, rtcheck, Assertions),
      Assertions \= [], !,
      generate_rtchecks(Assertions, Head, M, Loc, G1, G2, G),
      RTChecksInfo = rtchecks_info(Head, M, G1, G2, G)
    ).
generate_rtchecks(_, Goal, Goal).

call_rtchecks(rtchecks_info(Head, M, G1, G2, CM:Goal), CM:Goal) :- !,
    (M \= CM -> G0 = G1 ; G0 = G2),
    qualify_meta_goal(CM:Goal, Head),
    lists_to_lits(G0, RTChecks),
    call(RTChecks).
call_rtchecks(CM:RTChecks, CM:Goal) :-
    arg(1, RTChecks, Goal),
    call(RTChecks).

