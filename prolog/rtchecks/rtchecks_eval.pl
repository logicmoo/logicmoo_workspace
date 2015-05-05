:- module(rtchecks_eval,
	  [rtchecks_eval/1,
	   generate_rtchecks/3],
	  [assertions, nortchecks, nativeprops]).

:- use_module(library(implementation_module)).
:- use_module(library(qualify_meta_goal)).
:- use_module(rtchecks(rtchecks_tr)).

:- meta_predicate rtchecks_eval(goal).
rtchecks_eval(Goal) :-
    generate_rtchecks(_, Goal, RTChecks),
    call(RTChecks).

generate_rtchecks(Loc, CM:Goal, M:RTChecks) :-
    functor(Goal, F, A),
    functor(Head, F, A),
    implementation_module(CM:Goal, M),
    collect_assertions(Head, M, rtcheck, Assertions),
    Assertions \= [], !,
    current_prolog_flag(rtchecks_asrloc,  UseAsrLoc),
    current_prolog_flag(rtchecks_predloc, UsePredLoc),
    generate_rtchecks(Assertions, Head, M, [], Loc, (UsePredLoc, UseAsrLoc),
		      RTChecks, CM:Head),
    qualify_meta_goal(CM:Goal, Head).
generate_rtchecks(_, Goal, Goal).
