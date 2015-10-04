:- module(rtchecks_eval,
	  [rtchecks_eval/1,
	   generate_rtchecks/4]).

:- use_module(library(implementation_module)).
:- use_module(assertions(assrt_lib)).
:- use_module(library(qualify_meta_goal)).
:- use_module(rtchecks(rtchecks_gen)).
:- use_module(rtchecks(rtchecks_basic)).
:- use_module(library(resolve_calln)).

:- meta_predicate rtchecks_eval(0).
rtchecks_eval(M:Goal) :-
    generate_rtchecks(Goal, M, _, RTChecks),
    call(RTChecks).

generate_rtchecks(Goal, M, Loc, RTChecks) :-
    apply_body(generate_literal_rtchecks(Loc), M, Goal, RTChecks).

builtin_spec(G, S) :-
    predicate_property(system:G, meta_predicate(S)),
    once(arg(_, S, 0 )).

:- meta_predicate apply_body(3, ?, ?, ?).
apply_body(_, _, G, G) :- var(G), !.
apply_body(Apply, _, M:G, M:R) :- !,
    apply_body(Apply, M, G, R).
apply_body(Apply, M, G, R) :-
    builtin_spec(G, S), !,
    functor(G, F, A),
    functor(R, F, A),
    maparg(apply_meta_arg(Apply, M), 1, S, G, R).
apply_body(Apply, M, G, R) :-
    call(Apply, M, G, R).

apply_meta_arg(Apply, M, 0, G, R) :- !,
    apply_body(Apply, M, G, R).
apply_meta_arg(_, _, _, R, R).

:- meta_predicate maparg(3, +, +, +, +).
maparg(Apply, N, S, G, R) :-
    arg(N, S, AS), !,
    arg(N, G, AG),
    arg(N, R, AR),
    call(Apply, AS, AG, AR),
    succ(N, N1),
    maparg(Apply, N1, S, G, R).
maparg(_, _, _, _, _).

generate_literal_rtchecks(Loc, CM, Goal0, rtchecks_rt:RTChecks) :-
    resolve_calln(Goal0, Goal),
    ( proc_ppassertion(Goal, CM, Loc, RTChecks)
    ->true
    ; implementation_module(CM:Goal, M),
      generate_pred_rtchecks(Loc, Goal, M, RTChecks, Pred, PM),
      PM:Pred = CM:Goal
    ).

generate_pred_rtchecks(Loc, Goal, M, RTChecks, Pred, PM) :-
    ( assertion_head_body(Goal, M, _, prop, _, _, _, _, _, _)
    ->RTChecks = PM:Pred
    ; functor(Goal, F, A),
      functor(Head, F, A),
      ( collect_assertions(Head, M, rtcheck, Assertions),
	Assertions \= []
      ->generate_rtchecks(Assertions, M, Loc, G1, G2, G3, PM:Pred),
	functor(Pred, F, A),
	qualify_meta_goal(PM:Pred, Head),
	% TODO: Be careful if you want to refactorize this part, now CM is
	% static:
	lists_to_lits(G3, R3),
	( G1 == G2
	->RTChecks = R3
	; lists_to_lits(G1, R1),
	  RTChecks = checkif_modl(M, PM, R1, G2, R3)
	)
	% (M \= CM -> G0 = G1, G2 = G3 ; G0 = G3),
	% lists_to_lits(G0, RTChecks)
      ; RTChecks = PM:Pred
      )
    ).
