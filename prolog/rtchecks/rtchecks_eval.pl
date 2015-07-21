:- module(rtchecks_eval,
	  [rtchecks_eval/1,
	   generate_rtchecks/4]).

:- use_module(library(implementation_module)).
:- use_module(library(assertions/assrt_lib)).
:- use_module(library(qualify_meta_goal)).
:- use_module(rtchecks(rtchecks_gen)).
:- use_module(rtchecks(rtchecks_basic)).
:- use_module(library(extend_args)).

:- expects_dialect(swi).
:- meta_predicate rtchecks_eval(0).
rtchecks_eval(M:Goal) :-
    generate_rtchecks(_, M, Goal, RTChecks),
    call(M:RTChecks).

generate_rtchecks(Loc, M, Goal, RTChecks) :-
    apply_body(generate_literal_rtchecks(Loc), M, Goal, RTChecks).

builtin_spec(G, S) :-
    predicate_property(system:G, meta_predicate(S)),
    once(arg(_, S, 0 )).

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

maparg(Apply, N, S, G, R) :-
    arg(N, S, AS), !,
    arg(N, G, AG),
    arg(N, R, AR),
    call(Apply, AS, AG, AR),
    succ(N, N1),
    maparg(Apply, N1, S, G, R).
maparg(_, _, _, _, _).

% May be this is slow, but it works:
resolve_meta_call(M:Meta, M:Goal) :- !,
    resolve_meta_call(Meta, Goal).
resolve_meta_call(Meta, Goal) :-
    functor(Meta, call, A),
    A >= 2, !,
    Meta =.. [call, Call|Args],
    extend_args(Call, Args, Goal).
resolve_meta_call(Goal, Goal).

generate_literal_rtchecks(Loc, CM, Goal0, RTChecks) :-
    resolve_meta_call(Goal0, Goal),
    ( proc_ppassertion(Goal, _, [], Loc, RTChecks)
    ->true
    ; implementation_module(CM:Goal, M),
      ( assertion_head_body(Goal, M, _, prop, _, _, _, _, _)
      ->RTChecks = Goal0
      ; functor(Goal, F, A),
	functor(Head, F, A),
	( collect_assertions(Head, M, rtcheck, Assertions),
	  Assertions \= []
	->generate_rtchecks(Assertions, Head, M, Loc, G1, G2, G3, CM:Head),
	  qualify_meta_goal(CM:Goal, Head),
	  (M \= CM -> G0 = G1, G2 = G3 ; G0 = G3),
	  lists_to_lits(G0, RTChecks)
	; RTChecks = Goal0
	)
      )
    ).
