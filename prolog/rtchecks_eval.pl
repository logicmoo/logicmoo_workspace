:- module(rtchecks_eval,
	  [rtchecks_eval/1,
	   generate_rtchecks/3]).

:- use_module(assertions(assrt_lib)).
:- use_module(rtchecks(rtchecks_basic)).
:- use_module(rtchecks(rtchecks_gen)).
:- use_module(xlibrary(implementation_module)).
:- use_module(xlibrary(qualify_meta_goal)).
:- use_module(xlibrary(resolve_calln)).

:- meta_predicate rtchecks_eval(0).
rtchecks_eval(M:Goal) :-
    generate_rtchecks(Goal, M, RTChecks),
    call(RTChecks).

generate_rtchecks(Goal, M, RTChecks) :-
    apply_body(generate_literal_rtchecks, M, Goal, RTChecks).

rtcheck_lib(rtchecks_rt).
rtcheck_lib(nativeprops).

builtin_spec(G, S) :-
    predicate_property(system:G, meta_predicate(S)),
    predicate_property(system:G, imported_from(M)),
    \+ rtcheck_lib(M),
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

generate_literal_rtchecks(CM, Goal0, RTChecks) :-
    resolve_calln(Goal0, Goal),
    ( proc_ppassertion(Goal, CM, RTChecks0)
    ->RTChecks = RTChecks0
    ; implementation_module(CM:Goal, M),
      generate_pred_rtchecks(Goal, M, RTChecks0, Pred, PM),
      ( RTChecks0 == PM:Pred
      ->RTChecks = CM:Goal
      ; PM:Pred  = CM:Goal,
	RTChecks = RTChecks0
      )
    ).

generate_pred_rtchecks(Goal, M, RTChecks, Pred, PM) :-
    ( assertion_head_body(Goal, M, _, prop, _, _, _, _, _, _)
    ->RTChecks = PM:Pred
    ; functor(Goal, F, A),
      functor(Head, F, A),
      ( collect_assertions(Head, M, rtcheck, Assertions),
	Assertions \= []
      ->generate_rtchecks(Assertions, M, G1, G2, G3, PM:Pred),
	functor(Pred, F, A),
	qualify_meta_goal(Pred, M, PM, Head),
	% TODO: Be careful if you want to refactorize this part, now CM is
	% static:
	lists_to_lits(G3, R3),
	( G1 == G2
	->RTChecks = R3
	; lists_to_lits(G1, R1),
	  RTChecks = checkif_modl(M, PM, M:R1, G2, M:R3)
	)
	% (M \= CM -> G0 = G1, G2 = G3 ; G0 = G3),
	% lists_to_lits(G0, RTChecks)
      ; RTChecks = PM:Pred
      )
    ).
