:- module(rtchecks_eval,
	  [rtchecks_eval/1,
	   generate_rtchecks/3]).

:- use_module(library(assrt_lib)).
:- use_module(library(rtchecks_gen)).
:- use_module(library(implementation_module)).
:- use_module(library(resolve_calln)).
:- use_module(library(qualify_meta_goal)).

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
    ( proc_ppassertion(Goal, CM, RTChecks)
    ->true
    ; implementation_module(CM:Goal, M),
      generate_pred_rtchecks(Goal, M, CM, RTChecks)
    ).

generate_pred_rtchecks(Goal, M, CM, RTChecks) :-
    ( asr_head_prop(_, AM, Goal, _, prop, _, _),
      implementation_module(AM:Goal, M)
    ->RTChecks = CM:Goal
    ; ( qualify_meta_goal(Goal, M, CM, Pred),
        collect_assertions(Pred, M, rtcheck, AsrL),
	AsrL \= []
      ->RTChecks = rtchecks_rt:rtcheck_goal(Pred, M, CM, AsrL)
      ; RTChecks = CM:Goal
      )
    ).
