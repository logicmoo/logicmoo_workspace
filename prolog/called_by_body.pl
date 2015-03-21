:- module(called_by_body, [called_by_body/4]).

:- use_module(library(implementation_module)).

called_by_body(Body, CM, Body, CM) :- var(Body), !, fail.
called_by_body(CM:Body, _, H, M) :- called_by_body(Body, CM, H, M).
called_by_body((A,B),  CM, H, M) :- !,
    ( called_by_body(A, CM, H, M)
    ; called_by_body(B, CM, H, M)
    ).
called_by_body((A;B),  CM, H, M) :- !,
    ( called_by_body(A, CM, H, M)
    ; called_by_body(B, CM, H, M)
    ).
called_by_body(Goal, CM, H, M) :-
    predicate_property(CM:Goal, meta_predicate(Spec)),
    called_by_args(Goal, Spec, CM, H, M).
called_by_body(Goal, CM, Goal, M) :-
    implementation_module(CM:Goal, M).

called_by_args(Goal, Spec, CM, H, M) :-
    arg(N, Goal, Arg),
    arg(N, Spec, SA),
    called_by_arg(SA, Arg, CM, H, M).

called_by_arg(0, Goal, CM, H, M) :- !, called_by_body(Goal, CM, H, M).
called_by_arg(^, Goal, CM, H, M) :- !, called_by_body(Goal, CM, H, M).
called_by_arg(N, Goal, CM, H, M) :-
    integer(N),
    extend_args(Goal, N, Goal1),
    called_by_body(Goal1, CM, H, M).

extend_args(Goal, _, Goal) :- var(Goal), !, fail.
extend_args(M:Goal0, N, M:Goal) :- !,
    extend_args(Goal0, N, Goal).
extend_args(Goal, N, GoalEx) :-
    callable(Goal), !,
    Goal =.. List,
    length(Extra, N),
    append(List, Extra, ListEx),
    GoalEx =.. ListEx.
