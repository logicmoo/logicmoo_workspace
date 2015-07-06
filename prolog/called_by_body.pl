:- module(called_by_body, [called_by_body/4]).

:- use_module(library(implementation_module)).
:- use_module(library(extend_args)).

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
    length(Extra, N),
    extend_args(Goal, Extra, Goal1),
    called_by_body(Goal1, CM, H, M).
