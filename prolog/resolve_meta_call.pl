:- module(resolve_meta_call,
	  [resolve_meta_call/2]).

:- use_module(library(extend_args)).

% May be this is slow, but it works:
resolve_meta_call(M:Meta, M:Goal) :- !,
    resolve_meta_call(Meta, Goal).
resolve_meta_call(Meta, Goal) :-
    functor(Meta, call, A),
    A >= 2, !,
    Meta =.. [call, Call|Args],
    extend_args(Call, Args, Meta2),
    resolve_meta_call(Meta2, Goal).
resolve_meta_call(Goal, Goal).
