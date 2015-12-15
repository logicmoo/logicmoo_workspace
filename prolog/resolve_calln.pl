:- module(resolve_calln,
	  [resolve_calln/2,
	   do_resolve_calln/2]).

:- use_module(xlibrary(extend_args)).

% May be this is slow, but it works:
resolve_calln(M:CallN, M:Goal) :- !,
    resolve_calln(CallN, Goal).
resolve_calln(Meta, Goal) :-
    do_resolve_calln(Meta, Meta2), !,
    resolve_calln(Meta2, Goal).
resolve_calln(Goal, Goal).

do_resolve_calln(Meta, Meta2) :-
    functor(Meta, call, A),
    A >= 2,
    Meta =.. [call, Call|Args],
    extend_args(Call, Args, Meta2).
