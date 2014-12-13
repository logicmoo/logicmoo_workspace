:- module(qualify_meta_goal, [qualify_meta_goal/3]).

:- use_module(library(check), []). % for add_module/3
qualify_meta_goal(M:Goal0, Meta, M:Goal) :-
    functor(Goal0, F, N),
    functor(Goal, F, N),
    meta_goal(1, M, Meta, Goal0, Goal).

module_qualified(:) :- !.
module_qualified(N) :- integer(N), N >= 0.

meta_goal(N, M, Meta, Goal0, Goal) :-
    arg(N, Meta,  ArgM),
    !,
    arg(N, Goal0, Arg0),
    arg(N, Goal,  Arg),
    N1 is N + 1,
    ( module_qualified(ArgM) ->
      check:add_module(Arg0, M, Arg)
    ; Arg = Arg0
    ),
    meta_goal(N1, Meta, Goal0, Goal).
meta_goal(_, _, _, _).
