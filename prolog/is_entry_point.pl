:- module(is_entry_point, [is_entry_point/2]).

is_entry_property(exported).
is_entry_property((public)).
is_entry_property(imported_from(_)).
is_entry_property(multifile).

:- dynamic http_dispatch:handler/4.
:- multifile http_dispatch:handler/4.

:- multifile is_entry_point_hook/2.

is_entry_point_hook(term_expansion(_, _), _).
is_entry_point_hook(goal_expansion(_, _), _).
is_entry_point_hook(term_expansion(_, _, _, _), _).
is_entry_point_hook(goal_expansion(_, _, _, _), _).
is_entry_point_hook(thread_message_hook(_, _, _), user).
is_entry_point_hook(prolog_trace_interception(_, _, _, _), user).

is_entry_point(H, M) :- is_entry_point_hook(H, M), !.
is_entry_point(H, M) :-
    functor(H, Name, A),
    A>0,
    succ(A2, A),
    functor(H2, Name, A2),
    http_dispatch:handler(_, M:H2, _, _), !.
is_entry_point(H, M) :-
    is_entry_property(Prop),
    predicate_property(M:H, Prop), !.
