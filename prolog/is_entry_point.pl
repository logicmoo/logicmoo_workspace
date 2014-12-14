:- module(is_entry_point, [is_entry_point/1]).

is_entry_property(exported).
is_entry_property((public)).
is_entry_property(imported_from(_)).
is_entry_property(multifile).

:- dynamic http_dispatch:handler/4.
:- multifile http_dispatch:handler/4.

:- meta_predicate is_entry_point(:).
is_entry_point(M:H) :-
    functor(H, Name, A),
    A>0,
    succ(A2, A),
    functor(H2, Name, A2),
    http_dispatch:handler(_, M:H2, _, _), !.
is_entry_point(Ref) :-
    is_entry_property(Prop),
    predicate_property(Ref, Prop), !.
