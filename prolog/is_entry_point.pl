:- module(is_entry_point, [is_entry_point/1]).

is_entry_property(exported).
is_entry_property((public)).
is_entry_property(imported_from(_)).
is_entry_property(multifile).

:- meta_predicate is_entry_point(:).
is_entry_point(Ref) :-
    is_entry_property(Prop),
    predicate_property(Ref, Prop),
    !.
