:- module(change_alias, [change_alias/3]).

:- meta_predicate change_alias(2,+,-).
change_alias(Changer, Alias0, Alias) :-
    compound(Alias0), !,
    functor(Alias0, F, A),
    functor(Alias, F, A),
    arg(1, Alias0, Arg0),
    arg(1, Alias, Arg),
    change_alias(Changer, Arg0, Arg).
change_alias(Changer, Alias0, Alias) :-
    call(Changer, Alias0, Alias).
