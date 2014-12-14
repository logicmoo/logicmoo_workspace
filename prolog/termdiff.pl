:- module(termdiff, [termdiff/4]).

:- meta_predicate termdiff(+,+,2,-).
%% termdiff(+Term1, +Term2, :Diff, -List) is det.
%
termdiff(Term1, Term2, D, [E|L]) :-
    compound(Term1),
    compound(Term2),
    functor(Term1, F, A),
    functor(Term2, F, A), !,
    arg(E, Term1, Arg1),
    arg(E, Term2, Arg2),
    termdiff(Arg1, Arg2, D, L).
termdiff(Term1, Term2, D, []) :-
    call(D, Term1, Term2).
