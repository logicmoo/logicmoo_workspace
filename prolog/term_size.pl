:- module(term_size, [term_size/2]).

term_size(Term, Size) :-
    term_size(Term, 1, Size).

term_size(Term) -->
    {var(Term)}, !.
term_size(Term) -->
    {atomic(Term)}, !.
term_size(Term) -->
    {compound(Term)},
    {functor(Term, _, N)},
    inc(N),
    term_size_arg(1, Term).

inc(N, S0, S) :- S is S0 + N.

term_size_arg(N0, Term) -->
    {arg(N0, Term, Arg)}, !,
    term_size(Arg),
    {succ(N0, N)},
    term_size_arg(N, Term).
term_size_arg(_, _) --> [].
