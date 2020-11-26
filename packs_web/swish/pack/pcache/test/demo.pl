:- use_module(library(apply_macros)).
:- use_module('../prolog/cache_rocks').

:- initialization cache_open('.cache').

numlist(N) :-
    forall(between(1, N, _),
           cached(numlist(1, 10, _))).

my_numlist(N) :-
    forall(between(1, N, _),
           cached(my_numlist(1, 10, _))).

my_numlist(L, U, Ns) :-
    must_be(integer, L),
    must_be(integer, U),
    L =< U,
    my_numlist_(L, U, Ns).

my_numlist_(U, U, List) :-
    !,
    List = [U].
my_numlist_(L, U, [L|Ns]) :-
    L2 is L+1,
    my_numlist_(L2, U, Ns).

native_numlist(N) :-
    forall(between(1, N, _),
           my_numlist(1, 10, _)).

p(1, a).
p(2, b).
p(3, c).

sine(Degrees, Val) :-
    Rad is Degrees*pi/180,
    Val is sin(Rad).

:- cache_dynamic
    age(+,-).

td(F,T,Q) :-
    forall(between(F,T,I),
           (   cache_assert(age(name, I)),
               (   I mod Q =:= 0
               ->  age(name, _Age)
               ;   true
               )
           )).
