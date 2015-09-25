:- module(near, [near/3]).

% :- prop near(A, B, Eps) # "Verifies that abs(@var{B} -
%    @var{A})/(abs(@var{B}) + (@var{A})) =< @var{Eps}.".

near(A, B, _) :-
    A==B, !.
near(A, _, _) :-
    var(A),
    !,
    fail.
near(_, B, _) :-
    var(B),
    !,
    fail.
near(A, B, Eps) :-
    number(A),
    number(B),
    !,
    near_num(A, B, Eps).
near(A, B, Eps) :-
    functor(A, F, N),
    functor(B, F, N),
    near_args(N, A, B, Eps).

near_args(0, _, _, _) :- !.
near_args(N, A, B, Eps) :-
    arg(N, A, ArgA),
    arg(N, B, ArgB),
    !,
    near(ArgA, ArgB, Eps),
    N1 is N - 1,
    near_args(N1, A, B, Eps).
near_args(_, _, _, _).

near_num(A,      B,      Eps) :- A =:= 0, !, abs(B) =< Eps.
near_num(A,      B,      Eps) :- B =:= 0, !, abs(A) =< Eps.
near_num(A,      B,      Eps) :-
    2 * abs(B - A) / (abs(A) + abs(B)) =< Eps.
