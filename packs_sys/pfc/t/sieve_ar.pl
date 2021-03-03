% Benchmark prime number generation  using   the  sieve  of Eratosthenes
% algorithm with assert/retract. This benchmark tests assert/retract and
% JIT index generation and destruction.
%
% Auther: Jan Wielemaker
% Copyright: Public domain.

:- use_module(library(assert_db_local)).
:- install_L.
:- disable_L.
:- enable_L.

top :- clean, primes(10000), fail.
top.


:- if(\+current_predicate(forall/2)).
forall(Cond,Act) :- \+ ( Cond, \+ Act ).
:- endif.
:- if(\+current_predicate(ignore/1)).
ignore(Goal) :- (Goal->true;true).
:- endif.
:- if(\+current_predicate(between/3)).
:- use_module(library(between)).
:- endif.

:- dynamic((prime/1, candidate/1)).

clean :-
    retractall(prime(_)),
    retractall(candidate(_)).

setup(N):- forall(between(2, N, I), assertz(candidate(I))).

primes(N) :-
    setup(N),
    sieve(N).

sieve(Max) :-
    retract(candidate(First)),
    !,
    assertz(prime(First)),
    First < Max,
    sieve(First, 2, Max),
    sieve(Max).
sieve(_).

sieve(N, Mul, Max) :-
    I is N*Mul,
    I =< Max,
    !,
    ignore(retract(candidate(I))),
    Mul2 is Mul+1,
    sieve(N, Mul2, Max).
sieve(_, _, _).

:- clean.

:- setup(40).
:- listing(clean).
