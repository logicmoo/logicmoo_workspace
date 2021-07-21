:- module(primes,
          [ r_primes/1
          ]).
:- use_module(library(debug)).
:- use_module(library(redis)).
:- use_module(library(redis_streams)).
:- use_module(library(statistics)).

:- use_module(common).

r_primes(PerSecond) :-
    Sleep is 1/PerSecond,
    repeat,
    A is random(1 000 000 000),
    call_time(r_prime(A, p(_,_,Consumer,_CPU,_)), Time),
    format('~w answered ~D in ~6f sec~n', [Consumer, A, Time.wall]),
    sleep(Sleep),
    fail.

r_prime(Num, Reply) :-
    rlist_id(Id),
    add_candidate(Num, Id),
    redis(test, blpop(Id, 10), [_,Reply]),
    redis(test, del(Id), _).

rlist_id(Id) :-
    current_prolog_flag(pid, Pid),
    thread_self(Me),
    thread_property(Me, id(Tid)),
    format(atom(Id), 'list:~w:~w', [Pid, Tid]).

add_candidate(I, Into) :-
    get_time(Now),
    xadd(test, candidates, _, _{candidate:I, time:Now, drain:Into}).

