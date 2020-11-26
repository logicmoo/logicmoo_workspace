:- module(prime_node, []).
:- use_module(library(redis_streams)).
:- use_module(library(redis)).
:- use_module(library(broadcast)).
:- use_module(library(main)).
:- use_module(library(statistics)).

:- use_module(common).

:- initialization(main,main).

main([Name]) :-
    set_prolog_flag(toplevel_goal, prolog),
    node(Name).

node(Name) :-
    make_group,
    catch_with_backtrace(
        listen_primes(Name),
        E,
        print_message(error, E)).

listen_primes(Consumer) :-
    thread_create(xlisten_group(test, primes, Consumer, [candidates],
                                [ block(0.1)
                                ]),
                  _, [alias(Consumer), detached(true)]).

:- listen(redis_consume(candidates, Data, Context),
          check_prime_string(Data, Context)).

check_prime_string(Data, Context) :-
    number_string(N, Data.get(candidate)),
    number_string(T0, Data.get(time)),
    !,
    call_time(is_prime(N), Dict, True),
    get_time(T1),
    T is T1-T0,
    redis(test, rpush(Data.drain,
                      p(N,True,Context.consumer,Dict.cpu,T) as prolog)).

is_prime(1) :- !.
is_prime(2) :- !.
is_prime(N) :-
    End is floor(sqrt(N)),
    (   between(2, End, I),
        N mod I =:= 0
    ->  !, fail
    ;   true
    ).
