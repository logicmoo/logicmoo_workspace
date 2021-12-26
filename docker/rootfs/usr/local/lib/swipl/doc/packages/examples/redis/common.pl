:- module(common,
          [ make_group/0
          ]).

:- redis_server(test, localhost:6379, [version(3)]).

:- debug(redis(recover)).
:- debug(redis(claimed)).

make_group :-
    catch(redis(test, xgroup(create, candidates, primes, $, mkstream), _),
          error(redis_error(busygroup,_),_),
          true).
