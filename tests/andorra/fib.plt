:- begin_tests(fib).

:- use_module(fib).

test(fib) :-
    forall(fib(20, _X), true).

:- end_tests(fib).
