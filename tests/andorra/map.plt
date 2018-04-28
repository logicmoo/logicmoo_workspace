:- begin_tests(map).

:- use_module(map).

test(map) :-
    forall(test_all(100), true).

:- end_tests(map).
