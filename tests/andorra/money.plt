:- begin_tests(money).

:- use_module(money).

test(money) :-
    forall(money, true).

:- end_tests(money).
