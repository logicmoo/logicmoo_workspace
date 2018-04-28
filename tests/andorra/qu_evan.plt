:- begin_tests(qu_evan).

:- use_module(qu_evan).

test(qu_evan) :-
    forall(queen(8, _), true).

:- end_tests(qu_evan).
