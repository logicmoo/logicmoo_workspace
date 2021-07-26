:- begin_tests(lists).

:- ensure_loaded(parser_charniak).

test(test_charniak) :-
        test_charniak1,
        test_charniak2,
        test_charniak3.

:- end_tests(test_charniak).

