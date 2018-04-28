:- begin_tests(mutest).

:- use_module(mutest).

test(mutest) :-
    forall(mutest, true).

:- end_tests(mutest).


