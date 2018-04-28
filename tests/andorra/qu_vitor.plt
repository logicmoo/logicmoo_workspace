:- begin_tests(qu_vitor).

:- use_module(qu_vitor).

test(qu_vitor) :-
    forall(run(8, _), true).
    
:- end_tests(qu_vitor).
