

:- include('loop_check_tests.plt').

test(loop_inf0):- must(\+ loop_inf0).

test(loop_inf1):- must( loop_inf1),!.
