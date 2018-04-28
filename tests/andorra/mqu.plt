:- begin_tests(mqu).

:- use_module(mqu).

test(mqu) :-
    forall(mqu(s(s(s(s(s(s(s(s(0 ))))))))), true).

:- end_tests(mqu).
