:- begin_test(cr_list_handling).
:- use_module(prolog/cplint_r).

/* Deterministically true. */
test(build_xy_list, [true(Out = [[1]-5,[2]-6,[3]-7,[4]-8])]) :-
    X=[1,2,3,4],
    Y=[5,6,7,8],
    build_xy_list(X,Y,Out).

/* Deterministically false. Different lengths */
test(build_xy_list, [false]) :-
    X=[1,2,3,4],
    Y=[5,6,7],
    build_xy_list(X,Y,Out).

:- end_test(cr_histogram_r).
