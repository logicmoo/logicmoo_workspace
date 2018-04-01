:- begin_tests(andorra).

% Some of this tests become extremely slow, if you disable andorra:
:- use_module(dia_sums).
:- use_module(fib).
:- use_module(map).
:- use_module(money).
:- use_module(mqu).
:- use_module(mutest).
:- use_module(qu_evan).
:- use_module(qu_vitor).

test(dia_sums) :-
    forall(dia_sums, true).

test(fib) :-
    forall(fib(20, _X), true).

test(map) :-
    forall(test_all(100), true).

test(money) :-
    forall(money, true).

test(mqu) :-
    forall(mqu(s(s(s(s(s(s(s(s(0 ))))))))), true).

test(mutest) :-
    forall(mutest, true).

test(qu_evan) :-
    forall(queen(8, _), true).

test(qu_vitor) :-
    forall(run(8, _), true).
    
:- end_tests(andorra).
