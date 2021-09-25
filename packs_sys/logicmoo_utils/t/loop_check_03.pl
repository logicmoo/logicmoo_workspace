
:- include('loop_check_tests.plt').

loop_inf41(X) :- loop_check(loop_inf41(X),X=1).
loop_inf41(2).

test(loop_inf41):- mpred_test((   loop_inf41(X),X=1)),!.

test(loop_inf4a):- must((   loop_inf41(X),X=2,!)).
