:- dynamic failed_test/1.

b5tests :-
	retrievaltest,
	displaytest.


start_msg(FN) :-
	nl,write('*** Testfile: '),
	write(FN),nl,nl.


succ_msg(N,TN) :-
	write('+++ '),
	write(TN),
	tab(1),
	write(N),
	tab(1),
	write('succeeded.'),nl.


fail_msg(N,TN) :-
	write('--- '),
	write(TN),
	tab(1),
	write(N),
	tab(1),
	write('FAILED.'),nl,
	assertz(failed_test(N)).


res_msg(_) :-
	\+ failed_test(_),
	nl,write('*** No test failed.'),nl.
res_msg(TN) :-
	nl,write('*** The following tests failed unexpectedly:'),nl,nl,
	failed_test(Arg),
	write('--> '),write(TN),tab(1),
	write(Arg),nl,fail.
res_msg(_) :-
	retractall(failed_test(_)).


end_msg(FN) :-
	nl,write('*** Testfile: '),
	write(FN),write(' ended.'),nl.


