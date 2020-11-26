
time(Predicate, Times, Result):-
	Times>0,
	statistics(runtime,[StartTime,_]),
        run_predicate(Times, Predicate),
	statistics(runtime,[EndTime,_]),
	run_predicate(Times, true),
	statistics(runtime, [ConstantTime,_]),
	Constant is ConstantTime - EndTime,
        RunTime is EndTime - StartTime,
	Result is (RunTime - Constant) / 1000.

run_predicate(Times, Predicate):-
	(Times = 0 -> true 
	; \+ \+(Predicate),
	  NextTimes is Times - 1,
	  run_predicate(NextTimes, Predicate)).

nrev([],[]).
nrev([Head|Tail], Result):-
	nrev(Tail,RTail),
	app(RTail, [Head], Result).

app([],L,L).
app([Head|L1],L2,[Head|L3]):-
	app(L1, L2,L3).


lips(Iterations, LIPS):-
	time(nrev([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
		   21,22,23,24,25,26,27,28,29,30],_X),Iterations, Time),
    LIPS is (496.0 * Iterations) / Time.

