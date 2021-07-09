
:- expects_dialect(lps).

% Dining philosophers, detaching the dining table and cutlery from the guests and their seating arrangements 
% First start the program in diningPhilosphers-table.pl; then execute this program with the engine command below
% You may have to edit the other program's thread ID, lps1, check the output when starting the table
maxTime(10).

fluents table_(_). % inject fluent with golps(...,[...,delta_state([ + table(ThreadId) ]),...])

philosopher(P) :- setof(P,F1^F2^adjacent(F1,P,F2),Ps), member(P,Ps). % avoid retyping our guests...

adjacent(fork1, socrates, fork2).
adjacent(fork2, plato, fork3).
adjacent(fork3, aristotle, fork4).
adjacent(fork4, hume, fork5).
adjacent(fork5, kant, fork1).

if philosopher(P)
then table_(T), dine(P,T) from T1 to T2.

dine(P,Table) from T1 to T3	if
	adjacent(F1, P, F2),
	lps_ask(Table,[pickup(P, F1), pickup(P, F2)]) from T1 to T2,
	lps_ask(Table,[putdown(P, F1), putdown(P, F2)]) from T2 to T3.

/** <examples>
?- go(T,[dc,delta_state([ + table_(lps1) ])]).
*/