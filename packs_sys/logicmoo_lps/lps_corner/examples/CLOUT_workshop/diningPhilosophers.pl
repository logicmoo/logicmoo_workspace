
:- expects_dialect(lps).

maxTime(7).
fluents		available(_).
actions		pickup(_,_), putdown(_,_).

initially	available(fork1),
		available(fork2),
		available(fork3),
		available(fork4),
		available(fork5).

philosopher(socrates).
philosopher(plato).
philosopher(aristotle).
philosopher(hume).
philosopher(kant).

adjacent(fork1, socrates, fork2).
adjacent(fork2, plato, fork3).
adjacent(fork3, aristotle, fork4).
adjacent(fork4, hume, fork5).
adjacent(fork5, kant, fork1).

if		philosopher(P)
then		dine(P) from T1 to T2.

dine(P) from T1 to T3	if
	adjacent(F1, P, F2),
	pickup(P, F1) from T1 to T2,
	pickup(P, F2) from T1 to T2,
	putdown(P, F1) from T2 to T3,
	putdown(P, F2) from T2 to T3 .

pickup(P, F)	terminates	available(F).
putdown(P, F)	initiates	available(F).

false	pickup(P, F),    not available(F).
false	pickup(P1, F),  pickup(P2, F), P1 \= P2.

/** <examples>
?- go(Timeline).
*/