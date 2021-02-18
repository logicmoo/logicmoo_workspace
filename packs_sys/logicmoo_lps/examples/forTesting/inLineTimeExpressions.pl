
:- expects_dialect(lps).

maxTime(8).
actions a(_).

fluents f1, f2.
initially f1.

if f1 at T, f1 at T+2 then 
	a(f1f1f1) from T+3, 
	initiate f2.

if f2 at T 
	then a(f2) from T-1. % should fail