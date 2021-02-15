
:- expects_dialect(lps).

maxTime(10).

withinPeriod(First-Last) at T if
	T >= First, Last >= T.
    
actions a(_).

if withinPeriod(3 - 7) at T then 
	a(hello/T) from T.

if true then
	withinPeriod(3 - 7) at T1,
	a(hi/T1) from T1 to T2,
	a(hi2/T2) from T2,
	fail.
