
:- expects_dialect(lps).

maxTime(6).

simulatedRealTimeBeginning('2014-06-01'). 
simulatedRealTimePerCycle(43200). % 12 hours

actions bye(_Date), hello(_Date).

if Date=2014/6/2, real_date_00(Date) from T1 to T2 
then hello(Date). 

real_date(Y/M/D) at T if 
	real_time(Now) at T, stamp_date_time(Now,date(Y,M,D,_,_,_,_,_,_),local).

real_date_00(Date) from T1 to T2 if  % More limited approach,  negated goal does not bind date
	not real_date(Date) at T1,
	T2 is T1+1,
	real_date(Date) at T2.
