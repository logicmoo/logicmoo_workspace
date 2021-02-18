
:- expects_dialect(lps).

:- use_module(library(date)).
maxTime(10).

simulatedRealTimeBeginning('2014-06-01'). 
simulatedRealTimePerCycle(43200). % 12 hours


real_time(T) from T1 to T2 if \+ atom(T), real_time(T) at T1, T2 is T1+1.
real_time(T) from T1 to T2 if 
	atom(T), parse_time(T,_,Tseconds), 
	real_time(TS) at T1, T2 is T1+1, Tseconds>=TS, real_time(TSnext) at T2, Tseconds<TSnext.

if real_time('2014-06-03') from T1 to T2 then writeln(T1-T2) from T2.
