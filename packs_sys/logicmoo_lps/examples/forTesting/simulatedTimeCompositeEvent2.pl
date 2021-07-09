
:- expects_dialect(lps).

maxTime(20).

simulatedRealTimeBeginning('2014-06-01'). 
simulatedRealTimePerCycle(14400). % 4 hours

actions bye(_Date), hello(_Date).

if real_date_00(Date) from T1 to T2 
then hello(Date) from T2. 

if real_date_24(Date) from T1 to T2 
then bye(Date) from T2. 

real_date(Y/M/D) at T if 
	real_time(Now) at T, stamp_date_time(Now,date(Y,M,D,_,_,_,_,_,_),local).

real_date_00(Date) from Last to First if  % last cycle of eve, first cycle of date
	real_date(Previous) at Last,
	First is Last+1,
	real_date(Date) at First,
	Date \= Previous.

real_date_24(Date) from Last to First if  % last cycle of Date, first cycle of next
	real_date(Date) at Last,
	First is Last+1,
	real_date(Next) at First,
	Date \= Next.


/** <examples> 
?- go(Timeline).
*/
