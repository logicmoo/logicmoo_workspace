
:- expects_dialect(lps).

real_date(Y/M/D) at T if 
	real_time(Now) at T, stamp_date_time(Now,date(Y,M,D,_,_,_,_,_,_),local).

real_date_begin(Date) from Last to First if  % last cycle of eve, first cycle of date
	real_date(Previous) at Last,
	First is Last+1,
	real_date(Date) at First,
	Date \= Previous.

real_date_end(Date) from Last to First if  % last cycle of Date, first cycle of next
	real_date(Date) at Last,
	First is Last+1,
	real_date(Next) at First,
	Date \= Next.

maxTime(15).
simulatedRealTimeBeginning('2014-05-31'). 
simulatedRealTimePerCycle(28800). % 8 hours; 3 cycles per calendar day

fluents today(_Date,_FirstCycle).

real_date_begin(Date) from _ to First initiates today(Date,First).

real_date_end(Date) terminates today(Date,_).

