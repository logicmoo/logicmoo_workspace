
:- expects_dialect(lps).

%%% LPS "system library":
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

% We shouldn't need both end_of_day and real_date_end.
end_of_day(Date) from T1 to T2 if real_date_end(Date) from T1 to T2.

real_date_add(Y/M/D,Days,NY/NM/ND) :- 
    date_time_stamp(date(Y,M,D),T), NewT is T + (24*3600*Days), 
    stamp_date_time(NewT,date(NY,NM,ND,_,_,_,_,_,_),local).
%%%

maxTime(12). 
simulatedRealTimeBeginning('2015-06-03'). 
simulatedRealTimePerCycle(21600). % 4 cycles per calendar day

fluents 	requested/2, advanced/2,
covenant/1, represent_warrant/1, paid/3,
notified/3, potential_defaulted/1, cured/1, defaulted/2, remedied/1, due_payable/2,
total_due/1.

initially requested(borrower,1000),advanced(lender,1000),potential_defaulted(pay(borrower,lender,550)),notified(lender,default(pay(borrower,lender,550)),2015/6/2).

end_of_day(Date2)
initiates defaulted(Requirement, Date2) 
if potential_defaulted( Requirement), 
	notified(lender, default(Requirement), Date1), real_date_add(Date1, 2, Date2),
	not defaulted(_, _), not cured(Requirement). 
