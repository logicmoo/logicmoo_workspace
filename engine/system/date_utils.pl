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

% We don't need both end_of_day and real_date_end.OK for now.
end_of_day(Date) from T1 to T2 if real_date_end(Date) from T1 to T2.

real_date_add(Y/M/D,Days,NY/NM/ND) :- 
    date_time_stamp(date(Y,M,D),T), NewT is T + (24*3600*Days), 
    stamp_date_time(NewT,date(NY,NM,ND,_,_,_,_,_,_),local).
%%%