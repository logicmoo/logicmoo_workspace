
%
:- include(library(lps_syntax)).

%%% LPS "system library"; some of these predicates have special handling in interpreter.pl
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

real_date_add(Date1,Days,NY/NM/ND) :- 
    nonvar(Date1), Date1=Y/M/D,
    date_time_stamp(date(Y,M,D),T), 
    real_date_add_hack(Days,T,NewT,NY,NM,ND),
    stamp_date_time(NewT,date(NY,NM,ND,_,_,_,_,_,_),local).

% somehow this if-thene-se is not being compiled properly (as C->A;B) by SWI 7.7.2 
% (on barebones SWI... on SWISH it works well...) when including this file:
real_date_add_hack(Days,T,NewT,_NY,_NM,_ND) :- nonvar(Days), !, NewT is T + 24*3600*Days.
real_date_add_hack(Days,T,NewT,NY,NM,ND) :- date_time_stamp(date(NY,NM,ND),NewT), Days is (NewT-T)/(24*3600).

%%%
