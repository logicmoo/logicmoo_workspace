/***********************************************************************

 
         name: calendar.pl
  description: Module that knows about dates and times
               Dates are represented as date(Y,M,D)
               Times are represented as time(H,M,S)

*/

:-module( calendar, [weekday/2, next_weekday/2, previous_weekday/2, today/1, tomorrow/1, now/1]).

:- use_module(library(system),[datime/1]).

%returns today's date
today(date(Y,M,D)):-
	datime(datime(Y,M,D,_,_,_)).

%returns tomorrow's date
tomorrow(Date):-
	datime(datime(Y,M,D,_,_,_)),
	succ_date(date(Y,M,D),Date).

%returns yesterday's date
yesterday(Date):-
	datime(datime(Y,M,D,_,_,_)),	
	succ_date(Date,date(Y,M,D)).


%weekday(+Date,?Weekday)
%returns day of week for a date
%algorithm due to http://www.terra.es/personal2/grimmer/
%works for 2000-2099
weekday(date(Y,M,D),Weekday):-
	ground(date(Y,M,D)),
	X1 is Y-2000,
	X2 is X1 // 4,
	X3 is X1 + X2,
	monthcode(X4,M),
	X5 is X3 + X4 + D,
	X6 is X5 mod 7,
	(
	  0 is Y mod 4,
	  M < 3,
	  X7 is X6 -1
	;
	  X7 is X6
	),
	weekdaycode(X7,Weekday).

%next_weekday(?Date,+Weekday)
%returns Date for next Weekday (or one week after that, or two weeks etc)
next_weekday(Date,Weekday):-
	ground(Weekday),
	today(Today),
	succ_date_iterate(forward,Today,Weekday,Date1),
	%return Date1 or iterate another week)
	(
	  Date = Date1
	;
	  succ_date(Date1,Date2),
	  succ_date_iterate(forward,Date2,Weekday,Date)
	  ).

previous_weekday(Date,Weekday):-
	ground(Weekday),
	today(Today),
	succ_date_iterate(back,Today,Weekday,Date1),
	%return Date1 or iterate another week)
	(
	  Date = Date1
	;
	  succ_date(Date2,Date1),
	  succ_date_iterate(back,Date2,Weekday,Date)
	  ).

%succ_date_iterate(+Direction,+StartDate,+Weekday,?Date)
%Date is date of next (or previous) Weekday relative to StartDate
succ_date_iterate(_Direction,Date,Weekday,Date):-
	weekday(Date,Weekday),!.

succ_date_iterate(forward,StartDate,Weekday,Date):-
	succ_date(StartDate,NewStartDate),
	succ_date_iterate(forward,NewStartDate,Weekday,Date).
	
succ_date_iterate(back,StartDate,Weekday,Date):-
succ_date(NewStartDate,StartDate),
	succ_date_iterate(back,NewStartDate,Weekday,Date).

%returns now's time
now(time(H,M,S)):-
	datime(datime(_,_,_,H,M,S)).


succ_date(X,Y):-
	var(X),
	var(Y),
	!,fail.

%last day of month

%january
succ_date(date(Y,1,31),date(Y,2,1)):-!.

%february
succ_date(date(Y,2,28),date(Y,3,1)):-
	N is Y mod 4,
	N \= 0,!.
succ_date(date(Y,2,29),date(Y,3,1)):-
	0 is Y mod 4,!.

%march
succ_date(date(Y,3,31),date(Y,4,1)):-!.

%april
succ_date(date(Y,4,30),date(Y,5,1)):-!.

%may
succ_date(date(Y,5,31),date(Y,6,1)):-!.

%june 
succ_date(date(Y,6,30),date(Y,7,1)):-!.

%july
succ_date(date(Y,7,31),date(Y,8,1)):-!.

%august
succ_date(date(Y,8,31),date(Y,9,1)):-!.

%september
 succ_date(date(Y,9,30),date(Y,10,1)):-!. 

%october
succ_date(date(Y,10,31),date(Y,11,1)):-!.

%november
succ_date(date(Y,11,30),date(Y,12,1)):-!.	  
	    

% december
succ_date(date(Y0,12,31),date(Y,1,1)):-!,
	(
	  var(Y0),
	  Y0 is Y -1;
	  Y is Y0 + 1
	).
%rest
succ_date(date(Y,M,D0),date(Y,M,D)):-
	(
	  var(D0),
	  D0 is D-1;
	  D is D0 + 1
	).


weekdaycode(1,monday).
weekdaycode(2,tuesday).
weekdaycode(3,wednesday).
weekdaycode(4,thursday).
weekdaycode(5,friday).
weekdaycode(6,saturday).
weekdaycode(7,sunday).
weekdaycode(N,Weekday):-
	N<1,
	M is N+7,
	weekdaycode(M,Weekday).


%monthcode(Code,Month)
monthcode(6,1).
monthcode(2,2).
monthcode(2,3).
monthcode(5,4).
monthcode(0,5).
monthcode(3,6).
monthcode(5,7).
monthcode(1,8).
monthcode(4,9).
monthcode(6,10).
monthcode(2,11).
monthcode(4,12).
	 


		
	


