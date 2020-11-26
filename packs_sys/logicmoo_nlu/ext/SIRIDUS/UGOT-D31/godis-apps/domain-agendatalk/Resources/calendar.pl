/***********************************************************************

 
         name: calendar.pl
  description: Knowledgebase about dates and times
               Dates are represented as date(Y,M,D)
               Times are represented as time(H,M,S)
       author: Rebecca Jonson

***************************************************************************/

:-module( calendar, [consistent/2,inconsistent/2,ampm_disamb/3, day2date/2, dayhalf/2, hour/3, weekday/2, next_weekday/2, previous_weekday/2, today/1, tomorrow/1, now/1, day2nr/2, month2nr/2]).

:- use_module(library(system),[datime/1]).
:-ensure_loaded(semsort_agendatalk).
%returns todays date

day2date(today, Date):-
 	today(Date).
day2date(tomorrow, Date):-
	tomorrow(Date).
day2date(aftertomorrow, Date):-
	datime(datime(Y,M,D,_,_,_)),
	succ_date(date(Y,M,D),Date1),
	succ_date(Date1,Date).
day2date(yesterday, Date):-
	yesterday(Date).
day2date(WeekDay, Date):-
	sem_sort(WeekDay, weekday),
	next_weekday(Date, WeekDay).
%%%next friday
day2date([next,WeekDay], Date):-
	sem_sort(WeekDay, weekday),
	succ_weekday(Date, WeekDay).
day2date(Day, date(Y,M,DayNr)):-
	sem_sort(Day,day),
	day2nr([Day], DayNr),
	datime(datime(Y,M,_,_,_,_)).
	
day2date([Day,Month],date(Y,M,D)):-
	sem_sort(Day,day),
	sem_sort(Month, month),
	day2nr([Day], D),
	month2nr([Month], M),
	datime(datime(Y,_,_,_,_,_)).
	
day2date([WD,Day,Month],date(Y,M,D)):-
	sem_sort(Day,day),
	sem_sort(Month,month),
	sem_sort(WD,weekday),
	day2nr([Day],D),
	month2nr([Month],M),
	datime(datime(Y,_,_,_,_,_)).



consistent(DATE,ANSWER):-
	(inconsistent(DATE),
	  !,fail
	;
	    ANSWER=yes).
inconsistent(date(_,4,31)).
inconsistent(date(_,6,31)).
inconsistent(date(_,9,31)).
inconsistent(date(_,11,31)).
inconsistent(date(_,2,30)).
inconsistent(date(_,2,31)).
inconsistent(date(Y,2,29)):-
	%datime(datime(Y,_,_,_,_,_)),
	N is Y mod 4,
	N \= 0.

	
	
today(date(Y,M,D)):-
	datime(datime(Y,M,D,_,_,_)).

%returns tomorrows date
tomorrow(Date):-
	datime(datime(Y,M,D,_,_,_)),
	succ_date(date(Y,M,D),Date).


%returns yesterdays date
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

succ_weekday(Date,Weekday):-
	ground(Weekday),
	today(Today),
	succ_date_iterate(forward,Today,Weekday,Date1),
	%return Date1 or iterate another week)
	%(
	%  Date = Date1
	%;
	  succ_date(Date1,Date2),
	  succ_date_iterate(forward,Date2,Weekday,Date).

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

%returns the current time
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

%hours
hour(one, 1, 13).
hour(two, 2, 14).
hour(three, 3, 15).
hour(four, 4, 16).
hour(five, 5, 17).
hour(six, 6, 18).
hour(seven, 7, 19).
hour(eight, 8, 20).
hour(nine, 9, 21).
hour(ten, 10, 22).
hour(eleven, 11, 23).
hour(twelve, 12, 24).

day2nr([first],01).
day2nr([second],02).
day2nr([third],03).
day2nr([fourth],04).
day2nr([fifth],05).
day2nr([sixth],06).
day2nr([seventh],07).
day2nr([eight],08).
day2nr([ninth], 09).
day2nr([tenth],10).
day2nr([eleventh],11).
day2nr([twelfth],12).
day2nr([thirteenth], 13).
day2nr([fourteenth],14).
day2nr([fifteenth], 15).
day2nr([sixteenth],16).
day2nr([seventeenth], 17).
day2nr([eighteenth],18).
day2nr([nineteenth], 19).
day2nr([twentieth], 20).
day2nr([twentyfirst], 21).
day2nr([twentysecond],22).
day2nr([twentythird], 23).
day2nr([twentyfourth],24).
day2nr([twentyfifth], 25).
day2nr([twentysixth],26).
day2nr([twentyseventh], 27).
day2nr([twentyeigth],28).
day2nr([twentyninth], 29).
day2nr([thirtieth],30).
day2nr([thirtyfirst], 31).

month2nr( [january],1).
month2nr( [february], 2 ).
month2nr( [march] ,3).
month2nr( [april] , 4).
month2nr( [may] ,5).
month2nr( [june], 6).
month2nr( [july],7).
month2nr( [august] , 8).
month2nr( [september] ,9).
month2nr( [october] , 10).
month2nr( [november] ,11).
month2nr( [december] , 12).
	
%ampm_disamb(OrigTime, AMPM, CorrectTime)
ampm_disamb(StartTime, am, StartTime):-
	dayhalf(StartTime, am).
ampm_disamb(StartTime,pm, StartTime):-
	dayhalf(StartTime, pm).
ampm_disamb(StartTime, pm, SolvedTime):-
	name(StartTime, [H1, H2, M1, M2]),
	name(StartHour, [H1,H2]),
	hour(_, StartHour, PMHour),	
	name(PMHour, [PMH1, PMH2]),
	name(SolvedTime, [PMH1, PMH2, M1, M2]).
ampm_disamb(StartTime, pm, SolvedTime):-
	name(StartTime, [H1, M1, M2]),
	name(StartHour, [H1]),
	hour(_, StartHour, PMHour),	
	name(PMHour, [PMH1, PMH2]),
	name(SolvedTime, [PMH1, PMH2, M1, M2]).

%dayhalf(Time, AMPM) checks what dayhalf a time belongs to
dayhalf(T, pm):-
        sem_sort(T, time),
        name(T, [T1, T2,T3,T4]),
        name( Hour, [T1,T2]),
        Hour > 12, 
        Hour =< 23.

dayhalf(T, am):-
        sem_sort(T, time),
        name(T, [T1, T2, T3, T4]),
        name(Hour, [T1,T2]),
        Hour >= 0,
        Hour =< 12.

dayhalf(T,am):-
        sem_sort(T,time),
        name(T, [T1,T2,T3]),
        name(Hour, [T1]),
        Hour >=0,
        Hour =<12.
%%% 1,2,3,4,5 o'oclock is always considered afternoon
timeknowledge(T,pm):-
        sem_sort(T,time),
        name(T, [T1,T2,T3]),
        name(Hour, [T1]),
        Hour >=1,
        Hour =<5.
timeknowledge(T,am):-
        sem_sort(T,time),
        name(T, [T1,T2,T3,T4]),
        name(Hour, [T1,T2]),
        Hour >=10,
        Hour =<12.

	
%split(Atom, CharList)
split('', []).
split(Atom,[C|Cs]):-
	atom_concat(C,Rest,Atom),
	atom_length(C,1),
	split(Rest,Cs).

