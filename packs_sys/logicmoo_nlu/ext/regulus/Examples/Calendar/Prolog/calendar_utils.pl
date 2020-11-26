
:- module(calendar_utils,
	[is_person_name/1,
	 today_time/1,
	 today/1,
	 next_occurrence_of_date/4,
	 last_occurrence_of_date/4,
	 next_weekday_after_date/3,
	 previous_weekday_before_date/3,
	 interval_for_date_pair/3,
	 interval_from_now_to_end_of_date/2,
	 interval_for_month/3,
	 interval_for_year/2,
	 dayofweek_for_date/2,
	 next_date/2,
	 previous_date/2,
	 end_of_n_days_from_date/3,
	 start_of_n_days_before_date/3,
	 is_dayofweek/1,
	 is_time_of_day/1,
	 is_timeperiod/1,
	 previous_dayofweek/2,
	 next_dayofweek/2,
	 beginning_of_time/1,
	 end_of_time/1,
	 datime_to_number/2,
	 intervals_intersect/2,
	 earlier_or_same_datime/2,
	 number_of_days_in_month/3]
    ).

%======================================================================

:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module('$REGULUS/PrologLib/utilities').

%======================================================================

% person(pierrette_bouillon, pierrette, bouillon, geneva, "+41 22 379 8679", "Pierrette.Bouillon@issco.unige.ch").

is_person_name(PersonName) :-
	database:person(FullName, FirstName, Surname, _Loc, _Phone, _Email),
	member(PersonName, [FullName, FirstName, Surname]).

%-------------------------------------------------------------------------

% Low-level calendar manipulation

today_time(Datime) :-
	(   get_notional_time(Datime) ->
	    true ;
	    datime(Datime)
	).

today(Today) :-
	(   get_notional_time(Datime) ->
	    true ;
	    datime(Datime)
	),
	Datime = datime(Year0, Month0, Day0, _Hour0, _Minute0, _Second0),
	Today = datime(Year0, Month0, Day0, 0, 0, 0).

next_occurrence_of_date(FromDate0, Day, Month, Date) :-
	FromDate0 = datime(Year0, Month0, Day0, _, _, _),
	FromDate = datime(Year0, Month0, Day0, 0, 0, 0),
	DateThisYear = datime(Year0, Month, Day, 0, 0, 0),
	Year1 is Year0 + 1,
	DateNextYear = datime(Year1, Month, Day, 0, 0, 0),
	(   FromDate = DateThisYear ->
	    Date = DateThisYear
	;
	    earlier_datime(FromDate, DateThisYear) ->
	    Date = DateThisYear
	;
	    Date = DateNextYear
	).

last_occurrence_of_date(FromDate0, Day, Month, Date) :-
	FromDate0 = datime(Year0, Month0, Day0, _, _, _),
	FromDate = datime(Year0, Month0, Day0, 0, 0, 0),
	DateThisYear = datime(Year0, Month, Day, 0, 0, 0),
	Year1 is Year0 - 1,
	DateLastYear = datime(Year1, Month, Day, 0, 0, 0),
	(   earlier_datime(DateThisYear, FromDate) ->
	    Date = DateThisYear ;
	    Date = DateLastYear
	).

next_weekday_after_date(Date, DayOfWeek, NextWeekday) :-
	is_dayofweek(DayOfWeek),
	dayofweek_for_date(Date, DayOfWeek0),
	next_dayofweek_after_date1(Date, DayOfWeek0, DayOfWeek, NextWeekday).

next_dayofweek_after_date1(Date, DayOfWeek0, DayOfWeek, NextWeekday) :-
	next_date(Date, Date1),
	next_dayofweek(DayOfWeek0, DayOfWeek1),
	!,
	(   DayOfWeek1 = DayOfWeek ->
	    NextWeekday = Date1 ;
	    next_dayofweek_after_date1(Date1, DayOfWeek1, DayOfWeek, NextWeekday)
	).

previous_weekday_before_date(Date, DayOfWeek, PreviousWeekday) :-
	is_dayofweek(DayOfWeek),
	dayofweek_for_date(Date, DayOfWeek0),
	previous_dayofweek_before_date1(Date, DayOfWeek0, DayOfWeek, PreviousWeekday).

previous_dayofweek_before_date1(Date, DayOfWeek0, DayOfWeek, PreviousWeekday) :-
	previous_date(Date, Date1),
	previous_dayofweek(DayOfWeek0, DayOfWeek1),
	!,
	(   DayOfWeek1 = DayOfWeek ->
	    PreviousWeekday = Date1 ;
	    previous_dayofweek_before_date1(Date1, DayOfWeek1, DayOfWeek, PreviousWeekday)
	).

interval_for_date_pair(Date1, Date2, Interval) :-
	Date1 = datime(Year1, Month1, Day1, _, _, _),
	Date2 = datime(Year2, Month2, Day2, _, _, _),
	StartDate1 = datime(Year1, Month1, Day1, 0, 0, 0),
	EndDate2 = datime(Year2, Month2, Day2, 23, 59, 59),
	Interval = interval(StartDate1, EndDate2).

interval_from_now_to_end_of_date(Date2, Interval) :-
	today_time(Now),
	Date2 = datime(Year2, Month2, Day2, _, _, _),
	EndDate2 = datime(Year2, Month2, Day2, 23, 59, 59),
	Interval = interval(Now, EndDate2).

interval_for_month(Year, Month, Interval) :-
	number_of_days_in_month(Month, Year, LastDayNumber),
	StartOfMonth = datime(Year, Month, 1, 0, 0, 0),
	EndOfMonth = datime(Year, Month, LastDayNumber, 23, 59, 59),
	Interval = interval(StartOfMonth, EndOfMonth).

interval_for_year(Year, Interval) :-
	StartOfYear = datime(Year, 1, 1, 0, 0, 0),
	EndOfYear = datime(Year, 12, 31, 23, 59, 59),
	Interval = interval(StartOfYear, EndOfYear).

% This is a very stupid way to work it out, but OK for now.
% Would be easy to cache a calender.
dayofweek_for_date(Date0, DayOfWeek) :-
	Date0 = datime(Y, Mo, D, _H, _Mi, _S),
	Date = datime(Y, Mo, D, 0, 0, 0),
	zero_day(ZeroDay, DayOfWeek0),
	(   ZeroDay = Date ->
	    DayOfWeek = DayOfWeek0
	;
	    earlier_datime(ZeroDay, Date) ->
	    get_dayofweek_for_date_counting_forwards(ZeroDay, DayOfWeek0, Date, DayOfWeek)
	;
	    earlier_datime(Date, ZeroDay),
	    get_dayofweek_for_date_counting_backwards(ZeroDay, DayOfWeek0, Date, DayOfWeek)
	),
	!.
dayofweek_for_date(Date, DayOfWeek) :-
	format('~N*** Error: bad call: ~w~n', [dayofweek_for_date(Date, DayOfWeek)]),
	!.

zero_day(datime(2007, 7, 7, 0, 0, 0), saturday).

get_dayofweek_for_date_counting_forwards(Date, DayOfWeek, Date, DayOfWeek) :-
	!.
get_dayofweek_for_date_counting_forwards(Date0, DayOfWeek0, Date, DayOfWeek) :-
	next_date(Date0, Date1),
	next_dayofweek(DayOfWeek0, DayOfWeek1),
	!,
	get_dayofweek_for_date_counting_forwards(Date1, DayOfWeek1, Date, DayOfWeek).

get_dayofweek_for_date_counting_backwards(Date, DayOfWeek, Date, DayOfWeek) :-
	!.
get_dayofweek_for_date_counting_backwards(Date0, DayOfWeek0, Date, DayOfWeek) :-
	previous_date(Date0, Date1),
	previous_dayofweek(DayOfWeek0, DayOfWeek1),
	!,
	get_dayofweek_for_date_counting_backwards(Date1, DayOfWeek1, Date, DayOfWeek).

next_date(datime(Year0, Month0, Day0, _Hour0, _Minute0, _Second0),
	  datime(Year1, Month1, Day1, 0, 0, 0)) :-
	number_of_days_in_month(Month0, Year0, NDaysInMonth),
	(   Day0 < NDaysInMonth ->
	    Year1 = Year0,
	    Month1 = Month0,
	    Day1 is Day0 + 1 ;

	    Month0 \== 12 ->
	    Year1 = Year0,
	    Month1 is Month0 + 1,
	    Day1 = 1 ;

	    Year1 is Year0 + 1,
	    Month1 is 1,
	    Day1 is 1
	).

previous_date(datime(Year0, Month0, Day0, _Hour0, _Minute0, _Second0),
	      datime(Year1, Month1, Day1, 0, 0, 0)) :-
	(   Day0 > 1 ->
	    Year1 = Year0,
	    Month1 = Month0,
	    Day1 is Day0 - 1 ;

	    Month0 \== 1 ->
	    Year1 = Year0,
	    Month1 is Month0 - 1,
	    number_of_days_in_month(Month1, Year0, NDaysInMonth),
	    Day1 = NDaysInMonth ;

	    Year1 is Year0 - 1,
	    Month1 is 12,
	    Day1 is 31
	).

end_of_n_days_from_date(0, Date, EndOfPeriod) :-
	Date = datime(Year, Month, Day, _, _, _),
	EndOfPeriod = datime(Year, Month, Day, 23, 59, 59),
	!.
end_of_n_days_from_date(N, Date, EndOfPeriod) :-
	N > 0,
	next_date(Date, Date1),
	N1 is N - 1,
	!,
	end_of_n_days_from_date(N1, Date1, EndOfPeriod).

start_of_n_days_before_date(0, Date, StartOfPeriod) :-
	Date = datime(Year, Month, Day, _, _, _),
	StartOfPeriod = datime(Year, Month, Day, 0, 0, 0),
	!.
start_of_n_days_before_date(N, Date, StartOfPeriod) :-
	N > 0,
	previous_date(Date, Date1),
	N1 is N - 1,
	!,
	start_of_n_days_before_date(N1, Date1, StartOfPeriod).

earlier_or_same_datime(Datime, Datime1) :-
	(   Datime = Datime1 ;
	    earlier_datime(Datime, Datime1)
	).

intervals_intersect(Interval1, Interval2) :-
	term_contains_subterm([Interval1, Interval2], generic),
	!,
	make_interval_generic(Interval1, Interval1Generic),
	make_interval_generic(Interval2, Interval2Generic),
	intervals_intersect(Interval1Generic, Interval2Generic).
intervals_intersect(interval(Start1, End1), interval(Start2, End2)) :-
	%\+ earlier_datime(End1, Start2),
	%\+ earlier_datime(End2, Start1).
	(   earlier_datime(Start1, Start2) ->
	    LaterStart = Start2 ;
	    LaterStart = Start1
	),
	(   earlier_datime(End1, End2) ->
	    EarlierEnd = End1 ;
	    EarlierEnd = End2
	),
	earlier_datime(LaterStart, EarlierEnd),
	dif(LaterStart, EarlierEnd).

make_interval_generic(interval(Start, End), interval(Start1, End1)) :-
	make_datime_generic(Start, Start1),
	make_datime_generic(End, End1),
	!.
make_interval_generic(Interval, IntervalGeneric) :-
	format('~N*** Error: bad call: ~w~n', [make_interval_generic(Interval, IntervalGeneric)]),
	!.

make_datime_generic(datime(_DOW, Year, Month, _Day, H, M, S),
		    Result) :-
	make_datime_generic(datime(Year, Month, _Day, H, M, S),
		    Result),
	!.
make_datime_generic(datime(_Year, _Month, _Day, H, M, S),
		    datime(2000, 0, 0, H, M, S)) :-
	!.
make_datime_generic(Datime, Datime1) :-
	format('~N*** Error: bad call: ~w~n', [make_datime_generic(Datime, Datime1)]),
	!.

is_timeperiod(day).
is_timeperiod(week).
is_timeperiod(month).
is_timeperiod(year).

is_time_of_day(morning).
is_time_of_day(afternoon).
is_time_of_day(evening).

is_dayofweek(monday).
is_dayofweek(tuesday).
is_dayofweek(wednesday).
is_dayofweek(thursday).
is_dayofweek(friday).
is_dayofweek(saturday).
is_dayofweek(sunday).

previous_dayofweek(Day, Day1) :-
	next_dayofweek(Day1, Day).

next_dayofweek(monday, tuesday).
next_dayofweek(tuesday, wednesday).
next_dayofweek(wednesday, thursday).
next_dayofweek(thursday, friday).
next_dayofweek(friday, saturday).
next_dayofweek(saturday, sunday).
next_dayofweek(sunday, monday).

/*
Thirty days hath September
April June and November
All the rest have thirty one
Excepting February alone
Which has twenty eight days clear
And twenty nine in each leap year
*/

% January
number_of_days_in_month(1, _, 31).
% February
number_of_days_in_month(2, Year, N) :-
	(   is_leap_year(Year) ->
	    N = 29 ;
	    N = 28
	).
% March
number_of_days_in_month(3, _, 31).
% April
number_of_days_in_month(4, _, 30).
% May
number_of_days_in_month(5, _, 31).
% June
number_of_days_in_month(6, _, 30).
% July
number_of_days_in_month(7, _, 31).
% August
number_of_days_in_month(8, _, 31).
% September
number_of_days_in_month(9, _, 30).
% October
number_of_days_in_month(10, _, 31).
% November
number_of_days_in_month(11, _, 30).
% December
number_of_days_in_month(12, _, 31).

is_leap_year(Year) :-
	0 is Year mod 4,
	\+ (( 0 is Year mod 100 )).

beginning_of_time(datime(1980, 0, 0, 0, 0, 0)).
end_of_time(datime(2100, 0, 0, 0, 0, 0)).

% These numbers just used for ordering
datime_to_number(Datime, Number) :-
	Datime = datime(Year, Month, Day, Hour, Minute, _Second),
	Number is Minute + 60 * ( Hour + 24 * ( Day + 30 * ( Month + 12 * ( Year - 2000 ) ) ) ).
