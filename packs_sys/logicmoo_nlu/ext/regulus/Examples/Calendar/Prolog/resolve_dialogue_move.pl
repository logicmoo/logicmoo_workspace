
:- module(resolve_dialogue_move,
	[resolve_dialogue_move/3]
    ).

%======================================================================

:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module('$REGULUS/Examples/Calendar/Prolog/simplify_dialogue_move').
:- use_module('$REGULUS/Examples/Calendar/Prolog/dialogue_utils').
:- use_module('$REGULUS/Examples/Calendar/Prolog/calendar_utils').
 
%======================================================================

resolve_dialogue_move(PatternIn, Context, PatternOut) :-
	resolve_referents(PatternIn, PatternNext1, Errors0-[]),
	sort(Errors0, Errors),
	(   Errors \== [] ->
	    PatternOut = Errors
	;
	    add_referent_from_context(PatternNext1, Context, PatternNext2),
	    simplify_dialogue_move(PatternNext2, PatternOut)
	),
	!.
resolve_dialogue_move(PatternIn, Context, PatternOut) :-
	format('~N*** Error: bad call: ~w~n',
	       [resolve_dialogue_move(PatternIn, Context, PatternOut)]),
	fail.

%======================================================================

resolve_referents([], [], Errors-Errors).
resolve_referents([F | R], [F1 | R1], ErrorsIn-ErrorsOut) :-
	resolve_referents_in_element(F, F1, ErrorsIn-ErrorsNext),
	!,
	resolve_referents(R, R1, ErrorsNext-ErrorsOut).

resolve_referents_in_element(aggregate(AggregateType, Body),
			     aggregate(AggregateType, ResolvedBody),
			     ErrorsIn-ErrorsOut) :-
	resolve_referents(Body, ResolvedBody, ErrorsIn-ErrorsOut).
resolve_referents_in_element(Feat=referent(RefDescription),
			     Feat=Referent,
			     ErrorsIn-ErrorsOut) :-
	find_all_possible_resolutions_for_referent(RefDescription, PossibleReferents, N),
	resolve_referents_in_element1(N, RefDescription, PossibleReferents, Referent, Error),
	(   Error = '*no_error*' ->
	    ErrorsIn = ErrorsOut ;

	    otherwise ->
	    ErrorsIn = [error(Error) | ErrorsOut]
	),
	!.
resolve_referents_in_element(FeatValue,
			     FeatValue,
			     ErrorsIn-ErrorsIn) :-
	!.
resolve_referents_in_element(F, F1, Errors) :-
	format('~N*** Error: bad call: ~w~n',
	       [resolve_referents_in_element(F, F1, Errors)]),
	fail.

find_all_possible_resolutions_for_referent(RefDescription, PossibleReferents, N) :-
	findall(PossibleReferent,
		resolve_referent(RefDescription, PossibleReferent),
		PossibleReferents0),
	sort(PossibleReferents0, PossibleReferents),
	length(PossibleReferents, N),
	!.

resolve_referents_in_element1(0, RefDescription, [], no_referent,
			      no_referent_for(RefDescription)) :-
	!.
resolve_referents_in_element1(1, _RefDescription, [Referent], Referent,
			      '*no_error*') :-
	!.
resolve_referents_in_element1(N, RefDescription, PossibleReferents, no_referent,
			      several_referents_for(RefDescription, PossibleReferents)) :-
	N > 1,
	!.

%======================================================================

add_referent_from_context(PatternIn, Context, PatternOut) :-
	needs_referent_information(PatternIn, ReferentType),
	get_referent_of_type(Context, ReferentType, Referent),
	add_referent_information_to_pattern(ReferentType, Referent, PatternIn, PatternNext),
	remove_referent_from_context_items(PatternNext, PatternOut),
	!.
add_referent_from_context(Pattern, _Context, Pattern1) :-
	Pattern = Pattern1.

needs_referent_information(Pattern, attendee) :-
	member(referent_from_context=attendee, Pattern),
	!.
needs_referent_information(Pattern, meeting) :-
	member(referent_from_context=meeting, Pattern),
	!.
needs_referent_information(Pattern, meeting) :-
	non_constraining_pattern(Pattern).

add_referent_information_to_pattern(meeting, Referent, Pattern, Pattern1) :-
	append([meeting=Referent], Pattern, Pattern1),
	!.
add_referent_information_to_pattern(attendee, Referent, Pattern, Pattern1) :-
	append([attendee=Referent], Pattern, Pattern1),
	!.
add_referent_information_to_pattern(ReferentType, Referent, Pattern, Pattern1) :-
	format('~N*** Error: bad call: ~w~n',
	       [add_referent_information_to_pattern(ReferentType, Referent, Pattern, Pattern1)]),
	fail.

get_referent_of_type(Context, ReferentType, Referent) :-
	member(referents=Referents, Context),
	findall(ID,
		member(record(ReferentType, ID), Referents),
		ReferentsOfGivenType),
	Referent = in_list(ReferentsOfGivenType).
	%length(ReferentsOfGivenType, N),
	%(   N = 1 ->
	%    ReferentsOfGivenType = [Referent]
	%;
	%    N > 1 ->
	%    Referent = in_list(ReferentsOfGivenType)
	%).

remove_referent_from_context_items([], []).
remove_referent_from_context_items([referent_from_context=_ | R], R1) :-
	!,
	remove_referent_from_context_items(R, R1).
remove_referent_from_context_items([F | R], [F | R1]) :-
	!,
	remove_referent_from_context_items(R, R1).


%======================================================================

resolve_referent(day_before_yesterday, DayBeforeYesterday) :-
	today(Today),
	previous_date(Today, Yesterday),
	previous_date(Yesterday, DayBeforeYesterday).

resolve_referent(yesterday, Yesterday) :-
	today(Today),
	previous_date(Today, Yesterday).

resolve_referent(today, Today) :-
	today(Today).

resolve_referent(tomorrow, Tomorrow) :-
	today(Today),
	next_date(Today, Tomorrow).

resolve_referent(day_after_tomorrow, DayAfterTomorrow) :-
	today(Today),
	next_date(Today, Tomorrow),
	next_date(Tomorrow, DayAfterTomorrow).

resolve_referent(nearest_named_weekday_in_future(DayOfWeek), Date) :-
	is_dayofweek(DayOfWeek),
	today(Today),
	next_weekday_after_date(Today, DayOfWeek, Date).

resolve_referent(nearest_named_weekday_in_past(DayOfWeek), Date) :-
	is_dayofweek(DayOfWeek),
	today(Today),
	previous_weekday_before_date(Today, DayOfWeek, Date).

resolve_referent(named_date(Day, Month, Year), Date) :-
	number(Day),
	number(Month),
	number(Year),
	Date = datime(Year, Month, Day, 0, 0, 0).

resolve_referent(nearest_named_date_in_future(Day, Month), Date) :-
	number(Day),
	number(Month),
	today(Today),
	next_occurrence_of_date(Today, Day, Month, Date).

resolve_referent(nearest_named_date_in_past(Day, Month), Date) :-
	number(Day),
	number(Month),
	today(Today),
	last_occurrence_of_date(Today, Day, Month, Date).

% "next Monday" can be the following Monday 
resolve_referent(next_weekday(DayOfWeek), Date) :-
	is_dayofweek(DayOfWeek),
	today(Today),
	next_weekday_after_date(Today, DayOfWeek, Date).
% "next Monday" can be the Monday after the following Monday, as long as today isn't also Monday
resolve_referent(next_weekday(DayOfWeek), Date) :-
	is_dayofweek(DayOfWeek),
	today(Today),
	dayofweek_for_date(Today, DayOfWeekForToday),
	DayOfWeekForToday \== DayOfWeek,
	next_weekday_after_date(Today, DayOfWeek, FollowingDayOfWeek),
	next_weekday_after_date(FollowingDayOfWeek, DayOfWeek, Date).

resolve_referent(past, interval(Start, End)) :-
	today_time(End),
	beginning_of_time(Start).

resolve_referent(future, interval(Start, End)) :-
	today_time(Start),
	end_of_time(End).

resolve_referent(this_week, Interval) :-
	today(Today),
	dayofweek_for_date(Today, DayofweekToday),
	(   DayofweekToday = monday ->
	    CurrentMonday = Today
	;
	    otherwise ->
	    previous_weekday_before_date(Today, monday, CurrentMonday)
	),
	next_weekday_after_date(CurrentMonday, sunday, FollowingSunday),
	interval_for_date_pair(CurrentMonday, FollowingSunday, Interval).

resolve_referent(next_week, Interval) :-
	today(Today),
	next_weekday_after_date(Today, monday, NextMonday),
	next_weekday_after_date(NextMonday, sunday, FollowingSunday),
	interval_for_date_pair(NextMonday, FollowingSunday, Interval).

resolve_referent(to_end_of_next_week, Interval) :-
	today(Today),
	next_weekday_after_date(Today, monday, NextMonday),
	next_weekday_after_date(NextMonday, sunday, FollowingSunday),
	interval_from_now_to_end_of_date(FollowingSunday, Interval).

resolve_referent(week_after_next, Interval) :-
	today(Today),
	next_weekday_after_date(Today, monday, NextMonday),
	next_weekday_after_date(NextMonday, monday, NextNextMonday),
	next_weekday_after_date(NextNextMonday, sunday, FollowingSunday),
	interval_for_date_pair(NextNextMonday, FollowingSunday, Interval).

resolve_referent(to_end_of_week_after_next, Interval) :-
	today(Today),
	next_weekday_after_date(Today, monday, NextMonday),
	next_weekday_after_date(NextMonday, monday, NextNextMonday),
	next_weekday_after_date(NextNextMonday, sunday, FollowingSunday),
	interval_from_now_to_end_of_date(FollowingSunday, Interval).

resolve_referent(last_week, Interval) :-
	today(Today),
	previous_weekday_before_date(Today, sunday, LastSunday),
	previous_weekday_before_date(LastSunday, monday, PreviousMonday),
	interval_for_date_pair(PreviousMonday, LastSunday, Interval).

resolve_referent(this_month, Interval) :-
	today(Today),
	Today = datime(Year, Month, _, _, _, _),
	interval_for_month(Year, Month, Interval).

resolve_referent(next_month, Interval) :-
	today(Today),
	Today = datime(Year, Month, _, _, _, _),
	(   Month = 12 ->
	    Month1 = 1,
	    Year1 is Year + 1
	;
	    otherwise ->
	    Month1 is Month + 1,
	    Year1 is Year
	),
	interval_for_month(Year1, Month1, Interval).

resolve_referent(to_end_of_next_month, Interval) :-
	today(Today),
	Today = datime(Year, Month, _, _, _, _),
	(   Month = 12 ->
	    Month1 = 1,
	    Year1 is Year + 1
	;
	    otherwise ->
	    Month1 is Month + 1,
	    Year1 is Year
	),
	interval_for_month(Year1, Month1, interval(_MonthStart, MonthEnd)),
	interval_from_now_to_end_of_date(MonthEnd, Interval).

resolve_referent(month_after_next, Interval) :-
	today(Today),
	Today = datime(Year, Month, _, _, _, _),
	(   Month = 11 ->
	    Month1 = 1,
	    Year1 is Year + 1
	;
	    Month = 12 ->
	    Month1 = 2,
	    Year1 is Year + 1
	;
	    otherwise ->
	    Month1 is Month + 2,
	    Year1 is Year
	),
	interval_for_month(Year1, Month1, Interval).

resolve_referent(to_end_of_month_after_next, Interval) :-
	today(Today),
	Today = datime(Year, Month, _, _, _, _),
	(   Month = 11 ->
	    Month1 = 1,
	    Year1 is Year + 1
	;
	    Month = 12 ->
	    Month1 = 2,
	    Year1 is Year + 1
	;
	    otherwise ->
	    Month1 is Month + 2,
	    Year1 is Year
	),
	interval_for_month(Year1, Month1, interval(_MonthStart, MonthEnd)),
	interval_from_now_to_end_of_date(MonthEnd, Interval).

resolve_referent(last_month, Interval) :-
	today(Today),
	Today = datime(Year, Month, _, _, _, _),
	(   Month = 1 ->
	    Month1 = 12,
	    Year1 is Year - 1
	;
	    otherwise ->
	    Month1 is Month - 1,
	    Year1 is Year
	),
	interval_for_month(Year1, Month1, Interval).

resolve_referent(named_year(Year), Interval) :-
	interval_for_year(Year, Interval).

resolve_referent(this_year, Interval) :-
	today(Today),
	Today = datime(Year, _, _, _, _, _),
	interval_for_year(Year, Interval).

resolve_referent(next_year, Interval) :-
	today(Today),
	Today = datime(Year, _, _, _, _, _),
	Year1 is Year + 1,
	interval_for_year(Year1, Interval).

resolve_referent(to_end_of_next_year, Interval) :-	
	today(Today),
	Today = datime(Year, _, _, _, _, _),
	Year1 is Year + 1,
	interval_for_year(Year1, interval(_YearStart, YearEnd)),
	interval_from_now_to_end_of_date(YearEnd, Interval).

resolve_referent(last_year, Interval) :-
	today(Today),
	Today = datime(Year, _, _, _, _, _),
	Year1 is Year - 1,
	interval_for_year(Year1, Interval).

resolve_referent(named_month(Month, Year), Interval) :-
	interval_for_month(Year, Month, Interval).    

resolve_referent(nearest_named_month_in_past(Month), Interval) :-
	today(Today),
	Today = datime(ThisYear, ThisMonth, _, _, _, _),
	(   ThisMonth >= Month ->
	    Year = ThisYear
	;
	    Year is ThisYear - 1
	),
	interval_for_month(Year, Month, Interval).    

resolve_referent(nearest_named_month_in_future(Month), Interval) :-
	today(Today),
	Today = datime(ThisYear, ThisMonth, _, _, _, _),
	(   ThisMonth =< Month ->
	    Year = ThisYear
	;
	    Year is ThisYear + 1
	),
	interval_for_month(Year, Month, Interval).

resolve_referent(morning, Interval) :-
	StartTime = datime(generic, generic, generic, 6, 0, 0),
	EndTime = datime(generic, generic, generic, 12, 0, 0),
	Interval = interval(StartTime, EndTime).

resolve_referent(afternoon, Interval) :-
	StartTime = datime(generic, generic, generic, 12, 0, 0),
	EndTime = datime(generic, generic, generic, 19, 0, 0),
	Interval = interval(StartTime, EndTime).

resolve_referent(the_next_n_days(N), interval(Today, EndOfPeriod)) :-
	today_time(Today),
	end_of_n_days_from_date(N, Today, EndOfPeriod).

resolve_referent(the_last_n_days(N), interval(StartOfPeriod, Today)) :-
	today_time(Today),
	start_of_n_days_before_date(N, Today, StartOfPeriod).

resolve_referent(n_days_ago(N), interval(StartOfPeriod, EndOfPeriod)) :-
	today_time(Today),
	start_of_n_days_before_date(N, Today, StartOfPeriod),
	end_of_n_days_from_date(0, StartOfPeriod, EndOfPeriod).

resolve_referent(n_weeks_ago(N), interval(StartOfPeriod, EndOfPeriod)) :-
	today_time(Today),
	NDays is (7 * N) + 3,
	start_of_n_days_before_date(NDays, Today, StartOfPeriod),
	end_of_n_days_from_date(6, StartOfPeriod, EndOfPeriod).

resolve_referent(n_months_ago(N), interval(StartOfPeriod, EndOfPeriod)) :-
	today_time(Today),
	NDays is (30 * N) + 15,
	start_of_n_days_before_date(NDays, Today, StartOfPeriod),
	end_of_n_days_from_date(30, StartOfPeriod, EndOfPeriod).

resolve_referent(the_next_day, Interval) :-
	resolve_referent(the_next_n_days(1), Interval).

resolve_referent(the_next_week, Interval) :-
	resolve_referent(the_next_n_days(7), Interval).

resolve_referent(the_next_month, Interval) :-
	resolve_referent(the_next_n_days(30), Interval).

resolve_referent(the_next_year, Interval) :-
	resolve_referent(the_next_n_days(365), Interval).

resolve_referent(the_next_n_weeks(N), Interval) :-
	N1 is 7 * N,
	resolve_referent(the_next_n_days(N1), Interval).

resolve_referent(the_next_n_months(N), Interval) :-
	N1 is 30 * N,
	resolve_referent(the_next_n_days(N1), Interval).

resolve_referent(the_next_n_years(N), Interval) :-
	N1 is 365 * N,
	resolve_referent(the_next_n_days(N1), Interval).

resolve_referent(the_last_day, Interval) :-
	resolve_referent(the_last_n_days(1), Interval).

resolve_referent(the_last_week, Interval) :-
	resolve_referent(the_last_n_days(7), Interval).

resolve_referent(the_last_month, Interval) :-
	resolve_referent(the_last_n_days(30), Interval).

resolve_referent(the_last_year, Interval) :-
	resolve_referent(the_last_n_days(365), Interval).

resolve_referent(the_last_n_weeks(N), Interval) :-
	N1 is 7 * N,
	resolve_referent(the_last_n_days(N1), Interval).

resolve_referent(the_last_n_months(N), Interval) :-
	N1 is 30 * N,
	resolve_referent(the_last_n_days(N1), Interval).

resolve_referent(the_last_n_years(N), Interval) :-
	N1 is 365 * N,
	resolve_referent(the_last_n_days(N1), Interval).

resolve_referent(time(H, M, morning), datime(generic, generic, generic, H, M, 0)).

resolve_referent(time(H, M, afternoon), datime(generic, generic, generic, H1, M, 0)) :-
	H1 is H + 12.

resolve_referent(time(H, M, any), datime(generic, generic, generic, H, M, 0)) :-
	H >= 7.

resolve_referent(time(H, M, any), datime(generic, generic, generic, H1, M, 0)) :-
	H < 7,
	H1 is H + 12.

resolve_referent(approximate_time(H, M, PartOfDay), interval(StartTime, EndTime)) :-
	resolve_referent(time(H, M, PartOfDay), datime(generic, generic, generic, H1, M1, S1)),
	(   H1 > 1 ->
	    HStart is H1 - 1,
	    StartTime = datime(generic, generic, generic, HStart, M1, S1)
	;
	    otherwise ->
	    StartTime = datime(generic, generic, generic, 0, 0, 0)
	),
	(   H1 < 23 ->
	    HEnd is H1 + 1,
	    EndTime = datime(generic, generic, generic, HEnd, M1, S1)
	;
	    otherwise ->
	    EndTime = datime(generic, generic, generic, 23, 59, 59)
	).

resolve_referent(person(PersonName), Person) :-
	name_for_attendee(Person, PersonName).

resolve_referent(i, Person) :-
	get_notional_speaker(PersonName),
	resolve_referent(person(PersonName), Person).

