
macro(past_tense,
      or(past, perfect)).
macro(not_past_tense,
      ( not(past), not(perfect) )).

macro(show_verb, show).
macro(show_verb, give).
macro(show_verb, list).

macro(date_or_on_date, date).
macro(date_or_on_date, on_date).
macro(date_or_on_date, in_time).
macro(date_or_on_date, for_time).

macro(next_or_the_next, next).
macro(next_or_the_next, the_next).

macro(last_or_the_last, last).
macro(last_or_the_last, the_last).

macro(null_or_the, null).
macro(null_or_the, the_sing).

macro(what_or_which, what).
macro(what_or_which, which).

macro(come_or_go, come).
macro(come_or_go, go).

macro(time_day_or_date, time).
macro(time_day_or_date, day).
macro(time_day_or_date, date).
macro(time_day_or_date, [and, time, date]).

macro(room_or_address, room).
macro(room_or_address, address).

macro(when_pattern, [time, when]).
macro(when_pattern, [time, term(what, null, [])]).
macro(when_pattern, term( @what_or_which, @time_day_or_date, _)).

macro(where_pattern, [loc, where]).
macro(where_pattern, [loc, term(what, null, [])]).
macro(where_pattern, term( @what_or_which, @room_or_address, _)).

macro(null_or_this, null).
macro(null_or_this, this).

%=========================================================
% Aggregate expressions

lf_boundary(term([ordinal, the_sing, N], meeting, _Body),
	    X^aggregate(nth_meeting(N), X)) :-
	number(N).

lf_boundary(term([[ordinal, the_sing, 1], N], meeting, _Body),
	    X^aggregate(first_n_meetings(N), X)) :-
	number(N).

lf_boundary(term( @next_or_the_next, meeting, _Body),
	    X^aggregate(next_n_meetings(1), X)).

lf_boundary(term([ @next_or_the_next, N], meeting, _Body),
	    X^aggregate(next_n_meetings(N), X)) :-
	number(N).

lf_boundary(term( @last_or_the_last, meeting, _Body),
	    X^aggregate(last_n_meetings(1), X)).

lf_boundary(term([ @last_or_the_last, N], meeting, _Body),
	    X^aggregate(last_n_meetings(N), X)) :-
	number(N).

%=========================================================
% Utterance types

% YNQ
lf_pattern([ynq, _],
	   (   not([show  | _]),
	       not([give  | _]),
	       not([list | _]),
	       not([agent, _]),
	       not(term(anyone, _, _)),
	       not(term(someone, _, _)),
	       not(term(_, person, _))
	   ),
	   utterance_type=ynq).
% WHQ
lf_pattern([whq, _],
	   utterance_type=whq).

lf_pattern([ @show_verb | _],
	   utterance_type=whq).

lf_pattern(@what_or_which,
	   utterance_type=whq).

lf_pattern(@when_pattern,
	   utterance_type=whq).

lf_pattern(@where_pattern,
	   utterance_type=whq).

lf_pattern([ynq, _],
	   or([ @show_verb | _],
	      [agent, _],
	      term(anyone, _, _),
	      term(someone, _, _),
	      term(_, person, _)
	     ),
	   utterance_type=whq).

%=========================================================
% Reference to current meeting

% Explicit "it"
lf_pattern(term(pro, it, []),
	   referent_from_context=meeting).

% Explicit "there"
lf_pattern([loc, there],
	   referent_from_context=meeting).

% Explicit "the/that meeting"
lf_pattern(term(the_sing, meeting, []),
	   referent_from_context=meeting).
lf_pattern(term(that, meeting, []),
	   referent_from_context=meeting).

% "Attend" with no object
lf_pattern([attend, _],
	   referent_from_context=meeting).

% "Participate" with no "in"
lf_pattern([participate, _],
	   not([in_meeting, _]),
	   referent_from_context=meeting).

% "Come/go" with no "to"
lf_pattern([ @come_or_go, _],
	   not([to_meeting, _]),
	   referent_from_context=meeting).

%=========================================================
% Reference to person

% "his"/"her"
lf_pattern([possessive, [term(pro, he, [])]],
	   referent_from_context=attendee).
lf_pattern([possessive, [term(pro, she, [])]],
	   referent_from_context=attendee).

%=========================================================
% Query objects (what type of object we're asking for)

% Who
lf_pattern(term(pro, who, []),
	   query_object=attendee).
lf_pattern([agent, _],
	   query_object=attendee).
lf_pattern(term(anyone, _, _),
	   query_object=attendee).
lf_pattern(term(someone, _, _),
	   query_object=attendee).
lf_pattern(term(_, person, _),
	   query_object=attendee).

% When
lf_pattern( @when_pattern,
	   % Condition
	   (not([start, _]), not([end, _])),  
	   query_object=when).
% When
lf_pattern( @time_day_or_date,
	   % Condition
	   ( @what_or_which, not([start, _]), not([end, _]) ),  
	   query_object=when).
% Start time
lf_pattern( @when_pattern,
	   % Condition
	   [start, _],                            
	   query_object=start_time).
% End time
lf_pattern( @when_pattern,
	   % Condition
	   [end, _],
	   query_object=end_time).
% Where
lf_pattern(@where_pattern,
	   query_object=where).
lf_pattern(location,
	   query_object=where).
lf_pattern(address,
	   (   not(term(name, _PersonName, _)),
	       not(term(pro, he, _)),
	       not(term(pro, she, _))
	   ),
	   query_object=where).

% Phone number
lf_pattern(phone_number,
	   query_object=phone_number).

% Address
lf_pattern(address,
	   term(name, PersonName, _),
	   query_object=attendee_address) :-
	is_person_name(PersonName).
lf_pattern(address,
	   or(term(pro, he, _), term(pro, she, _)),
	   query_object=attendee_address).

% Email address
lf_pattern(email_address,
	   query_object=email_address).


%=========================================================
% Dates

% Day before yesterday
lf_pattern([before_date, term(_, yesterday, _)],
	   term(_, day, _),
	   on_date=referent(day_before_yesterday)).

% Yesterday
lf_pattern(yesterday,
	   not([before_date, term(_, yesterday, _)]),
	   on_date=referent(yesterday)).

% Today
lf_pattern(today,
	   on_date=referent(today)).

% Tomorrow
lf_pattern(tomorrow,
	   not([after_date, term(_, tomorrow, _)]),
	   on_date=referent(tomorrow)).

% Day after tomorrow
lf_pattern([after_date, term(_, tomorrow, _)],
	   term(_, day, _),
	   on_date=referent(day_after_tomorrow)).

% on <Weekday>
lf_pattern([ @date_or_on_date, term( @null_or_this, DayOfWeek, _)],
	   % Condition
	   @not_past_tense,
	   on_date=referent(nearest_named_weekday_in_future(DayOfWeek))) :-
	is_dayofweek(DayOfWeek).
lf_pattern([ @date_or_on_date, term( @null_or_this, DayOfWeek, _)],
	   % Condition
	   @past_tense,
	   on_date=referent(nearest_named_weekday_in_past(DayOfWeek))) :-
	is_dayofweek(DayOfWeek).

% on <Weekday> morning/afternoon
lf_pattern([ @date_or_on_date, term( @null_or_this, _, [[nn, DayOfWeek]])],
	   % Condition
	   @not_past_tense,
	   on_date=referent(nearest_named_weekday_in_future(DayOfWeek))) :-
	is_dayofweek(DayOfWeek).
lf_pattern([ @date_or_on_date, term( @null_or_this, _, [[nn, DayOfWeek]])],
	   % Condition
	   @past_tense,
	   on_date=referent(nearest_named_weekday_in_past(DayOfWeek))) :-
	is_dayofweek(DayOfWeek).

% on <Date> 
lf_pattern([ @date_or_on_date, date(Year, Month, Day)],
	   on_date=referent(named_date(Day, Month, Year))) :-
	number(Year),
	number(Month),
	number(Day).
lf_pattern([ @date_or_on_date, date(unspecified, Month, Day)],
	   % Condition
	   @not_past_tense,
	   on_date=referent(nearest_named_date_in_future(Day, Month))) :-
	number(Month),
	number(Day).
lf_pattern([ @date_or_on_date, date(unspecified, Month, Day)],
	   % Condition
	   @past_tense,
	   on_date=referent(nearest_named_date_in_past(Day, Month))) :-
	number(Month),
	number(Day).

% next <Weekday>
lf_pattern([ @date_or_on_date, term(next, DayOfWeek, _)],
	   % Condition
	   @not_past_tense,
	   on_date=referent(next_weekday(DayOfWeek))).

%=========================================================
% Times

% At <Time>
lf_pattern([duration, time(H, M, PartOfDay)],
	   at_time=referent(time(H, M, PartOfDay))) :-
	number(H),
	number(M).
lf_pattern([at_time, time(H, M, PartOfDay)],
	   at_time=referent(time(H, M, PartOfDay))) :-
	number(H),
	number(M).
lf_pattern([duration, time(H, M, PartOfDay, [])],
	   at_time=referent(time(H, M, PartOfDay))) :-
	number(H),
	number(M).
lf_pattern([at_time, time(H, M, PartOfDay, [])],
	   at_time=referent(time(H, M, PartOfDay))) :-
	number(H),
	number(M).
% Around <Time>
lf_pattern([around_time, time(H, M, PartOfDay)],
	   in_interval=referent(approximate_time(H, M, PartOfDay))) :-
	number(H),
	number(M).
lf_pattern([around_time, time(H, M, PartOfDay, [])],
	   in_interval=referent(approximate_time(H, M, PartOfDay))) :-
	number(H),
	number(M).
% Morning
lf_pattern([term( @null_or_the, morning, _)],
	   in_interval=referent(morning)).
% Afternoon
lf_pattern([term( @null_or_the, afternoon, _)],
	   in_interval=referent(afternoon)).
% Evening
lf_pattern([term( @null_or_the, evening, _)],
	   in_interval=referent(evening)).

%=========================================================
% Intervals

% Past tense
lf_pattern(form(Tense, _),
	   tense_information=referent(past)) :-
	member(Tense, [past, [present, perfect]]).
% Present/future tense
lf_pattern(form(Tense, _),
	   tense_information=referent(future)) :-
	%member(Tense, [present, future]).
	member(Tense, [future]).
lf_pattern(come_up,
	   tense_information=referent(future)) :-
	member(Tense, [future]).
% This week
lf_pattern(term(this, week, _),
	   in_interval=referent(this_week)).
% This month
lf_pattern(term(this, month, _),
	   in_interval=referent(this_month)).
% This year
lf_pattern(term(this, year, _),
	   in_interval=referent(this_year)).
% Next week
lf_pattern(term(next, week, _),
	   in_interval=referent(next_week)).
% Next month
lf_pattern(term(next, month, _),
	   in_interval=referent(next_month)).
% Next year
lf_pattern(term(next, year, _),
	   in_interval=referent(next_year)).
% Last week
lf_pattern(term(last, week, _),
	   in_interval=referent(last_week)).
% Last month
lf_pattern(term(last, month, _),
	   in_interval=referent(last_month)).
% Last year
lf_pattern(term(last, year, _),
	   in_interval=referent(last_year)).
% The next week
lf_pattern(term(the_next, week, _),
	   in_interval=referent(the_next_week)).
% The next month
lf_pattern(term(the_next, month, _),
	   in_interval=referent(the_next_month)).
% The next year
lf_pattern(term(the_next, year, _),
	   in_interval=referent(the_next_year)).
% The last week
lf_pattern(term(the_last, week, _),
	   in_interval=referent(the_last_week)).
% The last month
lf_pattern(term(the_last, month, _),
	   in_interval=referent(the_last_month)).
% The last year
lf_pattern(term(the_last, year, _),
	   in_interval=referent(the_last_year)).
% The next N days
lf_pattern(term([ @next_or_the_next, N], day, _),
	   in_interval=referent(the_next_n_days(N))).
% The next N weeks
lf_pattern(term([ @next_or_the_next, N], week, _),
	   in_interval=referent(the_next_n_weeks(N))).
% The next N months
lf_pattern(term([ @next_or_the_next, N], month, _),
	   in_interval=referent(the_next_n_months(N))).
% The last N days
lf_pattern(term([ @last_or_the_last, N], day, _),
	   in_interval=referent(the_last_n_days(N))).
% The last N weeks
lf_pattern(term([ @last_or_the_last, N], week, _),
	   in_interval=referent(the_last_n_weeks(N))).
% The last N months
lf_pattern(term([ @last_or_the_last, N], month, _),
	   in_interval=referent(the_last_n_months(N))).
% N days ago
lf_pattern([ago, term(N, day, _)],
	   in_interval=referent(n_days_ago(N))) :-
	number(N).
% N weeks ago
lf_pattern([ago, term(N, week, _)],
	   in_interval=referent(n_weeks_ago(N))) :-
	number(N).
% N months ago
lf_pattern([ago, term(N, month, _)],
	   in_interval=referent(n_months_ago(N))) :-
	number(N).
% In June two thousand seven
lf_pattern([ @date_or_on_date, date(Year, Month, unspecified)],
	   in_interval=referent(named_month(Month, Year))) :-
	number(Year),
	number(Month).
% In June (past)
lf_pattern([ @date_or_on_date, date(unspecified, Month, unspecified)],
	   % Condition
	   @past_tense,
	   in_interval=referent(nearest_named_month_in_past(Month))) :-
	number(Month).
% In June (future)
lf_pattern([ @date_or_on_date, date(unspecified, Month, unspecified)],
	   % Condition
	   @not_past_tense,
	   in_interval=referent(nearest_named_month_in_future(Month))) :-
	number(Month).
% In two thousand seven
lf_pattern(date(Year, unspecified, unspecified),
	   in_interval=referent(named_year(Year))) :-
	number(Year).

%=========================================================
% Named entities

% Names of people are assumed to be attendees
lf_pattern(term(name, PersonName, _),
	   attendee=referent(person(PersonName))) :-
	is_person_name(PersonName).

% The problem with "I/me/my" is that it can occur in expressions like "show me a meeting"
% My meeting
%lf_pattern(term(_, meeting, _),
%	   local([possessive, [term(pro,i,[])]]),
%	   attendee=referent(i)).
lf_pattern([possessive, [term(pro,i,[])]],
	   attendee=referent(i)).

% I attend
lf_pattern([attend, term(pro,i,[]) | _],
	   attendee=referent(i)).
% I have a meeting
lf_pattern([have_meeting, term(pro,i,[]) | _],
	   attendee=referent(i)).

lf_pattern([at_loc, term(name, PlaceName, _)],
	   meeting_loc=PlaceName).

lf_pattern([from_loc, term(name, AffiliationName, _)],
	   affiliation=AffiliationName).
