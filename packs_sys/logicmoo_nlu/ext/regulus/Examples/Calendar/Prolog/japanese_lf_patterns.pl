
macro(past_tense,
      past).
macro(not_past_tense,
      not(past)).

macro(when_pattern, itsu).
macro(when_pattern, term(null, nanji, [])).

macro(where_pattern, doko).
macro(where_pattern, basyo).
macro(where_pattern, term(dono, heya, [])).

macro(who_pattern, dare).
macro(who_pattern, dareka).

macro(what_pattern, nani).
macro(what_pattern, domna).
macro(what_pattern, dono).
macro(what_pattern, ikutsu).
macro(what_pattern, nan_bon).

macro(next_pattern, term(null, tsugi, [])).
macro(next_pattern, term(null, kondo, [])).
macro(next_pattern, term(null, jikai, [])).
macro(next_pattern, term(null, ichiban_saisyo, [])).

macro(last_pattern, term(null, saigo, [])).
macro(last_pattern, term(null, ichiban_saikin, [])).

macro(meeting, kaigi).
macro(meeting, miitingu).

macro(attend, syusseki_suru).
macro(attend, kuru).
macro(attend, deru).

macro(morning_pattern,
      gozen).
macro(morning_pattern,
      asa).
macro(morning_pattern,
      kesa).
macro(afternoon_pattern,
      gogo).

macro(gen_ni_or_temporal,
      gen).
macro(gen_ni_or_temporal,
      ni).
macro(gen_ni_or_temporal,
      temporal).

macro(gen_or_ni,
      gen).
macro(gen_or_ni,
      ni).

macro(saikin_koko_or_kako,
      koko).
macro(saikin_koko_or_kako,
      kako).
macro(saikin_koko_or_kako,
      saikin).

macro(kagetsukan,
      kagetsukan).
macro(kagetsukan,
      kagetsu).

%=========================================================
% Aggregate expressions

lf_boundary(term(null, @meeting, List),
	    X^aggregate(next_n_meetings(1), X)) :-
	member([gen, @next_pattern], List).

lf_boundary(term(null, @meeting, List),
	    X^aggregate(last_n_meetings(1), X)) :-
	member([gen, @last_pattern], List).

% Hacky rules to get around bad attachments
lf_boundary(term(null, _, List),
	    X^aggregate(next_n_meetings(1), X)) :-
	member([gen, @next_pattern], List),
	member([gen, term(null, @meeting, [])], List).

lf_boundary(term(null, _, List),
	    X^aggregate(last_n_meetings(1), X)) :-
	member([gen, @last_pattern], List),
	member([gen, term(null, @meeting, List)], List).

%=========================================================
% Meta-commands

lf_pattern(kurikaesu,
	   command=repeat).

%=========================================================
% Utterance types

% YNQ
lf_pattern([question, _],
	   (   not(itsu),
	       not(term(null, nanji, [])),
	       not(doko),
	       not(term(dono, heya, [])),
	       not(nani),
	       not(domna),
	       not(dono),
	       not(dare),
	       not(dareka),
	       not(nan_bon),
	       not(ikutsu)
	   ),
	   utterance_type=ynq).

lf_pattern([phrase, _],
	   (   not(itsu),
	       not(term(null, nanji, [])),
	       not(doko),
	       not(term(dono, heya, [])),
	       not(nani),
	       not(domna),
	       not(dono),
	       not(dare),
	       not(dareka),
	       not(nan_bon),
	       not(ikutsu)
	   ),
	   utterance_type=ynq).
% WHQ

lf_pattern([polite_imperative | _],
	   utterance_type=whq).
lf_pattern( @when_pattern,
	   utterance_type=whq).
lf_pattern( @who_pattern,
	   utterance_type=whq).
lf_pattern( @what_pattern,
	   utterance_type=whq).
lf_pattern( @where_pattern,
	   utterance_type=whq).
lf_pattern(dareka,
	   utterance_type=whq).

%=========================================================
% Reference to current meeting

% "kaigi ni syusseki suru (etc)" without temporal modifier
lf_pattern( @attend,
	   (   [ni, term(null, @meeting, [])],
	       not([temporal, _])
	   ),
	   referent_from_context=meeting).
% "syusseki suru (etc)" without relative clause and without qualified object
lf_pattern( @attend,
	   (   not([clause, form(_, _)]),
	       not([ni, term(null, kaigi, [_ | _])]),
	       not([ni, term(null, miitingu, [_ | _])])
	   ),
	   referent_from_context=meeting).

%=========================================================

% When
lf_pattern( @when_pattern,
	    % Condition
	    ( not([kara, _]), not([made, _]), not(owaru), not(hajimaru) ),
	    query_object=when).

% Start time
lf_pattern( @when_pattern,
	   % Condition
	   [kara, _],                            
	   query_object=start_time).
lf_pattern( @when_pattern,
	   % Condition
	   hajimaru,                            
	   query_object=start_time).

% End time
lf_pattern( @when_pattern,
	   % Condition
	   owaru,
	   query_object=end_time).
lf_pattern( @when_pattern,
	   % Condition
	   [made, _],
	   query_object=end_time).

% Where
lf_pattern( @where_pattern,
	   query_object=where).

% Who
lf_pattern( @who_pattern,
	   query_object=attendee).

% Phone number
lf_pattern(denwa_bangou,
	   query_object=phone_number).

% Address
lf_pattern(juusyo,
	   term(name, PersonName, _),
	   query_object=attendee_address) :-
	is_person_name(PersonName).

% Email address
lf_pattern(meeru_adoresu,
	   query_object=email_address).

%=========================================================

% Day before yesterday
lf_pattern(ototsui,
	   on_date=referent(day_before_yesterday)).
lf_pattern(ototoi,
	   on_date=referent(day_before_yesterday)).

% Yesterday
lf_pattern(kinou,
	   on_date=referent(yesterday)).

% Today
lf_pattern(kyou,
	   on_date=referent(today)).
lf_pattern(kesa,
	   on_date=referent(today)).

% Tomorrow
lf_pattern(ashita,
	   on_date=referent(tomorrow)).
lf_pattern(asu,
	   on_date=referent(tomorrow)).

% Day after tomorrow
lf_pattern(asatte,
	   on_date=referent(day_after_tomorrow)).

% on <Date> 
lf_pattern([ @gen_ni_or_temporal, date(Year, Month, Day)],
	   on_date=referent(named_date(Day, Month, Year))) :-
	number(Year),
	number(Month),
	number(Day).
lf_pattern([ @gen_ni_or_temporal, date(unspecified, Month, Day)],
	   % Condition
	   @not_past_tense,
	   on_date=referent(nearest_named_date_in_future(Day, Month))) :-
	number(Month),
	number(Day).
lf_pattern([ @gen_ni_or_temporal, date(unspecified, Month, Day)],
	   % Condition
	   @past_tense,
	   on_date=referent(nearest_named_date_in_past(Day, Month))) :-
	number(Month),
	number(Day).

% on <Weekday>
lf_pattern([ @gen_ni_or_temporal, term(null, DayOfWeek, _)],
	   % Condition
	   @not_past_tense,
	   on_date=referent(nearest_named_weekday_in_future(EngDayOfWeek))) :-
	japanese_dayofweek(DayOfWeek, EngDayOfWeek).
lf_pattern([ @gen_ni_or_temporal, term(null, DayOfWeek, _)],
	   % Condition
	   @past_tense,
	   on_date=referent(nearest_named_weekday_in_past(EngDayOfWeek))) :-
	japanese_dayofweek(DayOfWeek, EngDayOfWeek).

% In June (past)
lf_pattern(date(unspecified, Month, unspecified),
	   % Condition
	   @past_tense,
	   in_interval=referent(nearest_named_month_in_past(Month))) :-
	number(Month).
% In June (future)
lf_pattern(date(unspecified, Month, unspecified),
	   % Condition
	   @not_past_tense,
	   in_interval=referent(nearest_named_month_in_future(Month))) :-
	number(Month).
lf_pattern(date(Year, Month, unspecified),
	   in_interval=referent(named_month(Month, Year))) :-
	number(Year),
	number(Month).

% The next N days
lf_pattern(term(N, nichikan, [[nn, term(null, mukou, [])]]),
	   in_interval=referent(the_next_n_days(N))).
% The next N weeks
lf_pattern(term(N, syuukan, [[nn, term(null, mukou, [])]]),
	   in_interval=referent(the_next_n_weeks(N))).
% The next N months
lf_pattern(term(N, @kagetsukan, [[nn, term(null, mukou, [])]]),
	   in_interval=referent(the_next_n_months(N))).
% The last N days
lf_pattern(term(N, nichikan, [[nn, term(null, @saikin_koko_or_kako, [])]]),
	   in_interval=referent(the_last_n_days(N))).
% The last N weeks
lf_pattern(term(N, syuukan, [[nn, term(null, @saikin_koko_or_kako, [])]]),
	   in_interval=referent(the_last_n_weeks(N))).
% The last N months
lf_pattern(term(N, @kagetsukan, [[nn, term(null, @saikin_koko_or_kako, [])]]),
	   in_interval=referent(the_last_n_months(N))).

%=========================================================
% Times

% <Time> ni
lf_pattern([ni, time(H, M, any, [])],
	   @afternoon_pattern,
	   at_time=referent(time(H, M, afternoon))) :-
	number(H),
	number(M).
lf_pattern([ni, time(H, M, any, [])],
	   @morning_pattern,
	   at_time=referent(time(H, M, morning))) :-
	number(H),
	number(M).
% <Time> goro
lf_pattern(time(H, M, any, Modifiers),
	   [time_qualifier, approximate],
	   in_interval=referent(approximate_time(H, M, any))) :-
	number(H),
	number(M).
% Morning
lf_pattern(@morning_pattern,
	   in_interval=referent(morning)).
% Afternoon
lf_pattern(@afternoon_pattern,
	   in_interval=referent(afternoon)).

%=========================================================

% Past tense
lf_pattern(form(past, _),
	   tense_information=referent(past)).
% Present tense
%lf_pattern(form(present, _),
%	   tense_information=referent(future)).
%lf_pattern(form(continuous_present, _),
%	   tense_information=referent(future)).
% This week
lf_pattern(konsyuu,
	   in_interval=referent(this_week)).
% This month
lf_pattern(kongetsu,
	   in_interval=referent(this_month)).
% This year
lf_pattern(kotoshi,
	   in_interval=referent(this_year)).

% Next week
lf_pattern(raisyuu,
	   not(made_ni),
	   in_interval=referent(next_week)).
% Next month
lf_pattern(raigetsu,
	   not(made_ni),
	   in_interval=referent(next_month)).
% Next year
lf_pattern(rainen,
	   not(made_ni),
	   in_interval=referent(next_year)).
% Week after next
lf_pattern(saraisyuu,
	   not(made_ni),
	   in_interval=referent(week_after_next)).
% Month after next
lf_pattern(saraigetsu,
	   not(made_ni),
	   in_interval=referent(month_after_next)).

% Up to end of next week
lf_pattern(raisyuu,
	   made_ni,
	   in_interval=referent(to_end_of_next_week)).
% Up to end of next month
lf_pattern(raigetsu,
	   made_ni,
	   in_interval=referent(to_end_of_next_month)).
% Up to end of next year
lf_pattern(rainen,
	   made_ni,
	   in_interval=referent(to_end_of_next_year)).
% Up to end of week after next
lf_pattern(saraisyuu,
	   made_ni,
	   in_interval=referent(to_end_of_week_after_next)).
% Up to end of month after next
lf_pattern(saraigetsu,
	   made_ni,
	   in_interval=referent(to_end_of_month_after_next)).

% Last week
lf_pattern(sensyuu,
	   in_interval=referent(last_week)).
% Last month
lf_pattern(sengetsu,
	   in_interval=referent(last_month)).
% Last year
lf_pattern(kyonen,
	   in_interval=referent(last_year)).

%=========================================================

% Named entities

% Names of people are assumed to be attendees
lf_pattern(term(name, PersonName, _),
	   attendee=referent(person(PersonName))) :-
	is_person_name(PersonName).

% Watashi
lf_pattern(term(null, watashi, []),
	   attendee=referent(i)).

% Place
lf_pattern([de, term(name, PlaceName, _)],
	   meeting_loc=PlaceName).
lf_pattern([gen, term(name, PlaceName, _)],
	   term(_, @meeting, List),
	   meeting_loc=PlaceName) :-
	member([gen, term(name, PlaceName, _)], List).

% Affiliation
lf_pattern(term(null, dareka, [[gen, term(name, AffiliationName, [])]]),
	   affiliation=AffiliationName).
