
% Occur verbs
help_class_member(start, occur_verb).
help_class_member(end, occur_verb).
help_class_member((come, up), occur_verb).
help_class_member((take, place), occur_verb).

help_class_member(starts, occur_verb).
help_class_member(ends, occur_verb).
help_class_member((comes, up), occur_verb).
help_class_member((takes, place), occur_verb).

help_class_member(started, occur_verb).
help_class_member(ended, occur_verb).
help_class_member((came, up), occur_verb).
help_class_member((took, place), occur_verb).

help_class_member(starting, occur_verb).
help_class_member(ending, occur_verb).
help_class_member((coming, up), occur_verb).
help_class_member((taking, place), occur_verb).

% Attend verbs
help_class_member(attend, attend_verb).
help_class_member(attends, attend_verb).
help_class_member(attended, attend_verb).
help_class_member(attending, attend_verb).

% Hold verbs

help_class_member(plan, hold_verb).
help_class_member(schedule, hold_verb).
help_class_member(hold, hold_verb).
help_class_member(miss, hold_verb).

help_class_member(plans, hold_verb).
help_class_member(schedules, hold_verb).
help_class_member(holds, hold_verb).
help_class_member(misses, hold_verb).

help_class_member(planned, hold_verb).
help_class_member(scheduled, hold_verb).
help_class_member(held, hold_verb).
help_class_member(missed, hold_verb).

help_class_member(planning, hold_verb).
help_class_member(scheduling, hold_verb).
help_class_member(holding, hold_verb).
help_class_member(missing, hold_verb).

% List verbs

help_class_member(show, list_verb).
help_class_member(give, list_verb).
help_class_member(list, list_verb).

help_class_member(shows, list_verb).
help_class_member(gives, list_verb).
help_class_member(lists, list_verb).

help_class_member(showed, list_verb).
help_class_member(gave, list_verb).
help_class_member(listed, list_verb).

help_class_member(showing, list_verb).
help_class_member(giving, list_verb).
help_class_member(listing, list_verb).

% Meeting nouns

help_class_member(meeting, meeting_noun).
help_class_member(meetings, meeting_noun).

% Loc nouns

help_class_member(room, loc_noun).
help_class_member(rooms, loc_noun).

% Who

help_class_member((what, people), who_expression).
help_class_member((which, people), who_expression).
help_class_member(who, who_expression).

% When

help_class_member((what, time), when_expression).
help_class_member(when, when_expression).

% Property nouns

help_class_member((phone, number), property_noun).
help_class_member(address, property_noun).
help_class_member(email, property_noun).
help_class_member((email, address), property_noun).
help_class_member(location, property_noun).

help_class_member((phone, numbers), property_noun).
help_class_member(addresses, property_noun).
help_class_member(emails, property_noun).
help_class_member((email, addresses), property_noun).
help_class_member(locations, property_noun).

% Time periods

help_class_member(day, time_period).
help_class_member(week, time_period).
help_class_member(month, time_period).
help_class_member(year, time_period).

help_class_member(days, time_period).
help_class_member(weeks, time_period).
help_class_member(months, time_period).
help_class_member(years, time_period).

% Days

help_class_member(yesterday, named_day).
help_class_member(today, named_day).
help_class_member(tomorrow, named_day).

% Time of day

help_class_member(morning, time_of_day).
help_class_member(afternoon, time_of_day).
help_class_member(evening, time_of_day).

% Day of week

help_class_member(monday, day_of_week).
help_class_member(tuesday, day_of_week).
help_class_member(wednesday, day_of_week).
help_class_member(thursday, day_of_week).
help_class_member(friday, day_of_week).
help_class_member(saturday, day_of_week).
help_class_member(sunday, day_of_week).

substitutable_help_class(day_of_week).

% Prep

help_class_member(Surface, preposition) :-
	lex_entry((p:[] --> Surface)).

% Article

help_class_member(Surface, article) :-
	lex_entry((d:[article=y] --> Surface)).

% Ordinal det

help_class_member(Surface, ordinal_det) :-
	lex_entry((d:[article=n, det_type=ordinal] --> Surface)).

% Timesuffix

help_class_member(Surface, timesuffix) :-
	lex_entry((timesuffix:[] --> Surface)).

% Person

help_class_member(Surface, person_name) :-
	lex_entry((name:[sem_n_type=agent] --> Surface)).

substitutable_help_class(person_name).

% Place

help_class_member(Surface, loc_name) :-
	lex_entry((name:[sem_n_type=loc] --> Surface)).

substitutable_help_class(loc_name).

% Cardinal number

help_class_member(Surface, cardinal_number) :-
	lex_entry((number:[agr=plur] --> Surface)).

substitutable_help_class(cardinal_number).

% Ordinal number

help_class_member(Surface, dayofmonth_ordinal) :-
	lex_entry((ordinal:[time_type=dayofmonth] --> Surface)).

substitutable_help_class(dayofmonth_ordinal).

% Month

help_class_member(Surface, month_name) :-
	lex_entry((month:[] --> Surface)).

substitutable_help_class(month_name).
