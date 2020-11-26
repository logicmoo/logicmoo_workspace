
macro(sem_help_class_member(Category, Sem, Class),
      ( help_class_member(Surface, Class) :-
	lex_entry((Category:[sem=Sem] --> Surface))
      )).

macro(verb_sem_help_class_member(Category, Verb, Class),
      @sem_help_class_member(Category, [[verb, Verb]], Class)).
macro(verb_sem_help_class_member(Category, Verb, Class),
      @sem_help_class_member(Category, [_Tense, [verb, Verb]], Class)).

% Occur verbs

@verb_sem_help_class_member(v, owaru, occur_verb).
@verb_sem_help_class_member(v, hajimaru, occur_verb).
@verb_sem_help_class_member(v, hiraku, occur_verb).
@verb_sem_help_class_member(v, yotei_suru, occur_verb).

% Attend verbs

@verb_sem_help_class_member(v, douseki_suru, attend_verb).
@verb_sem_help_class_member(v, syusseki_suru, attend_verb).
@verb_sem_help_class_member(v, kesseki_suru, attend_verb).
@verb_sem_help_class_member(v, kuru, attend_verb).
@verb_sem_help_class_member(v, deru, attend_verb).

% List verbs

@verb_sem_help_class_member(v, oshieu, list_verb).
@sem_help_class_member(n, risuto, list_verb).

% Meeting nouns

help_class_member(kaigi, meeting_noun).
help_class_member(miitingu, meeting_noun).

% Who

help_class_member(dare, who_expression).
help_class_member(syussekisya, who_expression).

% When

help_class_member(itsu, when_expression).
help_class_member(nanji, when_expression).

% Where

help_class_member(doko, where_expression).
help_class_member(basyo, where_expression).
help_class_member(heya, where_expression).

% Property nouns

help_class_member(juusyo, property_noun).
help_class_member((meeru, adoresu), property_noun).
help_class_member((denwa, bangou), property_noun).

% Time periods

help_class_member(sensyuu, named_time_period).
help_class_member(konsyuu, named_time_period).
help_class_member(raisyuu, named_time_period).
help_class_member(sengetsu, named_time_period).
help_class_member(kongetsu, named_time_period).
help_class_member(raigetsu, named_time_period).
help_class_member(kotoshi, named_time_period).
help_class_member(kyonen, named_time_period).
help_class_member(rainen, named_time_period).

help_class_member(syuukan, time_period).
help_class_member(kagetsukan, time_period).
help_class_member(kagetsu, time_period).

% Days

help_class_member(ototoi, named_day).
help_class_member(ototsui, named_day).
help_class_member(kinou, named_day).
help_class_member(kyou, named_day).
help_class_member(asu, named_day).
help_class_member(ashita, named_day).
help_class_member(asatte, named_day).

% Time of day

help_class_member(asa, time_of_day).
help_class_member(gozen, time_of_day).
help_class_member(gogo, time_of_day).
help_class_member(kesa, time_of_day).

% Next/last

help_class_member(tsugi, next_or_last).
help_class_member(kondo, next_or_last).
help_class_member(jikai, next_or_last).
help_class_member(saigo, next_or_last).

% Day of week

help_class_member(getsuyoubi, day_of_week).
help_class_member(kayoubi, day_of_week).
help_class_member(suiyoubi, day_of_week).
help_class_member(mokuyoubi, day_of_week).
help_class_member(kinyoubi, day_of_week).
help_class_member(doyoubi, day_of_week).
help_class_member(nichiyoubi, day_of_week).

help_class_member(getsuyou, day_of_week).
help_class_member(kayou, day_of_week).
help_class_member(suiyou, day_of_week).
help_class_member(mokuyou, day_of_week).
help_class_member(kinyou, day_of_week).
help_class_member(doyou, day_of_week).
help_class_member(nichiyou, day_of_week).

substitutable_help_class(day_of_week).

% Prep

help_class_member(Surface, preposition) :-
	lex_entry((p:[] --> Surface)).

% Person

help_class_member(Surface, person_name) :-
	lex_entry((name:[sem_np_type=agent] --> Surface)).

substitutable_help_class(person_name).

% Place

help_class_member(Surface, loc_name) :-
	lex_entry((name:[sem_np_type=loc] --> Surface)).

substitutable_help_class(loc_name).

% Cardinal number

help_class_member(Surface, cardinal_number) :-
	lex_entry((number:[] --> Surface)).

substitutable_help_class(cardinal_number).

% Day

help_class_member(Surface, day_number) :-
	lex_entry((day:[] --> Surface)).

substitutable_help_class(day_number).

% Month

help_class_member(Surface, month_name) :-
	lex_entry((month:[] --> Surface)).

substitutable_help_class(month_name).

% Year

help_class_member(Surface, year_name) :-
	lex_entry((year:[] --> Surface)).

substitutable_help_class(year_name).
