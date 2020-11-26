
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(dialogue_manager,
	[initial_dialogue_state/1,
	 update_dialogue_state/5]
    ).

%======================================================================

:- use_module(library(lists)).
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module('$REGULUS/Examples/Calendar/Prolog/dialogue_utils').
:- use_module('$REGULUS/Examples/Calendar/Prolog/calendar_utils').

%======================================================================

% Initial dialogue state = initial world state
initial_dialogue_state([]).

%======================================================================

% DIALOGUE MANAGEMENT

% Don't update LF context when utterance is a repeat
update_dialogue_state(Pattern, _LF, InS, AbsAct, OutS) :-
	repeat_pattern(Pattern),
	!,
	(   member(last_utt=LastUtt, InS) ->
	    AbsAct = LastUtt
	;
	    otherwise ->
	    AbsAct = say(i_dont_understand, present)
	),
	OutS = InS.
update_dialogue_state(Pattern, LF, InS, AbsAct, OutS) :-
	replace_lf_in_dialogue_state(InS, LF, NextS),
	(   error_pattern(Pattern) ->
	    AbsAct = say(error_list(Pattern), present),
	    replace_last_utterance_in_dialogue_state(NextS, AbsAct, OutS)
	;
	    \+ non_constraining_pattern(Pattern) ->
	    update_dialogue_state1(Pattern, NextS, AbsOutput, NextS1),
	    tense_of_lf(LF, Tense),
	    AbsAct = say(AbsOutput, Tense),
	    replace_last_utterance_in_dialogue_state(NextS1, AbsAct, OutS)
	;
	    otherwise ->
	    AbsAct = say(i_dont_understand, present),
	    OutS = NextS
	),
	!.
update_dialogue_state(_Move, _LF, _InS, _AbsAct, _OutS) :-
	format('~N~nUnable to update.~n', []),
	fail.

%----------------------------------------------------------------------

tense_of_lf(LF, Tense) :-
	term_contains_functor(LF, past/0),
	!,
	Tense = past.
tense_of_lf(_LF, Tense) :-
	Tense = present,
	!.
tense_of_lf(LF, Tense) :-
	format('~N*** Error: bad call: ~w~n', [tense_of_lf(LF, Tense)]),
	fail.

%----------------------------------------------------------------------

update_dialogue_state1(Pattern, InS, AbsOutput, OutS) :-
	get_query_object_from_pattern(Pattern, QueryObject),
	evaluate_aggregates_in_pattern(Pattern, Pattern1, AggregateReferents-[]),
	findall(AnswerReferent,
		(   matching_record(Pattern1, Record),
		    record_to_referent(Record, QueryObject, AnswerReferent)
		),
		AnswerReferents0),
	sort(AnswerReferents0, AnswerReferents),
	implicit_referents_for_list(AnswerReferents, ImplicitReferents),
	append_list([AggregateReferents, AnswerReferents, ImplicitReferents], AllReferents0),
	sort(AllReferents0, AllReferents),
	update_dialogue_context(InS, Pattern, AllReferents, NewReferentsP, OutS),
	(   ( ynq_pattern(Pattern1), AnswerReferents = [] ) ->
	    AbsOutput = no
	;
	    %( ynq_pattern(Pattern1), NewReferentsP = no_new_referents ) ->
	    ( ynq_pattern(Pattern1), dont_mention_referents_in_answer(AnswerReferents, Pattern, NewReferentsP) ) ->
	    AbsOutput = yes
	;
	    otherwise ->
	    AbsOutput = referent_list(AnswerReferents)
	).

%----------------------------------------------------------------------

evaluate_aggregates_in_pattern([], [], RefsIn-RefsIn).
evaluate_aggregates_in_pattern([F | R], [F1 | R1], RefsIn-RefsOut) :-
	evaluate_aggregate(F, F1, RefsIn-RefsNext),
	!,
	evaluate_aggregates_in_pattern(R, R1, RefsNext-RefsOut).
evaluate_aggregates_in_pattern([F | R], [F | R1], RefsIn-RefsOut) :-
	!,
	evaluate_aggregates_in_pattern(R, R1, RefsIn-RefsOut).

evaluate_aggregate(aggregate(next_n_meetings(N), Constraints0), meeting=in_list(SelectedMeetingIDs), RefsIn-RefsOut) :-
	add_future_to_constraints(Constraints0, Constraints),
	find_matching_records(Constraints, MeetingIDs),
	sort_meeting_ids_by_time(MeetingIDs, SortedMeetingIDs, ascending),
	firstn_or_all(SortedMeetingIDs, N, SelectedMeetingIDs),
	meeting_ids_to_referents(SelectedMeetingIDs, RefsIn-RefsOut).

evaluate_aggregate(aggregate(last_n_meetings(N), Constraints0), meeting=in_list(SelectedMeetingIDs), RefsIn-RefsOut) :-
	add_past_to_constraints(Constraints0, Constraints),
	find_matching_records(Constraints, MeetingIDs),
	sort_meeting_ids_by_time(MeetingIDs, SortedMeetingIDs, descending),
	firstn_or_all(SortedMeetingIDs, N, SelectedMeetingIDs),
	meeting_ids_to_referents(SelectedMeetingIDs, RefsIn-RefsOut).

evaluate_aggregate(aggregate(nth_meeting(N), Constraints), meeting=SelectedMeetingID, RefsIn-RefsOut) :-
	find_matching_records(Constraints, MeetingIDs),
	sort_meeting_ids_by_time(MeetingIDs, SortedMeetingIDs, ascending),
	safe_nth(N, SortedMeetingIDs, SelectedMeetingID),
	meeting_ids_to_referents([SelectedMeetingID], RefsIn-RefsOut).

evaluate_aggregate(aggregate(first_n_meetings(N), Constraints), meeting=in_list(SelectedMeetingIDs), RefsIn-RefsOut) :-
	find_matching_records(Constraints, MeetingIDs),
	sort_meeting_ids_by_time(MeetingIDs, SortedMeetingIDs, ascending),
	firstn_or_all(SortedMeetingIDs, N, SelectedMeetingIDs),
	meeting_ids_to_referents(SelectedMeetingIDs, RefsIn-RefsOut).

find_matching_records(Constraints, MeetingIDs) :-
	findall(MeetingID,
		(   matching_record(Constraints, Record),
		    meeting_id_for_record(Record, MeetingID)
		),
		MeetingIDs0),
	sort(MeetingIDs0, MeetingIDs).	

meeting_ids_to_referents([], RefsIn-RefsIn).
meeting_ids_to_referents([MeetingID | RMeetingIDs], [record(meeting, MeetingID) | RNext]-ROut) :-
	meeting_ids_to_referents(RMeetingIDs, RNext-ROut).

add_future_to_constraints(Constraints, [InFuture | Constraints]) :-
	InFuture = ( tense_information=interval(Start, End) ),
	today_time(Start),
	end_of_time(End).

add_past_to_constraints(Constraints, [InPast | Constraints]) :-
	InPast = ( tense_information=interval(Start, End) ),
	beginning_of_time(Start),
	today_time(End).

sort_meeting_ids_by_time(IDs, SortedIDs, Direction) :-
	tag_meeting_ids_by_time(IDs, TaggedIDs),
	keysort(TaggedIDs, SortedTaggedIDs0),
	(   Direction = ascending ->
	    SortedTaggedIDs = SortedTaggedIDs0 ;
	    reverse(SortedTaggedIDs0, SortedTaggedIDs)
	),
	unkey_list(SortedTaggedIDs, SortedIDs).

tag_meeting_ids_by_time([], []).
tag_meeting_ids_by_time([F | R], [Time-F | R1]) :-
	time_number_for_meeting_id(F, Time),
	!,
	tag_meeting_ids_by_time(R, R1).	

%----------------------------------------------------------------------

get_query_object_from_pattern(Pattern, QueryObject) :-
	member(query_object=QueryObject, Pattern),
	!.
get_query_object_from_pattern(_Pattern, QueryObject) :-
	QueryObject = meeting.

record_list_to_referent_list([], _QueryObject, []) :-
	!.
record_list_to_referent_list([F | R], QueryObject, [F1 | R1]) :-
	record_to_referent(F, QueryObject, F1),
	record_list_to_referent_list(R, QueryObject, R1),
	!.
record_list_to_referent_list(Records, QueryObject, Referents) :-
	format('~N*** Error: bad call: ~w~n',
	       [record_list_to_referent_list(Records, QueryObject, Referents)]),
	fail.

ynq_pattern(Pattern) :-
	member(utterance_type=ynq, Pattern).
	
%----------------------------------------------------------------------

dont_mention_referents_in_answer(AnswerReferents, Pattern, _NewReferentsP) :-
	AnswerReferents = [record(meeting, MeetingId)],
	member(meeting=MeetingId, Pattern),
	!.

%----------------------------------------------------------------------

implicit_referents_for_list([], []).
implicit_referents_for_list([F | R], [F1 | R1]) :-
	implicit_meeting_referent_for_attribute(F, F1),
	!,
	implicit_referents_for_list(R, R1).
implicit_referents_for_list([_F | R], R1) :-
	!,
	implicit_referents_for_list(R, R1).

implicit_meeting_referent_for_attribute(attribute(RecordType, ID, _AttributeType),
					record(RecordType, ID)).

%----------------------------------------------------------------------

% Doing it this way means that in a 'who' question the meeting becomes a reference
% No cut on this clause, since we can have multiple attendees.
record_to_referent(Record, attendee, attribute(meeting, MeetingID, attendee(AttendeeID))) :-
	member(meeting=MeetingID, Record),
	member(attendee=AttendeeID, Record).
record_to_referent(Record, QueryObject, record(QueryObject, Value)) :-
	QueryObject \== attendee,
	member(QueryObject=Value, Record),
	!.
record_to_referent(Record, QueryObject, attribute(Component, ComponentID, QueryObject)) :-
	QueryObject \== attendee,
	attribute(QueryObject, Component),
	member(Component=ComponentID, Record).

attribute(when, meeting).
attribute(where, meeting).
attribute(start_time, meeting).
attribute(end_time, meeting).
attribute(duration, meeting).

attribute(phone_number, attendee).
attribute(email_address, attendee).
attribute(attendee_address, attendee).
attribute(affiliation, attendee).

%----------------------------------------------------------------------

replace_lf_in_dialogue_state([], LF, [lf=LF]).
replace_lf_in_dialogue_state([lf=_OldLF | R], LF, [lf=LF | R]) :-
	!.
replace_lf_in_dialogue_state([F | R], LF, [F | R1]) :-
	replace_lf_in_dialogue_state(R, LF, R1).

replace_last_utterance_in_dialogue_state([], AbsAct, [last_utt=AbsAct]).
replace_last_utterance_in_dialogue_state([last_utt = _Old | R], AbsAct, [last_utt = AbsAct | R]) :-
	!.
replace_last_utterance_in_dialogue_state([F | R], AbsAct, [F | R1]) :-
	!,
	replace_last_utterance_in_dialogue_state(R, AbsAct, R1).
	
%----------------------------------------------------------------------

update_dialogue_context(InS, Pattern, Referents, NewReferentsP, OutS) :-
	get_lf_from_dialogue_state(InS, LF),
	update_dialogue_context1(InS, Pattern, Referents, LF, NewReferentsP, OutS).

update_dialogue_context1(InS, _Pattern, Referents, LF, NewReferentsP, OutS) :-
	(   \+ member(referents=_OldReferents, InS) ;
	    member(referents=[], InS) 
	),
	OutS = [lf=LF, referents=Referents],
	(   Referents = [] ->
	    NewReferentsP = no_new_referents ;
	    NewReferentsP = new_referents
	),
	!.
update_dialogue_context1(InS, _Pattern, Referents, LF, NewReferentsP, OutS) :-
	Referents = [],
	member(referents=OldReferents, InS),
	OutS = [lf=LF, referents=OldReferents],
	NewReferentsP = no_new_referents,
	!.
update_dialogue_context1(InS, _Pattern, Referents, LF, NewReferentsP, OutS) :-
	member(referents=OldReferents, InS),
	update_referents(OldReferents, Referents, NewReferentsP, NewReferents),
	OutS = [lf=LF, referents=NewReferents],
	!.
update_dialogue_context(InS, Pattern, Referents, LF, NewReferentsP, OutS) :-
	format('~N*** Error: bad call: ~w~n',
	       [update_dialogue_context(InS, Pattern, Referents, LF, NewReferentsP, OutS)]),
	fail.

update_referents(OldReferents, Referents, NewReferentsP, NewReferents) :-
	(   ( member(Ref, Referents), \+ member(Ref, OldReferents) ) ->
	    NewReferentsP = new_referents ;
	    NewReferentsP = no_new_referents
	),
	types_of_referents(Referents, Types),
	remove_referents_of_types(OldReferents, Types, PrunedOldReferents),
	append(PrunedOldReferents, Referents, NewReferents).

types_of_referents(Referents, Types) :-
	findall(Type,
		( member(Referent, Referents), type_of_referent(Referent, Type) ),
		Types).

type_of_referent(record(Type, _ID), Type) :-
	!.
type_of_referent(attribute(_RecordType, _ID, Type), Type) :-
	!.

remove_referents_of_types([], _Types, []) :-
	!.
remove_referents_of_types([Referent | R], Types, R1) :-
	type_of_referent(Referent, Type),
	member(Type, Types),
	remove_referents_of_types(R, Types, R1),
	!.
remove_referents_of_types([Referent | R], Types, [Referent | R1]) :-
	type_of_referent(Referent, Type),
	\+ member(Type, Types),
	remove_referents_of_types(R, Types, R1),
	!.
remove_referents_of_types(OldReferents, Types, PrunedOldReferents) :-
	format('~N*** Error: bad call: ~w~n',
	       [remove_referents_of_type(OldReferents, Types, PrunedOldReferents)]),
	fail.

pattern_refers_to_meeting(_Pattern).

pattern_specifies_date(Pattern) :-
	member(on_date=_, Pattern),
	!.
pattern_specifies_date(Pattern) :-
	member(in_interval=_, Pattern),
	!.
pattern_specifies_date(Pattern) :-
	member(aggregate(next_meeting, _), Pattern),
	!.

pattern_specifies_affiliation(Pattern) :-
	member(affiliation=_, Pattern).

pattern_specifies_location(Pattern) :-
	member(location=_, Pattern).

%----------------------------------------------------------------------

matching_record(Pattern, Record) :-
	find_record(Pattern, Record),
	pattern_matches_record(Pattern, Record).

% meeting(MeetingID, Day, Month, Year, StartTime, EndTime, LocID).
% attends(PersonID, MeetingID).
% person(PersonID, FirstName, LastName, Affiliation, Phone, Email).
% location(LocID, Name, Country, City, Organisation).

find_record(Pattern, Record) :-
	record_type_for_pattern(Pattern, RecordType),
	!,
	find_record1(RecordType, Record).

record_type_for_pattern(Pattern, RecordType) :-
	member(query_object=QueryObject, Pattern),
	person_property_query_object(QueryObject),
	!,
	RecordType = person.
record_type_for_pattern(_Pattern, meeting).

person_property_query_object(phone_number).
person_property_query_object(attendee_address).
person_property_query_object(email_address).

find_record1(person, Record) :-
	database:person(PersonID, _FirstName, _LastName, _Affiliation, _Phone, _Email),
	Record = [attendee=PersonID].	
find_record1(meeting, Record) :-
	database:meeting(MeetingID, _Day, _Month, _Year, _StartTime, _EndTime, LocID),
	findall(attendee=PersonID,
		database:attends(PersonID, MeetingID),
		Attendees),
		
	Record = [meeting=MeetingID, location=LocID | Attendees].	

pattern_matches_record([], _Record).
pattern_matches_record([F | R], Record) :-
	pattern_element_matches_record(F, Record),
	!,
	pattern_matches_record(R, Record).

pattern_element_matches_record(utterance_type=_, _Record) :-
	!.
pattern_element_matches_record(query_object=_, _Record) :-
	!.
pattern_element_matches_record(referent_from_context=_, _Record) :-
	!.
pattern_element_matches_record(meeting=in_list(IDs), Record) :-
	member(ID, IDs),
	meeting_id_for_record(Record, ID),
	!.
pattern_element_matches_record(meeting=ID, Record) :-
	atomic(ID),
	meeting_id_for_record(Record, ID),
	!.
% Can have several attendees in a record.
pattern_element_matches_record(attendee=in_list(List), Record) :-
	attendee_for_record(Record, Attendee),
	name_for_attendee(Attendee, Name),
	member(Name, List).	
pattern_element_matches_record(attendee=Name, Record) :-
	attendee_for_record(Record, Attendee),
	name_for_attendee(Attendee, Name).
pattern_element_matches_record(affiliation=Name, Record) :-
	affiliation_for_record(Record, Name),
	!.
pattern_element_matches_record(meeting_loc=Name, Record) :-
	meeting_loc_for_record(Record, Loc),
	name_for_location(Loc, Name),
	!.
pattern_element_matches_record(on_date=PatternDatime, Record) :-
	interval_for_record(Record, RecordInterval),
	Datime0 = PatternDatime,
	next_date(Datime0, Datime1),
	intervals_intersect(RecordInterval, interval(Datime0, Datime1)),
	!.
pattern_element_matches_record(tense_information=Interval, Record) :-
	pattern_element_matches_record(in_interval=Interval, Record),
	!.
pattern_element_matches_record(in_interval=PatternInterval, Record) :-
	interval_for_record(Record, RecordInterval),
	intervals_intersect(PatternInterval, RecordInterval),
	!.

			  