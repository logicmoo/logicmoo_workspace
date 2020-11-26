
:- module(dialogue_utils,
	[make_dialogue_move_canonical/2,
	 non_constraining_pattern/1,
	 non_constraining_pattern_element/1,
	 get_lf_from_dialogue_state/2,
	 error_pattern/1,
	 repeat_pattern/1,
	 meeting_id_for_record/2,
	 interval_for_record/2,
	 datime_for_record/2,
	 affiliation_for_record/2,
	 meeting_loc_for_record/2,
	 time_number_for_record/2,
	 time_number_for_meeting_id/2,
	 interval_for_meeting_id/2,
	 attendee_for_record/2,
	 name_for_attendee/2,
	 affiliation_for_attendee/2,
	 name_for_location/2,
	 prep/2
	]).

%----------------------------------------------------------------------

:- use_module(library(lists)).
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module('$REGULUS/Examples/Calendar/Prolog/calendar_utils').

%----------------------------------------------------------------------

make_dialogue_move_canonical(MoveIn, MoveOut) :-
	make_dialogue_move_canonical1(MoveIn, MoveNext),
	sort(MoveNext, MoveOut),
	!.
make_dialogue_move_canonical(MoveIn, MoveOut) :-
	format('~N*** Error: bad call: ~w~n', [make_dialogue_move_canonical(MoveIn, MoveOut)]),
	fail.

make_dialogue_move_canonical1([F | R], [F1 | R1]) :-
	make_dialogue_move_element_canonical1(F, F1),
	!,
	make_dialogue_move_canonical1(R, R1).
make_dialogue_move_canonical1(Other, Other).

make_dialogue_move_element_canonical1(aggregate(Type, Body), aggregate(Type, Body1)) :-
	make_dialogue_move_canonical(Body, Body1),
	!.
make_dialogue_move_element_canonical1(Other, Other).

non_constraining_pattern([]).
non_constraining_pattern([F | R]) :-
	non_constraining_pattern_element(F),
	!,
	non_constraining_pattern(R).

error_pattern(Pattern) :-
	member(error(_), Pattern).

repeat_pattern(Pattern) :-
	member(command=repeat, Pattern).

non_constraining_pattern_element(utterance_type=_).
non_constraining_pattern_element(tense_information=_).
non_constraining_pattern_element(query_object=_).

get_lf_from_dialogue_state(InS, LF) :-
	member(lf=LF, InS),
	!.
get_lf_from_dialogue_state(_InS, LF) :-
	LF = '*no_lf*'.

meeting_id_for_record(Record, Id) :-
	member(meeting=Id, Record).

interval_for_record(Record, Interval) :-
	member(meeting=Id, Record),
	interval_for_meeting_id(Id, Interval).

interval_for_meeting_id(Id, interval(StartDatime, EndDatime)) :-
	database:meeting(Id, Day, Month, Year, StartTime, EndTime, _Place),
	StartTime = StartHours:StartMins,
	EndTime = EndHours:EndMins,
	StartDatime = datime(Year, Month, Day, StartHours, StartMins, 0),
	EndDatime = datime(Year, Month, Day, EndHours, EndMins, 0).

datime_for_record(Record, Datime) :-
	member(meeting=Id, Record),
	database:meeting(Id, Day, Month, Year, StartTime, _EndTime, _Place),
	StartTime = Hours:Mins,
	Datime = datime(Year, Month, Day, Hours, Mins, 0).
%datime_for_record(Record, Datime) :-
%	member(meeting=Id, Record),
%	meeting(Id, Day, Month, Year, _StartTime, _EndTime, _Place),
%	Datime = datime(Year, Month, Day, 0, 0, 0).

time_number_for_meeting_id(ID, Number) :-
	time_number_for_record([meeting=ID], Number).

time_number_for_record(Record, Number) :-
	datime_for_record(Record, Datime),
	datime_to_number(Datime, Number).

attendee_for_record(Record, Attendee) :-
	member(attendee=Attendee, Record).

affiliation_for_record(Record, Affiliation) :-
	member(attendee=Attendee, Record),
	affiliation_for_attendee(Attendee, Affiliation).

meeting_loc_for_record(Record, LocID) :-
	member(location=LocID, Record). 

% person(pierrette_bouillon, pierrette, bouillon, geneva, "+41 22 379 8679", "Pierrette.Bouillon@issco.unige.ch").
name_for_attendee(Attendee, Name) :-
	database:person(Attendee, FirstName, Surname, _Affiliation, _Phone, _Email),
	member(Name, [Attendee, FirstName, Surname]).
 
affiliation_for_attendee(Attendee, Affiliation) :-
	database:person(Attendee, _FirstName, _Surname, Affiliation, _Phone, _Email).

% location(ID, LocName, Country, City, Organisation).
name_for_location(ID, Name) :-
	database:location(ID, LocName, Country, City, Organisation),
	member(Name, [LocName, Country, City, Organisation]).	

prep(at_loc, temporal).
prep(at_meeting, temporal).
prep(date, temporal).
prep(from_loc, temporal).
prep(on_date, temporal).
prep(in_time, temporal).
prep(at_time, temporal).
prep(for_time, temporal).
prep(duration, temporal).
prep(duration_time, temporal).
prep(during_time, temporal).

