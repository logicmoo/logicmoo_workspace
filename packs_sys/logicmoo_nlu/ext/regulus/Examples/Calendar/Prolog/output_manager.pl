
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(output_manager,
	[abstract_action_to_action/2]
    ).

%======================================================================

:- use_module('$REGULUS/Prolog/generate').

:- use_module(library(lists)).

:- use_module('$REGULUS/PrologLib/utilities').

%======================================================================

% OUTPUT MANAGEMENT: ABSTRACT ACTION TO CONCRETE ACTION

abstract_action_to_action(say(AbsResponse, Tense), tts(Atom)) :-
	perform_output_generation(AbsResponse, Tense, Atom),
	!.

perform_output_generation(AbsAction, Tense, OutputAtom) :-
	generation_grammar(AbsAction, Tense, OutputWords, []),
	fix_orthography(OutputWords, OutputWords1),
	join_with_spaces(OutputWords1, OutputAtom),
	!.
perform_output_generation(_AbsAction, _Tense, OutputAtom) :-
	OutputAtom = 'sorry, i can\'t produce any output',
	!.
perform_output_generation(_AbsAction, _Tense, _OutputAtom) :-
	format('~N~nError in output manager.~n', []),
	fail.

generation_grammar(no, _Tense) --> ['no'].
generation_grammar(yes, _Tense) --> ['yes'].
generation_grammar(unable_to_interpret, _Tense) --> ['sorry that doesn\'t make sense'].
generation_grammar(ambiguous, _Tense) --> ['sorry, that\'s ambiguous'].
generation_grammar(i_dont_understand, _Tense) --> ['sorry, I don\'t understand'].

generation_grammar(referent_list([]), _Tense) --> ['sorry, can\'t find anything answering that description'].
% Single object of any type. Describe it.
generation_grammar(referent_list([Single]), Tense) -->
	{referent_to_description(Single, Description)},
	generation_grammar(Description, Tense),
	!.
% More than two meetings. Just say how many there are.
generation_grammar(referent_list(Meetings), Tense) -->
	{referent_list_type(Meetings, meeting)},
	{length(Meetings, N)},
	{N > 2},
	[there],
	be_verb(plur, Tense),
	meeting_number(N),
	[meetings],
	!.
% Two meetings on different days. Say "there are two meetings" and the days.
generation_grammar(referent_list([First, Second]), Tense) -->
	{referent_list_type([First, Second], meeting)},
	{date_and_times_for_meeting(First, Date1, _FromTime1, _ToTime1)},
	{date_and_times_for_meeting(Second, Date2, _FromTime2, _ToTime2)},
	{Date1 \== Date2},
	[there],
	be_verb(plur, Tense),
	meeting_number(2),
	[meetings, ':', on],
	date(Date1),
	[and],
	date(Date2),
	!.
% Two meetings on the same day. Say "two meetings" and the day.
generation_grammar(referent_list([First, Second]), Tense) -->
	{referent_list_type([First, Second], meeting)},
	{date_and_times_for_meeting(First, Date1, _FromTime1, _ToTime1)},
	{date_and_times_for_meeting(Second, Date2, _FromTime2, _ToTime2)},
	{Date1 = Date2},
	[there],
	be_verb(plur, Tense),
	meeting_number(2),
	[meetings, ',', both, on],
	date(Date1),
	!.
% At least two non-meeting items. Convert to sorted, uniqued list of descriptions
% and say that.
generation_grammar(referent_list(RefList), Tense) -->
	{referent_list_to_description_list(RefList, SortedDescList)},
	generation_grammar(SortedDescList, Tense).
% One description item. Say it.
generation_grammar(description_list([Item]), Tense) -->
	generation_grammar(Item, Tense).
% Two description item. Say first item, and, other item.
generation_grammar(description_list([F, S]), Tense) -->
	generation_grammar(F, Tense),
	[and],
	!,
	generation_grammar(S, Tense).
% More than one description item. Say first item, comma, other items.
generation_grammar(description_list([F | R]), Tense) -->
	generation_grammar(F, Tense),
	[','],
	!,
	generation_grammar(description_list(R), Tense).
% Meeting. Say "meeting in Loc on Date"
generation_grammar(meeting(Date, _FromTime, LocName), _Tense) -->
	[meeting, in],
	[LocName],
	[on],
	date(Date).
% Date and time. Say "Date Time"
generation_grammar(date_and_time(Date, Time), _Tense) -->
	time(Time),
	[on],
	date(Date).
% Place, person name, etc. Say it.
generation_grammar(place(LocName), _Tense) -->
	[LocName].
generation_grammar(person_name(FirstName, LastName), _Tense) -->
	[FirstName],
	[LastName].
generation_grammar(phone_number(PhoneAtom), _Tense) -->
	[PhoneAtom].
generation_grammar(email_address(EmailAtom), _Tense) -->
	[EmailAtom].
generation_grammar(affiliation(Affiliation), _Tense) -->
	[Affiliation].

generation_grammar(error_list([]), _Tense) --> [].
generation_grammar(error_list([Single]), Tense) -->
	generation_grammar(Single, Tense),
	!.
generation_grammar(error_list([F | R]), Tense) -->
	generation_grammar(F, Tense),
	!,
	generation_grammar(error_list(R), Tense).

% error(several_referents_for(person(marianne),[marianne_santaholma,marianne_starlander]))

generation_grammar(error(several_referents_for(Reference, List)), Tense) -->
	['Do you mean'],
	error_list(List, Reference, Tense).

generation_grammar(error(no_referent_for(i)), _Tense) -->
	['Who are you'].


error_list([First, Second], Type, Tense) -->
	error_list_element(First, Type, Tense),
	[or],
	!,
	error_list_element(Second, Type, Tense).
error_list([F | R], Type, Tense) -->
	error_list_element(F, Type, Tense),
	[','],
	!,
	error_list(R, Type, Tense).

error_list_element(PersonID, person(_), Tense) -->
	{referent_to_description(record(attendee, PersonID), Description)},
	generation_grammar(Description, Tense).

error_list_element(datime(Year, Month, Day, _Hour, _Min, _Sec), _Ref, _Tense) -->
	date(date(Day, Month, Year)).

be_verb(sing, past) --> [was].
be_verb(plur, past) --> [were].
be_verb(sing, _Tense) --> [is].
be_verb(plur, _Tense) --> [are].

date(date(Year, Month, Day)) -->
	month(Month),
	[Day],
	[Year].
time(time(Hours, Mins)) -->
	hour(Hours),
	[':'],
	mins(Mins).

hour(0) --> ['00'].
hour(Hours) --> [Hours].

mins(0) --> ['00'].
mins(Mins) --> [Mins].

month(1) --> ['January'].
month(2) --> ['February'].
month(3) --> ['March'].
month(4) --> ['April'].
month(5) --> ['May'].
month(6) --> ['June'].
month(7) --> ['July'].
month(8) --> ['August'].
month(9) --> ['September'].
month(10) --> ['October'].
month(11) --> ['November'].
month(12) --> ['December'].

meeting_number(N) -->
	[N].

%======================================================================

referent_list_type([F | _R], Type) :-
	referent_type(F, Type).

referent_type(record(meeting, _MeetingID), meeting).
referent_type(attribute(meeting, _MeetingID, Attribute), Type) :-
	Type = Attribute.

date_and_times_for_meeting(record(meeting, MeetingID), Date, Time1, Time2) :-
	database:meeting(MeetingID, Day, Month, Year, FromTime, ToTime, _LocID),
	Date = date(Year, Month, Day),
	FromTime = Hour1:Min1,
	Time1 = time(Hour1, Min1),
	ToTime = Hour2:Min2,
	Time2 = time(Hour2, Min2).

referent_list_to_description_list(RefList, description_list(SortedDescList)) :-
	referent_list_to_description_list1(RefList, DescList),
	sort(DescList, SortedDescList).

referent_list_to_description_list1([], []).
referent_list_to_description_list1([F | R], [F1 | R1]) :-
	referent_to_description(F, F1),
	referent_list_to_description_list1(R, R1).

referent_to_description(record(meeting, MeetingID), meeting(Date, FromTime, LocName)) :-
	date_and_times_for_meeting(record(meeting, MeetingID), Date, _Time1, _Time2),
	database:meeting(MeetingID, _Day, _Month, _Year, FromTime, _ToTime, LocID),
	database:location(LocID, LocName, _Country, _City, _Organisation),
	!.
referent_to_description(attribute(meeting, MeetingID, when), date_and_time(Date, Time1)) :-
	date_and_times_for_meeting(record(meeting, MeetingID), Date, Time1, _Time2),
	!.
referent_to_description(attribute(meeting, MeetingID, start_time), date_and_time(Date, Time1)) :-
	date_and_times_for_meeting(record(meeting, MeetingID), Date, Time1, _Time2),
	!.
referent_to_description(attribute(meeting, MeetingID, end_time), date_and_time(Date, Time2)) :-
	date_and_times_for_meeting(record(meeting, MeetingID), Date, _Time1, Time2),
	!.
referent_to_description(attribute(meeting, MeetingID, where), place(LocName)) :-
	database:meeting(MeetingID, _Day, _Month, _Year, _FromTime, _ToTime, LocID),
	database:location(LocID, LocName, _Country, _City, _Organisation),
	!.
referent_to_description(attribute(meeting, _MeetingID, attendee(PersonID)), Description) :-
	referent_to_description(record(attendee, PersonID), Description),
	!.
referent_to_description(record(attendee, PersonID), person_name(FirstName, LastName)) :-
	database:person(PersonID, FirstName, LastName, _Affiliation, _Phone, _Email),
	!.
referent_to_description(attribute(attendee, PersonID, phone_number), phone_number(PhoneAtom)) :-
	database:person(PersonID, _FirstName, _LastName, _Affiliation, PhoneString, _Email),
	atom_codes(PhoneAtom, PhoneString),
	!.
referent_to_description(attribute(attendee, PersonID, attendee_address), affiliation(Affiliation)) :-
	database:person(PersonID, _FirstName, _LastName, Affiliation, _PhoneString, _Email),
	!.
referent_to_description(attribute(attendee, PersonID, email_address), email_address(EmailAtom)) :-
	database:person(PersonID, _FirstName, _LastName, _Affiliation, _PhoneString, EmailString),
	atom_codes(EmailAtom, EmailString),
	!.
referent_to_description(X, Y) :-
	format('~N*** Error: bad call: ~w~n', [referent_to_description(X, Y)]),
	fail.
