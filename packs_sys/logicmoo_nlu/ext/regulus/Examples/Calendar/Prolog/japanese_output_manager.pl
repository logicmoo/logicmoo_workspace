
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(output_manager,
	[abstract_action_to_action/2]
    ).

%======================================================================

:- use_module('$REGULUS/Prolog/generate').
:- use_module('$REGULUS/Prolog/speech_output').

:- use_module(library(lists)).

:- use_module('$REGULUS/PrologLib/utilities').

:- ensure_loaded('$REGULUS/Examples/Calendar/Prolog/japanese_generated_names.pl').

%======================================================================

% OUTPUT MANAGEMENT: ABSTRACT ACTION TO CONCRETE ACTION

abstract_action_to_action(say(AbsResponse, Tense), wavfile_tts(Atom, WavfileAtom)) :-
	perform_output_generation(AbsResponse, Tense, Atom),
	(   atom_to_speech_output_form(Atom, WavfileAtom) -> 
	    true
	;
	    WavfileAtom = null
	),
	!.

perform_output_generation(AbsAction, Tense, OutputAtom) :-
	generation_grammar(AbsAction, Tense, OutputWords, []),
	fix_orthography(OutputWords, OutputWords1),
	join_with_spaces(OutputWords1, OutputAtom),
	!.
perform_output_generation(_AbsAction, _Tense, OutputAtom) :-
	OutputAtom = 'syusturyoku dekimasen',
	!.
perform_output_generation(_AbsAction, _Tense, _OutputAtom) :-
	format('~N~nError in output manager.~n', []),
	fail.

generation_grammar(no, _Tense) --> ['iie'].
generation_grammar(yes, _Tense) --> ['hai'].
generation_grammar(unable_to_interpret, _Tense) --> ['wakarimasen'].
generation_grammar(i_dont_understand, _Tense) --> ['wakarimasen'].

% Empty list. Say you can't find anything.
generation_grammar(referent_list([]), _Tense) --> ['gaitou suru deeta ga ari masen'].
% Single object of any type. Describe it.
generation_grammar(referent_list([Single]), Tense) -->
	{referent_to_description(Single, Description)},
	generation_grammar(Description, Tense),
	main_verb_for_referent(Single, Tense),
	!.
% More than two meetings. Just say how many there are.
generation_grammar(referent_list(Meetings), Tense) -->
	{referent_list_type(Meetings, meeting)},
	{length(Meetings, N)},
	{N > 2},
	[kaigi, wa],
	meeting_number(N),
	main_verb_for_number(Tense),
	!.
% Two meetings on different days. Say "two meetings" and the days.
generation_grammar(referent_list([First, Second]), Tense) -->
	{referent_list_type([First, Second], meeting)},
	{date_and_times_for_meeting(First, Date1, _FromTime1, _ToTime1)},
	{date_and_times_for_meeting(Second, Date2, _FromTime2, _ToTime2)},
	{Date1 \== Date2},
	[kaigi, wa],
	meeting_number(2),
	main_verb_for_number(Tense),
	date(Date1),
	[to],
	date(Date2),
	main_verb_for_referent(Date1, Tense),
	!.
% Two meetings on the same day. Say "two meetings" and the day.
generation_grammar(referent_list([First, Second]), Tense) -->
	{referent_list_type([First, Second], meeting)},
	{date_and_times_for_meeting(First, Date1, _FromTime1, _ToTime1)},
	{date_and_times_for_meeting(Second, Date2, _FromTime2, _ToTime2)},
	{Date1 = Date2},
	[kaigi, wa],
	meeting_number(2),
	main_verb_for_number(Tense),
	date(Date1),
	main_verb_for_referent(Date1, Tense),
	!.
% At least two non-meeting items. Convert to sorted, uniqued list of descriptions
% and say that.
generation_grammar(referent_list(RefList), Tense) -->
	{referent_list_to_description_list(RefList, SortedDescList)},
	generation_grammar(SortedDescList, Tense).
% One description item. Say, followed by 'desu'
generation_grammar(description_list([Item]), Tense) -->
	generation_grammar(Item, Tense),
	main_verb_for_referent(Item, Tense).
% More than one description item. Say first item, 'to', other items.
generation_grammar(description_list([F | R]), Tense) -->
	generation_grammar(F, Tense),
	[to],
	!,
	generation_grammar(description_list(R), Tense).
% Meeting. Say "Date ni Place de kaigi"
generation_grammar(meeting(Date, _FromTime, LocName), _Tense) -->
	date(Date),
	[ni],
	place_name(LocName),
	[de, kaigi].
% Date and time. Say "Date Time"
generation_grammar(date_and_time(Date, Time), _Tense) -->
	date(Date),
	time(Time).
% Place, person name, etc. Say it.
generation_grammar(place(LocName), _Tense) -->
	place_name(LocName).
generation_grammar(person_name(FirstName, LastName), _Tense) -->
	person_name(FirstName),
	person_name(LastName).
generation_grammar(phone_number(PhoneAtom), _Tense) -->
	phone_number(PhoneAtom).
generation_grammar(email_address(EmailAtom), _Tense) -->
	email_address(EmailAtom).
generation_grammar(affiliation(Affiliation), _Tense) -->
	affiliation(Affiliation).

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
	error_list(List, Reference, Tense).

generation_grammar(error(no_referent_for(i)), _Tense) -->
	['anata no onamae wa nan desu ka'].

error_list([], _Type, _Tense) --> [].
error_list([F | R], Type, Tense) -->
	error_list_element(F, Type, Tense),
	!,
	error_list(R, Type, Tense).

error_list_element(PersonID, person(_), Tense) -->
	{referent_to_description(record(attendee, PersonID), Description)},
	generation_grammar(Description, Tense),
	[desu, ka].

error_list_element(datime(Year, Month, Day, _Hour, _Min, _Sec), _Ref, _Tense) -->
	date(Day, Month, Year).

main_verb_for_referent(attribute(_, _, when), past) -->
	[deshita],
	!.
main_verb_for_referent(attribute(_, _, when), _Tense) -->
	[desu],
	!.
main_verb_for_referent(record(meeting, _), past) -->
	[ga, arimashita],
	!.
main_verb_for_referent(record(meeting, _), _Tense) -->
	[ga, arimasu],
	!.
main_verb_for_referent(_Other, past) -->
	[deshita],
	!.
main_verb_for_referent(_Other, _Tense) -->
	[desu],
	!.

main_verb_for_number(past) -->
	[arimashita],
	!.
main_verb_for_number(_Tense) -->
	[arimasu],
	!.

date(date(_Year, Month, Day)) -->
	month(Month),
	day(Day).
	%[Year].

time(time(Hours, 0)) -->
	hour(Hours).

time(time(Hours, Mins)) -->
	hour(Hours),
	mins(Mins).

hour(0) --> [reiji].
hour(1) --> [ichiji].
hour(2) --> [niji].
hour(3) --> [sanji].
hour(4) --> [yoji].
hour(5) --> [goji].
hour(6) --> [rokuji].
hour(7) --> [shichiji].
hour(8) --> [hachiji].
hour(9) --> [kuji].
hour(10) --> [juuji].
hour(11) --> [juu, ichiji].
hour(12) --> [juu, niji].
hour(13) --> [juu, sanji].
hour(14) --> [juu, yoji].
hour(15) --> [juu, goji].
hour(16) --> [juu, rokuji].
hour(17) --> [juu, shichiji].
hour(18) --> [juu, hachiji].
hour(19) --> [juu, kuji].
hour(20) --> [nijuuji].
hour(21) --> [nijuu, ichiji].
hour(22) --> [nijuu, niji].
hour(23) --> [nijuu, sanji].

mins(0) --> [].
mins(1) --> [ippun].
mins(2) --> [nifun].
mins(3) --> [sanpun].
mins(4) --> [yonpun].
mins(5) --> [gofun].
mins(6) --> [roppun].
mins(7) --> [nanafun].
mins(8) --> [hachifun].
mins(9) --> [kyuufun].
mins(10) --> [juppun].
mins(11) --> [juu, ippun].
mins(12) --> [juu, nifun].
mins(13) --> [juu, sanpun].
mins(14) --> [juu, yonpun].
mins(15) --> [juu, gofun].
mins(16) --> [juu, roppun].
mins(17) --> [juu, nanafun].
mins(18) --> [juu, happun].
mins(19) --> [juu, kyuufun].
mins(20) --> [nijuppun].
mins(21) --> [nijuu, ippun].
mins(22) --> [nijuu, nifun].
mins(23) --> [nijuu, sanpun].
mins(24) --> [nijuu, yonpun].
mins(25) --> [nijuu, gofun].
mins(26) --> [nijuu, roppun].
mins(27) --> [nijuu, nanafun].
mins(28) --> [nijuu, happun].
mins(29) --> [nijuu, kyuufun].
mins(30) --> [sanjuppun].
mins(31) --> [sanjuu, ippun].
mins(32) --> [sanjuu, nifun].
mins(33) --> [sanjuu, sanpun].
mins(34) --> [sanjuu, yonpun].
mins(35) --> [sanjuu, gofun].
mins(36) --> [sanjuu, roppun].
mins(37) --> [sanjuu, nanafun].
mins(38) --> [sanjuu, happun].
mins(39) --> [sanjuu, kyuufun].
mins(40) --> [yonjuppun].
mins(41) --> [yonjuu, ippun].
mins(42) --> [yonjuu, nifun].
mins(43) --> [yonjuu, sanpun].
mins(44) --> [yonjuu, yonpun].
mins(45) --> [yonjuu, gofun].
mins(46) --> [yonjuu, roppun].
mins(47) --> [yonjuu, nanafun].
mins(48) --> [yonjuu, happun].
mins(49) --> [yonjuu, kyuufun].
mins(50) --> [gojuppun].
mins(51) --> [gojuu, ippun].
mins(52) --> [gojuu, nifun].
mins(53) --> [gojuu, sanpun].
mins(54) --> [gojuu, yonpun].
mins(55) --> [gojuu, gofun].
mins(56) --> [gojuu, roppun].
mins(57) --> [gojuu, nanafun].
mins(58) --> [gojuu, happun].
mins(59) --> [gojuu, kyuufun].

month(1) --> [ichigatsu].
month(2) --> [nigatsu].
month(3) --> [sangatsu].
month(4) --> [shigatsu].
month(5) --> [gogatsu].
month(6) --> [rokugatsu].
month(7) --> [shichigatsu].
month(8) --> [hachigatsu].
month(9) --> [kugatsu].
month(10) --> [juugatsu].
month(11) --> [juuichigatsu].
month(12) --> [juunigatsu].

day(1) --> [tsuitachi].
day(2) --> [futsuka].
day(3) --> [mikka].
day(4) --> [yokka].
day(5) --> [itsuka].
day(6) --> [muika].
day(7) --> [nanoka].
day(8) --> [youka].
day(9) --> [kokonoka].
day(10) --> [touka].
day(11) --> [juu, ichinichi].
day(12) --> [juu, ninichi].
day(13) --> [juu, sannichi].
day(14) --> [juu, yokka].
day(15) --> [juu, gonichi].
day(16) --> [juu, rokunichi].
day(17) --> [juu, shichinichi].
day(18) --> [juu, hachinichi].
day(19) --> [juu, kunichi].
day(20) --> [hatsuka].
day(21) --> [nijuu, ichinichi].
day(22) --> [nijuu, ninichi].
day(23) --> [nijuu, sannichi].
day(24) --> [nijuu, yokka].
day(25) --> [nijuu, gonichi].
day(26) --> [nijuu, rokunichi].
day(27) --> [nijuu, shichinichi].
day(28) --> [nijuu, hachinichi].
day(29) --> [nijuu, kunichi].
day(30) --> [sanjuu, nichi].
day(31) --> [sanjuu, ichinichi].

meeting_number(1) --> [ikken].
meeting_number(2) --> [niken].
meeting_number(3) --> [sanken].
meeting_number(4) --> [yonken].
meeting_number(5) --> [goken].
meeting_number(6) --> [rokken].
meeting_number(7) --> [nanaken].
meeting_number(8) --> [hakken].
meeting_number(9) --> [kyuuken].
meeting_number(10) --> [jukken].
meeting_number(11) --> [juu, ikken].
meeting_number(12) --> [juu, niken].
meeting_number(13) --> [juu, sanken].
meeting_number(14) --> [juu, yonken].
meeting_number(15) --> [juu, goken].
meeting_number(16) --> [juu, rokken].
meeting_number(17) --> [juu, nanaken].
meeting_number(18) --> [juu, hakken].
meeting_number(19) --> [juu, kyuuken].
meeting_number(20) --> [nijukken].
meeting_number(21) --> [nijuu, ikken].
meeting_number(22) --> [nijuu, niken].
meeting_number(23) --> [nijuu, sanken].
meeting_number(24) --> [nijuu, yonken].
meeting_number(25) --> [nijuu, goken].
meeting_number(26) --> [nijuu, rokken].
meeting_number(27) --> [nijuu, nanaken].
meeting_number(28) --> [nijuu, hakken].
meeting_number(29) --> [nijuu, kyuuken].
meeting_number(30) --> [sanjukken].
meeting_number(31) --> [sanjuu, ikken].
meeting_number(32) --> [sanjuu, niken].
meeting_number(33) --> [sanjuu, sanken].
meeting_number(34) --> [sanjuu, yonken].
meeting_number(35) --> [sanjuu, goken].
meeting_number(36) --> [sanjuu, rokken].
meeting_number(37) --> [sanjuu, nanaken].
meeting_number(38) --> [sanjuu, hakken].
meeting_number(39) --> [sanjuu, kyuuken].
meeting_number(40) --> [yonjukken].
meeting_number(41) --> [yonjuu, ikken].
meeting_number(42) --> [yonjuu, niken].
meeting_number(43) --> [yonjuu, sanken].
meeting_number(44) --> [yonjuu, yonken].
meeting_number(45) --> [yonjuu, goken].
meeting_number(46) --> [yonjuu, rokken].
meeting_number(47) --> [yonjuu, nanaken].
meeting_number(48) --> [yonjuu, hakken].
meeting_number(49) --> [yonjuu, kyuuken].
meeting_number(50) --> [gojukken].

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
