
:- module(create_name_lexicon,
	[load_speech_forms_of_words/1,
	 create_name_lexicon/2,
	 create_name_lexicon/3]
    ).

%======================================================================

:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module('$REGULUS/Prolog/regulus_declarations').

%======================================================================

create_name_lexicon(InFile, OutFile) :-
	create_name_lexicon(InFile, OutFile, regulus(english)).

create_name_lexicon(InFile, OutFile, RuleType) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),
	
	prolog_file_to_list(AbsInFile, InList),
	length(InList, InN),
	format('~N~n--- Read database file (~d records) ~w~n', [InN, AbsInFile]),

	findall(Entry,
		extract_lexicon_entry_from_list(InList, Entry, RuleType),
		Entries),
	sort(Entries, OutList),

	length(OutList, OutN),
	list_to_prolog_file(OutList, AbsOutFile),
	format('~N~n--- Written lexicon file (~d records) ~w~n', [OutN, AbsOutFile]),
	!.

extract_lexicon_entry_from_list(List, Entry, RuleType) :-
	member(Record, List),
	extract_lexicon_entry_from_db_record(Record, Entry, RuleType).

extract_lexicon_entry_from_db_record(Record, Entry, RuleType) :-
	extract_place_name_entry_from_db_record(Record, Entry, RuleType).
extract_lexicon_entry_from_db_record(Record, Entry, RuleType) :-
	extract_person_name_entry_from_db_record(Record, Entry, RuleType).
extract_lexicon_entry_from_db_record(Record, Entry, RuleType) :-
	extract_affiliation_entry_from_db_record(Record, Entry, RuleType).
extract_lexicon_entry_from_db_record(Record, Entry, RuleType) :-
	extract_phone_number_entry_from_db_record(Record, Entry, RuleType).
extract_lexicon_entry_from_db_record(Record, Entry, RuleType) :-
	extract_email_address_entry_from_db_record(Record, Entry, RuleType).

/*
Place names:

person(pierrette_bouillon, pierrette, bouillon, geneva, "+41 22 379 8679", "Pierrette.Bouillon@issco.unige.ch").

location(nikos_room_1, 'Nikos\'s room', switzerland, geneva, geneva_university).
location(saint_margarets_road, 'Saint Margarets Road', england, cambridge, null).

@place_name(geneva, geneva).
@place_name((geneva, university), geneva_university).
*/

extract_place_name_entry_from_db_record(Record, Entry, RuleType) :-
	Record = person(_ID, _FirstName, _LastName, Location, _Phone, _Email),
	surface_form_from_semantic_value(Location, LocationSurface),
	(   RuleType = regulus(_Lang) ->
	    Entry = @place_name(LocationSurface, Location)
	;
	    comma_list_to_list(LocationSurface, LocationSurfaceList),
	    Entry = ( place_name(Location) --> LocationSurfaceList )
	).
extract_place_name_entry_from_db_record(Record, Entry, RuleType) :-
	Record = location(_ID, RoomID, Country, City, Organisation),
	member(Location, [RoomID, Country, City, Organisation]),
	surface_form_from_semantic_value(Location, LocationSurface),
	(   RuleType = regulus(_Lang) ->
	    Entry = @place_name(LocationSurface, Location)
	;
	    comma_list_to_list(LocationSurface, LocationSurfaceList),
	    Entry = ( place_name(Location) --> LocationSurfaceList )
	).

/*
Person names:

person(pierrette_bouillon, pierrette, bouillon, geneva, "+41 22 379 8679", "Pierrette.Bouillon@issco.unige.ch").

@person_name((pierrette, bouillon), pierrette_bouillon).
@person_name(pierrette, pierrette).
@person_name(bouillon, bouillon).

*/

% "pierrette" or "bouillon"
extract_person_name_entry_from_db_record(Record, Entry, RuleType) :-
	Record = person(_ID, FirstName, LastName, _Location, _Phone, _Email),
	member(Name, [FirstName, LastName]),
	surface_form_from_semantic_value(Name, NameSurface),
	(   RuleType = regulus(_Lang) ->
	    Entry = @person_name(NameSurface, Name)
	;
	    comma_list_to_list(NameSurface, NameSurfaceList),
	    Entry = ( person_name(Name) --> NameSurfaceList )
	).
% "yukie san" or "nakao san"
extract_person_name_entry_from_db_record(Record, Entry, RuleType) :-
	RuleType = regulus(japanese),
	Record = person(_ID, FirstName, LastName, _Location, _Phone, _Email),
	join_with_underscore([FirstName, san], FirstNameSan),
	join_with_underscore([LastName, san], LastNameSan),
	(   [SemName, SynName] = [FirstName, FirstNameSan]
	;
	    [SemName, SynName] = [LastName, LastNameSan]
	),
	surface_form_from_semantic_value(SynName, NameSurface),
	Entry = @person_name(NameSurface, SemName).
% "pierrette bouillon" or "yukie nakao" 
extract_person_name_entry_from_db_record(Record, Entry, RuleType) :-
	Record = person(ID, FirstName, LastName, _Location, _Phone, _Email),
	join_with_underscore([FirstName, LastName], Name),
	surface_form_from_semantic_value(Name, NameSurface),
	(   RuleType = regulus(_Lang) ->
	    Entry = @person_name(NameSurface, ID)
	;
	    comma_list_to_list(NameSurface, NameSurfaceList),
	    Entry = ( person_name(Name) --> NameSurfaceList )
	).
% "yukie san" or "nakao san"
extract_person_name_entry_from_db_record(Record, Entry, RuleType) :-
	RuleType = regulus(japanese),
	Record = person(ID, FirstName, LastName, _Location, _Phone, _Email),
	join_with_underscore([LastName, FirstName], Name),
	surface_form_from_semantic_value(Name, NameSurface),
	(   RuleType = regulus(_Lang) ->
	    Entry = @person_name(NameSurface, ID)
	;
	    comma_list_to_list(NameSurface, NameSurfaceList),
	    Entry = ( person_name(Name) --> NameSurfaceList )
	).
% "nakao yukie san"
extract_person_name_entry_from_db_record(Record, Entry, RuleType) :-
	RuleType = regulus(japanese),
	Record = person(ID, FirstName, LastName, _Location, _Phone, _Email),
	join_with_underscore([LastName, FirstName], Name),
	join_with_underscore([Name, san], NameSan),
	surface_form_from_semantic_value(NameSan, NameSurface),
	Entry = @person_name(NameSurface, ID).
% "yukie nakao san" 
extract_person_name_entry_from_db_record(Record, Entry, RuleType) :-
	RuleType = regulus(japanese),
	Record = person(ID, FirstName, LastName, _Location, _Phone, _Email),
	join_with_underscore([FirstName, LastName], Name),
	join_with_underscore([Name, san], NameSan),
	surface_form_from_semantic_value(NameSan, NameSurface),
	Entry = @person_name(NameSurface, ID).

extract_affiliation_entry_from_db_record(Record, Entry, prolog) :-
	Record = person(_ID, _FirstName, _LastName, Affiliation, _Phone, _Email),
	surface_form_from_semantic_value(Affiliation, AffiliationSurface),
	comma_list_to_list(AffiliationSurface, AffiliationSurfaceList),
	Entry = ( affiliation(Affiliation) --> AffiliationSurfaceList ).

extract_phone_number_entry_from_db_record(Record, Entry, prolog) :-
	Record = person(_ID, _FirstName, _LastName, _Affiliation, PhoneString, _Email),
	atom_codes(Phone, PhoneString),
	surface_form_from_semantic_value(Phone, PhoneSurface),
	comma_list_to_list(PhoneSurface, PhoneSurfaceList),
	Entry = ( phone_number(Phone) --> PhoneSurfaceList ).

extract_email_address_entry_from_db_record(Record, Entry, prolog) :-
	Record = person(_ID, _FirstName, _LastName, _Affiliation, _Phone, EmailString),
	atom_codes(Email, EmailString),
	surface_form_from_semantic_value(Email, EmailSurface),
	comma_list_to_list(EmailSurface, EmailSurfaceList),
	Entry = ( email_address(Email) --> EmailSurfaceList ).

:- dynamic speech_form_of_word/2.

load_speech_forms_of_words(File) :-
	safe_absolute_file_name(File, AbsFile),
	prolog_file_to_list(AbsFile, List),
	length(List, N),
	format('~N--- Read file (~d entries) ~w~n', [N, AbsFile]),
	load_speech_forms_of_words1(List),
	!.

load_speech_forms_of_words1([]).
load_speech_forms_of_words1([F | R]) :-
	load_speech_forms_of_word(F),
	!,
	load_speech_forms_of_words1(R).

load_speech_forms_of_word(speech_form_of_word(Text, Speech)) :-
	assertz(speech_form_of_word(Text, Speech)),
	!.
load_speech_forms_of_word(Other) :-
	format('~N*** Warning: bad entry in speech_form_of_word file "~w", item discarded~n', [Other]).

%surface_form_from_semantic_value(SemValue, _Surface) :-
%	atom_chars(SemValue, SemValueChars),
%	member(0' , SemValueChars),
%	!,
%	format('~N*** Warning: space char in semantic value "~w", item discarded~n', [SemValue]),
%	fail.
surface_form_from_semantic_value(SemValue, Surface) :-
	SemValue \== null,
	split_atom_into_words(SemValue, 0'_, SemValueComponentsList),
	add_speech_forms_of_words(SemValueComponentsList, SemValueComponentsList1),
	list_to_comma_list(SemValueComponentsList1, Surface).

add_speech_forms_of_words([], []).
add_speech_forms_of_words([F | R], [F1 | R1]) :-
	(   speech_form_of_word(F, SpeechF) ->
	    F1 = @st(SpeechF, F)
	;
	    otherwise ->
	    F1 = F
	),
	!,
	add_speech_forms_of_words(R, R1).
