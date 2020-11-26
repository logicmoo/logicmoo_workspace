:- ensure_loaded('$REGULUS/PrologLib/compatibility').
 
/*

Format examples for flat records:

Prompt
Course		english_course
Lesson		touristoffice 
Group		musical_name
Text		Frag : ein Ticket für König der Löwen
Tags		ticket_number=one musical=lion_king
Response	i would like one ticket for the lion king 
Response	i want one ticket for the lion king
Response	could i have one ticket for the lion king
Response	one ticket for the lion king please
EndPrompt

PromptTemplate i_would_like_a_ticket_for_musical GERMAN ENGLISH SEMANTIC
Course		english_course
Lesson		touristoffice 
Group		musical_name
Text		Frag : ein Ticket für GERMAN
Tags		ticket_number=one musical=SEMANTIC
Response	i ( want | would like ) one ticket for ENGLISH ?please
Response	( could | can ) i have ( one | a ) ticket for ENGLISH ?please
Response	( one | a ) ticket for ENGLISH ?please
EndPromptTemplate

ApplyTemplate i_would_like_a_ticket_for_musical "König der Löwen" "the lion king" "lion_king"

(Questionnaire)

Group
Questionnaire red_cross
Name Religion 
Fillers christian muslim no_religion
Next MotherTongue
EndGroup

Question
Questionnaire red_cross
Group Religion 
Translation/French êtes-vous chrétien
Variant are you ?a christian
Answers yes=christian no
EndQuestion

Answer
Content christian
Code CHR
Translation/French chrétien
EndAnswer

*/

:- module(lite_read_flat_file,
	[read_flat_file/4,
	 read_flat_file/3,
	 read_flat_file/2,

	 read_single_flat_file_to_pre_units/4,

	 write_flat_file_summary_for_adding_new_l1/2,

	 expand_abstract_response/2,
	 
	 test_read_flat_file/1,

	 response_to_lf/2]
    ).

:- use_module('$REGULUS/Prolog/lite_read_sign_csv').
:- use_module('$REGULUS/Prolog/lite_utils').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%======================================================================

% If this string is included in a prompt, it will be ignored.
ignore_prompt_string("*NULL*").

%======================================================================

:- dynamic flat_phrase/2.
:- dynamic flat_tr_phrase/2.
:- dynamic flat_template/4.

%======================================================================

test_read_flat_file(english_course) :-
	read_flat_file([[namespace=user,
			 main_spec=[['$CALLSLT/LiteContent/english_course/english_course/grammars/english.txt', [french, german]]],
			 aux_spec=[['$CALLSLT/LiteContent/english_course/english_course/grammars/english_chinese.txt', [chinese]]]]],
		       List,
		       call),
	list_to_prolog_file_prettyprint_with_encoding(List, '$CALLSLT/LiteContent/english_course/english_course/grammars/english_expanded.pl', 'UTF-8').

test_read_flat_file(english_course_dont_expand) :-
	read_flat_file([[namespace=user,
			 main_spec=[['$CALLSLT/LiteContent/english_course/english_course/grammars/english.txt', [french, german]]],
			 aux_spec=[['$CALLSLT/LiteContent/english_course/english_course/grammars/english_chinese.txt', [chinese]]]]],
		       List,
		       call,
		       dont_expand),
	list_to_prolog_file_prettyprint_with_encoding(List, '$CALLSLT/LiteContent/english_course/english_course/grammars/english_dont_expand.pl', 'UTF-8').

test_read_flat_file(restaurant) :-
	read_flat_file([[namespace=restaurant,
			 main_spec=[['$CALLSLT/LiteContent/restaurant/restaurant/grammars/restaurant.txt', [arabic, chinese, english, german, italian]]],
			 aux_spec=[]]],
		       List,
		       call),
	list_to_prolog_file_prettyprint_with_encoding(List, '$CALLSLT/LiteContent/restaurant/restaurant/grammars/restaurant_expanded.pl', 'UTF-8').

test_read_flat_file(eng_templatised_extended) :-
	read_flat_file([[namespace=user,
			 main_spec=[['$CALLSLT/Eng/corpora/english_course_flat_templatised_extended.txt', [french, german]]],
			 aux_spec=[['$CALLSLT/Eng/corpora/english_course_flat_chinese.txt', [chinese]]]
			]],
		       List,
		       call),
	list_to_prolog_file_prettyprint_with_encoding(List, '$CALLSLT/Eng/corpora/english_course_flat_templatised_extended.pl', 'UTF-8').

test_read_flat_file(visual2) :-
	read_flat_file([[namespace=visual,
			 main_spec=[['$CALLSLT/LiteContent/visual/visual2/grammars/visual2.txt', [english]]],
			 aux_spec=[]]],
			List,
		       call),
	list_to_prolog_file_prettyprint(List, '$CALLSLT/LiteContent/visual/visual2/grammars/visual2.pl').

test_read_flat_file(trainslate1) :-
	read_flat_file([[namespace=trainslate1,
			 main_spec=[['$MED_SLT2/Fre/corpora/trainslate_minicorpus.txt', [sign, english]]],
			 aux_spec=[]]],
		       List,
		       translation),
	list_to_prolog_file_prettyprint(List, '$MED_SLT2/Fre/corpora/trainslate_minicorpus.pl').

test_read_flat_file(trainslate2) :-
	read_flat_file([[namespace=trainslate1,
			 main_spec=[['$MED_SLT2/Fre/corpora/trainslate_minicorpus_phrases.txt', [sign, english]]],
			 aux_spec=[]]],
		       List,
		       translation,
		       dont_expand),
	list_to_prolog_file_prettyprint(List, '$MED_SLT2/Fre/corpora/trainslate_minicorpus_phrases.pl').

test_read_flat_file(trainslate3) :-
	read_flat_file([[namespace=trainslate1,
			 main_spec=[['$MED_SLT2/Fre/corpora/trainslate_minicorpus_phrases2.txt', [sign, english]]],
			 aux_spec=[]]],
		       List,
		       translation,
		       dont_expand),
	list_to_prolog_file_prettyprint(List, '$MED_SLT2/Fre/corpora/trainslate_minicorpus_phrases2.pl').

test_read_flat_file(trainslate4) :-
	read_flat_file([[namespace=trainslate1,
			 main_spec=[['$MED_SLT2/Fre/corpora/trainslate_french.txt', [french]],
				    ['$MED_SLT2/Fre/corpora/trainslate_cities.txt', [french, gloss, head, gaze, eyebrows, aperture, mouthing]]],
			 aux_spec=[['$MED_SLT2/Fre/corpora/trainslate_sign.csv', [gloss, head, gaze, eyebrows, aperture, mouthing]]]]],
		       List,
		       translation,
		       dont_expand),
	list_to_prolog_file_prettyprint(List, '$MED_SLT2/Fre/corpora/trainslate_modular.pl').

test_read_flat_file(hello_phrases) :-
	read_flat_file([[namespace=hello,
			 main_spec=[['$CALLSLT/LiteContent/hello/hello_world_course/grammars/hello_world_phrases.txt', [french]]],
			 aux_spec=[]]],
			List,
		       call),
	list_to_prolog_file_prettyprint(List, '$CALLSLT/LiteContent/hello/hello_world_course/grammars/hello_world_phrases.pl').

test_read_flat_file(hello_phrases_dont_expand) :-
	read_flat_file([[namespace=hello,
			 main_spec=[['$CALLSLT/LiteContent/hello/hello_world_course/grammars/hello_world_phrases.txt', [french]]],
			 aux_spec=[]]],
			List,
		       call,
		       dont_expand),
	list_to_prolog_file_prettyprint(List, '$CALLSLT/LiteContent/hello/hello_world_course/grammars/hello_world_phrases_dont_expand.pl').

test_read_flat_file(hello_phrases_dont_expand2) :-
	read_flat_file([[namespace=hello,
			 main_spec=[['$CALLSLT/LiteContent/hello/hello_world_course/grammars/hello_world_phrases2.txt', [french]]],
			 aux_spec=[]]],
			List,
		       call,
		       dont_expand),
	list_to_prolog_file_prettyprint(List, '$CALLSLT/LiteContent/hello/hello_world_course/grammars/hello_world_phrases_dont_expand2.pl').

test_read_flat_file(questionnaire_toy) :-
	read_flat_file([[namespace=user,
			 main_spec=[['$CALLSLT/Que/corpora/red_cross_grammar_small.txt', [french, spanish]]],
			 aux_spec=[]]],
		       List,
		       questionnaire),
	list_to_prolog_file_prettyprint(List, '$CALLSLT/Que/corpora/red_cross_grammar_small_expanded.pl').

test_read_flat_file(pre_units1) :-
	read_single_flat_file_to_pre_units('$CALLSLT/LiteContent/hello/hello_world_course/grammars/hello_world_phrases.txt',
					   call, any_languages,
					   List),
	list_to_prolog_file_prettyprint(List, '$CALLSLT/tmp/pre_units.pl').

test_read_flat_file(pre_units2) :-
	read_single_flat_file_to_pre_units('$CALLSLT/LiteContent/hello/hello_world_course/grammars/hello_world_phrases.txt',
					   call, any_languages,
					   List),
	list_to_prolog_file_prettyprint(List, '$CALLSLT/tmp/pre_units.pl').

test_read_flat_file(pre_units3) :-
	read_single_flat_file_to_pre_units('$CALLSLT/LiteContent/english_course/english_course/grammars/english_chinese.txt',
					   call, any_languages,
					   List),
	list_to_prolog_file_prettyprint_with_encoding(List, '$CALLSLT/tmp/pre_units.pl', 'UTF-8').

test_read_flat_file(pre_units4) :-
	read_single_flat_file_to_pre_units('$CALLSLT/LiteContent/english_course/english_course/grammars/english_chinese.txt',
					   call, default_encoding, any_languages,
					   List),
	list_to_prolog_file_prettyprint_with_encoding(List, '$CALLSLT/tmp/pre_units.pl', 'UTF-8').

test_read_flat_file(pre_units5) :-
	read_single_flat_file_to_pre_units('$CALLSLT/LiteContent/english_course/english_course/grammars/english.txt',
					   call, any_languages,
					   List),
	list_to_prolog_file_prettyprint_with_encoding(List, '$CALLSLT/tmp/pre_units.pl', 'UTF-8').

%======================================================================

read_flat_file(File, Units) :-
	read_flat_file([namespace=user,
			main_spec=[[File, [default]]],
			aux_spec=[]],
		       Units, call, flat).

read_flat_file(FileNamespaceList, Units, Type) :-
	read_flat_file(FileNamespaceList, Units, Type, flat).

read_flat_file(FileNamespaceList, Units, Type, EType) :-
	read_flat_file1(FileNamespaceList, UnitsList, Type, EType),
	append_list(UnitsList, Units),
	!.
read_flat_file(FileNamespaceList, _Units, _Type, _EType) :-
	format_lite_error('~N*** Error reading flat file(s)~n', []),
	display_file_alist(FileNamespaceList),
	!,
	fail.

read_single_flat_file_to_pre_units(File, Type, Languages, PreUnits) :-
	on_exception(_Exception,
		     read_single_flat_file_to_pre_units1(File, Type, Languages, PreUnits),
		     fail
		    ).

read_single_flat_file_to_pre_units1(File, Type, Languages, PreUnits) :-	
	check_flat_file_type(Type),
	read_flat_file_to_line_list(File, Lines),
	flat_file_lines_to_pre_units(Lines, outside_unit, PreUnits-[], Type, Languages, 1, no_error-_Error),
	!.

display_file_alist(FileNamespaceList) :-
	format('~NFile spec:~n~n', []),
	prettyprint(FileNamespaceList),
	!.

read_flat_file1([], [], _Type, _EType).
read_flat_file1([Alist | R], [Units | R1], Type, EType) :-
	read_flat_file_for_namespace(Alist, Units, Type, EType),
	!,
	read_flat_file1(R, R1, Type, EType).

read_flat_file_for_namespace(FileAlist, Units, Type, EType) :-
	check_flat_file_type(Type),
	check_expansion_type(EType),
	member(namespace=Namespace, FileAlist),
	member(main_spec=MainFiles, FileAlist),
	member(aux_spec=AuxFiles, FileAlist),
	read_flat_file_main(MainFiles, Type, EType, Namespace, []-Units0, []-MainLanguages, 0-NUnits, 0-NResponses),
	read_flat_file_aux(AuxFiles, Type, EType, Namespace, MainLanguages, Units0, Units1),
	add_namespace_and_course_to_phrase_ids(Units1, Units),
	%print_units_to_tmp_file(Units),
	check_consistency_of_units(Units, Type),
	format('~N--- Found ~d units and ~d responses for namespace "~w"~n', [NUnits, NResponses, Namespace]),
	!.
read_flat_file_for_namespace(FileAlist, _Units, _Type, _EType) :-
	format_lite_error('~N*** Error reading flat file(s) for namespace~n', []),
	display_file_alist_for_namespace(FileAlist),
	!,
	fail.

display_file_alist_for_namespace(FileAlist) :-
	format('~NFile spec:~n~n', []),
	prettyprint(FileAlist),
	!.

check_flat_file_type(Type) :-
	valid_flat_file_type(Type),
	!.
check_flat_file_type(Type) :-
	format_lite_error('~N*** Error: unknown flat file type: ~w~n', [Type]),
	fail.

valid_flat_file_type(call).
valid_flat_file_type(questionnaire).
valid_flat_file_type(translation).

check_expansion_type(Type) :-
	valid_expansion_type(Type),
	!.
check_expansion_type(Type) :-
	format_lite_error('~N*** Error: unknown expansion type: ~w~n', [Type]),
	fail.

valid_expansion_type(flat).
valid_expansion_type(dont_expand).

%======================================================================

read_flat_file_main([], _Type, _EType, _Namespace, Units-Units, Languages-Languages, NUnits-NUnits, NResponses-NResponses).
read_flat_file_main([First | Rest], Type, EType, Namespace, UnitsIn-UnitsOut, LanguagesIn-LanguagesOut, NUnitsIn-NUnitsOut, NResponsesIn-NResponsesOut) :-
	read_flat_file_unit(First, LanguagesNext, LanguagesIn, Units0, Type, EType, NUnits, NResponses),
	add_namespace_and_course_to_units(Units0, Namespace, Type, Units),
	append(UnitsIn, Units, UnitsNext),
	NUnitsNext is NUnitsIn + NUnits,
	NResponsesNext is NResponsesIn + NResponses,
	!,
	read_flat_file_main(Rest, Type, EType, Namespace, UnitsNext-UnitsOut, LanguagesNext-LanguagesOut, NUnitsNext-NUnitsOut, NResponsesNext-NResponsesOut).

read_flat_file_aux([], _Type, _EType, _Namespace, _MainLanguages, Units, Units).
read_flat_file_aux([F | R], Type, EType, Namespace, MainLanguages, UnitsIn, UnitsOut) :-
	read_flat_file_unit(F, Languages, MainLanguages, Units0, new_text(Type), EType, _NUnits, _NResponses),
	add_namespace_and_course_to_units(Units0, Namespace, Type, Units),
	combine_units(UnitsIn, MainLanguages, Languages, Units, UnitsNext),
	!,
	read_flat_file_aux(R, Type, EType, Namespace, MainLanguages, UnitsNext, UnitsOut).

combine_units([], _MainLanguages, _Languages, _Units, []).
combine_units([F | R], MainLanguages, Languages, Units, [F1 | R1]) :-
	update_unit(F, MainLanguages, Languages, Units, F1),
	!,
	combine_units(R, MainLanguages, Languages, Units, R1).

read_flat_file_unit([File, _Encoding, Languages], Languages, PreviousLanguages, Units, Type, EType, NUnits, NResponses) :-
	read_flat_file_unit([File, Languages], Languages, PreviousLanguages, Units, Type, EType, NUnits, NResponses),
	!.
read_flat_file_unit([File, Languages], Languages, PreviousLanguages, Units, Type, EType, NUnits, NResponses) :-
	maybe_preprocess_csv_lite_file(File, Type, File1),
	read_flat_file_to_line_list(File1, Lines),
	%append(Languages, PreviousLanguages, AllLanguages),
	add_new_languages_to_list(Languages, PreviousLanguages, AllLanguages),
	flat_file_lines_to_units(Lines, Units, Type, EType, AllLanguages, NUnits, NResponses, Error),
	(   Error = no_error ->
	    true
	;
	    otherwise ->
	    safe_absolute_file_name(File, AbsFile),
	    format_lite_error('~N*** Errors found in ~w~n', [AbsFile])
	).

add_new_languages_to_list([], LanguagesIn, LanguagesIn).
add_new_languages_to_list([F | R], LanguagesIn, LanguagesOut) :-
	member(F, LanguagesIn),
	!,
	add_new_languages_to_list(R, LanguagesIn, LanguagesOut).
add_new_languages_to_list([F | R], LanguagesIn, LanguagesOut) :-
	append(LanguagesIn, [F], LanguagesNext),
	!,
	add_new_languages_to_list(R, LanguagesNext, LanguagesOut).

maybe_preprocess_csv_lite_file(CSVFile, Type, OutFile) :-
	translation_or_new_text(Type),
	safe_absolute_file_name(CSVFile, AbsCSVFile), 
	pathname_has_extension(AbsCSVFile, csv),
	!,
	tmp_regulus_file('tmp_expanded_csv_file.txt', TextFile),
	(   safe_file_exists(AbsCSVFile) ->
	    find_sicstus_encoding_for_file(AbsCSVFile, Encoding),
	    read_sign_csv(CSVFile, Encoding, TextFile),
	    OutFile = TextFile
	;
	    % If we can't find the CSV file, pass it back
	    otherwise ->
	    format('~N*** Warning: file ~w not found, unable to preprocess~n', [AbsCSVFile]),
	    OutFile = CSVFile
	).
maybe_preprocess_csv_lite_file(File, _Type, File).

read_flat_file_to_line_list(File, Lines) :-
	safe_absolute_file_name(File, AbsFile),
	(   safe_file_exists(AbsFile) ->
	    find_sicstus_encoding_for_file(AbsFile, Encoding),
	    read_file_to_atom_list(AbsFile, Encoding, Lines),
	    length(Lines, NLines),
	    format('~N--- Read Lite file (~d lines, encoding = ~w) ~w~n', [NLines, Encoding, AbsFile])
	;
	    otherwise ->
	    format('~N*** Warning: file ~w not found~n', [AbsFile]),
	    Lines = []
	).

update_unit(Unit, MainLanguages, NewLanguages, Units, CombinedUnit) :-
	text_lines_for_unit(MainLanguages, Unit, TextLines),
	member(OtherUnit, Units),
	text_lines_for_unit(MainLanguages, OtherUnit, OtherTextLines),
	common_member_exists(TextLines, OtherTextLines),
	text_lines_for_unit(NewLanguages, OtherUnit, NewTextLines),
	add_new_text_lines_to_unit(Unit, NewTextLines, CombinedUnit),
	!.
update_unit(Unit, _MainLanguages, NewLanguages, _Units, Unit) :-
	maybe_warn_about_failed_update_of_unit(Unit, NewLanguages).

maybe_warn_about_failed_update_of_unit(unit(FromLine-ToLine, _List), NewLanguages) :-
	format('~N*** Warning: didn\'t find material for ~w to add to unit in lines ~d-~d~n',
	       [NewLanguages, FromLine, ToLine]),
	!.
maybe_warn_about_failed_update_of_unit(_Unit, _NewLanguages).

text_lines_for_unit(Languages, Unit, TextLines) :-
	Unit = unit(_Lines, List),
	text_lines_for_unit1(Languages, List, TextLines).

text_lines_for_unit1([], _Lines, []).
text_lines_for_unit1([F | R], Lines, [text(F)=Text | R1]) :-
	member(text(F)=Text, Lines),
	!,
	text_lines_for_unit1(R, Lines, R1).
text_lines_for_unit1([_F | R], Lines, R1) :-
	!,
	text_lines_for_unit1(R, Lines, R1).

common_member_exists(L1, L2) :-
	member(X, L1),
	member(X, L2),
	!.

add_new_text_lines_to_unit(Unit, NewTextLines, CombinedUnit) :-
	Unit = unit(Lines, List),
	add_new_text_lines_to_list(List, NewTextLines, CombinedList),
	CombinedUnit = unit(Lines, CombinedList),
	!.

add_new_text_lines_to_list([text(X)=Text | R], NewTextLines, CombinedList) :-
	append(NewTextLines, [text(X)=Text | R], CombinedList),
	!.
add_new_text_lines_to_list([F | R], NewTextLines, [F | R1]) :-
	add_new_text_lines_to_list(R, NewTextLines, R1).

%======================================================================

add_namespace_and_course_to_units(UnitsIn, Namespace, Type, UnitsOut) :-
	add_namespace_to_units(UnitsIn, Namespace, Type, UnitsNext),
	maybe_add_course_to_units(UnitsNext, Type, UnitsOut).
	    
%----------------------------------------------------------------------

add_namespace_to_units([], _Namespace, _Type, []).
add_namespace_to_units([F | R], Namespace, Type, [F1 | R1]) :-
	add_namespace_to_unit(F, Namespace, Type, F1),
	!,
	add_namespace_to_units(R, Namespace, Type, R1).

add_namespace_to_unit(UnitIn, Namespace, Type, UnitOut) :-
	add_namespace_to_unit_body(UnitIn, Namespace, Type, UnitNext),
	UnitNext = UnitOut.
	%add_namespace_to_phrase_ids(UnitNext, Namespace, UnitOut).

add_namespace_to_unit_body(template_unit(Lines, List), Namespace, _CALLOrQuestionnaire, template_unit(Lines, List1)) :-
	List1 = [namespace=Namespace | List],
	!.
add_namespace_to_unit_body(unit(Lines, List), Namespace, translation, unit(Lines, List1)) :-
	List1 = [namespace=Namespace | List],
	!.
add_namespace_to_unit_body(unit(Lines, List), Namespace, _CALLOrQuestionnaire, unit(Lines, List2)) :-
	List1 = [namespace=Namespace | List],
	lf_and_incorrect_lf_for_unit(unit(Lines, List1), LFs),
	append(List1, LFs, List2),
	!.
add_namespace_to_unit_body(group_unit(Lines, List), Namespace, _CALLOrQuestionnaire, group_unit(Lines, List1)) :-
	List1 = [namespace=Namespace | List],
	!.
add_namespace_to_unit_body(answer_unit(Lines, List), Namespace, questionnaire, answer_unit(Lines, List1)) :-
	List1 = [namespace=Namespace | List],
	!.
add_namespace_to_unit_body(multimedia_unit(Lines, List), Namespace, call, multimedia_unit(Lines, List1)) :-
	List1 = [namespace=Namespace | List],
	!.
add_namespace_to_unit_body(phrase_score_unit(Lines, List), Namespace, call, phrase_score_unit(Lines, List1)) :-
	List1 = [namespace=Namespace | List],
	!.
add_namespace_to_unit_body(phrase_unit(Lines, List), Namespace, call, phrase_unit(Lines, List1)) :-
	List1 = [namespace=Namespace | List],
	!.
add_namespace_to_unit_body(tr_phrase_unit(Lines, List), Namespace, translation, tr_phrase_unit(Lines, List1)) :-
	List1 = [namespace=Namespace | List],
	!.
add_namespace_to_unit_body(course_unit(Lines, List), Namespace, _Any, course_unit(Lines, List1)) :-
	List1 = [namespace=Namespace | List],
	!.
add_namespace_to_unit_body(coverage_unit(Lines, List), Namespace, call, unit(Lines, List2)) :-
	List1 = [namespace=Namespace | List],
	append(List1, [lf=dummy_correct, incorrect_lf=dummy_incorrect], List2),
	!.
add_namespace_to_unit_body(F, Namespace, Type, F1) :-
	format_lite_error('~N*** Error: bad call: ~w~n', [add_namespace_to_unit_body(F, Namespace, Type, F1)]),
	fail.

%----------------------------------------------------------------------

maybe_add_course_to_units(UnitsIn, Type, UnitsOut) :-
	Type = call,
	single_course_definition_in_units(UnitsIn, Course),
	!,
	add_course_to_units(UnitsIn, Course, UnitsOut).
maybe_add_course_to_units(UnitsIn, Type, UnitsOut) :-
	Type = translation,
	single_domain_definition_in_units(UnitsIn, Course),
	!,
	add_course_to_units(UnitsIn, Course, UnitsOut).
maybe_add_course_to_units(UnitsIn, _Type, UnitsIn).

single_course_definition_in_units(Units, SingleCourse) :-
	findall(Course,
		(   member(course_unit(_Lines, List), Units),
		    member(name=Course, List)),
		Courses),
	Courses = [SingleCourse],
	!.

single_domain_definition_in_units(Units, SingleCourse) :-
	findall(Course,
		(   member(course_unit(_Lines, List), Units),
		    member(name=Course, List)),
		Courses),
	Courses = [SingleCourse],
	!.
 
add_course_to_units([], _Course, []).
add_course_to_units([F | R], Course, [F1 | R1]) :-
	add_course_to_unit(F, Course, F1),
	!,
	add_course_to_units(R, Course, R1).

add_course_to_unit(UnitIn, Course, UnitOut) :-
	add_course_to_unit_body(UnitIn, Course, UnitNext),
	UnitNext = UnitOut.
	%add_course_to_phrase_ids(UnitNext, Course, UnitOut).

add_course_to_unit_body(template_unit(Lines, List), Course, template_unit(Lines, List1)) :-
	add_course_if_possible_and_consistent(List, Lines, Course, List1),
	!.
add_course_to_unit_body(unit(Lines, List), Course, unit(Lines, List1)) :-
	add_course_if_possible_and_consistent(List, Lines, Course, List1),
	!.
add_course_to_unit_body(group_unit(Lines, List), Course, group_unit(Lines, List1)) :-
	add_course_if_possible_and_consistent(List, Lines, Course, List1),
	!.
add_course_to_unit_body(multimedia_unit(Lines, List), Course, multimedia_unit(Lines, List1)) :-
	add_course_if_possible_and_consistent(List, Lines, Course, List1),
	!.
add_course_to_unit_body(phrase_unit(Lines, List), Course, phrase_unit(Lines, List1)) :-
	add_course_if_possible_and_consistent(List, Lines, Course, List1),
	!.
add_course_to_unit_body(tr_phrase_unit(Lines, List), Course, tr_phrase_unit(Lines, List1)) :-
	add_course_if_possible_and_consistent(List, Lines, Course, List1),
	!.
add_course_to_unit_body(phrase_score_unit(Lines, List), Course, phrase_score_unit(Lines, List1)) :-
	add_course_if_possible_and_consistent(List, Lines, Course, List1),
	!.
add_course_to_unit_body(coverage_unit(Lines, List), Course, coverage_unit(Lines, List1)) :-
	add_course_if_possible_and_consistent(List, Lines, Course, List1),
	!.
add_course_to_unit_body(course_unit(Lines, List), _Course, course_unit(Lines, List)) :-
	!.
add_course_to_unit_body(F, Course, F1) :-
	format_lite_error('~N*** Error: bad call: ~w~n', [add_course_to_unit_body(F, Course, F1)]),
	fail.

add_course_if_possible_and_consistent(List, _Lines, Course, List) :-
	member(domain=Course, List),
	!.
add_course_if_possible_and_consistent(List, FromLine-ToLine, Course, _List1) :-
	member(domain=Course1, List),
	Course \== Course1,
	!,
	format_lite_error('~N*** Error in lines ~d-~d: unknown course specified~n', [FromLine, ToLine, Course1]),
	fail.
add_course_if_possible_and_consistent(List, _Lines, Course, List1) :-
	List1 = [domain=Course | List],
	!.

%======================================================================

add_namespace_and_course_to_phrase_ids(In, Out) :-
	add_namespace_and_course_to_phrase_ids(In, not_found, Out).

add_namespace_and_course_to_phrase_ids(Var, _Args, Var) :-
	var(Var),
	!.
add_namespace_and_course_to_phrase_ids(Atom, _Args, Atom) :-
	atomic(Atom),
	!.
add_namespace_and_course_to_phrase_ids(List, not_found, List1) :-
	member(namespace=Namespace, List),
	member(domain=Course, List),
	add_namespace_and_course_to_phrase_ids(List, [Namespace, Course], List1),
	!.
add_namespace_and_course_to_phrase_ids(tr_phrase(Id, I), [Namespace, Course], tr_phrase(Namespace, Course, Id, I)) :-
	!.
add_namespace_and_course_to_phrase_ids(T, Args, T1) :-
	compound(T),
	functor(T, F, N),
	functor(T1, F, N),
	add_namespace_and_course_to_phrase_ids_args(N, T, Args, T1).

add_namespace_and_course_to_phrase_ids_args(I, _T, _Args, _T1) :-
	I < 1,
	!.
add_namespace_and_course_to_phrase_ids_args(I, T, Args, T1) :-
	arg(I, T, Arg),
	arg(I, T1, Arg1),
	add_namespace_and_course_to_phrase_ids(Arg, Args, Arg1),
	I1 is I - 1,
	!,
	add_namespace_and_course_to_phrase_ids_args(I1, T, Args, T1).

%add_namespace_to_phrase_ids(Var, _Namespace, Var) :-
%	var(Var),
%	!.
%add_namespace_to_phrase_ids(Atom, _Namespace, Atom) :-
%	atomic(Atom),
%	!.
%add_namespace_to_phrase_ids(phrase(Id), Namespace, phrase(Namespace, Id)) :-
%	!.
%add_namespace_to_phrase_ids(tr_phrase(Id, I), Namespace, tr_phrase(Namespace, Id, I)) :-
%	!.
%add_namespace_to_phrase_ids(T, Namespace, T1) :-
%	compound(T),
%	functor(T, F, N),
%	functor(T1, F, N),
%	add_namespace_to_phrase_ids_args(N, T, Namespace, T1).
%
%add_namespace_to_phrase_ids_args(I, _T, _Namespace, _T1) :-
%	I < 1,
%	!.
%add_namespace_to_phrase_ids_args(I, T, Namespace, T1) :-
%	arg(I, T, Arg),
%	arg(I, T1, Arg1),
%	add_namespace_to_phrase_ids(Arg, Namespace, Arg1),
%	I1 is I - 1,
%	!,
%	add_namespace_to_phrase_ids_args(I1, T, Namespace, T1).
%
%add_course_to_phrase_ids(Var, _Course, Var) :-
%	var(Var),
%	!.
%add_course_to_phrase_ids(Atom, _Course, Atom) :-
%	atomic(Atom),
%	!.
%add_course_to_phrase_ids(phrase(Namespace, Id), Course, phrase(Namespace, Course, Id)) :-
%	!.
%add_course_to_phrase_ids(tr_phrase(Namespace, Id, I), Course, tr_phrase(Namespace, Course, Id, I)) :-
%	!.
%add_course_to_phrase_ids(T, Course, T1) :-
%	compound(T),
%	functor(T, F, N),
%	functor(T1, F, N),
%	add_course_to_phrase_ids_args(N, T, Course, T1).
%
%add_course_to_phrase_ids_args(I, _T, _Course, _T1) :-
%	I < 1,
%	!.
%add_course_to_phrase_ids_args(I, T, Course, T1) :-
%	arg(I, T, Arg),
%	arg(I, T1, Arg1),
%	add_course_to_phrase_ids(Arg, Course, Arg1),
%	I1 is I - 1,
%	!,
%	add_course_to_phrase_ids_args(I1, T, Course, T1).

%======================================================================

lf_and_incorrect_lf_for_unit(unit(_Lines, List), [lf=LF, incorrect_lf=IncorrectLF]) :-
	member(namespace=Namespace, List),
	member(responses=Responses, List),
	Responses = [Response | _R],
	response_to_lf(Response, LF),
	member(namespace=Namespace, List),
	format_to_atom('incorrect_version_of_~w', [LF], IncorrectLF),
	!.
lf_and_incorrect_lf_for_unit(_Other, []).

response_to_lf(ResponseAtom, LF) :-
	atom(ResponseAtom),
	atom_codes(ResponseAtom, Str),
	clean_up_str_for_lf(Str, Str1),
	atom_codes(LF, Str1).
response_to_lf(ResponseList, LF) :-
	is_list(ResponseList),
	expand_and_remove_stars(ResponseList, flat, _InTemplateP, ResponseAtom, no_stars-no_stars),
	response_to_lf(ResponseAtom, LF),
	!.
response_to_lf(ResponseAtom, LF) :-
	format_lite_error('~N*** Bad call: ~w~n', [response_to_lf(ResponseAtom, LF)]),
	fail.

clean_up_str_for_lf(Str, Str2) :-
	clean_up_str_for_lf1(Str, Str1),
	truncate_str_for_lf(Str1, Str2).

clean_up_str_for_lf1([], []).
clean_up_str_for_lf1([F | R], [F1 | R1]) :-
	(   bad_char_for_lf(F) ->
	    F1 = 0'_
	;
	    otherwise ->
	    F1 = F
	),
	!,
	clean_up_str_for_lf1(R, R1).

bad_char_for_lf(0' ).
bad_char_for_lf(0'\').
bad_char_for_lf(0'-).

truncate_str_for_lf(Str1, Str2) :-
	length(Str1, N),
	N < 100,
	Str2 = Str1,
	!.
truncate_str_for_lf(Str1, Str2) :-
	prefix_length(Str1, Prefix, 95),
	append(Prefix, "_etc", Str2),
	!.
	
%======================================================================

check_consistency_of_units(_Units, call).
check_consistency_of_units(_Units, translation).
check_consistency_of_units(Units, questionnaire) :-
	all_namespaces_in_units(Units, Namespaces),
	check_consistency_of_units_for_questionnaire(Namespaces, Units).

check_consistency_of_units_for_questionnaire([], _Units).
check_consistency_of_units_for_questionnaire([F | R], Units) :-
	check_consistency_of_units_for_questionnaire_and_namespace(F, Units),
	!,
	check_consistency_of_units_for_questionnaire(R, Units).

check_consistency_of_units_for_questionnaire_and_namespace(Namespace, Units) :-
	all_groups_are_defined(Namespace, Units),
	all_answers_are_defined(Namespace, Units),
	all_fillers_in_group_have_nexts(Namespace, Units),
	all_nexts_are_to_well_defined_groups(Namespace, Units),
	!.

all_namespaces_in_units(Units, Namespaces) :-
	findall(Namespace,
		(   member(Unit, Units),
		    namespace_in_unit_etc(Unit, Namespace)
		),
		Namespaces0),
	sort(Namespaces0, Namespaces).

namespace_in_unit_etc(unit(_Lines, List), Namespace) :-
	member(namespace=Namespace, List),
	!.
namespace_in_unit_etc(group_unit(_Lines, List), Namespace) :-
	member(namespace=Namespace, List),
	!.
namespace_in_unit_etc(answer_unit(_Lines, List), Namespace) :-
	member(namespace=Namespace, List),
	!.

%---------------------------------

all_groups_are_defined(Namespace, Units) :-
	complain_about_undefined_group(Namespace, Units).

complain_about_undefined_group(Namespace, Units) :-
	member(unit(FromLine-ToLine, List), Units),
	member(namespace=Namespace, List),
	member(group=Group, List),
	\+ group_defined_in_units(Group, Namespace, Units),
	!,
	format_lite_error('~N*** Error in lines ~d-~d: group ~w is not defined (namespace "~w")~n', [FromLine, ToLine, Group, Namespace]),
	fail.
complain_about_undefined_group(_Namespace, _Units).

group_defined_in_units(Group, Namespace, Units) :-
	member(group_unit(_Lines, List), Units),
	member(namespace=Namespace, List),
	member(name=Group, List),
	!.

%---------------------------------

all_answers_are_defined(Namespace, Units) :-
	complain_about_undefined_answer_in_unit(Namespace, Units),
	complain_about_undefined_answer_in_group(Namespace, Units).

complain_about_undefined_answer_in_unit(Namespace, Units) :-
	member(unit(FromLine-ToLine, List), Units),
	member(namespace=Namespace, List),
	member(answers=Answers, List),
	member(_Answer=Filler, Answers),
	\+ answer_defined_in_units(Filler, Namespace, Units),
	!,
	format_lite_error('~N*** Error in lines ~d-~d: answer ~w is not defined (namespace "~w")~n', [FromLine, ToLine, Filler, Namespace]),
	fail.
complain_about_undefined_answer_in_unit(_Namespace, _Units).

complain_about_undefined_answer_in_group(Namespace, Units) :-
	member(group_unit(FromLine-ToLine, List), Units),
	member(namespace=Namespace, List),
	member(fillers=Fillers, List),
	member(Filler, Fillers),
	\+ answer_defined_in_units(Filler, Namespace, Units),
	!,
	format_lite_error('~N*** Error in lines ~d-~d: answer ~w is not defined (namespace "~w")~n', [FromLine, ToLine, Filler, Namespace]),
	fail.
complain_about_undefined_answer_in_group(_Namespace, _Units).

answer_defined_in_units(Answer, Namespace, Units) :-
	member(answer_unit(_Lines, List), Units),
	member(namespace=Namespace, List),
	member(content=Answer, List),
	!.

%---------------------------------

all_fillers_in_group_have_nexts(Namespace, Units) :-
	complain_about_filler_with_no_next(Namespace, Units).

complain_about_filler_with_no_next(Namespace, Units) :-
	member(group_unit(FromLine-ToLine, List), Units),
	member(namespace=Namespace, List),
	member(fillers=Fillers0, List),
	Fillers = [skip | Fillers0],
	member(Filler, Fillers),
	\+ next_for_filler_defined_in_group(Filler, List),
	!,
	format_lite_error('~N*** Error in lines ~d-~d: no "Next" defined for "~w" (namespace "~w")~n', [FromLine, ToLine, Filler, Namespace]),
	fail.
complain_about_filler_with_no_next(_Namespace, _Units).

next_for_filler_defined_in_group(Filler, List) :-
	member(next=[_Target, Triggers], List),
	(   Triggers = any
	;
	    member(Filler, Triggers)
	),
	!.

%---------------------------------

all_nexts_are_to_well_defined_groups(Namespace, Units) :-
	complain_about_next_with_no_target(Namespace, Units).

complain_about_next_with_no_target(Namespace, Units) :-
	member(group_unit(FromLine-ToLine, List), Units),
	member(namespace=Namespace, List),
	member(domain=Questionnaire, List),
	member(next=[Target, _Triggers], List),
	\+ group_is_in_questionnaire(Target, Namespace, Questionnaire, Units),
	!,
	format_lite_error('~N*** Error in lines ~d-~d: the group "~w" is not defined for questionnaire "~w"~n',
	       [FromLine, ToLine, Target, Questionnaire]),
	fail.
complain_about_next_with_no_target(_Namespace, _Units).

group_is_in_questionnaire(Group, _Namespace, _Questionnaire, _Units) :-
	Group = 'Exit',
	!.
group_is_in_questionnaire(Group, Namespace, Questionnaire, Units) :-
	member(group_unit(_Lines, List), Units),
	member(namespace=Namespace, List),
	member(domain=Questionnaire, List),
	member(name=Group, List),
	!.

%======================================================================

flat_file_lines_to_units(Lines, Units, Type, EType, Languages, NUnits, NResponses, Error) :-
	strip_comments_from_lines(Lines, Lines1),
	flat_file_lines_to_pre_units(Lines1, outside_unit, PreUnits-[], Type, Languages, 1, no_error-Error),
	Error = no_error,
	internalise_phrases(PreUnits, Type, EType, Languages, PreUnits1),
	internalise_templates(PreUnits1, Type, EType, Languages, PreUnits2),
	expand_template_applications(PreUnits2, PreUnits3),
	pre_units_to_units(PreUnits3, Type, EType, Languages, Units0),
	remove_duplicated_incorrect_responses(Units0, EType, Units),
	length(Units, NUnits),
	count_responses_in_units(Units, NResponses),
	!.
flat_file_lines_to_units(_Lines, Units, _Type, _EType, _Languages, NUnits, NResponses, Error) :-
	Units = [],
	NUnits = 0,
	NResponses = 0,
	Error = error.

strip_comments_from_lines([], []).
strip_comments_from_lines([F | R], [F1 | R1]) :-
	strip_comment_from_line(F, F1),
	!,
	strip_comments_from_lines(R, R1).

strip_comment_from_line('', '') :-
	!.
strip_comment_from_line(Line, Line1) :-
	split_atom_into_words(Line, 0'#, Components),
	Components = [Line1 | _],
	!.
% Shouldn't be necessary, but just in case.
strip_comment_from_line(Line, Line).

count_responses_in_units(Units, NResponses) :-
	findall(Response,
		(   member(Unit, Units),
		    Unit = unit(_Lines, List),
		    member(responses=Responses, List),
		    member(Response, Responses)
		),
		AllResponses),
	length(AllResponses, NResponses).

flat_file_lines_to_pre_units([], outside_unit, Out-Out, _Type, _Languages, _I, Ein-Ein) :-
	!.
flat_file_lines_to_pre_units([], inside_unit(UnitType, _I1, _Content), Out-Out, Type, _Languages, _I2, _Ein-error) :-
	unit_type_to_print_form(UnitType, Type, UnitType1),
	format_lite_error('~N*** Error: file ended inside ~w~n', [UnitType1]).
% For debugging
%flat_file_lines_to_pre_units([F | _R], _State, _InOut, _Type, _Languages, I, _EinEout) :-
%	format('~N~d: ~w~n', [I, F]),
%	fail.
flat_file_lines_to_pre_units([F | R], State, In-Out, Type, Languages, I, Ein-Eout) :-
	is_comment_line(F),
	!,
	I1 is I + 1,
	flat_file_lines_to_pre_units(R, State, In-Out, Type, Languages, I1, Ein-Eout).
flat_file_lines_to_pre_units([F | R], outside_unit, In-Out, Type, Languages, I, Ein-Eout) :-
	is_start_line(UnitName, F, Type),
	!,
	I1 is I + 1,
	flat_file_lines_to_pre_units(R, inside_unit(UnitName, I, []), In-Out, Type, Languages, I1, Ein-Eout).
flat_file_lines_to_pre_units([F | R], outside_unit, In-Out, Type, Languages, I, Ein-Eout) :-
	is_start_line(template_unit(TemplateType), F, Type, NameAndParameters),
	!,
	I1 is I + 1,
	flat_file_lines_to_pre_units(R, inside_unit(template_unit(TemplateType, NameAndParameters), I, []), In-Out, Type, Languages, I1, Ein-Eout).
flat_file_lines_to_pre_units([F | R], outside_unit, [F1 | Next]-Out, Type, Languages, I, Ein-Eout) :-
	is_template_application_line(F, Type),
	pre_parse_line(F, _Tag=TemplateApplicationAtom, Type, Languages, I, Ein-Enext),
	F1 = template_application(I, TemplateApplicationAtom),
	!,
	I1 is I + 1,
	flat_file_lines_to_pre_units(R, outside_unit, Next-Out, Type, Languages, I1, Enext-Eout).
flat_file_lines_to_pre_units([F | R], outside_unit, In-Out, Type, Languages, I, _Ein-Eout) :-
	format_lite_error('~N*** Error: unknown line ~d "~w"~n', [I, F]),
	!,
	I1 is I + 1,
	flat_file_lines_to_pre_units(R, outside_unit, In-Out, Type, Languages, I1, error-Eout).
flat_file_lines_to_pre_units([F | R], inside_unit(UnitName, StartLine, Content), In-Out, Type, Languages, I, Ein-Eout) :-
	is_end_line(UnitName, F, Type),
	!,
	Unit =.. [UnitName, StartLine-I, Content],
	In = [Unit | Next],
	I1 is I + 1,
	flat_file_lines_to_pre_units(R, outside_unit, Next-Out, Type, Languages, I1, Ein-Eout).
flat_file_lines_to_pre_units([F | R], inside_unit(template_unit(TemplateType, NameAndParameters), StartLine, Content), In-Out, Type, Languages, I, Ein-Eout) :-
	is_end_line(template_unit(TemplateType), F, Type),
	!,
	In = [template_unit(StartLine-I, TemplateType, NameAndParameters, Content) | Next],
	I1 is I + 1,
	flat_file_lines_to_pre_units(R, outside_unit, Next-Out, Type, Languages, I1, Ein-Eout).
flat_file_lines_to_pre_units([F | R], inside_unit(UnitName, _StartLine, _Content), In-Out, Type, Languages, I, _Ein-Eout) :-
	atomic(UnitName),
	is_end_line(OtherUnitName, F, Type),
	UnitName \== OtherUnitName,
	!,
	unit_type_to_print_form(UnitName, Type, PrintForm),
	format_lite_error('~N*** Error: ~w definition terminated by ~w in line ~d~n', [PrintForm, F, I]),
	I1 is I + 1,
	flat_file_lines_to_pre_units(R, outside_unit, In-Out, Type, Languages, I1, error-Eout).
flat_file_lines_to_pre_units([F | R], inside_unit(template_unit(TemplateType, Parameters), _StartLine, _Content), In-Out, Type, Languages, I, _Ein-Eout) :-
	is_end_line(OtherUnitName, F, Type),
	OtherUnitName \== template_unit(TemplateType),
	!,
	unit_type_to_print_form(template_unit(TemplateType, Parameters), Type, PrintForm),
	format_lite_error('~N*** Error: ~w terminated by ~w in line ~d~n', [PrintForm, F, I]),
	I1 is I + 1,
	flat_file_lines_to_pre_units(R, outside_unit, In-Out, Type, Languages, I1, error-Eout).
flat_file_lines_to_pre_units([F | R], inside_unit(UnitType, StartLine, Content), In-Out, Type, Languages, I, Ein-Eout) :-
	pre_parse_line(F, F1, Type, Languages, I, Ein-Enext),
	!,
	append(Content, [F1], Content1),
	I1 is I + 1,
	flat_file_lines_to_pre_units(R, inside_unit(UnitType, StartLine, Content1), In-Out, Type, Languages, I1, Enext-Eout).
flat_file_lines_to_pre_units([F | R], inside_unit(UnitType, _StartLine, _Content), In-Out, Type, Languages, I, _Ein-Eout) :-
	unit_type_to_print_form(UnitType, Type, UnitType1),
	format_lite_error('~N*** Error: unknown line ~d "~w" in ~w~n', [I, F, UnitType1]),
	!,
	I1 is I + 1,
	flat_file_lines_to_pre_units(R, outside_unit, In-Out, Type, Languages, I1, error-Eout).

is_start_line(unit, Line, Type) :-
	call_or_new_text(Type),
	is_single_word_of_which_lowercase_form_is(Line, prompt).
is_start_line(unit, Line, questionnaire) :-
	is_single_word_of_which_lowercase_form_is(Line, question).
is_start_line(unit, Line, Type) :-
	translation_or_new_text(Type),
	is_single_word_of_which_lowercase_form_is(Line, utterance).

is_start_line(group_unit, Line, call) :-
	is_single_word_of_which_lowercase_form_is(Line, lesson).
is_start_line(group_unit, Line, questionnaire) :-
	is_single_word_of_which_lowercase_form_is(Line, group).

is_start_line(answer_unit, Line, questionnaire) :-
	is_single_word_of_which_lowercase_form_is(Line, answer).

is_start_line(multimedia_unit, Line, call) :-
	is_single_word_of_which_lowercase_form_is(Line, multimedia).

is_start_line(coverage_unit, Line, call) :-
	is_single_word_of_which_lowercase_form_is(Line, coverage).

is_start_line(phrase_unit, Line, call) :-
	is_single_word_of_which_lowercase_form_is(Line, phrase).

is_start_line(tr_phrase_unit, Line, translation) :-
	is_single_word_of_which_lowercase_form_is(Line, trphrase).

is_start_line(phrase_score_unit, Line, call) :-
	is_single_word_of_which_lowercase_form_is(Line, phrasescore).

is_start_line(course_unit, Line, call) :-
	is_single_word_of_which_lowercase_form_is(Line, course).
is_start_line(course_unit, Line, translation) :-
	is_single_word_of_which_lowercase_form_is(Line, domain).

is_start_line(template_unit(TemplateType), Line, Type, NameAndParameters) :-
	split_off_initial_word_and_following_whitespaces(Line, Tag0, NameAndParameters),
	lowercase_atom(Tag0, Tag),
	(   Type = call, Tag = prompttemplate, TemplateType = unit
	;
	    Type = questionnaire, Tag = questiontemplate, TemplateType = unit
	;
	    Type = translation, Tag = utterancetemplate, TemplateType = unit
	;
	    Type = translation, Tag = trphrasetemplate, TemplateType = tr_phrase
	).

is_end_line(unit, Line, Type) :-
	call_or_new_text(Type),
	is_single_word_of_which_lowercase_form_is(Line, endprompt).
is_end_line(unit, Line, questionnaire) :-
	is_single_word_of_which_lowercase_form_is(Line, endquestion).
is_end_line(unit, Line, Type) :-
	translation_or_new_text(Type),
	is_single_word_of_which_lowercase_form_is(Line, endutterance).

is_end_line(group_unit, Line, call) :-
	is_single_word_of_which_lowercase_form_is(Line, endlesson).
is_end_line(group_unit, Line, questionnaire) :-
	is_single_word_of_which_lowercase_form_is(Line, endgroup).

is_end_line(answer_unit, Line, questionnaire) :-
	is_single_word_of_which_lowercase_form_is(Line, endanswer).

is_end_line(multimedia_unit, Line, call) :-
	is_single_word_of_which_lowercase_form_is(Line, endmultimedia).

is_end_line(coverage_unit, Line, call) :-
	is_single_word_of_which_lowercase_form_is(Line, endcoverage).

is_end_line(phrase_unit, Line, call) :-
	is_single_word_of_which_lowercase_form_is(Line, endphrase).

is_end_line(tr_phrase_unit, Line, translation) :-
	is_single_word_of_which_lowercase_form_is(Line, endtrphrase).

is_end_line(phrase_score_unit, Line, call) :-
	is_single_word_of_which_lowercase_form_is(Line, endphrasescore).

is_end_line(course_unit, Line, call) :-
	is_single_word_of_which_lowercase_form_is(Line, endcourse).
is_end_line(course_unit, Line, translation) :-
	is_single_word_of_which_lowercase_form_is(Line, enddomain).

is_end_line(template_unit(unit), Line, call) :-
	is_single_word_of_which_lowercase_form_is(Line, endprompttemplate).
is_end_line(template_unit(unit), Line, questionnaire) :-
	is_single_word_of_which_lowercase_form_is(Line, endquestiontemplate).
is_end_line(template_unit(unit), Line, translation) :-
	is_single_word_of_which_lowercase_form_is(Line, endutterancetemplate).
is_end_line(template_unit(tr_phrase), Line, translation) :-
	is_single_word_of_which_lowercase_form_is(Line, endtrphrasetemplate).

is_template_application_line(Line, _Type) :-
	split_off_initial_word_and_following_whitespaces(Line, Tag0, _MainLine),
	lowercase_atom(Tag0, Tag),
	Tag = applytemplate.


is_single_word_of_which_lowercase_form_is(Line, LowercaseWord) :-
	split_atom_into_words(Line, Words),
	Words = [Word],
	lowercase_atom(Word, Word1),
	Word1 = LowercaseWord.

call_or_new_text(call).
call_or_new_text(new_text(call)).

translation_or_new_text(translation).
translation_or_new_text(new_text(translation)).

unit_type_to_print_form(unit, call, 'Prompt') :- !.
unit_type_to_print_form(unit, new_text(call), 'Prompt') :- !.
unit_type_to_print_form(unit, questionnaire, 'Question') :- !.
unit_type_to_print_form(unit, translation, 'Utterance') :- !.
unit_type_to_print_form(unit, new_text(translation), 'Utterance') :- !.

unit_type_to_print_form(group_unit, call, 'Lesson') :- !.
unit_type_to_print_form(group_unit, questionnaire, 'Group') :- !.

unit_type_to_print_form(answer_unit, _Type, 'Answer') :- !.

unit_type_to_print_form(multimedia_unit, _Type, 'Multimedia') :- !.

unit_type_to_print_form(coverage_unit, _Type, 'Coverage') :- !.

unit_type_to_print_form(phrase_unit, _Type, 'Phrase') :- !.

unit_type_to_print_form(tr_phrase_unit, _Type, 'TrPhrase') :- !.

unit_type_to_print_form(phrase_score_unit, _Type, 'PhraseScore') :- !.

unit_type_to_print_form(course_unit, call, 'Course') :- !.
unit_type_to_print_form(course_unit, translatio, 'Domain') :- !.

unit_type_to_print_form(template_unit(unit, _Parameters), call, 'PromptTemplate') :- !.
unit_type_to_print_form(template_unit(unit, _Parameters), questionnaire, 'QuestionTemplate') :- !.
unit_type_to_print_form(template_unit(unit, _Parameters), translation, 'UtteranceTemplate') :- !.
unit_type_to_print_form(template_unit(tr_phrase, _Parameters), translation, 'TrPhraseTemplate') :- !.

unit_type_to_print_form(Other, _Type, Other).

pre_parse_line(Atom, Tag = MainLine, Type, Languages, _I, Ein-Eout) :-
	split_off_initial_word_and_following_whitespaces(Atom, Tag0, MainLine),
	parse_tag(Tag0, Type, Languages, Tag, Ein-Eout),
	!.
pre_parse_line(Atom, _Result, _Type, _Languages, I, _Ein-error) :-
	format_lite_error('~N*** Error in line ~d, "~w"~n', [I, Atom]),
	fail,
	!.

parse_tag(Tag0, Type, Languages, Tag, Ein-Eout) :-
	lowercase_atom(Tag0, Tag1),
	split_atom_into_words(Tag1, 0'/, TagComponents),
	(   (   call_or_new_text(Type),
		TagComponents = [text, Lang0],
		lowercase_atom(Lang0, Lang),
		( Languages = any_languages ; member(Lang, Languages) )
	    ) ->
	    Tag = text(Lang), Ein = Eout
	;
	    (   TagComponents = [helpfile, Lang0],
		lowercase_atom(Lang0, Lang),
		( Languages = any_languages ; member(Lang, Languages) )
	    ) ->
	    Tag = helpfile(Lang), Ein = Eout
	;
	    ( call_or_new_text(Type), TagComponents = [text], member(default, Languages)) ->
	    Tag = text(default), Ein = Eout
	;
	    ( Type = questionnaire, TagComponents = [translation, Lang0], lowercase_atom(Lang0, Lang), member(Lang, Languages) ) ->
	    Tag = text(Lang), Ein = Eout
	;
	    (  Type = questionnaire, TagComponents = [translation], member(default, Languages)) ->
	    Tag = text(default), Ein = Eout
		;
	    ( translation_or_new_text(Type), TagComponents = [target, Lang0], lowercase_atom(Lang0, Lang), member(Lang, Languages) ) ->
	    Tag = text(Lang), Ein = Eout
	;
	    ( translation_or_new_text(Type), TagComponents = [target], member(default, Languages)) ->
	    Tag = text(default), Ein = Eout
	;
	    TagComponents = [Atom] ->
	    Tag = Atom, Ein = Eout
	;
	    otherwise ->
	    format_lite_error('~N*** Error: unknown tag "~w"~n', [Tag0]), error = Eout
	),
	!.

split_off_initial_word_and_following_whitespaces(Atom, Tag, MainLine) :-
	atom_codes(Atom, Str),
	split_off_initial_word(Str, TagStr, RestStr0),
	split_off_whitespaces(RestStr0, RestStr1),
	remove_trailing_whitespaces(RestStr1, RestStr2),
	atom_codes(Tag, TagStr),
	atom_codes(MainLine, RestStr2),
	!.

split_off_initial_word([F | R], [], [F | R]) :-
	whitespace_char(F),
	!.
split_off_initial_word([F | R], [F | RFirst], Rest) :-
	split_off_initial_word(R, RFirst, Rest).

split_off_whitespaces([], []).
split_off_whitespaces([F | R], [F | R]) :-
	\+ whitespace_char(F),
	!.
split_off_whitespaces([_F | R], Rest) :-
	!,
	split_off_whitespaces(R, Rest).

remove_trailing_whitespaces([], []).
remove_trailing_whitespaces(Str, []) :-
	all_whitespaces(Str),
	!.
remove_trailing_whitespaces([F | R], [F | R1]) :-
	!,
	remove_trailing_whitespaces(R, R1).

all_whitespaces([]).
all_whitespaces([F | R]) :-
	whitespace_char(F),
	!,
	all_whitespaces(R).

is_comment_line(Line) :-
	atom_codes(Line, Str),
	split_off_whitespaces(Str, RestStr),
	(   RestStr = [F | _],
	    comment_char(F)
	;
	    RestStr = []
	).

comment_char(0'#).
comment_char(0'%).

%======================================================================

internalise_phrases(PreUnitsIn, Type, EType, Languages, PreUnitsOut) :-
	init_internalise_phrases,
	internalise_phrases1(PreUnitsIn, Type, EType, Languages, PreUnitsOut, 0-N, 0-N1),
	(   N = 0 ->
	    %format('~N--- No phrases found~n', [])
	    true
	;
	    otherwise ->
	    format('~N--- ~d phrases internalised~n', [N])
	),
	(   N1 = 0 ->
	    %format('~N--- No TrPhrases found~n', [])
	    true
	;
	    otherwise ->
	    format('~N--- ~d TrPhrases internalised~n', [N])
	),
	check_no_phrase_cycles.

init_internalise_phrases :-
	retractall(flat_phrase(_, _)),
	retractall(flat_tr_phrase(_, _)).

%======================================================================

internalise_phrases1([], _Type, _EType, _Languages, [], N-N, N1-N1).
internalise_phrases1([F | R], Type, EType, Languages, Result, In-Out, In1-Out1) :-
	F = phrase_unit(_Lines, _Content),
	internalise_phrase(F, Type, EType, Languages),
	(   EType = flat ->
	    Result = R1
	;
	    otherwise ->
	    Result = [F | R1]
	),
	Next is In + 1,
	!,
	internalise_phrases1(R, Type, EType, Languages, R1, Next-Out, In1-Out1).
internalise_phrases1([F | R], Type, EType, Languages, Result, In-Out, In1-Out1) :-
	F = tr_phrase_unit(_Lines, _Content),
	internalise_tr_phrase(F, Type, EType, Languages),
	(   EType = flat ->
	    Result = R1
	;
	    otherwise ->
	    Result = [F | R1]
	),
	Next is In + 1,
	!,
	internalise_phrases1(R, Type, EType, Languages, R1, Next-Out, In1-Out1).
internalise_phrases1([F | R], Type, EType, Languages, [F | R1], In-Out, In1-Out1) :-
	!,
	internalise_phrases1(R, Type, EType, Languages, R1, In-Out, In1-Out1).

internalise_phrase(PhraseUnit, Type, EType, Languages) :-
	pre_unit_to_unit(PhraseUnit, Type, EType, Languages, inside_phrase, PhraseUnit1),
	internalise_phrase1(PhraseUnit1, PhraseUnit2),
	store_phrase_unit(PhraseUnit2),
	!.
internalise_phrase(phrase_unit(FromLine-ToLine, _Type, _EType, _Languages)) :-
	format_lite_error('~N*** Error in lines ~d-~d: unable to internalize phrase.~n', [FromLine, ToLine]),
	fail.

internalise_tr_phrase(PhraseUnit, Type, EType, Languages) :-
	pre_unit_to_unit(PhraseUnit, Type, EType, Languages, inside_phrase, PhraseUnit1),
	internalise_tr_phrase1(PhraseUnit1, PhraseUnit2),
	store_phrase_unit(PhraseUnit2),
	!.
internalise_tr_phrase(tr_phrase_unit(FromLine-ToLine, _Type, _EType, _Languages)) :-
	format_lite_error('~N*** Error in lines ~d-~d: unable to internalize TrPhrase.~n', [FromLine, ToLine]),
	fail.

internalise_phrase1(PhraseUnit1, PhraseUnit2) :-
	PhraseUnit1 = phrase_unit(_Lines, List),
	member(phraseid=PhraseId, List),
	member(responses=Responses, List),
	responses_to_disjunction(Responses, Disjunction),
	PhraseUnit2 = flat_phrase(PhraseId, Disjunction),
	!.

internalise_tr_phrase1(PhraseUnit1, PhraseUnit2) :-
	PhraseUnit1 = tr_phrase_unit(_Lines, List),
	member(trphraseid=PhraseId, List),
	%member(responses=Responses, List),
	%responses_to_disjunction(Responses, Disjunction),
	PhraseUnit2 = flat_tr_phrase(PhraseId, no_info),
	!.

responses_to_disjunction([X], X) :-
	!.
responses_to_disjunction([F | R], or(F, R1)) :-
	!,
	responses_to_disjunction(R, R1).
responses_to_disjunction(Responses, Disjunction) :-
	format_lite_error('~N*** Error: bad call: ~w~n',
	       [responses_to_disjunction(Responses, Disjunction)]),
	fail.

store_phrase_unit(flat_phrase(PhraseId, Disjunction)) :-
	flat_phrase(PhraseId, Disjunction),
	!.
store_phrase_unit(flat_phrase(PhraseId, Disjunction)) :-
	assertz(flat_phrase(PhraseId, Disjunction)),
	!.
store_phrase_unit(flat_tr_phrase(PhraseId, Disjunction)) :-
	flat_tr_phrase(PhraseId, Disjunction),
	!.
store_phrase_unit(flat_tr_phrase(PhraseId, Disjunction)) :-
	assertz(flat_tr_phrase(PhraseId, Disjunction)),
	!.

%======================================================================

:- dynamic accessible_from_phrase/2.

check_no_phrase_cycles :-
	init_check_no_phrase_cycles,
	all_phrase_ids(PhraseIds),
	all_phrase_id_rhs_phrases_pairs(PhraseIds, PhraseIdRHSPhrasesPairs),
	store_accessibility_from_phrase_id_rhs_phrases_pairs(PhraseIdRHSPhrasesPairs),
	extend_accessibility_from_phrase_id_rhs_phrases_pairs(PhraseIdRHSPhrasesPairs),
	!,
	(   phrase_can_access_itself(BadPhrase) ->
	    format_lite_error('~N*** Error: cycle of phrases for phrase "~w"~n', [BadPhrase]),
	    fail
	;
	    otherwise ->
	    format('~N--- Checked phrases for cycles, none found~n', [])
	).
check_no_phrase_cycles :-
	format_lite_error('~N*** Error: unable to check phrases for cycles~n', []),
	fail.

init_check_no_phrase_cycles :-
	retractall(accessible_from_phrase(_, _)).

all_phrase_ids(PhraseIds) :-
	findall(PhraseId,
		flat_phrase(PhraseId, _Disjunction),
		PhraseIds).

all_phrase_id_rhs_phrases_pairs([], []).
all_phrase_id_rhs_phrases_pairs([F | R], [F-PhraseIds | R1]) :-
	findall(PhraseId,
		(   flat_phrase(F, Disjunction),
		    phrase_id_in_phrase_rhs(Disjunction, PhraseId)
		),
		PhraseIds0),
	sort(PhraseIds0, PhraseIds),
	!,
	all_phrase_id_rhs_phrases_pairs(R, R1).

phrase_id_in_phrase_rhs([F | R], PhraseId) :-
	(   phrase_id_in_phrase_rhs(F, PhraseId)
	;
	    phrase_id_in_phrase_rhs(R, PhraseId)
	).
phrase_id_in_phrase_rhs(or(F, R), PhraseId) :-
	(   phrase_id_in_phrase_rhs(F, PhraseId)
	;
	    phrase_id_in_phrase_rhs(R, PhraseId)
	).
phrase_id_in_phrase_rhs(optional(F), PhraseId) :-
	phrase_id_in_phrase_rhs(F, PhraseId).
phrase_id_in_phrase_rhs(phrase(PhraseId), PhraseId).

store_accessibility_from_phrase_id_rhs_phrases_pairs([]).
store_accessibility_from_phrase_id_rhs_phrases_pairs([LHS-RHS | R]) :-
	store_accessibility_from_phrase_id_rhs_phrases_pairs1(RHS, LHS),
	!,
	store_accessibility_from_phrase_id_rhs_phrases_pairs(R).

store_accessibility_from_phrase_id_rhs_phrases_pairs1([], _LHS) :-
	!.
store_accessibility_from_phrase_id_rhs_phrases_pairs1([F | R], LHS) :-
	(   accessible_from_phrase(LHS, F) ->
	    true
	;
	    assertz(accessible_from_phrase(LHS, F))
	),
	!,
	store_accessibility_from_phrase_id_rhs_phrases_pairs1(R, LHS).

extend_accessibility_from_phrase_id_rhs_phrases_pairs([]).
extend_accessibility_from_phrase_id_rhs_phrases_pairs([F | R]) :-
	extend_accessibility_from_phrase_id_rhs_phrases_pair(F),
	!,
	extend_accessibility_from_phrase_id_rhs_phrases_pairs(R).

extend_accessibility_from_phrase_id_rhs_phrases_pair(LHS-RHS) :-
	findall(LHS1,
		accessible_from_phrase(LHS1, LHS),
		LHS1s),
	extend_accessibility_from_phrase_id_rhs_phrases_pair1(LHS1s, RHS),
	!.

extend_accessibility_from_phrase_id_rhs_phrases_pair1([], _RHS).
extend_accessibility_from_phrase_id_rhs_phrases_pair1([F | R], RHS) :-
	extend_accessibility_from_phrase_id_rhs_phrases_pair2(F, RHS),
	!,
	extend_accessibility_from_phrase_id_rhs_phrases_pair1(R, RHS).

extend_accessibility_from_phrase_id_rhs_phrases_pair2(_LHS, []).
extend_accessibility_from_phrase_id_rhs_phrases_pair2(LHS, [F | R]) :-
	extend_accessibility_from_phrase_id_rhs_phrases_pair3(LHS, F),
	!,
	extend_accessibility_from_phrase_id_rhs_phrases_pair2(LHS, R).

extend_accessibility_from_phrase_id_rhs_phrases_pair3(LHS, RHS) :-
	(   accessible_from_phrase(LHS, RHS) ->
	    true
	;
	    assertz(accessible_from_phrase(LHS, RHS))
	),
	!.

phrase_can_access_itself(BadPhrase) :-
	accessible_from_phrase(BadPhrase, BadPhrase).
	
%======================================================================


internalise_templates(PreUnitsIn, Type, EType, Languages, PreUnitsOut) :-
	init_internalise_templates,
	internalise_templates1(PreUnitsIn, Type, EType, Languages, PreUnitsOut, 0-N),
	(   N = 0 ->
	    format('~N--- No templates found~n', [])
	;
	    otherwise ->
	    format('~N--- ~d templates internalised~n', [N])
	).

init_internalise_templates :-
	retractall(flat_template(_, _, _, _)).

internalise_templates1([], _Type, _EType, _Languages, [], N-N).
internalise_templates1([F | R], Type, EType, Languages, [F1 | R1], In-Out) :-
	F = template_unit(_Lines, _TemplateType, _NameAndParameters, _Content),
	internalise_template(F),
	internalise_template_for_rule_acquisition(F, Type, EType, Languages, F1),
	Next is In + 1,
	!,
	internalise_templates1(R, Type, EType, Languages, R1, Next-Out).
internalise_templates1([F | R], Type, EType, Languages, [F | R1], In-Out) :-
	!,
	internalise_templates1(R, Type, EType, Languages, R1, In-Out).

%------------------------------------------------

internalise_template(template_unit(Lines, TemplateType, NameAndParametersAtom, Content)) :-
	parse_template_name_and_parameters(NameAndParametersAtom, Name, ParameterVars, ParameterVarAlist),
	abstract_template_parameters(Content, ParameterVarAlist, Content1),
	store_template(Name, Lines, TemplateType, ParameterVars, Content1),
	!.
internalise_template(template_unit(FromLine-ToLine, _TemplateType, NameAndParametersAtom, _Content)) :-
	format_lite_error('~N*** Error internalising template definition (lines ~d-~d), "~w"~n',
			  [FromLine, ToLine, NameAndParametersAtom]),
	!.

%------------------------------------------------

internalise_template_for_rule_acquisition(template_unit(Lines, TemplateType, NameAndParametersAtom, Content),
					  Type, EType, Languages,
					  template_unit(Lines, [template_name=Name, template_args=ParameterVars, template_type=TemplateType | Content4])) :-
	parse_template_name_and_parameters(NameAndParametersAtom, Name, ParameterVars, ParameterVarAlist),
	(   TemplateType = unit ->
	    pre_unit_to_unit0(unit(Lines, Content), Type, EType, Languages, in_template, unit(Lines, Content1))
	;
	    TemplateType = tr_phrase ->
	    pre_unit_to_unit0(tr_phrase_unit(Lines, Content), Type, EType, Languages, in_template, tr_phrase_unit(Lines, Content1))
	),
	abstract_template_parameters(Content1, ParameterVarAlist, Content2),
	%make_ground(ParameterVars),
	ground_vars_using_parameter_var_alist(ParameterVarAlist),
	strings_to_atoms_in_template_body(Content2, Content3),
	split_atoms_in_responses(Content3, Content4),
	!.
internalise_template_for_rule_acquisition(Unit, Type, _EType, Languages, Unit1) :-
	format_lite_error('~N*** Error: bad call: ~w~n', [internalise_template_for_rule_acquisition(Unit, Type, Languages, Unit1)]),
	fail.

%------------------------------------------------

parse_template_name_and_parameters(NameAndParametersAtom, Name, ParameterVars, ParameterVarAlist) :-
	split_atom_into_words(NameAndParametersAtom, Components),
	(   Components = [] ->
	    format_lite_error('~N*** Error: no template name defined~n', []),
	    fail
	;
	    otherwise ->
	    Components = [Name | ParametersAtoms]
	),
	(   ParametersAtoms = [] ->
	    format_lite_error('~N*** Error: no parameters defined~n', []),
	    fail
	;
	    otherwise ->
	    parameter_atoms_to_vars_and_alist(ParametersAtoms, ParameterVars, ParameterVarAlistWithLengths),
	    % We need to try the longest parameters first, in case one parameter is a prefix of another one.
	    keysort(ParameterVarAlistWithLengths, ParameterVarAlistWithLengthsSorted),
	    reverse(ParameterVarAlistWithLengthsSorted, ParameterVarAlistWithLengthsSortedReversed),
	    unkey_list(ParameterVarAlistWithLengthsSortedReversed, ParameterVarAlist)
	).

parameter_atoms_to_vars_and_alist([], [], []).
parameter_atoms_to_vars_and_alist([F | R], [F1 | R1], [F2 | R2]) :-
	parameter_atom_to_var_and_alist_element(F, F1, F2),
	!,
	parameter_atoms_to_vars_and_alist(R, R1, R2).

parameter_atom_to_var_and_alist_element(Atom, Var, Length-(String-Var)) :-
	atom_codes(Atom, String),
	length(String, Length).

ground_vars_using_parameter_var_alist([]).
ground_vars_using_parameter_var_alist([F | R]) :-
	ground_var_in_parameter_var_alist(F),
	!,
	ground_vars_using_parameter_var_alist(R).

ground_var_in_parameter_var_alist(String-Var) :-
	atom_codes(Atom, String),
	Var = '$VAR'(Atom),
	!.
ground_var_in_parameter_var_alist(Other) :-
	format_lite_error('~N*** Error: bad call: ~w~n', [ground_var_in_parameter_var_alist(Other)]),
	fail.

abstract_template_parameters([], _ParameterVarAlist, []).
abstract_template_parameters([F | R], ParameterVarAlist, [F1 | R1]) :-
	abstract_template_parameters_for_line(F, ParameterVarAlist, F1),
	!,
	abstract_template_parameters(R, ParameterVarAlist, R1).

abstract_template_parameters_for_line([], _ParameterVarAlist, []) :-
	!.
abstract_template_parameters_for_line(Number, ParameterVarAlist, AtomAndVarList) :-
	number(Number),
	number_codes(Number, Chars),
	abstract_template_parameters_for_line1(Chars, CurrentStr-CurrentStr, ParameterVarAlist, AtomAndVarList).
abstract_template_parameters_for_line(Atom, ParameterVarAlist, AtomAndVarList) :-
	atom(Atom),
	atom_codes(Atom, Chars),
	abstract_template_parameters_for_line1(Chars, CurrentStr-CurrentStr, ParameterVarAlist, AtomAndVarList).
abstract_template_parameters_for_line(Key=Atom, ParameterVarAlist, Key=AtomAndVarList) :-
	atomic(Atom),
	abstract_template_parameters_for_line(Atom, ParameterVarAlist, AtomAndVarList),
	!.
abstract_template_parameters_for_line(Key=AtomList, ParameterVarAlist, Key=AtomAndVarListList) :-
	is_atom_list(AtomList),
	abstract_template_parameters_for_line_list(AtomList, ParameterVarAlist, AtomAndVarListList),
	!.
abstract_template_parameters_for_line(Key=KeyValList, ParameterVarAlist, Key=ParameterisedKeyValList) :-
	is_key_val_list(KeyValList),
	abstract_template_parameters_for_key_val_list(KeyValList, ParameterVarAlist, ParameterisedKeyValList),
	!.
abstract_template_parameters_for_line(Key=ResponseList, ParameterVarAlist, Key=ParameterisedResponseList) :-
	is_abstract_response_list(ResponseList),
	abstract_template_parameters_for_abstract_response_list(ResponseList, ParameterVarAlist, ParameterisedResponseList),
	!.
abstract_template_parameters_for_line(Other, ParameterVarAlist, AtomAndVarList) :-
	format_lite_error('*** Error: bad call: ~w~n',
			  [abstract_template_parameters_for_line(Other, ParameterVarAlist, AtomAndVarList)]),
	fail.

abstract_template_parameters_for_line_list([], _ParameterVarAlist, []).
abstract_template_parameters_for_line_list([F | R], ParameterVarAlist, [F1 | R1]) :-
	abstract_template_parameters_for_line(F, ParameterVarAlist, F1),
	!,
	abstract_template_parameters_for_line_list(R, ParameterVarAlist, R1).

abstract_template_parameters_for_key_val_list([], _ParameterVarAlist, []).
abstract_template_parameters_for_key_val_list([Key=F | R], ParameterVarAlist, [Key=F1 | R1]) :-
	abstract_template_parameters_for_line(F, ParameterVarAlist, F1),
	!,
	abstract_template_parameters_for_key_val_list(R, ParameterVarAlist, R1).

abstract_template_parameters_for_abstract_response_list([], _ParameterVarAlist, []).
abstract_template_parameters_for_abstract_response_list([F | R], ParameterVarAlist, [F1 | R1]) :-
	abstract_template_parameters_for_abstract_response(F, ParameterVarAlist, F1),
	abstract_template_parameters_for_abstract_response_list(R, ParameterVarAlist, R1).

abstract_template_parameters_for_abstract_response(Atom, ParameterVarAlist, Result) :-
	atomic(Atom),
	abstract_template_parameters_for_line(Atom, ParameterVarAlist, Result).
abstract_template_parameters_for_abstract_response([F | R], ParameterVarAlist, [F1 | R1]) :-
	abstract_template_parameters_for_abstract_response(F, ParameterVarAlist, F1),
	abstract_template_parameters_for_abstract_response(R, ParameterVarAlist, R1).
abstract_template_parameters_for_abstract_response(or(F, R), ParameterVarAlist, or(F1, R1)) :-
	abstract_template_parameters_for_abstract_response(F, ParameterVarAlist, F1),
	abstract_template_parameters_for_abstract_response(R, ParameterVarAlist, R1).
abstract_template_parameters_for_abstract_response(optional(F), ParameterVarAlist, optional(F1)) :-
	abstract_template_parameters_for_abstract_response(F, ParameterVarAlist, F1).
abstract_template_parameters_for_abstract_response(phrase(F), ParameterVarAlist, phrase(F1)) :-
	abstract_template_parameters_for_abstract_response(F, ParameterVarAlist, F1).

abstract_template_parameters_for_line1([], CurrentStr-RestStr, _ParameterVarAlist, Result) :-
	RestStr = [],
	(   CurrentStr = [] ->
	    Result = []
	;
	    otherwise ->
	    Result = [CurrentStr]
	),
	!.
abstract_template_parameters_for_line1(Chars, CurrentStr-CurrentStrOut, ParameterVarAlist, AtomAndVarList) :-
	member(ParameterStr-Var, ParameterVarAlist),
	append(ParameterStr, CharsNext, Chars),
	CurrentStrOut = [],
	(   CurrentStr = [] ->
	    AtomAndVarList = [Var | AtomAndVarListNext]
	;
	    otherwise ->
	    AtomAndVarList = [CurrentStr, Var | AtomAndVarListNext]
	),
	!,
	abstract_template_parameters_for_line1(CharsNext, CurrentStrNext-CurrentStrNext, ParameterVarAlist, AtomAndVarListNext).
abstract_template_parameters_for_line1([F | R], CurrentStr-[F | CurrentStrNext], ParameterVarAlist, AtomAndVarList) :-
	!,
	abstract_template_parameters_for_line1(R, CurrentStr-CurrentStrNext, ParameterVarAlist, AtomAndVarList).

store_template(Name, FromLine-ToLine, TemplateType, ParameterVars, _Content) :-
	flat_template(Name, ParameterVars, TemplateType, _OtherContent),
	!,
	format_lite_error('~N*** Error: template ~w (lines ~d-~d) previously defined~n', [Name, FromLine, ToLine]).
store_template(Name, _Lines, TemplateType, ParameterVars, Content) :-
	assertz(flat_template(Name, ParameterVars, TemplateType, Content)).	

%======================================================================

expand_template_applications(PreUnits, PreUnits1) :-
	expand_template_applications1(PreUnits, PreUnits1, 0-N),
	(   N = 0 ->
	    format('~N--- No template applications found~n', [])
	;
	    otherwise ->
	    format('~N--- ~d template applications expanded~n', [N])
	).

expand_template_applications1([], [], N-N).
expand_template_applications1([F | R], [F1 | R1], In-Out) :-
	expand_template_application_if_possible(F, F1, In-Next),
	!,
	expand_template_applications1(R, R1, Next-Out).

expand_template_application_if_possible(template_application(Line, TemplateApplicationAtom), Expanded, In-Out) :-
	expand_template_application(Line, TemplateApplicationAtom, Expanded),
	Out is In + 1,
	!.
expand_template_application_if_possible(Other, Other, In-In).

expand_template_application(Line, TemplateApplicationAtom, Expanded) :-
	parse_template_application_atom(TemplateApplicationAtom, Name, Args),
	expand_template_application1(Line, Name, Args, Expanded),
	!.
expand_template_application(Line, TemplateApplicationAtom, null) :-
	format_lite_error('~N*** Error expanding template application (line ~d): "~w"~n', [Line, TemplateApplicationAtom]),
	!.

expand_template_application1(_Line, Name, _Args, _Expanded) :-
	\+ flat_template(Name, _, _, _),
	!,
	format_lite_error('~N*** Error: no template called ~w~n', [Name]),
	fail.
expand_template_application1(_Line, Name, Args, _Expanded) :-
	\+ flat_template(Name, Args, _, _),
	!,
	format_lite_error('~N*** Error: template ~w has wrong number of arguments~n', [Name]),
	fail.
expand_template_application1(Line, Name, Args, Expanded) :-
	flat_template(Name, Args, TemplateType, Body0),
	join_up_args_in_expanded_template(Body0, Body1),
	add_template_application_to_body(Body1, Name, Args, Body0, Body2),
	%Body1 = Body2,
	(   TemplateType = unit ->
	    Expanded = unit(Line-Line, Body2)
	;
	    TemplateType = tr_phrase ->
	    Expanded = tr_phrase_unit(Line-Line, Body2)
	),
	!.
expand_template_application1(Line, Name, Args, Expanded) :-
	format_lite_error('~N*** Error: bad call: ~w~n', [expand_template_application1(Line, Name, Args, Expanded)]),
	fail.

join_up_args_in_expanded_template([], []).
join_up_args_in_expanded_template([F | R], [F1 | R1]) :-
	join_up_args_in_expanded_template_line(F, F1),
	!,
	join_up_args_in_expanded_template(R, R1).

join_up_args_in_expanded_template_line(Key=StringList, Key=Atom) :-
	is_list(StringList),
	append_list(StringList, String),
	atom_codes(Atom, String).
join_up_args_in_expanded_template_line(StringList, Atom) :-
	is_list(StringList),
	append_list(StringList, String),
	atom_codes(Atom, String).

%======================================================================

add_template_application_to_body(BodyIn, Name, Args, _Body, BodyOut) :-
	strings_to_atoms_in_template_args(Args, Args1),
	BodyOut = [template_application=[Name, Args1] | BodyIn],
	!.
%add_template_application_to_body(BodyIn, Name, Args, Body, BodyOut) :-
%	strings_to_atoms_in_template_args(Args, Args1),
%	strings_to_atoms_in_template_body(Body, Body1),
%	BodyOut = [template_application=[Name, Args1, Body1] | BodyIn],
%	!.
add_template_application_to_body(BodyIn, Name, Args, Body, BodyOut) :-
	format_lite_error('~N*** Error: bad call: ~w~n', [add_template_application_to_body(BodyIn, Name, Args, Body, BodyOut)]),
	fail.

strings_to_atoms_in_template_args(StringList, AtomList) :-
	string_list_to_atom_list(StringList, AtomList).

strings_to_atoms_in_template_body([], []).
%strings_to_atoms_in_template_body([Key = Val | R], [responses = CorrectResponses, incorrect_responses=IncorrectResponses | R1]) :-
%	( Key = response ; Key = variant ),
%	string_list_to_atom_list(Val, Val1),
%	findall([Expanded, ContainsStars],
%		parse_and_expand_atom_list(Val1, Expanded, no_stars-ContainsStars),
%	       Pairs),
%	findall(CorrectResponse,
%		member([CorrectResponse, no_stars], Pairs),
%		CorrectResponses),
%	findall(IncorrectResponse,
%		member([IncorrectResponse, stars], Pairs),
%		IncorrectResponses),
%	!,
%	strings_to_atoms_in_template_body(R, R1).
strings_to_atoms_in_template_body([Key = Val | R], [Key = Val1 | R1]) :-
	member(Key, [responses, incorrect_responses, unexpanded_responses, audiohelp, texthelp]),
	list_of_string_lists_to_list_of_atoms_list(Val, Val1),
	!,
	strings_to_atoms_in_template_body(R, R1).
strings_to_atoms_in_template_body([Key = Val | R], [Key = Val1 | R1]) :-
	member(Key, [answers, tags]),
	list_of_key_string_pairs_to_key_atom_pairs(Val, Val1),
	!,
	strings_to_atoms_in_template_body(R, R1).
strings_to_atoms_in_template_body([Key = Val | R], [Key = Val1 | R1]) :-
	string_list_to_atom_list(Val, Val1),
	!,
	strings_to_atoms_in_template_body(R, R1).

string_list_to_atom_list([], []).
string_list_to_atom_list([F | R], [F1 | R1]) :-
	(   is_prolog_string(F) ->
	    atom_codes(F1, F)
	;
	    otherwise ->
	    F1 = F
	),
	!,
	string_list_to_atom_list(R, R1).

% responses = [[[97,114,101,32,121,111,117,32,97,32],C], [[97,114,101,32,121,111,117,32],C]]

list_of_string_lists_to_list_of_atoms_list([], []).
list_of_string_lists_to_list_of_atoms_list([F | R], [F1 | R1]) :-
	string_list_to_atom_list(F, F1),
	!,
	list_of_string_lists_to_list_of_atoms_list(R, R1).

% answers = [yes=[D], no=[[110,111]]]

list_of_key_string_pairs_to_key_atom_pairs([], []).
list_of_key_string_pairs_to_key_atom_pairs([Key = F | R], [Key = F1 | R1]) :-
	string_list_to_atom_list(F, F1),
	!,
	list_of_key_string_pairs_to_key_atom_pairs(R, R1).

%======================================================================

split_atoms_in_responses([], []).
split_atoms_in_responses([responses=Responses | R], [responses=Responses1 | R]) :-
	split_atoms_in_responses1(Responses, Responses1),
	!.
split_atoms_in_responses([F | R], [F | R1]) :-
	split_atoms_in_responses(R, R1).

split_atoms_in_responses1([], []).
split_atoms_in_responses1([F | R], [F1 | R1]) :-
	split_atoms_in_atom_list(F, F1),
	!,
	split_atoms_in_responses1(R, R1).

split_atoms_in_atom_list([], []).
split_atoms_in_atom_list([F | R], Out) :-
	atom(F),
	F \== [],
	split_atom_into_words(F, Components),
	append(Components, R1, Out),
	!,
	split_atoms_in_atom_list(R, R1).
split_atoms_in_atom_list([F | R], [F | R1]) :-
	!,
	split_atoms_in_atom_list(R, R1).

%======================================================================

pre_units_to_units([], _Type, _EType, _Languages, []).
pre_units_to_units([F | R], Type, EType, Languages, [F1 | R1]) :-
	pre_unit_to_unit0(F, Type, EType, Languages, not_in_template, F1),
	!,
	pre_units_to_units(R, Type, EType, Languages, R1).

pre_unit_to_unit0(F, Type, EType, Languages, InTemplateP, F1) :-
	on_exception(
	Exception, 
	pre_unit_to_unit(F, Type, EType, Languages, InTemplateP, F1),
	inform_about_exception_in_pre_unit_to_unit(Exception, F, Type, EType, InTemplateP, Languages)
    ).

inform_about_exception_in_pre_unit_to_unit(Exception, F, Type, EType, InTemplateP, Languages) :-
	format('~NInternal error ~w in pre_unit_to_unit call:~n',
	       [Exception]),
	prettyprint(pre_unit_to_unit(F, Type, EType, Languages, InTemplateP, '_')),
	fail.

pre_unit_to_unit(template_unit(StartLine-EndLine, List), _Type, _EType, _Languages, _InTemplateP, template_unit(StartLine-EndLine, List)) :-
	!.
pre_unit_to_unit(unit(StartLine-EndLine, ListIn), Type, EType, Languages, InTemplateP, unit(StartLine-EndLine, ListOut)) :-
	findall(ExpandedUnit,
		(   member(Unit, ListIn),
		    parse_and_expand_unit(Unit, Type, EType, Languages, InTemplateP, ExpandedUnit)
		),
		ListNext),
	group_unit_components(ListNext, Type, Languages, ListOut),
	!.
pre_unit_to_unit(unit(StartLine-EndLine, _ListIn), _Type, _EType, _Languages, _InTemplateP, _Out) :-
	!,
	format_lite_error('~N*** Error in lines ~d-~d~n', [StartLine, EndLine]),
	fail.
pre_unit_to_unit(group_unit(StartLine-EndLine, ListIn), Type, EType, Languages, InTemplateP, group_unit(StartLine-EndLine, ListOut)) :-
	findall(ExpandedUnit,
		(   member(Unit, ListIn),
		    parse_and_expand_unit(Unit, Type, EType, Languages, InTemplateP, ExpandedUnit)
		),
		ListNext),
	group_group_unit_components(ListNext, Type, Languages, ListOut),
	!.
pre_unit_to_unit(group_unit(StartLine-EndLine, _ListIn), _Type, _EType, _Languages, _InTemplateP, _Out) :-
	!,
	format_lite_error('~N*** Error in lines ~d-~d~n', [StartLine, EndLine]),
	fail.
pre_unit_to_unit(answer_unit(StartLine-EndLine, ListIn), Type, EType, Languages, InTemplateP, answer_unit(StartLine-EndLine, ListOut)) :-
	findall(ExpandedUnit,
		(   member(Unit, ListIn),
		    parse_and_expand_unit(Unit, Type, EType, Languages, InTemplateP, ExpandedUnit)
		),
		ListNext),
	group_answer_unit_components(ListNext, Type, Languages, ListOut),
	!.
pre_unit_to_unit(answer_unit(StartLine-EndLine, _ListIn), _Type, _EType, _Languages, _InTemplateP, _Out) :-
	!,
	format_lite_error('~N*** Error in lines ~d-~d~n', [StartLine, EndLine]),
	fail.
pre_unit_to_unit(multimedia_unit(StartLine-EndLine, ListIn), Type, EType, Languages, InTemplateP, multimedia_unit(StartLine-EndLine, ListOut)) :-
	findall(ExpandedUnit,
		(   member(Unit, ListIn),
		    parse_and_expand_unit(Unit, Type, EType, Languages, InTemplateP, ExpandedUnit)
		),
		ListNext),
	group_multimedia_unit_components(ListNext, Type, Languages, ListOut),
	!.
pre_unit_to_unit(multimedia_unit(StartLine-EndLine, _ListIn), _Type, _EType, _Languages, _InTemplateP, _Out) :-
	!,
	format_lite_error('~N*** Error in lines ~d-~d~n', [StartLine, EndLine]),
	fail.
pre_unit_to_unit(coverage_unit(StartLine-EndLine, ListIn), Type, EType, Languages, InTemplateP, coverage_unit(StartLine-EndLine, ListOut)) :-
	findall(ExpandedUnit,
		(   member(Unit, ListIn),
		    parse_and_expand_unit(Unit, Type, EType, Languages, InTemplateP, ExpandedUnit)
		),
		ListNext),
	group_coverage_unit_components(ListNext, Type, Languages, ListOut),
	!.
pre_unit_to_unit(coverage_unit(StartLine-EndLine, _ListIn), _Type, _EType, _Languages, _InTemplateP, _Out) :-
	!,
	format_lite_error('~N*** Error in lines ~d-~d~n', [StartLine, EndLine]),
	fail.
pre_unit_to_unit(phrase_unit(StartLine-EndLine, ListIn), Type, _EType, Languages, InTemplateP, phrase_unit(StartLine-EndLine, ListOut)) :-
	findall(ExpandedUnit,
		(   member(Unit, ListIn),
		    parse_unit(Unit, Type, Languages, InTemplateP, ExpandedUnit)
		),
		ListNext),
	group_phrase_unit_components(ListNext, Type, Languages, ListOut),
	!.
pre_unit_to_unit(phrase_unit(StartLine-EndLine, _ListIn), _Type, _EType, _Languages, _InTemplateP, _Out) :-
	!,
	format_lite_error('~N*** Error in lines ~d-~d~n', [StartLine, EndLine]),
	fail.
pre_unit_to_unit(tr_phrase_unit(StartLine-EndLine, ListIn), Type, _EType, Languages, InTemplateP, tr_phrase_unit(StartLine-EndLine, ListOut)) :-
	findall(ExpandedUnit,
		(   member(Unit, ListIn),
		    parse_unit(Unit, Type, Languages, InTemplateP, ExpandedUnit)
		),
		ListNext),
	group_tr_phrase_unit_components(ListNext, Type, Languages, ListOut),
	!.
pre_unit_to_unit(tr_phrase_unit(StartLine-EndLine, _ListIn), _Type, _EType, _Languages, _InTemplateP, _Out) :-
	!,
	format_lite_error('~N*** Error in lines ~d-~d~n', [StartLine, EndLine]),
	fail.
pre_unit_to_unit(phrase_score_unit(StartLine-EndLine, ListIn), Type, EType, Languages, InTemplateP, phrase_score_unit(StartLine-EndLine, ListOut)) :-
	findall(ExpandedUnit,
		(   member(Unit, ListIn),
		    parse_and_expand_unit(Unit, Type, EType, Languages, InTemplateP, ExpandedUnit)
		),
		ListNext),
	group_phrase_score_unit_components(ListNext, Type, Languages, ListOut),
	!.
pre_unit_to_unit(phrase_score_unit(StartLine-EndLine, _ListIn), _Type, _EType, _Languages, _InTemplateP, _Out) :-
	!,
	format_lite_error('~N*** Error in lines ~d-~d~n', [StartLine, EndLine]),
	fail.
pre_unit_to_unit(course_unit(StartLine-EndLine, ListIn), Type, EType, Languages, InTemplateP, course_unit(StartLine-EndLine, ListOut)) :-
	findall(ExpandedUnit,
		(   member(Unit, ListIn),
		    parse_and_expand_unit(Unit, Type, EType, Languages, InTemplateP, ExpandedUnit)
		),
		ListNext),
	group_course_unit_components(ListNext, Type, Languages, ListOut),
	!.
pre_unit_to_unit(course_unit(StartLine-EndLine, _ListIn), _Type, _EType, _Languages, _InTemplateP, _Out) :-
	!,
	format_lite_error('~N*** Error in lines ~d-~d~n', [StartLine, EndLine]),
	fail.
pre_unit_to_unit(In, Type, EType, Languages, InTemplateP, Out) :-
	format_lite_error('~N*** Error: bad call: ~w~n', [pre_unit_to_unit(In, Type, EType, Languages, InTemplateP, Out)]),
	fail.

%======================================================================

parse_and_expand_unit(ResponseOrVariant=ResponseAtom, Type, EType, _Languages, InTemplateP, Result) :-
	(   ( call_or_new_text(Type), ResponseOrVariant = response )
	;
	    ( Type = questionnaire, ResponseOrVariant = variant )
	;
	    ( translation_or_new_text(Type), ResponseOrVariant = source )
	),
	!,
	parse_response_atom(ResponseAtom, InTemplateP, AbstractResponse),
	expand_and_remove_stars(AbstractResponse, EType, InTemplateP, ExpandedResponseAtom, no_stars-StarsFound),
	(   (   StarsFound = stars,
		Result = ( incorrect_response=ExpandedResponseAtom )
	    )
	;
	    (   StarsFound = no_stars,
		Result = ( response=ExpandedResponseAtom )
	    )
	;
	    (   Result = ( unexpanded_response=ResponseAtom )
	    )
	).
parse_and_expand_unit(Other, Type, _EType, Languages, InTemplateP, Result) :-
	parse_unit(Other, Type, Languages, InTemplateP, Result).

parse_unit(template_application=Application, _Type, _Languages, _InTemplateP, template_application=Application) :-
	!.
parse_unit(tags=TagsAtom, _Type, _Languages, InTemplateP, tags=Tags) :-
	!,
	parse_tags_atom(TagsAtom, InTemplateP, Tags).
parse_unit(answers=AnswersAtom, _Type, _Languages, InTemplateP, answers=AnswersAlist) :-
	!,
	parse_answers_atom(AnswersAtom, InTemplateP, AnswersAlist).
parse_unit(ResponseOrVariant=ResponseAtom, Type, _Languages, InTemplateP, ResponseOrVariant=AbstractResponse) :-
	(   ( call_or_new_text(Type), ResponseOrVariant = response )
	;
	    ( Type = questionnaire, ResponseOrVariant = variant )
	;
	    ( Type = translation, ResponseOrVariant = source )
	),
	!,
	parse_response_atom(ResponseAtom, InTemplateP, AbstractResponse).
parse_unit(text(Lang)=Atom, Type, Languages, InTemplateP, text(Lang)=AbstractTranslation) :-
	translation_or_new_text(Type),
	member(Lang, Languages),
	!,
	parse_translation_atom(Atom, InTemplateP, AbstractTranslation).
parse_unit(text(Lang)=Atom, _Type, Languages, _InTemplateP, text(Lang)=Atom) :-
	member(Lang, Languages),
	!.
parse_unit(Tag=Atom, _Type, _Languages, _InTemplateP, Tag=Atom1) :-
	member(Tag, [phraseid]),
	(   phrase_id_atom(Atom, Atom1) ->
	    true
	;
	    otherwise ->
	    format_lite_error('~N*** Error: bad PhraseId "~w". PhraseIds must be in lowercase and start with a dollar sign~n', [Atom]),
	    fail
	),
	!.
parse_unit(Tag=Atom, _Type, _Languages, _InTemplateP, Tag=Atom1) :-
	member(Tag, [trphraseid]),
	(   tr_phrase_id_atom(Atom, Atom1) ->
	    true
	;
	    otherwise ->
	    format_lite_error('~N*** Error: bad TrPhraseId "~w". TrPhraseIds must be in lowercase and start with two dollar signs~n', [Atom]),
	    fail
	),
	!.
parse_unit(next=Atom, _Type, _Languages, _InTemplateP, next=[Target, Triggers]) :-
	!,
	split_atom_into_words(Atom, Words),
	(   Words = [Target] ->
	    Triggers = any
	;
	    ( Words = [Target, If | Triggers], lowercase_atom(If, if), Triggers \== [] ) ->
	    true
	;
	    otherwise ->
	    format_lite_error('~N*** Error: bad line "Next ~w"~n', [Atom]),
	    fail
	),
	!.
parse_unit(Tag=Atom, _Type, _Languages, _InTemplateP, Tag=Atom1) :-
	member(Tag, [phrase, printname, description, popup, texthelp]),
	split_atom_into_words(Atom, Words),
	join_with_spaces(Words, Atom1),
	!.
parse_unit(Tag=Atom, _Type, _Languages, _InTemplateP, Tag=[File, Transcription]) :-
	member(Tag, [audiohelp]),
	split_atom_into_words(Atom, Words),
	Words = [File | TranscriptionWords],
	join_with_spaces(TranscriptionWords, Transcription),
	!.
parse_unit(Tag=Atom, _Type, _Languages, _InTemplateP, Tag1=Atom1) :-
	member(Tag, [course, questionnaire, lesson, name, content, code, l2,
		     script, id, file, multimedia, helpfile, helpfile(_Language), client, feedback]),
	!,
	split_atom_into_words(Atom, Words),
	(   Words = [Atom1] ->
	    maybe_substitute_tag_for_alias(Tag, Tag1)
	;
	    otherwise ->
	    format_lite_error('~N*** Error: multiple values "~w" in ~w line~n', [Atom, Tag]),
	    fail
	),
	(   ( possible_values_for_tag(Tag, Values), \+ member(Atom1, Values) ) ->
	    format_lite_error('~N*** Error: unknown value "~w" for tag "~w". Permitted values: ~w', [Atom1, Tag, Values]),
	    fail
	;
	    otherwise ->
	    true
	),		  
	!.
% Treat groups as numbers if possible, for ordering purposes.
parse_unit(Tag=Atom, _Type, _Languages, _InTemplateP, Tag=IntOrAtom) :-
	member(Tag, [group]),
	!,
	split_atom_into_words(Atom, [Word]),
	(   atom_to_int(Word, Int) ->
	    IntOrAtom = Int
	;
	    otherwise ->
	    IntOrAtom = Atom
	),
	!.
parse_unit(Tag=Atom, _Type, _Languages, _InTemplateP, Tag=Int) :-
	member(Tag, [score, acceptbonus, maxtries, startscore, rejectpenalty, skippenalty, plainbadge, bronzebadge, silverbadge, goldbadge]),
	!,
	split_atom_into_words(Atom, [Word]),
	(   atom_to_int(Word, Int) ->
	    true
	;
	    otherwise ->
	    format_lite_error('~N*** Error: non-number "~w" in ~w line~n', [Atom, Tag]),
	    fail
	),
	!.
parse_unit(Tag=Atom, _Type, _Languages, _InTemplateP, Tag=List) :-
	member(Tag, [fillers, languages, users]),
	!,
	split_atom_into_words(Atom, List),
	!.
parse_unit(Tag=Atom, _Type, _Languages, _InTemplateP, Tag=List) :-
	member(Tag, [badges]),
	!,
	split_atom_into_words(Atom, List),
	length(List, N),
	(   N = 14 ->
	    true
	;
	    otherwise ->
	    format_lite_error('~N*** Error: list of badges "~w" does not contain 14 (2 + 4 + 4 + 4) elements~n', [Atom]),
	    fail
	),
	!.
parse_unit(Tag=_Atom, _Type, _Languages, _InTemplateP, _) :-
	format_lite_error('~N*** Error: unknown tag: ~w~n', [Tag]),
	fail.

maybe_substitute_tag_for_alias(Tag, Tag1) :-
	alias_for_tag(Tag, Tag1),
	!.
maybe_substitute_tag_for_alias(Tag, Tag1) :-
	Tag = Tag1.

alias_for_tag(questionnaire, domain).
alias_for_tag(course, domain).

possible_values_for_tag(client, [translation_game_client, multimedia_client, dialogue_client]).
possible_values_for_tag(feedback, [default, none, colour_highlighting_on_response, color_highlighting_on_prompt]).

%----------------------------------------------------

expand_and_remove_stars(AbstractResponse, flat, _InTemplateP, ExpandedResponseAtom, StarsIn-StarsFound) :-
	check_for_bad_phrase(AbstractResponse),
	expand_abstract_response(AbstractResponse, ResponseWords0),
	remove_stars_if_any(ResponseWords0, ResponseWords, StarsIn-StarsFound),
	ResponseWords \== [],
	join_with_spaces(ResponseWords, ExpandedResponseAtom).
expand_and_remove_stars(AbstractResponse, dont_expand, _InTemplateP, ResponseWords, StarsIn-StarsFound) :-
	check_for_bad_phrase(AbstractResponse),
	expand_abstract_response_for_stars(AbstractResponse, ResponseWords0),
	remove_stars_if_any(ResponseWords0, ResponseWords, StarsIn-StarsFound),
	ResponseWords \== [].

check_for_bad_phrase(Atom) :-
	atomic(Atom),
	!.
check_for_bad_phrase([]).
check_for_bad_phrase([F | R]) :-
	check_for_bad_phrase(F),
	check_for_bad_phrase(R).
check_for_bad_phrase(or(F, R)) :-
	check_for_bad_phrase(F),
	check_for_bad_phrase(R).
check_for_bad_phrase(optional(F)) :-
	check_for_bad_phrase(F).
check_for_bad_phrase(phrase(PhraseId)) :-
	(   flat_phrase(PhraseId, _) ->
	    true
	;
	    otherwise ->
	    format_lite_error('~N*** Error: phrase "~w" not defined~n', [PhraseId]),
	    fail
	).
% Need to do this checking later, since TrPhrases may not yet have been expanded.
check_for_bad_phrase(tr_phrase(PhraseId, _I)) :-
	(   flat_tr_phrase(PhraseId, _) ->
	    true
	;
	    otherwise ->
	    %format_lite_error('~N*** Error: TrPhrase "~w" not defined~n', [PhraseId]),
	    true
	).

remove_stars_if_any([], [], StarsFound-StarsFound).
remove_stars_if_any(['*' | R], R1, _In-Out) :-
	!,
	remove_stars_if_any(R, R1, stars-Out).
remove_stars_if_any([F | R], [F | R1], In-Out) :-
	!,
	remove_stars_if_any(R, R1, In-Out).

%----------------------------------------------------

expand_abstract_response([], []) :-
	!.
expand_abstract_response([F | R], Result) :-
	expand_abstract_response(F, F1),
	expand_abstract_response(R, R1),
	append(F1, R1, Result).
expand_abstract_response(phrase(PhraseId), Result) :-
	flat_phrase(PhraseId, RHS),
	expand_abstract_response(RHS, Result).
expand_abstract_response(or(X, Y), Result) :-
	!,
	(   expand_abstract_response(X, Result)
	;
	    expand_abstract_response(Y, Result)
	).
expand_abstract_response(optional(X), Result) :-
	!,
	expand_abstract_response(or(X, []), Result).
expand_abstract_response('*incorrect*', ['*']) :-
	!.
expand_abstract_response(Atom, [Atom]) :-
	atomic(Atom),
	!.

%----------------------------------------------------

expand_abstract_response_for_stars([], []) :-
	!.
expand_abstract_response_for_stars([F | R], Result) :-
	expand_abstract_response_for_stars(F, F1),
	expand_abstract_response_for_stars(R, R1),
	combine_list_components(F1, R1, Result).
expand_abstract_response_for_stars(phrase(PhraseId), phrase(PhraseId)).
expand_abstract_response_for_stars(tr_phrase(PhraseId, I), tr_phrase(PhraseId, I)).
expand_abstract_response_for_stars(or(X, Y), Result) :-
	term_contains_functor(or(X, Y), '*incorrect*'/0),
	!,
	(   expand_abstract_response_for_stars(X, Result)
	;
	    expand_abstract_response_for_stars(Y, Result)
	).
expand_abstract_response_for_stars(or(X, Y), or(X, Y)).
expand_abstract_response_for_stars(optional(X), Result) :-
	term_contains_functor(optional(X), '*incorrect*'/0),
	!,
	expand_abstract_response_for_stars(or(X, []), Result).
expand_abstract_response_for_stars(optional(X), optional(X)) :-
	!.
expand_abstract_response_for_stars('*incorrect*', '*') :-
	!.
expand_abstract_response_for_stars(Atom, Atom) :-
	atomic(Atom),
	!.

combine_list_components([], X, X) :-
	!.
combine_list_components(X, [], X) :-
	!.
combine_list_components(NonList, List, [NonList | List] ) :-
	is_list(List),
	\+ is_list(NonList),
	!.
combine_list_components(List, NonList, Result) :-
	is_list(List),
	\+ is_list(NonList),
	append(List, [NonList], Result),
	!.
combine_list_components(NonList1, NonList2, [NonList1, NonList2]) :-
	\+ is_list(NonList1),	
	\+ is_list(NonList2),
	!.
combine_list_components(List1, List2, Result) :-
	append(List1, List2, Result),
	!.
combine_list_components(X, Y, Z) :-
	format_lite_error('~N*** Error: bad call: ~w~n', [combine_list_components(X, Y, Z)]),
	fail.

%----------------------------------------------------

group_unit_components(ListIn, new_text(_CallOrTranslation), Languages, ListOut) :-
	get_text_components_for_languages_from_list(Languages, ListIn, TextComponents),
	append_list(TextComponents, Text),
	findall(Response, member(response=Response, ListIn), Responses),
	append_list([Text, [responses=Responses]], ListOut).

group_unit_components(ListIn, call, Languages, ListOut) :-
	get_unique_or_zero_component_from_list(template_application, ListIn, TemplateApplication),
	get_unique_or_zero_component_from_list(domain, ListIn, Domain),
	get_unique_component_from_list(lesson, ListIn, Lesson),
	get_unique_component_from_list_or_default(group, ListIn, 1, Group),
	get_text_components_for_languages_from_list(Languages, ListIn, TextComponents),
	append_list(TextComponents, Text),
	get_unique_or_zero_component_from_list(tags, ListIn, Tags),
	get_unique_or_zero_component_from_list(multimedia, ListIn, Multimedia),
	findall(UnexpandedResponse, member(unexpanded_response=UnexpandedResponse, ListIn), UnexpandedResponses0),
	findall(Response, member(response=Response, ListIn), Responses0),
	findall(IncorrectResponse, member(incorrect_response=IncorrectResponse, ListIn), IncorrectResponses0),
	sort(UnexpandedResponses0, UnexpandedResponses),
	sort(Responses0, Responses),
	sort(IncorrectResponses0, IncorrectResponses),
	get_zero_or_more_components_from_list(ListIn, audiohelp, AudioHelps),
	get_zero_or_more_components_from_list(ListIn, texthelp, TextHelps),	
	append_list([TemplateApplication, Domain, Lesson, Group, Text, Tags, Multimedia,
		     [unexpanded_responses=UnexpandedResponses, responses=Responses, incorrect_responses=IncorrectResponses],
		     AudioHelps,
		     TextHelps],
		    ListOut).

group_unit_components(ListIn, questionnaire, Languages, ListOut) :-
	get_unique_or_zero_component_from_list(template_application, ListIn, TemplateApplication),
	get_unique_component_from_list(group, ListIn, Group),
	get_text_components_for_languages_from_list(Languages, ListIn, TextComponents),
	append_list(TextComponents, Text),
	findall(Response, member(response=Response, ListIn), Responses),
	get_unique_component_from_list(answers, ListIn, Answers),
	append_list([TemplateApplication, Group, Text, [responses=Responses], Answers], ListOut).

group_unit_components(ListIn, translation, Languages, ListOut) :-
	get_unique_or_zero_component_from_list(template_application, ListIn, TemplateApplication),
	get_text_components_for_languages_from_list(Languages, ListIn, TextComponents),
	append_list(TextComponents, Text),
	findall(Response, member(response=Response, ListIn), Responses),
	append_list([TemplateApplication, Text, [responses=Responses]], ListOut).

group_group_unit_components(ListIn, call, Languages, ListOut) :-
	get_unique_or_zero_component_from_list(domain, ListIn, Domain),
	get_unique_component_from_list(name, ListIn, Name),
	get_unique_component_from_list(printname, ListIn, PrintName),
	get_unique_component_from_list_or_default(description, ListIn, '(no description)', Description),
	get_unique_or_zero_component_from_list(helpfile, ListIn, HelpFile),
	get_helpfile_components_for_languages_from_list(Languages, ListIn, HelpFileComponents),
	append_list(HelpFileComponents, HelpFile2),
	get_unique_or_zero_component_from_list(badges, ListIn, Badges),
	get_unique_or_zero_component_from_list(script, ListIn, Script),
	append_list([Domain, Name, PrintName, Description, HelpFile, HelpFile2, Badges, Script], ListOut).

group_group_unit_components(ListIn, questionnaire, Languages, ListOut) :-
	get_unique_component_from_list(domain, ListIn, Domain),
	get_unique_component_from_list(name, ListIn, Name),
	get_unique_component_from_list(code, ListIn, Code),
	get_unique_component_from_list(printname, ListIn, PrintName),
	get_unique_or_zero_component_from_list(helpfile, ListIn, HelpFile),
	get_helpfile_components_for_languages_from_list(Languages, ListIn, HelpFileComponents),
	append_list(HelpFileComponents, HelpFile2),
	get_unique_or_zero_component_from_list(popup, ListIn, Popup),
	get_unique_component_from_list(fillers, ListIn, Fillers),
	get_one_or_more_components_from_list(ListIn, next, Nexts),
	append_list([Domain, Name, Code, PrintName, HelpFile, HelpFile2, Popup, Fillers, Nexts], ListOut).

group_answer_unit_components(ListIn, questionnaire, Languages, ListOut) :-
	get_unique_component_from_list(content, ListIn, Content),
	get_unique_component_from_list(printname, ListIn, PrintName),
	get_unique_or_zero_component_from_list(code, ListIn, Code),
	get_text_components_for_languages_from_list(Languages, ListIn, TextComponents),
	append_list(TextComponents, Text),
	append_list([Content, PrintName, Code, Text], ListOut).

group_multimedia_unit_components(ListIn, call, _Languages, ListOut) :-
	get_unique_component_from_list(id, ListIn, Id),
	get_unique_or_zero_component_from_list(domain, ListIn, Domain),
	findall(File, member(file=File, ListIn), Files),
	append_list([Id, Domain, [files=Files]], ListOut).

group_coverage_unit_components(ListIn, call, Languages, ListOut) :-
	get_unique_or_zero_component_from_list(lesson, ListIn, Lesson),
	findall(Response, member(response=Response, ListIn), Responses0),
	findall(text(Language)=dummy_text, member(Language, Languages), Text),
	sort(Responses0, Responses),
	append_list([Lesson, Text, [responses=[], incorrect_responses=Responses]], ListOut).

group_phrase_unit_components(ListIn, call, _Languages, ListOut) :-
	get_unique_component_from_list(phraseid, ListIn, Name),
	findall(Response, member(response=Response, ListIn), Responses),
	Responses \== [],
	append_list([Name, [responses=Responses]], ListOut).

group_tr_phrase_unit_components(ListIn, translation, Languages, ListOut) :-
	get_unique_or_zero_component_from_list(template_application, ListIn, TemplateApplication),
	get_unique_component_from_list(trphraseid, ListIn, Name),
	%get_unique_component_from_list(source, ListIn, Source),
	findall(Response, member(source=Response, ListIn), Responses),
	get_text_components_for_languages_from_list(Languages, ListIn, TextComponents),
	append_list(TextComponents, Text),
	append_list([TemplateApplication, Name, Text, [responses=Responses]], ListOut).

group_phrase_score_unit_components(ListIn, call, _Languages, ListOut) :-
	get_unique_or_zero_component_from_list(domain, ListIn, Domain),
	get_unique_component_from_list(phrase, ListIn, Phrase),
	get_unique_component_from_list(score, ListIn, Score),
	append_list([Domain, Phrase, Score], ListOut).

group_course_unit_components(ListIn, call, _Languages, ListOut) :-
	get_unique_component_from_list(name, ListIn, Name),
	get_unique_component_from_list_or_default(client, ListIn, translation_game_client, Client),
	get_unique_component_from_list_or_default(feedback, ListIn, default, Feedback),
	get_unique_component_from_list(l2, ListIn, L2),
	get_unique_component_from_list(languages, ListIn, Languages),
	get_unique_component_from_list_or_default(users, ListIn, [any], Users),	
	get_unique_component_from_list_or_default(acceptbonus, ListIn, 100, AcceptBonus),
	get_unique_component_from_list_or_default(startscore, ListIn, 100, StartScore),
	get_unique_component_from_list_or_default(maxtries, ListIn, 2, MaxTries),
	get_unique_component_from_list_or_default(rejectpenalty, ListIn, 2, RejectPenalty),
	get_unique_component_from_list_or_default(skippenalty, ListIn, 5, SkipPenalty),
	get_unique_component_from_list_or_default(plainbadge, ListIn, 0, PlainBadge),
	get_unique_component_from_list_or_default(bronzebadge, ListIn, 0, BronzeBadge),
	get_unique_component_from_list_or_default(silverbadge, ListIn, 90, SilverBadge),
	get_unique_component_from_list_or_default(goldbadge, ListIn, 100, GoldBadge),
	append_list([Name, Client, Feedback, L2, Languages, Users, AcceptBonus,
		     StartScore, MaxTries, RejectPenalty, SkipPenalty,
		     PlainBadge, BronzeBadge, SilverBadge, GoldBadge],
		    ListOut).

group_course_unit_components(ListIn, translation, _Languages, ListOut) :-
	get_unique_component_from_list(name, ListIn, Name),
	get_unique_component_from_list(l2, ListIn, L2),
	get_unique_component_from_list(languages, ListIn, Languages),
	append_list([Name, L2, Languages],
		    ListOut).

get_text_components_for_languages_from_list([], _ListIn, []).
get_text_components_for_languages_from_list([F | R], ListIn, Out) :-
	get_unique_or_zero_component_from_list(text(F), ListIn, F1),
	(   prompt_to_ignore(F1) ->
	    Out = R1
	;
	    otherwise ->
	    Out = [F1 | R1]
	),
	!,
	get_text_components_for_languages_from_list(R, ListIn, R1).

get_helpfile_components_for_languages_from_list([], _ListIn, []).
get_helpfile_components_for_languages_from_list([F | R], ListIn, [F1 | R1]) :-
	get_unique_or_zero_component_from_list(helpfile(F), ListIn, F1),
	!,
	get_helpfile_components_for_languages_from_list(R, ListIn, R1).

get_one_or_more_components_from_list(ListIn, Tag, Components) :-
	get_zero_or_more_components_from_list(ListIn, Tag, Components),
	(   Components = [] ->
	    format_lite_error('~N*** Error: no "~w" components', [Tag]),
	    fail
	;
	    otherwise ->
	    true
	).

get_zero_or_more_components_from_list([], _Tag, []) :-
	!.
get_zero_or_more_components_from_list([Tag=Value | R], Tag, [Tag=Value | R1]) :-
	!,
	get_zero_or_more_components_from_list(R, Tag, R1).
get_zero_or_more_components_from_list([_Other | R], Tag, R1) :-
	!,
	get_zero_or_more_components_from_list(R, Tag, R1).

get_unique_component_from_list(Key, List, [Key=Value]) :-
	(   member(Key=Value, List) ->
	    true
	;
	    otherwise ->
	    format_lite_error('~N*** Error: field "~w" not filled', [Key]),
	    fail
	),
	(   ( member(Key=Value1, List), Value \== Value1 ) ->
	    format_lite_error('~N*** Error: field "~w" multiply filled', [Key]),
	    fail
	;
	    otherwise ->
	    true
	),
	!.

get_unique_component_from_list_or_default(Key, List, Default, [Key=Value]) :-
	(   member(Key=Value, List) ->
	    true
	;
	    otherwise ->
	    Value = Default
	),
	(   ( member(Key=Value, List), member(Key=Value1, List), Value \== Value1 ) ->
	    format_lite_error('~N*** Error: field "~w" multiply filled', [Key]),
	    fail
	;
	    otherwise ->
	    true
	),
	!.

get_unique_or_zero_component_from_list(Key, List, _Result) :-
	member(Key=Value, List),
	member(Key=Value1, List),
	Value \== Value1,
	!,
	format_lite_error('~N*** Error: field "~w" multiply filled', [Key]),
	fail.
get_unique_or_zero_component_from_list(Key, List, Result) :-
	member(Key=Value, List),
	Result = [Key=Value],
	!.
get_unique_or_zero_component_from_list(_Key, _List, Result) :-
	Result = [],
	!.

%----------------------------------------------------

% In flat, we want to remove all incorrect responses that are identical to a correct response
% - they will only create false ambiguities
remove_duplicated_incorrect_responses(UnitsIn, flat, UnitsOut) :-
	store_correct_responses(UnitsIn),
	remove_duplicated_incorrect_responses1(UnitsIn, UnitsOut, 0-N),
	(   N > 0 ->
	    format('~N--- Removed ~d incorrect responses duplicating correct ones~n', [N])
	;
	    otherwise ->
	    true
	).
% If we aren't expanding, there is nothing we can do.
remove_duplicated_incorrect_responses(UnitsIn, dont_expand, UnitsIn) :-
	!.

:- dynamic stored_correct_response/1.

store_correct_responses(Units) :-
	retractall(stored_correct_response(_)),
	store_correct_responses1(Units).

store_correct_responses1([]).
store_correct_responses1([F | R]) :-
	store_correct_responses2(F),
	!,
	store_correct_responses1(R).

store_correct_responses2(unit(_Lines, List)) :-
	member(responses=Responses, List),
	store_correct_responses3(Responses),
	!.
store_correct_responses2(_Other).

store_correct_responses3([]).
store_correct_responses3([F | R]) :-
	assertz(stored_correct_response(F)),
	!,
	store_correct_responses3(R).

remove_duplicated_incorrect_responses1([], [], In-In).
remove_duplicated_incorrect_responses1([F | R], [F1 | R1], In-Out) :-
	remove_duplicated_incorrect_responses2(F, F1, In-Next),
	!,
	remove_duplicated_incorrect_responses1(R, R1, Next-Out).

remove_duplicated_incorrect_responses2(unit(Lines, List), unit(Lines, List1), In-Out) :-
	remove_duplicated_incorrect_responses3(List, List1, In-Out),
	!.
remove_duplicated_incorrect_responses2(coverage_unit(Lines, List), coverage_unit(Lines, List1), In-Out) :-
	remove_duplicated_incorrect_responses3(List, List1, In-Out),
	!.
remove_duplicated_incorrect_responses2(Other, Other, In-In).

remove_duplicated_incorrect_responses3([], [], In-In).
remove_duplicated_incorrect_responses3([F | R], [F1 | R1], In-Out) :-
	remove_duplicated_incorrect_responses4(F, F1, In-Next),
	!,
	remove_duplicated_incorrect_responses3(R, R1, Next-Out).

remove_duplicated_incorrect_responses4(incorrect_responses=List, incorrect_responses=List1, In-Out) :-
	remove_duplicated_incorrect_responses5(List, List1, In-Out),
	!.
remove_duplicated_incorrect_responses4(Other, Other, In-In).

remove_duplicated_incorrect_responses5([], [], In-In).
remove_duplicated_incorrect_responses5([F | R], R1, In-Out) :-
	stored_correct_response(F),
	Next is In + 1,
	!,
	remove_duplicated_incorrect_responses5(R, R1, Next-Out).
remove_duplicated_incorrect_responses5([F | R], [F | R1], In-Out) :-
	!,
	remove_duplicated_incorrect_responses5(R, R1, In-Out).

%----------------------------------------------------

write_flat_file_summary_for_adding_new_l1(InternalisedFlatFile, SummaryFile, NewL1) :-
	safe_absolute_file_name(InternalisedFlatFile, InFile),
	safe_absolute_file_name(SummaryFile, OutFile),

	prolog_file_to_list(InFile, InList),
	length(InList, InN),
	format('~N--- Read internalised flat file (~d records) ~w~n', [InN, InFile]),

	make_flat_file_summary_list(InList, NewL1, OutList),

	write_out_flat_file_summary_for_adding_new_l1(OutList, OutFile),
	length(OutList, OutN),
	format('~N--- Written summary file (~d records) ~w~n', [OutN, OutFile]).

/*
unit((41-41), 
     [(domain=english_course), (lesson=airport), (group=leaving_date), 
      (text(french)='Dites : Je veux partir le lundi'), 
      (text(german)='Sag : Ich möchte am Montag abreisen'), 
      (responses = 
       ['i need to leave on monday please', 'i need to leave on monday', 
        'i want to leave on monday please', 'i want to leave on monday', 
        'i would like to leave on monday please', 'i would like to leave on monday']),
      (incorrect_responses=[])]).
*/

make_flat_file_summary_list(InList, NewL1, OutList) :-
	all_l1_languages_in_list(InList, L1s),
	findall(Record,
		(   member(Unit, InList),
		    Unit = unit(_Lines, Body),
		    flat_file_summary_for_unit(L1s, Body, Record)
		),
		Records0),
	sort(Records0, Records),
	add_sample_responses_and_new_l1_to_summary_records(Records, InList, NewL1, OutList).

all_l1_languages_in_list(List, L1s) :-
	findall(L1,
		(   member(unit(_Lines, Body), List),
		    member(text(L1)=_, Body)
		),
		L1s0),
	sort(L1s0, L1s).

flat_file_summary_for_unit([], _Unit, []).
flat_file_summary_for_unit([L1 | L1s], Unit, [text(L1)=Text | R]) :-
	member(text(L1)=Text, Unit),
	!,
	flat_file_summary_for_unit(L1s, Unit, R).

add_sample_responses_and_new_l1_to_summary_records([], _List, _NewL1, []).
add_sample_responses_and_new_l1_to_summary_records([F | R], List, NewL1, [F1 | R1]) :-
	add_sample_response_and_new_l1_to_summary_record(F, List, NewL1, F1),
	!,
	add_sample_responses_and_new_l1_to_summary_records(R, List, NewL1, R1).

add_sample_response_and_new_l1_to_summary_record(Record, List, NewL1, Record1) :-
	find_sample_response_for_record(Record, List, SampleResponse),
	append(Record, [text(NewL1)='?', response=SampleResponse], Record1),
	!.
 
find_sample_response_for_record(Record, List, SampleResponse) :-
	member(text(L1)=Text, Record),
	member(unit(_Lines, Body), List),
	member(text(L1)=Text, Body),
	member(responses=Responses, Body),
	shortest_atom(Responses, SampleResponse),
	!.
find_sample_response_for_record(Record, _List, SampleResponse) :-
	format_lite_error('~N*** Error: bad call: ~w~n',
	       [find_sample_response_for_record(Record, '(...)', SampleResponse)]),
	fail.

write_out_flat_file_summary_for_adding_new_l1(List, File) :-
	open(File, write, S),
	write_out_flat_file_summary_for_adding_new_l1_list(List, S),
	close(S).

write_out_flat_file_summary_for_adding_new_l1_list([], _S).
write_out_flat_file_summary_for_adding_new_l1_list([F | R], S) :-
	write_out_flat_file_summary_record(F, S),
	!,
	write_out_flat_file_summary_for_adding_new_l1_list(R, S).

write_out_flat_file_summary_record(Record, S) :-
	format(S, '~N~nPrompt~n', []),
	write_out_flat_file_summary_record1(Record, S),
	format(S, '~NEndPrompt~n', []),
	!.

write_out_flat_file_summary_record1([], _S).
write_out_flat_file_summary_record1([F | R], S) :-
	write_out_flat_file_summary_record_line(F, S),
	!,
	write_out_flat_file_summary_record1(R, S).

write_out_flat_file_summary_record_line(Key=Value, S) :-
	write_out_flat_file_summary_record_key(Key, S),
	write_out_flat_file_summary_record_value(Value, S).

write_out_flat_file_summary_record_key(text(L1), S) :-
	format(S, '~NText/~w~20|', [L1]),
	!.
write_out_flat_file_summary_record_key(response, S) :-
	format(S, '~NResponse~20|', []),
	!.
write_out_flat_file_summary_record_key(Key, S) :-
	format_lite_error('~N*** Error: bad call: ~w~n',
	       [write_out_flat_file_summary_record_key(Key, S)]),
	fail.

write_out_flat_file_summary_record_value(Value, S) :-
	format(S, '~w~n', [Value]),
	!.
 
shortest_atom(Atoms, ShortestAtom) :-
	findall(Length-Atom,
		(   member(Atom, Atoms),
		    atom_codes(Atom, Codes),
		    length(Codes, Length)
		),
		Pairs),
	keysort(Pairs, SortedPairs),
	SortedPairs = [_ShortestLength-ShortestAtom | _Rest],
	!.

%----------------------------------------------------

print_units_to_tmp_file(Units) :-
	safe_absolute_file_name('$REGULUS/tmp/tmp_lite_units.pl', File),
	list_to_prolog_file_prettyprint(Units, File),
	length(Units, N),
	format('~N--- ~d units printed to ~w~n', [N, File]),
	!.

%----------------------------------------------------

prompt_to_ignore(Atom) :-
	ignore_prompt_string(IgnoreString),
	atom(Atom),
	atom_codes(Atom, Str),
	is_substring(IgnoreString, Str).


	