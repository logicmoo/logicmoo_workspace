:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(lite_update_grammar,
	[update_grammar/5,
	 
	 test_update_grammar/1]
    ).

:- use_module('$REGULUS/Prolog/lite_read_flat_file').
:- use_module('$REGULUS/Prolog/lite_write_flat_file').
:- use_module('$REGULUS/Prolog/lite_utils').

:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%======================================================================

allow_negative_examples(no).
%allow_negative_examples(yes).

%======================================================================

test_update_grammar(small2) :-
	update_grammar([[['$CALLSLT/Eng/corpora/english_course_flat_templatised_small2.txt', default_encoding, [french, german]]]-user],  %Flat file
		       '$CALLSLT/Eng/corpora/EnglishCourseMay2014/corpus_for_annotation_mr_cleaned_with_wavfiles_small2.pl',  %Annotated corpus
		       '$CALLSLT/Eng/corpora/english_course_flat_templatised_small2_extended.txt',                            %Output
		       call,
		       german,
		       default_encoding).

test_update_grammar(small3) :-
	update_grammar([[['$CALLSLT/Eng/corpora/english_course_flat_templatised_small3.txt', default_encoding, [french, german]]]-user],          %Flat file
		       '$CALLSLT/Eng/corpora/ICASSP2015Spreadsheets/lrec_plus_rumlingen.pl',  %Annotated corpus
		       '$CALLSLT/Eng/corpora/english_course_flat_templatised_small3_extended.txt',                            %Output
		       call,
		       german,
		       default_encoding).

test_update_grammar(small4) :-
	update_grammar([[['$CALLSLT/Eng/corpora/english_course_flat_templatised_small4.txt', default_encoding, [french, german]]]-user],          %Flat file
		       '$CALLSLT/Eng/corpora/EnglishCourseMay2014/corpus_for_annotation_mr_cleaned_with_wavfiles.pl',  %Annotated corpus
		       '$CALLSLT/Eng/corpora/english_course_flat_templatised_small4_extended.txt',                            %Output
		       call,
		       german,
		       default_encoding).

test_update_grammar(1) :-
	update_grammar([[['$CALLSLT/Eng/corpora/english_course_flat_templatised.txt', default_encoding, [french, german]]]-user],          %Flat file
		       '$CALLSLT/Eng/corpora/EnglishCourseMay2014/corpus_for_annotation_mr_cleaned_with_wavfiles.pl',  %Annotated corpus
		       '$CALLSLT/Eng/corpora/english_course_flat_templatised_extended.txt',                            %Output
		       call,
		       german,
		       default_encoding).

test_update_grammar(rumlingen) :-
	update_grammar([[['$CALLSLT/Eng/corpora/english_course_flat_templatised.txt', default_encoding, [french, german]]]-user],          %Flat file
		       '$CALLSLT/Eng/corpora/ICASSP2015Spreadsheets/rumlingen_corpus_full.pl',  %Annotated corpus
		       '$CALLSLT/Eng/corpora/english_course_flat_templatised_extended.txt',                            %Output
		       call,
		       german,
		       default_encoding),
	read_flat_file([[['$CALLSLT/Eng/corpora/english_course_flat_templatised_extended.txt', default_encoding, [french, german]],
			 ['$CALLSLT/Eng/corpora/english_course_flat_chinese.txt', 'UTF-8', [chinese]]]-user],
		       _List,
		       call).

test_update_grammar(lrec_plus_rumlingen) :-
	test_update_grammar(lrec_plus_rumlingen_full).

test_update_grammar(lrec_plus_rumlingen_minimal) :-
	test_update_grammar(lrec_plus_rumlingen(minimal)).

test_update_grammar(lrec_plus_rumlingen_templates) :-
	test_update_grammar(lrec_plus_rumlingen(templates)).

test_update_grammar(lrec_plus_rumlingen_full) :-
	test_update_grammar(lrec_plus_rumlingen(full)).

test_update_grammar(lrec_plus_rumlingen(CompileType)) :-
	update_grammar([[['$CALLSLT/Eng/corpora/english_course_flat_templatised.txt', default_encoding, [french, german]]]-user],          %Flat file
		       '$CALLSLT/Eng/corpora/ICASSP2015Spreadsheets/lrec_plus_rumlingen.pl',  %Annotated corpus
		       '$CALLSLT/Eng/corpora/english_course_flat_templatised_extended.txt',                            %Output
		       call,
		       german,
		       default_encoding,
		       CompileType),
	read_flat_file([[['$CALLSLT/Eng/corpora/english_course_flat_templatised_extended.txt', default_encoding, [french, german]],
			 ['$CALLSLT/Eng/corpora/english_course_flat_chinese.txt', 'UTF-8', [chinese]]]-user],
		       _List,
		       call).

		       
%======================================================================

update_grammar(FlatGrammarFileIn, AnnotationsFile, FlatGrammarFileOut, Type, L1, Encoding) :-
	update_grammar(FlatGrammarFileIn, AnnotationsFile, FlatGrammarFileOut, Type, L1, Encoding, full).

update_grammar(FlatGrammarFileIn, AnnotationsFile, FlatGrammarFileOut, Type, L1, Encoding, CompileType) :-
	load_annotations_file(AnnotationsFile),
	read_flat_file(FlatGrammarFileIn, UnitsIn, Type),
	load_templates(UnitsIn),
	load_responses(UnitsIn),
	
	update_units_list(UnitsIn, UnitsOut, L1, 0-N),
	include_new_units(UnitsOut, UnitsOut1, CompileType, 0-Good, 0-Bad),
	
	sort_new_units(UnitsOut1, PlainUnits-[], PreTemplates-[], PreTemplateApplications-[]),
	add_pre_templates_to_current_templates(PreTemplates, CompileType, NewTemplates),
	combine_pre_template_applications(PreTemplateApplications, TemplateApplications),

	insert_template_applications_after_templates(NewTemplates, TemplateApplications, NewTemplatesAndTemplateApplications-[]),

	append_list([PlainUnits, NewTemplatesAndTemplateApplications], AllUnitsOut),
	safe_absolute_file_name(FlatGrammarFileOut, AbsFlatGrammarFileOut),
	write_flat_file(AllUnitsOut, FlatGrammarFileOut, Type, Encoding),
	format('~N--- Processed ~d flat units. Added ~d positive and ~d negative responses~n', [N, Good, Bad]),
	format('~N--- Modified grammar written to ~w~n', [AbsFlatGrammarFileOut]),
	!.

:- dynamic loaded_corpus_record/2.

load_annotations_file(File) :-
	retractall(loaded_corpus_record(_, _)),
	safe_absolute_file_name(File, AbsFile),
	prolog_file_to_list(AbsFile, List),
	length(List, N),
	load_annotations_file1(List),
	format('~N--- Loaded annotations file (~d entries) ~w~n', [N, AbsFile]),
	!.
load_annotations_file(File) :-
	format('~N*** Error: bad call: ~w~n',
	       [load_annotations_file(File)]),
	fail.

load_annotations_file1([]).
load_annotations_file1([F | R]) :-
	load_annotations_file_record(F),
	!,
	load_annotations_file1(R).

load_annotations_file_record(corpus_record(List)) :-
	clean_annotation_file_record(List, List1),
	member(prompt=Prompt, List1),
	assertz(loaded_corpus_record(Prompt, List1)),
	!.
load_annotations_file_record(Record) :-
	format('~N*** Error: bad call: ~w~n',
	       [load_annotations_file_record(Record)]),
	fail.

clean_annotation_file_record([], []).
clean_annotation_file_record([F | R], [F1 | R1]) :-
	clean_annotation_file_record_field(F, F1),
	!,
	clean_annotation_file_record(R, R1).

clean_annotation_file_record_field(correctWords=Words, correctWords=Words1) :-
	make_response_canonical(Words, Words1),
	!.
clean_annotation_file_record_field(Other, Other).

make_response_canonical(Atom, Atom1) :-
	lowercase_atom(Atom, LowercaseAtom),
	split_atom_into_words(LowercaseAtom, Words),
	join_with_spaces(Words, Atom1).

:- dynamic loaded_template/2.

load_templates(Units) :-
	retractall(loaded_template(_, _)),
	load_templates1(Units, 0-N),
	format('~N--- Loaded ~d templates~n', [N]),
	!.
load_templates(_Units) :-
	format('~N*** Error: unable to load templates~n', []),
	!.

load_templates1([], N-N).
load_templates1([F | R], In-Out) :-
	load_templates2(F, In-Next),
	!,
	load_templates1(R, Next-Out).

load_templates2(template_unit(_Lines, List), In-Out) :-
	member(template_name=Name, List),
	assertz(loaded_template(Name, List)),
	Out is In + 1,
	!.
load_templates2(_Other, I-I).

:- dynamic loaded_response/1.

load_responses(Units) :-
	retractall(loaded_response(_)),
	load_responses1(Units, 0-N),
	format('~N--- Loaded ~d responses~n', [N]),
	!.
load_responses(_Units) :-
	format('~N*** Error: unable to load responses~n', []),
	!.

load_responses1([], N-N).
load_responses1([F | R], In-Out) :-
	load_responses2(F, In-Next),
	!,
	load_responses1(R, Next-Out).

load_responses2(unit(_Lines, List), In-Out) :-
	member(responses=Responses, List),
	length(Responses, N),
	load_responses3(Responses),
	Out is In + N,
	!.
load_responses2(_Other, In-In).

load_responses3([]).
load_responses3([F | R]) :-
	assertz(loaded_response(F)),
	!,
	load_responses3(R).

%------------------------------------------

update_units_list([], [], _L1, N-N).
update_units_list([F | R], [F1 | R1], L1, Nin-Nout) :-
	update_unit(F, F1, L1),
	Nnext is Nin + 1,
	format('.', []),
	(   0 is Nnext mod 100 ->
	    
	    format(' (~d) ~n', [Nnext])
	;
	    
	    true
	),
	flush_output(user),
	!,
	update_units_list(R, R1, L1, Nnext-Nout).

update_unit(unit(Lines, List), unit(Lines, List, NewResponses), L1) :-
	corpus_examples_relevant_to_unit(List, L1, PossibleCorrect, PossibleIncorrect),
	NewResponses = [new_responses=PossibleCorrect, new_incorrect_responses=PossibleIncorrect],
	!.
update_unit(Other, Other, _L1) :-
	!.

/*

Format example for internalized flat record:

unit((151-151), 
     [(namespace=user), (domain=english_course), (lesson=airport), (group=leaving_date), 
      (text(french)='Dis : Je veux partir le dimanche matin'), 
      (text(german)='Sag : Ich möchte am Sonntagmorgen abreisen'), 
      (responses = 
       ['i need to leave on sunday morning please', 'i need to leave on sunday morning', 
        'i want to leave on sunday morning please', 'i want to leave on sunday morning', 
        'i would like to leave on sunday morning please', 
        'i would like to leave on sunday morning']),
      (incorrect_responses=[]), (lf=i_need_to_leave_on_sunday_morning_please), 
      (incorrect_lf=incorrect_version_of_i_need_to_leave_on_sunday_morning_please)]).

Format example for annotated corpus record:

corpus_record([(user='Cyril'), (time='2013-11-20_12-33-49'), (lesson=hotel), 
               (prompt='Sag : Ich möchte mit Visa bezahlen'), 
               (recognisedWords='i would like to pay with visa'), 
               (correctWords='i would like to pay with visa'), (help=no_help), (rec=rec), 
               (wavfile = 
                '$CALLSLT/Eval/EnglishCourse2013/Data/wavfiles/cyril/2013-11-20_12-29-37/utt_028.wav'),
               (recordingQuality='low background noise'), (vocabulary=correct), 
               (grammar=correct), (pronunciation='3 correct'), (fluency='3 correct'), 
               (fluencyAdditionalInfo='')]).

*/

corpus_examples_relevant_to_unit(List, L1, PossibleCorrect, PossibleIncorrect) :-
	member(lesson=Lesson, List),
	member(text(L1)=Prompt, List),
	examples_for_lesson_and_prompt(Lesson, Prompt, Pairs),
	separate_examples_into_correct_and_incorrect(Pairs, PossibleCorrect, PossibleIncorrect),
	!.
corpus_examples_relevant_to_unit(List, L1, PossibleCorrect, PossibleIncorrect) :-
	format('~N*** Error: bad call: ~w~n',
	       [corpus_examples_relevant_to_unit(List, L1, PossibleCorrect, PossibleIncorrect)]),
	fail.

examples_for_lesson_and_prompt(Lesson, Prompt, Pairs) :-
	findall([Example, CorrectP],
		example_for_lesson_and_prompt(Lesson, Prompt, Example, CorrectP),
		Pairs).

example_for_lesson_and_prompt(Lesson, Prompt, Example, CorrectP) :-
	loaded_corpus_record(Prompt, List),
	member(lesson=Lesson, List),
	member(correctWords=Example, List),
	% No point in using examples we've already included, or discarded sentences
	\+ loaded_response(Example),
	Example \== discard,
	(   ( member(vocabulary=correct, List), member(grammar=correct, List) ) ->
	    CorrectP = correct
	;
	    allow_negative_examples(no) ->
	    fail
	;
	    otherwise ->
	    CorrectP = incorrect
	).

separate_examples_into_correct_and_incorrect(Pairs, PossibleCorrect, PossibleIncorrect) :-
	findall(Correct,
		member([Correct, correct], Pairs),
		PossibleCorrect),
	findall(Incorrect,
		member([Incorrect, incorrect], Pairs),
		PossibleIncorrect),
	!.

include_new_units([], [], _CompileType, Gin-Gin, Bin-Bin).
include_new_units([F | R], Out, CompileType, Gin-Gout, Bin-Bout) :-
	include_new_units1(F, Out-R1, CompileType, Gin-Gnext, Bin-Bnext),
	!,
	include_new_units(R, R1, CompileType, Gnext-Gout, Bnext-Bout).

include_new_units1(unit(Lines, List, NewResponses), Out-R1, CompileType, Gin-Gout, Bin-Bout) :-
	member(template_application=_, List),
	member(CompileType, [templates, full]),
	!,
	include_new_units_template_application(unit(Lines, List, NewResponses), Out-R1, Gin-Gout, Bin-Bout).
include_new_units1(unit(Lines, List, NewResponses), [unit(Lines, List1) | R1]-R1, _CompileType, Gin-Gout, Bin-Bout) :-
	member(new_responses=PossibleCorrect, NewResponses),
	member(new_incorrect_responses=PossibleIncorrect, NewResponses),
	member(responses=Responses, List),
	include_new_units2(List, Responses, PossibleCorrect, PossibleIncorrect, List1, Gin-Gout, Bin-Bout),
	!.
include_new_units1(Unit, [Unit | R]-R, _CompileType, Gin-Gin, Bin-Bin) :-
	Unit = group_unit(_Lines, _List),
	!.
include_new_units1(Unit, [Unit | R]-R, _CompileType, Gin-Gin, Bin-Bin) :-
	Unit = multimedia_unit(_Lines, _List),
	!.
include_new_units1(Unit, [Unit | R]-R, _CompileType, Gin-Gin, Bin-Bin) :-
	Unit = phrase_score_unit(_Lines, _List),
	!.
include_new_units1(_Other, Out-Out, _CompileType, Gin-Gin, Bin-Bin).

include_new_units2([], _Responses, _PossibleCorrect, _PossibleIncorrect, [], Gin-Gin, Bin-Bin).
include_new_units2([unexpanded_responses=UnexpandedResponses | R], Responses, PossibleCorrect, PossibleIncorrect,
		   [unexpanded_responses=UnexpandedResponses1 | R], Gin-Gout, Bin-Bout) :-
	add_new_responses(UnexpandedResponses, Responses, PossibleCorrect, PossibleIncorrect, UnexpandedResponses1, Gin-Gout, Bin-Bout),
	!.
include_new_units2([F | R], Responses, PossibleCorrect, PossibleIncorrect, [F | R1], Gin-Gout, Bin-Bout) :-
	!,
	include_new_units2(R, Responses, PossibleCorrect, PossibleIncorrect, R1, Gin-Gout, Bin-Bout).

add_new_responses(UnexpandedResponses, Responses, PossibleCorrect, PossibleIncorrect, UnexpandedResponses1, Gin-Gout, Bin-Bout) :-
	findall(CorrectResponse1,
		(   member(CorrectResponse, PossibleCorrect),
		    \+ member(CorrectResponse, Responses),
		    mark_response_as_correct(CorrectResponse, CorrectResponse1)
		),
		NewCorrectResponses0
	       ),
	sort(NewCorrectResponses0, NewCorrectResponses),
	
	findall(IncorrectResponse1,
		(   member(IncorrectResponse, PossibleIncorrect),
		    \+ null_response(IncorrectResponse),
		    mark_response_as_incorrect(IncorrectResponse, IncorrectResponse1)
		),
		NewIncorrectResponses0
	       ),
	sort(NewIncorrectResponses0, NewIncorrectResponses),
	
	append_list([UnexpandedResponses, NewCorrectResponses, NewIncorrectResponses], UnexpandedResponses1),
	length(NewCorrectResponses, NNewCorrect),
	length(NewIncorrectResponses, NNewIncorrect),
	Gout is Gin + NNewCorrect,
	Bout is Bin + NNewIncorrect,
	!.
add_new_responses(UnexpandedResponses, Responses, PossibleCorrect, PossibleIncorrect, UnexpandedResponses1, G, B) :-
	format('~N*** Error: bad call: ~w~n',
	       [add_new_responses(UnexpandedResponses, Responses, PossibleCorrect, PossibleIncorrect, UnexpandedResponses1, G, B)]),
	fail.

%----------------------------------------

/*

template_unit((111-118), 
              [(namespace=user), (template_name=i_want_to_leave_on_date), 
               (template_args=[A,B,C]), (domain=[english_course]), (lesson=[airport]), 
               (group=[leaving_date]), (text(french)=['Dis : Je veux partir ',A]), 
               (text(german)=['Sag : Ich möchte am ',B,' abreisen']), 
               (unexpanded_responses = 
                [['i ( need | want | would like ) to leave on ',C,' ?please']]),
               (responses = 
                [[i,need,to,leave,on,C], [i,need,to,leave,on,C,please], [i,want,to,leave,on,C],
                 [i,want,to,leave,on,C,please], [i,would,like,to,leave,on,C], 
                 [i,would,like,to,leave,on,C,please]]),
               (incorrect_responses=[])]).

template_application=[i_want_to_leave_on_date,['le lundi','Montag',monday]]

*/

include_new_units_template_application(unit(_Lines, List, NewResponses), Out-R1, Gin-Gout, Bin-Bout) :-
	member(template_application=[TemplateName, Args], List),
	member(responses=OldResponses, List),
	loaded_template(TemplateName, TemplateList),
	member(template_args=FormalArgs, TemplateList),
	member(responses=AbstractResponses, TemplateList),
	find_response_args(FormalArgs, Args, AbstractResponses, ResponseArgPairs),
	make_trivial_pre_template_application_unit(TemplateName, Args, TrivialUnit),
	Out = [TrivialUnit | Next],
	include_new_units_template_app1(NewResponses, OldResponses, 
					AbstractResponses, ResponseArgPairs, TemplateName, Args, FormalArgs,
					Next-R1, Gin-Gout, Bin-Bout),
	!.
include_new_units_template_application(_Other, R1-R1, Gin-Gin, Bin-Bin).

find_response_args([], [], _AbstractResponses, []).
find_response_args([FormalArg | FormalArgs], [Arg | Args], AbstractResponses, [FormalArg=Arg1 | R]) :-
	term_contains_subterm(AbstractResponses, FormalArg),
	split_atom_into_words(Arg, Arg1),
	!,
	find_response_args(FormalArgs, Args, AbstractResponses, R).
find_response_args([_FormalArg | FormalArgs], [_Arg | Args], AbstractResponses, R) :-
	!,
	find_response_args(FormalArgs, Args, AbstractResponses, R).

include_new_units_template_app1(NewResponses, OldResponses, 
				AbstractResponses, ResponseArgPairs, TemplateName, Args, FormalArgs,
				In-Out, Gin-Gout, Bin-Bout) :-
	member(new_responses=PossibleCorrect, NewResponses),
	member(new_incorrect_responses=PossibleIncorrect, NewResponses),
	include_new_units_template_app2(PossibleCorrect, correct, OldResponses, 
					AbstractResponses, ResponseArgPairs, TemplateName, Args, FormalArgs,
					In-Next, Gin-Gnext, Bin-Bnext),
	include_new_units_template_app2(PossibleIncorrect, incorrect, OldResponses, 
					AbstractResponses, ResponseArgPairs, TemplateName, Args, FormalArgs,
					Next-Out, Gnext-Gout, Bnext-Bout).

include_new_units_template_app2([], _CorrectP, _OldResponses, 
				_AbstractResponses, _ResponseArgPairs, _TemplateName, _Args, _FormalArgs,
				In-In, Gin-Gin, Bin-Bin).
include_new_units_template_app2([F | R], CorrectP, OldResponses, 
				AbstractResponses, ResponseArgPairs, TemplateName, Args, FormalArgs,
				In-Out, Gin-Gout, Bin-Bout) :-
	include_new_units_template_app3(F, CorrectP, OldResponses, 
					AbstractResponses, ResponseArgPairs, TemplateName, Args, FormalArgs,
					In-Next, Gin-Gnext, Bin-Bnext),
	!,
	include_new_units_template_app2(R, CorrectP, OldResponses, 
					AbstractResponses, ResponseArgPairs, TemplateName, Args, FormalArgs,
					Next-Out, Gnext-Gout, Bnext-Bout).

include_new_units_template_app3(Response, _CorrectP, OldResponses, 
				_AbstractResponses, _ResponseArgPairs, _TemplateName, _Args, _FormalArgs,
				In-In, Gin-Gin, Bin-Bin) :-
	member(Response, OldResponses),
	!.
include_new_units_template_app3(Response, _CorrectP, _OldResponses, _AbstractResponses, _ResponseArgPairs, _TemplateName, _Args, _FormalArgs,
				In-In, Gin-Gin, Bin-Bin) :-
	null_response(Response),
	!.
include_new_units_template_app3(Response, CorrectP, OldResponses, AbstractResponses, ResponseArgPairs, TemplateName, Args, FormalArgs,
				In-Out, Gin-Gout, Bin-Bout) :-
	template_or_template_application_response(Response, CorrectP, OldResponses, AbstractResponses, ResponseArgPairs, TemplateName, Args, FormalArgs, Unit),
	In = [Unit | Out],
	(   CorrectP = correct ->
	    Gout is Gin + 1,
	    Bout is Bin
	;
	    otherwise ->
	    Gout is Gin,
	    Bout is Bin + 1
	),
	!.
include_new_units_template_app3(_Response, _CorrectP, _OldResponses, _AbstractResponses, _ResponseArgPairs, _TemplateName, _Args, _FormalArgs,
				In-In, Gin-Gin, Bin-Bin) :-
	!.

make_trivial_pre_template_application_unit(TemplateName, Args, Unit) :-
	Unit = pre_template_application_unit(TemplateName, Args, null, null, correct).

/*
Args = ['une soeur cadette','eine jüngere Schwester','one younger sister']
ResponseWords = [i, have, a, young, sister]
AbstractResponse = [i, have, ENGLISH]
ENGLISH = [a, young, sister]
TemplateApplicationUnit=pre_template_application_unit(i_have_sibling,
						      ['une soeur cadette','eine jüngere Schwester','one younger sister'],
						      'one younger sister',
						      'a young sister',
						      incorrect)
*/
% We are able to generalise the response into a pre-template, since it contains a string corresponding to one of the response args.
%template_or_template_application_response(Response, CorrectP, _OldResponses, AbstractResponses, ResponseArgPairs, TemplateName,
%					  Args, _FormalArgs, TemplateApplicationUnit) :-
%	split_atom_into_words(Response, ResponseWords),
%	member(AbstractResponse, AbstractResponses),
%	list_match(ResponseWords, AbstractResponse, Substitutions-[]),
%	Substitutions = [FormalArg=NewArg0],
%	member(FormalArg=OldArg0, ResponseArgPairs),
%	join_with_spaces(OldArg0, OldArg),
%	join_with_spaces(NewArg0, NewArg),
%	TemplateApplicationUnit = pre_template_application_unit(TemplateName, Args, OldArg, NewArg, CorrectP),
%	!.
template_or_template_application_response(Response, CorrectP, _OldResponses, AbstractResponses, _ResponseArgPairs, TemplateName,
					  Args, FormalArgs, TemplateApplicationUnit) :-
	split_atom_into_words(Response, ResponseWords),
	member(AbstractResponse, AbstractResponses),
	list_match(ResponseWords, AbstractResponse, Substitutions-[]),
	Substitutions = [FormalArg=NewArg0],
	%member(FormalArg=OldArg0, ResponseArgPairs),
	safe_nth(ArgPos, FormalArgs, FormalArg),
	%join_with_spaces(OldArg0, OldArg),
	join_with_spaces(NewArg0, NewArg),
	%TemplateApplicationUnit = pre_template_application_unit(TemplateName, Args, OldArg, NewArg, CorrectP),
	TemplateApplicationUnit = pre_template_application_unit(TemplateName, Args, ArgPos, NewArg, CorrectP),
	!.

/*
ResponseWords = [i, got, a, younger sister]
Arg = [a, younger, sister]
Response = [i, got, C]
pre_template=pre_template_unit(i_have_sibling, '* i got C')
*/
template_or_template_application_response(Response, CorrectP, _OldResponses, _AbstractResponses, ResponseArgPairs, TemplateName,
					  _Args, _FormalArgs, PreTemplateUnit) :-
	split_atom_into_words(Response, ResponseWords),
	list_var(front, FrontVar),
	list_var(back, BackVar),
	member(FormalArg=Arg, ResponseArgPairs),
	append_list([[FrontVar], Arg, [BackVar]], Pattern),
	list_match(ResponseWords, Pattern, Substitutions-[]),
	member(FrontVar=Front, Substitutions),
	member(BackVar=Back, Substitutions),
	append_list([Front, [FormalArg], Back], NewResponse),
	PreTemplateUnit = pre_template_unit(TemplateName, NewResponse, CorrectP),
	!.	

%----------------------------------------

sort_new_units([], PlainUnitsIn-PlainUnitsIn, PreTemplatesIn-PreTemplatesIn, PreTemplateApplicationsIn-PreTemplateApplicationsIn).
sort_new_units([F | R], PlainUnitsIn-PlainUnitsOut, PreTemplatesIn-PreTemplatesOut, PreTemplateApplicationsIn-PreTemplateApplicationsOut) :-
	(   is_pre_template_unit(F) ->
	    PlainUnitsIn = PlainUnitsNext,
	    PreTemplatesIn = [F | PreTemplatesNext],
	    PreTemplateApplicationsIn = PreTemplateApplicationsNext
	;
	    is_pre_template_application_unit(F) ->
	    PlainUnitsIn = PlainUnitsNext,
	    PreTemplatesIn = PreTemplatesNext,
	    PreTemplateApplicationsIn = [F | PreTemplateApplicationsNext]
	;
	    % We are including the template units as part of processing the new pre-template units.
	    is_template_unit(F) ->
	    PlainUnitsIn = PlainUnitsNext,
	    PreTemplatesIn = PreTemplatesNext,
	    PreTemplateApplicationsIn = PreTemplateApplicationsNext
	;
	    otherwise ->
	    PlainUnitsIn = [F | PlainUnitsNext],
	    PreTemplatesIn = PreTemplatesNext,
	    PreTemplateApplicationsIn = PreTemplateApplicationsNext
	),
	!,
	sort_new_units(R, PlainUnitsNext-PlainUnitsOut, PreTemplatesNext-PreTemplatesOut, PreTemplateApplicationsNext-PreTemplateApplicationsOut).

is_template_unit(Unit) :-
	compound(Unit),
	functor(Unit, template_unit, _).

is_pre_template_unit(Unit) :-
	compound(Unit),
	functor(Unit, pre_template_unit, _).

is_pre_template_application_unit(Unit) :-
	compound(Unit),
	functor(Unit, pre_template_application_unit, _).

%----------------------------------------

add_pre_templates_to_current_templates(PreTemplates, CompileType, NewTemplates) :-
	get_all_stored_template_units(OldTemplates),
	add_pre_templates_to_current_templates1(OldTemplates, PreTemplates, CompileType, NewTemplates),
	!.

get_all_stored_template_units(OldTemplates) :-
	findall(List,
		loaded_template(_Name, List),
		OldTemplates).

add_pre_templates_to_current_templates1([], _PreTemplates, _CompileType, []).
add_pre_templates_to_current_templates1([F | R], PreTemplates, CompileType, [F1 | R1]) :-
	add_pre_templates_to_template(F, PreTemplates, CompileType, F1),
	!,
	add_pre_templates_to_current_templates1(R, PreTemplates, CompileType, R1).

add_pre_templates_to_template(TemplateUnit, PreTemplates, CompileType, TemplateUnit2) :-
	pre_templates_in_list_for_template_unit(PreTemplates, TemplateUnit, RelevantPreTemplates),
	pre_templates_to_responses(RelevantPreTemplates, Responses),
	add_responses_to_template1(TemplateUnit, Responses, CompileType, TemplateUnit1),
	lists_with_list_vars_to_atom_in_template_unit(TemplateUnit1, TemplateUnit2).

add_responses_to_template1(List, NewResponses, CompileType, template_unit(null_lines, List1)) :-
	add_responses_to_template2(List, NewResponses, CompileType, List1).

add_responses_to_template2([unexpanded_responses=UnexpandedResponses0 | R],
			   NewResponses, CompileType,
			   [unexpanded_responses=UnexpandedResponses1 | R]) :-
	turn_unexpanded_responses_into_atoms(UnexpandedResponses0, UnexpandedResponses),
	add_new_responses_to_unexpanded_responses(UnexpandedResponses, NewResponses, CompileType, UnexpandedResponses1),
	!.
add_responses_to_template2([F | R], NewResponses, CompileType, [F | R1]) :-
	!,
	add_responses_to_template2(R, NewResponses, CompileType, R1).

turn_unexpanded_responses_into_atoms([], []).
turn_unexpanded_responses_into_atoms([F | R], [F1 | R1]) :-
	turn_unexpanded_response_into_atom(F, F1),
	!,
	turn_unexpanded_responses_into_atoms(R, R1).

turn_unexpanded_response_into_atom(UnexpandedResponseIn, UnexpandedResponseOut) :-
	list_with_list_vars_to_atom(UnexpandedResponseIn, UnexpandedResponseOut),
	!.
turn_unexpanded_response_into_atom(UnexpandedResponseIn, UnexpandedResponseOut) :-
	format('~N*** Error: bad call: ~w~n',
	       [turn_unexpanded_response_into_atom(UnexpandedResponseIn, UnexpandedResponseOut)]),
	fail.

pre_templates_in_list_for_template_unit(PreTemplates, TemplateUnit, RelevantPreTemplates) :-
	findall(PreTemplate,
		(   member(PreTemplate, PreTemplates),
		    pre_template_matches_template_unit(PreTemplate, TemplateUnit)
		),
		RelevantPreTemplates).

pre_template_matches_template_unit(PreTemplate, TemplateUnit) :-
	PreTemplate = pre_template_unit(TemplateName, _ResponseWords, _CorrectP),
	member(template_name=TemplateName, TemplateUnit),
	!.

pre_templates_to_responses([], []).
pre_templates_to_responses([F | R], [F1 | R1]) :-
	pre_template_to_response(F, F1),
	!,
	pre_templates_to_responses(R, R1).

pre_template_to_response(PreTemplate, Response) :-
	PreTemplate = pre_template_unit(_TemplateName, ResponseWords, CorrectP),
	replace_list_vars_by_names_in_list(ResponseWords, ResponseWords1),
	(   CorrectP = incorrect ->
	    ResponseWords2 = ['*' | ResponseWords1]
	;
	    otherwise ->
	    ResponseWords2 = ResponseWords1
	),
	join_with_spaces(ResponseWords2, Response),
	!.

%------------------------------------------

% If we're using the 'templates' strategy, just append the new responses after the unexpanded ones. 
add_new_responses_to_unexpanded_responses(UnexpandedResponses, NewResponses, CompileType, UnexpandedResponses1) :-
	CompileType \== full,
	mark_responses_as_new(NewResponses, NewResponses1),
	sort(NewResponses1, NewResponses2),
	append(UnexpandedResponses, NewResponses2, UnexpandedResponses1),
	!.
% If we're using the 'full' strategy, try to perform regular-expression merging.
add_new_responses_to_unexpanded_responses(UnexpandedResponses, NewResponses, _CompileType, UnexpandedResponses1) :-
	init_add_new_responses(UnexpandedResponses, UnexpandedResponsesWrapped,
			       NewResponses, NewResponsesWrappedIn),
	add_new_responses_to_unexpanded_responses1(NewResponsesWrappedIn-NewResponsesWrappedOut, 
						   UnexpandedResponsesWrapped-MergedResponsesWrapped),
	strip_wrappers_in_add_new_responses(MergedResponsesWrapped, MergedResponses),
	strip_wrappers_in_add_new_responses(NewResponsesWrappedOut, NewResponsesOut),
	sort(NewResponsesOut, NewResponsesOutSorted),
	append(MergedResponses, NewResponsesOutSorted, UnexpandedResponses1),
	!.
add_new_responses_to_unexpanded_responses(UnexpandedResponses, NewResponses, CompileType, UnexpandedResponses1) :-
	format('~N*** Error: bad call: ~w~n',
	       [add_new_responses_to_unexpanded_responses(UnexpandedResponses, NewResponses, CompileType, UnexpandedResponses1)]),
	fail.

:- dynamic failed_rmerge/2.

rmerge_threshold(1).

init_add_new_responses(UnexpandedResponses, UnexpandedResponsesWrapped,
		       NewResponses, NewResponsesWrapped) :-
	retractall(failed_rmerge(_, _)),
	wrap_list(UnexpandedResponses, plain, UnexpandedResponsesWrapped),
	wrap_as_correct_or_incorrect(NewResponses, NewResponsesWrapped),
	!.

wrap_list([], _Tag, []).
wrap_list([F | R], Tag, [s(Tag, F) | R1]) :-
	wrap_list(R, Tag, R1).

wrap_as_correct_or_incorrect([], []).
wrap_as_correct_or_incorrect([F | R], [s(Tag, F) | R1]) :-
	atom_codes(F, Str),
	(   member(0'*, Str) ->
	    Tag = incorrect
	;
	    otherwise ->
	    Tag = correct
	),
	!,
	wrap_as_correct_or_incorrect(R, R1).

strip_wrappers_in_add_new_responses([], []).
strip_wrappers_in_add_new_responses([F | R], [F1 | R1]) :-
	strip_wrapper_in_add_new_response(F, F1),
	!,
	strip_wrappers_in_add_new_responses(R, R1).

strip_wrapper_in_add_new_response(s(plain, Sent), Sent) :-
	!.
strip_wrapper_in_add_new_response(s(correct, Sent), Sent1) :-
	mark_response_as_new(Sent, Sent1),
	!.
strip_wrapper_in_add_new_response(s(incorrect, Sent), Sent1) :-
	mark_response_as_new(Sent, Sent1),
	!.
strip_wrapper_in_add_new_response(s(merged, Sent), Sent1) :-
	mark_response_as_merged(Sent, Sent1),
	!.

add_new_responses_to_unexpanded_responses1(NewIn-NewOut, MergedIn-MergedOut) :-
	add_new_responses_to_unexpanded_responses2(NewIn, NewNext-[], MergedIn-MergedNext),
	!,
	% Nothing has been merged, and there are no correct utterances left. We're done.
	(   ( NewIn = NewNext, \+ member(s(correct, _), NewIn) ) ->
	    NewOut = NewNext,
	    MergedOut = MergedNext
	;
	    % Nothing has been merged, but there are correct utterances left.
	    % Move one of them to "Merged" and see if we can do some more merging.
	    NewIn = NewNext ->
	    delete(NewNext, s(correct, Sent), NewNext1),
	    append(MergedNext, [s(correct, Sent)], MergedNext1),
	    add_new_responses_to_unexpanded_responses1(NewNext1-NewOut, MergedNext1-MergedOut)
	;
	    % We did succeed in merging something, go round again.
	    otherwise ->
	    add_new_responses_to_unexpanded_responses1(NewNext-NewOut, MergedNext-MergedOut)
	).

add_new_responses_to_unexpanded_responses2([], NewIn-NewIn, MergedIn-MergedIn).
add_new_responses_to_unexpanded_responses2([F | R], NewIn-NewOut, MergedIn-MergedOut) :-
	add_new_responses_to_unexpanded_responses3(F, NewIn-NewNext, MergedIn-MergedNext),
	!,
	add_new_responses_to_unexpanded_responses2(R, NewNext-NewOut, MergedNext-MergedOut).

add_new_responses_to_unexpanded_responses3(Sent, [Sent | NewOut]-NewOut, MergedIn-MergedIn) :-
	Sent = s(incorrect, _),
	!.
add_new_responses_to_unexpanded_responses3(s(correct, Sent), NewIn-NewOut, MergedIn-MergedOut) :-
	add_new_responses_to_unexpanded_responses4(MergedIn, Sent, MergedOut, no_merge-Result),
	(   Result = no_merge ->
	    NewIn = [s(correct, Sent) | NewOut]
	;
	    otherwise ->
	    NewIn = NewOut
	),
	!.

add_new_responses_to_unexpanded_responses4([], _Sent, [], Merged-Merged).
add_new_responses_to_unexpanded_responses4([F | R], Sent, [F1 | R1], MergedIn-MergedOut) :-
	F = s(_Tag, FSent),
	(   rmerge_sent_and_pattern_with_caching(Sent, FSent, F1Sent) ->
	    F1 = s(merged, F1Sent),
	    MergedOut = merged,
	    R1 = R
	;
	    otherwise ->
	    F1 = F,
	    MergedNext = MergedIn,
	    !,
	    add_new_responses_to_unexpanded_responses4(R, Sent, R1, MergedNext-MergedOut)
	).

rmerge_sent_and_pattern_with_caching(Sent, Pattern, Pattern1) :-
	rmerge_threshold(Threshold),
	(   failed_rmerge(Sent, Pattern) ->
	    fail
	;
	    ( rmerge_sentence_and_pattern(Sent, Pattern, in_template, Score, Pattern1), Score =< Threshold ) ->
	    true
	;
	    assertz(failed_rmerge(Sent, Pattern)),
	    fail
	).					   

%------------------------------------------


lists_with_list_vars_to_atom_in_template_unit(template_unit(Lines, List),
					      template_unit(Lines, List1)) :-
	lists_with_list_vars_to_atom_in_template_unit1(List, List1).

lists_with_list_vars_to_atom_in_template_unit1([], []).
lists_with_list_vars_to_atom_in_template_unit1([F | R], [F1 | R1]) :-
	lists_with_list_vars_to_atom_in_template_unit2(F, F1),
	!,
	lists_with_list_vars_to_atom_in_template_unit1(R, R1).

lists_with_list_vars_to_atom_in_template_unit2(tags=Val, tags=Val1) :-
	!,
	list_vars_to_atom_in_alist(Val, Val1).
lists_with_list_vars_to_atom_in_template_unit2(Key=Val, Key=Val1) :-
	member(Key, [domain, lesson, group, text(_)]),
	!,
	list_with_list_vars_to_atom(Val, Val1).
lists_with_list_vars_to_atom_in_template_unit2(Key=Val, Key=Val).

list_vars_to_atom_in_alist([], []).
list_vars_to_atom_in_alist([Key=Val | R], [Key=Val1 | R1]) :-
	list_with_list_vars_to_atom(Val, Val1),
	!,
	list_vars_to_atom_in_alist(R, R1).

list_with_list_vars_to_atom(List, Atom) :-
	replace_list_vars_by_names_in_list(List, List1),
	append_atoms(List1, Atom),
	!.

replace_list_vars_by_names_in_list([], []).
replace_list_vars_by_names_in_list([F | R], [F1 | R1]) :-
	replace_list_var_by_name_in_list(F, F1),
	!,
	replace_list_vars_by_names_in_list(R, R1).

replace_list_var_by_name_in_list(F, F1) :-
	list_var(Name, F),
	!,
	F1 = Name.
replace_list_var_by_name_in_list(F, F1) :-
	atomic(F),
	F1 = F,
	!.
replace_list_var_by_name_in_list(F, F1) :-
	format('~N*** Error: bad call: ~w~n', [replace_list_var_by_name_in_list(F, F1)]),
	fail.
	
%----------------------------------------

combine_pre_template_applications(PreTemplateApplications, TemplateApplications) :-
	sort_pre_template_applications(PreTemplateApplications, PreTemplateApplicationGroups),
	combine_pre_template_applications1(PreTemplateApplicationGroups, TemplateApplications).

% pre_template_application_unit(TemplateName, Args, OldArg, NewArg, CorrectP)
sort_pre_template_applications(PreTemplateApplications, PreTemplateApplicationGroups) :-
	sort(PreTemplateApplications, SortedPreTemplateApplications),
	sort_pre_template_applications1(SortedPreTemplateApplications, '*null*', [], PreTemplateApplicationGroups).

sort_pre_template_applications1([], _CurrentNameAndArgs, CurrentGroup, Groups) :-
	(   CurrentGroup = [] ->
	    Groups = []
	;
	    otherwise ->
	    Groups = [CurrentGroup]
	),
	!.
sort_pre_template_applications1([F | R], CurrentNameAndArgs, CurrentGroup, Groups) :-
	F = pre_template_application_unit(TemplateName, Args, _OldArg, _NewArg, _CorrectP),
	(   [TemplateName, Args] = CurrentNameAndArgs ->
	    append(CurrentGroup, [F], NextGroup),
	    NextNameAndArgs = CurrentNameAndArgs,
	    Groups = NextGroups
	;
	    otherwise ->
	    NextGroup = [F],
	    NextNameAndArgs = [TemplateName, Args],
	    (   CurrentGroup = [] ->
		Groups = NextGroups
	    ;
		otherwise ->
		Groups = [CurrentGroup | NextGroups]
	    )
	),
	!,
	sort_pre_template_applications1(R, NextNameAndArgs, NextGroup, NextGroups).

combine_pre_template_applications1([], []).
combine_pre_template_applications1([F | R], [F1 | R1]) :-
	combine_pre_template_applications2(F, F1),
	!,
	combine_pre_template_applications1(R, R1).

% pre_template_application_unit(TemplateName, Args, OldArg, NewArg, CorrectP)

% Just the original template application, no updates.
combine_pre_template_applications2(PreTemplateApplicationGroup, TemplateApplication) :-
	PreTemplateApplicationGroup = [pre_template_application_unit(TemplateName, Args, null, null, correct)],
	TemplateApplication = template_application_unit(TemplateName, Args, ''),
	!.
% This assumes we only have one template argument which appears in Responses.
%combine_pre_template_applications2(PreTemplateApplicationGroup, TemplateApplication) :-
%	new_args_in_pre_template_application_group(PreTemplateApplicationGroup, AllNewArgs),
%	member(pre_template_application_unit(TemplateName, Args, OldArg, _NewArg, _CorrectP), PreTemplateApplicationGroup),
%	OldArg \== null,
%	form_disjunction([OldArg | AllNewArgs], DisjunctiveArg),
%	substitute_in_term(Args, OldArg, DisjunctiveArg, Args1),
%	length(AllNewArgs, NNew),
%	format_to_atom('ADDED ~d', [NNew], Comment),
%	TemplateApplication = template_application_unit(TemplateName, Args1, Comment),
%	!.	       
combine_pre_template_applications2(PreTemplateApplicationGroup, TemplateApplication) :-
	new_args_in_pre_template_application_group(PreTemplateApplicationGroup, AllNewArgs),
	member(pre_template_application_unit(TemplateName, Args, ArgPos, _NewArg, _CorrectP), PreTemplateApplicationGroup),
	ArgPos \== null,
	safe_nth(ArgPos, Args, OldArg),
	form_disjunction([OldArg | AllNewArgs], DisjunctiveArg),
	%substitute_in_term(Args, OldArg, DisjunctiveArg, Args1),
	replace_nth_element(ArgPos, Args, DisjunctiveArg, Args1),
	length(AllNewArgs, NNew),
	format_to_atom('ADDED ~d', [NNew], Comment),
	TemplateApplication = template_application_unit(TemplateName, Args1, Comment),
	!.	       

new_args_in_pre_template_application_group(PreTemplateApplicationGroup, AllNewArgs) :-
	findall(NewArg1,
		(   member(PreTemplateApplication, PreTemplateApplicationGroup),
		    PreTemplateApplication = pre_template_application_unit(_TemplateName, _Args, _OldArg, NewArg, CorrectP),
		    NewArg \== null,
		    mark_arg_as_correct_or_incorrect(NewArg, CorrectP, NewArg1)
		),
		AllNewArgs),
	!.

%----------------------------------------

%insert_template_applications_after_templates(NewTemplates, TemplateApplications, NewTemplatesAndTemplateApplications-[])

insert_template_applications_after_templates([], _TemplateApplications, Out-Out).
insert_template_applications_after_templates([F | R], TemplateApplications, In-Out) :-
	In = [F | Next1],
	F = template_unit(_Lines, List),
	member(template_name=TemplateName, List),
	insert_template_applications_for_template(TemplateApplications, TemplateName, Next1-Next2),
	!,
	insert_template_applications_after_templates(R, TemplateApplications, Next2-Out).

insert_template_applications_for_template([], _TemplateName, In-In).
insert_template_applications_for_template([F | R], TemplateName, In-Out) :-
	F = template_application_unit(TemplateName1, _Args, _Comment),
	(   TemplateName = TemplateName1 ->
	    In = [F | Next]
	;
	    otherwise ->
	    In = Next
	),
	!,
	insert_template_applications_for_template(R, TemplateName, Next-Out).

%----------------------------------------

% list_match(List1, List2, Substitutions)

list_match([], [], Subs-Subs).
list_match([F | R], [F | R1], SubsIn-SubsOut) :-
	list_match(R, R1, SubsIn-SubsOut).
list_match(List, [Var | R], SubsIn-SubsOut) :-
	is_list_var(Var),
	list_match([Var | R], List, SubsIn-SubsOut).
list_match([Var | R], List, SubsIn-SubsOut) :-
	is_list_var(Var),
	append(Front, Back, List),
	SubsIn = [Var=Front | SubsNext],
	list_match(R, Back, SubsNext-SubsOut).

list_var(Id, '$VAR'(Id)).

is_list_var(Var) :-
	list_var(_Id, Var).

%----------------------------------------

form_disjunction(ArgList, DisjunctiveArg) :-
	form_disjunction1(ArgList, DisjunctiveArg1),
	format_to_atom('( ~w )', [DisjunctiveArg1], DisjunctiveArg).

form_disjunction1([Atom], Atom) :-
	!.
form_disjunction1([F | R], Atom) :-
	form_disjunction1(R, RAtom),
	format_to_atom('~w | ~w', [F, RAtom], Atom),
	!.

mark_arg_as_correct_or_incorrect(Arg, CorrectP, ArgOut) :-
	(   atomic(Arg) ->
	    Arg1 = Arg
	;
	    is_list(Arg) ->
	    join_with_spaces(Arg, Arg1)
	),
	(   CorrectP = incorrect ->
	    format_to_atom('* ~w', [Arg1], ArgOut)
	;
	    otherwise ->
	    ArgOut = Arg1
	).

mark_responses_as_new([], []).
mark_responses_as_new([F | R], [F1 | R1]) :-
	mark_response_as_new(F, F1),
	!,
	mark_responses_as_new(R, R1).

mark_response_as_new(Response, Response1) :-
	atom_codes(Response, Str),
	(   member(0'*, Str) ->
	    format_to_atom('~w~80|# ADDED/INCORRECT', [Response], Response1)
	;
	    otherwise ->
	    format_to_atom('~w~80|# ADDED/CORRECT', [Response], Response1)
	),
	!.

mark_response_as_merged(Response, Response1) :-
	format_to_atom('~w~80|# ADDED/CORRECT (MERGED)', [Response], Response1),
	!.

mark_response_as_correct(Response, Response1) :-
	format_to_atom('~w~80|# ADDED/CORRECT', [Response], Response1),
	!.

mark_response_as_incorrect(IncorrectResponse, IncorrectResponse1) :-
	format_to_atom('* ~w~80|# ADDED/INCORRECT', [IncorrectResponse], IncorrectResponse1),
	!.

null_response(Response) :-
	whitespace_atom(Response).

% replace_nth_element(Pos, List, NewVal, List1)

replace_nth_element(Pos, List, NewVal, List1) :-
	replace_nth_element1(Pos, List, NewVal, List1),
	!.
replace_nth_element(Pos, List, NewVal, List1) :-
	format('~N*** Error: bad call: ~w~n', [replace_nth_element(Pos, List, NewVal, List1)]),
	fail.

replace_nth_element1(1, [_OldVal | R], NewVal, [NewVal | R]) :-
	!.
replace_nth_element1(I, [F | R], NewVal, [F | R1]) :-
	I > 1,
	I1 is I - 1,
	!,
	replace_nth_element1(I1, R, NewVal, R1).

