:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(lite_write_flat_file,
	[write_flat_file/3,
	 write_flat_file/4,
	 
	 test_write_flat_file/1]
    ).

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%======================================================================

test_write_flat_file(eng_all) :-
	read_and_write_flat_file('$CALLSLT/Eng/GeneratedFiles/flat_callslt_expanded_flat.pl',
				 '$CALLSLT/Eng/GeneratedFiles/flat_callslt_reconstituted.txt').

read_and_write_flat_file(InFile0, OutFile0) :-
	safe_absolute_file_name(InFile0, InFile),
	safe_absolute_file_name(OutFile0, OutFile),
	
	prolog_file_to_list_vars_as_consts(InFile, List, 'UTF-8'),
	format('~N--- Read file, ~w~n', [InFile]),
	write_flat_file(List, OutFile, call, 'UTF-8'),
	format('~N--- Written file, ~w~n', [OutFile]).

%======================================================================

write_flat_file(List, File, Type) :-
	write_flat_file(List, File, Type, default_encoding).

write_flat_file(List, File, Type, Encoding) :-
	length(List, NUnits),
	(   Encoding = default_encoding ->
	    open(File, write, S)
	;
	    otherwise ->
	    open(File, write, S, [encoding(Encoding)])
	),
	
	write_flat_file1(List, S, Type, 0-NResponses),
	
	close(S),
	format('~N--- Written flat file (~d prompts, ~d responses) ~w~n',
	       [NUnits, NResponses, File]).

write_flat_file1([], _S, _Type, I-I).
write_flat_file1([F | R], S, Type, In-Out) :-
	write_unit_to_flat_file(F, S, Type, In-Next),
	!,
	write_flat_file1(R, S, Type, Next-Out).

write_unit_to_flat_file(Unit, S, Type, In-Out) :-
	( var(Unit) ; var(In) ),
	format('~N*** Bad call: ~w~n', [write_unit_to_flat_file(Unit, S, Type, In-Out)]),
	fail.
write_unit_to_flat_file(course_unit(_Lines, List), S, call, In-In) :-
	!,
	write_course_unit_to_flat_file(List, S).
write_unit_to_flat_file(group_unit(_Lines, List), S, call, In-In) :-
	!,
	write_group_unit_to_flat_file(List, S).
write_unit_to_flat_file(multimedia_unit(_Lines, List), S, call, In-In) :-
	!,
	write_multimedia_unit_to_flat_file(List, S).
write_unit_to_flat_file(phrase_score_unit(_Lines, List), S, call, In-In) :-
	!,
	write_phrase_score_unit_to_flat_file(List, S).
write_unit_to_flat_file(unit(_Lines, List), S, call, In-Out) :-
	there_are_responses_in_unit(List, NResponses),
	Next is In + NResponses,
	!,
	(   member(template_application=[Name, Args], List) ->
	    write_unit_to_flat_file(template_application_unit(Name, Args), S, call, Next-Out)
	;
	    otherwise ->
	    write_prompt_to_flat_file(List, S),
	    Out = Next
	).
write_unit_to_flat_file(unit(_Lines, List), S, call, In-Out) :-
	there_are_responses_in_unit(List, NResponses),
	Out is In + NResponses,
	!,
	write_prompt_to_flat_file(List, S).
write_unit_to_flat_file(template_unit(_Lines, List), S, call, In-Out) :-
	there_are_responses_in_unit(List, NResponses),
	Out is In + NResponses,
	!,
	write_prompt_template_to_flat_file(List, S).
write_unit_to_flat_file(template_application_unit(Name, Args), S, call, In-In) :-
	!,
	write_template_application_to_flat_file(Name, Args, '', S).
write_unit_to_flat_file(template_application_unit(Name, Args, Comment), S, call, In-In) :-
	!,
	write_template_application_to_flat_file(Name, Args, Comment, S).
write_unit_to_flat_file(_Prompt, _S, _Type, In-In).

there_are_responses_in_unit(List, NResponses) :-
	member(unexpanded_responses=Responses, List),
	length(Responses, NResponses),
	NResponses > 0,
	!.
there_are_responses_in_unit(List, NResponses) :-
	member(responses=Responses, List),
	length(Responses, NResponses),
	NResponses > 0,
	!.

/*
course_unit((1-5), 
            [(namespace=restaurant),
	     (name=restaurant),
	     (client=translation_game_client), 
             (feedback=default),
	     (l2=french), 
             (languages=[english,chinese,arabic,german,italian]),
	     (users=[any]), 
             (acceptbonus=100),
	     (startscore=100),
	     (maxtries=2),
	     (rejectpenalty=2), 
             (skippenalty=5),
	     (plainbadge=0),
	     (bronzebadge=0),
	     (silverbadge=90), 
             (goldbadge=100)]).
*/

write_course_unit_to_flat_file(List, S) :-
	format(S, '~N~nCourse~n', []),
	%write_namespace_to_flat_file(List, S),
	%write_domain_to_flat_file(List, S),
	write_name_to_flat_file(List, S),
	write_client_to_flat_file(List, S),
	write_feedback_to_flat_file(List, S),
	write_l2_to_flat_file(List, S),
	write_languages_to_flat_file(List, S),
	write_users_to_flat_file(List, S),
	write_acceptbonus_to_flat_file(List, S),
	write_startscore_to_flat_file(List, S),
	write_maxtries_to_flat_file(List, S),
	write_rejectpenalty_to_flat_file(List, S),
	write_skippenalty_to_flat_file(List, S),
	write_plainbadge_to_flat_file(List, S),
	write_bronzebadge_to_flat_file(List, S),
	write_silverbadge_to_flat_file(List, S),
	write_goldbadge_to_flat_file(List, S),
	format(S, '~NEndCourse~n', []),
	!.
write_course_unit_to_flat_file(List, S) :-
	format('~N*** Error: bad call: ~w~n',
	       [write_course_unit_to_flat_file(List, S)]),
	fail.

/*

group_unit((7-14), 
           [(domain=restaurant),
	    (namespace=restaurant), 
            (name=un_situation_commander_au_restaurant_conditionnel_noms_singuliers), 
            (printname='Ordering (conditional tense)'),
	    (description='(no description)'), 
            (helpfile(arabic)='singular_nouns_help_arabic.html'), 
            (helpfile(chinese)='singular_nouns_help_chinese.html'), 
            (helpfile(english)='singular_nouns_help_english.html'), 
            (helpfile(german)='singular_nouns_help.html')]).

*/

write_group_unit_to_flat_file(List, S) :-
	format(S, '~N~nLesson~n', []),
	%write_namespace_to_flat_file(List, S),
	%write_domain_to_flat_file(List, S),
	write_name_to_flat_file(List, S),
	write_printname_to_flat_file(List, S),
	write_description_to_flat_file(List, S),
	write_script_to_flat_file(List, S),
	write_badges_to_flat_file(List, S),
	write_helpfiles_to_flat_file(List, S),
	format(S, '~NEndLesson~n', []),
	!.
write_group_unit_to_flat_file(List, S) :-
	format('~N*** Error: bad call: ~w~n',
	       [write_group_unit_to_flat_file(List, S)]),
	fail.

/*
multimedia_unit(9-13,
		[namespace=user,
		 id=alt_musical,
		 files=['tourist_catherine_sub2.flv',
			'tourist_catherine_pardon.flv']
		 ]).

Multimedia
Id alt_musical
File tourist_catherine_sub2.flv
File tourist_catherine_pardon.flv
EndMultimedia
*/

write_multimedia_unit_to_flat_file(List, S) :-
	format(S, '~N~nMultimedia~n', []),
	write_id_to_flat_file(List, S),
	write_files_to_flat_file(List, S),
	format(S, '~NEndMultimedia~n', []).

/*
phrase_score_unit(1-4,
		  [namespace=user,
		   phrase='have you got',
		   score=2]).

PhraseScore
Phrase have you got
Score 2
EndPhraseScore
*/

write_phrase_score_unit_to_flat_file(List, S) :-
	format(S, '~N~nPhraseScore~n', []),
	write_phrase_to_flat_file(List, S),
	write_score_to_flat_file(List, S),
	format(S, '~NEndPhraseScore~n', []).

/*
template_application_unit(i_want_to_leave_on_date, 
			  ['le mardi après-midi',
			   'Dienstagnachmittag', 
			   'tuesday afternoon'])

ApplyTemplate i_want_to_leave_on_date "le mardi après-midi" "Dienstagnachmittag" "tuesday afternoon"
*/

write_template_application_to_flat_file(Name, Args, Comment, S) :-
	format(S, '~N~nApplyTemplate ~w ', [Name]),
	write_template_application_args_and_comment_to_flat_file(Args, Comment, S),
	!.
write_template_application_to_flat_file(Name, Args, Comment, S) :-
	format('~N*** Error: bad call: ~w~n',
	       [write_template_application_to_flat_file(Name, Args, Comment, S)]),
	fail.

write_template_application_args_and_comment_to_flat_file([], Comment, S) :-
	(   Comment = '' ->
	    format(S, '~n', [])
	;
	    otherwise ->
	    format(S, '  #~w~n', [Comment])
	),
	!.
write_template_application_args_and_comment_to_flat_file([F | R], Comment, S) :-
	write_template_application_arg_to_flat_file(F, S),
	!,
	write_template_application_args_and_comment_to_flat_file(R, Comment, S).

write_template_application_arg_to_flat_file(Arg, S) :-
	format(S, '"~w" ', [Arg]),
	!.


write_prompt_template_to_flat_file(List, S) :-
	write_prompt_template_header_to_flat_file(List, S),
	join_up_template_args(List, List1),
	write_prompt_to_flat_file1(List1, S),
	format(S, '~NEndPromptTemplate~n', []),
	!.
write_prompt_template_to_flat_file(List, S) :-
	format('~N*** Error: bad call: ~w~n',
	       [write_prompt_template_to_flat_file(List, S)]),
	fail.

join_up_template_args([], []).
join_up_template_args([F | R], [F1 | R1]) :-
	join_up_template_arg(F, F1),
	!,
	join_up_template_args(R, R1).

join_up_template_arg(Key=List, Key=Atom) :-
	member(Key, [lesson, group, text(_)]),
	append_atoms_if_possible(List, Atom),
	!.
join_up_template_arg(Key=List, Key=List1) :-
	member(Key, [responses, unexpanded_responses, audiohelp]),
	append_atoms_if_possible_in_list(List, List1),
	!.
join_up_template_arg(tags=Tags, tags=Tags1) :-
	join_up_template_arg_in_tags(Tags, Tags1),
	!.
join_up_template_arg(Other, Other).

join_up_template_arg_in_tags([], []).
join_up_template_arg_in_tags([F | R], [F1 | R1]) :-
	join_up_template_arg_in_tag(F, F1),
	!,
	join_up_template_arg_in_tags(R, R1).

join_up_template_arg_in_tag(Key=List, Key=Atom) :-
	append_atoms_if_possible(List, Atom),
	!.

append_atoms_if_possible(List, Atom) :-
	is_list_of_atoms(List),
	!,
	append_atoms(List, Atom).
append_atoms_if_possible(Other, Other).

append_atoms_if_possible_in_list([], []).
append_atoms_if_possible_in_list([F | R], [F1 | R1]) :-
	append_atoms_if_possible(F, F1),
	append_atoms_if_possible_in_list(R, R1).

/*
unit((175-194), 
     [(domain=restaurant), (namespace=restaurant), 
      (lesson=un_situation_commander_au_restaurant_conditionnel_noms_singuliers),
      (group=1), 
      (text(english)='ASK-FOR POLITELY 1 TEA'),
      (text(german)='HÖFLICH BITTEN_UM 1 TEE'), 
      (text(italian)='CHIEDI IN_MODO_EDUCATO 1 TE'), 
      (unexpanded_responses = 
       ['auriez vous un thé', 'est-ce que vous auriez un thé', 'j aimerais un thé', 
        'je prendrai un thé', 'je voudrais un thé', 'je voudrais un thé s\'il vous plaît', 
        'vous auriez un thé']),
      (responses = 
       ['auriez vous un thé', 'est-ce que vous auriez un thé', 'j aimerais un thé', 
        'je prendrai un thé', 'je voudrais un thé', 'je voudrais un thé s\'il vous plaît', 
        'vous auriez un thé']),
      (incorrect_responses=[]), 
      (audiohelp=['helpfiles/johanna-2012-03-02_14-02-11-utt_093.wav','j aimerais un thé']), 
      (audiohelp=['helpfiles/johanna-2012-03-02_14-02-11-utt_002.wav','j aimerais un thé']), 
      (audiohelp=['helpfiles/johanna-2010-11-05_12-58-55-utt09.wav','je voudrais un thé']), 
      (lf=auriez_vous_un_thé), (incorrect_lf=incorrect_version_of_auriez_vous_un_thé)]).
*/

write_prompt_to_flat_file1(List, S) :-
	%write_namespace_to_flat_file(List, S),
	%write_domain_to_flat_file(List, S),
	write_lesson_to_flat_file(List, S),
	write_group_to_flat_file(List, S),
	write_texts_to_flat_file(List, S),
	write_tags_to_flat_file(List, S),
	write_responses_to_flat_file(List, S),
	write_audio_helps_to_flat_file(List, S),
	!.

write_prompt_template_header_to_flat_file(List, S) :-
	member(template_name=Name, List),
	member(template_args=Args, List),
	format(S, '~N~nPromptTemplate ~w ', [Name]),
	write_template_args_to_flat_file(Args, S),
	format(S, '~n', []),
	!.

write_template_args_to_flat_file([], _S).
write_template_args_to_flat_file([F | R], S) :-
	format(S, '~w ', [F]),
	!,
	write_template_args_to_flat_file(R, S).

write_client_to_flat_file(List, S) :-
	member(client=X, List),
	format(S, '~NClient~20|~w~n', [X]),
	!.

write_feedback_to_flat_file(List, S) :-
	member(feedback=X, List),
	format(S, '~NFeedback~20|~w~n', [X]),
	!.

write_l2_to_flat_file(List, S) :-
	member(l2=X, List),
	format(S, '~NL2~20|~w~n', [X]),
	!.

write_languages_to_flat_file(List, S) :-
	member(languages=Langs, List),
	join_with_spaces(Langs, X),
	format(S, '~NLanguages~20|~w~n', [X]),
	!.

write_users_to_flat_file(List, S) :-
	member(users=Users, List),
	join_with_spaces(Users, X),
	format(S, '~NUsers~20|~w~n', [X]),
	!.

write_acceptbonus_to_flat_file(List, S) :-
	member(acceptbonus=X, List),
	format(S, '~NAcceptBonus~20|~w~n', [X]),
	!.

write_startscore_to_flat_file(List, S) :-
	member(startscore=X, List),
	format(S, '~NStartscore~20|~w~n', [X]),
	!.

write_maxtries_to_flat_file(List, S) :-
	member(maxtries=X, List),
	format(S, '~NMaxtries~20|~w~n', [X]),
	!.

write_rejectpenalty_to_flat_file(List, S) :-
	member(rejectpenalty=X, List),
	format(S, '~NRejectPenalty~20|~w~n', [X]),
	!.

write_skippenalty_to_flat_file(List, S) :-
	member(skippenalty=X, List),
	format(S, '~NSkipPenalty~20|~w~n', [X]),
	!.

write_plainbadge_to_flat_file(List, S) :-
	member(plainbadge=X, List),
	format(S, '~NPlainBadge~20|~w~n', [X]),
	!.

write_bronzebadge_to_flat_file(List, S) :-
	member(bronzebadge=X, List),
	format(S, '~NBronzeBadge~20|~w~n', [X]),
	!.

write_silverbadge_to_flat_file(List, S) :-
	member(silverbadge=X, List),
	format(S, '~NSilverBadge~20|~w~n', [X]),
	!.

write_goldbadge_to_flat_file(List, S) :-
	member(goldbadge=X, List),
	format(S, '~NGoldBadge~20|~w~n', [X]),
	!.

write_badges_to_flat_file(List, S) :-
	member(badges=Badges, List),
	join_with_spaces(Badges, X),
	format(S, '~NBadges~20|~w~n', [X]),
	!.
write_badges_to_flat_file(_List, _S).

write_audio_helps_to_flat_file(List, S) :-
	findall([File, Transcription],
		member(audiohelp=[File, Transcription], List),
		Helpfiles),
	write_audio_helps_to_flat_file1(Helpfiles, S),
	!.

write_audio_helps_to_flat_file1([], _S).
write_audio_helps_to_flat_file1([F | R], S) :-
	write_audio_help_to_flat_file(F, S),
	write_audio_helps_to_flat_file1(R, S).

write_audio_help_to_flat_file([File, Transcription], S) :-
	format(S, '~NAudioHelp~20|~w ~w~n', [File, Transcription]),
	!.

write_domain_to_flat_file(List, S) :-
	member(domain=Domain, List),
	format(S, '~NDomain~20|~w~n', [Domain]),
	!.

write_lesson_to_flat_file(List, S) :-
	member(lesson=Lesson, List),
	format(S, '~NLesson~20|~w~n', [Lesson]),
	!.
write_lesson_to_flat_file(_List, _S).

write_id_to_flat_file(List, S) :-
	member(id=Id, List),
	format(S, '~NId~20|~w~n', [Id]),
	!.
write_id_to_flat_file(List, S) :-
	format('~N*** Error: bad call: ~w~n',
	       [write_id_to_flat_file(List, S)]),
	fail.

write_phrase_to_flat_file(List, S) :-
	member(phrase=Phrase, List),
	format(S, '~NPhrase~20|~w~n', [Phrase]),
	!.
write_phrase_to_flat_file(List, S) :-
	format('~N*** Error: bad call: ~w~n',
	       [write_phrase_to_flat_file(List, S)]),
	fail.

write_score_to_flat_file(List, S) :-
	member(score=Score, List),
	format(S, '~NScore~20|~w~n', [Score]),
	!.
write_score_to_flat_file(List, S) :-
	format('~N*** Error: bad call: ~w~n',
	       [write_score_to_flat_file(List, S)]),
	fail.

write_group_to_flat_file(List, S) :-
	member(group=Group, List),
	group \== null,
	format(S, '~NGroup~20|~w~n', [Group]),
	!.
write_group_to_flat_file(_List, _S).

write_helpfiles_to_flat_file(List, S) :-
	findall([L1, File],
		helpfile_in_list(List, L1, File),
		Pairs),
	write_helpfiles_to_flat_file1(Pairs, S).

helpfile_in_list(List, L1, File) :-
	member(helpfile=File, List),
	L1 = ''.
helpfile_in_list(List, L1, File) :-
	member(helpfile(L1)=File, List).

write_helpfiles_to_flat_file1([], _S).
write_helpfiles_to_flat_file1([F | R], S) :-
	write_helpfile_to_flat_file(F, S),
	!,
	write_helpfiles_to_flat_file1(R, S).

write_helpfile_to_flat_file(['', File], S) :-
	format(S, '~NHelpFile~20|~w~n', [File]),
	!.
write_helpfile_to_flat_file([L1, File], S) :-
	format(S, '~NText/~w~20|~w~n', [L1, File]),
	!.

write_texts_to_flat_file(List, S) :-
	findall([L1, Text],
		member(text(L1)=Text, List),
		Pairs),
	write_texts_to_flat_file1(Pairs, S).

write_texts_to_flat_file1([], _S).
write_texts_to_flat_file1([F | R], S) :-
	write_text_to_flat_file(F, S),
	!,
	write_texts_to_flat_file1(R, S).

write_text_to_flat_file([L1, Text], S) :-
	format(S, '~NText/~w~20|~w~n', [L1, Text]),
	!.

write_tags_to_flat_file(List, S) :-
	member(tags=Tags, List),
	Tags \== [],
	!,
	format(S, '~NTags~20|', []),
	write_tags_to_flat_file1(Tags, S),
	!.
write_tags_to_flat_file(_List, _S).

write_tags_to_flat_file1([], _S).
write_tags_to_flat_file1([F | R], S) :-
	format(S, '~w ', [F]),
	!,
	write_tags_to_flat_file1(R, S).

write_responses_to_flat_file(List, S) :-
	(   member(unexpanded_responses=Responses, List) ->
	    true
	;
	    otherwise ->
	    member(responses=Responses, List)
	),
	write_responses_to_flat_file1(Responses, S).

write_responses_to_flat_file1([], _S).
write_responses_to_flat_file1([F | R], S) :-
	format(S, '~NResponse~20|~w~n', [F]),
	!,
	write_responses_to_flat_file1(R, S).

write_files_to_flat_file(List, S) :-
	member(files=Files, List),
	write_files_to_flat_file1(Files, S),
	!.
write_files_to_flat_file(List, S) :-
	format('~N*** Error: bad call: ~w~n',
	       [write_files_to_flat_file(List, S)]),
	fail.
	
write_files_to_flat_file1([], _S).
write_files_to_flat_file1([F | R], S) :-
	format(S, '~NFile~20|~w~n', [F]),
	!,
	write_files_to_flat_file1(R, S).

write_namespace_to_flat_file(List, S) :-
	member(namespace=Name, List),
	format(S, '~NNamespace~20|~w~n', [Name]),
	!.

write_name_to_flat_file(List, S) :-
	member(name=Name, List),
	format(S, '~NName~20|~w~n', [Name]),
	!.
	
write_printname_to_flat_file(List, S) :-
	member(printname=Name, List),
	format(S, '~NPrintName~20|~w~n', [Name]),
	!.
	
write_description_to_flat_file(List, S) :-
	member(description=Name, List),
	format(S, '~NDescription~20|~w~n', [Name]),
	!.
	
write_script_to_flat_file(List, S) :-
	member(script=Name, List),
	format(S, '~NScript~20|~w~n', [Name]),
	!.
write_script_to_flat_file(_List, _S).

write_prompt_to_flat_file(List, S) :-
	format(S, '~N~nPrompt~n', []),
	write_prompt_to_flat_file1(List, S),
	format(S, '~NEndPrompt~n', []),
	!.
write_prompt_to_flat_file(List, S) :-
	format('~N*** Error: bad call: ~w~n',
	       [write_prompt_to_flat_file(List, S)]),
	fail.
