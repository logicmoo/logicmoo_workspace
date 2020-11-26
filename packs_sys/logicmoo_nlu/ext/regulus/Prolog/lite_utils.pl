:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(lite_utils,
	[assemble_lite_file_spec/1,
	 assemble_lite_file_spec_for_namespace/2,
	 assemble_l1_grammar_alist_spec/1,
	 assemble_lf_translation_answer_file_alist/1,
	 assemble_l2_file_alist/1,

	 namespaces_with_all_files_present/1,

	 parse_tags_atom/3,
	 parse_answers_atom/3,
	 parse_response_atom/3,
	 parse_translation_atom/3,
	 parse_template_application_atom/3,

	 rmerge_sentence_and_pattern/5,
	 rmerge/4,

	 nuance_main_grammar_name_for_id/2,
	 nuance_top_grammar_name_for_id/2,
	 nuance_grammar_name_for_phrase_id/2,
	 nuance_grammar_name_for_tr_phrase_id/2,

	 phrase_id_atom/2,
	 tr_phrase_id_atom/2,
	 is_atom_list/1, 
	 is_key_val_list/1,
	 is_abstract_response/1,
	 is_abstract_response_list/1,

	 init_stored_lite_errors/0,
	 format_lite_error/2,
	 lite_errors_found/0	 
	]
    ).

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%======================================================================

assemble_lite_file_spec(FileSpec) :-
	collect_all_namespaces(Namespaces),
	assemble_lite_file_spec_for_namespaces(Namespaces, FileSpec).

collect_all_namespaces(Namespaces) :-
	findall(Namespace,
		(   user:regulus_config(lite_file(_Type, Namespace, _Languages), _File)
		;
		    user:regulus_config(lite_file(_Type, Namespace, _Encoding, _Languages), _File)
		),
		Namespaces0
	       ),
	sort(Namespaces0, Namespaces).

assemble_lite_file_spec_for_namespaces([], []).
assemble_lite_file_spec_for_namespaces([Namespace | R], [FileSpec | R1]) :-
	assemble_lite_file_spec_for_namespace(Namespace, FileSpec),
	!,
	assemble_lite_file_spec_for_namespaces(R, R1).

assemble_lite_file_spec_for_namespace(Namespace, FileSpec) :-
	main_lite_file_spec_for_namespace(Namespace, MainSpec),
	auxiliary_lite_file_spec_for_namespace(Namespace, AuxSpec),
	FileSpec = [namespace=Namespace, main_spec=MainSpec, aux_spec=AuxSpec].

main_lite_file_spec_for_namespace(Namespace, MainSpec) :-
	findall([File, Languages],
		(   user:regulus_config(lite_file(_Type, Namespace, Languages), File)
		;
		    user:regulus_config(lite_file(_Type, Namespace, _Encoding, Languages), File)
		),
		MainSpec),
	!.

auxiliary_lite_file_spec_for_namespace(Namespace, AuxSpec) :-
	findall([File, Languages],
		(   user:regulus_config(auxiliary_lite_file(_Type, Namespace, Languages), File)
		;
		    user:regulus_config(auxiliary_lite_file(_Type, Namespace, _Encoding, Languages), File)
		),
		AuxSpec).
 
%assemble_lite_file_spec_no_namespaces(FileSpec) :-
%	main_lite_file_spec_no_namespaces(MainSpec),
%	auxiliary_lite_file_spec_no_namespaces(AuxSpec),
%	FileSpec = [MainSpec | AuxSpec].
%
%main_lite_file_spec_no_namespaces(MainSpec) :-
%	user:regulus_config(lite_file(_Type), File),
%	MainSpec = [File, default_encoding, [default]],
%	!.
%main_lite_file_spec_no_namespaces(MainSpec) :-
%	user:regulus_config(lite_file(_Type, Encoding, Languages), File),
%	MainSpec = [File, Encoding, Languages],
%	!.
%
%auxiliary_lite_file_spec_no_namespaces(AuxSpec) :-
%	findall([File, Encoding, Languages],
%		user:regulus_config(auxiliary_lite_file(_Type, Encoding, Languages), File),
%		AuxSpec).

assemble_l1_grammar_alist_spec(L1GrammarFileAlist) :-
	findall(L1-File,
		user:regulus_config(lite_generation_file(L1), File),
		L1GrammarFileAlist).

assemble_lf_translation_answer_file_alist(L1GrammarFileAlist) :-
	findall(L1-File,
		user:regulus_config(lite_questionnaire_answer_file(L1), File),
		L1GrammarFileAlist).

assemble_l2_file_alist(L2FileAlist) :-
	findall(L2-File,
		user:regulus_config(lite_questionnaire_l2_file(L2), File),
		L2FileAlist).
	
%======================================================================

namespaces_with_all_files_present(Namespaces) :-
	assemble_lite_file_spec(FileSpec),
	namespaces_in_file_spec_with_all_files_present(FileSpec, Namespaces).

namespaces_in_file_spec_with_all_files_present([], []).
namespaces_in_file_spec_with_all_files_present([F | R], [Namespace | Namespaces]) :-
	namespace_component_with_all_files_present(F, Namespace),
	!,
	namespaces_in_file_spec_with_all_files_present(R, Namespaces).
namespaces_in_file_spec_with_all_files_present([_F | R], Namespaces) :-
	!,
	namespaces_in_file_spec_with_all_files_present(R, Namespaces).

namespace_component_with_all_files_present(Component, Namespace) :-
	member(namespace=Namespace, Component),
	member(main_spec=MainSpec, Component),
	member(aux_spec=AuxSpec, Component),
	append(MainSpec, AuxSpec, AllSpecs),
	findall(File, member([File, _Languages], AllSpecs), Files),
	all_files_in_list_exist(Files),
	!.

all_files_in_list_exist([]).
all_files_in_list_exist([F | R]) :-
	safe_file_exists(F),
	!,
	all_files_in_list_exist(R).

%======================================================================

parse_tags_atom(TagsAtom, InTemplateP, Tags) :-
	atom_codes(TagsAtom, Str),
	tags(Tags, InTemplateP, Str, []),
	!.
parse_tags_atom(TagsAtom, _InTemplateP, _Tags) :-
	format('~N*** Error: unable to parse tags: ~w~n', [TagsAtom]),
	fail.

parse_answers_atom(AnswersAtom, InTemplateP, AnswersAlist) :-
	atom_codes(AnswersAtom, Str),
	answers(AnswersAlist, InTemplateP, Str, []),
	!.
parse_answers_atom(AnswersAtom, _InTemplateP, _AnswersAlist) :-
	format('~N*** Error: unable to parse answers: ~w~n', [AnswersAtom]),
	fail.

parse_response_atom(ResponseAtom, InTemplateP, AbstractResponse) :-
	atom_codes(ResponseAtom, Str),
	response(AbstractResponse, InTemplateP, Str, []),
	!.
parse_response_atom(ResponseAtom, InTemplateP, _AbstractResponse) :-
	diagnose_bad_response_atom(ResponseAtom, InTemplateP, Diagnosis),
	format('~N*** Error: unable to parse response: ~w ~w~n', [ResponseAtom, Diagnosis]),
	fail.

parse_translation_atom(Atom, InTemplateP, Abstract) :-
	atom_codes(Atom, Str),
	translation(Abstract, InTemplateP, Str, []),
	!.

parse_template_application_atom(TemplateApplicationAtom, Name, Args) :-
	atom_codes(TemplateApplicationAtom, Str),
	template_application(Name, Args, Str, []),
	!.
parse_template_application_atom(TemplateApplicationAtom, _Name, _Args) :-
	format('~N*** Error: unable to parse template application: ~w~n', [TemplateApplicationAtom]),
	fail.

%----------------------------------------------------

diagnose_bad_response_atom(ResponseAtom, InTemplateP, Diagnosis) :-
	atom_codes(ResponseAtom, Str),
	(   ( str_contains_uppercase(Str), InTemplateP \== in_template ) ->
	    Diagnosis = '[CONTAINS CAPITAL LETTERS]'
	;
	    str_contains_punctuation_marks(Str) ->
	    Diagnosis = '[CONTAINS PUNCTUATION MARKS]'
	;
	    str_ends_in_question_mark(Str) ->
	    Diagnosis = '[ENDS IN QUESTION-MARK]'
	;
	    otherwise ->
	    Diagnosis = ''
	).

str_contains_uppercase([F | _R]) :-
	uppercase_char(F),
	!.
str_contains_uppercase([_F | R]) :-
	!,
	str_contains_uppercase(R).

str_contains_punctuation_marks([F | _R]) :-
	member(F, ",!"),
	!.
str_contains_punctuation_marks([_F | R]) :-
	!,
	str_contains_punctuation_marks(R).

str_ends_in_question_mark([0'? | R]) :-
	all_whitespace_str(R).
str_ends_in_question_mark([_F | R]) :-
	str_ends_in_question_mark(R).

all_whitespace_str([]).
all_whitespace_str([F | R]) :-
	whitespace_char(F),
	!,
	all_whitespace_str(R).

%======================================================================

tags([Tag | Tags], InTemplateP) -->
	whitespaces,
	tag(Tag, InTemplateP),
	whitespaces,
	!,
	tags(Tags, InTemplateP),
	whitespaces.
tags([], _InTemplateP) -->
	whitespaces.

tag(Key = Value, not_in_template) -->
	prolog_atom(Key),
	whitespaces,
	[0'=],
	whitespaces,
	prolog_atom_or_number(Value).

tag(Key = Value, in_template) -->
	prolog_atom(Key),
	whitespaces,
	[0'=],
	whitespaces,
	possibly_uppercase_prolog_atom_or_number(Value).

%----------------------------------------------------

answers([Answer | Answers], InTemplateP) -->
	whitespaces,
	answer(Answer, InTemplateP),
	whitespaces,
	!,
	answers(Answers, InTemplateP),
	whitespaces.
answers([], _InTemplateP) -->
	whitespaces.

answer(Key = Value, not_in_template) -->
	prolog_atom(Key),
	whitespaces,
	[0'=],
	whitespaces,
	prolog_atom_or_number(Value).
answer(Key = Value, in_template) -->
	prolog_atom(Key),
	whitespaces,
	[0'=],
	whitespaces,
	possibly_uppercase_prolog_atom_or_number(Value).
answer(Key = Key, _InTemplateP) -->
	prolog_atom(Key).

%----------------------------------------------------

template_application(Name, Args) -->
	prolog_atom(Name),
	whitespaces,
	quoted_string_list(Args),
	whitespaces.

quoted_string_list([F | R]) -->
	quoted_string(F),
	whitespaces,
	!,
	quoted_string_list(R).
quoted_string_list([]) -->
	[].

%----------------------------------------------------

translation([Unit | Units], Context) -->
	whitespaces,
	translation_unit(Unit, Context),
	!,
	whitespaces,
	translation(Units, Context),
	whitespaces.
translation([], _Context) --> [].

translation_unit(tr_phrase(Atom, Index), _Context) -->
	[0'$, 0'$],
	phrase_id_atom(Atom),
	[0':],
	number(Index),
	!.
translation_unit(tr_phrase(Atom, 1), _Context) -->
	[0'$, 0'$],
	phrase_id_atom(Atom),
	!.

translation_unit(Atom, _Context) -->
	nonspace_str(Str),
	{Str \== []},
	{atom_codes(Atom, Str)}.

nonspace_str([Char | R]) -->
	[Char],
	{\+ whitespace_char(Char)},
	!,
	nonspace_str(R).
nonspace_str([]) --> [].

%----------------------------------------------------

response([Unit | Units], Context) -->
	whitespaces,
	response_unit(Unit, Context),
	!,
	whitespaces,
	response(Units, Context),
	whitespaces.
response([], _Context) --> [].

response_unit(Body, Context) -->
	[0'(],
	!,
	conjunctive_or_disjunctive_response(Body, _ConjunctiveOrDisjunctive, Context),
	[0')].
 
response_unit('*incorrect*', _Context) -->
	[0'*],
	!.

response_unit(optional(Unit), Context) -->
	[0'?],
	!,
	response_unit(Unit, Context).

response_unit(phrase(Atom), _Context) -->
	[0'$],
	phrase_id_atom(Atom).

response_unit(tr_phrase(Atom, Index), _Context) -->
	[0'$, 0'$],
	phrase_id_atom(Atom),
	[0':],
	number(Index),
	!.
response_unit(tr_phrase(Atom, 1), _Context) -->
	[0'$, 0'$],
	phrase_id_atom(Atom),
	!.

response_unit(Atom, Context) -->
	{ Context \== in_template },
	word_atom(Atom).
response_unit(Atom, in_template) -->
	word_atom_including_uppercase(Atom).

conjunctive_or_disjunctive_response(Body, ConjunctiveOrDisjunctive, Context) -->
	whitespaces,
	response(F, Context),
	whitespaces,
	conjunctive_or_disjunctive_response_rest(F, ConjunctiveOrDisjunctive, Body, Context).

conjunctive_or_disjunctive_response_rest(Unit1, disjunctive, Body, Context) -->
	[0'|],
	!,
	whitespaces,
	response(Unit2, Context),
	whitespaces,
	conjunctive_or_disjunctive_response_rest(or(Unit1, Unit2), disjunctive, Body, Context),
	whitespaces.

conjunctive_or_disjunctive_response_rest(Unit1, _ConjunctiveOrDisjunctive, Unit1, _Context) -->
	whitespaces.

%----------------------------------------------------

whitespaces -->
	[Char],
	{whitespace_char(Char)},
	!,
	whitespaces.
whitespaces --> [].

lowercase_atom(Atom) -->
	lowercase_str(Str),
	{Str \== []},
	{atom_codes(Atom, Str)}.

word_atom(Atom) -->
	word_str(Str),
	{Str \== []},
	{atom_codes(Atom, Str)}.

word_atom_including_uppercase(Atom) -->
	word_str_including_uppercase(Str),
	{Str \== []},
	{atom_codes(Atom, Str)}.

possibly_uppercase_prolog_atom_or_number(X) -->
	word_atom_including_uppercase(X).
possibly_uppercase_prolog_atom_or_number(X) -->
	number(X).

prolog_atom_or_number(X) -->
	prolog_atom(X).
prolog_atom_or_number(X) -->
	number(X).

prolog_atom(Atom) -->
	prolog_atom_str(Str),
	{Str \== []},
	{atom_codes(Atom, Str)}.

phrase_id_atom(Atom) -->
	phrase_id_str(Str),
	{Str \== []},
	{atom_codes(Atom, Str)}.

number(N) -->
	digit_str(Str),
	{Str \== []},
	{number_codes(N, Str)}.

prolog_atom_str([F | R]) -->
	[F],
	{lowercase_char(F)},
	!,
	non_initial_prolog_atom_str(R).

quoted_string(Str) -->
	[0'"], %"
	nonquote_str(Str),
	[0'"]. %"

nonquote_str([Char | R]) -->
	[Char],
	{Char \== 0'"}, %"
	!,
	nonquote_str(R).
nonquote_str([]) --> [].	    

lowercase_str([Char | R]) -->
	[Char],
	{lowercase_char(Char)},
	!,
	lowercase_str(R).
lowercase_str([]) --> [].

word_str([Char | R]) -->
	[Char],
	{word_char(Char)},
	!,
	word_str(R).
word_str([]) --> [].

word_str_including_uppercase([Char | R]) -->
	[Char],
	{word_char_including_uppercase(Char)},
	!,
	word_str_including_uppercase(R).
word_str_including_uppercase([]) --> [].

phrase_id_str([Char | R]) -->
	[Char],
	{phrase_id_char(Char)},
	!,
	phrase_id_str(R).
phrase_id_str([]) --> [].

non_initial_prolog_atom_str([Char | R]) -->
	[Char],
	{non_initial_prolog_atom_char(Char)},
	!,
	non_initial_prolog_atom_str(R).
non_initial_prolog_atom_str([]) --> [].

digit_str([Char | R]) -->
	[Char],
	{digit_char(Char)},
	!,
	digit_str(R).
digit_str([]) --> [].

word_char(Char) :-
	(   lowercase_char(Char)
	;
	    word_non_alphanumeric_char(Char)
	),
	!.

word_char_including_uppercase(Char) :-
	(   lowercase_char(Char)
	;
	    uppercase_char(Char)
	;
	    word_non_alphanumeric_char(Char)
	),
	!.

non_initial_prolog_atom_char(Char) :-
	(   lowercase_char(Char)
	;
	    uppercase_char(Char)
	;
	    digit_char(Char)
	;
	    prolog_non_alphanumeric_char(Char)
	),
	!.

phrase_id_char(Char) :-
	(   lowercase_char(Char)
	;
	    digit_char(Char)
	;
	    prolog_non_alphanumeric_char(Char)
	),
	!.

prolog_non_alphanumeric_char(0'_).
prolog_non_alphanumeric_char(0'-).
prolog_non_alphanumeric_char(0'/).

word_non_alphanumeric_char(0'\').
word_non_alphanumeric_char(0'-).
%word_non_alphanumeric_char(0'*).
word_non_alphanumeric_char(0'/).

%------------------------------------------------------------------------------------

generate_response([], '') :-
	!.
generate_response([F], Out) :-
	generate_response(F, Out),
	!.
generate_response(Atom, Atom) :-
	atomic(Atom),
	!.
generate_response([F | R], Out) :-
	generate_response(F, FOut),
	generate_response(R, ROut),
	format_to_atom('~w ~w', [FOut, ROut], Out),
	!.
generate_response(or(F, R), Out) :-
	generate_response(F, FOut),
	generate_response(R, ROut),
	format_to_atom('( ~w | ~w )', [FOut, ROut], Out),
	!.
generate_response(or(Args), Out) :-
	generate_response_for_or_body(Args, ArgsOut),
	format_to_atom('( ~w )', [ArgsOut], Out),
	!.
generate_response(optional(F), Out) :-
	is_list(F),
	generate_response(F, FOut),
	format_to_atom('?( ~w )', [FOut], Out),
	!.
generate_response(optional(F), Out) :-
	generate_response(F, FOut),
	format_to_atom('?~w', [FOut], Out).


generate_response_for_or_body([F], Out) :-
	generate_response(F, Out),
	!.
generate_response_for_or_body([F | R], Out) :-
	generate_response(F, FOut),
	generate_response_for_or_body(R, ROut),
	format_to_atom('~w | ~w', [FOut, ROut], Out),
	!.

%------------------------------------------------------------------------------------

% L1 is a list, L2 is a parsed regular expression using or/2 and optional/1.

% rmerge(L1, L2, N, Merge)

% Merge is an expression which includes both L1 and L2, N is the number of changes added to L2.

% rmerge_tmp(Pos1, Pos2, N, Merge)
%
% The best match between L1 up to Pos1 and L2 up to Pos2
% involves N changes to L2, giving Merge.

:- dynamic rmerge_tmp/4.

rmerge_sentence_and_pattern(SentAtom, PatternAtom, InTemplate, Score, MergedPattern) :-
	parse_response_atom(SentAtom, InTemplate, SentList),
	parse_response_atom(PatternAtom, InTemplate, PatternList),
	rmerge(SentList, PatternList, Score, MergedList),
	make_rpattern_canonical(MergedList, MergedList1),
	generate_response(MergedList1, MergedPattern),
	!.
rmerge_sentence_and_pattern(SentAtom, PatternAtom, Score, MergedPattern) :-
	format('~N*** Error: bad call: ~w~n',
	       [rmerge_sentence_and_pattern(SentAtom, PatternAtom, Score, MergedPattern)]),
	fail.

rmerge(L1, L2, Total, Merge) :-
	is_list(L1),
	is_list(L2),
	init_rmerge,
	length(L1, LastPos1),
	length(L2, LastPos2),
	N is LastPos1 + LastPos2,
	rmerge1(1, N, LastPos1, LastPos2, L1, L2),
	rmerge_tmp(LastPos1, LastPos2, Total, Merge0),
	reverse(Merge0, Merge),
	!.
rmerge(L1, L2, Total, I, D, S, Matches) :-
	format('~N*** Error: bad call: ~w.~n', [rmerge(L1, L2, Total, I, D, S, Matches)]),
	fail.

init_rmerge :-
	retractall(rmerge_tmp(_Pos1, _Pos2, _N, _M)),
	assertz(rmerge_tmp(0, 0, 0, [])),
	!.

rmerge1(I, N, _LastPos1, _LastPos2, _L1, _L2) :-
	I > N,
	!.
rmerge1(I, N, LastPos1, LastPos2, L1, L2) :-
	I =< N,
	rmerge2(0, I, LastPos1, LastPos2, L1, L2),
	I1 is I + 1,
	!,
	rmerge1(I1, N, LastPos1, LastPos2, L1, L2).

rmerge2(Pos1, I, _LastPos1, _LastPos2, _L1, _L2) :-
	Pos1 > I,
	!.
rmerge2(Pos1, I, LastPos1, LastPos2, L1, L2) :-
	Pos1 =< I,
	Pos2 is I - Pos1,
	rmerge3(Pos1, Pos2, LastPos1, LastPos2, L1, L2),
	Pos1Next is Pos1 + 1,
	!,
	rmerge2(Pos1Next, I, LastPos1, LastPos2, L1, L2).

rmerge3(Pos1, Pos2, LastPos1, LastPos2, _L1, _L2) :-
	( Pos1 > LastPos1 ; Pos2 > LastPos2 ),
	!.
rmerge3(Pos1, Pos2, _LastPos1, _LastPos2, L1, L2) :-
	insertion_score(Pos1, Pos2, L1, L2, InsTotal-InsMerge),
	deletion_score(Pos1, Pos2, L1, L2, DelTotal-DelMerge),
	substitution_score(Pos1, Pos2, L1, L2, SubTotal-SubMerge),
	substitution2_score(Pos1, Pos2, L1, L2, Sub2Total-Sub2Merge),
	best_rmerge_score([InsTotal-InsMerge,
			   DelTotal-DelMerge,
			   SubTotal-SubMerge,
			   Sub2Total-Sub2Merge],
			  BestTotal-BestMerge),
	assertz(rmerge_tmp(Pos1, Pos2, BestTotal, BestMerge)).

best_rmerge_score(Pairs, BestTotal-BestInfo) :-
	keysort(Pairs, SortedPairs),
	SortedPairs = [BestTotal-BestInfo | _Rest],
	!.

% We can't insert to reach position 0
insertion_score(Pos1, _Pos2, _L1, _L2, Total-M) :-
	Pos1 = 0,
	impossible_score(Total-M),
	!.
insertion_score(Pos1, Pos2, L1, _L2, Total-M) :-
	Pos1Minus1 is Pos1 - 1,
	rmerge_tmp(Pos1Minus1, Pos2, NextTotal, NextM),
	Total is NextTotal + 1,
	safe_nth(Pos1, L1, Word1),
	M = [optional(Word1) | NextM],
	!.

% We can't delete from position 0
deletion_score(_Pos1, Pos2, _L1, _L2, Total-M) :-
	Pos2 = 0,
	impossible_score(Total-M),
	!.
deletion_score(Pos1, Pos2, _L1, L2, Total-M) :-
	Pos2Minus1 is Pos2 - 1,
	rmerge_tmp(Pos1, Pos2Minus1, NextTotal, NextM),
	safe_nth(Pos2, L2, Word2),
	(   matches_null(Word2) ->
	    Total is NextTotal,
	    M = [Word2 | NextM]
	;
	    otherwise ->
	    Total is NextTotal + 1,
	    M = [optional(Word2) | NextM]
	),
	!.

% We can't substitute if either string is at position 0
substitution_score(Pos1, Pos2, _L1, _L2, Total-M) :-
	( Pos1 = 0 ; Pos2 = 0 ),
	impossible_score(Total-M),
	!.
substitution_score(Pos1, Pos2, L1, L2, Total-M) :-
	Pos1Minus1 is Pos1 - 1,	
	Pos2Minus1 is Pos2 - 1,
	safe_nth(Pos1, L1, Word1),
	safe_nth(Pos2, L2, Word2),
	rmerge_tmp(Pos1Minus1, Pos2Minus1, NextTotal, NextM),
	simple_rmerge([Word1], [Word2], Merge0, ThisTotal),
	safe_reverse(Merge0, Merge),
	safe_append(Merge, NextM, M),
	Total is NextTotal + ThisTotal,
	%(   matches(Word1, Word2) ->
	%    Total is NextTotal,
	%    M = [Word2 | NextM]
	%;
	%    otherwise ->
	%    Total is NextTotal + 1,
	%    M = [or(Word2, Word1) | NextM]
	%),
	!.

% substitute2: match two elements from the first string against one of the second.

% We can't substitute2 if either string is at position 0 or the first one is at position 1
substitution2_score(Pos1, Pos2, _L1, _L2, Total-M) :-
	( Pos1 = 0 ; Pos1 = 1 ; Pos2 = 0 ),
	impossible_score(Total-M),
	!.
substitution2_score(Pos1, Pos2, L1, L2, Total-M) :-
	Pos1Minus1 is Pos1 - 1,
	Pos1Minus2 is Pos1 - 2,	
	Pos2Minus1 is Pos2 - 1,
	safe_nth(Pos1Minus1, L1, Word1a),
	safe_nth(Pos1, L1, Word1b),
	safe_nth(Pos2, L2, Word2),
	rmerge_tmp(Pos1Minus2, Pos2Minus1, NextTotal, NextM),
	simple_rmerge([Word1a, Word1b], [Word2], Merge0, ThisTotal),
	safe_reverse(Merge0, Merge),
	safe_append(Merge, NextM, M),
	Total is NextTotal + ThisTotal,
	!.

matches_null(optional(_Body)) :-
	!.
matches_null(or(P, Q)) :-
	(   matches_null(P) ->
	    true
	;
	    otherwise ->
	    matches_null(Q)
	),
	!.

simple_rmerge(X, Y, BestMerged, BestScore) :-
	findall(Score-Merged,
		simple_rmerge1(X, Y, Merged, Score),
		Pairs),
	keysort(Pairs, SortedPairs),
	SortedPairs = [BestScore-BestMerged | _],
	!.

simple_rmerge1(X, X, X, 0) :-
	!.
simple_rmerge1(X, [X], [X], 0) :-
	!.
simple_rmerge1([X], X, [X], 0) :-
	!.
simple_rmerge1(X, Y, or(X, Y), Score) :-
	gen_length(or(X, Y), Score).

simple_rmerge1([X], [X1], [X2], Score) :-
	simple_rmerge1(X, X1, X2, Score).
simple_rmerge1([X], [X1], X2, Score) :-
	simple_rmerge1([X], X1, X2, Score).

simple_rmerge1([X, Y], [X1, Y1], [X2, Y2], Total) :-
	simple_rmerge1(X, X1, X2, S1),
	simple_rmerge1(Y, Y1, Y2, S2),
	Total is S1 + S2.
simple_rmerge1([X, Y], [X1], [X2], Total) :-
	simple_rmerge1([X, Y], X1, X2, Total).
simple_rmerge1(X, optional(Body), optional(Merged), Total) :-
	simple_rmerge1(X, Body, Merged, Total).
simple_rmerge1(X, or(P, Q), or(Merged, Q), Score) :-
	simple_rmerge1(X, P, Merged, Score).
simple_rmerge1(X, or(P, Q), or(P, Merged), Score) :-
	simple_rmerge1(X, Q, Merged, Score).

gen_length([], 0) :-
	!.
gen_length([F | R], Score) :-
	gen_length(F, ScoreF),
	gen_length(R, ScoreR),
	Score is ScoreR + ScoreF,
	!.
gen_length(or(X, Y), Score) :-
	gen_length(X, Len1),
	gen_length(Y, Len2),
	safe_min_list([Len1, Len2], Score),
	!.
gen_length(optional(Body), Score) :-
	gen_length(Body, Score),
	!.
gen_length(_Other, 1).

%matches(Word, Word) :-
%	!.
%matches([X, Y], [X1, Y1]) :-
%	matches(X, X1),
%	matches(Y, Y1).
%matches(X, optional(Body)) :-
%	matches(X, Body),
%	!.
%matches(X, or(P, Q)) :-
%	(   matches(X, P) ->
%	    true
%	;
%	    otherwise ->
%	    matches(X, Q)
%	),
%	!.

impossible_score(3000-[1000, []]).

%----------------------------------------------------

make_rpattern_canonical(A, A) :-
	atomic(A),
	!.
make_rpattern_canonical([F | R], [F1 | R1]) :-
	make_rpattern_canonical(F, F1),
	make_rpattern_canonical(R, R1),
	!.
make_rpattern_canonical(optional(P), optional(P1)) :-
	make_rpattern_canonical(P, P1),
	!.
make_rpattern_canonical(or(P, Q), Out) :-
	make_rpattern_canonical(P, P1),
	make_rpattern_canonical(Q, Q1),
	make_single_element_list_into_element(P1, P2),
	make_single_element_list_into_element(Q1, Q2),
	(   ( P2 = or(ArgsP), Q2 = or(ArgsQ) ) ->
	    append(ArgsP, ArgsQ, AllArgs)
	;
	    P2 = or(ArgsP) ->
	    append(ArgsP, [Q2], AllArgs)
	;
	    Q2 = or(ArgsQ) ->
	    append([P2], ArgsQ, AllArgs)
	;
	    otherwise ->
	    AllArgs = [P2, Q2]
	),
	Out = or(AllArgs),
	!.
make_rpattern_canonical(X, Y) :-
	format('~N*** Error: bad call: ~w~n',
	       [make_rpattern_canonical(X, Y)]),
	fail.

make_single_element_list_into_element([X], X) :-
	!.
make_single_element_list_into_element(X, X).

%----------------------------------------------------

is_atom_list(List) :-
	is_list(List),
	is_atom_list1(List).

is_atom_list1([]).
is_atom_list1([F | R]) :-
	atom(F),
	!,
	is_atom_list1(R).

%----------------------------------------------------

nuance_main_grammar_name_for_id(default, '.MAIN') :-
	!.
nuance_main_grammar_name_for_id(Namespace:Domain:Id, MainGrammar) :-
	format_to_atom('.MAIN__~w__~w__~w', [Namespace, Domain, Id], MainGrammar),
	!.
nuance_main_grammar_name_for_id(Namespace:Id, MainGrammar) :-
	format_to_atom('.MAIN__~w__~w', [Namespace, Id], MainGrammar),
	!.
nuance_main_grammar_name_for_id(Id, MainGrammar) :-
	format_to_atom('.MAIN__~w', [Id], MainGrammar),
	!.

nuance_top_grammar_name_for_id(default, 'TOP') :-
	!.
nuance_top_grammar_name_for_id(Namespace:Domain:Id, MainGrammar) :-
	format_to_atom('TOP__~w__~w__~w', [Namespace, Domain, Id], MainGrammar),
	!.
nuance_top_grammar_name_for_id(Namespace:Id, MainGrammar) :-
	format_to_atom('TOP__~w__~w', [Namespace, Id], MainGrammar),
	!.
nuance_top_grammar_name_for_id(Id, TopGrammar) :-
	format_to_atom('TOP__~w', [Id], TopGrammar),
	!.

nuance_grammar_name_for_phrase_id(phrase(Namespace, Domain, PhraseId), Grammar) :-
	format_to_atom('PHRASE_~w__~w__~w', [Namespace, Domain, PhraseId], Grammar),
	!.

nuance_grammar_name_for_tr_phrase_id(tr_phrase(Namespace, Domain, PhraseId), Grammar) :-
	format_to_atom('PHRASE_~w__~w__~w', [Namespace, Domain, PhraseId], Grammar),
	!.

%----------------------------------------------------

is_key_val_list(List) :-
	is_list(List),
	is_key_val_list1(List).

is_key_val_list1([]).
is_key_val_list1([Key=Val | R]) :-
	atomic(Key),
	atomic(Val),
	!,
	is_key_val_list1(R).

is_abstract_response_list([]).
is_abstract_response_list([F | R]) :-
	is_abstract_response(F),
	is_abstract_response_list(R).

is_abstract_response(Atom) :-
	atomic(Atom).
is_abstract_response([F | R]) :-
	is_abstract_response(F),
	is_abstract_response(R).
is_abstract_response(or(F, R)) :-
	is_abstract_response(F),
	is_abstract_response(R).
is_abstract_response(optional(F)) :-
	is_abstract_response(F).
is_abstract_response(phrase(F)) :-
	is_abstract_response(F).

safe_reverse(X, Y) :-
	is_list(X),
	reverse(X, Y),
	!.
safe_reverse(X, X).

safe_append(X, Y, Z) :-
	is_list(X),
	append(X, Y, Z),
	!.
safe_append(X, Y, [X | Y]) :-
	!.

phrase_id_atom(Atom, Atom1) :-
	atom(Atom),
	atom_codes(Atom, Str),
	Str = [0'$ | Str1],
	phrase_id_string(Str1),
	atom_codes(Atom1, Str1).

tr_phrase_id_atom(Atom, Atom1) :-
	atom(Atom),
	atom_codes(Atom, Str),
	Str = [0'$, 0'$ | Str1],
	phrase_id_string(Str1),
	atom_codes(Atom1, Str1).

phrase_id_string(Str) :-
	Str \== [],
	phrase_id_string1(Str).

phrase_id_string1([]).
phrase_id_string1([F | R]) :-
	phrase_id_char(F),
	!,
	phrase_id_string1(R).

%----------------------------------------------------

:- dynamic store_lite_errors/0.
:- dynamic stored_lite_error/1.

init_stored_lite_errors :-
	retractall(stored_lite_error(_)),
	retractall(store_lite_errors),
	assertz(store_lite_errors).

format_lite_error(FormatString, Args) :-
	format(FormatString, Args),
	(   store_lite_errors ->
	    format_to_atom(FormatString, Args, ErrorAtom),
	    assertz(stored_lite_error(ErrorAtom))
	;
	    otherwise ->
	    true
	),
	!.

lite_errors_found :-
	stored_lite_error(_ErrorAtom).
