:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(lite_flat_file_to_grammar,
	[compile_lite_from_cfg_file_parameters/0,
	 compile_current_lite_namespace_from_cfg_file_parameters/0,
	 	 
	 lite_flat_file_to_grammar_files/8,
	 lite_flat_file_to_questionnaire_files/7,
	 lite_flat_file_to_translation_files/5,

	 load_top_nuance_grammars_file/0,
	 get_top_level_nuance_grammars/1,
	 
	 test_flat_file_to_grammar/1]
    ).

:- use_module('$REGULUS/Prolog/lite_read_flat_file').
:- use_module('$REGULUS/Prolog/lite_utils').

:- use_module('$REGULUS/Prolog/dialogue').
:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(samsort)).

%======================================================================

test_flat_file_to_grammar(all_courses) :-
	lite_flat_file_to_grammar_files([[['$CALLSLT/Eng/corpora/english_course_flat_templatised.txt', default_encoding, [french, german]],
					  ['$CALLSLT/Eng/corpora/english_course_flat_chinese.txt', 'UTF-8', [chinese]]]-user,
					 [['$CALLSLT/Eng/corpora/toy_course.txt', default_encoding, [french, german]]]-toy,
					 [['$CALLSLT/Eng/corpora/toy_hello_course.txt', default_encoding, [french]]]-hello,	    
					 [['$CALLSLT/Eng/corpora/pronunciation_flat.txt', default_encoding, [french]]]-pronunciation
					],
					'$CALLSLT/Eng/GeneratedFiles/english_course_flat_templatised.regulus',
					'$CALLSLT/Eng/GeneratedFiles/english_course_flat_direct.grammar',
					'$CALLSLT/Eng/GeneratedFiles/english_course_tables.pl',
					%[french-'$CALLSLT/Eng/GeneratedFiles/english_course_flat_generation_french.regulus',
					%german-'$CALLSLT/Eng/GeneratedFiles/english_course_flat_generation_german.regulus',
					%chinese-'$CALLSLT/Eng/GeneratedFiles/english_course_flat_generation_chinese.regulus'],
					'$CALLSLT/Eng/GeneratedFiles/english_course_flat_corpus.pl',
					'$CALLSLT/Eng/GeneratedFiles/english_course_flat_corpus_only_domains.pl',
					'$CALLSLT/Eng/GeneratedFiles/english_course_flat_corpus_lf.pl').

test_flat_file_to_grammar(english_course_fr_ge_ch) :-
	lite_flat_file_to_grammar_files([[['$CALLSLT/Eng/corpora/english_course_flat_templatised.txt', default_encoding, [french, german]],
					  ['$CALLSLT/Eng/corpora/english_course_flat_chinese.txt', 'UTF-8', [chinese]]]-user],
					'$CALLSLT/Eng/GeneratedFiles/english_course_flat_templatised.regulus',
					'$CALLSLT/Eng/GeneratedFiles/english_course_flat_direct.grammar',
					'$CALLSLT/Eng/GeneratedFiles/english_course_tables.pl',
					%[french-'$CALLSLT/Eng/GeneratedFiles/english_course_flat_generation_french.regulus',
					%german-'$CALLSLT/Eng/GeneratedFiles/english_course_flat_generation_german.regulus',
					%chinese-'$CALLSLT/Eng/GeneratedFiles/english_course_flat_generation_chinese.regulus'],
					'$CALLSLT/Eng/GeneratedFiles/english_course_flat_corpus.pl',
					'$CALLSLT/Eng/GeneratedFiles/english_course_flat_corpus_only_domains.pl',
					'$CALLSLT/Eng/GeneratedFiles/english_course_flat_corpus_lf.pl').
					
test_flat_file_to_grammar(eng_ger_small) :-
	lite_flat_file_to_grammar_files([[['$CALLSLT/Eng/corpora/english_course_flat_small.txt', default_encoding, [default]]]-user],
					'$CALLSLT/Eng/GeneratedFiles/english_course_flat_small.regulus',
					'$CALLSLT/Eng/GeneratedFiles/english_course_flat_direct_small.grammar',
					'$CALLSLT/Eng/GeneratedFiles/english_course_tables_small.pl',
					%[default-'$CALLSLT/Eng/GeneratedFiles/english_course_flat_generation_small.regulus'],
					'$CALLSLT/Eng/GeneratedFiles/english_course_flat_corpus_small.pl',
					'$CALLSLT/Eng/GeneratedFiles/english_course_flat_corpus_only_domains_small.pl',
					'$CALLSLT/Eng/GeneratedFiles/english_course_flat_corpus_lf_small.pl').

test_flat_file_to_grammar(questionnaire_small) :-
	lite_flat_file_to_questionnaire_files([[namespace=user,
						main_spec=[['$CALLSLT/Que/corpora/red_cross_grammar_small.txt', default_encoding, [french, spanish]]],
						aux_spec=[]]],
					      '$CALLSLT/Que/GeneratedFiles/red_cross_small.regulus',
					      '$CALLSLT/Que/GeneratedFiles/red_cross_direct_small.grammar',
					      [french-'$CALLSLT/Que/GeneratedFiles/red_cross_lf_translation_answers_french_small.pl',
					       spanish-'$CALLSLT/Que/GeneratedFiles/red_cross_lf_translation_answers_french_small.pl'],
					      '$CALLSLT/Que/GeneratedFiles/red_cross_corpus_only_domains_small.pl',
					      [french-'$CALLSLT/Que/GeneratedFiles/red_cross_speech_french_small.txt',
					       spanish-'$CALLSLT/Que/GeneratedFiles/red_cross_speech_spanish_small.txt']).

test_flat_file_to_grammar(questionnaire_cfg) :-
	lite_flat_file_to_questionnaire_files_cfg([[namespace=user,
						    main_spec=[['$CALLSLT/Que/corpora/red_cross_grammar.txt', default_encoding, [french, spanish]]],
						    %main_spec=[['$CALLSLT/Que/corpora/red_cross_grammar_14_1_15.txt', default_encoding, [french, spanish]]],
						    aux_spec=[]]],
					      '$CALLSLT/Que/GeneratedFiles/red_cross.regulus',
					      '$CALLSLT/Que/GeneratedFiles/red_cross_direct.grammar',
					      [french-'$CALLSLT/Que/GeneratedFiles/red_cross_lf_translation_answers_french.pl',
					       spanish-'$CALLSLT/Que/GeneratedFiles/red_cross_lf_translation_answers_french.pl'],
					      [french-'$CALLSLT/Que/GeneratedFiles/red_cross_speech_french.txt',
					       spanish-'$CALLSLT/Que/GeneratedFiles/red_cross_speech_spanish.txt']).


test_flat_file_to_grammar(questionnaire_trivial_cfg) :-
	lite_flat_file_to_questionnaire_files_cfg([[namespace=user,
						    %main_spec=[['$CALLSLT/Que/corpora/red_cross_grammar_trivial.txt', default_encoding, [french, spanish]]],
						    main_spec=[['$CALLSLT/Que/corpora/red_cross_grammar_small.txt', default_encoding, [french, spanish]]],
						    aux_spec=[]]],
					      '$CALLSLT/Que/GeneratedFiles/red_cross_trivial.regulus',
					      '$CALLSLT/Que/GeneratedFiles/red_cross_direct_trivial.grammar',
					      [french-'$CALLSLT/Que/GeneratedFiles/red_cross_lf_translation_answers_french_trivial.pl',
					       spanish-'$CALLSLT/Que/GeneratedFiles/red_cross_lf_translation_answers_french_trivial.pl'],
					      [french-'$CALLSLT/Que/GeneratedFiles/red_cross_speech_french_trivial.txt',
					       spanish-'$CALLSLT/Que/GeneratedFiles/red_cross_speech_spanish_trivial.txt']).


test_flat_file_to_grammar(trainslate1) :-
	lite_flat_file_to_translation_files([[['$MED_SLT2/Fre/corpora/trainslate_minicorpus.txt', default_encoding, [sign, english]]]-trainslate1],
					    '$MED_SLT2/Fre/GeneratedFiles/trainslate1.regulus',
					    '$MED_SLT2/Fre/GeneratedFiles/trainslate1.grammar',
					    '$MED_SLT2/Fre/GeneratedFiles/trainslate1_tables.pl').

test_flat_file_to_grammar(trainslate2) :-
	lite_flat_file_to_translation_files_cfg([[namespace=trainslate,
						  main_spec=[['$MED_SLT2/Fre/corpora/trainslate_minicorpus.txt', [sign, english]]],
						  aux_spec=[]]],
					    '$MED_SLT2/Fre/GeneratedFiles/trainslate1.regulus',
					    '$MED_SLT2/Fre/GeneratedFiles/trainslate1.grammar').

test_flat_file_to_grammar(trainslate3) :-
	lite_flat_file_to_translation_files_cfg([[namespace=trainslate,
						  main_spec=[['$MED_SLT2/Fre/corpora/trainslate_minicorpus_phrases2.txt', [sign, english]]],
						  aux_spec=[]]],
					    '$MED_SLT2/Fre/GeneratedFiles/trainslate2.regulus',
					    '$MED_SLT2/Fre/GeneratedFiles/trainslate2.grammar').

%======================================================================

compile_lite_from_cfg_file_parameters :-
	check_config_file_is_loaded,
	(   lite_file_is_defined(Type) ->
	    assemble_lite_file_spec(FileSpec),
	    compile_lite_from_cfg_file_parameters1(Type, FileSpec)
	;
	    otherwise ->
	    format_lite_error('~N*** Error: no Lite file is defined~n', []),
	    fail
	).

compile_current_lite_namespace_from_cfg_file_parameters :-
	check_config_file_is_loaded,
	(   \+ lite_file_is_defined(Type) ->
	    format_lite_error('~N*** Error: no Lite file is defined~n', []),
	    fail
	;
	    get_current_namespace_from_dialogue_state(Namespace) ->
	    assemble_lite_file_spec_for_namespace(Namespace, FileSpec),
	    compile_lite_from_cfg_file_parameters1(Type, [FileSpec])
	;
	    format_lite_error('~N*** Error: no current namespace found~n', []),
	    fail
	).

compile_lite_from_cfg_file_parameters1(call, FileSpec) :-
	get_config_item_or_complain(lite_regulus_grammar, L2GrammarFile),
	get_config_item_or_complain(lite_nuance_grammar, L2NuanceGrammarFile),
	get_config_item_or_complain(lite_nuance_top_grammars, L2NuanceTopGrammarFile),
	%assemble_l1_grammar_alist_spec(L1GrammarFileAlist),
	get_config_item_or_complain(lite_tables_file, TablesFile),
	get_config_item_or_complain(lite_full_corpus_file, CorpusFile),
	get_config_item_or_complain(lite_corpus_file, CorpusFile2),
	get_config_item_or_complain(lite_corpus_lf_file, CorpusLFFile),

	lite_flat_file_to_grammar_files(FileSpec, L2GrammarFile, L2NuanceGrammarFile, L2NuanceTopGrammarFile,
					%L1GrammarFileAlist,
					TablesFile,
					CorpusFile, CorpusFile2, CorpusLFFile).

compile_lite_from_cfg_file_parameters1(translation, FileSpec) :-
	get_config_item_or_complain(lite_regulus_grammar, L2GrammarFile),
	get_config_item_or_complain(lite_nuance_grammar, L2NuanceGrammarFile),
	get_config_item_or_complain(lite_nuance_top_grammars, L2NuanceTopGrammarFile),
	%assemble_l1_grammar_alist_spec(L1GrammarFileAlist),
	%get_config_item_or_complain(lite_tables_file, TablesFile),

	lite_flat_file_to_translation_files_cfg(FileSpec, L2GrammarFile, L2NuanceGrammarFile, L2NuanceTopGrammarFile).

compile_lite_from_cfg_file_parameters1(questionnaire, FileSpec) :-
	get_config_item_or_complain(lite_regulus_grammar, L2GrammarFile),
	get_config_item_or_complain(lite_nuance_grammar, L2NuanceGrammarFile),
	get_config_item_or_complain(lite_nuance_top_grammars, L2NuanceTopGrammarFile),
	assemble_lf_translation_answer_file_alist(LFTranslationAnswerFileAlist),
	assemble_l2_file_alist(L2FileAlist),
	(   user:regulus_config(lite_compilation, cfg) ->
	    lite_flat_file_to_questionnaire_files_cfg(FileSpec, L2GrammarFile, L2NuanceGrammarFile, L2NuanceTopGrammarFile,
						      LFTranslationAnswerFileAlist, L2FileAlist)
	;
	    otherwise ->
	    get_config_item_or_complain(lite_corpus_file, CorpusFile2),
	    lite_flat_file_to_questionnaire_files(FileSpec, L2GrammarFile, L2NuanceGrammarFile, L2NuanceTopGrammarFile,
						  LFTranslationAnswerFileAlist, CorpusFile2, L2FileAlist)
	).

lite_file_is_defined(Type) :-
	user:regulus_config(lite_file(Type), _File).
lite_file_is_defined(Type) :-
	user:regulus_config(lite_file(Type, _Namespace, _Languages), _File).
lite_file_is_defined(Type) :-
	user:regulus_config(lite_file(Type, _Namespace, _Encoding, _Languages), _File).


%======================================================================

load_top_nuance_grammars_file :-
	get_config_item_or_complain(lite_nuance_top_grammars, File),
	safe_absolute_file_name(File, AbsFile),
	(   safe_file_exists(AbsFile) ->
	    compile(AbsFile),
	    format('~N--- Loaded top-level Nuance grammar IDs from ~w~n', [AbsFile])
	;
	    otherwise ->
	    format('~N*** Error: unable to find file: ~w~n', [AbsFile]),
	    fail
	).
load_top_nuance_grammars_file :-
	format('~N*** Error: unable to load top-level Nuance grammar IDs~n', []),
	fail.

get_top_level_nuance_grammars(GrammarNames) :-
	(   current_predicate(lite_flat_file_to_grammar:top_grammar_name/1) ->
	    findall(GrammarName,
		    top_grammar_name(GrammarName),
		    GrammarNames)
	;
	    otherwise ->
	    format('~N*** Warning: no top-level Nuance grammar names defined~n', []),
	    GrammarNames = []
	).

%======================================================================
  
lite_flat_file_to_grammar_files(FlatFile, L2GrammarFile, L2NuanceGrammarFile, L2NuanceTopGrammarFile,
				%L1GrammarFileAlist,
				TablesFile,
				CorpusFile, CorpusFile2, CorpusLFFile) :-
	read_flat_file(FlatFile, Units, call),
	write_out_expanded_flat_file(FlatFile, Units),
	units_to_l2_grammar(Units, L2GrammarFile),
	units_to_l2_nuance_grammar(call, Units, L2NuanceGrammarFile, L2NuanceTopGrammarFile),
	%units_to_l1_grammars(Units, L1GrammarFileAlist),
	units_to_tables_file(Units, TablesFile),
	units_to_corpus(Units, CorpusFile),
	units_to_corpus_only_domains(Units, CorpusFile2),
	%first_l1_id(L1GrammarFileAlist, L1Id),
	first_l1_id(FlatFile, L1Id),
	units_to_corpus_lf_file(Units, L1Id, CorpusLFFile).

lite_flat_file_to_translation_files(FlatFile,
				    L2GrammarFile, L2NuanceGrammarFile, L2NuanceTopGrammarFile, TablesFile) :-
	read_flat_file(FlatFile, Units, translation),
	write_out_expanded_flat_file(FlatFile, Units),
	units_to_l2_grammar(Units, L2GrammarFile),
	units_to_l2_nuance_grammar(call, Units, L2NuanceGrammarFile, L2NuanceTopGrammarFile),
	units_to_tables_file(Units, TablesFile).

lite_flat_file_to_translation_files_cfg(FlatFile, _L2GrammarFile, L2NuanceGrammarFile, L2NuanceTopGrammarFile) :-
	read_flat_file(FlatFile, Units, translation, dont_expand),
	write_out_expanded_flat_file(FlatFile, Units),
	%units_to_l2_cfg_translation_grammar(Units, L2GrammarFile),
	units_to_l2_nuance_cfg_translation_grammar(Units, L2NuanceGrammarFile, L2NuanceTopGrammarFile).

lite_flat_file_to_questionnaire_files(FlatFile, L2GrammarFile, L2NuanceGrammarFile, L2NuanceTopGrammarFile,
				      LFTranslationAnswerFileAlist, CorpusFile2, L2FileAlist) :-
	read_flat_file(FlatFile, Units, questionnaire),
	write_out_expanded_flat_file(FlatFile, Units),
 	store_group_domain_association(Units),
	units_to_l2_grammar(Units, L2GrammarFile),
	units_to_l2_nuance_grammar(questionnaire, Units, L2NuanceGrammarFile, L2NuanceTopGrammarFile),
	units_to_lf_translation_answer_file_alist(Units, LFTranslationAnswerFileAlist),
	units_to_corpus_only_domains(Units, CorpusFile2),
	units_to_l2_file_alist(Units, L2FileAlist).

lite_flat_file_to_questionnaire_files_cfg(FlatFile, L2GrammarFile, L2NuanceGrammarFile, L2NuanceTopGrammarFile,
					  LFTranslationAnswerFileAlist, L2FileAlist) :-
	read_flat_file(FlatFile, Units, questionnaire, dont_expand),
	write_out_expanded_flat_file(FlatFile, Units),
 	store_group_domain_association(Units),
	units_to_l2_cfg_grammar(Units, L2GrammarFile),
	units_to_l2_nuance_cfg_grammar(questionnaire, Units, L2NuanceGrammarFile, L2NuanceTopGrammarFile),
	units_to_lf_translation_answer_file_alist(Units, LFTranslationAnswerFileAlist),
	units_to_l2_file_alist(Units, L2FileAlist).

%first_l1_id(L1GrammarFileAlist, L1Id) :-
%	L1GrammarFileAlist = [L1Id-_File | _],
%	!.
first_l1_id(FlatFileSpec, L1Id) :-
	%FlatFileSpec = [[[_File, [L1Id | _]] | _]-_Namespace | _],
	FlatFileSpec = [FirstNamespaceItem | _R],
	member(main_spec=List, FirstNamespaceItem),
	List = [FirstFileItem | _R1],
	FirstFileItem = [_FlatFile, [L1Id | _]],
	!.
first_l1_id(FlatFileSpec, L1Id) :-
	format_lite_error('~N*** Error: bad call: ~w~n', [first_l1_id(FlatFileSpec, L1Id)]),
	fail.

write_out_expanded_flat_file(FlatFile, Units) :-
	expanded_flat_file(FlatFile, ExpandedFlatFile),
	list_to_prolog_file_prettyprint_with_encoding(Units, ExpandedFlatFile, 'UTF-8'),
	length(Units, N),
	format('~N--- Written out expanded file (~d records) ~w~n', [N, ExpandedFlatFile]),
	!.

expanded_flat_file(_FlatFile, ExpandedFlatFile) :-
	current_predicate(user:get_regulus_config_item/2),
	user:get_regulus_config_item(expanded_flat_file, ExpandedFlatFile),
	!.
expanded_flat_file(_FlatFile, ExpandedFlatFile) :-
	\+ current_predicate(user:get_regulus_config_item/2),
	ExpandedFlatFile = '$REGULUS/tmp/tmp_flat_expanded.pl',
	!.
expanded_flat_file(FlatFile, ExpandedFlatFile) :-
	format_lite_error('~N*** Error: bad call: ~w~n', [expanded_flat_file(FlatFile, ExpandedFlatFile)]),
	fail.

units_to_l2_grammar(Units, File) :-
	all_response_lf_pairs_in_units(Units, Pairs),
	response_lf_pairs_to_grammar_rules(Pairs, Rules),
	l2_grammar_preamble(Preamble),
	append(Preamble, Rules, Grammar),
	write_out_grammar(Grammar, 'L2', File).

units_to_l2_cfg_grammar(Units, File) :-
	init_units_to_l2_cfg_grammar,
	
	all_response_lf_pairs_in_units(Units, UnitPairs),
	response_lf_pairs_to_cfg_grammar_rules(UnitPairs, MainRules),

	all_phraseid_response_pairs_in_units(Units, PhrasePairs),
	phraseid_response_pairs_to_cfg_grammar_rules(PhrasePairs, PhraseRules),

	all_generated_rules(GeneratedRules),

	append_list([MainRules, PhraseRules, GeneratedRules], AllRules),
	
	l2_grammar_preamble_for_rules(AllRules, Preamble),
	append(Preamble, AllRules, Grammar),
	write_out_grammar(Grammar, 'L2', File).

%--------------------------------------------------

% Converting Lite grammar to Regulus CFG grammar

:- dynamic generated_cfg_rule/2.

init_units_to_l2_cfg_grammar :-
	retractall(generated_rule_counter(_)),
	retractall(generated_cfg_rule(_, _)),
	assertz(generated_rule_counter(1)).

response_lf_pairs_to_cfg_grammar_rules([], []).
response_lf_pairs_to_cfg_grammar_rules([F | R], [F1 | R1]) :-
	response_lf_pair_to_cfg_grammar_rule(F, F1),
	!,
	response_lf_pairs_to_cfg_grammar_rules(R, R1).

all_phraseid_response_pairs_in_units(Units, PhrasePairs) :-
	findall([PhraseId, Response],
		(   member(phrase_unit(_Lines, Body), Units),
		    member(phraseid=PhraseId, Body),
		    member(response=Response, Body)
		),
		PhrasePairs).

phraseid_response_pairs_to_cfg_grammar_rules([], []).
phraseid_response_pairs_to_cfg_grammar_rules([F | R], [F1 | R1]) :-
	phraseid_response_pair_to_cfg_grammar_rule(F, F1),
	!,
	phraseid_response_pairs_to_cfg_grammar_rules(R, R1).
		    
phraseid_response_pair_to_cfg_grammar_rule([PhraseId, Response], Rule) :-
	make_response_canonical(Response, Response1),
	response_to_cfg_grammar_rule_body(Response1, Body),
	phraseid_to_category(PhraseId, Category),
	Rule = ( Category:[] --> Body ).

response_lf_pair_to_cfg_grammar_rule([Response, LF], Rule) :-
	make_response_canonical(Response, Response1),
	response_to_cfg_grammar_rule_body(Response1, Body),
	Rule = ( top:[sem=[[null, LF]]] --> Body ).

make_response_canonical(Atom, Atom) :-
	atomic(Atom),
	!.
make_response_canonical(phrase(Namespace, Domain, PhraseId), phrase(Namespace, Domain, PhraseId)) :-
	!.
make_response_canonical(tr_phrase(Namespace, Domain, PhraseId, Index), tr_phrase(Namespace, Domain, PhraseId, Index)) :-
	!.
make_response_canonical(optional(Body), optional(Body1)) :-
	make_response_canonical(Body, Body1),
	!.
make_response_canonical([F | R], [F1 | R1]) :-
	make_response_canonical(F, F1),
	make_response_canonical(R, R1),
	!.
make_response_canonical(or(P, Q), Result) :-
	make_response_canonical(P, P1),
	make_response_canonical(Q, Q1),
	combine_ors(P1, Q1, Result).

combine_ors(P, Q, or(Body1)) :-
	combine_ors1(P, Q, or(Body)),
	samsort(Body, Body1).

combine_ors1(or(P), or(Q), or(PQ)) :-
	append(P, Q, PQ),
	!.
combine_ors1(or(P), Q, or(PQ)) :-
	append(P, [Q], PQ),
	!.
combine_ors1(P, or(Q), or(PQ)) :-
	append([P], Q, PQ),
	!.
combine_ors1(P, Q, or([P, Q])) :-
	!.

response_to_cfg_grammar_rule_body(Atom, Atom) :-
	atomic(Atom),
	!.
response_to_cfg_grammar_rule_body([F], F1) :-
	response_to_cfg_grammar_rule_body(F, F1),
	!.
response_to_cfg_grammar_rule_body([F | R], (F1, R1)) :-
	response_to_cfg_grammar_rule_body(F, F1),
	response_to_cfg_grammar_rule_body(R, R1),
	!.
response_to_cfg_grammar_rule_body(phrase(Namespace, Domain, PhraseId), Category:[]) :-
	phraseid_to_category(Namespace, Domain, PhraseId, Category),
	!.
response_to_cfg_grammar_rule_body(optional(Body), LHS) :-
	response_to_cfg_grammar_rule_body(Body, Body1),
	find_or_generate_cfg_rule_for(optional(Body1), ( LHS --> _RHS )),
	!.
response_to_cfg_grammar_rule_body(or(Body), LHS) :-
	response_to_cfg_grammar_rule_body_list(Body, Body1),
	find_or_generate_cfg_rule_for(or(Body1), ( LHS --> _RHS )),
	!.
response_to_cfg_grammar_rule_body(X, Y) :-
	format_lite_error('~N*** Error: bad code: ~w~n',
			  [response_to_cfg_grammar_rule_body(X, Y)]),
	fail.

response_to_cfg_grammar_rule_body_list([], []).
response_to_cfg_grammar_rule_body_list([F | R], [F1 | R1]) :-
	response_to_cfg_grammar_rule_body(F, F1),
	response_to_cfg_grammar_rule_body_list(R, R1).

find_or_generate_cfg_rule_for(Description, Rule) :-
	generated_cfg_rule(Description, Rule),
	!.
find_or_generate_cfg_rule_for(Description, Rule) :-
	generate_cfg_rule(Description, Rule),
	assertz(generated_cfg_rule(Description, Rule)),
	!.

generate_cfg_rule(Description, ( Category:[] --> Body )) :-
        generate_new_category(Category),
	generate_cfg_rule_body(Description, Body),
	!.

generate_cfg_rule_body(optional(Body), ?(Body)) :-
	!.
generate_cfg_rule_body(or(Body), Disjunction) :-
	list_to_disjunction(Body, Disjunction),
	!.
generate_cfg_rule_body(X, Y) :-
	format_lite_error('~N*** Error: bad code: ~w~n',
			  [generate_cfg_rule_body(X, Y)]),
	fail.

all_generated_rules(Rules) :-
	findall(Rule,
		generated_cfg_rule(_Description, Rule),
		Rules).

l2_grammar_preamble_for_rules(Rules, FullPreamble) :-
	l2_grammar_preamble(GeneralPreamble),
	findall(Def,
		(   member(Rule, Rules),
		    Rule = ( Category:[] --> _Body ),
		    Def = category(Category, [])
		),
		CategoryDefs),
	append(GeneralPreamble, CategoryDefs, FullPreamble).

:- dynamic generated_rule_counter/1.

generate_new_category(Category) :-
	generated_rule_counter(I),
	format_to_atom('tmp_lite_cat_~d', [I], Category),
	I1 is I + 1,
	retract(generated_rule_counter(_)),
	assertz(generated_rule_counter(I1)),
	!.

list_to_disjunction([F], F) :-
	!.
list_to_disjunction([F | R], (F ; R1)) :-
	list_to_disjunction(R, R1).

%--------------------------------------------------

units_to_l2_nuance_grammar(Type, Units, File, TopGrammarsFile) :-
	safe_absolute_file_name(File, AbsFile),
	open(AbsFile, write, S),

	grammar_ids_for_units(Type, Units, AllGrammarIds),
	units_to_l2_nuance_grammars_for_ids(AllGrammarIds, Type, Units, S),

	close(S),
	write_out_nuance_top_grammars_file(AllGrammarIds, TopGrammarsFile),
	format('--- Written Nuance grammar ~w~n', [AbsFile]).

%--------------------------------------------------

% Converting Lite grammar to Nuance CFG translation grammar

units_to_l2_nuance_cfg_translation_grammar(Units, File, TopGrammarsFile) :-
	safe_absolute_file_name(File, AbsFile),
	open(AbsFile, write, S),

	grammar_ids_for_units(translation, Units, AllGrammarIds),
	units_to_l2_nuance_cfg_translation_grammars_for_ids(AllGrammarIds, Units, S),

	tr_phrase_ids_for_units(Units, AllTrPhraseIds),
	units_to_l2_nuance_cfg_translation_grammars_for_tr_phrase_ids(AllTrPhraseIds, Units, S),
	
	all_phraseid_responses_pairs_in_units(Units, PhrasePairs),
	phraseid_responses_pairs_to_nuance_cfg_grammar(PhrasePairs, S),

	close(S),
	write_out_nuance_top_grammars_file(AllGrammarIds, TopGrammarsFile),
	format('--- Written Nuance grammar ~w~n', [AbsFile]).

units_to_l2_nuance_cfg_translation_grammars_for_ids([], _Units, _S).
units_to_l2_nuance_cfg_translation_grammars_for_ids([F | R], Units, S) :-
	units_to_l2_nuance_cfg_translation_grammars_for_id(F, Units, S),
	!,
	units_to_l2_nuance_cfg_translation_grammars_for_ids(R, Units, S).

units_to_l2_nuance_cfg_translation_grammars_for_id(Id, Units, S) :-
	all_translation_tuples_for_id_in_units(Units, Id, Tuples),
	write_l2_nuance_translation_grammar_preamble_for_id(Id, MainGrammar, S),
	write_translation_tuples_as_cfg_grammar_rules_for_id(Tuples, S),
	write_l2_nuance_grammar_coda(S),
	length(Tuples, NTuples),
	format('~N--- Written Nuance grammar "~w", ~d rules~n', [MainGrammar, NTuples]),
	!.

all_translation_tuples_for_id_in_units(Units, Id, Tuples) :-
	findall(Tuple,
		(   member(Unit, Units),
		    id_is_valid_for_unit(Id, Unit, translation),
		    translation_tuple_in_unit(Unit, Tuple)
		),
		Tuples0),
	sort(Tuples0, Tuples).

all_translation_tuples_for_tr_phrase_id_in_units(Units, Id, Tuples) :-
	findall(Tuple,
		(   member(Unit, Units),
		    tr_phrase_id_for_unit(Unit, Id),
		    translation_tuple_in_tr_phrase_unit(Unit, Tuple)
		),
		Tuples0),
	sort(Tuples0, Tuples).

/*
unit((21-25), 
     [(domain=trainslate), (namespace=trainslate1), 
      (text(sign) = 
       ['DE', tr_phrase(trainslate1,trainslate,city,1), 'POUR', tr_phrase(trainslate1,trainslate,city,2)]),
      (text(english) = 
       [from, tr_phrase(trainslate1,trainslate,city,1), to, tr_phrase(trainslate1,trainslate,city,2)]),
      (responses = 
       [[de, tr_phrase(trainslate1,trainslate,city,1), à, tr_phrase(trainslate1,trainslate,city,2)]])]).
*/

translation_tuple_in_unit(Unit, Tuple) :-
	Unit = unit(_Lines, List),
	translation_tuple_in_unit1(List, Tuple).

translation_tuple_in_tr_phrase_unit(Unit, Tuple) :-
	Unit = tr_phrase_unit(_Lines, List),
	translation_tuple_in_unit1(List, Tuple).

translation_tuple_in_unit1([], []).
translation_tuple_in_unit1([Key=Val | R], [Key=Val | R1]) :-
	(  Key = text(_) ; Key = responses ),
	!,
	translation_tuple_in_unit1(R, R1).
translation_tuple_in_unit1([_F | R], R1) :-
	!,
	translation_tuple_in_unit1(R, R1).

write_l2_nuance_translation_grammar_preamble_for_id(Id, MainGrammar, S) :-
	write_l2_nuance_grammar_preamble_for_id(Id, MainGrammar, S).

write_translation_tuples_as_cfg_grammar_rules_for_id([], _S).
write_translation_tuples_as_cfg_grammar_rules_for_id([F | R], S) :-
	write_translation_tuple_as_cfg_grammar_rules_for_id(F, S),
	write_translation_tuples_as_cfg_grammar_rules_for_id(R, S).

write_translation_tuple_as_cfg_grammar_rules_for_id(Tuple, S) :-
	member(responses=Responses, Tuple),
	findall(Lang=Translation,
		member(text(Lang)=Translation, Tuple),
		Translations),
	write_translation_tuple_as_cfg_grammar_rules_for_id1(Responses, Translations, S).

write_translation_tuple_as_cfg_grammar_rules_for_id1([], _Translations, _S).
write_translation_tuple_as_cfg_grammar_rules_for_id1([F | R], Translations, S) :-
	write_translation_tuple_as_cfg_grammar_rules_for_id2(F, Translations, S),
	!,
	write_translation_tuple_as_cfg_grammar_rules_for_id1(R, Translations, S).

/*
unit((21-25), 
     [(domain=trainslate), (namespace=trainslate1), 
      (text(sign) = 
       ['DE', tr_phrase(trainslate1,trainslate,city,1), 'POUR', tr_phrase(trainslate1,trainslate,city,2)]),
      (text(english) = 
       [from, tr_phrase(trainslate1,trainslate,city,1), to, tr_phrase(trainslate1,trainslate,city,2)]),
      (responses = 
       [[de, tr_phrase(trainslate1,trainslate,city,1), à, tr_phrase(trainslate1,trainslate,city,2)]])]).

  ( from City:city1 to City:city2 )
      { return( [
		  <sign strcat("DE" strcat(" " strcat($city1.sign strcat(" " strcat("POUR" strcat(" " $city2.sign))))))>
                  <french strcat("de" strcat(" " strcat($city1.french strcat(" " strcat("à" strcat(" " $city2.french))))))> 
                ])}
]
*/

write_translation_tuple_as_cfg_grammar_rules_for_id2(Response, Translations, S) :-
	response_to_nuance_cfg_atom(Response, Response1),
	format(S, '~N( ~w )~n', [Response1]),
	format(S, '~N     { return( [~n', []),
	write_translation_tuple_sem_values(Translations, S),
	format(S, '~N               ] ) }~n', []),
	!.

write_translation_tuple_sem_values([], _S).
write_translation_tuple_sem_values([F | R], S) :-
	write_translation_tuple_sem_value(F, S),
	write_translation_tuple_sem_values(R, S).

write_translation_tuple_sem_value(Lang=Translation, S) :-
	translation_to_nuance_sem_value(Translation, Lang, Value),
	format(S, '~N                 < ~w ~w >~n', [Lang, Value]),	
	!.

translation_to_nuance_sem_value([], _Lang, "") :-
	!.
translation_to_nuance_sem_value([Single], Lang, Value) :-
	translation_element_to_nuance_sem_value(Single, Lang, Value),
	!.
translation_to_nuance_sem_value([F | R], Lang, Value) :-
	translation_element_to_nuance_sem_value(F, Lang, Value1),
	translation_to_nuance_sem_value(R, Lang, Value2),
	format_to_atom('strcat( ~w strcat( " " ~w ) )', [Value1, Value2], Value),
	!.

translation_element_to_nuance_sem_value(Atom, _Lang, Value) :-
	atomic(Atom),
	format_to_atom('"~w"', [Atom], Value),
	!.
translation_element_to_nuance_sem_value(tr_phrase(_Namespace, _Domain, PhraseId, Index), Lang, Value) :-
	%nuance_grammar_name_for_tr_phrase_id(tr_phrase(Namespace, Domain, PhraseId), Grammar),
	format_to_atom('$~w~d.~w', [PhraseId, Index, Lang], Value),
	!.

tr_phrase_ids_for_units(Units, Ids) :-
	findall(Id,
		(   member(Unit, Units),
		    tr_phrase_id_for_unit(Unit, Id)
		),
		Ids0),
	sort(Ids0, Ids).

tr_phrase_id_for_unit(Unit, Id) :-
	Unit = tr_phrase_unit(_Lines, List),
	member(trphraseid=PhraseId, List),
	member(domain=Domain, List),
	member(namespace=Namespace, List),
	Id = tr_phrase(Namespace, Domain, PhraseId).

units_to_l2_nuance_cfg_translation_grammars_for_tr_phrase_ids([], _Units, _S).
units_to_l2_nuance_cfg_translation_grammars_for_tr_phrase_ids([F | R], Units, S) :-
	units_to_l2_nuance_cfg_translation_grammars_for_tr_phrase_id(F, Units, S),
	!,
	units_to_l2_nuance_cfg_translation_grammars_for_tr_phrase_ids(R, Units, S).

units_to_l2_nuance_cfg_translation_grammars_for_tr_phrase_id(TrPhraseId, Units, S) :-
	all_translation_tuples_for_tr_phrase_id_in_units(Units, TrPhraseId, Tuples),
	write_l2_nuance_translation_grammar_preamble_for_tr_phrase_id(TrPhraseId, Grammar, S),
	write_translation_tuples_as_cfg_grammar_rules_for_id(Tuples, S),
	write_l2_nuance_grammar_coda(S),
	length(Tuples, NTuples),
	format('~N--- Written Nuance grammar "~w", ~d rules~n', [Grammar, NTuples]),
	!.

write_l2_nuance_translation_grammar_preamble_for_tr_phrase_id(TrPhraseId, Grammar, S) :-
	nuance_grammar_name_for_tr_phrase_id(TrPhraseId, Grammar),
	format(S, '~N~n~w~n', [Grammar]),
	format(S, '~N[~n', []),
	!.
	

%--------------------------------------------------

% Converting Lite grammar to Nuance CFG grammar


units_to_l2_nuance_cfg_grammar(Type, Units, File) :-
	safe_absolute_file_name(File, AbsFile),
	open(AbsFile, write, S),

	grammar_ids_for_units(Type, Units, AllGrammarIds),
	units_to_l2_nuance_cfg_grammars_for_ids(AllGrammarIds, Type, Units, S),

	all_phraseid_responses_pairs_in_units(Units, PhrasePairs),
	phraseid_responses_pairs_to_nuance_cfg_grammar(PhrasePairs, S),

	close(S),
	format('--- Written Nuance grammar ~w~n', [AbsFile]).

units_to_l2_nuance_cfg_grammars_for_ids([], _Type, _Units, _S) :-
	!.
units_to_l2_nuance_cfg_grammars_for_ids([F | R], Type, Units, S) :-
	units_to_l2_nuance_cfg_grammars_for_id(F, Type, Units, S),
	!,
	units_to_l2_nuance_cfg_grammars_for_ids(R, Type, Units, S).

units_to_l2_nuance_cfg_grammars_for_id(Id, Type, Units, S) :-
	all_response_lf_pairs_for_id_in_units(Units, Type, Id, Pairs),
	write_l2_nuance_grammar_preamble_for_id(Id, MainGrammar, S),
	write_response_lf_pairs_as_cfg_grammar_rules_for_id(Pairs, S),
	write_l2_nuance_grammar_coda(S),
	length(Pairs, NPairs),
	format('~N--- Written Nuance grammar "~w", ~d rules~n', [MainGrammar, NPairs]),
	!.

write_response_lf_pairs_as_cfg_grammar_rules_for_id([], _S) :-
	!.
write_response_lf_pairs_as_cfg_grammar_rules_for_id([F | R], S) :-
	write_response_lf_pair_as_cfg_grammar_rules_for_id(F, S),
	!,
	write_response_lf_pairs_as_cfg_grammar_rules_for_id(R, S).

write_response_lf_pair_as_cfg_grammar_rules_for_id([Response, LF], S) :-
	response_to_nuance_cfg_atom(Response, Response1),
	format(S, '~N( ~w )~n', [Response1]),
	format(S, '~N     {return( ( ( null ~w ) ) )}~n', [LF]),
	!.

response_to_nuance_cfg_atom(Response, ResponseAtom) :-
	make_response_canonical_for_nuance(Response, Response1),
	response_to_nuance_cfg_atom1(Response1, ResponseAtom).

response_to_nuance_cfg_atom1([], '') :-
	!.
response_to_nuance_cfg_atom1(Atom, Atom) :-
	atomic(Atom),
	!.
response_to_nuance_cfg_atom1(optional(Body), Result) :-
	response_to_nuance_cfg_atom1(Body, Body1),
	format_to_atom('?( ~w )', [Body1], Result),
	!.
response_to_nuance_cfg_atom1([F], Result) :-
	response_to_nuance_cfg_atom1(F, F1),
	format_to_atom('~w', [F1], Result),
	!.
response_to_nuance_cfg_atom1([F | R], Result) :-
	response_to_nuance_cfg_atom1(F, F1),
	response_to_nuance_cfg_atom1(R, R1),
	format_to_atom('~w ~w', [F1, R1], Result),
	!.
response_to_nuance_cfg_atom1(or(Body), Result) :-
	response_to_nuance_cfg_atom1_bracketed_list(Body, Body1),
	format_to_atom('[ ~w ]', [Body1], Result),
	!.

response_to_nuance_cfg_atom1_bracketed_list([Body], Result) :-
	response_to_nuance_cfg_atom1(Body, Body1),
	format_to_atom('( ~w )', [Body1], Result),
	!.
response_to_nuance_cfg_atom1_bracketed_list([F | R], Result) :-
	response_to_nuance_cfg_atom1(F, F1),
	response_to_nuance_cfg_atom1_bracketed_list(R, R1),
	format_to_atom('( ~w ) ~w', [F1, R1], Result),
	!.

make_response_canonical_for_nuance(Atom, Atom) :-
	atomic(Atom),
	!.
make_response_canonical_for_nuance(phrase(Namespace, Domain, PhraseId), Grammar) :-
	nuance_grammar_name_for_phrase_id(phrase(Namespace, Domain, PhraseId), Grammar),
	!.
make_response_canonical_for_nuance(tr_phrase(Namespace, Domain, PhraseId, Index), FullPhraseId) :-
	nuance_grammar_name_for_tr_phrase_id(tr_phrase(Namespace, Domain, PhraseId), Grammar),
	format_to_atom('~w:~w~d', [Grammar, PhraseId, Index], FullPhraseId),
	!.
make_response_canonical_for_nuance(optional(Body), optional(Body1)) :-
	make_response_canonical_for_nuance(Body, Body1),
	!.
make_response_canonical_for_nuance([F | R], [F1 | R1]) :-
	make_response_canonical_for_nuance(F, F1),
	make_response_canonical_for_nuance(R, R1),
	!.
make_response_canonical_for_nuance(or(P, Q), Result) :-
	make_response_canonical_for_nuance(P, P1),
	make_response_canonical_for_nuance(Q, Q1),
	combine_ors(P1, Q1, Result).

all_phraseid_responses_pairs_in_units(Units, PhrasePairs) :-
	findall([phrase(Namespace, Domain, PhraseId), Responses],
		(   member(phrase_unit(_Lines, Body), Units),
		    member(namespace=Namespace, Body),
		    member(domain=Domain, Body),
		    member(phraseid=PhraseId, Body),
		    responses_in_phrase_body(Body, Responses)
		),
		PhrasePairs).

responses_in_phrase_body([], []).
responses_in_phrase_body([response=Response | R], [Response | R1]) :-
	!,
	responses_in_phrase_body(R, R1).
responses_in_phrase_body([_F | R], R1) :-
	!,
	responses_in_phrase_body(R, R1).

phraseid_responses_pairs_to_nuance_cfg_grammar([], _S).
phraseid_responses_pairs_to_nuance_cfg_grammar([F | R], S) :-
	phraseid_responses_pair_to_nuance_cfg_grammar(F, S),
	phraseid_responses_pairs_to_nuance_cfg_grammar(R, S).

phraseid_responses_pair_to_nuance_cfg_grammar([PhraseId, Responses], S) :-
	write_l2_nuance_grammar_preamble_for_phrase_grammar(PhraseId, S),
	write_responses_as_cfg_grammar_rules(Responses, S),
	write_l2_nuance_grammar_coda(S),
	!.

write_l2_nuance_grammar_preamble_for_phrase_grammar(PhraseId, S) :-
	nuance_grammar_name_for_phrase_id(PhraseId, Grammar),
	format(S, '~N~n~w~n', [Grammar]),
	format(S, '~N[~n', []),
	!.

write_responses_as_cfg_grammar_rules([], _S).
write_responses_as_cfg_grammar_rules([F | R], S) :-
	write_response_as_cfg_grammar_rule(F, S),
	!,
	write_responses_as_cfg_grammar_rules(R, S).

write_response_as_cfg_grammar_rule(Response, S) :-
	response_to_nuance_cfg_atom(Response, Response1),
	format(S, '~N( ~w )~n', [Response1]),
	!.

%--------------------------------------------------

grammar_ids_for_units(call, Units, AllGrammarIds) :-
	all_domain_ids_in_units(Units, DomainIds),
	all_lesson_ids_in_units(Units, LessonIds),
	append_list([[default], DomainIds, LessonIds], AllGrammarIds),
	!.
grammar_ids_for_units(questionnaire, Units, AllGrammarIds) :-
	all_domain_ids_in_units(Units, DomainIds),
	all_group_ids_in_units(Units, GroupIds),
	append_list([[default], DomainIds, GroupIds], AllGrammarIds),
	!.
grammar_ids_for_units(translation, Units, AllGrammarIds) :-
	all_domain_ids_in_units(Units, DomainIds),
	append_list([[default], DomainIds], AllGrammarIds),
	!.
grammar_ids_for_units(Type, _Units, AllGrammarIds) :-
	format_lite_error('~N*** Error: bad call: ~w~n', [grammar_ids_for_units(Type, '(... units ...)', AllGrammarIds)]),
	fail.

units_to_l2_nuance_grammars_for_ids([], _Type, _Units, _S).
units_to_l2_nuance_grammars_for_ids([F | R], Type, Units, S) :-
	units_to_l2_nuance_grammar_for_id(F, Type, Units, S),
	!,
	units_to_l2_nuance_grammars_for_ids(R, Type, Units, S).

units_to_l2_nuance_grammar_for_id(Id, Type, Units, S) :-
	all_response_lf_pairs_for_id_in_units(Units, Type, Id, Pairs),
	write_l2_nuance_grammar_preamble_for_id(Id, MainGrammar, S),
	write_response_lf_pairs_as_grammar_rules_for_id(Pairs, S),
	write_l2_nuance_grammar_coda(S),
	length(Pairs, NPairs),
	format('~N--- Written Nuance grammar "~w", ~d rules~n', [MainGrammar, NPairs]),
	!.

units_to_l1_grammars(_Units, []).
units_to_l1_grammars(Units, [LangId-File | R]) :-
	units_to_l1_grammar(Units, LangId, File),
	!,
	units_to_l1_grammars(Units, R).

units_to_l1_grammar(Units, LangId, File) :-
	all_text_prompt_lf_pairs_in_units(Units, LangId, Pairs),
	text_prompt_lf_pairs_to_grammar_rules(Pairs, Rules),
	l1_grammar_preamble(Preamble),
	append(Preamble, Rules, Grammar),
	write_out_grammar(Grammar, 'L1', File).

units_to_lf_translation_answer_file_alist(_Units, []).
units_to_lf_translation_answer_file_alist(Units, [LangId-File | R]) :-
	units_to_lf_translation_answer_file(Units, LangId, File),
	!,
	units_to_lf_translation_answer_file_alist(Units, R).

units_to_lf_translation_answer_file(Units, LangId, File) :-
	all_lf_translation_answer_tuples_in_units(Units, LangId, Tuples),
	write_out_grammar(Tuples, 'LF-translation-answer', File).

units_to_corpus(Units, File) :-
	all_corpus_entries_in_units(Units, CorpusEntries),
	write_out_corpus(CorpusEntries, 'full', File).

units_to_corpus_only_domains(Units, File) :-
	all_corpus_entries_in_units_only_domains(Units, CorpusEntries),
	write_out_corpus(CorpusEntries, 'domains only', File).

units_to_corpus_lf_file(Units, L1Id, CorpusLFFile) :-
	all_corpus_lf_entries_in_units(Units, L1Id, Entries),
	write_out_corpus(Entries, 'LF', CorpusLFFile).

units_to_l2_file_alist(_Units, []).
units_to_l2_file_alist(Units, [LangId-File | R]) :-
	units_to_l2_file(Units, LangId, File),
	!,
	units_to_l2_file_alist(Units, R).

units_to_l2_file(Units, LangId, File) :-
	safe_absolute_file_name(File, AbsFile),
	all_l2_speech_file_words_lines_in_units(Units, LangId, Lines),
	length(Lines, NLines),
	write_atom_list_to_file(Lines, AbsFile),
	format('~N--- Written out speech file (~d lines, language = ~w) ~w~n',
	       [NLines, LangId, AbsFile]).

units_to_tables_file(Units, File) :-
	all_sent_atom_namespace_lf_l1_text_tuples_in_units(Units, Tuples),
	write_out_corpus(Tuples, 'sent-namespace-lf-l1-text', File).

%======================================================================

all_domain_ids_in_units(Units, Ids) :-
	findall(Id,
		(   member(Unit, Units),
		    domain_id_for_unit(Unit, Id)
		),
		Ids0),
	sort(Ids0, Ids).

all_lesson_ids_in_units(Units, Ids) :-
	findall(Id,
		(   member(Unit, Units),
		    lesson_id_for_unit(Unit, Id)
		),
		Ids0),
	sort(Ids0, Ids).

all_group_ids_in_units(Units, Ids) :-
	findall(Id,
		(   member(Unit, Units),
		    group_id_for_unit(Unit, Id)
		),
		Ids0),
	sort(Ids0, Ids).

%======================================================================

:- dynamic domain_and_namespace_for_group/3.

store_group_domain_association(Units) :-
	retractall(domain_for_group(_, _, _)),
	store_group_domain_namespace_association1(Units).

store_group_domain_namespace_association1([]).
store_group_domain_namespace_association1([F | R]) :-
	store_group_domain_namespace_association2(F),
	!,
	store_group_domain_namespace_association1(R).

store_group_domain_namespace_association2(group_unit(_Lines, List)) :-
	member(name=Group, List),
	member(domain=Domain, List),
	member(namespace=Namespace, List),
	assertz(domain_and_namespace_for_group(Group, Domain, Namespace)),
	!.
store_group_domain_namespace_association2(_Other).

group_domain_namespace_table_defined :-
	domain_and_namespace_for_group(_Group, _Domain, _Namespace),
	!.

%======================================================================

all_sent_atom_namespace_lf_l1_text_tuples_in_units(Units, Tuples) :-
	findall(Tuple,
		(   member(Unit, Units),
		    sent_atom_namespace_lf_l1_text_tuple_in_units(Unit, Tuple)
		),
		Tuple0),
	sort(Tuple0, Tuples).

all_response_lf_pairs_in_units(Units, Pairs) :-
	findall(Pair,
		(   member(Unit, Units),
		    response_lf_pair_in_unit(Unit, Pair)
		),
		Pairs0),
	sort(Pairs0, Pairs).

all_response_lf_pairs_for_id_in_units(Units, Type, Id, Pairs) :-
	findall(Pair,
		(   member(Unit, Units),
		    id_is_valid_for_unit(Id, Unit, Type),
		    response_lf_pair_in_unit(Unit, Pair)
		),
		Pairs0),
	sort(Pairs0, Pairs).

id_is_valid_for_unit(Id, Unit, call) :-
	(   Id = default
	;
	    domain_id_for_unit(Unit, Id)
	;
	    lesson_id_for_unit(Unit, Id)
	),
	!.
id_is_valid_for_unit(Id, Unit, questionnaire) :-
	(   Id = default
	;
	    domain_id_for_unit(Unit, Id)
	;
	    group_id_for_unit(Unit, Id)
	),
	!.
id_is_valid_for_unit(Id, Unit, translation) :-
	(   Id = default
	;
	    domain_id_for_unit(Unit, Id)
	),
	!.

sent_atom_namespace_lf_l1_text_tuple_in_units(Unit, Tuple) :-
	namespace_for_unit(Unit, Namespace),
	lf_for_unit(Unit, LF0),
	response_in_unit(Unit, Response),
	LF = [[null, LF0]],
	text_prompt_for_unit(Unit, L1LangId, TextPrompt),
	Tuple = sent_atom_namespace_lf_l1_text(Response, Namespace, LF, L1LangId, TextPrompt).	
sent_atom_namespace_lf_l1_text_tuple_in_units(Unit, Tuple) :-
	namespace_for_unit(Unit, Namespace),
	incorrect_lf_for_unit(Unit, LF0),
	incorrect_response_in_unit(Unit, Response),
	LF = [[null, LF0]],
	text_prompt_for_unit(Unit, L1LangId, TextPrompt0),
	format_to_atom('Incorrect version of: ~w', [TextPrompt0], TextPrompt),
	Tuple = sent_atom_namespace_lf_l1_text(Response, Namespace, LF, L1LangId, TextPrompt).	

response_lf_pair_in_unit(Unit, [Response, LF]) :-
	lf_for_unit(Unit, LF),
	response_in_unit(Unit, Response).
response_lf_pair_in_unit(Unit, [Response, LF]) :-
	incorrect_lf_for_unit(Unit, LF),
	incorrect_response_in_unit(Unit, Response).

response_in_unit(Unit, Response) :-
	Unit = unit(_Lines, List),
	member(responses=Responses, List),
	member(Response, Responses).

incorrect_response_in_unit(Unit, Response) :-
	Unit = unit(_Lines, List),
	member(incorrect_responses=Responses, List),
	member(Response, Responses).

all_l2_speech_file_words_lines_in_units(Units, LangId, Lines) :-
	findall(Line,
		(   member(Unit, Units),
		    l2_speech_line_for_unit(Unit, LangId, Line)
		),
		Lines0),
	sort(Lines0, Lines).

l2_speech_line_for_unit(Unit, LangId, Line) :-
	text_prompt_for_unit(Unit, LangId, Translation),
	speech_file_for_text_prompt(Translation, File),
	format_to_atom('~w ~w', [File, Translation], Line).

all_lf_translation_answer_tuples_in_units(Units, LangId, Tuples) :-
	findall(Tuple,
		(   member(Unit, Units),
		    lf_translation_answer_tuple_in_unit(Unit, LangId, Tuple)
		),
		Tuples0),
	sort(Tuples0, Tuples1),
	correct_fillers_etc_in_tuples(Tuples1, LangId, Units, Tuples).

lf_translation_answer_tuple_in_unit(Unit, LangId,
				    lf_translation_answer([[null, LF]], Translation, Answers, Namespace, Group)) :-
	lf_for_unit(Unit, LF),
	group_id_for_unit(Unit, Group),
	namespace_for_unit(Unit, Namespace),
	text_prompt_for_unit(Unit, LangId, Translation),
	answers_for_unit(Unit, Answers).

correct_fillers_etc_in_tuples([], _LangId, _Units, []).
correct_fillers_etc_in_tuples([F | R], LangId, Units, [F1 | R1]) :-
	correct_fillers_etc_in_tuple(F, LangId, Units, F1),
	!,
	correct_fillers_etc_in_tuples(R, LangId, Units, R1).

correct_fillers_etc_in_tuple(lf_translation_answer(LF, Translation, Answers, Namespace, Group),
			 LangId,
			 Units,
			 lf_translation_answer(LF, Translation, AnswersAlist, Answers1, TranslatedAnswers, Namespace)) :-
	correct_fillers_etc_in_answers(Answers, LangId, Units, Group, AnswersAlist, Answers1, TranslatedAnswers).

correct_fillers_etc_in_answers([], _LangId, _Units, _Group, [], [], []).
correct_fillers_etc_in_answers([Answer=Filler | R], LangId, Units, Group, [Answer1=Filler1 | R1], [Answer1 | R2], [Answer2 | R3]) :-
	printname_for_answer(Answer, Units, Answer1),
	translation_for_answer(Answer, LangId, Units, Answer2),
	fillers_for_group_in_units(Group, Units, Fillers),
	(   member(Filler, Fillers) ->
	    Filler1 = Filler
	;
	    otherwise ->
	    Filler1 = not_filler
	),
	!,
	correct_fillers_etc_in_answers(R, LangId, Units, Group, R1, R2, R3).

printname_for_answer(Answer, Units, Answer1) :-
	member(answer_unit(_Lines, List), Units),
	member(content=Answer, List),
	member(printname=Answer1, List),
	!.
printname_for_answer(Answer, _Units, _Answer1) :-
	format_lite_error('~N*** Error: unable to find print form for answer "~w"~n', [Answer]),
	fail.

translation_for_answer(Answer, LangId, Units, Answer1) :-
	member(answer_unit(_Lines, List), Units),
	member(content=Answer, List),
	member(text(LangId)=Answer1, List),
	!.
translation_for_answer(Answer, LangId, _Units, _Answer1) :-
	format_lite_error('~N*** Error: unable to find ~w translation for answer "~w"~n', [LangId, Answer]),
	fail.

fillers_for_group_in_units(Namespace:Domain:Group, Units, Fillers) :-
	member(group_unit(_Lines, List), Units),
	member(name=Group, List),
	member(namespace=Namespace, List),
	domain_and_namespace_for_group(Group, Domain, Namespace),
	member(fillers=Fillers, List),
	!.
fillers_for_group_in_units(Group, _Units, _Fillers) :-
	format_lite_error('~N*** Error: unable to find fillers for group "~w"~n', [Group]),
	fail.

all_text_prompt_lf_pairs_in_units(Units, LangId, Pairs) :-
	findall(Pair,
		(   member(Unit, Units),
		    text_prompt_lf_pair_in_unit(Unit, LangId, Pair)
		),
		Pairs0),
	sort(Pairs0, Pairs).

text_prompt_lf_pair_in_unit(Unit, LangId, [TextPrompt, LF]) :-
	lf_for_unit(Unit, LF),
	text_prompt_for_unit(Unit, LangId, TextPrompt).
text_prompt_lf_pair_in_unit(Unit, LangId, [TextPrompt, LF]) :-
	incorrect_lf_for_unit(Unit, LF),
	incorrect_text_prompt_for_unit(Unit, LangId, TextPrompt).

text_prompt_for_unit(Unit, LangId, TextPrompt) :-
	Unit = unit(_Lines, List),
	member(text(LangId)=TextPrompt, List).

incorrect_text_prompt_for_unit(Unit, LangId, TextPrompt) :-
	Unit = unit(_Lines, List),
	member(text(LangId)=TextPrompt0, List),
	member(incorrect_responses=Responses, List),
	Responses \== [],
	format_to_atom('~w (incorrect)', [TextPrompt0], TextPrompt).

domain_id_for_unit(Unit, Id) :-
	Unit = unit(_Lines, List),
	member(domain=Domain, List),
	member(namespace=Namespace, List),
	Id = Namespace:Domain.
%domain_id_for_unit(Unit, Id) :-
%	Unit = unit(_Lines, List),
%	member(group=Group, List), 
%	domain_and_namespace_for_group(Group, Domain, Namespace),
%	Id = Namespace:Domain.

lesson_id_for_unit(Unit, Id) :-
	Unit = unit(_Lines, List),
	member(lesson=Lesson, List),
	member(domain=Domain, List),
	member(namespace=Namespace, List),
	Id = Namespace:Domain:Lesson.

answers_for_unit(Unit, Answers) :-
	Unit = unit(_Lines, List),
	member(answers=Answers, List).

group_id_for_unit(Unit, Id) :-
	group_domain_namespace_table_defined,
	!,
	Unit = unit(_Lines, List),
	member(group=Group, List),
	member(namespace=Namespace, List),
	domain_and_namespace_for_group(Group, Domain, Namespace),
	Id = Namespace:Domain:Group.
group_id_for_unit(Unit, Id) :-
	Unit = unit(_Lines, List),
	member(group=Group, List),
	member(domain=Domain, List),
	member(namespace=Namespace, List),
	Id = Namespace:Domain:Group.

lf_for_unit(Unit, LF) :-
	Unit = unit(_Lines, List),
	member(lf=LF, List).

incorrect_lf_for_unit(Unit, IncorrectLF) :-
	Unit = unit(_Lines, List),
	member(incorrect_lf=IncorrectLF, List).

namespace_for_unit(Unit, Namespace) :-
	Unit = unit(_Lines, List),
	member(namespace=Namespace, List).


%======================================================================

response_lf_pairs_to_grammar_rules_for_id(Pairs, LessonId, Rules) :-
	top_grammar_symbol_for_lesson_id(LessonId, TopGrammar),
	response_lf_pairs_to_grammar_rules_for_id1(Pairs, TopGrammar, Rules).

response_lf_pairs_to_grammar_rules_for_id1([], _TopGrammar, []).
response_lf_pairs_to_grammar_rules_for_id1([F | R], TopGrammar, [F1 | R1]) :-
	response_lf_pair_to_grammar_rule_for_id(F, TopGrammar, F1),
	!,
	response_lf_pairs_to_grammar_rules_for_id1(R, TopGrammar, R1).

response_lf_pairs_to_grammar_rules([], []).
response_lf_pairs_to_grammar_rules([F | R], [F1 | R1]) :-
	response_lf_pair_to_grammar_rule(F, F1),
	!,
	response_lf_pairs_to_grammar_rules(R, R1).

response_lf_pair_to_grammar_rule_for_id([Response, LF], TopGrammar, Rule) :-
	split_atom_into_words(Response, Words),
	list_to_comma_list(Words, Body),
	Rule = ( TopGrammar:[sem=[[null, LF]]] --> Body ).

response_lf_pair_to_grammar_rule([Response, LF], Rule) :-
	split_atom_into_words(Response, Words),
	list_to_comma_list(Words, Body),
	Rule = ( top:[sem=[[null, LF]]] --> Body ).

l2_grammar_preamble(Preamble) :-
	Preamble = [ feature_value_space(yes_no,[[y,n]]),
		     feature(dummy, yes_no),
		     category('.MAIN',[gsem]),
		     top_level_category('.MAIN'),
		     category(top,[sem]),
		     ( '.MAIN':[gsem=[value=Sem]] --> top:[sem=Sem] )
		   ].

l2_grammar_preamble_for_id(Preamble, LessonId) :-
	main_grammar_symbol_for_lesson_id(LessonId, MainGrammar),
	top_grammar_symbol_for_lesson_id(LessonId, TopGrammar),
	Preamble = [ category(MainGrammar,[gsem]),
		     category(TopGrammar,[sem]),
		     ( MainGrammar:[gsem=[value=Sem]] --> TopGrammar:[sem=Sem] )
		   ].

global_specialised_l2_grammar_preamble(Preamble) :-
	Preamble = [ feature_value_space(yes_no,[[y,n]]),
		     feature(dummy, yes_no),
		     category('.MAIN',[gsem]),
		     top_level_category('.MAIN')
		   ].

%======================================================================

/*
.MAIN__directions
[
( TOP__directions:v_0 )
     {  < value $v_0 > }
]

TOP__english_course
[
( a discount is acceptable )
     {return( ( ( null a_discount_is_acceptable ) ) )}
...
]
*/

write_l2_nuance_grammar_preamble_for_id(LessonId, MainGrammar, S) :-
	nuance_main_grammar_name_for_id(LessonId, MainGrammar),
	nuance_top_grammar_name_for_id(LessonId, TopGrammar),
	format(S, '~N~n~w~n', [MainGrammar]),
	format(S, '~N[~n', []),
	format(S, '~N( ~w:v_0 )~n', [TopGrammar]),
	format(S, '~N     {  < value $v_0 > }~n', []),
	format(S, '~N]~n', []),
	format(S, '~N~n', []),
	format(S, '~N~w~n', [TopGrammar]),
	format(S, '~N[~n', []),
	!.

write_response_lf_pairs_as_grammar_rules_for_id([], _S) :-
	!.
write_response_lf_pairs_as_grammar_rules_for_id([F | R], S) :-
	write_response_lf_pair_as_grammar_rules_for_id(F, S),
	!,
	write_response_lf_pairs_as_grammar_rules_for_id(R, S).

write_response_lf_pair_as_grammar_rules_for_id([Response, LF], S) :-
	format(S, '~N( ~w )~n', [Response]),
	format(S, '~N     {return( ( ( null ~w ) ) )}~n', [LF]),
	!.	

write_l2_nuance_grammar_coda(S) :-
	format(S, '~N]~n', []),
	!.

%======================================================================

top_grammar_symbol_for_lesson_id(default, 'top') :-
	!.
top_grammar_symbol_for_lesson_id(LessonId, TopGrammar) :-
	format_to_atom('top__~w', [LessonId], TopGrammar).

main_grammar_symbol_for_lesson_id(default, '.MAIN') :-
	!.
main_grammar_symbol_for_lesson_id(LessonId, MainGrammar) :-
	format_to_atom('.MAIN__~w', [LessonId], MainGrammar).

%======================================================================

text_prompt_lf_pairs_to_grammar_rules([], []).
text_prompt_lf_pairs_to_grammar_rules([F | R], [F1 | R1]) :-
	text_prompt_lf_pair_to_grammar_rule(F, F1),
	!,
	text_prompt_lf_pairs_to_grammar_rules(R, R1).

text_prompt_lf_pair_to_grammar_rule([Text, LF], Rule) :-
	Rule = ( top:[sem=[[null, LF]]] --> Text ).

l1_grammar_preamble(Preamble) :-
	Preamble = [ feature_value_space(yes_no,[[y,n]]),
		     feature(dummy, yes_no),
		     category('.MAIN',[gsem]),
		     category(top,[sem]),
		     top_level_category('.MAIN'),
		     ( '.MAIN':[gsem=[value=Sem]] --> top:[sem=Sem] )
		   ].

%======================================================================

all_corpus_entries_in_units(Units, CorpusEntries) :-
	findall(CorpusEntry,
		(   member(Unit, Units),
		    corpus_entry_for_unit(Unit, CorpusEntry)
		),
		CorpusEntries).

all_corpus_entries_in_units_only_domains(Units, CorpusEntries) :-
	findall(CorpusEntry,
		(   member(Unit, Units),
		    corpus_entry_for_unit_only_domains(Unit, CorpusEntry)
		),
		CorpusEntries).

/*

Format example for internalized flat record:

unit(749-757,
     [domain=english_course,
      lesson=airport,
      group=nationality,
      text='Sag : Ich bin aus Deutschland',
      tags=[nationality_contact=germany],
      responses=['i am from germany','i am german']
     ]
    ).

sent('i am from germany', [default, english_course, group(contact)=origin, nationality_contact=germany]).

*/

corpus_entry_for_unit(Unit, CorpusEntry) :-
	Unit = unit(_Lines, List),
	
	(   member(domain=_Domain, List) ->
	    %DomainPart = [default, Domain]
	    DomainPart = [default]
	;
	    otherwise ->
	    DomainPart = [default]
	),
	(   ( member(lesson=Lesson, List), member(group=Group, List) ) ->
	    GroupPart = [Lesson, group(Lesson)=Group]
	;
	    otherwise ->
	    GroupPart = []
	),
	(   member(tags=Tags, List) ->
	    TagsPart = Tags
	;
	    otherwise ->
	    TagsPart = []
	),
	append_list([DomainPart, GroupPart, TagsPart], Annotations),

	member(responses=Responses, List),
	member(Response, Responses),

	CorpusEntry = sent(Response, Annotations).

corpus_entry_for_unit_only_domains(Unit, CorpusEntry) :-
	Unit = unit(_Lines, List),
	
	(   member(domain=_Domain, List) ->
	    %DomainPart = [default, Domain]
	    DomainPart = [default]
	;
	    otherwise ->
	    DomainPart = [default]
	),
	(   member(lesson=Lesson, List) ->
	    LessonPart = [Lesson]
	;
	    otherwise ->
	    LessonPart = []
	),
		
	append_list([DomainPart, LessonPart], Annotations),

	member(responses=Responses, List),
	member(Response, Responses),

	CorpusEntry = sent(Response, Annotations).

%======================================================================

all_corpus_lf_entries_in_units(Units, L1Id, Entries) :-
	findall(Entry,
		(   member(Unit, Units),
		    example_lf_record_for_unit(Unit, L1Id, Entry)
		),
		Entries0),
	sort(Entries0, Entries).

example_lf_record_for_unit(Unit, L1Id, Entry) :-
	lf_for_unit(Unit, LF),
	response_in_unit(Unit, Response),
	text_prompt_for_unit(Unit, L1Id, TextPrompt),
	Entry = lf_for_sent(Response, [[null, LF]], TextPrompt).

%======================================================================

write_out_grammar(Grammar, Tag, File) :-
	safe_absolute_file_name(File, AbsFile),
	list_to_prolog_file_prettyprint_with_encoding(Grammar, AbsFile, 'UTF-16LE'),
	length(Grammar, N),
	format('~N--- Written out ~w grammar (~d rules): ~w~n', [Tag, N, AbsFile]).

write_out_corpus(CorpusEntries, Tag, File) :-
	safe_absolute_file_name(File, AbsFile),
	list_to_prolog_file_with_encoding(CorpusEntries, AbsFile, 'UTF-16LE'),
	length(CorpusEntries, N),
	format('~N--- Written out ~w corpus (~d entries): ~w~n', [Tag, N, AbsFile]).

write_out_translation_rules(TranslationRules, File) :-
	safe_absolute_file_name(File, AbsFile),
	list_to_prolog_file(TranslationRules, AbsFile),
	length(TranslationRules, N),
	format('~N--- Written out translation rules (~d entries): ~w~n', [N, AbsFile]).

write_out_nuance_top_grammars_file(GrammarIds, File) :-
	safe_absolute_file_name(File, AbsFile),
	findall(top_grammar_name(GrammarName),
		(   member(GrammarId, GrammarIds),
		    single_nuance_main_grammar_name_for_id(GrammarId, GrammarName)
		),
		TopGrammarNameRecords),
	list_to_prolog_file(TopGrammarNameRecords, AbsFile),
	length(TopGrammarNameRecords, N),
	format('~N--- Written out top grammar names (~d entries): ~w~n', [N, AbsFile]).

single_nuance_main_grammar_name_for_id(GrammarId, GrammarName) :-
	nuance_main_grammar_name_for_id(GrammarId, GrammarName),
	!.

%======================================================================

speech_file_for_text_prompt(Translation, File) :-
	atom_codes(Translation, Str),
	correct_chars_for_speech_file_name(Str, Str1),
	format_to_atom('~s.flv', [Str1], File).

correct_chars_for_speech_file_name([], []).
correct_chars_for_speech_file_name([F | R], [F | R1]) :-
	okay_char_for_speech_file_name(F),
	!,
	correct_chars_for_speech_file_name(R, R1).
correct_chars_for_speech_file_name([F | R], [F1 | R1]) :-
	substitute_char_for_speech_file_name(F, F1),
	!,
	correct_chars_for_speech_file_name(R, R1).
correct_chars_for_speech_file_name([_F | R], R1) :-
	!,
	correct_chars_for_speech_file_name(R, R1).

okay_char_for_speech_file_name(Char) :-
	lowercase_char(Char),
	\+ accented_char(Char),
	!.
okay_char_for_speech_file_name(0'_).

substitute_char_for_speech_file_name(Char, Char2) :-
	lowercase_uppercase(Char1, Char),
	(   deaccent_char(Char1, Char2) ->
	    true
	;
	    otherwise ->
	    Char2 = Char1
	),
	!.
substitute_char_for_speech_file_name(Char, Char1) :-
	deaccent_char(Char, Char1),
	!.
substitute_char_for_speech_file_name(Char, 0'_) :-
	whitespace_char(Char),
	!.
substitute_char_for_speech_file_name(0'-, 0'_).
substitute_char_for_speech_file_name(0'?, 0'q).

