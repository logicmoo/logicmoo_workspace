% ebl_postprocess.pl

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(ebl_postprocess,
	  [ebl_postprocess/7,
	   ebl_postprocess_and_display_rules/2,
	   add_tag_to_files_in_list/3,
	   get_multiple_grammar_info/2]
      ).

%---------------------------------------------------------------

'LOAD_DYNAMIC_LEXICON_SUPPORT_IF_AVAILABLE'.

:- use_module('$REGULUS/Prolog/regulus_binarise').
:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/regulus_declarations').
:- use_module('$REGULUS/Prolog/dcg2regulus').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).
:- use_module(library(lists)).
:- use_module(library(terms)).
%:- use_module(library(assoc)).

%---------------------------------------------------------------

% InFile should be a file of EBL training output produced by ebl_train.

% ebl_postprocess does the following:
%
% 1. Removes duplicate entries
%
% 2. Binarises rules 
%
% 3. Adds declarations to the beginning of the file. These include dynamic lexicon info if available.

%---------------------------------------------------------------

number_of_rule_examples_to_write_out(1000).

%---------------------------------------------------------------

ebl_postprocess(InFile, MultipleGrammarDeclsFile,
		OutFile, NoBinariseFile, RationalisedCorpusFile,
		FilterPred, ContextUseThreshold) :-
	absolute_file_name(InFile, AbsoluteInFile),
	absolute_file_name(MultipleGrammarDeclsFile, AbsoluteMultipleGrammarDeclsFile),
	absolute_file_name(OutFile, AbsoluteOutFile),
	absolute_file_name(NoBinariseFile, AbsoluteNoBinariseFile),
	absolute_file_name(RationalisedCorpusFile, AbsoluteRationalisedCorpusFile),

	(   file_exists(AbsoluteInFile, read) ->
	    format('~N~nPost-processing raw EBL rules from ~w:~n', [AbsoluteInFile])
	;
	    format2error('~N~nError: could not find file ~w:~n', [AbsoluteInFile]),
	    fail
	),

	get_and_store_multiple_grammar_info(AbsoluteInFile, GrammarTags, AbsoluteMultipleGrammarDeclsFile),

	ebl_postprocess_main(GrammarTags, AbsoluteInFile,
			     AbsoluteOutFile, AbsoluteNoBinariseFile, AbsoluteRationalisedCorpusFile,
			     FilterPred, ContextUseThreshold),
	!.

%---------------------------------------------------------------

ebl_postprocess_and_display_rules([], _Mode) :-
	!.
ebl_postprocess_and_display_rules([F | R], Mode) :-
	ebl_postprocess_and_display_rule(F, Mode),
	!,
	ebl_postprocess_and_display_rules(R, Mode).
ebl_postprocess_and_display_rules(Other, Mode) :-
	format('~N*** Error: bad call: ~w~n',
	       [ebl_postprocess_and_display_rules(Other, Mode)]),
	fail.

ebl_postprocess_and_display_rule(AnnotatedRule, Mode) :-
	AnnotatedRule = rule(PrologRule, _PrologRuleContext, _FullWords, Words, _Tags),
	prolog_dcg_rule_to_regulus(PrologRule, RegulusRule),
	summarise_rule(RegulusRule, RuleSummary),
	format('~N~nRule of form "~w"~n', [RuleSummary]),
	format('~Nderived from ~w~n', [Words]),
	(   Mode = ebl_verbose ->
	    format('~N~n', []),
	    make_ground(RegulusRule),
	    prettyprint(RegulusRule)
	;
	    otherwise ->
	    true
	),
	!.
ebl_postprocess_and_display_rule(AnnotatedRule, Mode) :-
	format('~N*** Error: bad call: ~w~n',
	       [ebl_postprocess_and_display_rule(AnnotatedRule, Mode)]),
	fail.

%---------------------------------------------------------------

add_tag_to_files_in_list([], _Tag, []) :-
	!.
add_tag_to_files_in_list([F | R], Tag, [F1 | R1]) :-
	add_tag_to_file(F, Tag, F1),
	add_tag_to_files_in_list(R, Tag, R1),
	!.
add_tag_to_files_in_list(List, Tag, List1) :-
	format2error('~N~n*** Error: bad call: ~w.~n~n',
		     [add_tag_to_files_in_list(List, Tag, List1)]),
	fail.

%---------------------------------------------------------------

get_multiple_grammar_info(MultipleGrammarDeclsFile, GrammarTags) :-
	prolog_file_to_list(MultipleGrammarDeclsFile, List),
	List = [grammar_tags(GrammarTags)],
	!.
get_multiple_grammar_info(MultipleGrammarDeclsFile, GrammarTags) :-
	format2error('~N~n*** Error: bad call: ~w.~n~n',
		     [get_multiple_grammar_info(MultipleGrammarDeclsFile, GrammarTags)]),
	fail.

%---------------------------------------------------------------

ebl_postprocess_main([], _AbsoluteInFile,
		     _AbsoluteOutFile, _AbsoluteNoBinariseFile, _AbsoluteRationalisedCorpusFile,
		     _FilterPred,_ContextUseThreshold).
ebl_postprocess_main([F | R], AbsoluteInFile,
		     AbsoluteOutFile, AbsoluteNoBinariseFile, AbsoluteRationalisedCorpusFile,
		     FilterPred, ContextUseThreshold) :-
	ebl_postprocess_single_grammar(F, AbsoluteInFile,
				       AbsoluteOutFile, AbsoluteNoBinariseFile, AbsoluteRationalisedCorpusFile,
				       FilterPred, ContextUseThreshold),
	!,
	ebl_postprocess_main(R, AbsoluteInFile,
			     AbsoluteOutFile, AbsoluteNoBinariseFile, AbsoluteRationalisedCorpusFile,
			     FilterPred, ContextUseThreshold).

ebl_postprocess_single_grammar(GrammarTag, AbsoluteInFile,
			       AbsoluteOutFile, AbsoluteNoBinariseFile, AbsoluteRationalisedCorpusFile,
			       FilterPred, ContextUseThreshold) :-
	format('~N~n  -- Reading and classifying raw EBL rules for grammar tag "~w".~n', [GrammarTag]),
	read_learned_grammar_file(AbsoluteInFile, GrammarTag, GrammarUnits),
	add_tag_to_files_in_list([AbsoluteOutFile, AbsoluteNoBinariseFile, AbsoluteRationalisedCorpusFile],
				 GrammarTag,
				 [AbsoluteOutFile1, AbsoluteNoBinariseFile1, AbsoluteRationalisedCorpusFile1]),
	!,
	
	ebl_postprocess1(GrammarUnits, AbsoluteOutFile1, AbsoluteNoBinariseFile1, FilteredGrammarUnits, 
			 FilterPred, ContextUseThreshold),
	!,

	make_rationalised_corpus(FilteredGrammarUnits, AbsoluteRationalisedCorpusFile1).

%---------------------------------------------------------------

read_learned_grammar_file(InFile, GrammarTag, GrammarUnits) :-
	open(InFile, read, S),
	
	empty_assoc_generic(NullGrammarUnitsAssoc),
	read_learned_grammar_stream(S, GrammarTag, NullGrammarUnitsAssoc-GrammarUnitsAssoc, 0-NUnits),
	format('~N--- Read raw specialised grammar, ~d items~n', [NUnits]),
	assoc_generic_to_list(GrammarUnitsAssoc, GrammarUnitsUnsorted),
	sort_grammar_units_by_number_of_examples(GrammarUnitsUnsorted, GrammarUnits),
	count_units_taken_from_lexicon(GrammarUnits),

	close(S).

read_learned_grammar_stream(S, GrammarTag, InAssoc-OutAssoc, NIn-NOut) :-
	read(S, T),
	NNext is NIn + 1,
	!,
	read_learned_grammar_stream1(T, GrammarTag, S, InAssoc-OutAssoc, NNext-NOut).

read_learned_grammar_stream1(end_of_file, _GrammarTag, _S, InAssoc-InAssoc, NOut-NOut) :-
	!.
read_learned_grammar_stream1(AnnotatedRule, GrammarTag, S, InAssoc-OutAssoc, NIn-NOut) :-
	add_annotated_learned_rule_to_assoc_generic(AnnotatedRule, GrammarTag, NIn, InAssoc-NextAssoc),
	!,
	read_learned_grammar_stream(S, GrammarTag, NextAssoc-OutAssoc, NIn-NOut).

%---------------------------------------------------------------

add_annotated_learned_rule_to_assoc_generic(AnnotatedRule, GrammarTag, N, InAssoc-OutAssoc) :-
	AnnotatedRule = rule(PrologRule, PrologRuleContext, FullWords, Words, Tags),
	member(GrammarTag, Tags),
	combine_full_words_and_words(FullWords, Words, WordsInContext),
	make_ground(PrologRule),
	(   get_assoc_generic(PrologRule, InAssoc, [OldFirstOccurrence, OldGeneralisedContext, OldExamples]) ->
	    term_subsumer(PrologRuleContext, OldGeneralisedContext, GeneralisedContext),
	    Examples = [WordsInContext | OldExamples],
	    FirstOccurrence = OldFirstOccurrence
	;
	    otherwise ->
	    GeneralisedContext = PrologRuleContext,
	    Examples = [WordsInContext],
	    FirstOccurrence = N
	),
	put_assoc_generic(PrologRule, InAssoc, [FirstOccurrence, GeneralisedContext, Examples], OutAssoc),
	!.
add_annotated_learned_rule_to_assoc_generic(_AnnotatedRule, _GrammarTag, _N, InAssoc-InAssoc).

%---------------------------------------------------------------

sort_grammar_units_by_number_of_examples(GrammarUnits, GrammarUnitsOut) :-
	tag_grammar_units_by_number_of_examples_and_first_occurrence(GrammarUnits, TaggedGrammarUnits),
	keysort(TaggedGrammarUnits, SortedTaggedGrammarUnits),
	unkey_list_and_remove_indices(SortedTaggedGrammarUnits, SortedGrammarUnits),
	%reverse(SortedGrammarUnits, GrammarUnitsOut),
	SortedGrammarUnits = GrammarUnitsOut,
	!.
sort_grammar_units_by_number_of_examples(GrammarUnits, GrammarUnitsOut) :-
	format2error('~N*** Error: bad call: ~w~n', [sort_grammar_units_by_number_of_examples(GrammarUnits, GrammarUnitsOut)]),
	fail.

tag_grammar_units_by_number_of_examples_and_first_occurrence([], []).
tag_grammar_units_by_number_of_examples_and_first_occurrence([F0 | R], [[MinusNExamples, Index]-F | R1]) :-
	remove_duplicate_examples_in_grammar_unit(F0, F),
	%F0 = F,
	number_of_examples_in_grammar_unit(F, NExamples),
	index_of_first_occurrence_of_grammar_unit(F, Index),
	MinusNExamples is -1 * NExamples,
	!,
	tag_grammar_units_by_number_of_examples_and_first_occurrence(R, R1).

remove_duplicate_examples_in_grammar_unit(Rule-[Index, Context, Examples],
					  Rule-[Index, Context, Examples1]) :-
	safe_remove_duplicates_preserving_order(Examples, Examples1),
	%sort(Examples, Examples1),
	!.
remove_duplicate_examples_in_grammar_unit(F0, F) :-
	format2error('~N*** Error: bad call: ~w~n', [remove_duplicate_examples_in_grammar_unit(F0, F)]),
	fail.

number_of_examples_in_grammar_unit(_Rule-[_Index, _Context, Examples], NExamples) :-
	length(Examples, NExamples),
	!.
number_of_examples_in_grammar_unit(F, NExamples) :-
	format2error('~N*** Error: bad call: ~w~n', [number_of_examples_in_grammar_unit(F, NExamples)]),
	fail.

index_of_first_occurrence_of_grammar_unit(_Rule-[Index, _Context, _Examples], Index) :-
	!.
index_of_first_occurrence_of_grammar_unit(F, Index) :-
	format2error('~N*** Error: bad call: ~w~n', [index_of_first_occurrence_of_grammar_unit(F, Index)]),
	fail.

unkey_list_and_remove_indices([], []).
unkey_list_and_remove_indices([F | R], [F1 | R1]) :-
	unkey_and_remove_index(F, F1),
	!,
	unkey_list_and_remove_indices(R, R1).

unkey_and_remove_index(_Key-(Rule-[_Index, Context, Examples]),
		       Rule-[Context, Examples]) :-
	!.
unkey_and_remove_index(F, F1) :-
	format2error('~N*** Error: bad call: ~w~n', [unkey_and_remove_index(F, F1)]),
	fail.

%---------------------------------------------------------------

count_units_taken_from_lexicon(GrammarUnits) :-
	count_units_taken_from_lexicon1(GrammarUnits, 0-N),
	(   N > 0 ->
	    format('~N~n  -- ~d entries taken from lexicon and not appearing in training examples.~n', [N]) ;
	    true
	),
	!.
count_units_taken_from_lexicon(GrammarUnits) :-
	format2error('~N*** Error: bad call: ~w~n', [count_units_taken_from_lexicon(GrammarUnits)]),
	fail.

count_units_taken_from_lexicon1([], N-N) :-
	!.
count_units_taken_from_lexicon1([F | R], In-Out) :-
	(   unit_taken_from_lexicon(F) ->
	    Next is In + 1 ;
	    Next = In
	),
	!,
	count_units_taken_from_lexicon1(R, Next-Out).

unit_taken_from_lexicon(_Rule-[_Context, Examples]) :-
	Examples = [SingleExample],
	SingleExample = ['[', lexicon, ']'],
	!.			 
	
%---------------------------------------------------------------

ebl_postprocess1(GrammarUnits, AbsoluteOutFile, AbsoluteNoBinariseFile, FilteredGrammarUnits,
		 FilterPred, ContextUseThreshold) :-
	format('~N~n  -- Combining rules and contexts, and converting to Regulus form.~n', []),
	combine_rules_with_contexts_and_convert_to_regulus_in_alist(GrammarUnits, GrammarUnits1, ContextUseThreshold),
	filter_grammar_units(GrammarUnits1, FilterPred, FilteredGrammarUnits),
	current_regulus_declarations_in_external_form(Decls, Schedules0),
	!,
	% This also adds frequency tags to the rules
	split_off_examples_in_grammar_units(FilteredGrammarUnits, SplitFilteredGrammarUnits),

	% We need to copy the grammar units, because the variables get instantiated during writing out
	copy_term(SplitFilteredGrammarUnits, GrammarUnits1a),
	append_list([Decls, Schedules0, GrammarUnits1a], OutGrammarUnitsNoBinarise),
	write_grammar_file(OutGrammarUnitsNoBinarise, AbsoluteNoBinariseFile),
	format('~N~nSpecialised Regulus grammar without binarisation transform written to ~w.~n~n', [AbsoluteNoBinariseFile]),
	
	format('~N  -- Performing binarisation transform.~n', []),
	binarise_grammar_alist(SplitFilteredGrammarUnits, GrammarUnits2, NewCats, NewFeats),
	adjust_schedules_for_new_features(Schedules0, NewFeats, Schedules),
	!,
	
	append_list([Decls, NewFeats, NewCats, Schedules, GrammarUnits2], OutGrammarUnits),

	write_grammar_file(OutGrammarUnits, AbsoluteOutFile),
	format('~N~nSpecialised Regulus grammar written to ~w.~n~n', [AbsoluteOutFile]),
	!.
ebl_postprocess1(_InGrammarUnits, _AbsoluteOutFile, _ContextUseThreshold) :-
	format2error('~N~n*** Error in ebl_postprocess1.~n~n', []),
	fail.

%---------------------------------------------------------------

:- dynamic tmp_grammar_unit/1.

/*
combine_rules_with_contexts_and_convert_to_regulus_in_alist([], [], _ContextUseThreshold).
combine_rules_with_contexts_and_convert_to_regulus_in_alist([F | R], [F1 | R1], ContextUseThreshold) :-
	combine_rule_with_context_and_convert_to_regulus(F, F1, ContextUseThreshold),
	!,
	combine_rules_with_contexts_and_convert_to_regulus_in_alist(R, R1, ContextUseThreshold).
*/

combine_rules_with_contexts_and_convert_to_regulus_in_alist(UnitsIn, UnitsOut, ContextUseThreshold) :-
	retractall(tmp_grammar_unit(_)),
	combine_rules_with_contexts_and_convert_to_regulus_in_alist1(UnitsIn, ContextUseThreshold),
	findall(Unit, tmp_grammar_unit(Unit), UnitsOut).

combine_rules_with_contexts_and_convert_to_regulus_in_alist1(Units, ContextUseThreshold) :-
	member(Unit, Units),
	combine_rule_with_context_and_convert_to_regulus(Unit, UnitOut, ContextUseThreshold),
	assertz(tmp_grammar_unit(UnitOut)),
	fail.
combine_rules_with_contexts_and_convert_to_regulus_in_alist1(_Units, _ContextUseThreshold) :-
	format('~N  -- Rules combined with contexts~n', []).

combine_rule_with_context_and_convert_to_regulus(GroundPrologRule-[GeneralisedContext, Examples], RegulusRule-Examples1, ContextUseThreshold) :-
	unground(GroundPrologRule, PrologRule),
	length(Examples, NExamples),
	(   NExamples >= ContextUseThreshold ->
	    combine_rule_with_context(PrologRule, GeneralisedContext) ;
	    true
	),
	prolog_dcg_rule_to_regulus(PrologRule, RegulusRule),
	reverse(Examples, Examples1),
	!.
combine_rule_with_context_and_convert_to_regulus(F, F1, ContextUseThreshold) :-
	format2error('~N*** Error: bad call: ~w~n', [combine_rule_with_context_and_convert_to_regulus(F, F1, ContextUseThreshold)]),
	fail.

combine_rule_with_context(PrologRule, GeneralisedContext) :-
	PrologRule = GeneralisedContext,
	!.
combine_rule_with_context(PrologRule, GeneralisedContext) :-
	format2error('~N*** Error: bad call: ~w~n', [combine_rule_with_context(PrologRule, GeneralisedContext)]),
	fail.

%---------------------------------------------------------------

filter_grammar_units(GrammarUnits, null, GrammarUnits) :-
	!.
filter_grammar_units(_GrammarUnits, FilterPred, _FilteredGrammarUnits) :-
	\+ current_predicate(user:FilterPred/1),
	!,
	format2error('~N~n*** Error: predicate "~w/1" is not defined. Unable to filter specialised grammar.~n~n', [FilterPred]),
	fail.
filter_grammar_units(GrammarUnits, FilterPred, FilteredGrammarUnits) :-
	format('~N~n  -- Filtering using predicate "~w/1"~n', [FilterPred]),
	filter_grammar_units1(GrammarUnits, FilterPred, FilteredGrammarUnits, 0-NFiltered),
	format('~N  -- ~d specialised rules filtered out~n', [NFiltered]),
	!.
filter_grammar_units(_Units, _FilterPred, _Units1) :-
	format2error('~N*** Error: bad call to filter_grammar_units/3~n', []),
	fail.

filter_grammar_units1([], _FilterPred, [], Cin-Cin).
filter_grammar_units1([F | R], FilterPred, [F1 | R1], Cin-Cout) :-
	filter_grammar_unit(F, FilterPred, F1, Cin-Cnext),
	!,
	filter_grammar_units1(R, FilterPred, R1, Cnext-Cout).

filter_grammar_unit(F, null, F, Cin-Cin) :-
	!.
filter_grammar_unit(Rule-Examples, FilterPred, ignore(Rule-Examples), Cin-Cout) :-
	Call =.. [FilterPred, Rule],
	call(user:Call),
	Cout is Cin + 1,
	!.
filter_grammar_unit(F, _FilterPred, F, Cin-Cin).

%---------------------------------------------------------------

split_off_examples_in_grammar_units([], []) :-
	!.
split_off_examples_in_grammar_units([GrammarUnit | R], [ExamplesInfo, Rule | R1]) :-
	split_off_examples_in_grammar_unit(GrammarUnit, ExamplesInfo, Rule),
	!,
	split_off_examples_in_grammar_units(R, R1).

split_off_examples_in_grammar_unit(ignore(GrammarUnit), ignore(ExamplesInfo), ignore(Rule)) :-
	split_off_examples_in_grammar_unit(GrammarUnit, ExamplesInfo, Rule),
	!.
split_off_examples_in_grammar_unit(GrammarUnit, ExamplesInfo, LabelledRule) :-
	GrammarUnit = Rule-Examples,
	summarise_rule(Rule, RuleSummary),
	length(Examples, NExamples),
	ExamplesInfo = examples(RuleSummary, Examples),
	LabelledRule = frequency_labelled_rule(NExamples, Rule),
	!.
split_off_examples_in_grammar_unit(GrammarUnit, ExamplesInfo, Rule) :-
	format2error('~N*** Error: bad call: ~w~n', [split_off_examples_in_grammar_unit(GrammarUnit, ExamplesInfo, Rule)]),
	fail.

%---------------------------------------------------------------

current_regulus_declarations_in_external_form(Decls, Schedules) :-
	findall(feature_value_space(Name, Space), feature_value_space0(Name, Space), FeatureValueSpaces),
	findall(feature(Feat, Space), feature(Feat, Space), Features),
	findall(category(CatName, Feats), category_internal(CatName, Feats), Categories),
	findall(top_level_category(Cat), top_level_category(Cat), TopLevelCategories),
	findall(feature_instantiation_schedule(Schedule), feature_instantiation_schedule(Schedule), Schedules),
	findall(lexical_feature_default(Feat, Default), lexical_feature_default(Feat, Default), LexFeatDefaults),

	current_dynamic_lexicon_entries(DynamicLexiconEntries),
	
	append_list([FeatureValueSpaces, Features, Categories, TopLevelCategories, LexFeatDefaults, DynamicLexiconEntries],
		    Decls).

%---------------------------------------------------------------

/*
Originally, we added all the new features as one group, at the end. This saves time when it works.

Unfortunately, it turns out that there are cases where it's too optimistic, so back off to the
more conservative strategy of adding each new feature as a separate group on its own.
*/
adjust_schedules_for_new_features([], _NewFeats, []) :-
	!.
adjust_schedules_for_new_features([feature_instantiation_schedule(Schedule)], 
				  NewFeatDecls,
				  [feature_instantiation_schedule(Schedule1)]) :-
	%findall(NewFeat, member(feature(NewFeat, _SpaceId), NewFeatDecls), NewFeats),
	%append(Schedule, [NewFeats], Schedule1),
	findall([NewFeat],
		member(feature(NewFeat, _SpaceId), NewFeatDecls),
		NewFeats),
	append(Schedule, NewFeats, Schedule1),
	!.
adjust_schedules_for_new_features(Schedules0, NewFeatDecls, Schedules) :-
	format2error('~NError: bad call: ~w~n', [adjust_schedules_for_new_features(Schedules0, NewFeatDecls, Schedules)]),
	fail.

%---------------------------------------------------------------

write_grammar_file(OutGrammarUnits, OutFile) :-
	open_regulus_file(OutFile, write, S),
	write_grammar_stream(OutGrammarUnits, S, 0-NTopLevelRules),
	warn_if_more_than_one_top_level_rule(NTopLevelRules),
	close(S).

warn_if_more_than_one_top_level_rule(NTopLevelRules) :-
	NTopLevelRules > 1,
	!,
	nl, 
	format('~N*** WARNING: ~d top-level rules found in specialised grammar.', [NTopLevelRules]),
	format('~N*** Having more than one top-level rule may cause problems for the parser.~n', []),
	format('~N*** The easiest way to avoid this is probably to reorganise the operationality criteria.~n', []).
warn_if_more_than_one_top_level_rule(_NTopLevelRules).

write_grammar_stream([], _S, NTopIn-NTopIn).
write_grammar_stream([F | R], S, NTopIn-NTopOut) :-
	write_grammar_item_to_stream(F, S, NTopIn-NTopNext),
	!,
	write_grammar_stream(R, S, NTopNext-NTopOut).

write_grammar_item_to_stream(ignore(examples(RuleSummary, Examples)), S, NTopIn-NTopIn) :-
	format(S, '~N% *** RULE IGNORED ***~n', []),
	write_rule_examples_to_stream(RuleSummary, Examples, S),
	!.
write_grammar_item_to_stream(examples(RuleSummary, Examples), S, NTopIn-NTopOut) :-
	write_rule_examples_to_stream(RuleSummary, Examples, S),
	(   is_top_level_rule_summary(RuleSummary) ->
	    NTopOut is NTopIn + 1 ;
	    NTopOut = NTopIn
	),
	!.
write_grammar_item_to_stream(F, S, NTopIn-NTopIn) :-
	numbervars(F, 0, _),
	write_grammar_item_to_stream1(F, S),
	!.

is_top_level_rule_summary((HeadCat --> _BodySummary)) :-
	top_level_category(HeadCat),
	!.

% Unfortunately, this runs into a subtle bug in the SICStus parser, if we have a hyphen ('--')
% or something similar in the RHS. Need to work around it...
%write_grammar_item_to_stream1(F, S) :-
%	format(S, '~N~q.~n~n', [F]),
%	!.

write_grammar_item_to_stream1(ignore(F), S) :-
	format(S, '~N%', []),
	write_grammar_item_to_stream2(F, S),
	format(S, '.~n~n', []),
	!.
write_grammar_item_to_stream1(frequency_labelled_rule(NExamples, Rule), S) :-
	format(S, '~Nfrequency_labelled_item(~d,(~n', [NExamples]),
	write_grammar_item_to_stream2(Rule, S),
	format(S, '~n)).~n~n', []),
	!.
write_grammar_item_to_stream1(F, S) :-
	format(S, '~N', []),
	write_grammar_item_to_stream2(F, S),
	format(S, '.~n~n', []),
	!.

write_grammar_item_to_stream2((H --> B), S) :-
	write_grammar_item_to_stream2(H, S),
	format(S, ' --> ', []),
	write_grammar_item_to_stream2(B, S),
	!.
write_grammar_item_to_stream2((L, R), S) :-
	write_grammar_item_to_stream2(L, S),
	format(S, ', ', []),
	write_grammar_item_to_stream2(R, S),
	!.
write_grammar_item_to_stream2(Atom, S) :-
	problem_atom_for_write_grammar_item_to_stream(Atom),
	atom_codes(Atom, Codes),
	add_escape_codes_in_string(Codes, Codes1),
	format(S, '\'~s\'', [Codes1]),
	!.
write_grammar_item_to_stream2(Other, S) :-
	format(S, '~q', [Other]),
	!.

add_escape_codes_in_string([], []).
add_escape_codes_in_string([F | R], [0'\\, F | R1]) :-
	code_requiring_escape(F),
	!,
	add_escape_codes_in_string(R, R1).
add_escape_codes_in_string([F | R], [F | R1]) :-
	!,
	add_escape_codes_in_string(R, R1).

code_requiring_escape(0'\').

% Always put quotes around atoms - a bit ugly, but otherwise we
% get weird problems with some character sets.
problem_atom_for_write_grammar_item_to_stream(Atom) :-
	atom(Atom),
	!.
problem_atom_for_write_grammar_item_to_stream('--').
problem_atom_for_write_grammar_item_to_stream('-').

write_rule_examples_to_stream(RuleSummary, Examples, S) :-
	length(Examples, NExamples),
	number_of_rule_examples_to_write_out(Limit),
	(   NExamples = 1 ->
	    format(S, '~N% Single example of rule of form "~w"~n', [RuleSummary]) ;
	    format(S, '~N% ~d examples of rule of form "~w"~n', [NExamples, RuleSummary])
	),
	write_rule_examples_to_stream1(Examples, S, Limit),
	!.
write_rule_examples_to_stream(RuleSummary, Examples, S) :-
	format2error('*** Error: bad call: ~w~n', [write_rule_examples_to_stream(RuleSummary, Examples, S)]),
	fail.

write_rule_examples_to_stream1([], S, _Count) :-
	!,
	format(S, '~n', []).
write_rule_examples_to_stream1([Example], S, Count) :-
	Count =< 0,
	!,
	write_rule_example_to_stream(Example, S),
	format(S, '~n', []).
write_rule_examples_to_stream1(Examples, S, Count) :-
	Count =< 0,
	!,
	length(Examples, NLeft),
	(   NLeft > 0 ->
	    format(S, '~N% (~d more examples)~n~n', [NLeft]) ;
	    true
	).
write_rule_examples_to_stream1([F | R], S, Count) :-
	write_rule_example_to_stream(F, S),
	Count1 is Count - 1,
	!,
	write_rule_examples_to_stream1(R, S, Count1).

write_rule_example_to_stream(Example, S) :-
	format(S, '~N% ', []),
        write_rule_example_to_stream1(Example, S).

write_rule_example_to_stream1([], S) :-
	format(S, '~n', []),
	!.
write_rule_example_to_stream1([F | R], S) :-
	format(S, '~w ', [F]),
	!,
	write_rule_example_to_stream1(R, S).

%---------------------------------------------------------------

combine_full_words_and_words(FullWords, Words, WordsInContext) :-
	combine_full_words_and_words1(FullWords, Words, WordsInContext),
	!.
combine_full_words_and_words(FullWords, Words, WordsInContext) :-
	format2error('~N*** Error: bad call: ~w~n', [combine_full_words_and_words(FullWords, Words, WordsInContext)]),
	fail.

combine_full_words_and_words1(FullWords, Words, WordsInContext) :-
	append(Words, Rest, FullWords),
	!,
	append_list([['['], Words, [']'], Rest], WordsInContext).
combine_full_words_and_words1([F | RestFullWords], Words, [F | RestWordsInContext]) :-
	!,
	combine_full_words_and_words1(RestFullWords, Words, RestWordsInContext).

%=====================================================================================================

make_rationalised_corpus(GrammarUnits, AbsoluteRationalisedCorpusFile) :-
	format('~N  -- Creating rationalised corpus.~n', []),

	strip_brackets_and_assign_multiplicities(GrammarUnits, GrammarUnits1),
	find_true_multiplicities(GrammarUnits1, MultiplicitiesAssoc),

	grammar_units_to_corpus_assoc_generic(GrammarUnits1, MultiplicitiesAssoc, CorpusAssoc),
	corpus_assoc_generic_to_rationalised_corpus_list(CorpusAssoc, SizeOfCorpus, RationalisedCorpusList),
	
	write_rationalised_corpus_to_file(RationalisedCorpusList, SizeOfCorpus, AbsoluteRationalisedCorpusFile),
	format('~N~nRationalised corpus file written to ~w.~n~n', [AbsoluteRationalisedCorpusFile]),
	!.
make_rationalised_corpus(_GrammarUnits, _AbsoluteRationalisedCorpusFile) :-
	format2error('~N~n*** Error in make_rationalised_corpus.~n~n', []),
	fail.

%---------------------------------------------------------------

strip_brackets_and_assign_multiplicities([], []) :-
	!.
strip_brackets_and_assign_multiplicities([ignore(_F) | R], R1) :-
	strip_brackets_and_assign_multiplicities(R, R1),
	!.
strip_brackets_and_assign_multiplicities([F | R], [F1 | R1]) :-
	strip_brackets_and_assign_multiplicities1(F, F1),
	strip_brackets_and_assign_multiplicities(R, R1),
	!.
strip_brackets_and_assign_multiplicities(_GrammarUnits, _GrammarUnits1) :-
	format2error('~N~n*** Error in strip_brackets_and_assign_multiplicities.~n~n', []),
	fail.

strip_brackets_and_assign_multiplicities1(Rule-ExamplesIn, Rule-ExamplesOut) :-
	strip_brackets_from_examples_list(ExamplesIn, Examples1),
	assign_multiplicities(Examples1, ExamplesOut),
	!.
strip_brackets_and_assign_multiplicities1(F, F1) :-
	format2error('~N~n*** Error: bad call: ~w.~n~n', [strip_brackets_and_assign_multiplicities1(F, F1)]),
	fail.
	
strip_brackets_from_examples_list([], []) :-
	!.
strip_brackets_from_examples_list([F | R], [F1 | R1]) :-
	strip_brackets_from_example(F, F1),
	strip_brackets_from_examples_list(R, R1),
	!.
strip_brackets_from_examples_list(X, Y) :-
	format2error('~N~n*** Error: bad call: ~w.~n~n', [strip_brackets_from_examples_list(X, Y)]),
	fail.
	
%---------------------------------------------------------------

strip_brackets_from_example([], []) :-
	!.
strip_brackets_from_example(['[' | R], R1) :-
	!,
	strip_brackets_from_example(R, R1).
strip_brackets_from_example([']' | R], R1) :-
	!,
	strip_brackets_from_example(R, R1).
strip_brackets_from_example([F | R], [F | R1]) :-
	!,
	strip_brackets_from_example(R, R1).

%---------------------------------------------------------------

find_true_multiplicities(GrammarUnits, MultiplicitiesAssoc) :-
	empty_assoc_generic(EmptyAssoc),
	find_true_multiplicities1(GrammarUnits, EmptyAssoc-MultiplicitiesAssoc),
	!.
find_true_multiplicities(_GrammarUnits, _MultiplicitiesAssoc) :-
	format2error('~N~n*** Error in find_true_multiplicities.~n~n', []),
	fail.

find_true_multiplicities1([], Assoc-Assoc) :-
	!.
find_true_multiplicities1([F | R], AssocIn-AssocOut) :-
	find_true_multiplicities2(F, AssocIn-AssocNext),
	!,
	find_true_multiplicities1(R, AssocNext-AssocOut).

find_true_multiplicities2(Unit, AssocIn-AssocOut) :-
	Unit = _Rule-Examples,
	find_true_multiplicities3(Examples, AssocIn-AssocOut).

find_true_multiplicities3([], AssocIn-AssocIn).
find_true_multiplicities3([F | R], AssocIn-AssocOut) :-
	find_true_multiplicities4(F, AssocIn-AssocNext),
	!,
	find_true_multiplicities3(R, AssocNext-AssocOut).

find_true_multiplicities4(F, AssocIn-AssocOut) :-	
	F = multiexample(Words, Multiplicity),
	(   get_assoc_generic(Words, AssocIn, OldMultiplicity) ->
	    NewMultiplicity is min(OldMultiplicity, Multiplicity) ;
	    NewMultiplicity = Multiplicity
	),
	put_assoc_generic(Words, AssocIn, NewMultiplicity, AssocOut),
	!.
find_true_multiplicities4(F, Assocs) :-	
	format2error('~N~n*** Error: bad call: ~w.~n~n', [find_true_multiplicities4(F, Assocs)]),
	fail.

%---------------------------------------------------------------

grammar_units_to_corpus_assoc_generic(GrammarUnits, MultiplicitiesAssoc, CorpusAssoc) :-
	empty_assoc_generic(EmptyAssoc),
	grammar_units_to_corpus_assoc_generic1(GrammarUnits, 1, MultiplicitiesAssoc, EmptyAssoc-CorpusAssoc),
	!.
grammar_units_to_corpus_assoc_generic(_GrammarUnits, _MultiplicitiesAssoc, _CorpusAssoc) :-
	format2error('~N~n*** Error in grammar_units_to_corpus_assoc_generic.~n~n', []),
	fail.

grammar_units_to_corpus_assoc_generic1([], _N, _MultiplicitiesAssoc, AssocIn-AssocIn) :-
	!.
grammar_units_to_corpus_assoc_generic1([F | R], N, MultiplicitiesAssoc, AssocIn-AssocOut) :-
	update_corpus_assoc_generic_from_grammar_unit(F, N, MultiplicitiesAssoc, AssocIn-AssocNext),
	N1 is N + 1,
	!,
	grammar_units_to_corpus_assoc_generic1(R, N1, MultiplicitiesAssoc, AssocNext-AssocOut).
grammar_units_to_corpus_assoc_generic1(List, N, MultiplicitiesAssoc, AssocIn-AssocOut) :-
	format2error('~N~n*** Error: bad call: ~w.~n~n', [grammar_units_to_corpus_assoc_generic1(List, N, MultiplicitiesAssoc, AssocIn-AssocOut)]),
	fail.

update_corpus_assoc_generic_from_grammar_unit(Unit, N, MultiplicitiesAssoc, AssocIn-AssocOut) :-
	Unit = _Rule-Examples,
	get_true_example_count(Examples, MultiplicitiesAssoc, 0-NTrueExamples),
	Data = [NTrueExamples, N, _Tag],
	update_corpus_assoc_generic_from_examples(Examples, Data, MultiplicitiesAssoc, AssocIn-AssocOut),
	!.
update_corpus_assoc_generic_from_grammar_unit(Unit, N, MultiplicitiesAssoc, AssocIn-AssocOut) :-
	format2error('~N~n*** Error: bad call: ~w.~n~n', [update_corpus_assoc_generic_from_grammar_unit(Unit, N, MultiplicitiesAssoc, AssocIn-AssocOut)]),
	fail.

update_corpus_assoc_generic_from_examples([], _Data, _MultiplicitiesAssoc, AssocIn-AssocIn) :-
	!.
update_corpus_assoc_generic_from_examples([F | R], Data, MultiplicitiesAssoc, AssocIn-AssocOut) :-
	update_corpus_assoc_generic_from_example(F, Data, MultiplicitiesAssoc, AssocIn-AssocNext),
	!,
	update_corpus_assoc_generic_from_examples(R, Data, MultiplicitiesAssoc, AssocNext-AssocOut).

update_corpus_assoc_generic_from_example(Example, Data, MultiplicitiesAssoc, AssocIn-AssocOut) :-
	Example =  multiexample(Words, _Multiplicity),
	get_assoc_generic(Words, MultiplicitiesAssoc, CorrectMultiplicity),
	CorrectedExample = multiexample(Words, CorrectMultiplicity),
	(   get_assoc_generic(CorrectedExample, AssocIn, OldData) ->
	    true ;
	    OldData = []
	),
	put_assoc_generic(CorrectedExample, AssocIn, [Data | OldData], AssocOut),
	!.
update_corpus_assoc_generic_from_example(Example, Data, MultiplicitiesAssoc, AssocIn-AssocOut) :-
	format2error('~N~n*** Error: bad call: ~w.~n~n', [update_corpus_assoc_generic_from_example(Example, Data, MultiplicitiesAssoc, AssocIn-AssocOut)]),
	fail.

get_true_example_count([], _MultiplicitiesAssoc, NExamples-NExamples).
get_true_example_count([F | R], MultiplicitiesAssoc, NIn-NOut) :-
	F = multiexample(Words, _Multiplicity),
	get_assoc_generic(Words, MultiplicitiesAssoc, CorrectMultiplicity),
	NNext is NIn + CorrectMultiplicity,
	!,
	get_true_example_count(R, MultiplicitiesAssoc, NNext-NOut).

%---------------------------------------------------------------

corpus_assoc_generic_to_rationalised_corpus_list(CorpusAssoc, NCorpusExamples, RationalisedCorpusList) :-
	assoc_generic_to_list(CorpusAssoc, CorpusAlist),
	count_corpus_examples_in_corpus_alist(CorpusAlist, 0-NCorpusExamples),
	corpus_alist_to_rationalised_corpus_list(CorpusAlist, RationalisedCorpusList),
	!.
corpus_assoc_generic_to_rationalised_corpus_list(CorpusAssoc, RationalisedCorpusList) :-
	format2error('~N~n*** Error: bad call: ~w.~n~n', [corpus_assoc_generic_to_rationalised_corpus_list(CorpusAssoc, RationalisedCorpusList)]),
	fail.

corpus_alist_to_rationalised_corpus_list(CorpusAlist, RationalisedCorpusList) :-
	ground(CorpusAlist),
	RationalisedCorpusList = [],
	!.
corpus_alist_to_rationalised_corpus_list(CorpusAlist, [BestExample-[Score, CorpusExamplesCovered] | R]) :-
	highest_scoring_example_in_corpus_alist(CorpusAlist, BestExample, Score, Data),
	make_ground(Data),
	count_examples_covered_in_corpus_alist(CorpusAlist, 0-CorpusExamplesCovered),
	!,
	corpus_alist_to_rationalised_corpus_list(CorpusAlist, R).

highest_scoring_example_in_corpus_alist(CorpusAlist, BestExample, Score, Data) :-
	score_examples_in_corpus_alist(CorpusAlist, ScoredExamples),
	keysort(ScoredExamples, SortedExamples),
	reverse(SortedExamples, SortedExamples1),
	SortedExamples1 = [Score-[BestExample, Data] | _Rest],
	!.
highest_scoring_example_in_corpus_alist(CorpusAlist, BestExample, Score, Data) :-
	format2error('~N~n*** Error: bad call: ~w.~n~n', [highest_scoring_example_in_corpus_alist(CorpusAlist, BestExample, Score, Data)]),
	fail.

score_examples_in_corpus_alist([], []).
score_examples_in_corpus_alist([F | R], [F1 | R1]) :-
	score_example_in_corpus_alist(F, F1),
	!,
	score_examples_in_corpus_alist(R, R1).

score_example_in_corpus_alist(Multiexample-Data, Score-[Multiexample, Data]) :-
	score_example_in_corpus_alist1(Data, 0-Score),
	!.
score_example_in_corpus_alist(F, F1) :-
	format2error('~N~n*** Error: bad call: ~w.~n~n', [score_example_in_corpus_alist(F, F1)]),
	fail.

score_example_in_corpus_alist1([], Score-Score) :-
	!.
score_example_in_corpus_alist1([[NExamples, _N, Tag] | R], ScoreIn-ScoreOut) :-
	(   var(Tag) ->
	    ScoreNext is ScoreIn + NExamples ;
	    ScoreNext is ScoreIn
	),
	!,
	score_example_in_corpus_alist1(R, ScoreNext-ScoreOut).

count_examples_covered_in_corpus_alist([], Covered-Covered) :-
	!.
count_examples_covered_in_corpus_alist([Example-Data | R], CoveredIn-CoveredOut) :-
	Example = multiexample(_Words, Multiplicity),
	(   ground(Data) ->
	    CoveredNext is CoveredIn + Multiplicity ;
	    CoveredNext is CoveredIn
	),
	count_examples_covered_in_corpus_alist(R, CoveredNext-CoveredOut),
	!.
count_examples_covered_in_corpus_alist(CorpusAlist, CorpusExamplesCovered) :-
	format2error('~N~n*** Error: bad call: ~w.~n~n', [count_examples_covered_in_corpus_alist(CorpusAlist, CorpusExamplesCovered)]),
	fail.

count_corpus_examples_in_corpus_alist([], In-In).
count_corpus_examples_in_corpus_alist([Example-_Data | R], In-Out) :-
	Example = multiexample(_Words, Multiplicity),
	Next is In + Multiplicity,
	!,
	count_corpus_examples_in_corpus_alist(R, Next-Out).

%---------------------------------------------------------------

write_rationalised_corpus_to_file(RationalisedCorpusList, NCorpusExamples, File) :-
	open_regulus_file(File, write, S),
	write_rationalised_corpus_to_stream(RationalisedCorpusList, NCorpusExamples, S, 1),
	close(S),
	!.
write_rationalised_corpus_to_file(RationalisedCorpusList, File) :-
	format2error('~N~n*** Error: bad call: ~w.~n~n', [write_rationalised_corpus_to_file(RationalisedCorpusList, File)]),
	fail.

write_rationalised_corpus_to_stream([], _NCorpusExamples, _S, _N) :-
	!.
write_rationalised_corpus_to_stream([Example-[Score, Covered] | R], NCorpusExamples, S, N) :-
	write_rationalised_corpus_item_to_stream(N, Example, Score, Covered, NCorpusExamples, S),
	N1 is N + 1,
	!,
	write_rationalised_corpus_to_stream(R, NCorpusExamples, S, N1).

write_rationalised_corpus_item_to_stream(N, multiexample(Example, Multiplicity), Score, Covered, NCorpusExamples, S) :-
	Percentage is ( Covered * 100.0 ) / NCorpusExamples,
	join_with_spaces(Example, ExampleAtom),

	format(S, '~N%Example ~d (~d copies). ~d new rule occurrences covered. Corpus coverage: ~d/~d (~2f%)~n',
	       [N, Multiplicity, Score, Covered, NCorpusExamples, Percentage]),
	format(S, '~N~q.~n~n', [sent(ExampleAtom)]),
	!.
write_rationalised_corpus_item_to_stream(Example, Score, Covered, NCorpusExamples, S) :-
	format2error('~N~n*** Error: bad call: ~w.~n~n', [write_rationalised_corpus_item_to_stream(Example, Score, Covered, NCorpusExamples, S)]),
	fail.

/*
total_score_for_list([], Score-Score) :-
	!.
total_score_for_list([_Example-Score | R], In-Out) :-
	Next is In + Score,
	total_score_for_list(R, Next-Out),
	!.
total_score_for_list(L, Score) :-
	format2error('~N~n*** Error: bad call: ~w.~n~n', [total_score_for_list(L, Score)]),
	fail.
*/

%---------------------------------------------------------------

get_and_store_multiple_grammar_info(RulesFile, GrammarTags, MultipleGrammarDeclsFile) :-
	open(RulesFile, read, S),
	
	read_grammar_tags_from_learned_grammar_file(S, []-GrammarTags),

	close(S),

	list_to_prolog_file([grammar_tags(GrammarTags)], MultipleGrammarDeclsFile),
	!.
get_and_store_multiple_grammar_info(RulesFile, GrammarTags, MultipleGrammarDeclsFile) :-
	format2error('~N~n*** Error: bad call: ~w.~n~n',
		     [get_and_store_multiple_grammar_info(RulesFile, GrammarTags, MultipleGrammarDeclsFile)]),
	fail.

read_grammar_tags_from_learned_grammar_file(S, InTags-OutTags) :-
	read(S, T),
	read_grammar_tags_from_learned_grammar_file1(T, S, InTags-OutTags).

read_grammar_tags_from_learned_grammar_file1(end_of_file, _S, InTags-InTags).
read_grammar_tags_from_learned_grammar_file1(T, S, InTags-OutTags) :-
	add_tag_from_rule_to_tags(T, InTags-NextTags),
	!,
	read_grammar_tags_from_learned_grammar_file(S, NextTags-OutTags).

add_tag_from_rule_to_tags(AnnotatedRule, InTags-OutTags) :-
	AnnotatedRule = rule(_PrologRule, _PrologRuleContext, _FullWords, _Words, Tags),
	append(InTags, Tags, OutTags0),
	sort(OutTags0, OutTags),
	!.
add_tag_from_rule_to_tags(AnnotatedRule, Tags) :-
	format2error('~N~n*** Error: bad call: ~w.~n~n',
		     [add_tag_from_rule_to_tags(AnnotatedRule, Tags)]),
	fail.

%---------------------------------------------------------------

assign_multiplicities([], []) :-
	!.
assign_multiplicities(In, Out) :-
	assign_unique_tags(In, Tagged, 0),
	keysort(Tagged, SortedTagged),
	remove_unique_tags(SortedTagged, SortedUntagged),
	SortedUntagged = [First | Rest],
	assign_multiplicities1(Rest, Out, First, 1),
	!.
assign_multiplicities(In, Out) :-
	format2error('~N~n*** Error: bad call: ~w.~n~n', [assign_multiplicities(In, Out)]),
	fail.

assign_unique_tags([], [], _N).
assign_unique_tags([F | R], [F-N | R1], N) :-
	N1 is N + 1,
	!,
	assign_unique_tags(R, R1, N1).

remove_unique_tags([], []).
remove_unique_tags([F-_N | R], [F | R1]) :-
	remove_unique_tags(R, R1).

assign_multiplicities1([], [multiexample(X, N)], X, N) :-
	!.
assign_multiplicities1([F | R], [multiexample(X, N) | R1], X, N) :-
	F \== X,
	!,
	assign_multiplicities1(R, R1, F, 1).
assign_multiplicities1([X | R], R1, X, N) :-
	N1 is N + 1,
	!,
	assign_multiplicities1(R, R1, X, N1).

%---------------------------------------------------------------

add_tag_to_file(InFile, Tag, OutFile) :-
	absolute_file_name(InFile, InFile1),
	add_tag_to_file1(InFile1, Tag, OutFile),
	!.
add_tag_to_file(File, Tag, File1) :-
	format2error('~N~n*** Error: bad call: ~w.~n~n',
		     [add_tag_to_file(File, Tag, File1)]),
	fail.

add_tag_to_file1(File, Tag, File1) :-
	atom_codes(Tag, TagChars),
	atom_codes(File, FileChars),
	add_tag_to_chars(FileChars, TagChars, File1Chars),
	atom_codes(File1, File1Chars),
	!.

add_tag_to_chars(FileChars, TagChars, File1Chars) :-
	append(MainChars, ".pl", FileChars),
	append_list([MainChars, "_", TagChars, ".pl"], File1Chars),
	!.
add_tag_to_chars(FileChars, TagChars, File1Chars) :-
	append(MainChars, ".regulus", FileChars),
	append_list([MainChars, "_", TagChars, ".regulus"], File1Chars),
	!.
add_tag_to_chars(FileChars, TagChars, File1Chars) :-
	append(MainChars, ".slot_definitions", FileChars),
	append_list([MainChars, "_", TagChars, ".slot_definitions"], File1Chars),
	!.
add_tag_to_chars(FileChars, TagChars, File1Chars) :-
	append_list([FileChars, "_", TagChars], File1Chars),
	!.

