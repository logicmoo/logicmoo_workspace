
:- module(help,
	  [make_help_sentences/2,
	   make_help_sentences_qa/2,

	   compile_help_resources/5,
	   load_help_resources/2,

	   remove_help_resources/0,

	   list_missing_help_declarations/1,

	   get_help_matches/3,
	   get_help_matches/4,
	   
	   get_help_matches_with_lf/3,
	   get_help_matches_with_lf/4,
	   
	   get_help_matches_in_translation_context/4,

	   plain_ngram_match_score/3,

	   help_trace_shows_trivial_matching/1,

	   switch_on_help_response/0,
	   switch_off_help_response/0,
	   help_response_is_on/0,

	   show_help_response/1,
	   show_help_response_for_translation_context/2,

	   help_process_file/2,

	   csv_paraphrase_file_to_help_input_file/2
	  ]
	 ).

:- use_module('$REGULUS/Prolog/regulus_read').
:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/regulus2dcg').
:- use_module('$REGULUS/Prolog/translate').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(ordsets)).

%----------------------------------------------------------------------

n_help_sents(5).

help_weight(unigrams, 2).
help_weight(bigrams, 6).
help_weight(trigrams, 18).

help_weight(backed_off_unigrams, 1).
help_weight(backed_off_bigrams, 3).
help_weight(backed_off_trigrams, 9).

% By default, pick the shortest match
help_weight(length, -0.01).

%----------------------------------------------------------------------

:- dynamic help_response_is_on/0.

switch_on_help_response :-
	retractall(help_response_is_on),
	assertz(help_response_is_on).

switch_off_help_response :-
	retractall(help_response_is_on).

%----------------------------------------------------------------------

make_help_sentences(Files, OutFile) :-
	make_help_sentences(Files, OutFile, no_context).

make_help_sentences_qa(Files, OutFile) :-
	make_help_sentences(Files, OutFile, context).

%----------------------------------------------------------------------

compile_help_resources(SourceFiles, HelpClassesFile, CorpusFile, BackedOffCorpusFile, CompiledHelpClassesFile) :-
	make_help_sentences(SourceFiles, CorpusFile),
	
	compile_help_class_file(HelpClassesFile, CompiledHelpClassesFile),
	abolish_if_defined(help_class_member/6),
	abolish_if_defined(user:substitutable_help_class/1),
	safe_compile_with_redefine_warnings_off(user, CompiledHelpClassesFile),

	warn_about_duplicate_help_class_entries,
	
	make_backed_off_help_sentences(CorpusFile, BackedOffCorpusFile),
	abolish_if_defined(help_sent/5),
	safe_compile_with_redefine_warnings_off(user, BackedOffCorpusFile).

%----------------------------------------------------------------------

load_help_resources(BackedOffCorpusFile, CompiledHelpClassesFile) :-
	abolish_if_defined(user:help_class_member/6),
	abolish_if_defined(user:substitutable_help_class/1),
	safe_compile_with_redefine_warnings_off(user, CompiledHelpClassesFile),
	
	abolish_if_defined(user:help_sent/5),
	safe_compile_with_redefine_warnings_off(user, BackedOffCorpusFile).

%----------------------------------------------------------------------

remove_help_resources :-
	abolish_if_defined(user:help_class_member/6),
	abolish_if_defined(user:substitutable_help_class/1),
	abolish_if_defined(user:help_sent/5).

%----------------------------------------------------------------------

make_help_sentences(Files, OutFile, ContextP) :-
	(   (   Files = [use_combined_interlingua_corpus(FromLang, ToLang), InterlinguaCorpus],
		ContextP = no_context ) ->
	    collect_source_sents_from_interlingua_corpus(InterlinguaCorpus, FromLang, ToLang, SourceSents)
	;
	    otherwise ->
	    collect_source_sents_from_file(Files, SourceSents-[], ContextP)
	),
	write_help_sentences(SourceSents, OutFile),
	!.
	
%----------------------------------------------------------------------

collect_source_sents_from_interlingua_corpus(File, FromLang, ToLang, SourceSents) :-
	absolute_file_name(File, AbsFile),
	prolog_file_to_list(AbsFile, List),
	length(List, N),
	format('~N--- Read combined interlingua corpus (~d records) ~w~n', [N, AbsFile]),
	collect_source_sents_from_interlingua_list(List, FromLang, ToLang, SourceSents-[]).

collect_source_sents_from_interlingua_list([], _FromLang, _ToLang, Sents-Sents).
collect_source_sents_from_interlingua_list([F | R], FromLang, ToLang, SentsIn-SentsOut) :-
	collect_source_sents_from_interlingua_item(F, FromLang, ToLang, SentsIn-SentsNext),
	!,
	collect_source_sents_from_interlingua_list(R, FromLang, ToLang, SentsNext-SentsOut).

collect_source_sents_from_interlingua_item(Item, FromLang, ToLang, SentsIn-SentsOut) :-
	Item = interlingua_item(_Surface, _Form, List),
	member(to(ToLang)-_, List),
	!,
	collect_source_sents_from_interlingua_item1(List, FromLang, SentsIn-SentsOut).
collect_source_sents_from_interlingua_item(_Other, _FromLang, _ToLang, SentsIn-SentsIn).

collect_source_sents_from_interlingua_item1([], _FromLang, Sents-Sents).
collect_source_sents_from_interlingua_item1([from(FromLang)-[Source0, LF] | R], FromLang, [sent(Source, LF) | NextSents]-OutSents) :-
	(   ( atom(Source0), Source0 = Source ) ;
	    Source0 = Source+_Context
	),
	!,
	collect_source_sents_from_interlingua_item1(R, FromLang, NextSents-OutSents).
collect_source_sents_from_interlingua_item1([_other | R], FromLang, InSents-OutSents) :-
	!,
	collect_source_sents_from_interlingua_item1(R, FromLang, InSents-OutSents).

%----------------------------------------------------------------------

collect_source_sents_from_file([], Sources-Sources, _ContextP).
collect_source_sents_from_file([F | R], SourcesIn-SourcesOut, ContextP) :-
	collect_source_sents_from_file(F, SourcesIn-SourcesNext, ContextP),
	!,
	collect_source_sents_from_file(R, SourcesNext-SourcesOut, ContextP).

collect_source_sents_from_file(File, SourcesIn-SourcesOut, ContextP) :-	
	absolute_file_name(File, AbsFile),
	prolog_file_to_list(AbsFile, List),
	length(List, N),
	format('~N--- Read translation/dialogue output file (~d records) ~w~n', [N, AbsFile]),
	collect_source_sents_from_list(List, SourcesIn-SourcesOut, ContextP).

collect_source_sents_from_list([], SourcesIn-SourcesIn, _ContextP).
collect_source_sents_from_list([F | R], [sent(Source, LF) | SourcesNext]-SourcesOut, ContextP) :-
	collect_source_sent_from_item(F, Source, LF, ContextP),
	!,
	collect_source_sents_from_list(R, SourcesNext-SourcesOut, ContextP).
collect_source_sents_from_list([_F | R], SourcesIn-SourcesOut, ContextP) :-
	!,
	collect_source_sents_from_list(R, SourcesIn-SourcesOut, ContextP).

collect_source_sent_from_item(F, Source, LF, no_context) :-
	F = dialogue_record(Items),
	member(sent=Source, Items),
	atom_codes(Source, SourceChars),
	\+ interpret_string_as_raw_lf_input(SourceChars, _RawLF),
	member(abstract_action_and_out_state=[_Action, _State, Judgement], Items),
	\+ member(Judgement, [bad, error]),
	member(parse=LF, Items), 	
	!.
collect_source_sent_from_item(F, Source, LF, no_context) :-
	F = translation(Source0, _Target, Info, Judgement),
	(   ( atom(Source0), Source0 = Source ) ;
	    Source0 = Source+_Context
	),
	\+ member(Judgement, [bad, error]),
	member(source_representation=LF, Info),
	!.
collect_source_sent_from_item(F, SourcePlusContext, LF, context) :-
	F = translation(Source0, _Target, Info, Judgement),
	Source0 = Source+Context,
	\+ member(Judgement, [bad, error]),
	format_to_atom('~w | ~w', [Context, Source], SourcePlusContext),
	member(source_representation=LF, Info),
	!.

%----------------------------------------------------------------------

write_help_sentences(Sents, OutFile) :-
	absolute_file_name(OutFile, AbsOutFile),
	sort(Sents, SortedSents),
	length(SortedSents, Count),
	list_to_prolog_file(SortedSents, AbsOutFile),
	format('~N--- Written help sents file (~d items) ~w~n', [Count, AbsOutFile]).

%=======================================================================

compile_help_class_file(InFile, OutFile) :-
	(   user:grammar_is_loaded ->
	    true
	;
	    format2error('~N*** Error: grammar must be loaded to compile help classes.~n', []),
	    fail
	),
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),
	
	read_generic_regulus_related_file_or_files(AbsInFile, RulesIn),
	separate_help_class_rules(RulesIn, HelpClassRulesIn-[], SubstitutableRulesIn-[]),

	compile_substitutable_class_rules(SubstitutableRulesIn, SubstitutableRulesOut),
	
	compile_help_class_list(HelpClassRulesIn, []-HelpClassRulesOut),
	safe_remove_duplicates_preserving_order(HelpClassRulesOut, FilteredHelpClassRulesOut),

	append(SubstitutableRulesOut, FilteredHelpClassRulesOut, AllRulesOut),

	length(AllRulesOut, NOut),
	list_to_prolog_file(AllRulesOut, AbsOutFile),
	format('~N--- Written ~d compiled help class entries to ~w~n', [NOut, AbsOutFile]),
	!.
compile_help_class_file(InFile, OutFile) :-
	format2error('~N*** Error: bad call: ~w~n', [compile_help_class_file(InFile, OutFile)]),
	fail.

separate_help_class_rules([], HelpClassRules-HelpClassRules, SubstitutableRules-SubstitutableRules).
separate_help_class_rules([F | R], HelpClassRulesIn-HelpClassRulesOut, SubstitutableRulesIn-SubstitutableRulesOut) :-
	classify_help_class_rule(F, Type),
	(   Type = help_class_member ->
	    HelpClassRulesIn = [F | HelpClassRulesNext],
	    SubstitutableRulesIn = SubstitutableRulesNext
	;
	    Type = substitutable_help_class ->
	    HelpClassRulesIn = HelpClassRulesNext,
	    SubstitutableRulesIn = [F | SubstitutableRulesNext]
	;
	    otherwise ->
	    format2error('~N*** Error: unknown help class type: ~w~n', [Type]),
	    fail
	),
	!,
	separate_help_class_rules(R, HelpClassRulesNext-HelpClassRulesOut, SubstitutableRulesNext-SubstitutableRulesOut).

classify_help_class_rule(rule(Body, _LineInfo), Type) :-
	nonvar(Body),
	Body = help_class_member(_Surface, _Class),
	Type = help_class_member,
	!.
classify_help_class_rule(rule(Body, _LineInfo), Type) :-
	nonvar(Body),
	Body = ( help_class_member(_Surface, _Class) :- lex_entry(_) ),
	Type = help_class_member,
	!.
classify_help_class_rule(rule(Body, _LineInfo), Type) :-
	nonvar(Body),
	Body = ( substitutable_help_class(_Class) ),
	Type = substitutable_help_class,
	!.
classify_help_class_rule(rule(Body, LineInfo), _Type) :-
	nonvar(Body),
	nonvar(LineInfo),
	format2error('~N*** Error: unknown item found in help class file: ~w~n', [Body]),
	inform_about_line_info(LineInfo),
	fail.

%=======================================================================

compile_substitutable_class_rules([], []).
compile_substitutable_class_rules([F | R], [F1 | R1]) :-
	compile_substitutable_class_rule(F, F1),
	!,
	compile_substitutable_class_rules(R, R1).

compile_substitutable_class_rule(rule(substitutable_help_class(Class), _LineInfo),
				 substitutable_help_class(Class)) :-
	!.
compile_substitutable_class_rule(X, Y) :-
	format2error('~N*** Error: bad call: ~w', [compile_substitutable_class_rule(X, Y)]),
	fail.

%=======================================================================

compile_help_class_list([], InRules-InRules).
compile_help_class_list([F | R], InRules-OutRules) :-
	compile_help_class_entry(F, Rules),
	append(InRules, Rules, NextRules),
	!,
	compile_help_class_list(R, NextRules-OutRules).

compile_help_class_entry(Record, CompiledRules) :-
	Record = rule(Body, LineInfo),
	findall(CompiledRule,
		compile_help_class_entry1(Body, CompiledRule),
		CompiledRules),
	(   CompiledRules \== [] ->
	    true
	;
	    class_for_help_class_entry(Body, Class) ->
	    format2error('~N*** Warning: nothing found for help definition (class = ~w) ', [Class]),
	    inform_about_line_info(LineInfo)
	;
	    otherwise ->
	    format2error('~N*** Warning: nothing found for help definition ', []),
	    inform_about_line_info(LineInfo)
	).

/*

Two kinds of entries, e.g.

help_class_member(monday, day_of_week).

help_class_member(Surface, person_name) :-
	lex_entry((name:[sem_n_type=agent] --> Surface)).

*/

class_for_help_class_entry(help_class_member(_Surface, Class),
			   Class) :-
	!.
class_for_help_class_entry(( help_class_member(_Surface, Class) :- _Body),
			   Class) :-
	!.

compile_help_class_entry1(RecordIn, RecordOut) :-
	compile_help_class_entry2(RecordIn, RecordNext),
	RecordNext = help_class_member(Surface, Class),
	comma_list_to_list(Surface, SurfaceList),
	SurfaceList = [FirstWord | _Rest],
	% Convert to d-list
	length(SurfaceList, Length),
	append(SurfaceList, SurfaceOut, SurfaceIn),
	RecordOut = help_class_member(FirstWord, Length, SurfaceIn, SurfaceOut, Class, (SurfaceList --> Class)),
	make_ground(RecordOut).

compile_help_class_entry2(help_class_member(Surface, Class),
			  help_class_member(Surface, Class)).
compile_help_class_entry2(( help_class_member(Surface, Class) :- lex_entry((Cat --> Surface))),
			  help_class_member(Surface, Class)) :-
	find_lex_entry(Cat, Surface).

find_lex_entry(Cat, Surface) :-
	internalise_cat(Cat, InternalCat),
	lex_entry_for_cat(InternalCat, Surface).

internalise_cat(Cat, InternalCat) :-
	user:grammar_is_loaded,
	Cat = CatName:Feats,
	(   member(sem=Sem, Feats) ->
	    true
	;
	    otherwise ->
	    Sem = _UninstantiatedSem
	),
	sort(Feats, SortedFeats),
	internalise_cat(CatName, SortedFeats, InternalCat0, head),
	InternalCat0 = cat(CatName, InternalFeats, _OriginalSem),
	InternalCat = cat(CatName, InternalFeats, Sem),
	!.
internalise_cat(Cat, InternalCat) :-
	format2error('~N*** Error: bad call: ~w~n', [internalise_cat(Cat, InternalCat)]),
	fail.

%=======================================================================

warn_about_duplicate_help_class_entries :-
	all_help_phrases(HelpPhraseList),
	warn_about_duplicate_help_class_entries1(HelpPhraseList),
	!.
warn_about_duplicate_help_class_entries :-
	format2error('~N*** Error: bad call: ~w~n', [warn_about_duplicate_help_class_entries]),
	fail.

all_help_phrases(HelpPhraseList) :-
	findall(HelpPhrase,
		user:help_class_member(_FirstWord, _Length, HelpPhrase, [], _SomeClass, _SomeSubst),
		HelpPhraseList0),
	safe_remove_duplicates_preserving_order(HelpPhraseList0, HelpPhraseList),
	!.

warn_about_duplicate_help_class_entries1([]).
warn_about_duplicate_help_class_entries1([F | R]) :-
	warn_about_duplicate_help_class_entry(F),
	!,
	warn_about_duplicate_help_class_entries1(R).

warn_about_duplicate_help_class_entry(HelpPhraseList) :-
	findall(Class,
		user:help_class_member(_FirstWord, _Length, HelpPhraseList, [], Class, _Subst),
		Classes),
	length(Classes, NClasses),
	(   NClasses =< 1 ->
	    true
	;
	    otherwise ->
	    list_to_comma_list(HelpPhraseList, HelpPhrase),
	    format('~N*** Warning: "~w" in multiple help classes (first one is used): ~w~n', [HelpPhrase, Classes])
	).

%=======================================================================

list_missing_help_declarations(OutFile) :-
	check_help_resources_are_loaded,   
	(   user:grammar_is_loaded ->
	    true
	;
	    format2error('~N*** Error: some grammar needs to be loaded~n', []),
	    fail
	),
	
	findall([DefaultClass, Surface],
		missing_help_declaration(Surface, DefaultClass, _Sem),
		Tuples0),
	sort(Tuples0, Tuples),
	length(Tuples, N),

	safe_absolute_file_name(OutFile, AbsOutFile),
	open(AbsOutFile, write, S),
	%write_opening_comment_for_missing_help_declarations(S),
	list_missing_help_declarations1(Tuples, S),
	close(S),
	format('~N~n--- Written ~d missing help declarations to ~w~n', [N, OutFile]),
	!.

missing_help_declaration(Surface, DefaultClass, Sem) :-
	user:dcg_clause(DCGRuleHead, DCGRuleBody),
	DCGRuleHead =.. [_CatName, _Tree, _InternalFeats, Sem, _GSem, _In, _Out],
	unpack_body_of_lexical_dcg_rule(DCGRuleBody, Surface),
	comma_list_to_list(Surface, SurfaceList),
	SurfaceList \== [],
	\+ user:help_class_member(_FirstWord, _Length, SurfaceList, [], _SomeClass, _Subst),
	join_with_underscore(SurfaceList, DefaultClass).

list_missing_help_declarations1([], _S).
list_missing_help_declarations1([F | R], S) :-
	list_missing_help_declaration(F, S),
	!,
	list_missing_help_declarations1(R, S).

list_missing_help_declaration([DefaultClass, Surface], S) :-
	format(S, '~N~q.~n', [help_class_member(Surface, DefaultClass)]),
	!.
list_missing_help_declaration(Item, S) :-
	format2error('~N*** Error: bad call: ~w~n', [list_missing_help_declaration(Item, S)]),
	fail.

%=======================================================================

lex_entry_for_cat(InternalCat, Surface) :-
	InternalCat = cat(CatName, InternalFeats, Sem),
	DCGRuleHead =.. [CatName, _Tree, InternalFeats, Sem, _GSem, _In, _Out],
	user:dcg_clause(DCGRuleHead, DCGRuleBody),
	unpack_body_of_lexical_dcg_rule(DCGRuleBody, Surface),
	ground(Surface).

unpack_body_of_lexical_dcg_rule((P, Q), (P1, Q1)) :-
	unpack_body_of_lexical_dcg_rule(P, P1),
	unpack_body_of_lexical_dcg_rule(Q, Q1).
unpack_body_of_lexical_dcg_rule('C'(_In, Surface, _Out), Surface).
unpack_body_of_lexical_dcg_rule((_In = [Surface | _Out]), Surface).

%=======================================================================

make_backed_off_help_sentences(CorpusFile, BackedOffCorpusFile)	:-
	absolute_file_name(CorpusFile, AbsCorpusFile),
	absolute_file_name(BackedOffCorpusFile, AbsBackedOffCorpusFile),

	prolog_file_to_list(AbsCorpusFile, InList),
	length(InList, InN),
	format('~N--- Read corpus file (~d records): ~w~n', [InN, AbsCorpusFile]),

	make_backed_off_help_sentence_list(InList, OutList),
	length(OutList, OutN),

	list_to_prolog_file(OutList, AbsBackedOffCorpusFile),
	format('~N--- Written backed off corpus file (~d records): ~w~n', [OutN, AbsBackedOffCorpusFile]),
	!.
make_backed_off_help_sentences(CorpusFile, BackedOffCorpusFile)	:-
	format2error('~N*** Error: bad call: ~w~n', [make_backed_off_help_sentences(CorpusFile, BackedOffCorpusFile)]),
	fail.

make_backed_off_help_sentence_list([], []).
make_backed_off_help_sentence_list([F | R], [F1 | R1]) :-
	make_backed_off_help_sentence_record(F, F1),
	!,
	make_backed_off_help_sentence_list(R, R1).

make_backed_off_help_sentence_record(sent(SentAtom, LF), help_sent(Words, WordsWithoutStopWords, BackedOffWords, Feats, LF)) :-
	split_atom_into_words(SentAtom, Words),
	remove_stop_words_in_list(Words, WordsWithoutStopWords),
	back_off_word_list_using_help_classes(Words, BackedOffWords),
	% Not really clear if we want to include stop words when extracting features
	ngram_features(WordsWithoutStopWords, BackedOffWords, Feats),
	%ngram_features(Words, BackedOffWords, Feats),
	!.
make_backed_off_help_sentence_record(F, F1) :-
	format2error('~N*** Error: bad call: ~w~n', [make_backed_off_help_sentence_record(F, F1)]),
	fail.

back_off_word_list_using_help_classes(Words, BackedOffWords) :-
	back_off_word_list_using_help_classes_keeping_subs(Words, BackedOffWords0, _Subs),
	delete(BackedOffWords0, stop_word, BackedOffWords).

back_off_word_list_using_help_classes_keeping_subs([], [], []).
back_off_word_list_using_help_classes_keeping_subs(In, Result, [Sub | Subs]) :-
	best_backoff_continuation(In, InRest, Result, ResultRest, Sub),
	!,
	back_off_word_list_using_help_classes_keeping_subs(InRest, ResultRest, Subs).
back_off_word_list_using_help_classes_keeping_subs([F | R], [F | R1], Subs) :-
	!,
	back_off_word_list_using_help_classes_keeping_subs(R, R1, Subs).

% We want to find the longest match, but we also want to make sure that
% we pick the first entry if a word is assigned to two help classes. So
% first find how long the longest possible match is, then pick the
% first entry that has the maximal match length.
best_backoff_continuation(In, ChosenInRest, ChosenResult, ChosenResultRest, ChosenSub) :-
	findall(Length-[InRest, Result, ResultRest, Sub],
		backoff_start_of_list(In, InRest, Length, Result, ResultRest, Sub),
		Pairs),
	keysort(Pairs, SortedPairs),
	reverse(SortedPairs, ReversedSortedPairs),
	% BestLength = length of longest match
	ReversedSortedPairs = [BestLength-_Tuple | _Rest],
	% Find the first element which gets a co-maximal match
	member(BestLength-[ChosenInRest, ChosenResult, ChosenResultRest, ChosenSub], Pairs),
	!.

/*
Typical compiled entry looks like this:

help_class_member(eighty,2,[eighty,six|A],A,cardinal_number).
*/

backoff_start_of_list(In, InRest, Length, Result, ResultRest, Sub) :-
	In = [FirstWord | _Rest],
	user:help_class_member(FirstWord, Length, In, InRest, BackedOff, Sub),
	Result = [BackedOff | ResultRest].

%=======================================================================

remove_stop_words_in_list([], []).
remove_stop_words_in_list(In, Out) :-
	In = [FirstWord | _Rest],
	user:help_class_member(FirstWord, _Length, In, InRest, stop_word, _Sub),
	!,
	remove_stop_words_in_list(InRest, Out).
remove_stop_words_in_list([F | R], [F | R1]) :-
	!,
	remove_stop_words_in_list(R, R1).

%=======================================================================

get_help_matches(Sent, N, Matches) :-
	get_help_matches(Sent, N, Matches, _Traces).

get_help_matches(Sent, N, Matches, Traces) :-
	check_help_resources_are_loaded,
	findall(Score-[Match, Score-Trace],
		help_match(Sent, Match, _LF, Score, Trace),
		Pairs),
	keysort(Pairs, SortedPairs),
	unkey_list(SortedPairs, SortedMatches),
	reverse(SortedMatches, ReversedSortedMatches0),
	safe_remove_duplicates_preserving_order(ReversedSortedMatches0, ReversedSortedMatches),
	firstn_or_all(ReversedSortedMatches, N, MatchesWithTraces),
	split_matches_and_traces(MatchesWithTraces, Matches, Traces).

get_help_matches_with_lf(Sent, N, Matches) :-
	get_help_matches_with_lf(Sent, N, Matches, _Traces).

get_help_matches_with_lf(Sent, N, Matches, Traces) :-
	check_help_resources_are_loaded,
	findall(Score-[Match, LF, Score-Trace],
		help_match(Sent, Match, LF, Score, Trace),
		Pairs),
	keysort(Pairs, SortedPairs),
	unkey_list(SortedPairs, SortedMatches),
	reverse(SortedMatches, ReversedSortedMatches0),
	safe_remove_duplicates_preserving_order(ReversedSortedMatches0, ReversedSortedMatches),
	firstn_or_all(ReversedSortedMatches, N, MatchesWithTraces),
	split_matches_and_traces(MatchesWithTraces, Matches, Traces).

get_help_matches_in_translation_context(Sent, Context, N, Matches) :-
	get_help_matches_in_translation_context(Sent, Context, N, Matches, _Traces).

get_help_matches_in_translation_context(Sent, Context, N, Matches, Traces) :-
	check_help_resources_are_loaded,
	findall(Score-[Match, LF, Score-Trace],
		help_match(Sent, Match, LF, Score, Trace),
		Pairs),
	keysort(Pairs, SortedPairs),
	unkey_list(SortedPairs, SortedMatches),
	reverse(SortedMatches, ReversedSortedMatches0),
	safe_remove_duplicates_preserving_order(ReversedSortedMatches0, ReversedSortedMatches),
	firstn_or_all_matches_with_good_interlingua_in_context(ReversedSortedMatches, N, Context, Matches, Traces).

%=======================================================================

split_matches_and_traces([], [], []) :-
	!.
split_matches_and_traces([[Match, Score-Trace] | R], [Match | R1], [Score-Trace | R2]) :-
	!,
	split_matches_and_traces(R, R1, R2).
split_matches_and_traces([[Match, LF, Score-Trace] | R], [[Match, LF] | R1], [Score-Trace | R2]) :-
	!,
	split_matches_and_traces(R, R1, R2).
split_matches_and_traces(MatchesWithTraces, Matches, Traces) :-
	format2error('~N*** Error: bad call: ~w.~n', [split_matches_and_traces(MatchesWithTraces, Matches, Traces)]),
	fail.

%=======================================================================

plain_ngram_match_score(Words1, Words2, Score) :-
	ngram_features(Words1, [], Feats1),
	ngram_features(Words2, [], Feats2),
	symmetrical_help_score(Feats1, Feats2, [], Score, _Trace),
	!.

%=======================================================================

check_help_resources_are_loaded :-
	current_predicate(user:help_sent/5),
	current_predicate(user:help_class_member/6),
	!.
check_help_resources_are_loaded :-
	format2error('~N*** Error: help resources are not loaded. Do COMPILE_HELP or LOAD_HELP to load them.~n', []),
	fail.

help_match(Sent, Match, LF, Score, Trace) :-
	split_atom_into_words(Sent, Words),
	remove_stop_words_in_list(Words, WordsWithoutStopWords),
	back_off_word_list_using_help_classes_keeping_subs(WordsWithoutStopWords, BackedOffWords, Subs),
	%ngram_features(WordsWithoutStopWords, BackedOffWords, Feats),
	ngram_features(Words, BackedOffWords, Feats),
	!,
	user:help_sent(MatchWords, _MatchWordsWithoutStopWords, _MatchBackedOffWords, MatchFeats, LF),
	apply_substitutable_classes(MatchWords, Subs, MatchWordsWithSubs),
	% If we applied substitutions, the cached features will no longer be correct,
	% so we will need to recalculate them on the fly
	(   MatchWords = MatchWordsWithSubs ->
	    MatchFeatsToUse = MatchFeats
	;
	    join_with_spaces(MatchWordsWithSubs, SentAtomWithSubs) ->
	    make_backed_off_help_sentence_record(sent(SentAtomWithSubs, LF),
						 help_sent(_Words1, _MatchWordsWithoutStopWords1, _BackedOffWords1, MatchFeatsToUse, _LF1))
	),
	symmetrical_help_score(Feats, MatchFeatsToUse, MatchWords, Score, Trace),
	join_with_spaces(MatchWordsWithSubs, Match).

symmetrical_help_score(Feats, MatchFeats, MatchWords, Score, Trace) :-
	length(MatchWords, NMatchWords),
	help_score(Feats, MatchFeats, 0-Score1, Trace-TraceNext1),
	help_score(MatchFeats, Feats, Score1-Score2, TraceNext1-TraceNext2),
	help_score_for_length(NMatchWords, Score2-Score, TraceNext2-[]).

help_score([], [], Score-Score, Trace-Trace).
help_score([FeatName-List1 | R], [FeatName-List2 | R1],
	   ScoreIn-ScoreOut, [TraceElt | TraceNext]-TraceOut) :-
	help_score_for_feature(FeatName, List1, List2, Score, TraceElt),
	ScoreNext is ScoreIn + Score,
	!,
	help_score(R, R1, ScoreNext-ScoreOut, TraceNext-TraceOut).

help_score_for_feature(FeatName, List1, List2, Score, TraceElt) :-
	ord_intersection(List1, List2, Intersection),
	length(Intersection, N),
	help_weight(FeatName, Weight),
	Score is Weight * N,
	TraceElt = FeatName:(Weight * N),
	!.
help_score_for_feature(FeatName, List1, List2, Score, TraceElt) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [help_score_for_feature(FeatName, List1, List2, Score, TraceElt)]),
	fail.

help_score_for_length(NMatchWords, ScoreIn-ScoreOut, [TraceElt | TraceOut]-TraceOut) :-
	FeatName = length,
	help_weight(FeatName, Weight),
	Score is Weight * NMatchWords,
	TraceElt = FeatName:(Weight * NMatchWords),
	ScoreOut is ScoreIn + Score,
	!.
help_score_for_length(NMatchWords, Score, Trace) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [help_score_for_length(NMatchWords, Score, Trace)]),
	fail.

% Say that help match is trivial if no backed-off words are matched.
% We use the backed-off words because words on the stop list will have been discarded.
help_trace_shows_trivial_matching(Trace) :-
	Trace = Score-Details,
	Score =< 4,
	member(backed_off_unigrams:(_Weight*BackedOffUnigramScore), Details),
	BackedOffUnigramScore = 0.

ngram_features(Words, BackedOffWords, Feats) :-
	unigrams_for_list(Words, Unigrams),
	bigrams_for_list(Words, Bigrams),
	trigrams_for_list(Words, Trigrams),

	unigrams_for_list(BackedOffWords, BackedOffUnigrams),
	bigrams_for_list(BackedOffWords, BackedOffBigrams),
	trigrams_for_list(BackedOffWords, BackedOffTrigrams),

	Feats = [unigrams-Unigrams,
		 bigrams-Bigrams,
		 trigrams-Trigrams,
		 backed_off_unigrams-BackedOffUnigrams,
		 backed_off_bigrams-BackedOffBigrams,
		 backed_off_trigrams-BackedOffTrigrams].

unigrams_for_list(Words, Unigrams) :-
	list_to_ord_set(Words, Unigrams).

bigrams_for_list(Words, Bigrams) :-
	bigrams_for_list1(Words, Bigrams0),
	list_to_ord_set(Bigrams0, Bigrams).

trigrams_for_list(Words, Trigrams) :-
	trigrams_for_list1(Words, Trigrams0),
	list_to_ord_set(Trigrams0, Trigrams).

bigrams_for_list1(L, []) :-
	length(L, N),
	N < 2,
	!.
bigrams_for_list1([X, Y | R], [Bigram | R1]) :-
	[X, Y] = Bigram,
	!,
	bigrams_for_list1([Y | R], R1).

trigrams_for_list1(L, []) :-
	length(L, N),
	N < 3,
	!.
trigrams_for_list1([X, Y, Z | R], [Trigram | R1]) :-
	[X, Y, Z] = Trigram,
	!,
	trigrams_for_list1([Y, Z | R], R1).

%=======================================================================

apply_substitutable_classes(MatchWords, QuerySubs, MatchWordsWithSubs) :-
	current_predicate(user:substitutable_help_class/1),
	QuerySubs \== [],
	back_off_word_list_using_help_classes_keeping_subs(MatchWords, _BackedOffMatchWords, MatchSubs),
	compose_substitutable_match_and_query_subs(MatchSubs, QuerySubs, CombinedSubs),
	apply_subs_to_word_list(MatchWords, CombinedSubs, MatchWordsWithSubs),
	!.
apply_substitutable_classes(MatchWords, _QuerySubs, MatchWords).

compose_substitutable_match_and_query_subs([], _QuerySubs, []).
compose_substitutable_match_and_query_subs([F | R], QuerySubs, [F1 | R1]) :-
	compose_substitutable_match_and_query_sub(F, QuerySubs, F1),
	!,
	compose_substitutable_match_and_query_subs(R, QuerySubs, R1).
compose_substitutable_match_and_query_subs([_F | R], QuerySubs, R1) :-
	!,
	compose_substitutable_match_and_query_subs(R, QuerySubs, R1).

compose_substitutable_match_and_query_sub((Words --> Class), QuerySubs, (Words --> QueryWords)) :-
	user:substitutable_help_class(Class),
	member((QueryWords --> Class), QuerySubs),
	!.

apply_subs_to_word_list(MatchWords, Subs, MatchWordsWithSubs) :-
	apply_subs_to_word_list1(MatchWords, Subs, MatchWordsWithSubs0),
	flatten(MatchWordsWithSubs0, MatchWordsWithSubs).

apply_subs_to_word_list1([], _Subs, []) :-
	!.
apply_subs_to_word_list1(MatchWords, Subs, [To | MatchWordsWithSubsRest]) :-
	member((From --> To), Subs),
	append(From, MatchWordsRest, MatchWords),
	!,
	apply_subs_to_word_list1(MatchWordsRest, Subs, MatchWordsWithSubsRest).
apply_subs_to_word_list1([F | MatchWordsRest], Subs, [F | MatchWordsWithSubsRest]) :-
	!,
	apply_subs_to_word_list1(MatchWordsRest, Subs, MatchWordsWithSubsRest).

%=======================================================================

firstn_or_all_matches_with_good_interlingua_in_context([], _N, _Context, [], []).
firstn_or_all_matches_with_good_interlingua_in_context(_Matches, N, _Context, [], []) :-
	N =< 0,
	!.
firstn_or_all_matches_with_good_interlingua_in_context([[Match, LF, TraceElt] | R], N, Context, [Match | R1], [TraceElt | R2]) :-
	N > 0,
	transfer_representation_to_surface_interlingua_using_context(LF, Context, _ContextOut, SurfaceInterlingua),
	\+ error_result(SurfaceInterlingua),
	N1 is N - 1,
	!,
	firstn_or_all_matches_with_good_interlingua_in_context(R, N1, Context, R1, R2).	
firstn_or_all_matches_with_good_interlingua_in_context([[DiscardedMatch, _LF, _TraceElt] | R], N, Context, R1, [_TraceElt | R2]) :-
	format('~N--- Help example "~w" discarded: fails to produce good interlingua~n', [DiscardedMatch]),
	N > 0,
	!,
	firstn_or_all_matches_with_good_interlingua_in_context(R, N, Context, R1, R2).	

%=======================================================================

help_process_file(InFile, OutFile) :-
	check_help_resources_are_loaded,
	get_n_help_sentences(N),
	
	prolog_file_to_list(InFile, InList),
	length(InList, NSents),
	format('~N~n--- Read file (~d records), ~w: ~n', [NSents, InFile]),

	open(OutFile, write, S),

	help_process_list(InList, N, 0, S),

	close(S),
	format('~N~n--- Output written to ~w: ~n', [OutFile]),
	!.

help_process_list([], _N, _Count, _S).
help_process_list([F | R], N, CountIn, S) :-
	help_process_item_for_batch(F, N, S),
	format(user, '.', []), 
	flush_output(user),
	CountNext is CountIn + 1,
	(   ( 0 is CountNext mod 100, CountNext > 0 ) ->
	    
	    format(' (~d) ~n', [CountNext]),
	    flush_output(user) ;
	    
	    true
	),
	!,
	help_process_list(R, N, CountNext, S).

help_process_item_for_batch(Item, N, S) :-
	compound(Item),
	functor(Item, sent, _),
	arg(1, Item, Sent),
	atom(Sent),
	!,
	help_process_item_for_batch1(Sent, N, S).
help_process_item_for_batch(Item, _N, _S) :-
	format('~N--- Warning: skipping item ~w: ~n', [Item]),
	!.

help_process_item_for_batch1(Sent, N, S) :-
	remove_stop_words_in_atom(Sent, SentWithoutStopWords),
	back_off_atom_using_help_classes(Sent, BackedOffSent),
	get_help_matches(Sent, N, Matches, Traces),
	format(S, '~N~n/*~n', []),
	format(S, '~N                  "Words: ~w"~n', [Sent]),
	show_help_response2(S, SentWithoutStopWords, BackedOffSent, Matches, Traces),
	format(S, '~N*/~n~n', []),
	print_sent_and_help_matches(S, Sent, Matches),
	!.

print_sent_and_help_matches(S, Sent, Matches) :-
	format(S, '~Nhelp_responses(~q,', [Sent]),
	format(S, '~N[~n', []),
	print_sent_and_help_matches1(S, Sent, Matches),
	format(S, '~N]).~n', []),
	!.

print_sent_and_help_matches1(_S, _Sent, []) :-
	!.
print_sent_and_help_matches1(S, Sent, [Last]) :-
	get_judgement_for_help_response(Last, Sent, Judgement),
	Item = [Last, Judgement],
	format(S, '~N ~q~n', [Item]),
	!.
print_sent_and_help_matches1(S, Sent, [F | R]) :-
	get_judgement_for_help_response(F, Sent, Judgement),
	Item = [F, Judgement],
	format(S, '~N ~q,~n', [Item]),
	!,
	print_sent_and_help_matches1(S, Sent, R).

get_judgement_for_help_response(Response, Sent, Judgement) :-
	(   Response = Sent ->
	    Judgement = good
	;
	    get_judgement_for_help_response1(Response, Sent, Judgement)
	).

get_judgement_for_help_response1(Response, Sent, Judgement) :-
	current_predicate(user:help_response_judgement/3),
	user:help_response_judgement(Response, Sent, Judgement),
	!.
get_judgement_for_help_response1(_Response, _Sent, Judgement) :-
	Judgement = '?help'.

%=======================================================================

show_help_response(Sent) :-
	check_help_resources_are_loaded,
	get_n_help_sentences(N),
	remove_stop_words_in_atom(Sent, SentWithoutStopWords),
	back_off_atom_using_help_classes(Sent, BackedOffSent),
	get_help_matches(Sent, N, Matches, Traces),
	show_help_response1(user, Sent, SentWithoutStopWords, BackedOffSent, Matches, Traces),
	!.
show_help_response(_Sent) :-
	format2error('~N*** Error: unable to get help response.~n', []),
	fail.

show_help_response_for_translation_context(Sent, Context) :-
	check_help_resources_are_loaded,
	get_n_help_sentences(N),
	remove_stop_words_in_atom(Sent, SentWithoutStopWords),
	back_off_atom_using_help_classes(Sent, BackedOffSent),
	get_help_matches_in_translation_context(Sent, Context, N, Matches, Traces),
	show_help_response1(user, Sent, SentWithoutStopWords, BackedOffSent, Matches, Traces),
	!.
show_help_response_for_translation_context(_Sent, _Context) :-
	format2error('~N*** Error: unable to get help response.~n', []),
	fail.

%=======================================================================

get_n_help_sentences(N) :-
	n_help_sents(N).

show_help_response1(S, _Words, SentWithoutStopWords, BackedOffSent, Matches, Traces) :-
	format(S, '~N~nHELP INFORMATION~n~n', []),
	show_help_response2(S, SentWithoutStopWords, BackedOffSent, Matches, Traces),
	format(S, '~N~nFormat of trace info: Feature:Weight*Value~n', []),
	format(S, '~N~nEND HELP INFORMATION~n~n', []).

show_help_response2(S, SentWithoutStopWords, BackedOffSent, Matches, Traces) :-
	format(S, '~NWords without stop words: "~w"~n', [SentWithoutStopWords]),
	format(S, '~N        Backed off words: "~w"~n~n', [BackedOffSent]),
	show_help_matches(S, Matches, Traces, 1),
	!.

show_help_matches(_S, [], [], _N).
show_help_matches(S, [F | R], [F1 | R1], I) :-
	show_help_match(S, F, F1, I),
	I1 is I + 1,
	!,
	show_help_matches(S, R, R1, I1).

show_help_match(S, Sent, Score-TraceInfo, I) :-
	back_off_atom_using_help_classes(Sent, BackedOffSent),
	compact_trace_info(TraceInfo, TraceInfo1),
	(   help_trace_shows_trivial_matching(Score-TraceInfo) ->
	    TrivialWarning = ' (PROBABLY TRIVIAL MATCH): '
	;
	    otherwise ->
	    TrivialWarning = ''
	),
	format(S, '~N#~d : "~w"~n', [I, Sent]),
	format(S, '~N     "~w" (backed off)~n', [BackedOffSent]),
	format(S, '~N     Score:~w~w ~w', [TrivialWarning, Score, TraceInfo1]),
	!.
show_help_match(S, F, F1, I) :-
	format2error('~N*** Error: bad call: ~w~n', [show_help_match(S, F, F1, I)]),
	fail.

compact_trace_info(TraceInfo, TraceInfo1) :-
	findall([Feat, Weight], help_weight(Feat, Weight), FeatsAndWeights),
	compact_trace_info1(FeatsAndWeights, TraceInfo, TraceInfo1).

compact_trace_info1([], _TraceInfo, []).
compact_trace_info1([F | R], TraceInfo, [F1 | R1]) :-
	compact_trace_info2(F, TraceInfo, F1),
	!,
	compact_trace_info1(R, TraceInfo, R1).

compact_trace_info2([Feat, Weight], TraceInfo, (Feat:(Weight*TotalScore))) :-
	findall(Score,
		member(Feat:(Weight*Score), TraceInfo),
		AllScores),
	safe_sum_list(AllScores, TotalScore),
	!.

%=======================================================================

back_off_atom_using_help_classes(Sent, BackedOffAtom) :-
	split_atom_into_words(Sent, Words),
	back_off_word_list_using_help_classes(Words, BackedOffWords),
	join_with_spaces(BackedOffWords, BackedOffAtom).

remove_stop_words_in_atom(Sent, SentWithoutStopWords) :-
	split_atom_into_words(Sent, Words),
	remove_stop_words_in_list(Words, WordsWithoutStopWords),
	join_with_spaces(WordsWithoutStopWords, SentWithoutStopWords).

%=======================================================================

csv_paraphrase_file_to_help_input_file(InFile, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),

	csv_file_to_list_of_lists(AbsInFile, InList),
	length(InList, NIn),
	format('~N--- Read CSV paraphrase file (~d records) ~w~n', [NIn, AbsInFile]),
	
	paraphrase_list_to_help_input_list(InList, OutList),
	
	length(OutList, NOut),
	list_to_prolog_file(OutList, AbsOutFile),
	format('~N--- Written Prolog paraphrase file with LFs (~d records) ~w~n', [NOut, AbsOutFile]).

paraphrase_list_to_help_input_list([], []).
paraphrase_list_to_help_input_list([F | R], [F1 | R1]) :-
	paraphrase_record_to_help_input_record(F, F1),
	!,
	paraphrase_list_to_help_input_list(R, R1).
paraphrase_list_to_help_input_list([_F | R], R1) :-
	!,
	paraphrase_list_to_help_input_list(R, R1).

paraphrase_record_to_help_input_record([Sent, Paraphrase | _Rest],
				       dialogue_record(Items)) :-
	atom(Sent),
	atom(Paraphrase),
	split_atom_into_words(Paraphrase, ParaphraseWords),
	!,
	(   ParaphraseWords = ['?'] ->
	    fail
	;
	    user:words_to_lf_and_tree_with_current_parser(ParaphraseWords, '.MAIN', LF, _Tree) ->
	    Items = [sent=Sent,
		     parse=LF,
		     abstract_action_and_out_state=[no_action, no_state, ok]]
	;
	    otherwise ->
	    format('*** Warning: unable to interpret "~w" as well-formed paraphrase~n', [Paraphrase]),
	    fail
	).
paraphrase_record_to_help_input_record(List, _) :-
	format('*** Warning: bad line "~w"~n', [List]),
	fail.

				       
	