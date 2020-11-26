% ebl_make_training_data.pl

:- use_module('$REGULUS/Prolog/analysis_constraints').
:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/compare_grammars').

:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

:- dynamic incremental_treebanking_off/0.
:- dynamic old_treebank_updated/0.
:- dynamic updated_from_old_treebank/1.
:- dynamic already_processed_ebl_data/1.
:- dynamic sent_in_current_training_corpus/3.

switch_off_incremental_treebanking :-
	retractall(incremental_treebanking_off),
	assertz(incremental_treebanking_off),
	!.

switch_on_incremental_treebanking :-
	retractall(incremental_treebanking_off),
	!.

save_reflective_dcg_grammar_for_treebank :-
	get_regulus_config_item(reflective_dcg_grammar, ReflectiveDCGGrammarFile),
	get_regulus_config_item(reflective_dcg_grammar_for_treebank, ReflectiveDCGGrammarFileForTreebank),
	safe_absolute_file_name(ReflectiveDCGGrammarFile, AbsReflectiveDCGGrammarFile),
	safe_absolute_file_name(ReflectiveDCGGrammarFileForTreebank, AbsReflectiveDCGGrammarFileForTreebank),
	safe_file_exists(AbsReflectiveDCGGrammarFile),
	copy_file(AbsReflectiveDCGGrammarFile, AbsReflectiveDCGGrammarFileForTreebank),
	format('~N~n--- Saved current grammar ~w as ~w~n',
	       [AbsReflectiveDCGGrammarFile, AbsReflectiveDCGGrammarFileForTreebank]),
	!.
save_reflective_dcg_grammar_for_treebank :-
	format('~N*** Warning: unable to save current grammar file for treebank~n', []).

save_parse_preferences_for_treebank :-
	regulus_config(parse_preferences, ParsePreferencesFile),
	!,
	get_regulus_config_item(parse_preferences_for_treebank, ParsePreferencesFileForTreebank),
	safe_absolute_file_name(ParsePreferencesFile, AbsParsePreferencesFile),
	safe_absolute_file_name(ParsePreferencesFileForTreebank, AbsParsePreferencesFileForTreebank),
	(   safe_file_exists(AbsParsePreferencesFile) ->
	    copy_file(AbsParsePreferencesFile, AbsParsePreferencesFileForTreebank),
	    format('~N~n--- Saved current parse preferences ~w as ~w~n',
	       [AbsParsePreferencesFile, AbsParsePreferencesFileForTreebank])
	;
	    format('~N~n*** Error: unable to find parse_preferences file ~w~n',
	       [AbsParsePreferencesFile])
	).
save_parse_preferences_for_treebank :-
	get_regulus_config_item(parse_preferences_for_treebank, ParsePreferencesFileForTreebank),
	safe_absolute_file_name(ParsePreferencesFileForTreebank, AbsParsePreferencesFileForTreebank),
	list_to_prolog_file([], AbsParsePreferencesFileForTreebank),
	format('~N~n--- Saved null parse preferences as ~w~n',
	       [AbsParsePreferencesFileForTreebank]),
	!.	

make_ebl_training_data(InFiles, GrammarAtom, HistoryFile, OutFile) :-
	make_ebl_training_data(InFiles, GrammarAtom, [], HistoryFile, OutFile, no_old_treebank).

make_ebl_training_data(InFiles, GrammarAtom, IgnoredSubdomains, HistoryFile, OutFile) :-
	make_ebl_training_data(InFiles, GrammarAtom, IgnoredSubdomains, HistoryFile, OutFile, no_old_treebank).

make_ebl_training_data(InFiles, GrammarAtom, IgnoredSubdomains, HistoryFile, OutFile, OldTreebank) :-
	init_make_ebl_training_data(InFiles, HistoryFile, GrammarAtom),
	(   OldTreebank = no_old_treebank ->
	    true
	;
	    ( \+ safe_file_exists(OutFile) ) ->
	    format('~N--- No existing treebank~n', [])
	;
	    incremental_treebanking_off ->
	    format('~N--- Incremental treebanking switched off, not trying to convert treebank~n', [])
	;
	    ( grammar_comparison_available, \+ non_lexical_grammar_rules_changed ) ->
	    update_treebank_from_comparison(OldTreebank, OutFile)
	;
	    ( grammar_comparison_available, non_lexical_grammar_rules_changed ) ->
	    format('~N--- Unable to convert existing treebank, since non-lexical rules changed in grammar~n', [])
	;
	    
	    true
	),		  
	
	timed_call(make_ebl_training_data_main(InFiles, GrammarAtom, IgnoredSubdomains, OutFile, NItems, NBad, NParses),
		   TimeTaken),
	report_on_ebl_training_data_run(NItems, NBad, NParses, TimeTaken),
	update_parsing_history_file(HistoryFile),
	!.

init_make_ebl_training_data(InFiles, HistoryFile, GrammarAtom) :-
	(   \+ grammar_is_loaded ->
	    format('~N~n--- Grammar is not loaded. Unable to create treebank.~n', []),
	    fail
	;
	    true
	),
	(   \+ regulus_preds:top_level_category(GrammarAtom) ->
	    format('~N~n--- The category ~w, defined by the regulus_config(top_level_cat,...) declaration, is not a top-level category in the currently loaded grammar. Unable to create treebank.~n', [GrammarAtom]),
	    fail
	;
	    true
	),
	retractall(old_treebank_updated),
	retractall(updated_from_old_treebank(_)),
	retractall(already_processed_ebl_data(_)),
	retractall(sent_in_current_training_corpus(_, _, _)),

	(   incremental_treebanking_off ->
	    true
	;
	    make_ebl_training_grammar_comparison_data
	),
	
	load_parsing_history_file(HistoryFile),

	store_sents_in_current_training_corpus_files(InFiles),
	!.

report_on_ebl_training_data_run(NItems, _NBad, _NParses, _TimeTaken) :-
	NItems = 0,
	format('~N~n--- There were no new corpus items to parse~n', []),
	!.
report_on_ebl_training_data_run(NItems, NBad, NParses, TimeTaken) :-
	number(NItems),
	number(NBad),
	number(TimeTaken),
	NItems > 0,
	format('~N~n--- ~d items, ~2f secs~n', [NItems, TimeTaken]),
	(   NItems = 0 ->
	    true ;
	    Proportion is ( NBad * 100.0 ) / NItems,
	    format('~N~n--- ~d items failed to parse (~1f%)~n', [NBad, Proportion])
	),
	(   NItems =< NBad ->
	    true ;
	    AverageAmbiguity is NParses / ( NItems - NBad ),
	    format('~N--- Average number of parses for successfully processed examples: ~2f~n', [AverageAmbiguity])
	),
	!.
report_on_ebl_training_data_run(_NItems, _NBad, _NParses, _TimeTaken) :-
	format2error('~N~n*** Error in creating EBL training data~n', []),
	fail.

%---------------------------------------------------------------

update_treebank_from_comparison(OldTreebankFile, TreebankFile) :-
	format('~N~n--- Trying to update treebank using grammar comparison~n', []),
	copy_old_treebank_file(TreebankFile, OldTreebankFile),
	timed_call(update_treebank_from_comparison_main(OldTreebankFile, TreebankFile, NProcessed),
		   TimeTaken),
	assertz(old_treebank_updated),
	format('~N--- Treebank successfully updated using grammar comparison, ~d items, ~1f seconds~n~n',
	       [NProcessed, TimeTaken]),
	!.
update_treebank_from_comparison(OldTreebank, TreebankFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [update_treebank_from_comparison(OldTreebank, TreebankFile)]),
	fail.

%---------------------------------------------------------------

update_treebank_from_comparison_main(OldFile, NewFile, N) :-
	safe_absolute_file_name(OldFile, AbsoluteOldFile),
	safe_absolute_file_name(NewFile, AbsoluteNewFile),

	open(AbsoluteOldFile, read, SIn),
	open_regulus_file(AbsoluteNewFile, write, SOut),

	update_treebank_from_comparison1(SIn, SOut, 0-N),

	close(SIn),
	close(SOut),
	!.
update_treebank_from_comparison_main(OldFile, NewFile, N) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [update_treebank_from_comparison_main(OldFile, NewFile, N)]),
	fail.

update_treebank_from_comparison1(SIn, SOut, NIn-NOut) :-
	safe_read(SIn, T),
	update_treebank_from_comparison2(T, SIn, SOut, NIn-NOut).

update_treebank_from_comparison2(end_of_file, _SIn, _SOut, NIn-NIn) :-
	!.
update_treebank_from_comparison2(T, SIn, SOut, NIn-NOut) :-
	(   non_updatable_treebank_item(T) ->
	    format('-', []),
	    NNext = NIn
	;
	    update_treebank_item_from_comparison(T, T1) ->
	    format(SOut, '~N~q.~n', [T1]),
	    format('.', []),
	    NNext is NIn + 1,
	    mark_treebank_item_as_updated(T)
	),
	flush_output(user),
	!,
	update_treebank_from_comparison1(SIn, SOut, NNext-NOut).

update_treebank_item_from_comparison(TIn, TOut) :-
	replace_line_items_based_on_grammar_comparison(TIn, TNext1),
	replace_annotations_based_on_current_training_corpus(TNext1, TOut).

replace_annotations_based_on_current_training_corpus(TIn, TOut) :-
	TIn = example(GrammarAtom, Tree, LF, Words, _OldAnnotations),
	sent_in_current_training_corpus(Words, Annotations, _),
	TOut = example(GrammarAtom, Tree, LF, Words, Annotations),
	!.
replace_annotations_based_on_current_training_corpus(TIn, TOut) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [replace_annotations_based_on_current_training_corpus(TIn, TOut)]),
	fail.

%---------------------------------------------------------------

non_updatable_treebank_item(Item) :-
	Item = example(_GrammarAtom, _Tree, _LF, Words, _Annotations),
	sent_not_in_current_training_corpus(Words),
	!.
non_updatable_treebank_item(Item) :-
	Item = example(_GrammarAtom, _Tree, _LF, Words, _Annotations),
	words_involve_changed_lexical_items(Words),
	!.

sent_not_in_current_training_corpus(Words) :-
	\+ sent_in_current_training_corpus(Words, _, _),
	!.

words_involve_changed_lexical_items(Words) :-
	changed_lexical_item(Words, _),
	!.
words_involve_changed_lexical_items([_F | R]) :-
	!,
	words_involve_changed_lexical_items(R).	    

%---------------------------------------------------------------
	
mark_treebank_item_as_updated(Item) :-
	Item = example(_GrammarAtom, _Tree, _LF, Words, _Annotations),
	(   updated_from_old_treebank(Words) ->
	    true
	;
	    assertz(updated_from_old_treebank(Words))
	),
	!.
mark_treebank_item_as_updated(Item) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [mark_treebank_item_as_updated(Item)]),
	fail.

ebl_record_updated_from_old_treebank(Record) :-
	compound(Record),
	functor(Record, sent, N),
	N >= 1,
	arg(1, Record, Sent),
	split_atom_into_words(Sent, Words),
	updated_from_old_treebank(Words),
	!.

%---------------------------------------------------------------

make_ebl_training_data_main(InFiles, GrammarAtom, IgnoredSubdomains, OutFile, NItems, NBad, NParses) :-
	safe_absolute_file_name(OutFile, AbsoluteOutFile),

	(   old_treebank_updated ->
	    open_regulus_file(AbsoluteOutFile, append, SOut),
	    format('~N--- Trying to parse any new items not in old treebank~n', [])
	;
	    open_regulus_file(AbsoluteOutFile, write, SOut)
	),
	
	(   make_ebl_training_data_main1(InFiles, GrammarAtom, IgnoredSubdomains, SOut, 0-NItems, 0-NBad, 0-NParses) ->
	    format('~N~n--- Results of parsing written to ~w.~n', [AbsoluteOutFile])
	;
	    format2error('~N~nError: something went wrong when creating file ~w.~n', [AbsoluteOutFile])
	),

	close(SOut).

make_ebl_training_data_main1([], _GrammarAtom, _IgnoredSubdomains, _SOut, NItems-NItems, NBad-NBad, NParses-NParses) :-
	!.
make_ebl_training_data_main1([F | R], GrammarAtom, IgnoredSubdomains, SOut,
			     NItemsIn-NItemsOut, NBadIn-NBadOut, NParsesIn-NParsesOut) :-
	make_ebl_training_data_for_file(F, GrammarAtom, IgnoredSubdomains, SOut,
					NItemsIn-NItemsNext, NBadIn-NBadNext, NParsesIn-NParsesNext),
	!,
	make_ebl_training_data_main1(R, GrammarAtom, IgnoredSubdomains, SOut,
				     NItemsNext-NItemsOut, NBadNext-NBadOut, NParsesNext-NParsesOut).
make_ebl_training_data_main1(File, GrammarAtom, IgnoredSubdomains, SOut,
			     NItemsIn-NItemsOut, NBadIn-NBadOut, NParsesIn-NParsesOut) :-
	\+ is_list(File),
	!,
	make_ebl_training_data_main1([File], GrammarAtom, IgnoredSubdomains, SOut,
				     NItemsIn-NItemsOut, NBadIn-NBadOut, NParsesIn-NParsesOut).

%---------------------------------------------------------------

make_ebl_training_data_for_file(InFile, GrammarAtom, IgnoredSubdomains, SOut, NItemsIn-NItemsOut, NBadIn-NBadOut, NParsesIn-NParsesOut) :-
	absolute_file_name(InFile, AbsoluteInFile),

	(   file_exists(AbsoluteInFile, read) ->
	    format('~N~nParsing corpus data in ~w:~n', [AbsoluteInFile])
	;
	    format2error('~N~nError: could not find file ~w:~n', [AbsoluteInFile]),
	    fail
	),

	open(InFile, read, SIn),
	make_ebl_training_data1(SIn, GrammarAtom, IgnoredSubdomains, SOut,
				0, NItemsIn-NItemsOut, NBadIn-NBadOut, NParsesIn-NParsesOut),
	close(SIn).

make_ebl_training_data1(SIn, GrammarAtom, IgnoredSubdomains, SOut,
			LastLine0, CountIn-CountOut, BadIn-BadOut, ParsesIn-ParsesOut) :-
	LastLine is LastLine0 + 1,
	safe_read(SIn, T),
	line_count(SIn, CurrentLine),
	make_ebl_training_data2(T, SIn, GrammarAtom, IgnoredSubdomains, SOut,
				LastLine-CurrentLine, CountIn-CountOut, BadIn-BadOut, ParsesIn-ParsesOut).
 
make_ebl_training_data2(end_of_file, _SIn, _GrammarAtom, _IgnoredSubdomains, _SOut,
 			_LastLine-_CurrentLine, CountIn-CountIn, BadIn-BadIn, ParsesIn-ParsesIn).
make_ebl_training_data2(T, SIn, GrammarAtom, IgnoredSubdomains, SOut,
			LastLine-CurrentLine, CountIn-CountOut, BadIn-BadOut, ParsesIn-ParsesOut) :-
	make_ebl_training_data_for_item(T, GrammarAtom, IgnoredSubdomains, SOut, LastLine-CurrentLine, GoodOrBad, NParses),
	(   GoodOrBad = ignore ->
	    CountNext = CountIn,
	    BadNext = BadIn,
	    ParsesIn = ParsesNext ;

	    GoodOrBad = bad ->
	    CountNext is CountIn + 1,
	    BadNext is BadIn + 1,
	    ParsesIn = ParsesNext ;

	    CountNext is CountIn + 1,
	    BadNext = BadIn,
	    ParsesNext is ParsesIn + NParses
	),	
	(   ( 0 is CountNext mod 100, CountNext > 0 ) ->
	    
	    format(' (~d) ~n', [CountNext]),
	    flush_output(user) ;
	    
	    true
	),
	!,
	make_ebl_training_data1(SIn, GrammarAtom, IgnoredSubdomains, SOut,
				CurrentLine, CountNext-CountOut, BadNext-BadOut, ParsesNext-ParsesOut).

%---------------------------------------------------------------

make_ebl_training_data_for_item(TIn, _GrammarAtom, _IgnoredSubdomains, _SOut, _LastLine-_CurrentLine, GoodOrBad, NParses) :-
	(   fake_sent_record(TIn)
	;
	    already_processed_ebl_data(TIn)
	;
	    ebl_record_updated_from_old_treebank(TIn)
	),
	!,
	GoodOrBad = ignore,
	NParses = 0.
make_ebl_training_data_for_item(TIn, GrammarAtom, IgnoredSubdomains, SOut, LastLine-CurrentLine, GoodOrBad, NParses) :-
	(   TIn = sent(SentAtom, Annotations, Constraints) ->
	    true ;
	    
	    TIn = sent(SentAtom, Annotations) ->
	    Constraints = [] ;
	    
	    TIn = sent(SentAtom) ->
	    Annotations = [default],
	    Constraints = []
	),
	(   is_list(Annotations) ->
	    
	    list_to_ord_set(Annotations, AnnotationsOS),
	    ord_subtract(AnnotationsOS, IgnoredSubdomains, Annotations1)
	;
	    otherwise ->
	    Annotations1 = Annotations
	),
	TIn1 = sent(SentAtom, Annotations1, Constraints),
	make_ebl_training_data_for_item(TIn1, GrammarAtom, SOut, LastLine-CurrentLine, GoodOrBad, NParses),
	assertz(already_processed_ebl_data(TIn)),
	!.
make_ebl_training_data_for_item(TIn, _GrammarAtom, _IgnoredSubdomains, _SOut, _LastLine-_CurrentLine, GoodOrBad, NParses) :-
	GoodOrBad = ignore,
	NParses = 0,
	format('~N*** Warning: bad record ignored: "~w"~n', [TIn]).

make_ebl_training_data_for_item(TIn, _GrammarAtom, _SOut, _LastLine-_CurrentLine, GoodOrBad, NParses) :-
	TIn = sent(_SentAtom, Annotations, _Constraints),
	(   Annotations = [ignore] ;
	    Annotations = []
	),
	!,
	GoodOrBad = ignore,
	NParses = 0,
	!.
make_ebl_training_data_for_item(TIn, GrammarAtom, SOut, LastLine-CurrentLine, GoodOrBad, NParses) :-
	TIn = sent(SentAtom, Annotations, Constraints),
	atom_codes(SentAtom, String),
	split_string_into_words(String, WordList),
	(   words_to_lf_and_tree_with_current_parser_respecting_constraints(WordList, GrammarAtom, Constraints, LF, Tree, NParses) ->

	    TOut = example(GrammarAtom, Tree, LF, WordList, Annotations),
	    GoodOrBad = good,
	    update_parsing_history_after_successful_parse(SentAtom, NParses),
	    format('.', []) ;

	    TOut = example(GrammarAtom, 'NO_PARSE', 'NO_LF', WordList, Annotations),
	    GoodOrBad = bad,
	    NParses = 0,
	    warn_and_report_parsing_history_after_unsuccessful_parse(SentAtom, LastLine-CurrentLine)
	),
	format(SOut, '~N~q.~n', [TOut]),
	%format(SOut, '~N', []),
	% Indent = 0, Width = 120
	%prettyprintq_to_stream(SOut, TOut, 0, 120),
	%format(SOut, '.~n', []),
	flush_output(user),
	!.
make_ebl_training_data_for_item(TIn, GrammarAtom, _SOut, _LineInfo, _GoodOrBad, _NParses) :-
	format('~N*** Internal error for: ~w, ~w~n', [TIn, GrammarAtom]).

%---------------------------------------------------------------

:- dynamic last_parsed/3.

load_parsing_history_file(HistoryFile) :-
	retractall(last_parsed(_, _, _)),
	absolute_file_name(HistoryFile, AbsHistoryFile),
	(   file_exists(AbsHistoryFile) ->
	    
	    prolog_file_to_list(AbsHistoryFile, HistoryList),
	    length(HistoryList, N),
	    load_parsing_history_list(HistoryList),
	    format('~N~n--- Read parsing history file (~d records) ~w~n', [N, AbsHistoryFile]) ;

	    format('~N~n*** Warning: couldn\'t find parsing history file ~w~n', [AbsHistoryFile])
	),
	!.
load_parsing_history_file(HistoryFile) :-
	format2error('~N*** Error: bad call: ~w~n', [load_parsing_history_file(HistoryFile)]),
	fail.

load_parsing_history_list([]).
load_parsing_history_list([F | R]) :-
	load_parsing_history_item(F),
	!,
	load_parsing_history_list(R).

load_parsing_history_item(last_parsed(Sent, Datime, NParses)) :-
	assertz(last_parsed(Sent, Datime, NParses)),
	!.
load_parsing_history_item(last_parsed(Sent, Datime)) :-
	NParses = unknown,
	assertz(last_parsed(Sent, Datime, NParses)),
	!.
load_parsing_history_item(Other) :-
	format2error('~N*** Error: unknown item in history file: ~w~n', [Other]),
	fail.

%---------------------------------------------------------------

show_parse_history_for_string(Pattern, HistoryFile) :-
	load_parsing_history_file(HistoryFile),
	show_parse_history_for_string1(Pattern),
	!.

% Typical record:
%
% last_parsed('the early morning', datime(2008,12,13,18,34,51), 2).

show_parse_history_for_string1(Pattern) :-
	findall(last_parsed(SentAtom, Datime, NParses),
		sent_atom_matching_search_pattern(Pattern, SentAtom, Datime, NParses),
		List0),
	sort(List0, List),
	!,
	length(List, N),
	(   N = 0 ->
	    format('~N~nFound no records matching pattern~n~n', [])
	;
	    N = 1 ->
	    format('~N~nFound one record matching pattern~n~n', [])
	;
	    true ->
	    format('~N~nFound ~d records matching pattern~n~n', [N])
	),	
	show_parse_history_records(List).

show_parse_history_records([]).
show_parse_history_records([F | R]) :-
	show_parse_history_record(F),
	!,
	show_parse_history_records(R).

show_parse_history_record(last_parsed(SentAtom, Datime, NParses)) :-
	datime_to_timestamp(Datime, Timestamp),
	format('~N~w ~d ~w~n', [Timestamp, NParses, SentAtom]),
	!.

sent_atom_matching_search_pattern(Pattern, SentAtom, Datime, NParses) :-
	last_parsed(SentAtom, Datime, NParses),
	split_atom_into_words(SentAtom, SentWords),
	sent_words_match_search_pattern(Pattern, SentWords).

sent_words_match_search_pattern([], _Words).
sent_words_match_search_pattern([FPat | RPat], [FPat | RWords]) :-
	sent_words_match_search_pattern(RPat, RWords).
sent_words_match_search_pattern(['*' | RPat], [_Word | RWords]) :-
	sent_words_match_search_pattern(RPat, RWords).
sent_words_match_search_pattern(['*' | RPat], [_Word | RWords]) :-
	sent_words_match_search_pattern(['*' | RPat], RWords).
sent_words_match_search_pattern(['*' | RPat], Words) :-
	sent_words_match_search_pattern(RPat, Words).
sent_words_match_search_pattern(Pat, [_Word | RWords]) :-
	sent_words_match_search_pattern(Pat, RWords).
	      
%---------------------------------------------------------------

update_parsing_history_after_successful_parse(SentAtom, NParses) :-
	retractall(last_parsed(SentAtom, _, _)),
	datime(Datime),
	assertz(last_parsed(SentAtom, Datime, NParses)),
	!.
update_parsing_history_after_successful_parse(SentAtom, NParses) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [update_parsing_history_after_successful_parse(SentAtom, NParses)]),
	fail.

%---------------------------------------------------------------

warn_and_report_parsing_history_after_unsuccessful_parse(SentAtom, LastLine-CurrentLine) :-
	format('~N*** Parsing failed for: "~w", ', [SentAtom]),
	(   LastLine = CurrentLine ->
	    format('line ~d ', [LastLine]) ;
	    format('lines ~d-~d ', [LastLine, CurrentLine])
	),
	(   last_parsed(SentAtom, LastDatime, _) ->

	    datime_to_timestamp(LastDatime, Timestamp),
	    format('(last produced parse, ~w)~n', [Timestamp]) ;

	    format('~n', [])
	),
	report_unknown_words_in_sent_atom(SentAtom).
warn_and_report_parsing_history_after_unsuccessful_parse(SentAtom, LineInfo) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [warn_and_report_parsing_history_after_unsuccessful_parse(SentAtom, LineInfo)]),
	fail.

%---------------------------------------------------------------

update_parsing_history_file(HistoryFile) :-
	absolute_file_name(HistoryFile, AbsHistoryFile),
	findall(last_parsed(SentAtom, Datime, NParses), last_parsed(SentAtom, Datime, NParses), HistoryList),
	list_to_prolog_file(HistoryList, AbsHistoryFile),
	length(HistoryList, N),
	load_parsing_history_list(HistoryList),
	format('~N~n--- Written parsing history file (~d records) ~w~n', [N, AbsHistoryFile]).
update_parsing_history_file(HistoryFile) :-
	format2error('~N*** Error: bad call: ~w~n', [update_parsing_history_file(HistoryFile)]),
	fail.

%---------------------------------------------------------------

store_sents_in_current_training_corpus_files([]) :-
	!.
store_sents_in_current_training_corpus_files(File) :-
	\+ is_list(File),
	store_sents_in_current_training_corpus_file(File),
	!.
store_sents_in_current_training_corpus_files([F | R]) :-
	store_sents_in_current_training_corpus_file(F),
	!,
	store_sents_in_current_training_corpus_files(R).
store_sents_in_current_training_corpus_files(Other) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [store_sents_in_current_training_corpus_files(Other)]),
	fail.

store_sents_in_current_training_corpus_file(File) :-
	safe_absolute_file_name(File, AbsFile),
	prolog_file_to_list(AbsFile, List),
	store_sents_in_current_training_corpus_file1(List),
	!.
store_sents_in_current_training_corpus_file(File) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [store_sents_in_current_training_corpus_file(File)]),
	fail.

store_sents_in_current_training_corpus_file1([]).
store_sents_in_current_training_corpus_file1([F | R]) :-
	store_sent_in_current_training_corpus_file1(F),
	!,
	store_sents_in_current_training_corpus_file1(R).

store_sent_in_current_training_corpus_file1(Record) :-
	compound(Record),
	functor(Record, sent, N),
	N > 0,
	arg(1, Record, Sent),
	split_atom_into_words(Sent, Words),
	\+ sent_in_current_training_corpus(Words, _, _),
	(   N > 1 ->
	    arg(2, Record, Tags)
	;
	    otherwise ->
	    Tags = [default]
	),
	(   N > 2 ->
	    arg(3, Record, Constraints)
	;
	    otherwise ->
	    Constraints = []
	),
	assertz(sent_in_current_training_corpus(Words, Tags, Constraints)),
	!.
store_sent_in_current_training_corpus_file1(_Record).

%---------------------------------------------------------------

words_to_lf_and_tree_with_current_parser_respecting_constraints(WordList, GrammarAtom, Constraints, LF, Tree, NParses) :-
	findall([Tree0, SynFeats0, Local0, Global0],
		parse_with_current_parser1(GrammarAtom, WordList, Tree0, SynFeats0, Local0, Global0),
		Tuples0),
	safe_remove_duplicates_preserving_order(Tuples0, Tuples),
	reorder_parse_tuples_using_preferences(Tuples, ReorderedTuples),	
	length(ReorderedTuples, NParses),
	(   get_analysis_respecting_constraints_from_reordered_tuples(ReorderedTuples, Constraints, LF, Tree) ->
	    true
	;
	    NParses > 0 ->
	    format('~N*** Warning: parses found, but all blocked by constraints: ~q~n', [Constraints]),
	    flush_output(user),
	    fail
	),
	!.

get_analysis_respecting_constraints_from_reordered_tuples(ReorderedTuples, Constraints, LF, Tree) :-
	member([Tree, _SynFeats, Local, Global], ReorderedTuples),
	real_lf_out_of_local_and_global(Local, Global, LF),
	parse_tree_to_summary(Tree, TreeSummary),
	check_analysis_constraints(Constraints, [lf=LF, tree=TreeSummary]).

%---------------------------------------------------------------

make_ebl_training_grammar_comparison_data :-
	init_store_grammar_comparison,
	fail.
make_ebl_training_grammar_comparison_data :-
	parse_preferences_have_changed,
	format('~N*** Parse preferences have changed, not safe to use treebank caching~n', []),
	!.
make_ebl_training_grammar_comparison_data :-
	get_regulus_config_item(reflective_dcg_grammar, ReflectiveDCGGrammarFile),
	get_regulus_config_item(reflective_dcg_grammar_for_treebank, ReflectiveDCGGrammarFileForTreebank),
	(   \+ safe_file_exists(ReflectiveDCGGrammarFileForTreebank) ->
	    format('~N*** No saved grammar from previous training run, unable to compare with current grammar~n', [])
	;
	    \+ safe_file_exists(ReflectiveDCGGrammarFile) ->
	    format('~N*** No current grammar, unable to compare with saved grammar from previous training run~n', [])
	;
	    otherwise ->
	    store_grammar_comparison(ReflectiveDCGGrammarFileForTreebank, ReflectiveDCGGrammarFile)
	),
	!.
make_ebl_training_grammar_comparison_data :-
	format2error('~N~n*** Error in make_ebl_training_grammar_comparison_data/0~n', []),
	fail.

%---------------------------------------------------------------

parse_preferences_have_changed :-
	get_old_parse_preferences(OldPreferencesOS),
	get_new_parse_preferences(NewPreferencesOS),
	!,
	(   OldPreferencesOS = NewPreferencesOS ->
	    fail
	;
	    true
	).
parse_preferences_have_changed :-
	format2error('~N~n*** Error in parse_preferences_have_changed/0~n', []),
	fail.

get_old_parse_preferences(PreferencesOS) :-
	regulus_config(parse_preferences, ParsePreferencesFile),
	get_parse_preferences_from_file(ParsePreferencesFile, PreferencesOS),
	!.
get_old_parse_preferences([]).

get_new_parse_preferences(PreferencesOS) :-
	get_regulus_config_item(parse_preferences_for_treebank, ParsePreferencesFile),
	get_parse_preferences_from_file(ParsePreferencesFile, PreferencesOS),
	!.
get_new_parse_preferences([]).

get_parse_preferences_from_file(ParsePreferencesFile, PreferencesOS) :-
	safe_absolute_file_name(ParsePreferencesFile, AbsParsePreferencesFile),
	safe_file_exists(AbsParsePreferencesFile),
	prolog_file_to_list(AbsParsePreferencesFile, Preferences),
	list_to_ord_set(Preferences, PreferencesOS),
	!.
get_parse_preferences_from_file(_ParsePreferencesFile, PreferencesOS) :-
	PreferencesOS = [].

%---------------------------------------------------------------

copy_old_treebank_file(InFile, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),
	format('~N--- Copying old treebank file ~w to ~w... ', [AbsInFile, AbsOutFile]),
	flush_output(user),
	timed_call(copy_file(InFile, OutFile),
		   TimeTaken),
	format('done (~1f seconds)~n', [TimeTaken]),
	!.
copy_old_treebank_file(InFile, OutFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [copy_old_treebank_file(InFile, OutFile)]),
	fail.

%---------------------------------------------------------------

make_sent_annotations_canonical(Annotations, AnnotationsOS) :-
	(   is_list(Annotations) ->
	    list_to_ord_set(Annotations, AnnotationsOS)
	;
	    otherwise ->
	    Annotations = AnnotationsOS
	).

