
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(translate_ngram_corpora,
	  [ngram_corpora_to_translation_input_file/2,
	   ngram_corpora_and_translation_output_to_comparison_files/8,

	   test_translate_ngram_corpora/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

test_translate_ngram_corpora(clitic_bigrams_to_translation_file) :-
	ngram_corpora_to_translation_input_file(['$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev.pl',
						 %'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_tu_vous.pl',
						 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_remove.pl',
						 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_ça.pl',
						 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_ceci.pl',
						 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_direct_object.pl',
						 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_indirect_object.pl'],
						
						'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_translation_input.csv').

test_translate_ngram_corpora(plus_or_quoi_bigrams_to_translation_file) :-
	ngram_corpora_to_translation_input_file(['$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev.pl',
						 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_pas.pl'],
						
						'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_translation_input.csv').
	
test_translate_ngram_corpora(plus_or_quoi_bigrams_to_translation_file2) :-
	ngram_corpora_to_translation_input_file(['$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev2.pl',
						 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_pas2.pl'],
						
						'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_translation_input2.csv').
	
test_translate_ngram_corpora(clitic_bigrams_to_comparison_file) :-
	ngram_corpora_and_translation_output_to_comparison_files(%OriginalNGramFile
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev.pl',
								 %'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_tu_vous.pl', 
								 %VariantNGramFiles
								 [remove-'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_remove.pl',
								  ça-'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_ça.pl',
								  ceci-'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_ceci.pl',
								  direct_object-'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_direct_object.pl',
								  indirect_object-'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_indirect_object.pl'],  
								 %TranslationOutputFile
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_translation_output.csv',
								 %SourceFile
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_source.txt',
								 %OriginalTranslationFile,
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_original_target.txt',
								 %VariantTranslationFiles
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_variant_target.txt',
								 %Human comparison
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_comparison.txt',

								 %Machine-readable associations
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_comparison.pl'
								).

test_translate_ngram_corpora(plus_or_quoi_to_comparison_file) :-
	ngram_corpora_and_translation_output_to_comparison_files(%OriginalNGramFile
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev.pl', 
								 %VariantNGramFiles
								 [plus_to_pas-'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_pas.pl'],  
								 %TranslationOutputFile
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_translation_output.csv',
								 %SourceFile
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_source.txt',
								 %OriginalTranslationFile,
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_original_target.txt',
								 %VariantTranslationFiles
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_variant_target.txt',
								 %Human comparison
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_comparison.txt',
								 %Machine-readable associations
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_comparison.pl'
								).

test_translate_ngram_corpora(plus_or_quoi_to_comparison_file2) :-
	ngram_corpora_and_translation_output_to_comparison_files(%OriginalNGramFile
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev2.pl', 
								 %VariantNGramFiles
								 [plus_to_pas-'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_pas2.pl'],  
								 %TranslationOutputFile
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_translation_output2.csv',
								 %SourceFile
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_source2.txt',
								 %OriginalTranslationFile,
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_original_target2.txt',
								 %VariantTranslationFiles
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_variant_target2.txt',
								 %Human comparison
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_comparison2.txt',
								 %Machine-readable associations
								 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_comparison2.pl'
								).

%---------------------------------------------------------------

/*
Take a list of ngram corpus files, typical lines

ngram_example([lui,avais],1,'Je viens d\'aller voir en assistance chez un contact et je avais à lui désactivé.')
ngram_example([en,fais],'*** NOT USED ***','Je n\'hésiterai pas, ne t\'en fais pas !').

Write out a one-column CSV file in UTF-8 containing all the examples whose second argument is not '*** NOT USED ***'.
Remove duplicates.
*/

ngram_corpora_to_translation_input_file(NGramCorpusFiles, FileForTranslation) :-
	read_ngram_corpus_files(NGramCorpusFiles, NGramCorpusLists),
	get_examples_from_ngram_corpus_lists(NGramCorpusLists, Examples),
	write_out_translation_input_file(Examples, FileForTranslation).

/*
Take:

- the original ngram corpus file (format as above)
- a list of variant corpus files (format as above)
- a translation output file (one-column CSV file in UTF-8)

Produce three parallel files:
- a text file of source sentences
- a text file of translations produced from the original source sentences
- a text file of translations produced from the different variant translations
*/

ngram_corpora_and_translation_output_to_comparison_files(OriginalNGramFile, VariantNGramFiles, TranslationOutputFile,
							 SourceFile, OriginalTranslationFile, VariantTranslationFile,
							 HumanComparisonFile, ComparisonFile) :-
	read_tagged_ngram_corpus_files([original-OriginalNGramFile | VariantNGramFiles], [OriginalList | VariantLists]),
	read_and_store_translation_output_file(TranslationOutputFile),
	add_translations_to_ngram_lists([OriginalList | VariantLists], [OriginalListWithTranslations | VariantListsWithTranslations]),
	create_source_trans_variant_trans_tuples(OriginalListWithTranslations, VariantListsWithTranslations, Tuples-[]),
	write_out_source_trans_variant_trans_files(Tuples, SourceFile, OriginalTranslationFile, VariantTranslationFile),
	write_out_source_trans_variant_trans_comparison_file(Tuples, ComparisonFile, pl),
	write_out_source_trans_variant_trans_comparison_file(Tuples, HumanComparisonFile, txt).

%---------------------------------------------------------------

read_ngram_corpus_files([], []).
read_ngram_corpus_files([F | R], [F1 | R1]) :-
	safe_absolute_file_name(F, AbsF),
	safe_prolog_file_to_list_printing_statistics(AbsF, F1, 'UTF-8'),
	!,
	read_ngram_corpus_files(R, R1).
read_ngram_corpus_files(X, Y) :-
	format('~N*** Error: bad call: ~w~n', [read_ngram_corpus_files(X, Y)]),
	fail.

get_examples_from_ngram_corpus_lists(NGramCorpusLists, Examples) :-
	findall(Example,
		(   member(List, NGramCorpusLists),
		    member(ngram_example(_Ngram, Multiplicity, Example), List),
		    Multiplicity \== '*** NOT USED ***'
		),
		Examples0),
	sort(Examples0, Examples).			   

write_out_translation_input_file(Examples, File) :-
	length(Examples, N),
	safe_absolute_file_name(File, AbsFile),
	open(AbsFile, write, S, [encoding('UTF-8')]),
	write_out_translation_input_file1(Examples, S),
	close(S),
	format('~N--- Written translation input file (~d elements) ~w~n', [N, AbsFile]),
	!.

write_out_translation_input_file1([], _S).
write_out_translation_input_file1([F | R], S) :-
	format(S, '~N"~w"~n', [F]),
	!,
	write_out_translation_input_file1(R, S).

%---------------------------------------------------------------

read_tagged_ngram_corpus_files([], []).
read_tagged_ngram_corpus_files([Tag-F | R], [Tag-F1 | R1]) :-
	safe_absolute_file_name(F, AbsF),
	safe_prolog_file_to_list_printing_statistics(AbsF, F1, 'UTF-8'),
	!,
	read_tagged_ngram_corpus_files(R, R1).
read_tagged_ngram_corpus_files(X, Y) :-
	format('~N*** Error: bad call: ~w~n', [read_tagged_ngram_corpus_files(X, Y)]),
	!,
	fail.

:- dynamic stored_translation/2.

read_and_store_translation_output_file(TranslationOutputFile) :-
	retractall(stored_translation(_, _)),
	safe_absolute_file_name(TranslationOutputFile, AbsTranslationOutputFile),
	csv_file_to_list_of_lists(AbsTranslationOutputFile, 'UTF-8', 0'", 0';, List), %"
	store_translation_output_file(List, 0-NStored),
	length(List, NRecords),
	format('~N--- Stored ~d translations (~d records) from ~w~n', [NStored, NRecords, AbsTranslationOutputFile]),
	!.

store_translation_output_file([], N-N).
store_translation_output_file([F | R], In-Out) :-
	store_translation_output_file_item(F, In-Next),
	!,
	store_translation_output_file(R, Next-Out).

store_translation_output_file_item([Source, Target], In-Out) :-
	assertz(stored_translation(Source, Target)),
	Out is In + 1,
	!.
store_translation_output_file_item(_Other, In-In).

add_translations_to_ngram_lists([], []).
add_translations_to_ngram_lists([Tag-F | R], [Tag-F1 | R1]) :-
	add_translations_to_ngram_list(F, F1),
	!,
	add_translations_to_ngram_lists(R, R1).

add_translations_to_ngram_list([], []).
add_translations_to_ngram_list([F | R], [F1 | R1]) :-
	add_translation_to_ngram_record(F, F1),
	!,
	add_translations_to_ngram_list(R, R1).

add_translation_to_ngram_record(ngram_example(Ngram, '*** NOT USED ***', Source),
				ngram_example_with_translation(Ngram, '*** NOT USED ***', Source, '*** NONE ***')) :-
	!.
add_translation_to_ngram_record(ngram_example(Ngram, Multiplicity, Source),
				ngram_example_with_translation(Ngram, Multiplicity, Source, Target)) :-
	Multiplicity \== '*** NOT USED ***',
	(   stored_translation(Source, Target) ->
	    true
	;
	    otherwise ->
	    Target = '*** NONE ***',
	    format('~N*** Warning: no stored translation for "~w"~n', [Source])
	),
	!.
add_translation_to_ngram_record(F, F1) :-
	format('~N*** Error: bad call: ~w~n', [add_translation_to_ngram_record(F, F1)]),
	fail.

create_source_trans_variant_trans_tuples(_SourceTag-_OriginalListWithTranslations, [], Tuples-Tuples).
create_source_trans_variant_trans_tuples(SourceTag-OriginalListWithTranslations, [Tag-VariantList | VariantLists], TuplesIn-TuplesOut) :-
	create_source_trans_variant_trans_tuples1(OriginalListWithTranslations, Tag, VariantList, TuplesIn-TuplesNext),
	!,
	create_source_trans_variant_trans_tuples(SourceTag-OriginalListWithTranslations, VariantLists, TuplesNext-TuplesOut).

create_source_trans_variant_trans_tuples1(OriginalListWithTranslations, Tag, VariantListWithTranslations, TuplesIn-TuplesOut) :-
	length(OriginalListWithTranslations, NOriginal),
	length(VariantListWithTranslations, NVariant),
	(   NOriginal = NVariant ->
	    true
	;
	    otherwise ->
	    format('~N*** Error: different numbers of records in original and variant lists~n', []),
	    fail
	),
	create_source_trans_variant_trans_tuples2(OriginalListWithTranslations, VariantListWithTranslations, Tag, TuplesIn-TuplesOut).

create_source_trans_variant_trans_tuples2([], [], _Tag, TuplesIn-TuplesIn).
create_source_trans_variant_trans_tuples2([F | R], [F1 | R1], Tag, [F2 | TuplesNext]-TuplesOut) :-
	source_trans_variant_trans_tuple(F, F1, Tag, F2),
	!,
	create_source_trans_variant_trans_tuples2(R, R1, Tag, TuplesNext-TuplesOut).
create_source_trans_variant_trans_tuples2([_F | R], [_F1 | R1], Tag, TuplesIn-TuplesOut) :-
	!,
	create_source_trans_variant_trans_tuples2(R, R1, Tag, TuplesIn-TuplesOut).

source_trans_variant_trans_tuple(ngram_example_with_translation(Ngram, Multiplicity1, OriginalSource, OriginalTarget),
				 ngram_example_with_translation(Ngram, Multiplicity2, VariantSource, VariantTarget),
				 Tag, 
				 [Ngram, OriginalSource, Tag, OriginalTarget, VariantSource, VariantTarget]) :-
	Multiplicity1 \== '*** NOT USED ***',
	Multiplicity2 \== '*** NOT USED ***',
	!.

write_out_source_trans_variant_trans_files(Tuples, SourceFile, OriginalTranslationFile, VariantTranslationFile) :-
	safe_absolute_file_name(SourceFile, AbsSourceFile),
	safe_absolute_file_name(OriginalTranslationFile, AbsOriginalTranslationFile),
	safe_absolute_file_name(VariantTranslationFile, AbsVariantTranslationFile),

	open(AbsSourceFile, write, SSource, [encoding('UTF-8')]),
	open(AbsOriginalTranslationFile, write, SOrig, [encoding('UTF-8')]),
	open(AbsVariantTranslationFile, write, SVariant, [encoding('UTF-8')]),

	write_out_source_trans_variant_trans_files1(Tuples, SSource, SOrig, SVariant),

	close(SSource),
	close(SOrig),
	close(SVariant),

	length(Tuples, N),
	format('~N--- Written files (~d records):~n', [N]),
	format('~N--- Source: ~w~n', [AbsSourceFile]),
	format('~N--- Original: ~w~n', [AbsOriginalTranslationFile]),
	format('~N--- Variants: ~w~n', [AbsVariantTranslationFile]),
	!.

write_out_source_trans_variant_trans_files1([], _SSource, _SOrig, _SVariant).
write_out_source_trans_variant_trans_files1([F | R], SSource, SOrig, SVariant) :-
	write_out_source_trans_variant_trans_record(F, SSource, SOrig, SVariant),
	!,
	write_out_source_trans_variant_trans_files1(R, SSource, SOrig, SVariant).

write_out_source_trans_variant_trans_record(Tuple, SSource, SOrig, SVariant) :-
	Tuple = [_Ngram, OriginalSource, _Tag, OriginalTarget, _VariantSource, VariantTarget],
	format(SSource, '~N~w~n', [OriginalSource]),
	format(SOrig, '~N~w~n', [OriginalTarget]),
	format(SVariant, '~N~w~n', [VariantTarget]),
	!.
write_out_source_trans_variant_trans_record(Tuple, SSource, SOrig, SVariant) :-
	format('~N*** Error: bad call: ~w~n', [write_out_source_trans_variant_trans_record(Tuple, SSource, SOrig, SVariant)]),
	fail.

write_out_source_trans_variant_trans_comparison_file(Tuples, File, Mode) :-
	safe_absolute_file_name(File, AbsFile),
	open(AbsFile, write, S, [encoding('UTF-8')]),
	sort(Tuples, SortedTuples),
	write_out_source_trans_variant_trans_comparison_file1(SortedTuples, S, '*no-ngram*', '*no-source*', Mode),
	close(S),
	length(Tuples, N),
	format('~N--- Written file (~d records):~w~n', [N, AbsFile]).

write_out_source_trans_variant_trans_comparison_file1([], _S, _LastNgram, _LastSource, _Mode).
write_out_source_trans_variant_trans_comparison_file1([F | R], S, LastNGram, LastSource, Mode) :-
	write_out_source_trans_variant_trans_comparison_file_record(F, S, LastNGram, LastSource, ThisNGram, ThisSource, Mode),
	!,
	write_out_source_trans_variant_trans_comparison_file1(R, S, ThisNGram, ThisSource, Mode).

write_out_source_trans_variant_trans_comparison_file_record(Tuple, S, _LastNGram, _LastSource, Ngram, OriginalSource, pl) :-
	Tuple = [Ngram, OriginalSource, Tag, OriginalTarget, VariantSource, VariantTarget],
	Record = variant_translation_with_details(OriginalSource, OriginalTarget, VariantSource, VariantTarget, Ngram, Tag),
	format(S, '~N~q.~n', [Record]),
	!.
write_out_source_trans_variant_trans_comparison_file_record(Tuple, S, LastNGram, LastSource, Ngram, OriginalSource, txt) :-
	Tuple = [Ngram, OriginalSource, Tag, OriginalTarget, VariantSource, VariantTarget],
	( LastNGram \== Ngram ;  LastSource \== OriginalSource ),
	format(S, '~N~n', []),
	format(S, '~N          NGram: ~w', [Ngram]),
	format(S, '~N         Source: ~w', [OriginalSource]),
	format(S, '~NOriginal target: ~w', [OriginalTarget]),
	format(S, '~N Variant source: ~w (transform: ~w)', [VariantSource, Tag]),
	format(S, '~N Variant target: ~w (transform: ~w)', [VariantTarget, Tag]),
	!.
write_out_source_trans_variant_trans_comparison_file_record(Tuple, S, LastNGram, LastSource, LastNGram, LastSource, txt) :-
	Tuple = [LastNGram, LastSource, Tag, _OriginalTarget, VariantSource, VariantTarget],
	format(S, '~N Variant source: ~w (transform: ~w)', [VariantSource, Tag]),
	format(S, '~N Variant target: ~w (transform: ~w)', [VariantTarget, Tag]),
	!.
write_out_source_trans_variant_trans_comparison_file_record(Tuple, S, Mode) :-
	format('~N*** Error: bad call: ~w~n', [write_out_source_trans_variant_trans_comparison_file_record(Tuple, S, Mode)]),
	fail.
	
