:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(extract_interesting_examples,
	  [extract_interesting_examples/5,
	   extract_interesting_examples/6,
	   
	   split_ngrams_example_file/6,
	   split_ngrams_example_file/4,
	   
	   test_extract_interesting_examples/1,
	   test_split_ngrams_example_file/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/CorpusTools/tokenize_sents').
:- use_module('$REGULUS/PrologLib/CorpusTools/utils').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(random)).
:- use_module(library(lists)).

%---------------------------------------------------------------

test_extract_interesting_examples(forum_tm_fr_plus_europarl) :-
	extract_interesting_examples('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered.pl',
				     500,
				     5,
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples.txt').

test_extract_interesting_examples(forum_tm_fr_plus_europarl_bigrams) :-
	extract_interesting_examples('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_bigrams.pl',
				     20,
				     3,
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples_bigrams.txt').

test_extract_interesting_examples(forum_tm_fr_plus_europarl_bigrams_csv) :-
	extract_interesting_examples('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_bigrams.pl',
				     20,
				     3,
				     csv,
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples_bigrams.csv').

test_extract_interesting_examples(forum_tm_fr_plus_europarl_bigrams_clitics_csv) :-
	extract_interesting_examples('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_bigrams_clitics.pl',
				     10,
				     5,
				     csv,
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples_bigrams_clitics.csv').

test_extract_interesting_examples(forum_tm_fr_plus_europarl_bigrams_clitics_pl) :-
	extract_interesting_examples('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_bigrams_clitics.pl',
				     5,
				     10000,
				     pl,
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples_bigrams_clitics.pl').

test_extract_interesting_examples(forum_tm_fr_plus_europarl_bigrams_plus_or_quoi_csv) :-
	extract_interesting_examples('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_bigrams_plus_or_quoi.pl',
				     %10,
				     0,
				     10000,
				     csv,
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples_bigrams_plus_or_quoi.csv').

test_extract_interesting_examples(forum_tm_fr_plus_europarl_bigrams_plus_or_quoi_pl) :-
	extract_interesting_examples('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_bigrams_plus_or_quoi.pl',
				     %10,
				     0,
				     10000,
				     pl,
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples_bigrams_plus_or_quoi.pl').

test_extract_interesting_examples(forum_tm_fr_plus_europarl_trigrams_plus_or_quoi_csv) :-
	extract_interesting_examples('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_trigrams_plus_or_quoi.pl',
				     %10,
				     0,
				     5,
				     csv,
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples_trigrams_plus_or_quoi.csv').
			     
test_extract_interesting_examples(forum_tm_fr_plus_europarl_bigrams_jamais_csv) :-
	extract_interesting_examples('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_bigrams_jamais.pl',
				     %10,
				     0,
				     5,
				     csv,
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples_bigrams_jamais.csv').
			     
test_extract_interesting_examples(forum_tm_fr_plus_europarl_trigrams_jamais_csv) :-
	extract_interesting_examples('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_trigrams_jamais.pl',
				     %10,
				     0,
				     5,
				     csv,
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples_trigrams_jamais.csv').
			     
test_extract_interesting_examples(forum_tm_fr_plus_europarl_bigrams_on_csv) :-
	extract_interesting_examples('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_bigrams_on.pl',
				     %10,
				     0,
				     5,
				     csv,
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples_bigrams_on.csv').
			     
test_extract_interesting_examples(forum_tm_fr_plus_europarl_trigrams) :-
	extract_interesting_examples('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_trigrams.pl',
				     10,
				     3,
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples_trigrams.txt').

test_extract_interesting_examples(forum_tm_fr_plus_europarl_trigrams_csv) :-
	extract_interesting_examples('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_trigrams.pl',
				     10,
				     3,
				     csv,
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples_trigrams.csv').
				     
test_extract_interesting_examples(forum_tm_fr_plus_europarl_good_trigrams) :-
	extract_interesting_examples('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_trigrams.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/good_trigrams.pl',
				     10,
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples_good_trigrams.txt').

test_extract_interesting_examples(forum_tm_fr_plus_europarl_trigrams_csv) :-
	extract_interesting_examples('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_trigrams.pl',
				     10,
				     2,
				     csv,
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples_trigrams.csv').
					     
test_extract_interesting_examples(forum_tm_fr_plus_europarl_good_trigrams_csv) :-
	extract_interesting_examples('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_trigrams.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/good_trigrams.pl',
				     10,
				     csv,
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples_good_trigrams.csv').
					     
test_extract_interesting_examples(forum_tm_fr_plus_europarl_small) :-
	extract_interesting_examples('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised_small.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl.pl',
				     2500,
				     5,
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples_small.txt').				     

test_extract_interesting_examples(forum_tm_fr_plus_europarl_very_small) :-
	extract_interesting_examples('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised_very_small.pl',
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl.pl',
				     2500,
				     5,
				     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples_very_small.txt').
				     
test_split_ngrams_example_file(bigrams_clitics) :-
	split_ngrams_example_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples_bigrams_clitics.pl',
				  3,
				  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev.pl',
				  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_test.pl').

test_split_ngrams_example_file(plus_or_quoi) :-
	split_ngrams_example_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_examples_bigrams_plus_or_quoi.pl',
				  3,
				  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev.pl',
				  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_test.pl').
	
test_split_ngrams_example_file(plus_or_quoi_potentially_interesting) :-
	split_ngrams_example_file('$ACCEPT/MT/GTFeb2012/AMTResults/plus_to_pas_v1_summary.pl',
				  Record^(   member(ngram_score=NgramScore, Record),
					     member(freq2=Freq2, Record),
					     (   NgramScore > 4.0
					     ;
						 Freq2 < 0.001
					     )
					 ),
				  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_test.pl',
				  5,
				  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev2.pl',
				  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_test2.pl').

			  
%---------------------------------------------------------------

/*

split_ngrams_example_file(AMTSummaryFile, ExtractionPred, InFile, NExamples, DevFile, TestFile)				  

split_ngrams_example_file(InFile, NExamples, DevFile, TestFile)


AMTSummaryFile is a Prolog-formatted UTF-8 file produced by crowdsource_translations:amt_translation_judgements_file_to_prolog/6

ExtractionPred is a predicate that identifies potentially relevant ngrams

If the first two args are defined, only examples with potentially relevant ngrams will be used.

InFile is a file of Prolog-formatted ngram examples with multiplicities. Typical example:

ngram_example([vous,servez],5,'Si vous vous servez de Identity Safe, avant mise à jour, faire une sauvegarde des identifiants.').

DevFile takes at most NExamples examples from each group in InFile to make DevFile, a text file of examples.

TestFile has the remaining examples, same format as InFile

*/

split_ngrams_example_file(InFile, NExamples, DevFile, TestFile) :-
	split_ngrams_example_file('*no_file*', '*no_extraction_pred*', InFile, NExamples, DevFile, TestFile).

split_ngrams_example_file(AMTSummaryFile, ExtractionPred, InFile, NExamples, DevFile, TestFile) :-
	mark_potentially_relevant_ngrams(AMTSummaryFile, ExtractionPred),
				     
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(DevFile, AbsDevFile),
	safe_absolute_file_name(TestFile, AbsTestFile),

	load_ngram_examples(AbsInFile),

	open(AbsDevFile, write, SDev, [encoding('UTF-8'), encoding_signature(true)]),
	open(AbsTestFile, write, STest, [encoding('UTF-8'), encoding_signature(true)]),

	find_all_ngram_examples(AllNgrams),
	split_ngrams_examples(AllNgrams, NExamples, SDev, STest),

	close(SDev),
	close(STest),
	format('~NWritten dev examples to ~w~n', [AbsDevFile]),
	format('~NWritten test examples to ~w~n', [AbsTestFile]).

%---------------------------------------------------------------

:- dynamic potentially_relevant_ngram/1.

potentially_relevant_ngrams_are_defined :-
	potentially_relevant_ngram(_).

mark_potentially_relevant_ngrams(AMTSummaryFile, ExtractionPred) :-
	retractall(potentially_relevant_ngram(_)),

	safe_absolute_file_name(AMTSummaryFile, AbsAMTSummaryFile),
	
	read_amt_summary_file(AbsAMTSummaryFile, List),
	mark_potentially_relevant_ngrams1(List, ExtractionPred, 0-N),
	format('~N--- Marked ~d potentially relevant ngrams from ~w~n', [N, AbsAMTSummaryFile]),
	!.
mark_potentially_relevant_ngrams(AMTSummaryFile, ExtractionPred) :-
	format('~N*** Error: bad call: ~w~n', [mark_potentially_relevant_ngrams(AMTSummaryFile, ExtractionPred)]),
	fail.

read_amt_summary_file(File, List) :-
	safe_prolog_file_to_list(File, List, 'UTF-8'),
	!.

mark_potentially_relevant_ngrams1([], _ExtractionPred, N-N).
mark_potentially_relevant_ngrams1([F | R], ExtractionPred, In-Out) :-
	mark_potentially_relevant_ngrams_in_record(F, ExtractionPred, In-Next),
	!,
	mark_potentially_relevant_ngrams1(R, ExtractionPred, Next-Out).

mark_potentially_relevant_ngrams_in_record(Record, ExtractionPred, In-Out) :-
	member(ngram=Ngram, Record),
	ExtractionPred = X^Body,
	copy_term(X^Body, X1^Body1),
	X1 = Record,
	Call = Body1,
	(   call(Call) ->
	    mark_as_potentially_relevant_ngram(Ngram, In-Out)
	;
	    otherwise ->
	    Out = In
	),
	!.
mark_potentially_relevant_ngrams_in_record(Record, ExtractionPred, InOut) :-
	format('~N*** Error: bad call: ~w~n', [mark_potentially_relevant_ngrams_in_record(Record, ExtractionPred, InOut)]),
	fail.

mark_as_potentially_relevant_ngram(Ngram, In-Out) :-
	(   potentially_relevant_ngram(Ngram) ->
	    Out = In
	;
	    otherwise ->
	    assertz(potentially_relevant_ngram(Ngram)),
	    format('~N--- Potentially relevant ngram: ~w~n', [Ngram]),
	    Out is In + 1
	).

%---------------------------------------------------------------
	
load_ngram_examples(AbsInFile) :-
	format('~NLoading ngram examples from ~w~n', [AbsInFile]),
	safe_compile(ngram_examples, AbsInFile).

find_all_ngram_examples(AllNgrams) :-
	(   potentially_relevant_ngrams_are_defined ->
	    findall(NGram,
		    (   potentially_relevant_ngram(NGram),
			ngram_examples:ngram_example(NGram, _Multiplicity, _Example)
		    ),
		    AllNgrams0)
	;
	    otherwise ->
	    findall(NGram,
		    ngram_examples:ngram_example(NGram, _Multiplicity, _Example),
		    AllNgrams0)
	),
	sort(AllNgrams0, AllNgrams),
	length(AllNgrams, N),
	format('~N--- Found examples for ~d ngrams~n', [N]).

split_ngrams_examples([], _NExamples, _SDev, _STest).
split_ngrams_examples([F | R], NExamples, SDev, STest) :-
	split_examples_for_single_ngram(F, NExamples, SDev, STest),
	!,
	split_ngrams_examples(R, NExamples, SDev, STest).

split_examples_for_single_ngram(NGram, NExamples, SDev, STest) :-
	all_records_for_ngram(NGram, Records),
	split_examples_for_single_ngram1(Records, NGram, NExamples, SDev, STest).

all_records_for_ngram(NGram, Records) :-
	findall(ngram_example(NGram, Multiplicity, Example),
		ngram_examples:ngram_example(NGram, Multiplicity, Example),
		Records0),
	random_permutation(Records0, Records).

% Reached end. Warn if we were still looking for examples
split_examples_for_single_ngram1([], NGram, NExamples, _SDev, _STest) :-
	(   NExamples > 0 ->
	    format('~N--- Warning: missing ~d examples for ~w~n', [NExamples, NGram])
	;
	    otherwise ->
	    true
	).
% We have found all our Dev examples. Write everything out to Test
split_examples_for_single_ngram1([F | R], NGram, NExamples, SDev, STest) :-
	NExamples =< 0,
	format(STest, '~N~q.~n', [F]),
	!,
	split_examples_for_single_ngram1(R, NGram, 0, SDev, STest).
split_examples_for_single_ngram1([F | R], NGram, NExamples, SDev, STest) :-
	NExamples > 0,
	F = ngram_example(NGram, Multiplicity, Example),
	format(SDev, '~N~q.~n', [ngram_example(NGram, 1, Example)]),
	Multiplicity1 is Multiplicity - 1,
	(   Multiplicity1 > 0 ->
	    format(STest, '~N~q.~n', [ngram_example(NGram, Multiplicity1, Example)])
	;
	    otherwise ->
	    true
	),
	NExamples1 is NExamples - 1,
	!,
	split_examples_for_single_ngram1(R, NGram, NExamples1, SDev, STest).

%---------------------------------------------------------------
				     
extract_interesting_examples(TokenizedFile, ComparisonFile, Cutoff, MaxExamples, OutFile) :-
	extract_interesting_examples(TokenizedFile, ComparisonFile, Cutoff, MaxExamples, text, OutFile).
				     
extract_interesting_examples(TokenizedFile, ComparisonFile, Cutoff, MaxExamples, Format, OutFile) :-
	check_format(Format),
	
	absolute_file_name(TokenizedFile, AbsTokenizedFile),
	absolute_file_name(ComparisonFile, AbsComparisonFile),
	absolute_file_name(OutFile, AbsOutFile),

	store_interesting_ngrams(AbsComparisonFile, Cutoff),

	store_interesting_examples(AbsTokenizedFile, MaxExamples),

	write_out_interesting_examples(AbsOutFile, Format).

check_format(text) :-
	!.
check_format(csv) :-
	!.
check_format(pl) :-
	!.
check_format(Other) :-
	format('~N*** Error in extract_interesting_examples/6: format arg "~w" must be "text", "csv" or "pl"~n', [Other]),
	fail.

%---------------------------------------------------------------

:- dynamic interesting_ngram/5.

store_interesting_ngrams(AbsComparisonFile, Cutoff) :-
	integer(Cutoff),
	!,
	retractall(interesting_ngram(_, _, _, _, _)),
	open(AbsComparisonFile, read, SIn, [encoding('UTF-8')]),
	store_interesting_ngrams(SIn, Cutoff, 0-N),
	close(SIn),
	format('~N--- Stored ~d potentially interesting ngrams from ~w~n', [N, AbsComparisonFile]).
store_interesting_ngrams(_AbsComparisonFile, InterestingNgramFile) :-
	retractall(interesting_ngram(_, _, _, _, _)),
	
	safe_absolute_file_name(InterestingNgramFile, AbsInterestingNgramFile),
	safe_prolog_file_to_list_printing_statistics(AbsInterestingNgramFile, InterestingNGrams),
	length(InterestingNGrams, N),
	
	store_interesting_ngrams_from_list(InterestingNGrams),

	format('~N--- Stored ~d potentially interesting ngrams from ~w~n', [N, AbsInterestingNgramFile]).

store_interesting_ngrams(SIn, Cutoff, NIn-NOut) :-
	read(SIn, Term),
	!,
	store_interesting_ngrams1(Term, SIn, Cutoff, NIn-NOut).

store_interesting_ngrams1(end_of_file, _SIn, _Cutoff, NIn-NIn) :-
	!.
store_interesting_ngrams1(Term, _SIn, Cutoff, NIn-NIn) :-
	Term = ngram(_Words, _Count1, _Count2, Score),
	Score < Cutoff,
	!.
store_interesting_ngrams1(Term, SIn, Cutoff, NIn-NOut) :-
	Term = ngram(Words, Count1, Count2, Score),
	Words = [FirstWord | _],
	(   Score > Cutoff ->
	    assertz(interesting_ngram(FirstWord, Words, Count1, Count2, Score)),
	    NNext is NIn + 1
	;
	    otherwise ->
	    NNext = NIn
	),
	!,
	store_interesting_ngrams(SIn, Cutoff, NNext-NOut).

store_interesting_ngrams_from_list([]).
store_interesting_ngrams_from_list([F | R]) :-
	store_interesting_ngram_from_list(F),
	!,
	store_interesting_ngrams_from_list(R).

store_interesting_ngram_from_list(interesting_ngram(Words)) :-
	Words = [FirstWord | _Rest],
	assertz(interesting_ngram(FirstWord, Words, 0.0, 0.0, 0.0)),
	!.
store_interesting_ngram_from_list(F) :-
	format('~N*** Error: bad call: ~w~n', [store_interesting_ngram_from_list(F)]),
	fail.

%---------------------------------------------------------------

:- dynamic interesting_example/2.
:- dynamic number_of_occurrences_of_example/2.

store_interesting_examples(AbsTokenizedFile, MaxExamples) :-
	format('~N--- Looking for potentially interesting examples in ~w~n', [AbsTokenizedFile]),
	retractall(interesting_example(_, _)),
	retractall(number_of_occurrences_of_example(_, _)),
	open(AbsTokenizedFile, read, SIn, [encoding('UTF-8')]),
	store_interesting_examples(SIn, MaxExamples, 0-N, 0),
	close(SIn),
	format('~N--- Stored ~d potentially interesting examples from ~w~n', [N, AbsTokenizedFile]).

store_interesting_examples(SIn, MaxExamples, NIn-NOut, I) :-
	read(SIn, Term),
	!,
	store_interesting_examples1(Term, SIn, MaxExamples, NIn-NOut, I).

store_interesting_examples1(end_of_file, _SIn, _MaxExamples, NIn-NIn, _I) :-
	!.
store_interesting_examples1(Term, SIn, MaxExamples, NIn-NOut, I) :-
	I1 is I + 1,
	(   0 is I1 mod 1000 ->
	    format('~d ', [I1]),
	    flush_output(user)
	;
	    otherwise ->
	    true
	),
	store_interesting_examples2(Term, MaxExamples, NIn-NNext),
	!,
	store_interesting_examples(SIn, MaxExamples, NNext-NOut, I1).

store_interesting_examples2(TokenizedSent, MaxExamples, NIn-NOut) :-
	make_tokenized_sent_canonical(TokenizedSent, TokenizedSent1),
	tokenized_sent_to_atom(TokenizedSent, TokenizedSentAtom),
	%join_with_spaces(TokenizedSent, TokenizedSentAtom),
	store_interesting_examples3(TokenizedSent1, TokenizedSentAtom, MaxExamples, NIn-NOut),
	!.
store_interesting_examples2(TokenizedSent, MaxExamples, N) :-
	format('~N*** Error: bad call "~w"~n', [store_interesting_examples2(TokenizedSent, MaxExamples, N)]),
	fail.

store_interesting_examples3([], _TokenizedSentAtom, _MaxExamples, NIn-NIn).
store_interesting_examples3([F | R], TokenizedSentAtom, MaxExamples, NIn-NOut) :-
	(   interesting_unigram_prefix(UniGram, [F | R]) ->
	    store_interesting_example(TokenizedSentAtom, UniGram, MaxExamples, NIn-NNext1)
	;
	    otherwise ->
	    NNext1 = NIn
	),
	(   interesting_bigram_prefix(BiGram, [F | R]) ->
	    store_interesting_example(TokenizedSentAtom, BiGram, MaxExamples, NNext1-NNext2)
	;
	    otherwise ->
	    NNext2 = NNext1
	),
	(   interesting_trigram_prefix(TriGram, [F | R]) ->
	    store_interesting_example(TokenizedSentAtom, TriGram, MaxExamples, NNext2-NNext3)
	;
	    otherwise ->
	    NNext3 = NNext2
	),
	!,
	store_interesting_examples3(R, TokenizedSentAtom, MaxExamples, NNext3-NOut).

interesting_unigram_prefix([F], [F | _R]) :-
	interesting_ngram(F, [F], _Count1, _Count2, _Score),
	!.

interesting_bigram_prefix([F, F1], [F, F1 | _R]) :-
	interesting_ngram(F, [F, F1], _Count1, _Count2, _Score),
	!.

interesting_trigram_prefix([F, F1, F2], [F, F1, F2 | _R]) :-
	interesting_ngram(F, [F, F1, F2], _Count1, _Count2, _Score),
	!.

store_interesting_example(TokenizedSentAtom, NGram, MaxExamples, NIn-NOut) :-
	inc_number_of_occurrences_of_example(TokenizedSentAtom),
	number_of_examples_for_ngram(NGram, NExamples),
	(   NExamples < MaxExamples,
	    \+ interesting_example(_AnyNGram, TokenizedSentAtom),
	    \+ bad_chars_in_atom(TokenizedSentAtom)
	),
	assertz(interesting_example(NGram, TokenizedSentAtom)),
	NOut is NIn + 1,
	!.
store_interesting_example(_TokenizedSent, _NGram, _MaxExamples, NIn-NIn).

number_of_examples_for_ngram(NGram, NExamples) :-	
	findall(x, interesting_example(NGram, _), Xs),
	length(Xs, NExamples),
	!.

inc_number_of_occurrences_of_example(Example) :-
	(   number_of_occurrences_of_example(Example, N) ->
	    retractall(number_of_occurrences_of_example(Example, _))
	;
	    N = 0
	),
	N1 is N + 1,
	assertz(number_of_occurrences_of_example(Example, N1)),
	!.

make_tokenized_sent_canonical(TokenizedSentIn, TokenizedSentOut) :-
	lowercase_atom_list(TokenizedSentIn, TokenizedSentNext),
	remove_spaces_and_punctuation(TokenizedSentNext, TokenizedSentOut),
	!.
make_tokenized_sent_canonical(TokenizedSentIn, TokenizedSentOut) :-
	format('~N*** Error: bad call "~w"~n', [make_tokenized_sent_canonical(TokenizedSentIn, TokenizedSentOut)]),
	fail.

bad_chars_in_atom(Atom) :-
	atom_codes(Atom, Codes),
	bad_chars_in_str(Codes).

bad_chars_in_str([F | _R]) :-
	bad_code(F),
	!.
bad_chars_in_str([_F | R]) :-
	bad_chars_in_str(R).

bad_code(0'"). %"

%---------------------------------------------------------------

write_out_interesting_examples(AbsOutFile, Format) :-
	open(AbsOutFile, write, SOut, [encoding('UTF-8'), encoding_signature(true)]),
	all_interesting_ngrams(Ngrams),
	write_out_interesting_examples(Ngrams, Format, SOut, 0-N),
	close(SOut),
	format('~N--- Written ~d potentially interesting examples to ~w~n', [N, AbsOutFile]).	

write_out_interesting_examples([], _Format, _SOut, N-N) :-
	!.
write_out_interesting_examples([F | R], Format, SOut, NIn-NOut) :-
	write_out_interesting_examples_for_ngram(F, Format, SOut, NIn-NNext),
	!,
	write_out_interesting_examples(R, Format, SOut, NNext-NOut).

write_out_interesting_examples_for_ngram(NGram, Format, SOut, NIn-NOut) :-
	NGram = interesting_ngram(_FirstWord, Words, Count1, Count2, _Score),
	(   Format = text ->
	    format(SOut,
		   '~N~n%NGram: ~w; #occurrences in first set: ~d; #occurrences in second set: ~d~n',
		   [Words, Count1, Count2]),
	    NGramAtom = ''
	;
	    Format = csv ->
	    format_to_atom('~w ~d ~d',
			   [Words, Count1, Count2],
			   NGramAtom)
	;
	    otherwise ->
	    NGramAtom = Words 
	),
	all_examples_for_ngram(Words, Examples),
	(   Examples = [] ->
	    NIn = NOut,
	    format('~N--- Warning: no examples for ~w~n', [Words])
	;
	    otherwise ->
	    write_out_interesting_examples_for_ngram1(Examples, Format, NGramAtom, SOut, NIn-NOut)
	).

write_out_interesting_examples_for_ngram1([], _Format, _NGramAtom, _SOut, NIn-NIn).
write_out_interesting_examples_for_ngram1([F | R], Format, NGramAtom, SOut, NIn-NOut) :-
	write_out_interesting_example(F, Format, NGramAtom, SOut),
	NNext is NIn + 1,
	!,
	write_out_interesting_examples_for_ngram1(R, Format, NGramAtom, SOut, NNext-NOut).

write_out_interesting_example(F, text, _NGramAtom, SOut) :-
	format(SOut, '~N~w~n', [F]).
write_out_interesting_example(F, csv, NGramAtom, SOut) :-
	format(SOut, '~N"~w";"~w"~n', [NGramAtom, F]).
write_out_interesting_example(F, pl, NGram, SOut) :-
	(   number_of_occurrences_of_example(F, N) ->
	    true
	;
	    N = 0
	),
	format(SOut, '~N~q.~n', [ngram_example(NGram, N, F)]).
write_out_interesting_example(F, Format, NGramAtom, SOut) :-
	format('~N*** Error: bad call "~w"~n', [write_out_interesting_example(F, Format, NGramAtom, SOut)]),
	fail.

all_interesting_ngrams(Ngrams) :-
	findall(interesting_ngram(FirstWord, Words, Count1, Count2, Score),
		interesting_ngram(FirstWord, Words, Count1, Count2, Score),
		Ngrams).

all_examples_for_ngram(NGram, Examples) :-
	findall(Example,
		interesting_example(NGram, Example),
		Examples).

