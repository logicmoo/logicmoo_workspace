:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(manipulate_ngrams,
	  [normalise_ngram_file/2,
	   add_normalised_ngram_files/3,
	   combine_normalised_ngram_files/3,
	   order_combined_ngrams/2,
	   order_combined_ngrams/3,

	   load_combined_ngrams/1,
	   normalised_frequencies_for_ngram/3,
 	   
	   test_normalise_ngram_file/1,
	   test_add_normalised_ngram_files/1,
	   test_combine_normalised_ngram_files/1,
	   test_order_combined_ngrams/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

test_normalise_ngram_file(forum) :-
	normalise_ngram_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams.pl',
			     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_normalised.pl').

test_normalise_ngram_file(tm_fr) :-
	normalise_ngram_file('$ACCEPT/MT/GTFeb2012/TM/tm_fr_ngrams.pl',
			     '$ACCEPT/MT/GTFeb2012/TM/tm_fr_ngrams_normalised.pl').

test_normalise_ngram_file(europarl_fr) :-
	normalise_ngram_file('$ACCEPT/MT/Europarl/Generated/europarl-v6-fr-ngrams.pl',
			     '$ACCEPT/MT/Europarl/Generated/europarl-v6-fr-ngrams-normalised.pl').

test_add_normalised_ngram_files(tm_fr_europarl_fr) :-
	add_normalised_ngram_files('$ACCEPT/MT/GTFeb2012/TM/tm_fr_ngrams_normalised.pl',
				   '$ACCEPT/MT/Europarl/Generated/europarl-v6-fr-ngrams-normalised.pl',
				   '$ACCEPT/MT/Europarl/Generated/europarl-fr-plus-tm-fr-ngrams-normalised.pl').

test_add_normalised_ngram_files(tm_fr_europarl_fr_short) :-
	add_normalised_ngram_files1('$ACCEPT/MT/Europarl/Generated/europarl-fr-plus-tm-fr-ngrams-normalised.pl').

test_combine_normalised_ngram_files(forum_europarl_fr) :-
	combine_normalised_ngram_files('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_normalised.pl',
				       '$ACCEPT/MT/Europarl/Generated/europarl-v6-fr-ngrams-normalised.pl',
				       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_europarl.pl').

test_combine_normalised_ngram_files(forum_tm_fr) :-
	combine_normalised_ngram_files('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_normalised.pl',
				       '$ACCEPT/MT/GTFeb2012/TM/tm_fr_ngrams_normalised.pl',
				       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm.pl').

test_combine_normalised_ngram_files(forum_tm_fr_plus_europarl) :-
	combine_normalised_ngram_files('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_normalised.pl',
				       %'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_normalised_20.pl',
				       '$ACCEPT/MT/Europarl/Generated/europarl-fr-plus-tm-fr-ngrams-normalised.pl',
				       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl.pl').

test_combine_normalised_ngram_files(forum_europarl_fr_short) :-
	combine_normalised_ngram_files1('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_europarl.pl').

test_order_combined_ngrams(forum_europarl_fr) :-
	order_combined_ngrams('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_europarl.pl',
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_europarl_ordered.pl').

test_order_combined_ngrams(forum_tm_fr) :-
	order_combined_ngrams('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm.pl',
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_ordered.pl').

test_order_combined_ngrams(forum_tm_fr_plus_europarl) :-
	order_combined_ngrams('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl.pl',
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered.pl').

test_order_combined_ngrams(forum_tm_fr_plus_europarl_split) :-
	read_ngram_file('$ACCEPT/MT/Europarl/Generated/europarl-fr-plus-tm-fr-ngrams-normalised.pl', ngrams2),
	test_order_combined_ngrams(forum_tm_fr_plus_europarl_split_main).

test_order_combined_ngrams(forum_tm_fr_plus_europarl_split_main) :-
	compile('$ACCEPT/MT/GTFeb2012/Prolog/stopwords.pl'),
	
	order_combined_ngrams('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl.pl',
			      unigram_and_alphabetic,
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_unigrams.pl'),
			      
	order_combined_ngrams('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl.pl',
			      %bigram_and_alphabetic,
			      bigram_and_alphabetic_no_stopwords,
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_bigrams.pl'),			      
	order_combined_ngrams('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl.pl',
			      %trigram_and_alphabetic,
			      %trigram_and_alphabetic_at_least_one_rare_bigram,
			      %trigram_and_alphabetic_two_rare_bigrams,
			      %trigram_and_alphabetic_no_rare_unigrams,
			      %trigram_and_alphabetic_no_rare_unigrams_or_bigrams,
			      %trigram_and_alphabetic_no_stopwords,
			      %trigram_and_alphabetic_rare_unigrams_or_bigrams_no_stopwords,
			      trigram_and_alphabetic_two_rare_bigrams_no_stopwords,
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_trigrams.pl').

test_order_combined_ngrams(forum_tm_fr_plus_europarl_clitics) :-
	compile('$ACCEPT/MT/GTFeb2012/Prolog/stopwords.pl'),
	
	order_combined_ngrams('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl.pl',
			      bigram_and_alphabetic_includes_clitic,
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_bigrams_clitics.pl'),			      
	order_combined_ngrams('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl.pl',
			      trigram_and_alphabetic_includes_clitic,
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_trigrams_clitics.pl').			      

test_order_combined_ngrams(forum_tm_fr_plus_europarl_plus_or_quoi) :-
	%compile('$ACCEPT/MT/GTFeb2012/Prolog/stopwords.pl'),
	
	order_combined_ngrams('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl.pl',
			      bigram_and_alphabetic_includes_plus_or_quoi,
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_bigrams_plus_or_quoi.pl'),			      
	order_combined_ngrams('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl.pl',
			      trigram_and_alphabetic_includes_plus_or_quoi,
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_trigrams_plus_or_quoi.pl').			      
test_order_combined_ngrams(forum_tm_fr_plus_europarl_jamais) :-
	%compile('$ACCEPT/MT/GTFeb2012/Prolog/stopwords.pl'),
	
	order_combined_ngrams('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl.pl',
			      bigram_and_alphabetic_includes_jamais,
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_bigrams_jamais.pl'),			      
	order_combined_ngrams('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl.pl',
			      trigram_and_alphabetic_includes_jamais,
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_trigrams_jamais.pl').

test_order_combined_ngrams(forum_tm_fr_plus_europarl_on) :-
	%compile('$ACCEPT/MT/GTFeb2012/Prolog/stopwords.pl'),
	
	order_combined_ngrams('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl.pl',
			      bigram_and_alphabetic_includes_on,
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_bigrams_on.pl'),			      
	order_combined_ngrams('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl.pl',
			      trigram_and_alphabetic_includes_on,
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl_ordered_trigrams_on.pl').						      
%---------------------------------------------------------------

normalise_ngram_file(InFile, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),

	read_ngram_file(AbsInFile, user),

	open(AbsOutFile, write, SOut, [encoding('UTF-8'), encoding_signature(true)]),

	write_normalised_ngrams(1, SOut, N1),
	write_normalised_ngrams(2, SOut, N2),
	write_normalised_ngrams(3, SOut, N3),

	close(SOut),
	N is N1 + N2 + N3,
	format('~N--- Written file (~d ngrams) ~w~n', [N, AbsOutFile]).

%---------------------------------------------------------------

combine_normalised_ngram_files(InFile1, InFile2, OutFile) :-
	load_both_ngram_sets(InFile1, InFile2),
	combine_normalised_ngram_files1(OutFile).

combine_normalised_ngram_files1(OutFile) :-
	safe_absolute_file_name(OutFile, AbsOutFile),

	open(AbsOutFile, write, SOut, [encoding('UTF-8'), encoding_signature(true)]),

	write_combined_ngrams(SOut, ngrams1, ngrams2),

	close(SOut),
	format('~N--- Written combined ngrams file ~w~n', [AbsOutFile]).

load_both_ngram_sets(InFile1, InFile2) :-
	safe_absolute_file_name(InFile1, AbsInFile1),
	safe_absolute_file_name(InFile2, AbsInFile2),	
	read_ngram_file(AbsInFile1, ngrams1),
	read_ngram_file(AbsInFile2, ngrams2),
	!.

load_combined_ngrams(InFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	read_ngram_file(AbsInFile, combined_ngrams),
	!.

normalised_frequencies_for_ngram(NGram, Score1, Score2) :-
	combined_ngrams:ngram(NGram, Score1, Score2),
	!.
normalised_frequencies_for_ngram(_NGram, Score1, Score2) :-
	Score1 = 0.0,
	Score2 = 0.0,
	!.

%---------------------------------------------------------------

add_normalised_ngram_files(InFile1, InFile2, OutFile) :-
	load_both_ngram_sets(InFile1, InFile2),
	add_normalised_ngram_files1(OutFile).

add_normalised_ngram_files1(OutFile) :-
	safe_absolute_file_name(OutFile, AbsOutFile),

	open(AbsOutFile, write, SOut, [encoding('UTF-8'), encoding_signature(true)]),

	write_added_ngrams(SOut, ngrams1, ngrams2, N),

	close(SOut),
	format('~N--- Written added ngrams file (~d ngrams)~w~n', [N, AbsOutFile]).

%---------------------------------------------------------------

order_combined_ngrams(InFile, OutFile) :-
	order_combined_ngrams(InFile, trivial, OutFile).

order_combined_ngrams(InFile, FilterPred, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),

	safe_prolog_file_to_list_printing_statistics(AbsInFile, InList, 'UTF-8'),

	format('~N--- Filtering elements using "~w"... ', [FilterPred]),
	filter_combined_ngrams(InList, FilterPred, NextList, 0-_NFiltered),
	format('done~n', []),
	
	format('~N--- Sorting elements... ', []),
	sort_combined_ngrams(NextList, OutList),
	format('done~n', []),

	length(OutList, N),

	list_to_prolog_file_with_encoding(OutList, AbsOutFile, 'UTF-8'),
	format('~N--- Written ordered ngrams comparison file (~d elements) ~w~n', [N, AbsOutFile]).

filter_combined_ngrams([], _FilterPred, [], N-N).
filter_combined_ngrams([F | R], FilterPred, Out, NIn-NOut) :-
	NNext is NIn + 1,
	(   0 is NNext mod 1000 ->
	    format('~d ', [NNext]),
	    flush_output(user)
	;
	    otherwise ->
	    true
	),
	Call =.. [FilterPred, F],
	(   call(Call) ->
	    Out = [F | R1]
	;
	    Out = R1
	),
	!,
	filter_combined_ngrams(R, FilterPred, R1, NNext-NOut).

sort_combined_ngrams(InList, OutList) :-
	add_key_to_combined_ngrams(InList, KeyedInList),
	keysort(KeyedInList, SortedKeyedInList),
	store_high_scoring_unigrams_and_bigrams(SortedKeyedInList),
	remove_ngrams_with_high_scoring_sub_ngrams(SortedKeyedInList, SortedKeyedInList1),
	unkey_list(SortedKeyedInList1, OutList).

add_key_to_combined_ngrams([], []).
add_key_to_combined_ngrams([F | R], [F1 | R1]) :-
	add_key_to_combined_ngram(F, F1),
	!,
	add_key_to_combined_ngrams(R, R1).

add_key_to_combined_ngram(ngram(Words, Count1, Count2),
			  MinusDiffScore-ngram(Words, Count1, Count2, DiffScore)) :-
	ngram_count_diff_score(Words, Count1, Count2, DiffScore),
	MinusDiffScore is -1 * DiffScore,
	!.

ngram_count_diff_score(Words, Count1, Count2, DiffScore) :-
	length(Words, Length),
	length_bonus(Length, LengthBonus),
	smooth_zero_score(Count2, SmoothedCount2),
	DiffScore is LengthBonus + (Count1 / SmoothedCount2),
	!.

%length_bonus(1, 1000) :-
%	!.
%length_bonus(2, 500) :-
%	!.
length_bonus(_Other, 0) :-
	!.

smooth_zero_score(Count, SmoothedCount) :-
	(   Count < 1.0 ->
	    SmoothedCount = 1.0
	;
	    otherwise ->
	    SmoothedCount = Count
	).	

%---------------------------------------------------------------

trivial(_AnyRecord).

unigram(ngram([_], _Count1, _Count2)).

bigram(ngram([_, _], _Count1, _Count2)).

trigram(ngram([_, _, _], _Count1, _Count2)).

unigram_and_alphabetic(ngram([A], _Count1, _Count2)) :-
	alphabetic_word_list([A]).

bigram_and_alphabetic(ngram([A, B], _Count1, _Count2)) :-
	alphabetic_word_list([A, B]).

trigram_and_alphabetic(ngram([A, B, C], _Count1, _Count2)) :-
	alphabetic_word_list([A, B, C]).

trigram_and_alphabetic_at_least_one_rare_bigram(ngram([A, B, C], _Count1, _Count2)) :-
	alphabetic_word_list([A, B, C]),
	at_least_one_rare_bigram([A, B, C]).

trigram_and_alphabetic_two_rare_bigrams_no_stopwords(ngram([A, B, C], Count1, Count2)) :-
	trigram_and_alphabetic_two_rare_bigrams(ngram([A, B, C], Count1, Count2)),
	trigram_and_alphabetic_no_stopwords(ngram([A, B, C], Count1, Count2)).

trigram_and_alphabetic_two_rare_bigrams(ngram([A, B, C], _Count1, _Count2)) :-
	alphabetic_word_list([A, B, C]),
	two_rare_bigrams([A, B, C]).

trigram_and_alphabetic_no_rare_unigrams(ngram([A, B, C], _Count1, _Count2)) :-
	alphabetic_word_list([A, B, C]),
	no_rare_unigrams([A, B, C]).

trigram_and_alphabetic_rare_unigrams_or_bigrams_no_stopwords(ngram([A, B, C], Count1, Count2)) :-
	trigram_and_alphabetic_no_rare_unigrams_or_bigrams(ngram([A, B, C], Count1, Count2)),
	trigram_and_alphabetic_no_stopwords(ngram([A, B, C], Count1, Count2)).

trigram_and_alphabetic_no_rare_unigrams_or_bigrams(ngram([A, B, C], _Count1, _Count2)) :-
	alphabetic_word_list([A, B, C]),
	no_rare_unigrams([A, B, C]),
	no_rare_bigrams([A, B, C]).

trigram_and_alphabetic_no_stopwords(ngram([A, B, C], _Count1, _Count2)) :-
	alphabetic_word_list([A, B, C]),
	\+ trigram_contains_stopword([A, B, C]).

bigram_and_alphabetic_no_stopwords(ngram([A, B], _Count1, _Count2)) :-
	alphabetic_word_list([A, B]),
	\+ bigram_contains_stopword([A, B]).

trigram_and_alphabetic_includes_clitic(ngram([A, B, C], _Count1, _Count2)) :-
	alphabetic_word_list([A, B, C]),
	trigram_contains_clitic([A, B, C]).

bigram_and_alphabetic_includes_clitic(ngram([A, B], _Count1, _Count2)) :-
	alphabetic_word_list([A, B]),
	bigram_contains_clitic([A, B]).

trigram_and_alphabetic_includes_plus_or_quoi(ngram([A, B, C], _Count1, _Count2)) :-
	alphabetic_word_list([A, B, C]),
	ngram_contains_plus_or_quoi([A, B, C]).

bigram_and_alphabetic_includes_plus_or_quoi(ngram([A, B], _Count1, _Count2)) :-
	alphabetic_word_list([A, B]),
	ngram_contains_plus_or_quoi([A, B]).

ngram_contains_plus_or_quoi(Ngram) :-
	member(plus, Ngram),
	!.
ngram_contains_plus_or_quoi(Ngram) :-
	member(quoi, Ngram).

trigram_and_alphabetic_includes_jamais(ngram([A, B, C], _Count1, _Count2)) :-
	alphabetic_word_list([A, B, C]),
	ngram_contains_jamais([A, B, C]).

bigram_and_alphabetic_includes_jamais(ngram([A, B], _Count1, _Count2)) :-
	alphabetic_word_list([A, B]),
	ngram_contains_jamais([A, B]).

ngram_contains_jamais(Ngram) :-
	member(jamais, Ngram),
	!.

trigram_and_alphabetic_includes_on(ngram([A, B, C], _Count1, _Count2)) :-
	alphabetic_word_list([A, B, C]),
	ngram_contains_on([A, B, C]).

bigram_and_alphabetic_includes_on(ngram([A, B], _Count1, _Count2)) :-
	alphabetic_word_list([A, B]),
	ngram_contains_on([A, B]).

ngram_contains_on(Ngram) :-
	member(on, Ngram),
	!.

at_least_one_rare_bigram([A, B, C]) :-
	(   rare_bigram([A, B])
	;
	    rare_bigram([B, C])
	).

two_rare_bigrams([A, B, C]) :-
	rare_bigram([A, B]),
	rare_bigram([B, C]).

no_rare_bigrams([A, B, C]) :-
	\+ rare_bigram([A, B]),
	\+ rare_bigram([B, C]).

no_rare_unigrams([A, B, C]) :-
	\+ rare_unigram(A),
	\+ rare_unigram(B),
	\+ rare_unigram(C).

rare_unigram(A) :-
	(   ngrams2:ngram(A, [A], Count2A) ->
	    Count2A < 5.0
	;
	    otherwise ->
	    true
	).

rare_bigram([A, B]) :-
	(   ngrams2:ngram(A, [A, B], Count2AB) ->
	    Count2AB < 5.0
	;
	    otherwise ->
	    true
	).

alphabetic_word_list([]).
alphabetic_word_list([F | R]) :-
	alphabetic_word(F),
	!,
	alphabetic_word_list(R).

alphabetic_word(Atom) :-
	atom_codes(Atom, Str),
	alphabetic_str(Str).

alphabetic_str([]).
alphabetic_str([F | R]) :-
	alphabetic_char(F),
	!,
	alphabetic_str(R).

alphabetic_char(F) :-
	(   lowercase_char(F)
	;
	    uppercase_char(F)
	),
	!.

%---------------------------------------------------------------

:- dynamic high_scoring_unigram/1.

:- dynamic high_scoring_bigram/2.

store_high_scoring_unigrams_and_bigrams(SortedKeyedInList) :-
	format('~N--- Storing high-scoring unigrams and bigrams... ', []),
	retractall(high_scoring_unigram(_)),
	retractall(high_scoring_bigram(_, _)),
	store_high_scoring_unigrams_and_bigrams1(SortedKeyedInList),
	format('done~n', []),
	!.

store_high_scoring_unigrams_and_bigrams1([]).
store_high_scoring_unigrams_and_bigrams1([F | R]) :-
	store_high_scoring_unigram_or_bigram(F),
	!,
	store_high_scoring_unigrams_and_bigrams1(R).

store_high_scoring_unigram_or_bigram(_MinusDiffScore-ngram(Words, _Count1, _Count2, DiffScore)) :-
	store_high_scoring_unigram_or_bigram1(DiffScore, Words).

store_high_scoring_unigram_or_bigram1(DiffScore, Words) :-
	length(Words, L),
	L < 3,
	high_score(DiffScore),
	!,
	store_high_scoring_unigram_or_bigram2(Words).
store_high_scoring_unigram_or_bigram1(_DiffScore, _Words).

high_score(Score) :-
	Score > 2000.

store_high_scoring_unigram_or_bigram2([W1]) :-
	assertz(high_scoring_unigram(W1)).
store_high_scoring_unigram_or_bigram2([W1, W2]) :-
	assertz(high_scoring_bigram(W1, W2)).

%---------------------------------------------------------------

remove_ngrams_with_high_scoring_sub_ngrams([], []).
remove_ngrams_with_high_scoring_sub_ngrams([F | R], R1) :-
	ngram_record_with_high_scoring_sub_ngrams(F),
	!,
	remove_ngrams_with_high_scoring_sub_ngrams(R, R1). 
remove_ngrams_with_high_scoring_sub_ngrams([F | R], [F | R1]) :-
	!,
	remove_ngrams_with_high_scoring_sub_ngrams(R, R1).

ngram_record_with_high_scoring_sub_ngrams(_Score-ngram(Words, _Count1, _Count2, _DiffScore)) :-
	ngram_with_high_scoring_sub_ngrams(Words),
	!.

ngram_with_high_scoring_sub_ngrams(Words) :-
	length(Words, L),
	L > 1,
	member(W, Words),
	high_scoring_unigram(W),
	!.
ngram_with_high_scoring_sub_ngrams([A, B, C]) :-
	(   high_scoring_bigram(A, B)
	;
	    high_scoring_bigram(B, C)
	),
	!.

%---------------------------------------------------------------


write_combined_ngrams(SOut, Module1, Module2) :-
	find_all_ngrams(Module1, NGrams1),
	write_combined_ngrams(NGrams1, SOut, Module2, 0).

find_all_ngrams(Module, NGrams) :-
	format('~N--- Collecting ngrams from module ~w~n', [Module]),
	findall(ngram(FirstWord, Words, Count),
		Module:ngram(FirstWord, Words, Count),
		NGrams),
	length(NGrams, N),
	format('~N--- Found ~d ngrams~n', [N]),
	!.

write_combined_ngrams([], _SOut, _Module2, _I).
write_combined_ngrams([F | R], SOut, Module2, I) :-
	F = ngram(FirstWord, Words, Count1),
	I1 is I + 1,
	(   0 is I1 mod 1000 ->
	    format('~d ', [I1]),
	    flush_output(user)
	;
	    otherwise ->
	    true
	),
	(   Module2:ngram(FirstWord, Words, Count2) ->
	    true
	;
	    otherwise ->
	    Count2 = 0.0
	),
	format(SOut, '~Nngram(~q, ~2f, ~2f).~n', [Words, Count1, Count2]),
	!,
	write_combined_ngrams(R, SOut, Module2, I1).
	
%---------------------------------------------------------------

write_added_ngrams(SOut, Module1, Module2, N) :-
	write_added_ngrams1(SOut, Module1, Module2, 0-N1),

	write_added_ngrams2(SOut, Module1, Module2, N1-N).

% All Ngrams in Module1, combined with Module2 if necessary.
write_added_ngrams1(SOut, Module1, Module2, NIn-NOut) :-
	findall(ngram(FirstWord, Words, Count),
		(   Module1:ngram(FirstWord, Words, Count1),
		    (   Module2:ngram(FirstWord, Words, Count2) ->
			safe_max_list([Count1, Count2], Count)
		    ;
			otherwise ->
			Count = Count1
		    )
		),
		List),
	write_ngrams(List, SOut, NIn-NOut).

% All Ngrams in Module2 that don't occur in Module2
write_added_ngrams2(SOut, Module1, Module2, NIn-NOut) :-
	findall(ngram(FirstWord, Words, Count1),
		(   Module2:ngram(FirstWord, Words, Count1),
		    \+ Module1:ngram(FirstWord, Words, _Count2)
		),
		List),
	write_ngrams(List, SOut, NIn-NOut).

write_ngrams([], _SOut, NIn-NIn).
write_ngrams([F | R], SOut, NIn-NOut) :-
	write_ngram(F, SOut, NIn-NNext),
	!,
	write_ngrams(R, SOut, NNext-NOut).

write_ngram(Ngram, SOut, NIn-NOut) :-
	format(SOut, '~N~q.~n', [Ngram]),
	NOut is NIn + 1,
	!.

%---------------------------------------------------------------

read_ngram_file(File, Module) :-
	%safe_compile(Module, File),
	safe_compile_with_redefine_warnings_off(Module, File),
	format('~N--- Loaded ngrams from ~w into module ~w~n', [File, Module]).

write_normalised_ngrams(Length, SOut, N) :-
	format('~N--- Finding N-grams of length ~d... ', [Length]),
	total_ngram_count(Length, user, List, N, Total),
	format('found (~d ngrams, ~d different). Writing out ngrams~n', [Total, N]),
	sort_ngrams(List, List1),
	write_normalised_ngrams1(List1, SOut, Total, 0).

total_ngram_count(Length, Module, List, N, Total) :-
	findall(Ngram,
		ngram_of_length_n(Ngram, Module, Length),
		List),
	length(List, N),
	total_ngram_count1(List, 0-Total).

ngram_of_length_n(ngram([X], Count), Module, 1) :-
	Module:ngram([X], Count).
ngram_of_length_n(ngram([X, Y], Count), Module, 2) :-
	Module:ngram([X, Y], Count).
ngram_of_length_n(ngram([X, Y, Z], Count), Module, 3) :-
	Module:ngram([X, Y, Z], Count).

total_ngram_count1([], In-In).
total_ngram_count1([ngram(_Words, Count) | R], In-Out) :-
	Next is In + Count,
	!,
	total_ngram_count1(R, Next-Out).

sort_ngrams(List, List1) :-
	tag_ngrams(List, TaggedList),
	keysort(TaggedList, SortedTaggedList),
	unkey_list(SortedTaggedList, List1).

tag_ngrams([], []).
tag_ngrams([F | R], [F1 | R1]) :-
	tag_ngram(F, F1),
	!,
	tag_ngrams(R, R1).

tag_ngram(ngram(Words, Count), Key-ngram(Words, Count)) :-
	Key is Count * -1.

write_normalised_ngrams1([], _SOut, _Total, _I) :-
	!.
write_normalised_ngrams1([F | R], SOut, Total, I) :-
	I1 is I + 1,
	(   0 is I1 mod 10000 ->
	    format('~d ', [I1]),
	    flush_output(user)
	;
	    otherwise ->
	    true
	),
	write_normalised_ngram(F, SOut, Total),
	!,
	write_normalised_ngrams1(R, SOut, Total, I1).

write_normalised_ngram(ngram(Words, Count), SOut, Total) :-
	Words = [FirstWord | _],
	NormalisedCount is (1000000 * Count) / Total,
	format(SOut, '~Nngram(~q, ~q, ~2f).~n', [FirstWord, Words, NormalisedCount]),
	!.
write_normalised_ngram(Ngram, SOut, Total) :-
	format('~N*** Error: bad call: ~w~n', [write_normalised_ngram(Ngram, SOut, Total)]),
	fail.

				


