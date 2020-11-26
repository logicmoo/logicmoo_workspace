:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(representative_corpus,
	  [make_rep_corpus/7,
	   clean_rep_corpus/3,
	   	   
	   test_representative_corpus/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/CorpusTools/tokenize_sents').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

test_representative_corpus(load_ngrams) :-
	safe_compile(user, '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_normalised.pl').

test_representative_corpus(small) :-
	make_rep_corpus('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised_small.pl', 
			'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/smart_representative_small.pl',
			'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/smart_representative_trace_small.txt',
			'UTF-8',
			3,  %Size
			50,  %NGroupd
			[unigram = 0.1,
			 bigram = 10,
			 trigram = 100,
			 p_unigram = -0.01,
			 p_bigram = -1,
			 p_trigram = -10,
			 min_length = 12,
			 max_length = 40]).

test_representative_corpus(full_12_40) :-
	test_representative_corpus(full_12_20),
	test_representative_corpus(full_21_28),
	test_representative_corpus(full_29_40).

test_representative_corpus(full_12_40_clean) :-
	test_representative_corpus(full_12_20_clean),
	test_representative_corpus(full_21_28_clean),
	test_representative_corpus(full_29_40_clean).

test_representative_corpus(full_12_20) :-
	make_rep_corpus('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl', 
			'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/smart_representative_12_20.pl',
			'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/smart_representative_trace_12_20.txt',
			'UTF-8',
			6,  %Size
			500,  %NGroupd
			[unigram = 0.1,
			 bigram = 10,
			 trigram = 100,
			 p_unigram = -0.01,
			 p_bigram = -1,
			 p_trigram = -10,
			 min_length = 12,
			 max_length = 20]).

test_representative_corpus(full_12_20_clean) :-
	clean_rep_corpus('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/smart_representative_12_20.pl',
			 'UTF-8',
			 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/smart_representative_12_20_cleaned.txt'
			 ).

test_representative_corpus(full_21_28) :-
	make_rep_corpus('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl', 
			'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/smart_representative_21_28.pl',
			'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/smart_representative_trace_21_28.txt',
			'UTF-8',
			6,  %Size
			500,  %NGroupd
			[unigram = 0.1,
			 bigram = 10,
			 trigram = 100,
			 p_unigram = -0.01,
			 p_bigram = -1,
			 p_trigram = -10,
			 min_length = 21,
			 max_length = 28]).

test_representative_corpus(full_21_28_clean) :-
	clean_rep_corpus('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/smart_representative_21_28.pl',
			 'UTF-8',
			 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/smart_representative_21_28_cleaned.txt').

test_representative_corpus(full_29_40) :-
	make_rep_corpus('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl', 
			'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/smart_representative_29_40.pl',
			'$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/smart_representative_trace_29_40.txt',
			'UTF-8',
			6,  %Size
			500,  %NGroupd
			[unigram = 0.1,
			 bigram = 10,
			 trigram = 100,
			 p_unigram = -0.01,
			 p_bigram = -1,
			 p_trigram = -10,
			 min_length = 29,
			 max_length = 40]).

test_representative_corpus(full_29_40_clean) :-
	clean_rep_corpus('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/smart_representative_29_40.pl',
			 'UTF-8',
			 '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/smart_representative_29_40_cleaned.txt').

%---------------------------------------------------------------

:- dynamic corpus/3.
:- dynamic selected_corpus/1.
:- dynamic selected_ngram/3.
:- dynamic total_score_for_selected_ngram_of_length/2.

%---------------------------------------------------------------

% Remove "Re:'
clean_rewrite(['Re', ':', ' ' | R]-R, Out-Out).
clean_rewrite(['Re', ' ', ':', ' ' | R]-R, Out-Out).
clean_rewrite(['Re', '\xA0\', ':', ' ' | R]-R, Out-Out).

% Don't include items with these strings
dubious_substring("___").
dubious_substring("---").
dubious_substring("***").
dubious_substring("\\").

%---------------------------------------------------------------

/*

A file of n-gram frequencies needs to be loaded first.

InCorpus is a file of tokenized sentences.
OutCorpus is a file of plain sentences
TraceFile shows trace output
GroupSize and NGroups are positive numbers
Parameters is a list of atom-value pairs giving values to the following parameters:
  unigram, bigram, trigram - positive weights for scores in n-gram freq file
  p_unigram, p_bigram, p_trigram - negative weights for scores in previously selected examples

Behavior:

Order the sentences in InCorpus by extracting n-grams and applying the parameters.
Pick the GroupSize examples with the highest score that have not already been picked.
Repeat NGroups times.

*/

make_rep_corpus(InCorpus, OutCorpus, TraceCorpus, Encoding, GroupSize, NGroups, Parameters) :-
	init_make_rep_corpus(InCorpus, OutCorpus, TraceCorpus, Encoding, AbsOutCorpus, AbsTraceCorpus, Parameters),
	make_rep_corpus1(0-NGroups, AbsOutCorpus, AbsTraceCorpus, Encoding, GroupSize, Parameters),
	format('~N--- Output written to ~w~n', [AbsOutCorpus]),
	format('~N---  Trace written to ~w~n', [AbsTraceCorpus]).

%---------------------------------------------------------------

clean_rep_corpus(InCorpus, Encoding, OutCorpus) :-
	safe_absolute_file_name(InCorpus, AbsInCorpus),
	safe_absolute_file_name(OutCorpus, AbsOutCorpus),

	read_rep_corpus(AbsInCorpus, Encoding, InList),

	clean_rep_corpus1(InList, OutList),

	write_rep_corpus(OutList, Encoding, AbsOutCorpus).

%---------------------------------------------------------------

init_make_rep_corpus(InCorpus, OutCorpus, TraceCorpus, Encoding, AbsOutCorpus, AbsTraceCorpus, Parameters) :-
	safe_absolute_file_name(InCorpus, AbsInCorpus),
	safe_absolute_file_name(OutCorpus, AbsOutCorpus),
	safe_absolute_file_name(TraceCorpus, AbsTraceCorpus),
			     
	check_ngrams_are_loaded,
	check_parameters(Parameters),
	init_selected_corpus,
	overwrite_file(AbsOutCorpus),
	overwrite_file(AbsTraceCorpus),
	load_corpus(AbsInCorpus, Encoding, Parameters),
	!.

check_ngrams_are_loaded :-
	current_predicate(user:ngram/3),
	!.
check_ngrams_are_loaded :-
	format('~N*** Error: user:ngram/3 is not defined~n', []),
	fail.

check_parameters(Parameters) :-
	is_list(Parameters),
	ground(Parameters),
	!,
	check_parameters1([unigram, bigram, trigram,
			   p_unigram, p_bigram, p_trigram,
			   min_length, max_length],
			  Parameters).
check_parameters(Parameters) :-
	format('~N*** Error: parameters ~w not a ground list~n', [Parameters]),
	fail.

check_parameters1([], _Parameters).
check_parameters1([F | R], Parameters) :-
	check_parameter(F, Parameters),
	!,
	check_parameters1(R, Parameters).

check_parameter(X, Parameters) :-
	member(X=Val, Parameters),
	number(Val),
	!.
check_parameter(X, Parameters) :-
	format('~N*** Error: parameter ~w not defined in ~w~n', [X, Parameters]),
	fail.

init_selected_corpus :-
	retractall(corpus(_, _, _)),
	retractall(selected_corpus(_)),
	retractall(selected_ngram(_, _, _)).

load_corpus(AbsInCorpus, Encoding, Parameters) :-
	safe_prolog_file_to_list(AbsInCorpus, List, Encoding),
	length(List, N),
	format('~N--- Loading corpus (~d lines) from ~w~n', [N, AbsInCorpus]),
	load_corpus1(List, Parameters, 0),
	format('~N--- Loaded corpus~n', []),
	!.

load_corpus1([], _Parameters, _I).
load_corpus1([F | R], Parameters, I) :-
	load_corpus_item(F, Parameters),
	I1 is I + 1,
	(   0 is I1 mod 1000 ->
	    
	    format(' (~d)', [I1]),
	    flush_output(user) ;
	    
	    true
	),
	!,
	load_corpus1(R, Parameters, I1).

load_corpus_item(TokenizedSent, Parameters) :-
	normalise_tokenized_sent(TokenizedSent, TokenizedSentNoSpaces),
	length(TokenizedSentNoSpaces, Length),
	(   normalized_tokenized_sent_fails_length_restrictions(Length, Parameters) ->
	    true
	;
	    corpus(TokenizedSent, TokenizedSentNoSpaces, _SomeScore) ->
	    true
	;
	    otherwise ->
	    weighted_ngram_score([unigram, bigram, trigram], TokenizedSentNoSpaces, Parameters, Score),
	    assertz(corpus(TokenizedSent, TokenizedSentNoSpaces, Score))
	).

normalized_tokenized_sent_fails_length_restrictions(Length, Parameters) :-
	member(min_length = Min, Parameters),
	Length < Min,
	!.
normalized_tokenized_sent_fails_length_restrictions(Length, Parameters) :-
	member(max_length = Max, Parameters),
	Length > Max,
	!.

%---------------------------------------------------------------

make_rep_corpus1(I-N, _OutCorpus, _TraceCorpus, _Encoding, _GroupSize, _Parameters) :-
	I >= N,
	!.
make_rep_corpus1(I-N, AbsOutCorpus, AbsTraceCorpus, Encoding, GroupSize, Parameters) :-
	I1 is I + 1,
	NextSentNumber is I * GroupSize + 1,
	LastInCycleSentNumber is I * GroupSize + GroupSize,
	format_to_atom('Cycle ~d (sents ~d-~d)', [I1, NextSentNumber, LastInCycleSentNumber], CycleMessage),
	order_corpus_sents(Parameters, OrderedCorpus),
	pick_new_examples(GroupSize, OrderedCorpus, NewExamples),
	write_out_new_examples(NewExamples, Parameters, Encoding, AbsOutCorpus, AbsTraceCorpus, CycleMessage),
	update_selected_corpus(NewExamples),
	!,
	make_rep_corpus1(I1-N, AbsOutCorpus, AbsTraceCorpus, Encoding, GroupSize, Parameters).

%---------------------------------------------------------------

order_corpus_sents(Parameters, OrderedCorpus) :-
	findall(NegScore-TokenisedSent,
		(   corpus_sentence_with_score(TokenisedSent, Parameters, Score),
		    NegScore is Score * -1
		),
		Pairs),
	keysort(Pairs, SortedPairs),
	unkey_list(SortedPairs, OrderedCorpus).

%---------------------------------------------------------------

corpus_sentence_with_score(TokenisedSent, Parameters, Score) :-
	corpus(TokenisedSent, TokenisedSentNoSpaces, StaticScore),
	weighted_ngram_score([p_unigram, p_bigram, p_trigram], TokenisedSentNoSpaces, Parameters, DynamicScore),
	Score is StaticScore + DynamicScore.

%---------------------------------------------------------------

pick_new_examples(GroupSize, OrderedCorpus, NewExamples) :-
	prefix_length(OrderedCorpus, NewExamples, GroupSize).

%---------------------------------------------------------------

update_selected_corpus(NewExamples) :-
	normalise_tokenized_sents(NewExamples, NewExamplesNoSpaces),
	%format('~NUpdating ngram counts... ', []),
	ngrams_in_examples(NewExamplesNoSpaces, NewNgrams),
	length(NewNgrams, _NNewNgrams),
	%format('~d ngrams... ', [NNewNgrams]),
	list_to_ordered_multiset(NewNgrams, NewExamplesMultiset),
	update_selected_corpus1(NewExamplesMultiset, 0-UnigramTotal, 0-BigramTotal, 0-TrigramTotal),
	update_selected_ngram_totals(UnigramTotal, BigramTotal, TrigramTotal),
	!.
update_selected_corpus(_NewExamples) :-
	format('~N*** Error: bad call to update_selected_corpus/1~n', []).

ngrams_in_examples(Examples, NGrams) :-
	findall(NGram,
		(   member(Example, Examples),
		    member(Length, [1, 2, 3]),
		    ngram_in_list(Example, Length, NGram)
		),
		NGrams
	       ).	       

update_selected_corpus1([], UniI-UniI, BiI-BiI, TriI-TriI).
update_selected_corpus1([F | R], UniI-UniO, BiI-BiO, TriI-TriO) :-
	update_selected_corpus_single(F, UniI-UniN, BiI-BiN, TriI-TriN),
	!,
	update_selected_corpus1(R, UniN-UniO, BiN-BiO, TriN-TriO).

update_selected_corpus_single(Count-Ngram, UniI-UniO, BiI-BiO, TriI-TriO) :-
	update_selected_ngram_count(Ngram, Count),
	length(Ngram, Length),
	add_to_total_ngram_count(Length, Count, UniI-UniO, BiI-BiO, TriI-TriO).

update_selected_ngram_count(Ngram, Count) :-
	Ngram = [F | _],
	(   selected_ngram(F, Ngram, OldCount) ->
	    NewCount is OldCount + Count,
	    retract(selected_ngram(F, Ngram, OldCount))
	;
	    otherwise ->
	    NewCount = Count
	),
	assertz(selected_ngram(F, Ngram, NewCount)).

add_to_total_ngram_count(1, Count, UniI-UniO, BiI-BiI, TriI-TriI) :-
	UniO is UniI + Count.
add_to_total_ngram_count(2, Count, UniI-UniI, BiI-BiO, TriI-TriI) :-
	BiO is BiI + Count.
add_to_total_ngram_count(3, Count, UniI-UniI, BiI-BiI, TriI-TriO) :-
	TriO is TriI + Count.

update_selected_ngram_totals(UnigramTotal, BigramTotal, TrigramTotal) :-
	update_selected_ngram_total(1, UnigramTotal),
	update_selected_ngram_total(2, BigramTotal),
	update_selected_ngram_total(3, TrigramTotal).

update_selected_ngram_total(Length, Addition) :-
	(   total_score_for_selected_ngram_of_length(Length, OldTotal) ->
	    NewTotal is OldTotal + Length,
	    retract(total_score_for_selected_ngram_of_length(Length, OldTotal))
	;
	    otherwise ->
	    NewTotal = Addition
	),
	assertz(total_score_for_selected_ngram_of_length(Length, NewTotal)).

%---------------------------------------------------------------

write_out_new_examples(Examples, Parameters, Encoding, AbsOutCorpus, AbsTraceCorpus, CycleMessage) :-
	tokenized_list_to_atom_list(Examples, ExamplesAtoms),
	(   Encoding = default_encoding ->
	    open(AbsOutCorpus, append, SOut),
	    open(AbsTraceCorpus, append, STrace)
	;
	    otherwise ->
	    open(AbsOutCorpus, append, SOut, [encoding(Encoding)]),
	    open(AbsTraceCorpus, append, STrace, [encoding(Encoding)])
	),
	write_out_new_examples1(SOut, ExamplesAtoms),
	
	format('~N~n~w~n', [CycleMessage]),
	write_out_new_examples_trace(user, Examples, Parameters),

	format(STrace, '~N~n~w~n', [CycleMessage]),
	write_out_new_examples_trace(STrace, Examples, Parameters),
	close(SOut),
	close(STrace).

write_out_new_examples1(_S, []).
write_out_new_examples1(S, [F | R]) :-
	format(S, '~N~w~n', [F]),
	!,
	write_out_new_examples1(S, R).

write_out_new_examples_trace(_S, [], _Parameters).
write_out_new_examples_trace(S, [F | R], Parameters) :-
	write_out_new_example_trace(S, F, Parameters),
	!,
	write_out_new_examples_trace(S, R, Parameters).

write_out_new_example_trace(S, TokenizedSent, Parameters) :-
	normalise_tokenized_sent(TokenizedSent, NormalisedTokenizedSent),
	length(NormalisedTokenizedSent, NWords),
	tokenized_sent_to_atom(TokenizedSent, AtomSent),
	corpus_sentence_with_score(TokenizedSent, Parameters, TotalScore),
	
	Components = [unigram, bigram, trigram, p_unigram, p_bigram, p_trigram],
	weighted_ngram_score1(Components, NormalisedTokenizedSent, Parameters, Scores),
	
	format(S, '~N~n~1f: ', [TotalScore]),
	write_out_new_example_trace_with_parameters(S, Components, Scores),
	format(S, '~N(~d tokens) ~w~n', [NWords, AtomSent]),
	!.
write_out_new_example_trace(S, TokenizedSent, Parameters) :-
	format('~N*** Error: bad call: ~w~n',
	       [write_out_new_example_trace(S, TokenizedSent, Parameters)]),
	fail.

write_out_new_example_trace_with_parameters(_S, [], []).
write_out_new_example_trace_with_parameters(S, [Para | Paras], [Weight * Score | Scores]) :-
	WeighedScore is Weight * Score,
	format(S, '~w: ~w * ~1f = ~1f ', [Para, Weight, Score, WeighedScore]),
	!,
	write_out_new_example_trace_with_parameters(S, Paras, Scores).

%---------------------------------------------------------------

weighted_ngram_score(Components, TokenizedSent, Parameters, Score) :-
	weighted_ngram_score1(Components, TokenizedSent, Parameters, Scores),
	combine_weighted_scores(Scores, 0-Score).

weighted_ngram_score1([], _TokenizedSent, _Parameters, []).
weighted_ngram_score1([Component | Components], TokenizedSent, Parameters, [Score | Scores]) :-
	weighted_ngram_score_component(Component, TokenizedSent, Parameters, Score),
	!,
	weighted_ngram_score1(Components, TokenizedSent, Parameters, Scores).

weighted_ngram_score_component(Component, TokenizedSent, Parameters, WeightedScore) :-
	member(Component=Weight, Parameters),
	component_type(Component, N, NgramType),
	ngrams_of_given_length_in_tokenized_list(N, TokenizedSent, Ngrams),
	ngram_scores(Ngrams, NgramType, Scores),
	%safe_average_list(Scores, Score),
	geometric_mean_of_list(Scores, Score),
	WeightedScore = Weight * Score,
	!.

combine_weighted_scores([], In-In).
combine_weighted_scores([Weight * Score | R], In-Out) :-
	WeightedScore is Weight * Score,
	Next is In + WeightedScore,
	!,
	combine_weighted_scores(R, Next-Out).

ngram_scores([], _NgramType, []).
ngram_scores([Ngram | Ngrams], NgramType, [Score | Scores]) :-
	ngram_score_of_type(Ngram, NgramType, Score),
	!,
	ngram_scores(Ngrams, NgramType, Scores).

ngram_score_of_type(Ngram, static, Score) :-
	Ngram = [F | _R],
	user:ngram(F, Ngram, Score),
	!.
ngram_score_of_type(Ngram, dynamic, Score) :-
	selected_ngram_score(Ngram, Score),
	!.
ngram_score_of_type(_Ngram, _Type, Score) :-
	Score = 1.0.

selected_ngram_score(Ngram, Score) :-
	Ngram = [F | _R],
	selected_ngram(F, Ngram, RawScore),
	length(Ngram, Length),
	total_score_for_selected_ngram_of_length(Length, Total),
	(   ( Total = 0 ; RawScore = 0 ) ->
	    Score = 1.0
	;
	    Score is ( 1000000 * RawScore ) / Total
	).

ngrams_of_given_length_in_tokenized_list(N, TokenizedSent, NGrams) :-
	findall(NGram,
		ngram_in_list(TokenizedSent, N, NGram),
		NGrams).

ngram_in_list(List, N, NGram) :-
	prefix_length(List, NGram, N).
ngram_in_list([_F | R], N, NGram) :-
	ngram_in_list(R, N, NGram).

component_type(unigram, 1, static).
component_type(bigram, 2, static).
component_type(trigram, 3, static).
component_type(p_unigram, 1, dynamic).
component_type(p_bigram, 2, dynamic).
component_type(p_trigram, 3, dynamic).

normalise_tokenized_sents([], []).
normalise_tokenized_sents([F | R], [F1 | R1]) :-
	normalise_tokenized_sent(F, F1),
	!,
	normalise_tokenized_sents(R, R1).

normalise_tokenized_sent([], []).
normalise_tokenized_sent([' ' | R], R1) :-
	!,
	normalise_tokenized_sent(R, R1).
normalise_tokenized_sent([F | R], [F1 | R1]) :-
	lowercase_atom(F, F1),
	!,
	normalise_tokenized_sent(R, R1).

%---------------------------------------------------------------

read_rep_corpus(InCorpus, Encoding, InList) :-
	read_file_to_atom_list(InCorpus, Encoding, InList),
	length(InList, N),
	format('~N--- Read corpus file (~d records) from ~w~n', [N, InCorpus]).

clean_rep_corpus1(InList, OutList) :-
	format('~N--- Cleaning corpus~n', []),
	clean_rep_corpus2(InList, NextList, 0),
	format('~N--- Removing duplicates~n', []),
	remove_duplicates_from_rep_corpus(NextList, OutList).

clean_rep_corpus2([], [], _I).
clean_rep_corpus2([F | R], Out, I) :-
	I1 is I + 1,
	(   clean_rep_corpus_item(F, F1) ->
	    Out = [F1 | R1]
	;
	    otherwise ->
	    Out = R1
	),
	(   0 is I1 mod 1000 ->
	    
	    format(' (~d)', [I1]),
	    flush_output(user) ;
	    
	    true
	),
	!,
	clean_rep_corpus2(R, R1, I1).

clean_rep_corpus_item(Atom, Atom1) :-
	atom_codes(Atom, Str),
	(   string_contains_dubious_substring(Str) ->
	    fail
	;
	    otherwise ->
	    tokenize_sent(Str, Tokenized),
	    clean_rep_corpus_item_words(Tokenized, Tokenized1),
	    tokenized_sent_to_atom(Tokenized1, Atom1)
	),
	!.

clean_rep_corpus_item_words([], []).
clean_rep_corpus_item_words(Words, Words1) :-
	clean_rewrite(Words-WordsOut, Words1-Words1Out),
	!,
	clean_rep_corpus_item_words(WordsOut, Words1Out).
clean_rep_corpus_item_words([F | R], [F | R1]) :-
	!,
	clean_rep_corpus_item_words(R, R1).

string_contains_dubious_substring(Str) :-
	dubious_substring(Substr),
	is_substring(Substr, Str),
	!.

remove_duplicates_from_rep_corpus(InList, OutList) :-
	safe_remove_duplicates_preserving_order(InList, OutList).

write_rep_corpus(OutList, Encoding, OutCorpus) :-
	length(OutList, N),
	write_atom_list_to_unicode_file(OutList, OutCorpus, Encoding),
	format('~N--- Written corpus file (~d records) to ~w~n', [N, OutCorpus]).

%---------------------------------------------------------------

geometric_mean_of_list([], 1) :-
	!.
geometric_mean_of_list(List, Mean) :-
	length(List, N),
	sum_of_logs(List, 0-Sum),
	Mean is exp(Sum / N).

sum_of_logs([], In-In).
sum_of_logs([F | R], In-Out) :-
	Next is log(F) + In,
	!,
	sum_of_logs(R, Next-Out).

%---------------------------------------------------------------

overwrite_file(File) :-
	open(File, write, S),
	close(S).

%---------------------------------------------------------------

tokenized_list_to_atom_list([], []).
tokenized_list_to_atom_list([F | R], [F1 | R1]) :-
	tokenized_sent_to_atom(F, F1),
	!,
	tokenized_list_to_atom_list(R, R1).
