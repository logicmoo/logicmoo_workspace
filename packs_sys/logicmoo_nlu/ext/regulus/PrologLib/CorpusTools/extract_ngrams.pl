:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(extract_ngrams,
	  [extract_ngrams/2,
	   count_ngrams/2,
	   
	   test_extract_ngrams/1,
	   test_count_ngrams/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/CorpusTools/utils').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

test_extract_ngrams(very_small) :-
	extract_ngrams('$ACCEPT/MT/Europarl/Generated/europarl_ez_tokenized_very_small.pl',
		       '$ACCEPT/MT/Europarl/Generated/europarl_ez_very_small_raw_ngrams.pl',
		       '$ACCEPT/MT/Europarl/Generated/europarl_ez_very_small_sorted_ngrams.pl',
		       '$ACCEPT/MT/Europarl/Generated/europarl_ez_very_small_ngrams.pl').

test_extract_ngrams(small) :-
	extract_ngrams('$ACCEPT/MT/Europarl/Generated/europarl_ez_tokenized_small.pl',
		       '$ACCEPT/MT/Europarl/Generated/europarl_ez_small_raw_ngrams.pl',
		       '$ACCEPT/MT/Europarl/Generated/europarl_ez_small_sorted_ngrams.pl',
		       '$ACCEPT/MT/Europarl/Generated/europarl_ez_small_ngrams.pl').

test_extract_ngrams(forum) :-
	extract_ngrams('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl',
		       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_raw_ngrams.pl',
		       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_sorted_ngrams.pl',
		       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams.pl').

test_extract_ngrams(tm_fr) :-
	extract_ngrams('$ACCEPT/MT/GTFeb2012/TM/tm_fr_tokenised.pl',
		       '$ACCEPT/MT/GTFeb2012/TM/tm_fr_raw_ngrams.pl',
		       '$ACCEPT/MT/GTFeb2012/TM/tm_fr_sorted_ngrams.pl',
		       '$ACCEPT/MT/GTFeb2012/TM/tm_fr_ngrams.pl').

test_extract_ngrams(europarl_fr) :-
	extract_ngrams('$ACCEPT/MT/Europarl/Generated/europarl-v6-fr-tokenized.pl',
		       '$ACCEPT/MT/Europarl/Generated/europarl-v6-fr-raw_ngrams.pl',
		       '$ACCEPT/MT/Europarl/Generated/europarl-v6-fr-sorted_ngrams.pl',
		       '$ACCEPT/MT/Europarl/Generated/europarl-v6-fr-ngrams.pl').

test_extract_ngrams(translated_fr_forum) :-
	extract_ngrams('$ACCEPT/MT/PostEdition/Data/SymantecData/translated_fr_tokenized.pl',
		       '$ACCEPT/MT/PostEdition/Data/SymantecData/translated_fr_raw_ngrams.pl',
		       '$ACCEPT/MT/PostEdition/Data/SymantecData/translated_fr_sorted_ngrams.pl',
		       '$ACCEPT/MT/PostEdition/Data/SymantecData/translated_fr_ngrams.pl').
	
test_count_ngrams(small) :-
	count_ngrams('$ACCEPT/MT/Europarl/Generated/europarl_ez_small_sorted_ngrams.pl',
		     '$ACCEPT/MT/Europarl/Generated/europarl_ez_small_ngrams.pl').

test_count_ngrams(forum) :-
	count_ngrams('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_sorted_ngrams.pl',
		     '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams.pl').

test_count_ngrams(europarl_fr) :-
	count_ngrams('$ACCEPT/MT/Europarl/Generated/europarl-v6-fr-sorted_ngrams.pl',
		     '$ACCEPT/MT/Europarl/Generated/europarl-v6-fr-ngrams.pl').

test_count_ngrams(europarl_fr_10000) :-
	count_ngrams('$ACCEPT/MT/Europarl/Generated/europarl-v6-fr-sorted_ngrams-10000.pl',
		     '$ACCEPT/MT/Europarl/Generated/europarl-v6-fr-ngrams-10000.pl').
	
%---------------------------------------------------------------

n_for_ngram(1).
n_for_ngram(2).
n_for_ngram(3).
n_for_ngram(4).
n_for_ngram(5).
n_for_ngram(6).

minimum_interesting_count(5).

%---------------------------------------------------------------

extract_ngrams(InFile, TmpFile, SortedTmpFile, OutFile) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(TmpFile, AbsTmpFile),
	absolute_file_name(SortedTmpFile, AbsSortedTmpFile),
	absolute_file_name(OutFile, AbsOutFile),

	open(AbsInFile, read, SIn, [encoding('UTF-8')]),
	open(AbsTmpFile, write, SOut, [encoding('UTF-8')]),

	extract_ngrams1(SIn, SOut, 0-NIn, 0-NTmpOut),
	format('~N--- Read file (~d lines) ~w~n', [NIn, AbsInFile]),
	format('~N--- Written tmp ngrams file (~d lines) ~w~n', [NTmpOut, AbsTmpFile]),

	close(SIn),
	close(SOut),

	sort_and_count_ngrams(AbsTmpFile, AbsSortedTmpFile, AbsOutFile, NOut),
	format('~N--- Written full ngrams file (~d lines) ~w~n', [NOut, AbsOutFile]),	
	!.

count_ngrams(SortedTmpFile, OutFile) :-
	absolute_file_name(SortedTmpFile, AbsSortedTmpFile),
	absolute_file_name(OutFile, AbsOutFile),
	
	format('~N--- Reading input from sorted ngrams file ~w~n', [AbsSortedTmpFile]),
	count_ngrams(AbsSortedTmpFile, AbsOutFile, NOut),
	format('~N--- Written full ngrams file (~d lines) ~w~n', [NOut, AbsOutFile]),
	!.

%---------------------------------------------------------------

extract_ngrams1(SIn, SOut, NInI-NInO, NTmpI-NTmpO) :-
	read(SIn, TokenisedList),
	!,
	extract_ngrams2(TokenisedList, SIn, SOut, NInI-NInO, NTmpI-NTmpO).

extract_ngrams2(TokenisedList, _SIn, _SOut, NIn-NIn, NTmp-NTmp) :-
	TokenisedList = end_of_file.
extract_ngrams2(TokenisedList, SIn, SOut, NInI-NInO, NTmpI-NTmpO) :-
	extract_ngrams_from_line(TokenisedList, SOut, NInI-NInN, NTmpI-NTmpN),
	!,
	extract_ngrams1(SIn, SOut, NInN-NInO, NTmpN-NTmpO).

%---------------------------------------------------------------

extract_ngrams_from_line(List, SOut, NIn-NOut, NTmpIn-NTmpOut) :-
	remove_spaces_and_punctuation(List, List1),
	lowercase_atom_list(List1, List2),
	findall(NGram,
		(   n_for_ngram(N),
		    ngram_in_list(List2, N, NGram)
		),
		NGrams),
	write_out_ngrams_to_tmp(NGrams, SOut, NTmpIn-NTmpOut),
	NOut is NIn + 1,
	(   0 is NOut mod 1000 ->
	    format('~d ', [NOut]),
	    flush_output(user)
	;
	    otherwise ->
	    true
	),
	!.
extract_ngrams_from_line(List, SOut, N, NTmp) :-
	format('~N*** Error: bad call: ~w~n', [extract_ngrams_from_line(List, SOut, N, NTmp)]),
	fail.

write_out_ngrams_to_tmp([], _SOut, NIn-NIn).
write_out_ngrams_to_tmp([F | R], SOut, NIn-NOut) :-
	write_out_ngram_to_tmp(F, SOut, NIn-NNext),
	!,
	write_out_ngrams_to_tmp(R, SOut, NNext-NOut).

write_out_ngram_to_tmp(NGram, SOut, NIn-NOut) :-
	format(SOut, '~N~q.~n', [NGram]),
	NOut is NIn + 1,
	!.

ngram_in_list(List, N, NGram) :-
	prefix_length(List, NGram, N).
ngram_in_list([_F | R], N, NGram) :-
	ngram_in_list(R, N, NGram).

%---------------------------------------------------------------

sort_and_count_ngrams(TmpFile, SortedTmpFile, OutFile, NOut) :-
	format('~N--- Sorting ngram file... ', []),
	sort_file(TmpFile, SortedTmpFile),
	format('done~n', []),

	count_ngrams(SortedTmpFile, OutFile, NOut).

count_ngrams(SortedTmpFile, OutFile, NOut) :-
	format('~N--- Collecting ngrams... ', []),
	minimum_interesting_count(Min),
	
	open(SortedTmpFile, read, SIn, [encoding('UTF-8')]),
	open(OutFile, write, SOut, [encoding('UTF-8'), encoding_signature(true)]),

	count_ngrams(SIn, SOut, Min, '*nothing*', 0, 0-_NIn, 0-NOut),
	close(SIn),
	close(SOut),
	
	format('done~n', []),
	!.

%---------------------------------------------------------------

count_ngrams(SIn, SOut, Min, CurrentNGram, CurrentCount, NInI-NInO, NOutI-NOutO) :-
	read_line(SIn, NGram),
	!,
	count_ngrams1(NGram, SIn, SOut, Min, CurrentNGram, CurrentCount, NInI-NInO, NOutI-NOutO).

count_ngrams1(NGram, _SIn, SOut, Min, CurrentNGram, CurrentCount, NInI-NInI, NOutI-NOutO) :-
	NGram = end_of_file,
	write_out_ngram(CurrentNGram, CurrentCount, Min, SOut, NOutI-NOutO),
	!.
count_ngrams1(NGram, SIn, SOut, Min, CurrentNGram, CurrentCount, NInI-NInO, NOutI-NOutO) :-
	NInN is NInI + 1,
	(   0 is NInN mod 50000 ->
	    format('~d ', [NInN]),
	    flush_output(user)
	;
	    otherwise ->
	    true
	),
	(   strings_same_modulo_casing(NGram, CurrentNGram) ->
	    NextNGram = CurrentNGram,
	    NextCount is CurrentCount + 1,
	    NOutN = NOutI
	;
	    otherwise ->
	    write_out_ngram(CurrentNGram, CurrentCount, Min, SOut, NOutI-NOutN),
	    NextNGram = NGram,
	    NextCount = 1
	),
	!,
	count_ngrams(SIn, SOut, Min, NextNGram, NextCount, NInN-NInO, NOutN-NOutO).	    

write_out_ngram(_NGram, Count, Min, _SOut, NOutI-NOutI) :-
	Count < Min,
	!.
write_out_ngram(NGram, Count, _Min, SOut, NOutI-NOutO) :-
	NOutO is NOutI + 1,
	strip_off_period(NGram, NGramMinusPeriod),
	lowercase_string(NGramMinusPeriod, NGramMinusPeriodLowerCased),
	format(SOut, '~Nngram(~s, ~d).~n', [NGramMinusPeriodLowerCased, Count]),
	!.
write_out_ngram(NGramWithCount, SOut) :-
	format('~N*** Error: bad call: ~w~n', [write_out_ngram(NGramWithCount, SOut)]),
	fail.

strip_off_period(Str, StrMinusPeriod) :-
	last(StrMinusPeriod, _Period, Str),
	!.
strip_off_period(Str, Str).

%---------------------------------------------------------------
