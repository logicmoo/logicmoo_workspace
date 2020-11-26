
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(analyse_pronunciations,
	  [get_pronunciations/3,
	   get_pronunciations_for_file/3,
	   get_pronunciations_for_file/4,
	   find_homophones_for_pronunciation_list/2,
	   find_confusion_sets/2,
	   merge_confusion_sets/2,
	   print_confusion_sets/2,
	   frequency_sort_confusion_file/2,
	   frequency_filter_confusion_file/4,
	   corpus_file_to_cfn_file/4,
	   
	   store_frequencies/1,
	   
	   test_analyse_pronunciations/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

/*

get_pronunciations(+Word, +Package, -Pronunciations)

get_pronunciations_for_file(+InFile, +Package, +OutFile)

get_pronunciations_for_file(+InFile, +Encoding, +Package, +OutFile)

find_homophones_for_pronunciation_list(InFile, OutFile).

*/

%---------------------------------------------------------------

test_analyse_pronunciations(1) :-
	Word = noires,
	get_pronunciations(Word, 'French', Pronunciations),
	format('~N~w:~n', [Word]),
	prettyprint(Pronunciations).

test_analyse_pronunciations(2) :-
	get_pronunciations_for_file('$ACCEPT/MT/Homophones/Data/small1.txt',
				    'French',
				    '$ACCEPT/MT/Homophones/Data/small_prons1.pl').

test_analyse_pronunciations(3) :-
	find_homophones_for_pronunciation_list('$ACCEPT/MT/Homophones/Data/fr_forum_pronunciations.pl',
					       '$ACCEPT/MT/Homophones/Data/fr_forum_homophones.pl'). 


test_analyse_pronunciations(4) :-
	find_confusion_sets('$ACCEPT/MT/Homophones/Data/fr_forum_homophones.pl',
			    '$ACCEPT/MT/Homophones/Data/fr_forum_alternates.pl'). 

test_analyse_pronunciations('4a') :-
	merge_confusion_sets(['$ACCEPT/MT/Homophones/Data/fr_forum_alternates.pl',
			      '$ACCEPT/MT/Homophones/Data/fr_forum_accent_alternates.pl'],
			     '$ACCEPT/MT/Homophones/Data/fr_forum_homophone_and_accent_alternates.pl'). 

test_analyse_pronunciations(5) :-
	print_confusion_sets('$ACCEPT/MT/Homophones/Data/fr_forum_alternates.pl',
			     '$ACCEPT/MT/Homophones/Data/fr_forum_alternates.txt'). 

test_analyse_pronunciations('5a') :-
	print_confusion_sets('$ACCEPT/MT/Homophones/Data/fr_forum_homophone_and_accent_alternates.pl',
			     '$ACCEPT/MT/Homophones/Data/fr_forum_homophone_and_accent_alternates.txt').

test_analyse_pronunciations(6) :-
	store_frequencies('$ACCEPT/MT/Homophones/Data/homophone_counts.txt',
			  'UTF-8').

test_analyse_pronunciations('7a') :-
	frequency_sort_confusion_file('$ACCEPT/MT/Homophones/Data/fr_forum_homophone_and_accent_alternates.txt',
				      'UTF-8',
				      '$ACCEPT/MT/Homophones/Data/fr_forum_homophone_and_accent_alternates_freq_sorted.txt').

test_analyse_pronunciations('8a') :-
	frequency_filter_confusion_file('$ACCEPT/MT/Homophones/Data/fr_forum_homophone_and_accent_alternates_freq_sorted.txt',
					'UTF-8',
					20,
					'$ACCEPT/MT/Homophones/Data/fr_forum_homophone_and_accent_alternates_freq_sorted_filtered.txt').

test_analyse_pronunciations(9) :-
	store_alternates('$ACCEPT/MT/Homophones/Data/fr_forum_homophone_and_accent_alternates_freq_sorted_filtered_edited.txt',
			       'UTF-8').

test_analyse_pronunciations(10) :-
	corpus_file_to_cfn_file(%'$ACCEPT/MT/Homophones/Data/devtest_a_small.fr',
				'$ACCEPT/MT/Homophones/Data/devtest_a.fr',
				'UTF-8',
				uniform,
				'$ACCEPT/MT/Homophones/Data/devtest_a_filtered_edited_uniform.cfn').

test_analyse_pronunciations(11) :-
	corpus_file_to_cfn_file(%'$ACCEPT/MT/Homophones/Data/devtest_a_small.fr',
				'$ACCEPT/MT/Homophones/Data/devtest_a.fr',
				'UTF-8',
				unigram,
				'$ACCEPT/MT/Homophones/Data/devtest_a_filtered_edited_unigram.cfn').

test_analyse_pronunciations(12) :-
	corpus_file_to_cfn_file(%'$ACCEPT/MT/Homophones/Data/devtest_b_small.fr',
				'$ACCEPT/MT/Homophones/Data/devtest_b.fr',
				'UTF-8',
				uniform,
				'$ACCEPT/MT/Homophones/Data/devtest_b_filtered_edited_uniform.cfn').

test_analyse_pronunciations(13) :-
	corpus_file_to_cfn_file(%'$ACCEPT/MT/Homophones/Data/devtest_b_small.fr',
				'$ACCEPT/MT/Homophones/Data/devtest_b.fr',
				'UTF-8',
				unigram,
				'$ACCEPT/MT/Homophones/Data/devtest_b_filtered_edited_unigram.cfn').
				
%---------------------------------------------------------------

print_confusion_sets(InFile, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),

	safe_prolog_file_to_list_printing_statistics(AbsInFile, List),
	
	confusion_sets_to_print_forms(List, List1),

	write_atom_list_to_unicode_file(List1, AbsOutFile),
	format('~NWritten confusion sets to ~w~n', [AbsOutFile]),
	!.

confusion_sets_to_print_forms([], []).
confusion_sets_to_print_forms([F | R], [F1 | R1]) :-
	confusion_set_item_to_print_form(F, F1),
	!,
	confusion_sets_to_print_forms(R, R1).

% confusion_set(ancien, [ancien,anciene,ancienne,anciennes,anciens,enciene]).

confusion_set_item_to_print_form(F, F1) :-
	F = confusion_set(Word, Alternates),
	append_atoms(Alternates, 0',, AlternatesAtom),
	format_to_atom('~w|~w', [Word, AlternatesAtom], F1),
	!.
confusion_set_item_to_print_form(F, F1) :-
	format('*** Error: bad call: ~w~n',
	       [confusion_set_item_to_print_form(F, F1)]),
	fail.
	
%---------------------------------------------------------------

find_confusion_sets(InFile, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),

	load_homophone_sets(AbsInFile),

	all_words_with_homophones(Words),
	length(Words, NWords),
	format('~N~nFound ~d words with homophones~n', [NWords]),

	all_confusion_sets_for_words(Words, 0, ConfusionSets),
	list_to_prolog_file(ConfusionSets, AbsOutFile),

	format('~N~nWritten confusion set file (~d sets): ~w~n',
	       [NWords, AbsOutFile]),
	!.

:- dynamic stored_homophones_for_word/2.
	
load_homophone_sets(File) :-
	retractall(stored_homophones_for_word(_, _)),
	safe_prolog_file_to_list_printing_statistics(File, List),
	load_homophones_list(List),
	format('~N~nStored homophones~n', []),
	!.

load_homophones_list([]).
load_homophones_list([F | R]) :-
	load_homophone_item(F),
	!,
	load_homophones_list(R).

% homophones(['A',v,'E',r,'E'], [avérait,avéré,avérées,avérés]).

load_homophone_item(homophones(_Pron, Words)) :-
	load_homophone_item1(Words, Words),
	!.
load_homophone_item(Other) :-
	format('*** Error: bad call: ~w~n',
	       [load_homophone_item(Other)]),
	fail.

load_homophone_item1([], _Words).
load_homophone_item1([F | R], Words) :-
	assertz(stored_homophones_for_word(F, Words)),
	!,
	load_homophone_item1(R, Words).

all_words_with_homophones(Words) :-
	findall(Word,
		stored_homophones_for_word(Word, _Homophones),
		Words0),
	sort(Words0, Words).

all_confusion_sets_for_words([], _N, []).
all_confusion_sets_for_words([F | R], I, [F1 | R1]) :-
	confusion_set_for_word(F, F1),
	I1 is I + 1,
	format('.', []),
	(   0 is I1 mod 100 ->
	    
	    format(' (~d) ~n', [I1]),
	    flush_output(user)
	;
	    
	    true
	),
	!,
	all_confusion_sets_for_words(R, I1, R1).

confusion_set_for_word(Word, confusion_set(Word, Alternates)) :-
	findall(Homophones,
		stored_homophones_for_word(Word, Homophones),
		HomophoneLists),
	append_list(HomophoneLists, AllHomophones0),
	sort(AllHomophones0, Alternates).
	
%---------------------------------------------------------------

merge_confusion_sets(InFileList, OutFile) :-
	safe_absolute_file_name(OutFile, AbsOutFile),
	
	load_confusion_sets(InFileList),
	find_all_words_with_confusion_set(Words),
	length(Words, N),
	format('~NFound ~d words with confusion sets~n', [N]),
	
	all_merged_confusion_sets_for_words(Words, 0, ConfusionSets),
	list_to_prolog_file(ConfusionSets, AbsOutFile),

	format('~N~nWritten merged confusion set file (~d sets): ~w~n',
	       [N, AbsOutFile]),
	!.

:- dynamic stored_confusion_set/2.

load_confusion_sets(InFileList) :-
	retractall(stored_confusion_set(_, _)),
	load_confusion_sets1(InFileList).

load_confusion_sets1([]).
load_confusion_sets1([F | R]) :-	
	load_confusion_set(F),
	!,
	load_confusion_sets1(R).

load_confusion_set(File) :-
	safe_absolute_file_name(File, AbsFile),
	safe_prolog_file_to_list_printing_statistics(AbsFile, List),
	load_confusion_set_list(List),
	!.

load_confusion_set_list([]).
load_confusion_set_list([F | R]) :-
	load_confusion_set_item(F),
	!,
	load_confusion_set_list(R).

load_confusion_set_item(confusion_set(W, S)) :-
	assertz(stored_confusion_set(W, S)),
	!.

find_all_words_with_confusion_set(Words) :-
	findall(Word,
		stored_confusion_set(Word, _S),
		Words0),
	sort(Words0, Words).

all_merged_confusion_sets_for_words([], _N, []).
all_merged_confusion_sets_for_words([F | R], I, [F1 | R1]) :-
	merged_confusion_set_for_word(F, F1),
	I1 is I + 1,
	format('.', []),
	(   0 is I1 mod 100 ->
	    
	    format(' (~d) ~n', [I1]),
	    flush_output(user)
	;
	    
	    true
	),
	!,
	all_merged_confusion_sets_for_words(R, I1, R1).

merged_confusion_set_for_word(Word, confusion_set(Word, Alternates)) :-
	findall(Homophones,
		stored_confusion_set(Word, Homophones),
		HomophoneLists),
	append_list(HomophoneLists, AllHomophones0),
	sort(AllHomophones0, Alternates).

%---------------------------------------------------------------

find_homophones_for_pronunciation_list(InFile, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),

	load_pronunciations_file(AbsInFile),
	
	all_pronunciations(AllProns),
	length(AllProns, NAllProns),
	format('~N~nFound ~d distinct pronunciations~n', [NAllProns]),
	
	all_homophone_lists(AllProns, 0-_NProns, AllHomophoneLists),
	
	list_to_prolog_file(AllHomophoneLists, AbsOutFile),
	length(AllHomophoneLists, NHomophones),

	format('~N~nWritten homophone file (~d lists): ~w~n',
	       [NHomophones, AbsOutFile]),
	!.

:- dynamic stored_pronunciation/2.

load_pronunciations_file(File) :-
	retractall(stored_pronunciation(_, _)),
	safe_prolog_file_to_list_printing_statistics(File, List),
	load_pronunciations_list(List),
	format('~N~nStored pronunciations~n', []),
	!.

load_pronunciations_list([]).
load_pronunciations_list([F | R]) :-
	load_pronunciation_item(F),
	!,
	load_pronunciations_list(R).

load_pronunciation_item(pronunciation(Word, Pron)) :-
	assertz(stored_pronunciation(Pron, Word)),
	!.
load_pronunciation_item(Other) :-
	format('*** Error: bad call: ~w~n',
	       [load_pronunciation_item(Other)]),
	fail.

all_pronunciations(AllProns) :-
	findall(Pron,
		stored_pronunciation(Pron, _Word),
		Prons),
	sort(Prons, AllProns).

all_homophone_lists([], N-N, []).
all_homophone_lists([F | R], In-Out, Output) :-
	(   nontrivial_homophone_list(F, F1) ->
	    Output = [F1 | R1]
	;
	    otherwise ->
	    Output = R1
	),
	Next is In + 1,
	format('.', []),
	(   0 is Next mod 100 ->
	    
	    format(' (~d) ~n', [Next]),
	    flush_output(user)
	;
	    
	    true
	),
	!,
	all_homophone_lists(R, Next-Out, R1).

nontrivial_homophone_list(Pron, homophones(Pron, Words)) :-
	findall(Word,
		stored_pronunciation(Pron, Word),
		Words0),
	sort(Words0, Words),
	length(Words, N),
	N > 1.
	
%---------------------------------------------------------------

get_pronunciations_for_file(InFile, Package, OutFile) :-
	get_pronunciations_for_file(InFile, default_encoding, Package, OutFile).

get_pronunciations_for_file(InFile, Encoding, Package, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),
	
	read_file_to_atom_list(AbsInFile, Encoding, List),
	length(List, NWordsCount),
	format('~N~nLooking up pronunciations (~d words)~n', [NWordsCount]),
	open(AbsOutFile, write, S),
	get_pronunciations_for_list(List, Package, 0-NWords, 0-NProns, S),
	close(S),
	format('~N~nWritten pronunciation file (~d words, ~d pronunciations): ~w~n',
	       [NWords, NProns, AbsOutFile]),
	!.

get_pronunciations_for_list([], _Package, NWordsIn-NWordsIn, NPronsIn-NPronsIn, _S).
get_pronunciations_for_list([F | R], Package, NWordsIn-NWordsOut, NPronsIn-NPronsOut, S) :-
	get_and_write_pronunciations_for_item(F, Package, NWordsIn-NWordsNext, NPronsIn-NPronsNext, S),
	!,
	get_pronunciations_for_list(R, Package, NWordsNext-NWordsOut, NPronsNext-NPronsOut, S).

get_and_write_pronunciations_for_item(Word, Package, NWordsIn-NWordsOut, NPronsIn-NPronsOut, S) :-
	get_pronunciations(Word, Package, Pronunciations),
	length(Pronunciations, NPronunciations),
	NWordsOut is NWordsIn + 1,
	NPronsOut is NPronsIn + NPronunciations,
	format('.', []),
	(   0 is NWordsOut mod 100 ->
	    
	    format(' (~d) ~n', [NWordsOut]),
	    flush_output(user)
	;
	    
	    true
	),
	write_pronunciations_for_item(Pronunciations, Word, S).

write_pronunciations_for_item([], _Word, _S).
write_pronunciations_for_item([F | R], Word, S) :-
	write_pronunciation_for_item(F, Word, S),
	write_pronunciations_for_item(R, Word, S).

write_pronunciation_for_item(Pron, Word, S) :-
	format(S, '~N~q.~n', [pronunciation(Word, Pron)]),
	!.

%---------------------------------------------------------------

get_pronunciations(Word, _Package, Pronunciations) :-
	\+ okay_word_for_pronounce(Word),
	Pronunciations = [].
get_pronunciations(Word, Package, Pronunciations) :-
	system_on_list_to_result([pronounce, Package, Word], List),
	parse_pronunciation_list(List, Word, Pronunciations),
	!.
get_pronunciations(Word, Package, Pronunciations) :-
	format('*** Warning: bad call: ~w~n',
	       [get_pronunciations(Word, Package, Pronunciations)]),
	Pronunciations = [].

parse_pronunciation_list([], _Word, []).
parse_pronunciation_list([F | R], Word, [F1 | R1]) :-
	parse_pronunciation_item(F, Word, F1),
	!,
	parse_pronunciation_list(R, Word, R1).
parse_pronunciation_list([_F | R], Word, R1) :-
	!,
	parse_pronunciation_list(R, Word, R1).

parse_pronunciation_item(F, Word, F1) :-
	split_atom_into_words(F, Words),
	% In real lines, first word should be the word
	% whose pronunciation we're looking up
	Words = [Word | F1],
	!.

okay_word_for_pronounce(Word) :-
	atom_codes(Word, Codes),
	length(Codes, N),
	N > 0,
	okay_chars_for_pronounce(Codes).

okay_chars_for_pronounce([]).
okay_chars_for_pronounce([F | R]) :-
	okay_char_for_pronounce(F),
	okay_chars_for_pronounce(R).

okay_char_for_pronounce(C) :-
	lowercase_char(C),
	!.
okay_char_for_pronounce(C) :-
	uppercase_char(C),
	!.
okay_char_for_pronounce(0'-).

%---------------------------------------------------------------

:- dynamic stored_frequency/2.

store_frequencies(File, Encoding) :-
	retractall(stored_frequency(_, _)),
	
	safe_absolute_file_name(File, AbsFile),
	
	read_file_to_atom_list(AbsFile, Encoding, List),
	length(List, N),
	format('~N--- Read freq file (~d lines)~n', [N]),
	
	store_frequencies1(List, 0),
	format('~N--- Stored frequencies~n', []),
	!.

store_frequencies1([], _N).
store_frequencies1([F | R], In) :-
	store_frequencies_for_line(F),
	Next is In + 1,
	format('.', []),
	(   0 is Next mod 1000 ->
	    
	    format(' (~d) ~n', [Next]),
	    flush_output(user)
	;
	    
	    true
	),
	!,
	store_frequencies1(R, Next).

%   129652              ae  9 est:122961 elles:3031 ai:2091 ait:714 es:539 aient:311 ais:2 aie:2 oeufs:1

store_frequencies_for_line(LineAtom) :-
	split_atom_into_words(LineAtom, Words),
	store_frequencies_for_line1(Words),
	!.

store_frequencies_for_line1([]).
store_frequencies_for_line1([F | R]) :-
	store_frequencies_for_line2(F),
	!,
	store_frequencies_for_line1(R).

store_frequencies_for_line2(Line) :-
	split_atom_into_words(Line, 0':, Words),
	Words = [Word, Freq0],
	coerce_atom_to_int(Freq0, Freq),
	\+ stored_frequency(Word, Freq),
	assertz(stored_frequency(Word, Freq)),
	!.
store_frequencies_for_line2(_Word).

%---------------------------------------------------------------

:- dynamic stored_alternates/2.

store_alternates(File, Encoding) :-
	retractall(stored_alternates(_, _)),
	
	safe_absolute_file_name(File, AbsFile),
	
	read_file_to_atom_list(AbsFile, Encoding, List),
	length(List, N),
	format('~N--- Read alternates file (~d lines)~n', [N]),
	
	store_alternates1(List, 0),
	format('~N--- Stored alternates~n', []),
	!.

store_alternates1([], _N).
store_alternates1([F | R], In) :-
	store_alternates_for_line(F),
	Next is In + 1,
	format('.', []),
	(   0 is Next mod 1000 ->
	    
	    format(' (~d) ~n', [Next]),
	    flush_output(user)
	;
	    
	    true
	),
	!,
	store_alternates1(R, Next).

%   129652              ae  9 est:122961 elles:3031 ai:2091 ait:714 es:539 aient:311 ais:2 aie:2 oeufs:1

store_alternates_for_line(LineAtom) :-
	split_atom_into_words(LineAtom, 0'|, [Head, AlternatesAtom]),
	split_atom_into_words(AlternatesAtom, 0',, Alternates),
	assertz(stored_alternates(Head, Alternates)),
	!.

%---------------------------------------------------------------

frequency_sort_confusion_file(InFile, Encoding, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),

	read_file_to_atom_list(AbsInFile, Encoding, List),
	length(List, N),
	format('~N--- Read confusion set file (~d lines)~n', [N]),

	frequency_tag_confusion_set_list(List, 0, List1),
	keysort(List1, SortedList1),
	reverse(SortedList1, SortedList2),
	unkey_list(SortedList2, SortedList3),

	write_atom_list_to_unicode_file(SortedList3, AbsOutFile, Encoding),
	!.

frequency_tag_confusion_set_list([], _N, []).
frequency_tag_confusion_set_list([F | R], In, [F1 | R1]) :-
	frequency_tag_confusion_set_item(F, F1),
	Next is In + 1,
	format('.', []),
	(   0 is Next mod 100 ->
	    
	    format(' (~d) ~n', [Next]),
	    flush_output(user)
	;
	    
	    true
	),
	!,
	frequency_tag_confusion_set_list(R, Next, R1).

% redirigé|redirige,rediriger,redirigé

frequency_tag_confusion_set_item(Line, Freq-Line) :-
	split_atom_into_words(Line, 0'|, [Word | _]),
	stored_frequency_or_zero(Word, Freq).

%---------------------------------------------------------------

frequency_filter_confusion_file(InFile, Encoding, Factor, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),

	read_file_to_atom_list(AbsInFile, Encoding, List),
	length(List, N),
	format('~N--- Read confusion set file (~d lines)~n', [N]),

	frequency_filter_confusion_set_list(List, Factor, 0, List1),
	length(List1, N1),

	write_atom_list_to_unicode_file(List1, AbsOutFile, Encoding),
	format('~N--- Written filtered confusion set file (~d lines)~n', [N1]),
	!.

frequency_filter_confusion_set_list([], _Factor, _N, []).
frequency_filter_confusion_set_list([F | R], Factor, In, Result) :-
	frequency_filter_confusion_set_item(F, Factor, NLeft, F1),
	(   NLeft > 1 ->
	    Result = [F1 | R1]
	;
	    otherwise ->
	    Result = R1
	),
	Next is In + 1,
	format('.', []),
	(   0 is Next mod 100 ->
	    
	    format(' (~d) ~n', [Next]),
	    flush_output(user)
	;
	    
	    true
	),
	!,
	frequency_filter_confusion_set_list(R, Factor, Next, R1).

% redirigé|redirige,rediriger,redirigé

frequency_filter_confusion_set_item(Line, Factor, NLeft, Line1) :-
	split_atom_into_words(Line, 0'|, [FromWord, ToWordsAtom]),
	split_atom_into_words(ToWordsAtom, 0',, ToWords),
	
	stored_frequency_or_zero(FromWord, FromFreq),
	Cutoff is FromFreq / Factor,
	
	frequency_filter_confusion_set_item1(ToWords, Cutoff, ToWords1),
	length(ToWords1, NLeft),
	
	append_atoms(ToWords1, 0',, ToWords1Atom),
	format_to_atom('~w|~w', [FromWord, ToWords1Atom], Line1),
	!.
frequency_filter_confusion_set_item(Line, Factor, NLeft, Line1) :-
	format('*** Error: bad call: ~w~n',
	       [frequency_filter_confusion_set_item(Line, Factor, NLeft, Line1)]),
	fail.

frequency_filter_confusion_set_item1([], _Cutoff, []).
frequency_filter_confusion_set_item1([F | R], Cutoff, Result) :-
	stored_frequency_or_zero(F, Freq),
	(   Freq < Cutoff ->
	    Result = R1
	;
	    otherwise ->
	    Result = [F | R1]
	),
	!,
	frequency_filter_confusion_set_item1(R, Cutoff, R1).

stored_frequency_or_zero(Word, Freq) :-
	(   stored_frequency(Word, Freq) ->
	    true
	;
	    Freq = 0
	),
	!.

stored_frequency_or_small_number(Word, Freq) :-
	(   stored_frequency(Word, Freq) ->
	    true
	;
	    Freq = 0.0001
	),
	!.

%---------------------------------------------------------------

corpus_file_to_cfn_file(InFile, Encoding, Type, OutFile) :-
	check_cfn_type(Type),
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),

	read_file_to_atom_list(AbsInFile, Encoding, List),
	length(List, N),
	format('~N--- Read corpus file (~d lines)~n', [N]),

	open(AbsOutFile, write, S, [encoding(Encoding)]),

	corpus_list_to_cfn_file(List, Type, 0, S),

	close(S),
	format('~N--- Written CFN file: ~w~n', [AbsOutFile]),
	!.

check_cfn_type(uniform) :-
	!.
check_cfn_type(unigram) :-
	!.
check_cfn_type(Other) :-
	format('~N*** Error: unknown CFN type ~w~n', [Other]),
	fail.

corpus_list_to_cfn_file([], _Type, _N, _S).
corpus_list_to_cfn_file([F | R], Type, In, S) :-
	corpus_item_to_cfn_item(F, Type, S),
	Next is In + 1,
	format('.', []),
	(   0 is Next mod 100 ->
	    
	    format(' (~d) ~n', [Next]),
	    flush_output(user)
	;
	    
	    true
	),
	corpus_list_to_cfn_file(R, Type, Next, S).

corpus_item_to_cfn_item(SentAtom, Type, S) :-
	format(S, '~N~n', []),
	split_atom_into_words(SentAtom, Words),
	words_to_cfn_items(Words, Type, S),
	!.

words_to_cfn_items([], _Type, _S).
words_to_cfn_items([F | R], Type, S) :-
	word_to_cfn_line(F, Type, S),
	!,
	words_to_cfn_items(R, Type, S).

word_to_cfn_line(Word, Type, S) :-
	get_alternates_for_word(Word, Alternates),
	get_probabilities_for_alternates(Alternates, Type, Probabilities),
	word_to_cfn_line1(Alternates, Probabilities, S).

word_to_cfn_line1([Alternate], _Probabilities, S) :-
	format(S, '~N~w 1~n', [Alternate]),
	!.
word_to_cfn_line1(Alternates, Probabilities, S) :-
	format(S, '~N', []),
	word_to_cfn_line2(Alternates, Probabilities, S),
	format(S, '~n', []),
	!.

word_to_cfn_line2([], [], _S).
word_to_cfn_line2([F | R], [F1 | R1], S) :-
	format(S, '~w ~5f ', [F, F1]),
	!,
	word_to_cfn_line2(R, R1, S).

get_alternates_for_word(Word, Alternates) :-
	(   stored_alternates(Word, Alternates) ->
	    true
	;
	    otherwise ->
	    Alternates = [Word]
	).

get_probabilities_for_alternates([_Alternate], _Type, [1]) :-
	!.
get_probabilities_for_alternates(Alternates, uniform, Probabilities) :-
	length(Alternates, N),
	P is 1.0 / N,
	get_probabilities_for_alternates_uniform(Alternates, Probabilities, P),
	!.
get_probabilities_for_alternates(Alternates, unigram, Probabilities) :-
	get_probabilities_for_alternates_unigram1(Alternates, Probabilities0),
	normalise_probabilities(Probabilities0, Probabilities),
	!.

get_probabilities_for_alternates_uniform([], [], _P).
get_probabilities_for_alternates_uniform([_F | _R], [P | R1], P) :-
	get_probabilities_for_alternates_uniform(_R, R1, P).

get_probabilities_for_alternates_unigram1([], []).
get_probabilities_for_alternates_unigram1([F | R], [F1 | R1]) :-
	stored_frequency_or_small_number(F, F1),
	!,
	get_probabilities_for_alternates_unigram1(R, R1).

normalise_probabilities(Probabilities0, Probabilities) :-
	safe_sum_list(Probabilities0, Total),
	normalise_probabilities1(Probabilities0, Total, Probabilities).

normalise_probabilities1([], _Total, []).
normalise_probabilities1([F | R], Total, [F1 | R1]) :-
	F1 is F / Total,
	!,
	normalise_probabilities1(R, Total, R1).


