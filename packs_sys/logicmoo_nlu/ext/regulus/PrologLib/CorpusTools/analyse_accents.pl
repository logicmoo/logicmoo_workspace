
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(analyse_accents,
	  [get_accents/2,
	   get_accents_for_file/2,
	   get_accents_for_file/3,
	   find_accent_classes_for_accent_list/2,
	   find_confusion_sets/2,
	   print_confusion_sets/2,
	   test_analyse_accents/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

/*

get_accents(+Word, -Accents)

get_accents_for_file(+InFile, +OutFile)

get_accents_for_file(+InFile, +Encoding, +OutFile)

find_accent_class_for_accent_list(InFile, OutFile).

*/

%---------------------------------------------------------------

test_analyse_accents(1) :-
	Word = accepté,
	get_accents(Word, [Deaccented]),
	format('~N~w -> ~w~n', [Word, Deaccented]).

test_analyse_accents(2) :-
	get_accents_for_file('$ACCEPT/MT/Homophones/Data/small1.txt',
			     '$ACCEPT/MT/Homophones/Data/small_prons1.pl').

test_analyse_accents('2a') :-
	get_accents_for_file('$ACCEPT/MT/Homophones/Data/mono-vocab.fr',
			     'UTF-8',
			     '$ACCEPT/MT/Homophones/Data/fr_forum_accents.pl').

test_analyse_accents(3) :-
	find_accent_classes_for_accent_list('$ACCEPT/MT/Homophones/Data/fr_forum_accents.pl',
					    '$ACCEPT/MT/Homophones/Data/fr_forum_accent_classes.pl'). 


test_analyse_accents(4) :-
	find_confusion_sets('$ACCEPT/MT/Homophones/Data/fr_forum_accent_classes.pl',
			    '$ACCEPT/MT/Homophones/Data/fr_forum_accent_alternates.pl'). 

test_analyse_accents(5) :-
	print_confusion_sets('$ACCEPT/MT/Homophones/Data/fr_forum_alternates.pl',
			     '$ACCEPT/MT/Homophones/Data/fr_forum_alternates.txt'). 
	
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

	load_accent_class_sets(AbsInFile),

	all_words_with_accent_classes(Words),
	length(Words, NWords),
	format('~N~nFound ~d words with accent_classes~n', [NWords]),

	all_confusion_sets_for_words(Words, 0, ConfusionSets),
	list_to_prolog_file(ConfusionSets, AbsOutFile),

	format('~N~nWritten confusion set file (~d sets): ~w~n',
	       [NWords, AbsOutFile]),
	!.

:- dynamic stored_accent_class_for_word/2.
	
load_accent_class_sets(File) :-
	retractall(stored_accent_class_for_word(_, _)),
	safe_prolog_file_to_list_printing_statistics(File, List),
	load_accent_classes_list(List),
	format('~N~nStored accent_classes~n', []),
	!.

load_accent_classes_list([]).
load_accent_classes_list([F | R]) :-
	load_accent_class_item(F),
	!,
	load_accent_classes_list(R).

% accent_classes(['A',v,'E',r,'E'], [avérait,avéré,avérées,avérés]).

load_accent_class_item(accent_class(_Pron, Words)) :-
	load_accent_class_item1(Words, Words),
	!.
load_accent_class_item(Other) :-
	format('*** Error: bad call: ~w~n',
	       [load_accent_class_item(Other)]),
	fail.

load_accent_class_item1([], _Words).
load_accent_class_item1([F | R], Words) :-
	assertz(stored_accent_class_for_word(F, Words)),
	!,
	load_accent_class_item1(R, Words).

all_words_with_accent_classes(Words) :-
	findall(Word,
		stored_accent_class_for_word(Word, _Accent_classes),
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
	findall(AccentClasses,
		stored_accent_class_for_word(Word, AccentClasses),
		AccentClassLists),
	append_list(AccentClassLists, AllAccentClasses0),
	sort(AllAccentClasses0, Alternates).
	
%---------------------------------------------------------------

find_accent_classes_for_accent_list(InFile, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),

	load_accents_file(AbsInFile),
	
	all_accents(AllProns),
	length(AllProns, NAllProns),
	format('~N~nFound ~d distinct accents~n', [NAllProns]),
	
	all_accent_class_lists(AllProns, 0-_NProns, AllHomophoneLists),
	
	list_to_prolog_file(AllHomophoneLists, AbsOutFile),
	length(AllHomophoneLists, NHomophones),

	format('~N~nWritten accent_class file (~d lists): ~w~n',
	       [NHomophones, AbsOutFile]),
	!.

:- dynamic stored_accent/2.

load_accents_file(File) :-
	retractall(stored_accent(_, _)),
	safe_prolog_file_to_list_printing_statistics(File, List),
	load_accents_list(List),
	format('~N~nStored accents~n', []),
	!.

load_accents_list([]).
load_accents_list([F | R]) :-
	load_accent_item(F),
	!,
	load_accents_list(R).

load_accent_item(accent(Word, Pron)) :-
	assertz(stored_accent(Pron, Word)),
	!.
load_accent_item(Other) :-
	format('*** Error: bad call: ~w~n',
	       [load_accent_item(Other)]),
	fail.

all_accents(AllProns) :-
	findall(Pron,
		stored_accent(Pron, _Word),
		Prons),
	sort(Prons, AllProns).

all_accent_class_lists([], N-N, []).
all_accent_class_lists([F | R], In-Out, Output) :-
	(   nontrivial_accent_class_list(F, F1) ->
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
	all_accent_class_lists(R, Next-Out, R1).

nontrivial_accent_class_list(Pron, accent_class(Pron, Words)) :-
	findall(Word,
		stored_accent(Pron, Word),
		Words0),
	sort(Words0, Words),
	length(Words, N),
	N > 1.
	
%---------------------------------------------------------------

get_accents_for_file(InFile, OutFile) :-
	get_accents_for_file(InFile, default_encoding, OutFile).

get_accents_for_file(InFile, Encoding, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),
	
	read_file_to_atom_list(AbsInFile, Encoding, List),
	length(List, NWordsCount),
	format('~N~nLooking up accents (~d words)~n', [NWordsCount]),
	open(AbsOutFile, write, S),
	get_accents_for_list(List, 0-NWords, 0-NProns, S),
	close(S),
	format('~N~nWritten accent file (~d words, ~d accents): ~w~n',
	       [NWords, NProns, AbsOutFile]),
	!.

get_accents_for_list([], NWordsIn-NWordsIn, NPronsIn-NPronsIn, _S).
get_accents_for_list([F | R], NWordsIn-NWordsOut, NPronsIn-NPronsOut, S) :-
	get_and_write_accents_for_item(F, NWordsIn-NWordsNext, NPronsIn-NPronsNext, S),
	!,
	get_accents_for_list(R, NWordsNext-NWordsOut, NPronsNext-NPronsOut, S).

get_and_write_accents_for_item(Word, NWordsIn-NWordsOut, NPronsIn-NPronsOut, S) :-
	get_accents(Word, Accents),
	length(Accents, NAccents),
	NWordsOut is NWordsIn + 1,
	NPronsOut is NPronsIn + NAccents,
	format('.', []),
	(   0 is NWordsOut mod 100 ->
	    
	    format(' (~d) ~n', [NWordsOut]),
	    flush_output(user)
	;
	    
	    true
	),
	write_accents_for_item(Accents, Word, S).

write_accents_for_item([], _Word, _S).
write_accents_for_item([F | R], Word, S) :-
	write_accent_for_item(F, Word, S),
	write_accents_for_item(R, Word, S).

write_accent_for_item(Pron, Word, S) :-
	format(S, '~N~q.~n', [accent(Word, Pron)]),
	!.

%---------------------------------------------------------------

get_accents(Word, Accents) :-
	\+ okay_word_for_pronounce(Word),
	Accents = [].
get_accents(Word, [Deaccented]) :-
	deaccent_word(Word, Deaccented),
	!.

deaccent_word(Word, Deaccented) :-
	atom_codes(Word, Codes),
	deaccent_codes(Codes, Codes1),
	atom_codes(Deaccented, Codes1).

deaccent_codes([], []).
deaccent_codes([F | R], [F1 | R1]) :-
	(   deaccent_code(F, F1) ->
	    true
	;
	    F1 = F
	),
	!,
	deaccent_codes(R, R1).	

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


deaccent_code(0'à, 0'a).
deaccent_code(0'á, 0'a).
deaccent_code(0'â, 0'a).
deaccent_code(0'ä, 0'a).
deaccent_code(0'æ, 0'e).
deaccent_code(0'å, 0'a).
deaccent_code(0'ç, 0'c).
deaccent_code(0'è, 0'e).
deaccent_code(0'é, 0'e).
deaccent_code(0'ê, 0'e).
deaccent_code(0'ë, 0'e).
deaccent_code(0'ì, 0'i).
deaccent_code(0'í, 0'i).
deaccent_code(0'î, 0'i).
deaccent_code(0'ï, 0'i).
deaccent_code(0'ò, 0'o).
deaccent_code(0'ó, 0'o).
deaccent_code(0'ö, 0'o).
deaccent_code(0'ô, 0'o).
deaccent_code(0'ù, 0'u).
deaccent_code(0'ú, 0'u).
deaccent_code(0'û, 0'u).
deaccent_code(0'ü, 0'u).
deaccent_code(0'À, 0'A).
deaccent_code(0'Á, 0'A).
deaccent_code(0'Â, 0'A).
deaccent_code(0'Ä, 0'A).
deaccent_code(0'Æ, 0'E).
deaccent_code(0'Å, 0'A).
deaccent_code(0'Ç, 0'C).
deaccent_code(0'È, 0'E).
deaccent_code(0'É, 0'E).
deaccent_code(0'Ê, 0'E).
deaccent_code(0'Ë, 0'E).
deaccent_code(0'Ì, 0'I).
deaccent_code(0'Í, 0'I).
deaccent_code(0'Î, 0'I).
deaccent_code(0'Ï, 0'I).
deaccent_code(0'Ò, 0'O).
deaccent_code(0'Ó, 0'O).
deaccent_code(0'Ö, 0'O).
deaccent_code(0'Ô, 0'O).
deaccent_code(0'Ù, 0'U).
deaccent_code(0'Ú, 0'U).
deaccent_code(0'Û, 0'U).
deaccent_code(0'Ü, 0'U).

