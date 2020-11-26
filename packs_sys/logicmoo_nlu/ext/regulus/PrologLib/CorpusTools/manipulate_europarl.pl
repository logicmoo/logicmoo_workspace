
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(manipulate_europarl,
	  [extract_ez/2,
	   test_europarl/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

test_europarl(0) :-
	extract_ez('$ACCEPT/MT/Europarl/Generated/europarl_ez_filtered_small.txt',
		   '$ACCEPT/MT/Europarl/Generated/europarl_ez_words_small.txt').

test_europarl(1) :-
	extract_ez('$ACCEPT/MT/Europarl/Generated/europarl_ez_filtered.txt',
		   '$ACCEPT/MT/Europarl/Generated/europarl_ez_words2.txt').

test_europarl('1a') :-
	extract_ez1('$ACCEPT/MT/Europarl/Generated/europarl_ez_filtered.txt',
		    '$ACCEPT/MT/Europarl/Generated/europarl_ez_words2.txt').

%---------------------------------------------------------------

verb_file('$ACCEPT/MT/Europarl/Mmorph/verbs.pl').

%---------------------------------------------------------------

extract_ez1(InFile, OutFile) :-
	load_verbs,
	extract_ez1(InFile, OutFile).
	
extract_ez1(InFile, OutFile) :-
	absolute_file_name(InFile, AbsInFile),
	read_unicode_file_to_atom_list(AbsInFile, InList),
	length(InList, N),
	format('~N--- Read file (~d lines) ~w~n', [N, AbsInFile]),
	extract_ez_words_from_list(InList, Words-[], 0),
	list_to_ordered_multiset(Words, Multiset),
	print_ez_words(Multiset, OutFile),
	!.

extract_ez_words_from_list([], Words-Words, _I).
extract_ez_words_from_list([F | R], In-Out, I) :-
	extract_ez_words_from_item(F, In-Next),
	I1 is I + 1,
	(   0 is I1 mod 1000 ->
	    format('~d ', [I1]),
	    flush_output(user)
	;
	    otherwise ->
	    true
	),
	extract_ez_words_from_list(R, Next-Out, I1).

extract_ez_words_from_item(Atom, In-Out) :-
	atom_codes(Atom, Str),
	replace_punctuation_with_spaces(Str, Str1),
	split_string_into_words(Str1, Words),
	extract_ez_words_from_word_list(Words, In-Out),
	!.
extract_ez_words_from_item(_Atom, In-In).

extract_ez_words_from_word_list([], In-In).
extract_ez_words_from_word_list([F | R], [F1 | Next]-Out) :-
	ez_word(F),
	lowercase_atom(F, F1),
	!,
	extract_ez_words_from_word_list(R, Next-Out).
extract_ez_words_from_word_list([_F | R], In-Out) :-
	!,
	extract_ez_words_from_word_list(R, In-Out).

ez_word(Word) :-
	atom_codes(Word, Str),
	append(_Start, "ez", Str).

print_ez_words(Multiset, File) :-
	safe_absolute_file_name(File, AbsFile),
	length(Multiset, N),
	open(AbsFile, write, S, [encoding('UTF-8')]),
	print_ez_words1(Multiset, S),
	close(S),
	format('~N--- Written -ez file (~d words) to ~w~n', [N, AbsFile]),
	!.
print_ez_words(_Multiset, File) :-
	format('~N*** Error: bad call: ~w~n', [print_ez_words('...', File)]),
	fail.

print_ez_words1([], _S).
print_ez_words1([F | R], S) :-
	print_ez_words_item(F, S),
	!,
	print_ez_words1(R, S).

print_ez_words_item(Freq-Item, S) :-
	number(Freq),
	atomic(Item),
	(   is_second_person_plural_verb(Item) ->
	    format(S, '~N~d~6|~w~n', [Freq, Item])
	;
	    otherwise ->
	    format(S, '~N~d~6|~w (NOT DECLARED AS VERB)~n', [Freq, Item])
	),
	!.
print_ez_words_item(Other, S) :-
	format('~N*** Error: bad call: ~w~n', [print_ez_words_item(Other, S)]),
	fail.

replace_punctuation_with_spaces([], []).
replace_punctuation_with_spaces([F | R], [0'  | R1]) :-
	punctuation_char(F),
	!,
	replace_punctuation_with_spaces(R, R1).
replace_punctuation_with_spaces([F | R], [F | R1]) :-
	!,
	replace_punctuation_with_spaces(R, R1).

punctuation_char(X) :-
	\+ lowercase_char(X),
	\+ uppercase_char(X),
	\+ digit_char(X),
	\+ whitespace_char(X).

%punctuation_char(0'.).
%punctuation_char(0',).
%punctuation_char(0'\').
%punctuation_char(0'\222\).
%punctuation_char(0';).
%punctuation_char(0':).
%punctuation_char(0'?).
%punctuation_char(0'!).
%punctuation_char(0'().
%punctuation_char(0')).
%punctuation_char(0'$).
%punctuation_char(0'%).
%punctuation_char(0'&).
%punctuation_char(0'").

%---------------------------------------------------------------

load_verbs :-
	verb_file(File),
	safe_compile(verbs, File),
	!.

% mmorph_entry([surface=accueilliez,lemma=accueillir,pos='Verb',(mode)=[subjunctive],tense=[present],number=[plural],person=['2'],form=[surface],type=['3']]).

is_second_person_plural_verb(Word) :-
	verbs:mmorph_entry([surface=Word | Feats]),
	member(pos='Verb', Feats),
	member(person=['2'], Feats),
	member(number=[plural], Feats),
	!.
