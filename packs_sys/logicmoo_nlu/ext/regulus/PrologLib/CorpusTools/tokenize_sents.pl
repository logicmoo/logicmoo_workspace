
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(tokenize_sents,
	  [tokenize_sent/2,
	   tokenize_sents_in_file/2,
	   tokenized_sent_to_atom/2,
	   load_multiwords/1,
	   remove_start_and_end_markers/2,
	   test_tokenize_sents/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

test_tokenize_sents(0) :-
	Sent = 'Je suis désolée, Monsieur Hänsch et Monsieur Cox, je n\'avais pas vu que vous demandiez la parole.',
	tokenize_sent(Sent, TokenizedSent),
	format('~NIn  : ~w~n', [Sent]),
	format('~NOut : ~w~n', [TokenizedSent]),
	tokenized_sent_to_atom(TokenizedSent, Sent1),
	format('~NBack: ~w~n', [Sent1]),
	!.

test_tokenize_sents(small) :-
	tokenize_sents_in_file('$ACCEPT/MT/Europarl/Generated/europarl_ez_filtered_small.txt',
			       '$ACCEPT/MT/Europarl/Generated/europarl_ez_tokenized_small.pl').

test_tokenize_sents(full) :-
	tokenize_sents_in_file('$ACCEPT/MT/Europarl/Generated/europarl_ez_filtered.txt',
			       '$ACCEPT/MT/Europarl/Generated/europarl_ez_tokenized.txt').

test_tokenize_sents(forum) :-
	tokenize_sents_in_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/all_files_v2.txt',
			       '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_tokenised.pl').

test_tokenize_sents(tm_fr) :-
	tokenize_sents_in_file('$ACCEPT/MT/GTFeb2012/TM/symc_bip_07_en_fr.fr',
			       '$ACCEPT/MT/GTFeb2012/TM/tm_fr_tokenised.pl').

test_tokenize_sents(all_europarl_fr) :-
	tokenize_sents_in_file('$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en.fr',
			       '$ACCEPT/MT/Europarl/Generated/europarl-v6-fr-tokenized.pl').

test_tokenize_sents(translated_fr_forum) :-
	tokenize_sents_in_file('$ACCEPT/MT/PostEdition/Data/SymantecData/train.fr.translate',
			       '$ACCEPT/MT/PostEdition/Data/SymantecData/translated_fr_tokenized.pl').
	
%---------------------------------------------------------------

load_multiwords(File) :-
	safe_absolute_file_name(File, AbsFile),
	safe_compile(multiwords, AbsFile),
	format('~N--- Loaded multiwords file ~w~n', [AbsFile]),
	!.

multiwords_are_defined :-
	current_predicate(multiwords:multiword/2).

%---------------------------------------------------------------
	
tokenize_sents_in_file(InFile, OutFile) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),

	open(AbsInFile, read, SIn, [encoding('UTF-8')]),
	open(AbsOutFile, write, SOut, [encoding('UTF-8')]),

	tokenize_sents_in_stream(SIn, SOut, 0-NIn, 0-NOut),

	close(SIn),
	close(SOut),

	%list_to_prolog_file_with_encoding(OutList, OutFile, 'UTF-8'),
	format('~N--- Read file (~d lines) ~w~n', [NIn, AbsInFile]),
	format('~N--- Written file (~d lines) ~w~n', [NOut, AbsOutFile]),
	!.

tokenize_sents_in_stream(SIn, SOut, InI-InO, OutI-OutO) :-
	read_line(SIn, Line),
	!,
	tokenize_sents_in_stream1(Line, SIn, SOut, InI-InO, OutI-OutO).

tokenize_sents_in_stream1(Line, _SIn, _SOut, InI-InI, OutI-OutI) :-
	Line = end_of_file,
	!.
tokenize_sents_in_stream1(Line, SIn, SOut, InI-InO, OutI-OutO) :-
	tokenize_and_write_line(Line, SOut, InI-InNext, OutI-OutNext),
	!,
	tokenize_sents_in_stream(SIn, SOut, InNext-InO, OutNext-OutO).

tokenize_and_write_line(Line, SOut, I-I1, O-O1) :-
	tokenize_sent(Line, Tokenized),
	(   member('BAD'(Rest), Tokenized) ->
	    format('~N*** Warning: unable to tokenize "~w"~n', [Rest]),
	    O1 is O
	;
	    otherwise ->
	    format(SOut, '~N~q.~n', [Tokenized]),
	    O1 is O + 1
	),
	I1 is I + 1,
	(   0 is I1 mod 1000 ->
	    format('~d ', [I1]),
	    flush_output(user)
	;
	    otherwise ->
	    true
	),
	!.

%---------------------------------------------------------------
	
tokenize_sent(Str, TokenizedSent) :-
	tokenized_sent(TokenizedSent0, Str, []),
	(   multiwords_are_defined ->
	    collapse_multiwords(TokenizedSent0, TokenizedSent1)
	;
	    otherwise ->
	    TokenizedSent0 = TokenizedSent1
	),
	add_start_and_end_markers(TokenizedSent1, TokenizedSent),
	!.

tokenized_sent_to_atom(TokenizedSent0, Atom) :-
	remove_start_and_end_markers(TokenizedSent0, TokenizedSent),
	tokenized_sent_to_string(TokenizedSent, Str-[]),
	atom_codes(Atom, Str),
	!.
tokenized_sent_to_atom(TokenizedSent, _Atom) :-
	format('~N*** Error: unable to convert "~w" to string~n', [TokenizedSent]),
	fail.

tokenized_sent_to_string([], In-In).
tokenized_sent_to_string([F | R], In-Out) :-
	tokenized_sent_element_to_string(F, F1),
	append(F1, Next, In),
	!,
	tokenized_sent_to_string(R, Next-Out).

tokenized_sent_element_to_string(l(Atom0), Str) :-
	lowercase_atom(Atom0, Atom),
	atom_codes(Atom, Str),
	!.
tokenized_sent_element_to_string(Atom, Str) :-
	atom_codes(Atom, Str),
	!.

%---------------------------------------------------------------

collapse_multiwords([], []).
collapse_multiwords(In, Out) :-
	multiwords:multiword(In-InNext, Out-OutNext),
	!,
	collapse_multiwords(InNext, OutNext).
collapse_multiwords([F | R], [F | R1]) :-
	collapse_multiwords(R, R1).

%---------------------------------------------------------------

add_start_and_end_markers(TokenizedSent0, TokenizedSent) :-
	append(['*start*' | TokenizedSent0], ['*end*'], TokenizedSent),
	!.

remove_start_and_end_markers([], []).
remove_start_and_end_markers(['*start*', ' ' | R], R1) :-
	!,
	remove_start_and_end_markers(R, R1).
remove_start_and_end_markers([F | R], R1) :-
	member(F, ['*start*', '*end*']),
	!,
	remove_start_and_end_markers(R, R1).
remove_start_and_end_markers([F | R], [F | R1]) :-
	!,
	remove_start_and_end_markers(R, R1).

%---------------------------------------------------------------

tokenized_sent([]) -->
	[].
tokenized_sent([F | R]) -->
	tokenized_sent_element(F),
	!,
	tokenized_sent(R).

tokenized_sent_element(Atom) -->
	special_word(Atom),
	!.
tokenized_sent_element(Atom) -->
	alphanum_string(Atom),
	!.
tokenized_sent_element(Atom) -->
	punctuation_mark(Atom),
	!.
tokenized_sent_element(' ') -->
	whitespace,
	!.
% In case something goes wrong.
tokenized_sent_element('BAD'(RestAtom), Rest, []) :-
	atom_codes(RestAtom, Rest),
	!.

alphanum_string(Atom) -->
	alphanum_char_sequence(Str),
	{Str \== []},
	{atom_codes(Atom, Str)},
	!.

punctuation_mark(Atom) -->
	[Char],
	{punctuation_char(Char)},
	{atom_codes(Atom, [Char])}.

alphanum_char_sequence([F | R]) -->
	alphanum_char(F),
	!,
	alphanum_char_sequence(R).
alphanum_char_sequence([]) -->
	[].

alphanum_char(Char) -->
	[Char],
	{alphanum_char(Char)}.

digit_char(D) -->
	[D],
	{digit_char(D)}.

whitespace -->
	[Char],
	{whitespace_char(Char)},
	!,
	possible_empty_whitespace.

possible_empty_whitespace -->
	whitespace,
	!.
possible_empty_whitespace -->
	[].    

alphanum_char(Char) :-
	uppercase_char(Char),
	!.
alphanum_char(Char) :-
	lowercase_char(Char),
	!.
alphanum_char(Char) :-
	digit_char(Char),
	!.

punctuation_char(X) :-
	\+ alphanum_char(X),
	\+ whitespace_char(X).

special_word('m\'') -->
	"m'".
special_word('t\'') -->
	"t'".
special_word('s\'') -->
	"s'".
special_word('l\'') -->
	"l'".
special_word('qu\'') -->
	"qu'".
special_word('j\'') -->
	"j'".
special_word('c\'') -->
	"c'".
special_word('n\'') -->
	"n'".
special_word('M\'') -->
	"M'".
special_word('T\'') -->
	"T'".
special_word('S\'') -->
	"S'".
special_word('L\'') -->
	"L'".
special_word('Qu\'') -->
	"Qu'".
special_word('J\'') -->
	"J'".
special_word('C\'') -->
	"C'".
special_word('N\'') -->
	"N'".

/*
punctuation_char(0'.).
punctuation_char(0',).
punctuation_char(0':).
punctuation_char(0';).
punctuation_char(0',).
punctuation_char(0'-).
punctuation_char(0'$).
punctuation_char(0'!).
punctuation_char(0'().
punctuation_char(0')).
punctuation_char(0'?).
punctuation_char(0'\').
punctuation_char(0'").  %"
punctuation_char(0'%).  
punctuation_char(0'&).
*/		
