
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(parse_mmorph,
	  [parse_mmorph/2,
	   test_parse_mmorph/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

test_parse_mmorph(verbs_small) :-
	parse_mmorph('$ACCEPT/MT/Europarl/Mmorph/verbs_small.txt',
		     '$ACCEPT/MT/Europarl/Mmorph/verbs_small.pl').

test_parse_mmorph(verbs_full) :-
	parse_mmorph('$ACCEPT/MT/Europarl/Mmorph/verbs.txt',
		     '$ACCEPT/MT/Europarl/Mmorph/verbs.pl').

test_parse_mmorph(prepositions) :-
	parse_mmorph('$ACCEPT/MT/Europarl/Mmorph/prepositions.txt',
		     '$ACCEPT/MT/Europarl/Mmorph/raw_prepositions.pl').

test_parse_mmorph(nouns) :-
	parse_mmorph('$ACCEPT/MT/Europarl/Mmorph/nouns.txt',
		     '$ACCEPT/MT/Europarl/Mmorph/raw_nouns.pl').

test_parse_mmorph(all) :-
	parse_mmorph('$ACCEPT/MT/Europarl/Mmorph/toto.txt',
		     '$ACCEPT/MT/Europarl/Mmorph/french_mmorph.pl').
	
%---------------------------------------------------------------
	
parse_mmorph(InFile, OutFile) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),
	
	read_file_to_atom_list(AbsInFile, InList),
	length(InList, N),
	format('~N--- Read file (~d lines) ~w~n', [N, AbsInFile]),
	
	parse_mmorph_list(InList, OutList, 0),

	length(OutList, N1),
	list_to_prolog_file(OutList, AbsOutFile),
	format('~N--- Written file (~d lines) ~w~n', [N1, AbsOutFile]),
	!.

parse_mmorph_list([], [], _I).
parse_mmorph_list([F | R], [F1 | R1], I) :-
	parse_mmorph_atom(F, F1),
	I1 is I + 1,
	(   0 is I1 mod 1000 ->
	    format('~d ', [I1]),
	    flush_output(user)
	;
	    otherwise ->
	    true
	),
	!,
	parse_mmorph_list(R, R1, I1).

parse_mmorph_atom(Atom, Analysis) :-
	atom_codes(Atom, Str),
	mmorph_expression(Analysis, Str, []),
	!.
parse_mmorph_atom(Atom, _Analysis) :-
	format('~N*** Error: unable to parse Mmorph line: "~w"~n', [Atom]),
	fail.

%---------------------------------------------------------------

% "accueill\0356tes" = "accueillir" Verb[ mode=indicative tense=past number=plural person=2 form=surface type=3 ]
% "cueillez" = "cueillir" Verb[ mode=indicative|imperative tense=present number=plural person=2 form=surface type=3 ]

mmorph_expression(mmorph_entry([surface=Surface, lemma=Lemma, pos=POS | OtherFeats])) -->
	quoted_alphanum_string(Surface),
	whitespace,
	"=",
	whitespace,
	quoted_alphanum_string(Lemma),
	whitespace,
	alphanum_string(POS),
	"[",
	whitespace,
	feat_val_list(OtherFeats),
	whitespace,
	"]".

feat_val_list([FeatVal | FeatVals]) -->
	feat_val_pair(FeatVal),
	whitespace,
	feat_val_list(FeatVals).
feat_val_list([]) -->
	[].

feat_val_pair(Feat=Val) -->
	alphanum_string(Feat),
	"=",
	val_list(Val).

val_list([F | R]) -->
	alphanum_string(F),
	val_list_rest(R).

val_list_rest(Vals) -->
	"|",
	val_list(Vals).
val_list_rest([]) -->
	[].

quoted_alphanum_string(Atom) -->
	"\"",
	alphanum_string(Atom),
	"\"".

alphanum_string(Atom) -->
	alphanum_char_sequence(Str),
	{atom_codes(Atom, Str)},
	!.

% Special case for ligature o+e
alphanum_char_sequence([0'o, 0'e | R]) -->
	"\\0275",
	!,
	alphanum_char_sequence(R).
% Special case for "&nul;"
alphanum_char_sequence(R) -->
	"&nul;",
	!,
	alphanum_char_sequence(R).
% Special case for "&strong_e;"
alphanum_char_sequence([0'e | R]) -->
	"&strong_e;",
	!,
	alphanum_char_sequence(R).
% Special case for "/"
alphanum_char_sequence([0'/ | R]) -->
	"/",
	!,
	alphanum_char_sequence(R).
alphanum_char_sequence([F | R]) -->
	alphanum_char(F),
	!,
	alphanum_char_sequence(R).
alphanum_char_sequence([]) -->
	[].

alphanum_char(Char) -->
	"\\",
	digit_char(D1),
	digit_char(D2),
	digit_char(D3),
	digit_char(D4),
	{escaped_char([D1, D2, D3, D4], Char)}.
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
	whitespace.
whitespace -->
	[].    

escaped_char(Str, Char) :-
	escaped_char1(Str, Char),
	!.
escaped_char(Str, _Char) :-
	format('~N*** Error: unknown escaped sequence: "~s"~n', [Str]),
	fail.

%escaped_char1("0275", 0'??).  % o+e ligature isn't in 
escaped_char1("0311", 0'É).
escaped_char1("0340", 0'à).
escaped_char1("0342", 0'â).
escaped_char1("0347", 0'ç).
escaped_char1("0350", 0'è).
escaped_char1("0351", 0'é).
escaped_char1("0352", 0'ê).
escaped_char1("0353", 0'ë).
escaped_char1("0356", 0'î).
escaped_char1("0357", 0'ï).
escaped_char1("0364", 0'ô).
escaped_char1("0366", 0'ö).
escaped_char1("0371", 0'ù).
escaped_char1("0373", 0'û).
escaped_char1("0374", 0'ü).
%escaped_char1("", 0').

alphanum_char(0'\') :-
	!.
alphanum_char(0'-) :-
	!.
alphanum_char(0'_) :-
	!.
alphanum_char(Char) :-
	uppercase_char(Char),
	!.
alphanum_char(Char) :-
	lowercase_char(Char),
	!.
alphanum_char(Char) :-
	digit_char(Char),
	!.


	    
	
	


