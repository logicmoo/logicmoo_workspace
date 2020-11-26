/*

Code now adapted to convert text into either a list of wavfiles or a
list of JPG files. All predicates are parallel.

Files are taken from a specified directory, and it is possible to
define files which correspond to multiple items. If a file of this
type is available, the one corresponding to the longest word sequence
is selected. A word with no corresponding file is rendered as

+ "<Text>"

*/

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(speech_output,
	  [make_wavfile_table/2,
	   wavfile_table_is_defined/0,
	   remove_wavfile_table/0,
	   atom_to_speech_output_form/2,
	   list_missing_wavfiles/5,
	   make_wavfile_edit_script/2,

	   make_jpeg_table/2,
	   jpeg_table_is_defined/0,
	   remove_jpeg_table/0,
	   atom_to_graphical_output_form/2,
	   list_missing_jpegs/5,
	   make_jpeg_edit_script/2,
	   
	   normal_chars_to_file_chars_in_word/2
	  ]).

%----------------------------------------------------------------------

:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).
:- use_module(library(lists)).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').

%----------------------------------------------------------------------

:- dynamic wavfile_or_jpeg/3.

%----------------------------------------------------------------------

make_wavfile_table(Directory, NFiles) :-
	make_wavfile_or_jpeg_table(Directory, NFiles, wav).
	
wavfile_table_is_defined :-
	wavfile_or_jpeg_table_is_defined(wav).
	
remove_wavfile_table :-
	remove_wavfile_or_jpeg_table(wav).

atom_to_speech_output_form(Atom, OutputForm) :-
	atom_to_speech_or_jpeg_output_form(Atom, OutputForm, wav).

list_missing_wavfiles(GenerationGrammarFile, CollocationsFile, Directory, File, NMissingFiles) :-
	list_missing_wavfiles_or_jpegs(GenerationGrammarFile, CollocationsFile, Directory, File, NMissingFiles, wav).

make_wavfile_edit_script(Directory, EditFile) :-
	make_wavfile_or_jpeg_edit_script(Directory, EditFile, wav).

%----------------------------------------------------------------------

make_jpeg_table(Directory, NFiles) :-
	make_wavfile_or_jpeg_table(Directory, NFiles, jpg).
	
jpeg_table_is_defined :-
	wavfile_or_jpeg_table_is_defined(jpg).
	
remove_jpeg_table :-
	remove_wavfile_or_jpeg_table(jpg).

atom_to_graphical_output_form(Atom, OutputForm) :-
	atom_to_speech_or_jpeg_output_form(Atom, OutputForm, jpg).

list_missing_jpegs(GenerationGrammarFile, CollocationsFile, Directory, File, NMissingFiles) :-
	list_missing_wavfiles_or_jpegs(GenerationGrammarFile, CollocationsFile, Directory, File, NMissingFiles, jpg).

make_jpeg_edit_script(Directory, EditFile) :-
	make_wavfile_or_jpeg_edit_script(Directory, EditFile, jpg).

%----------------------------------------------------------------------

make_wavfile_or_jpeg_edit_script(Directory, EditFile, WavOrJPEG) :-
	get_wavfiles_or_jpegs_in_directory(Directory, Files, WavOrJPEG),
	length(Files, N),
	(   N = 0 ->
	    format('~N--- There are no wavfiles/JPEGs in ~w~n', [Directory])
	;
	    otherwise ->
	    print_wavfile_or_jpeg_edit_script(Files, EditFile, WavOrJPEG),
	    format('~N--- Wrote ~w editing script for ~w (~d entries) to ~w~n~n',
		   [WavOrJPEG, Directory, N, EditFile])
	),
	!.
make_wavfile_or_jpeg_edit_script(Directory, EditFile, WavOrJPEG) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [make_wavfile_or_jpeg_edit_script(Directory, EditFile, WavOrJPEG)]),
	fail.

%----------------------------------------------------------------------

make_wavfile_or_jpeg_table(Directory, NFiles, WavOrJPEG) :-
	remove_wavfile_or_jpeg_table(WavOrJPEG),
	get_wavfiles_or_jpegs_in_directory(Directory, Files, WavOrJPEG),
	length(Files, NFiles),
	make_wavfile_or_jpeg_table1(Files, WavOrJPEG),
	!.
make_wavfile_or_jpeg_table(Directory, NFiles, WavOrJPEG) :-
	format2error('~N*** Error: bad call: ~w~n', [make_wavfile_or_jpeg_table(Directory, NFiles, WavOrJPEG)]),
	fail.

remove_wavfile_or_jpeg_table(WavOrJPEG) :-
	retractall(wavfile_or_jpeg(_, _, WavOrJPEG)).

wavfile_or_jpeg_table_is_defined(WavOrJPEG) :-
	wavfile_or_jpeg(_, _, WavOrJPEG),
	!.

get_wavfiles_or_jpegs_in_directory(Directory, Files, WavOrJPEG) :-
	absolute_file_name(Directory, AbsDirectory),
	(   safe_directory_exists(AbsDirectory) ->
	    safe_directory_files(AbsDirectory, AllFiles),
	    get_wavfiles_or_jpegs_in_list(AllFiles, Files, WavOrJPEG)
	;
	    otherwise ->
	    format('~N~n*** Warning: unable to find wavfile/JPEG directory ~w~n', [AbsDirectory]),
	    Files = []
	),
	!.

get_wavfiles_or_jpegs_in_list([], [], _WavfileOrJPEG).
get_wavfiles_or_jpegs_in_list([F | R], [F1 | R1], WavfileOrJPEG) :-
	is_wavfile_or_jpeg(F, F1, WavfileOrJPEG),
	!,
	get_wavfiles_or_jpegs_in_list(R, R1, WavfileOrJPEG).
get_wavfiles_or_jpegs_in_list([_F | R], R1, WavfileOrJPEG) :-
	!,
	get_wavfiles_or_jpegs_in_list(R, R1, WavfileOrJPEG).

is_wavfile_or_jpeg(File, Name, WavfileOrJPEG) :-
	split_atom_into_words(File, 0'., Components),
	Components = [Name, WavfileOrJPEG].

make_wavfile_or_jpeg_table1([], _WavfileOrJPEG).
make_wavfile_or_jpeg_table1([F | R], WavfileOrJPEG) :-
	make_wavfile_or_jpeg_table_line(F, WavfileOrJPEG),
	make_wavfile_or_jpeg_table1(R, WavfileOrJPEG).

make_wavfile_or_jpeg_table_line(File, WavfileOrJPEG) :-
	split_atom_into_words(File, 0'_, Components0),
	file_chars_to_normal_chars_in_word_list(Components0, Components),
	assertz(wavfile_or_jpeg(Components, File, WavfileOrJPEG)).

%----------------------------------------------------------------------

file_chars_to_normal_chars_in_word_list([], []) :-
	!.
file_chars_to_normal_chars_in_word_list([F | R], [F1 | R1]) :-
	file_chars_to_normal_chars_in_word(F, F1),
	file_chars_to_normal_chars_in_word_list(R, R1),
	!.
file_chars_to_normal_chars_in_word_list(Components0, Components) :-
	format2error('~N*** Error: bad call: ~w~n', [file_chars_to_normal_chars_in_word_list(Components0, Components)]),
	fail.

%----------------------------------------------------------------------

atom_to_speech_or_jpeg_output_form(Atom0, OutputForm, WavfileOrJPEG) :-
	wavfile_or_jpeg_table_is_defined(WavfileOrJPEG),
	lowercase_atom(Atom0, Atom),
	(   atom_to_wavfile_or_jpeg_list(Atom, Wavfiles, WavfileOrJPEG) ->
	    join_with_spaces(Wavfiles, OutputForm)
	;
	    format_to_atom('+text "~w"', [Atom], OutputForm)
	),
	!.
atom_to_speech_or_jpeg_output_form(Atom, OutputForm, WavfileOrJPEG) :-
	\+ wavfile_or_jpeg_table_is_defined(WavfileOrJPEG),
	format_to_atom('+tts "~w"', [Atom], OutputForm),
	!.
atom_to_speech_or_jpeg_output_form(Atom, OutputForm, WavfileOrJPEG) :-
	format2error('~N*** Error: bad call: ~q~n', [atom_to_speech_output_form(Atom, OutputForm, WavfileOrJPEG)]),
	fail.

atom_to_wavfile_or_jpeg_list(TargetAtom, Files, WavfileOrJPEG) :-
	split_atom_into_words(TargetAtom, Words),
	word_list_to_file_list(Words, Files, WavfileOrJPEG, ok-FinalStatus),
	FinalStatus = ok.

word_list_to_file_list([], [], _WavfileOrJPEG, Status-Status).
word_list_to_file_list(Words, Files, WavfileOrJPEG, StatusIn-StatusOut) :-
	(   find_best_wavfile_or_jpeg(Words, Wavfile, RestWords, WavfileOrJPEG) ->
	    Files = [Wavfile | RestFiles],
	    StatusNext = StatusIn
	;
	    WavfileOrJPEG = jpg ->
	    Words = [FirstWord | RestWords],
	    Files = [dummy | RestFiles],
	    StatusNext = StatusIn
	    %format2error('~N*** Warning: no JPEG for ~w~n', [FirstWord])
	;
	    otherwise ->
	    Words = [FirstWord | RestWords],
	    Files = RestFiles,
	    StatusNext = not_ok,
	    format2error('~N*** Error: no wavfile for ~w~n', [FirstWord])
	),
	!,
	word_list_to_file_list(RestWords, RestFiles, WavfileOrJPEG, StatusNext-StatusOut).

find_best_wavfile_or_jpeg(In, BestWavfile, BestRest, WavfileOrJPEG) :-
	findall(Length-[Wavfile, Rest],
		possible_wavfile_or_jpeg(In, Wavfile, Rest, Length, WavfileOrJPEG),
		Pairs),
	keysort(Pairs, Pairs1),
	reverse(Pairs1, [_BestLength-[BestWavfile, BestRest] | _]),
	!.

possible_wavfile_or_jpeg(In, Wavfile, Rest, Length, WavfileOrJPEG) :-
	In = [F | _],
	Prefix = [F | _],
	wavfile_or_jpeg(Prefix, Wavfile, WavfileOrJPEG),
	length(Prefix, Length),
	append(Prefix, Rest, In).	

%----------------------------------------------------------------------

list_missing_wavfiles_or_jpegs(GenerationGrammarFile, CollocationsFile, Directory, File, NMissingFiles, WavfileOrJPEG) :-
	(   wavfile_or_jpeg_table_is_defined(WavfileOrJPEG) ->
	    true
	;
	    make_wavfile_or_jpeg_table(Directory, _NFiles, WavfileOrJPEG)
	),
	find_missing_wavfiles_or_jpegs(GenerationGrammarFile, CollocationsFile, List, WavfileOrJPEG),
	sort(List, List1),
	length(List1, NMissingFiles),
	(   NMissingFiles > 0 ->
	    print_wavfile_or_jpeg_edit_script(List1, File, WavfileOrJPEG)
	;
	    true
	).

find_missing_wavfiles_or_jpegs(GenerationGrammarFile, CollocationsFile, List, WavfileOrJPEG) :-
	findall(W1,
		(   missing_wavfile_or_jpeg(GenerationGrammarFile, CollocationsFile, W, WavfileOrJPEG),
		    normal_chars_to_file_chars_in_word(W, W1)
		),
		List).

missing_wavfile_or_jpeg(GenerationGrammarFile, _CollocationsFile, W, WavfileOrJPEG) :-
	safe_file_exists(GenerationGrammarFile),
	prolog_file_to_list(GenerationGrammarFile, GenerationGrammarList),
	member(IncludeDecl, GenerationGrammarList),
	(   IncludeDecl = ( :- use_module(IncludedFile) )
	;
	    IncludeDecl = ( :- ensure_loaded(IncludedFile) )
	),
	atom(IncludedFile),
	missing_wavfile_or_jpeg(IncludedFile, null, W, WavfileOrJPEG).
missing_wavfile_or_jpeg(GenerationGrammarFile, _CollocationsFile, W, WavfileOrJPEG) :-
	safe_file_exists(GenerationGrammarFile),
	prolog_file_to_list(GenerationGrammarFile, GenerationGrammarList),
	member(Rule, GenerationGrammarList),
	dcg_rule(Rule),
	atom_in_rhs_of_dcg_rule(RHSAtom, Rule),
	split_atom_into_words(RHSAtom, Components),
	join_with_underscore(Components, W),
	\+ wavfile_or_jpeg_exists(W, WavfileOrJPEG).
missing_wavfile_or_jpeg(_GenerationGrammarFile, CollocationsFile, W, WavfileOrJPEG) :-
	CollocationsFile \== null,
	safe_file_exists(CollocationsFile),
	prolog_file_to_list(CollocationsFile, CollocationsFileList),
	member(better_collocation(_String, BetterString), CollocationsFileList),
	atom_codes(BetterStringAtom, BetterString),
	split_atom_into_words(BetterStringAtom, Components),
	member(W, Components),
	\+ wavfile_or_jpeg_exists(W, WavfileOrJPEG).

wavfile_or_jpeg_exists(W, WavfileOrJPEG) :-
	atom(W),
	normal_chars_to_file_chars_in_word(W, W1),
	lowercase_atom(W1, W2),
	wavfile_or_jpeg(_, W2, WavfileOrJPEG),
	!.

%----------------------------------------------------------------------

add_wavfile_or_jpeg_extensions_in_list([], [], _WavfileOrJPEG).
add_wavfile_or_jpeg_extensions_in_list([F | R], [F1 | R1], WavfileOrJPEG) :-
	add_wavfile_or_jpeg_extension(F, F1, WavfileOrJPEG),
	!,
	add_wavfile_or_jpeg_extensions_in_list(R, R1, WavfileOrJPEG).

add_wavfile_or_jpeg_extension(File, File1, WavfileOrJPEG) :-
	atom(File),
	append_atoms([File, WavfileOrJPEG], 0'., File1),
	!.
add_wavfile_or_jpeg_extension(File, File1, WavfileOrJPEG) :-
	format2error('~N*** Error: bad call: ~w~n', [add_wavfile_or_jpeg_extension(File, File1, WavfileOrJPEG)]),
	fail.

%----------------------------------------------------------------------

print_wavfile_or_jpeg_edit_script(List, File, WavfileOrJPEG) :-
	open(File, write, S),
	print_wavfile_or_jpeg_edit_script1(S, List, WavfileOrJPEG),
	close(S).

print_wavfile_or_jpeg_edit_script1(_S, [], _WavfileOrJPEG).
print_wavfile_or_jpeg_edit_script1(S, [F | R], WavfileOrJPEG) :-
	print_wavfile_or_jpeg_edit_script_item(S, F, WavfileOrJPEG),
	!,
	print_wavfile_or_jpeg_edit_script1(S, R, WavfileOrJPEG).

print_wavfile_or_jpeg_edit_script_item(S, Atom, wav) :-
	atom(Atom),
	atom_codes(Atom, Codes),
	length(Codes, NChars),
	(   NChars > 18 ->
	    format(S, '~Nxwavedit ~w.wav ep.EndSeconds=2~n', [Atom])
	;
	    otherwise ->
	    format(S, '~Nxwavedit ~w.wav~n', [Atom])
	),
	!.
print_wavfile_or_jpeg_edit_script_item(S, Atom, jpg) :-
	atom(Atom),
	current_predicate(regulus_preds:target_vocabulary_item/1),
	\+ regulus_preds:target_vocabulary_item(Atom),
	format(S, '~Njpgedit ~w.jpg (Warning: not in target vocabulary in stored corpus test)~n', [Atom]),
	!.
print_wavfile_or_jpeg_edit_script_item(S, Atom, jpg) :-
	atom(Atom),
	format(S, '~Njpgedit ~w.jpg~n', [Atom]),
	!.
print_wavfile_or_jpeg_edit_script_item(S, Atom, WavfileOrJPEG) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [print_wavfile_or_jpeg_edit_script_item(S, Atom, WavfileOrJPEG)]),
	fail.

%----------------------------------------------------------------------

dcg_rule((_LHS --> _RHS)).

atom_in_rhs_of_dcg_rule(RHSAtom, (_LHS --> RHS)) :-
	atom_in_rhs(RHSAtom, RHS).

atom_in_rhs(RHSAtom, List) :-
	nonvar(List),
	is_list(List),
	member(RHSAtom, List),
	atom(RHSAtom).
atom_in_rhs(RHSAtom, (L, R)) :-
	(   atom_in_rhs(RHSAtom, L) ;
	    atom_in_rhs(RHSAtom, R)
	).
atom_in_rhs(RHSAtom, (L ; R)) :-
	(   atom_in_rhs(RHSAtom, L) ;
	    atom_in_rhs(RHSAtom, R)
	).

%----------------------------------------------------------------------

file_chars_to_normal_chars_in_word(Word, Word1) :-
	atom(Word),
	atom_codes(Word, Chars),
	file_chars_to_normal_chars(Chars, Chars1),
	atom_codes(Word1, Chars1),
	!.
file_chars_to_normal_chars_in_word(Word, Word1) :-
	format2error('~N*** Error: bad call: ~w~n', [file_chars_to_normal_chars_in_word(Word, Word1)]),
	fail.

normal_chars_to_file_chars_in_word(Word, Word1) :-
	atom(Word),
	atom_codes(Word, Chars),
	file_chars_to_normal_chars(Chars1, Chars),
	atom_codes(Word1, Chars1),
	!.
normal_chars_to_file_chars_in_word(Word, Word1) :-
	format2error('~N*** Error: bad call: ~w~n', [normal_chars_to_file_chars_in_word(Word, Word1)]),
	fail.

%----------------------------------------------------------------------

file_chars_to_normal_chars([], []).
file_chars_to_normal_chars([F01, F02 | R], [F1 | R1]) :-
	file_chars_to_single_char([F01, F02], F1),
	!,
	file_chars_to_normal_chars(R, R1).
file_chars_to_normal_chars([F | R], [F | R1]) :-
	!,
	file_chars_to_normal_chars(R, R1).

%----------------------------------------------------------------------

file_chars_to_single_char("a1", 0'á).
file_chars_to_single_char("a2", 0'â).
file_chars_to_single_char("a3", 0'à).
file_chars_to_single_char("a4", 0'ä).
file_chars_to_single_char("a5", 0'å).

file_chars_to_single_char("c1", 0'ç).

file_chars_to_single_char("e1", 0'é).
file_chars_to_single_char("e2", 0'ê).
file_chars_to_single_char("e3", 0'è).
file_chars_to_single_char("e4", 0'ë).
file_chars_to_single_char("e6", 0'æ).

file_chars_to_single_char("i1", 0'í).
file_chars_to_single_char("i2", 0'î).
file_chars_to_single_char("i3", 0'ì).
file_chars_to_single_char("i4", 0'ï).

file_chars_to_single_char("n1", 0'ñ).

file_chars_to_single_char("o1", 0'ó).
file_chars_to_single_char("o2", 0'ô).
file_chars_to_single_char("o3", 0'ò).
file_chars_to_single_char("o4", 0'ö).

file_chars_to_single_char("u1", 0'ú).
file_chars_to_single_char("u2", 0'û).
file_chars_to_single_char("u3", 0'ù).
file_chars_to_single_char("u4", 0'ü).

%file_chars_to_single_char("-D", 0'.).
%file_chars_to_single_char("-P", 0'+).
%file_chars_to_single_char("-A", 0'@).
%file_chars_to_single_char("-d", 0'.).
%file_chars_to_single_char("-p", 0'+).
%file_chars_to_single_char("-a", 0'@).

