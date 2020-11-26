
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(translate_generation_orthography_to_recognition_form,
	[translate_sent_file_from_generation_orthography_to_recognition_form/3]
    ).

%======================================================================

:- use_module('$REGULUS/Prolog/generate').

:- use_module('$REGULUS/Prolog/regulus_read').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%======================================================================

translate_sent_file_from_generation_orthography_to_recognition_form(ConfigFile, InFile, OutFile) :-
	init_translate_sent_file_from_generation_orthography_to_recognition_form(ConfigFile),
	%safe_prolog_file_to_list_printing_statistics(InFile, InList),
	read_corpus_file_printing_statistics(InFile, InList),
	translate_sent_items_from_generation_orthography_to_recognition_form(InList, OutList),
	length(OutList, OutN),

	safe_absolute_file_name(OutFile, AbsOutFile),
	list_to_prolog_file(OutList, AbsOutFile),
	format('~N--- Printed recognition orthography version of file (~d entries) ~w~n', [OutN, AbsOutFile]),
	!.
translate_sent_file_from_generation_orthography_to_recognition_form(ConfigFile, InFile, OutFile) :-
	format('~N*** Error: bad call: ~w~n',
	       [translate_sent_file_from_generation_orthography_to_recognition_form(ConfigFile, InFile, OutFile)]),
	fail.

init_translate_sent_file_from_generation_orthography_to_recognition_form(ConfigFile) :-
	check_config_file_has_recognition_orthography_definition(ConfigFile),
	user:regulus_batch(ConfigFile,
			   ["LOAD_TRANSLATE"]).

check_config_file_has_recognition_orthography_definition(ConfigFile) :-
	read_generic_regulus_related_file_or_files(ConfigFile, RulesWithWrappers),
	strip_wrappers_from_rules(RulesWithWrappers, Rules),
	member(regulus_config(recognition_orthography_rules, File), Rules),
	format('~N~n--- CONFIG FILE CONTAINS A RECOGNITION_ORTHOGRAPHY_RULES ENTRY. RULES FILE IS ~w~n~n', [File]),
	!.
check_config_file_has_recognition_orthography_definition(ConfigFile) :-
	format('~N*** Error: config file ~w does not contain an entry of form "regulus_config(recognition_orthography_rules, _)"~n', [ConfigFile]),
	fail.

translate_sent_items_from_generation_orthography_to_recognition_form([], []).
translate_sent_items_from_generation_orthography_to_recognition_form([F | R], [F1 | R1]) :-
	translate_sent_item_from_generation_orthography_to_recognition_form(F, F1),
	!,
	translate_sent_items_from_generation_orthography_to_recognition_form(R, R1).
translate_sent_items_from_generation_orthography_to_recognition_form([_F | R], R1) :-
	!,
	translate_sent_items_from_generation_orthography_to_recognition_form(R, R1).

translate_sent_item_from_generation_orthography_to_recognition_form(sent(In), sent(Out)) :-
	convert_atom_from_generation_orthography_to_recognition_form(In, Out),
	!.
translate_sent_item_from_generation_orthography_to_recognition_form(sent(In, X), sent(Out, X)) :-
	convert_atom_from_generation_orthography_to_recognition_form(In, Out),
	!.
translate_sent_item_from_generation_orthography_to_recognition_form(sent(In, X, Y), sent(Out, X, Y)) :-
	convert_atom_from_generation_orthography_to_recognition_form(In, Out),
	!.
translate_sent_item_from_generation_orthography_to_recognition_form(F, _F1) :-
	format('~N*** Warning: bad item ~w~n', [F]),
	fail.

convert_atom_from_generation_orthography_to_recognition_form(In, Out) :-	
	atom(In),
	split_atom_into_words(In, WordsIn),
	fix_recognition_orthography(WordsIn, WordsOut),
	join_with_spaces(WordsOut, Out),
	!.
convert_atom_from_generation_orthography_to_recognition_form(In, _Out) :-
	format('~N*** Warning: unable to convert item "~w" to recognition form~n', [In]),
	fail.