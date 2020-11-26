:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(dynamic_lexicon_runtime,
	  [init_dynamic_lexicon_runtime/1,
	   assert_dynamic_lex_entry/1,
	   assert_dynamic_lex_entries/1
	  ]).

'LOAD_DYNAMIC_LEXICON_SUPPORT_IF_AVAILABLE'.

:- use_module('$REGULUS/Prolog/regulus_write_nuance').

:- use_module('$REGULUS/PrologLib/utilities').

'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).

'SICSTUS3/4'( ( :- use_module(library(system)) ),
	      ( :- use_module(library(system3), [shell/1, shell/2] ) ) ).

:- use_module(library(lists)).
:- use_module(library(terms)).

%---------------------------------------------------------------
/*

Typical dynamic lex association record:

dynamic_lex_association(% Macro
			@dimmable_and_switchable_dev_noun([A,B],C),
			% Top-level grammar name
			'',
			% Dynamic cat name
			'DYNAMIC_NOUN_DIMMABLE_PLUR_1',
			% Surface lexicon value
			B,
			% Semantic return value
			value(return,[[device,C]]),
			% Dynamic lex file
			'c:/cygwin/home/speech/regulus/examples/toy1/DynamicNuance/DYNAMIC_NOUN_DIMMABLE_PLUR_1.gsl'
			).
*/

%---------------------------------------------------------------

init_dynamic_lexicon_runtime(DynamicLexAssociationsFile) :-
	safe_absolute_file_name(DynamicLexAssociationsFile, AbsFile),
	safe_compile_with_redefine_and_single_var_warnings_off(user, AbsFile),
	format('~N--- Loaded dynamic lex associations from ~w~n', [AbsFile]),
	create_dynamic_lex_directory_if_necessary,
	init_dynamic_lexicon_database.

%---------------------------------------------------------------

assert_dynamic_lex_entry(MacroCall) :-
	dynamic_lexicon_database_records_for_macro_call(MacroCall, NewRecords),
	assert_dynamic_lex_entry_records(NewRecords),
	!.

%---------------------------------------------------------------

assert_dynamic_lex_entries(MacroCallList) :-
	\+ is_list(MacroCallList),
	!,
	format('~N*** Error: bad call: ~w: arg is not a list~n',
	       [assert_dynamic_lex_entries(MacroCallList)]),
	fail.
assert_dynamic_lex_entries(MacroCallList) :-
	dynamic_lexicon_database_records_for_macro_call_list(MacroCallList, ListOfRecordLists),
	append_list(ListOfRecordLists, NewRecords),
	assert_dynamic_lex_entry_records(NewRecords),
	!.

%---------------------------------------------------------------

dynamic_lexicon_database_records_for_macro_call_list([], []).
dynamic_lexicon_database_records_for_macro_call_list([F | R], [F1 | R1]) :-
	dynamic_lexicon_database_records_for_macro_call(F, F1),
	!,
	dynamic_lexicon_database_records_for_macro_call_list(R, R1).

%---------------------------------------------------------------

dynamic_lexicon_database_records_for_macro_call(MacroCall, NewRecords) :-
	\+ well_formed_lex_macro_call(MacroCall),
	!,
	format('~N*** Error: bad call: ~w: arg is not a well-formed macro call~n',
	       [dynamic_lexicon_database_records_for_macro_call(MacroCall, NewRecords)]),
	fail.
dynamic_lexicon_database_records_for_macro_call(MacroCall, NewRecords) :-
	findall(dynamic_lexicon_database(GrammarTag, DynamicCat, Surface, ReturnVal, File),
		(   user:dynamic_lex_association(MacroCall, GrammarTag, DynamicCat, Surface, ReturnVal, File),
		    \+ dynamic_lexicon_database(GrammarTag, DynamicCat, Surface, ReturnVal, File)
		),
		NewRecords),
	(   NewRecords = [] ->
	    format('~N*** Warning: dynamic lex entry assertion ~w produced no records~n', [MacroCall])
	;
	    otherwise ->
	    true
	).

%---------------------------------------------------------------

create_dynamic_lex_directory_if_necessary :-
	get_current_dynamic_lex_dir(Dir),
	\+ safe_directory_exists(Dir),
	make_directory(Dir),
	format('~N--- Created dynamic lexicon directory ~w~n', [Dir]),
	!.
create_dynamic_lex_directory_if_necessary.

get_current_dynamic_lex_dir(Dir) :-
	user:dynamic_lex_association(_Macro, _GrammarTag, _DynamicCat, _Surface, _ReturnVal, File),
	directory_and_file_for_pathname(File, Dir, _BaseFile),
	!.

%---------------------------------------------------------------

% dynamic_lexicon_database(GrammarTag, DynamicCat, Surface, ReturnVal, File)
:- dynamic dynamic_lexicon_database/5.

init_dynamic_lexicon_database :-
	retractall(dynamic_lexicon_database(_, _, _, _, _)),
	add_dummy_entries_to_dynamic_lexicon_database.

%---------------------------------------------------------------

add_dummy_entries_to_dynamic_lexicon_database :-
	findall(dynamic_lexicon_database(GrammarTag, DynamicCat, Surface, ReturnVal, File),
		(   user:dynamic_lex_association(_Macro, GrammarTag, DynamicCat, Surface, ReturnVal, File),
		    dummy_surface_form(Surface),
		    instantiate_vars_in_return_val_to_dummy_values(ReturnVal)
		),
		DummyRecords),
	assert_dynamic_lex_entry_records(DummyRecords),
	!.
add_dummy_entries_to_dynamic_lexicon_database :-
	format('~N*** Error: bad call: ~w~n',
	       [add_dummy_entries_to_dynamic_lexicon_database]),
	fail.

dummy_surface_form((this, is, not, something, that, should, ever, be, recognised)).

dummy_value(this_is_not_something_that_should_ever_be_recognised).

instantiate_vars_in_return_val_to_dummy_values(T) :-
	term_variables(T, Vars),
	instantiate_vars_in_list_to_dummy_values(Vars).

instantiate_vars_in_list_to_dummy_values([]).
instantiate_vars_in_list_to_dummy_values([F | R]) :-
	instantiate_var_to_dummy_value(F),
	!,
	instantiate_vars_in_list_to_dummy_values(R).

instantiate_var_to_dummy_value(Var) :-
	var(Var),
	!,
	dummy_value(Var).
instantiate_var_to_dummy_value(_).

%---------------------------------------------------------------

well_formed_lex_macro_call(MacroCall) :-
	MacroCall = @Body,
	ground(Body),
	!.

assert_dynamic_lex_entry_records(NewRecords) :-
	NewRecords = [],
	!.
assert_dynamic_lex_entry_records(NewRecords) :-
	changed_files_in_new_records(NewRecords, ChangedFiles),
	update_dynamic_lexicon_database(NewRecords),
	regenerate_changed_dynamic_grammar_files(ChangedFiles).

changed_files_in_new_records(Records, Files) :-
	findall(File,
		member(dynamic_lexicon_database(_GrammarTag, _DynamicCat, _Surface, _ReturnVal, File), Records),
		Files0),
	sort(Files0, Files).

update_dynamic_lexicon_database([]).
update_dynamic_lexicon_database([F | R]) :-
	assertz(F),
	!,
	update_dynamic_lexicon_database(R).

regenerate_changed_dynamic_grammar_files([]).
regenerate_changed_dynamic_grammar_files([F | R]) :-
	regenerate_changed_dynamic_grammar_file(F),
	!,
	regenerate_changed_dynamic_grammar_files(R).
		       
%---------------------------------------------------------------

regenerate_changed_dynamic_grammar_file(File) :-
	dynamic_lexicon_database(GrammarTag, DynamicCat, _Surface, _ReturnVal, File),
	findall([Surface, ReturnVal],
		dynamic_lexicon_database(_GrammarTag, _DynamicCat, Surface, ReturnVal, File),
		SurfaceReturnValPairs),
	length(SurfaceReturnValPairs, N),
	format('~N--- Writing ~d dynamic Nuance entries to ~w~n', [N, File]),
	write_changed_dynamic_grammar_file(File, DynamicCat, GrammarTag, SurfaceReturnValPairs).

write_changed_dynamic_grammar_file(File, DynamicCat, GrammarTag, SurfaceReturnValPairs) :-
	open(File, write, S),
	format(S, '~N~n~w~w~n[~n', [DynamicCat, GrammarTag]),
	write_changed_dynamic_grammar_file_lex_items(SurfaceReturnValPairs, GrammarTag, S),
	format(S, '~N]~n', []),
	close(S).

write_changed_dynamic_grammar_file_lex_items([], _GrammarTag, _S).
write_changed_dynamic_grammar_file_lex_items([F | R], GrammarTag, S) :-
	write_changed_dynamic_grammar_file_lex_item(F, GrammarTag, S),
	!,
	write_changed_dynamic_grammar_file_lex_items(R, GrammarTag, S).

write_changed_dynamic_grammar_file_lex_item([Surface, ReturnValue], GrammarTag, S) :-
	format(S, '~N( ', []),
	regulus_write_nuance:write_rule_body_to_stream(S, GrammarTag, Surface),
	format(S, ' )', []),
	regulus_write_nuance:write_return_value_to_stream(S, ReturnValue).
