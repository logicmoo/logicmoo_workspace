:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(dynamic_lexicon,
	  [expand_dynamic_lex_entry/2,
	   process_dynamic_lex_entries_in_rules/2,
	   load_intermediate_dynamic_lex_associations_file_if_defined/1,
	   load_dynamic_lex_associations_file_if_defined/1,
	   cached_dynamic_lexicon_entries_file_exists/1,
	   init_dynamic_lex_associations_table/1,
	   zero_cached_dynamic_lex_entries/0,
	   zero_intermediate_dynamic_lex_associations/0,
	   current_grammar_is_dynamic/0,
	   is_dynamic_nuance_rule/1,
	   current_dynamic_lexicon_entries/1,
	   write_dynamic_rule_group_list_item_and_store_associations/3,
	   write_dynamic_lex_associations_table/0
	  ]).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').

%---------------------------------------------------------------

expand_dynamic_lex_entry(_X, _Y) :-
	regulus_error('~NProcessing of dynamic lexicon entries not supported~n', []).

%---------------------------------------------------------------

process_dynamic_lex_entries_in_rules(RulesIn, RulesIn) :-
	\+ term_contains_functor(RulesIn, dynamic_lexicon/2),
	!.
process_dynamic_lex_entries_in_rules(_RulesIn, _RulesOut) :-
	regulus_error('~NProcessing of dynamic lexicon entries not supported~n', []).

%---------------------------------------------------------------

load_intermediate_dynamic_lex_associations_file_if_defined(undefined).

%---------------------------------------------------------------

load_dynamic_lex_associations_file_if_defined(undefined).

%---------------------------------------------------------------

current_dynamic_lexicon_entries(Entries) :-
	Entries = [].
	
%---------------------------------------------------------------

safe_get_intermediate_dynamic_lex_association(_Macro, _LexConstants, _SemConstants) :-
	fail.

%---------------------------------------------------------------

init_dynamic_lex_associations_table(_).

%---------------------------------------------------------------

zero_cached_dynamic_lex_entries.

%---------------------------------------------------------------

zero_intermediate_dynamic_lex_associations.

%---------------------------------------------------------------

current_grammar_is_dynamic :-
	fail.

%---------------------------------------------------------------

is_dynamic_nuance_rule(_) :-
	fail.

%---------------------------------------------------------------

write_dynamic_rule_group_list_item_and_store_associations(_, _, _) :-
	regulus_error('~NProcessing of dynamic lexicon entries not supported~n', []).

%---------------------------------------------------------------

write_dynamic_lex_associations_table.

