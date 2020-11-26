
:- module(regulus_preds,
	  [retract_all_regulus_preds/0,
	   retract_regulus_preds_for_transfer_files/0,
	   retract_regulus_preds_for_lf_pattern_files/0,
	   retract_regulus_preds_for_lf_rewrite_files/0,
	   retract_regulus_preds_for_generic_files/0,
	   retract_regulus_preds_for_collocation_rules/0,
	   
	   save_all_regulus_preds/1,
	   restore_all_regulus_preds/1,

	   save_grammar_macros/0,

           regulus_debug_level/1, regulus_switch/2,
	   ignore_regulus_item/1,
	   macro/2, default_macro/2, grammar_macro/2,
	   feature_value_space/2, feature_value_space0/2, 
	   feature/2, ignore_feature/1, lexical_feature_default/2,
	   feature_instantiation_schedule/1,
	   specialises/3, ignore_specialises/3, feature_value_space_substitution/3,
	   top_level_category/1, category_internal/2,
	   any_substituted_category/2,
	   previously_loaded_include_file/1,
	   category_valued_feature_space/3, subfeature/4,
	   external_grammar/2,
	   collocation_macro_internal/2,

	   vocabulary_item/1,
	   target_vocabulary_item/1,

	   set_current_config_file/1, get_current_config_file/1,
	   unset_current_config_file/0

	   %reachable_cat/2, reachable_word/2, reaches_gap/1,

	   %complete_edge/3, incomplete_edge/5, completed_edge/4,
	   %prediction/2, empty_agenda/5
	   ]
).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').
:- use_module(library(lists)).

%---------------------------------------------------------------------------------

:- dynamic regulus_debug_level/1, regulus_switch/2.

:- dynamic ignore_regulus_item/1.
:- dynamic macro/2, default_macro/2.
:- dynamic collocation_macro_internal/2.
:- dynamic feature_value_space/2, feature_value_space0/2.
:- dynamic ignore_feature/1, feature/2, lexical_feature_default/2.
:- dynamic feature_instantiation_schedule/1.
:- dynamic specialises/3, ignore_specialises/3, feature_value_space_substitution/3.
:- dynamic top_level_category/1, category_internal/2.
:- dynamic any_substituted_category/2.
:- dynamic previously_loaded_include_file/1.
:- dynamic category_valued_feature_space/3, subfeature/4.
:- dynamic external_grammar/2.
:- dynamic vocabulary_item/1.

:- dynamic current_config_file/1.

%---------------------------------------------------------------------------------
 
% ---> is used to define grammar productions in the Stanford notation

:- op(1100, xfx, --->).

% :: connects major category symbols to feature restriction tuples 
% (also Stanford, though we're not using it yet).

:- op(900, xfx, ::).

% We want the optional constituent operator to have lower precedence 
% than colon, which has 550.

:- op(600, fx, '?').


:- dynamic ignore_regulus_item/1.
:- dynamic macro/2, default_macro/2.
:- dynamic feature_value_space/2, feature_value_space0/2.
:- dynamic ignore_feature/1, feature/2, lexical_feature_default/2.
:- dynamic feature_instantiation_schedule/1.
:- dynamic specialises/3, ignore_specialises/3, feature_value_space_substitution/3.
:- dynamic top_level_category/1, category_internal/2.
:- dynamic any_substituted_category/2.
:- dynamic previously_loaded_include_file/1.
:- dynamic category_valued_feature_space/3, subfeature/4.
:- dynamic external_grammar/2.
:- dynamic vocabulary_item/1.

:- dynamic current_config_file/1.

%---------------------------------------------------------------------------------
 
% ---> is used to define grammar productions in the Stanford notation

:- op(1100, xfx, --->).

% :: connects major category symbols to feature restriction tuples 
% (also Stanford, though we're not using it yet).

:- op(900, xfx, ::).

% We want the equals-plus-macro operator to have the same precedence 
% as =, which has 700.

:- op(700, xfx, '=@').

% We want the optional constituent operator to have lower precedence 
% than colon, which has 550, and also the macro invocation, 600.

:- op(620, fx, '?').

% We want the macro invocation operator to have higher precedence 
% than =, which has 700.

:- op(600, fx, '@').

%---------------------------------------------------------------------------------

regulus_pred_pattern(ignore_regulus_item(_)).
regulus_pred_pattern(macro(_,_)).
regulus_pred_pattern(default_macro(_,_)).
regulus_pred_pattern(ignore_feature(_)).
regulus_pred_pattern(feature_value_space(_,_)).
regulus_pred_pattern(feature_value_space0(_,_)).
regulus_pred_pattern(lexical_feature_default(_,_)).
regulus_pred_pattern(specialises(_,_,_)).
regulus_pred_pattern(ignore_specialises(_,_,_)).
regulus_pred_pattern(feature_value_space_substitution(_,_,_)).
regulus_pred_pattern(feature(_,_)).
regulus_pred_pattern(feature_instantiation_schedule(_)).
regulus_pred_pattern(top_level_category(_)).
regulus_pred_pattern(category_internal(_,_)).
regulus_pred_pattern(any_substituted_category(_, _)).
regulus_pred_pattern(previously_loaded_include_file(_)).
regulus_pred_pattern(category_valued_feature_space(_,_,_)).
regulus_pred_pattern(subfeature(_, _, _, _)).
regulus_pred_pattern(external_grammar(_, _)).  
regulus_pred_pattern(vocabulary_item(_)).
regulus_pred_pattern(collocation_macro_internal(_, _)).

%---------------------------------------------------------------------------------

retract_all_regulus_preds :-	
	all_regulus_pred_patterns(Patterns),
	retract_pred_pattern_list(Patterns),
	!.
retract_all_regulus_preds :-
	format2error('~N*** Error: bad call: retract_all_regulus_preds', []),
	fail.

% Reduced set for compiling transfer files - we want to keep some of
% the other predicates intact, e.g. for parsing in DCG mode.
retract_regulus_preds_for_transfer_files :-
	retractall(previously_loaded_include_file(_)),
	retractall(ignore_regulus_item(_)),
	retractall(macro(_,_)).

% Similar for LF patterns and LF rewrite rules, but here just macros.
retract_regulus_preds_for_lf_pattern_files :-
	retractall(previously_loaded_include_file(_)),	
	retractall(macro(_,_)).

retract_regulus_preds_for_lf_rewrite_files :-
	retractall(previously_loaded_include_file(_)),	
	retractall(macro(_,_)).

retract_regulus_preds_for_generic_files :-
	retractall(previously_loaded_include_file(_)),	
	retractall(macro(_,_)).

retract_regulus_preds_for_collocation_rules :-
	retractall(collocation_macro_internal(_, _)).

%---------------------------------------------------------------------------------

% Macros get erased by later internalisations, but we sometimes want the grammar ones

save_grammar_macros :-
	retractall(grammar_macro(_, _)),
	findall([Head, Body],
		macro(Head, Body),
		Pairs),
	save_grammar_macros1(Pairs),
	length(Pairs, N),
	format('~N--- Stored ~d grammar macros~n', [N]),
	!.
save_grammar_macros :-
	format('~N*** Error: bad call: save_grammar_macros~n', []).

save_grammar_macros1([]).
save_grammar_macros1([F | R]) :-
	save_grammar_macro(F),
	!,
	save_grammar_macros1(R).

save_grammar_macro([Head, Body]) :-
	assertz(grammar_macro(Head, Body)).

%---------------------------------------------------------------------------------

save_all_regulus_preds(File) :-
	all_regulus_pred_patterns(Patterns),
	pred_patterns_to_list(Patterns, List),
	list_to_prolog_file(List, File),
	!.
save_all_regulus_preds(File) :-
	format2error('~N*** Error: bad call: ~w', [save_all_regulus_preds(File)]),
	fail.

%---------------------------------------------------------------------------------

restore_all_regulus_preds(File) :-
	retract_all_regulus_preds,
	prolog_file_to_list(File, List),
	assert_items_in_list(List),
	!.
restore_all_regulus_preds(File) :-
	format2error('~N*** Error: bad call: ~w', [restore_all_regulus_preds(File)]),
	fail.

%---------------------------------------------------------------------------------

pred_patterns_to_list([], []) :-
	!.
pred_patterns_to_list([F | R], Out) :-
	findall(F, call(F), FItems),
	pred_patterns_to_list(R, RItems),
	append(FItems, RItems, Out),
	!.

%---------------------------------------------------------------------------------

retract_pred_pattern_list([]) :-
	!.
retract_pred_pattern_list([F | R]) :-
	retractall(F),
	!,
	retract_pred_pattern_list(R).

all_regulus_pred_patterns(Patterns) :-
	findall(Pattern, regulus_pred_pattern(Pattern), Patterns).

%---------------------------------------------------------------------------------

assert_items_in_list([]) :-
	!.
assert_items_in_list([F | R]) :-
	assertz(F),
	!,
	assert_items_in_list(R).

%---------------------------------------------------------------------------------

set_current_config_file(File) :-
	unset_current_config_file,
	assertz(current_config_file(File)).

get_current_config_file(File) :-
	current_config_file(File).

unset_current_config_file :-
	retractall(current_config_file(_)).
