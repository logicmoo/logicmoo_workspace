
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(regulus_write_nuance,
	  [write_top_level_multiple_grammar_nuance_files/2,
	   write_rules_and_feats_to_nuance_files/4
	   ]
      ).

'LOAD_DYNAMIC_LEXICON_SUPPORT_IF_AVAILABLE'.

:- use_module('$REGULUS/Prolog/regulus_declarations').
:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(ordsets)).
:- use_module(library(assoc)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).

%---------------------------------------------------------------

write_rules_and_feats_to_nuance_files(GroupedRules, SemFeats, GrammarTag0, NuanceFile) :-
	%list_to_prolog_file(GroupedRules, 'C:/Temp/grouped_rules.pl'),
	(   GrammarTag0 = default ->
	    GrammarTag = '' ;
	    format_to_atom('__~w', [GrammarTag0], GrammarTag)
	),
	format('~N -- Writing out rules... ', []), flush_output(user),
	timed_call(write_rules_and_feats_to_nuance_files1(GroupedRules, SemFeats, GrammarTag, NuanceFile),
		   TimeTaken),
	format('~1f secs~n~n', [TimeTaken]).

%---------------------------------------------------------------

write_top_level_multiple_grammar_nuance_files(ComponentFiles, NuanceFile) :-
	nuance_rule_and_slot_definition_files_for_component_files(ComponentFiles, RuleFiles, SlotDefinitionFiles),
	get_all_slots_from_slot_definition_files(SlotDefinitionFiles, AllSlots),
	nuance_slot_definition_file(NuanceFile, NuanceSlotDefinitionFile),
	nuance_rule_file(NuanceFile, NuanceRuleFile),
	write_sem_feats_to_nuance_file(AllSlots, NuanceSlotDefinitionFile),
	write_top_level_nuance_rules_file_for_component_files(RuleFiles, NuanceRuleFile).

%---------------------------------------------------------------

write_rules_and_feats_to_nuance_files1(GroupedRules, SemFeats, GrammarTag, NuanceFile) :-
	nuance_slot_definition_file(NuanceFile, NuanceSlotDefinitionFile),
	nuance_rule_file(NuanceFile, NuanceRuleFile),
	write_sem_feats_to_nuance_file(SemFeats, NuanceSlotDefinitionFile),
	write_rules_to_nuance_file(GroupedRules, GrammarTag, NuanceRuleFile).

nuance_slot_definition_file(NuanceFile, NuanceSlotDefinitionFile) :-
	atomic(NuanceFile),
	!,
	append_atoms([NuanceFile, slot_definitions], 0'., NuanceSlotDefinitionFile).
nuance_slot_definition_file(NuanceFile, NuanceSlotDefinitionFile) :-
	NuanceFile =.. [FileSearchPath, NuanceFile1],
	!,
	nuance_slot_definition_file(NuanceFile1, NuanceSlotDefinitionFile1),
	NuanceSlotDefinitionFile =.. [FileSearchPath, NuanceSlotDefinitionFile1].
nuance_slot_definition_file(NuanceFile, _NuanceSlotDefinitionFile) :-
	regulus_error('~NUnable to interpret ~w as name of Nuance output file~n', NuanceFile).

nuance_rule_file(NuanceFile, NuanceRuleFile) :-
	atomic(NuanceFile),
	!,
	append_atoms([NuanceFile, grammar], 0'., NuanceRuleFile).
nuance_rule_file(NuanceFile, NuanceRuleFile) :-
	NuanceFile =.. [FileSearchPath, NuanceFile1],
	!,
	nuance_rule_file(NuanceFile1, NuanceRuleFile1),
	NuanceRuleFile =.. [FileSearchPath, NuanceRuleFile1].
nuance_rule_file(NuanceFile, _NuanceRuleFile) :-
	regulus_error('~NUnable to interpret ~w as name of Nuance output file~n', NuanceFile).

write_sem_feats_to_nuance_file(SemFeats, File) :-
	(   absolute_file_name(File, AbsFile) ->
	    true ;
	    regulus_error('~NUnable to interpret ~w as name of Nuance output file~n', File)
	),
	(   open(AbsFile, write, S) ->
	    true ;
	    regulus_error('~NUnable to open Nuance output file ~w~n', AbsFile)
	),

	write_sem_feats_to_nuance_stream(S, SemFeats),

	close(S),
	format('~N   -- Nuance slot names written to file ~w~n', [AbsFile]).

write_sem_feats_to_nuance_stream(_S, []).
write_sem_feats_to_nuance_stream(S, [F | R]) :-
	format(S, '~N~w~n', [F]),
	!,
	write_sem_feats_to_nuance_stream(S, R).
 
write_rules_to_nuance_file(Rules, GrammarTag, File) :-
	(   absolute_file_name(File, AbsFile) ->
	    true
	;
	    regulus_error('~NUnable to interpret ~w as name of Nuance output file~n', File)
	),
	(   open(AbsFile, write, S) ->
	    true
	;
	    regulus_error('~NUnable to open Nuance output file ~w~n', AbsFile)
	),

	write_nuance_rules_intro_to_stream(S),
	write_rules_to_nuance_stream(S, GrammarTag, Rules),

	close(S),
	format('~N   -- Nuance rules written to file ~w~n', [AbsFile]).
 
write_nuance_rules_intro_to_stream(S) :-
	write_external_grammars_to_stream(S),
	!.

write_external_grammars_to_stream(S) :-
	findall([GrammarName, Body], external_grammar(GrammarName, Body), Pairs),
	write_external_grammars_to_stream1(Pairs, S),
	!.
write_external_grammars_to_stream(_S) :-
	regulus_error('~Nwrite_external_grammars_to_stream failed~w~n', []).
 
write_external_grammars_to_stream1([], _S).
write_external_grammars_to_stream1([[GrammarName, Body] | R], S) :-
	%format('~N~n-- Including external grammar ~w~n', [GrammarName]),
	format(S, '~N~w ~w~n', [GrammarName, Body]),
	write_external_grammars_to_stream1(R, S).
 
write_rules_to_nuance_stream(_S, _GrammarTag, []).
write_rules_to_nuance_stream(S, GrammarTag, [F | R]) :-
	write_rule_group_to_nuance_stream(S, GrammarTag, F),
	!,
	write_rules_to_nuance_stream(S, GrammarTag, R).

write_rule_group_to_nuance_stream(S, GrammarTag, RuleGroup) :-
	RuleGroup = HeadCat-RuleList,
	is_list(RuleList),
	write_rule_group_head_to_nuance_stream(S, HeadCat, GrammarTag),
	write_rule_group_list_to_nuance_stream(S, HeadCat, GrammarTag, RuleList),
	format(S, '~N]~n', []).

write_rule_group_head_to_nuance_stream(S, HeadCat, GrammarTag) :-
	current_grammar_is_dynamic,
	is_top_level_nuance_grammar_atom(HeadCat, HeadCatWithoutInitialPeriod),
	format('~N  -- Dynamic grammar, so changed top-level grammar ~w~w to ~w~w:public~n',
	       [HeadCat, GrammarTag, HeadCatWithoutInitialPeriod, GrammarTag]),
	format(S, '~N~n~w~w:public~n[~n', [HeadCatWithoutInitialPeriod, GrammarTag]),
	!.
write_rule_group_head_to_nuance_stream(S, HeadCat, GrammarTag) :-
	format(S, '~N~n~w~w~n[~n', [HeadCat, GrammarTag]).	

is_top_level_nuance_grammar_atom(Atom, Atom1) :-
	atom_codes(Atom, String),
	String = [0'. | Rest],
	atom_codes(Atom1, Rest),
	!.

write_rule_group_list_to_nuance_stream(_S, _HeadCat, _GrammarTag, []).
write_rule_group_list_to_nuance_stream(S, HeadCat, GrammarTag, [F | R]) :-
	write_rule_group_list_item_to_nuance_stream(S, HeadCat, GrammarTag, F),
	!,
	write_rule_group_list_to_nuance_stream(S, HeadCat, GrammarTag, R).

write_rule_group_list_item_to_nuance_stream(S, HeadCat, GrammarTag, rule(Rule, LineInfo)) :-
	on_exception(
	Exception, 
	write_rule_group_list_item_to_nuance_stream1(S, HeadCat, GrammarTag, Rule), 
	inform_about_regulus_exception(Exception, LineInfo)
    ).
 
write_rule_group_list_item_to_nuance_stream1(S, HeadCat, GrammarTag, Rule) :-
	is_dynamic_nuance_rule(Rule),
	!,
	write_dynamic_rule_group_list_item_and_store_associations(S, HeadCat, GrammarTag, Rule).
write_rule_group_list_item_to_nuance_stream1(S, _HeadCat, GrammarTag, Rule) :-
	Rule = ( cat(_CatName, _SynFeatsWithVals, ReturnValue) --> Body ), 
	%make_ground(Rule),
	format(S, '( ', []),
	write_rule_body_to_stream(S, GrammarTag, Body),
	format(S, ' )', []),
	write_return_value_to_stream(S, ReturnValue),
	format(S, '~n', []).
 
write_rule_body_to_stream(S, GrammarTag, (P, Q)) :-
	!,
	write_rule_body_to_stream(S, GrammarTag, P),
	format(S, ' ', []),
	write_rule_body_to_stream(S, GrammarTag, Q).
write_rule_body_to_stream(S, GrammarTag, ?P) :-
	!,
	format(S, '?(', []),
	write_rule_body_to_stream(S, GrammarTag, P),
	format(S, ')', []).
write_rule_body_to_stream(S, GrammarTag, cat(CatName, _SynFeatsWithVals, Value)) :-
	!,
	format(S, '~w~w', [CatName, GrammarTag]),
	write_cat_value_to_stream(S, Value).
write_rule_body_to_stream(S, _GrammarTag, Atom) :-
	atomic(Atom),
	!,
	format(S, '~w', [Atom]).

% If we don't have a return value, no need to write anything.
write_return_value_to_stream(_S, no_value) :-
	!.
% If return value is an empty list, no need to write anything.
write_return_value_to_stream(_S, value(_ValueType, [])) :-
	!.
% If ValueType = global, realise as a global slot-filling structure
write_return_value_to_stream(S, value(global, Value)) :-
	!,
	format(S, '~n     { ', []),
	write_semantic_expression_list_to_stream(S, Value, head),
	format(S, ' }', []).
% If ValueType = return, realise as a return structure
write_return_value_to_stream(S, value(return, Value)) :-
	format(S, '~n     {return( ', []),
	write_semantic_expression_to_stream(S, Value, head),
	format(S, ' )}', []).
 
write_cat_value_to_stream(_S, no_value) :-
	!.
write_cat_value_to_stream(_S, value(_ValueType, [])) :-
	!.
write_cat_value_to_stream(S, value(_ValueType, Value)) :-
	format(S, ':', []),
	write_semantic_expression_to_stream(S, Value, nonhead).

%---------------------------------------------------------------

% [] denotes empty list rather than empty feat-val list -
% it turns out that the Nuance compiler doesn't like empty
% feat-val lists.
write_semantic_expression_to_stream(S, [], _HeadNonHead) :-
	\+ using_strcat_semantics,
	!,
	format(S, '()', []).
write_semantic_expression_to_stream(S, [], _HeadNonHead) :-
	using_strcat_semantics,
	!,
	format(S, '"[]"', []).
write_semantic_expression_to_stream(S, (Feat = Val), HeadNonHead) :-
	!,
	format(S, '< ', []),
	%write_semantic_expression_to_stream(S, Feat, HeadNonHead),
	format(S, '~w', [Feat]),
	format(S, ' ', []),
	write_semantic_expression_to_stream(S, Val, HeadNonHead),
	format(S, ' >', []).
write_semantic_expression_to_stream(S, FeatValList, HeadNonHead) :-
	is_feat_val_list(FeatValList),
	!,
	format(S, '[', []),
	write_semantic_expression_list_to_stream(S, FeatValList, HeadNonHead),
	format(S, ' ]', []).
write_semantic_expression_to_stream(S, NonFeatValList, HeadNonHead) :-
	is_non_feat_val_list(NonFeatValList),
	!,
	format(S, '(', []),
	write_semantic_expression_list_to_stream(S, NonFeatValList, HeadNonHead),
	format(S, ' )', []).
write_semantic_expression_to_stream(S, FuncExpr, HeadNonHead) :-
	functor(FuncExpr, F, N),
	gsl_function(F/N),
	FuncExpr =.. [F | Args],
	!,
	format(S, '~w(', [F]),
	write_semantic_expression_list_to_stream(S, Args, HeadNonHead),
	format(S, ' )', []).
write_semantic_expression_to_stream(S, Expr+Component, HeadNonHead) :-
	is_feat_val_list(Expr),
	memberchk(Component=Value,Expr),
	!,
	write_semantic_expression_to_stream(S, Value, HeadNonHead). 
write_semantic_expression_to_stream(S, Expr+Component, HeadNonHead) :-
	!,
	write_semantic_expression_to_stream(S, Expr, HeadNonHead),
	format(S, '.', []),
	%write_semantic_expression_to_stream(S, Component, HeadNonHead).
	format(S, '~w', [Component]).
write_semantic_expression_to_stream(S, '$VAR'(N), nonhead) :-
	number(N),
	!,
	format(S, 'v_~d', [N]).
write_semantic_expression_to_stream(S, '$VAR'(N), head) :-
	number(N),
	!,
	format(S, '$v_~d', [N]).
write_semantic_expression_to_stream(S, Atom, _HeadNonHead) :-
	\+ using_strcat_semantics,
	atomic(Atom),
	!,
	format(S, '~w', [Atom]).
write_semantic_expression_to_stream(S, Atom, _HeadNonHead) :-
	using_strcat_semantics,
	atomic(Atom),
	!,
	format(S, '"~w"', [Atom]).
write_semantic_expression_to_stream(_S, Other, HeadNonHead) :-
	regulus_error('~NUnable to write semantic expression ~w (context: ~w)~n', [Other, HeadNonHead]).

write_semantic_expression_list_to_stream(_S, [], _HeadNonHead) :-
	!.
write_semantic_expression_list_to_stream(S, [F | R], HeadNonHead) :-
	format(S, ' ', []),
	write_semantic_expression_to_stream(S, F, HeadNonHead),
	!,
	write_semantic_expression_list_to_stream(S, R, HeadNonHead).

%write_semantic_expression_comma_list_to_stream(S, (F, R)) :-
%	!,
%	format(S, ' ', []),
%	write_semantic_expression_to_stream(S, F),
%	write_semantic_expression_comma_list_to_stream(S, R).
%write_semantic_expression_comma_list_to_stream(S, Other) :-
%	!,
%	format(S, ' ', []),
%	write_semantic_expression_to_stream(S, Other).

%---------------------------------------------------------------
 
nuance_rule_and_slot_definition_files_for_component_files([], [], []).
nuance_rule_and_slot_definition_files_for_component_files([ComponentFile | ComponentFiles],
							  [RuleFile | RuleFiles],
							  [SlotDefinitionFile | SlotDefinitionFiles]) :-
	nuance_rule_and_slot_definition_files_for_component_file(ComponentFile, RuleFile, SlotDefinitionFile),
	!,
	nuance_rule_and_slot_definition_files_for_component_files(ComponentFiles, RuleFiles, SlotDefinitionFiles).

nuance_rule_and_slot_definition_files_for_component_file(ComponentFile, AbsRuleFile, AbsSlotDefinitionFile) :-
	nuance_slot_definition_file(ComponentFile, SlotDefinitionFile),
	nuance_rule_file(ComponentFile, RuleFile),
	absolute_file_name(SlotDefinitionFile, AbsSlotDefinitionFile),
	absolute_file_name(RuleFile, AbsRuleFile),
	file_exists_or_give_up(AbsSlotDefinitionFile),
	file_exists_or_give_up(AbsRuleFile).

file_exists_or_give_up(File) :-
	file_exists(File),
	!.
file_exists_or_give_up(File) :-
	format2error('~N*** Error: can\'t find file: ~w~n', [File]),
	fail.	

%---------------------------------------------------------------

get_all_slots_from_slot_definition_files(SlotDefinitionFiles, AllSlots) :-
	findall(Slot, slot_in_slot_definitions_file_list(Slot, SlotDefinitionFiles), AllSlots0),
	sort(AllSlots0, AllSlots).

slot_in_slot_definitions_file_list(Slot, SlotDefinitionFiles) :-
	member(File, SlotDefinitionFiles),
	read_file_to_atom_list(File, Slots),
	member(Slot, Slots).

%---------------------------------------------------------------

write_top_level_nuance_rules_file_for_component_files(RuleFiles, NuanceRuleFile) :-
	length(RuleFiles, NGrammars),
	absolute_file_name(NuanceRuleFile, AbsNuanceRuleFile),
	cat_files(RuleFiles, NuanceRuleFile),
	format('~N   -- Nuance rules for merged file (~d grammars) written to file ~w~n',
	       [NGrammars, AbsNuanceRuleFile]),
	!.
write_top_level_nuance_rules_file_for_component_files(RuleFiles, NuanceRuleFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [write_top_level_nuance_rules_file_for_component_files(RuleFiles, NuanceRuleFile)]),
	fail.

/*
% This would be a more logical way to do it, but the Nuance utility compute-grammar-probs
% seems to have issues with included grammars...

write_top_level_nuance_rules_file_for_component_files(RuleFiles, NuanceRuleFile) :-
	open(NuanceRuleFile, write, S),
	write_top_level_nuance_rules_file_for_component_files1(RuleFiles, S),
	close(S),
	!.
write_top_level_nuance_rules_file_for_component_files(RuleFiles, NuanceRuleFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [write_top_level_nuance_rules_file_for_component_files(RuleFiles, NuanceRuleFile)]),
	fail.					    

write_top_level_nuance_rules_file_for_component_files1([], _S).
write_top_level_nuance_rules_file_for_component_files1([File | RestFiles], S) :-
	format(S, '~N#include "~w"~n', [File]),
	!,
	write_top_level_nuance_rules_file_for_component_files1(RestFiles, S).
*/

%---------------------------------------------------------------

