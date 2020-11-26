% regulus_expand.pl

% Code for expanding and filtering Regulus rules

:- module(regulus_expand,
	  [expand_and_filter_regulus_rules/2,
	   filter_grouped_regulus_rules/2]
      ).

:- use_module('$REGULUS/Prolog/regulus_declarations').
:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(ordsets)).
%:- use_module(library(assoc)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).

%---------------------------------------------------------------

:- dynamic grounded_rule/3, mother_cat_to_id/2, ungrounded_rule/1, grounded_rule_id_counter/1.
:- dynamic expansion_failed/0.

%---------------------------------------------------------------

expand_and_filter_regulus_rules(Rules, FilteredGroupedRules) :-
	init_expand_and_filter_regulus_rules(Rules, Schedule),
	expand_and_filter_regulus_rules1(Schedule, first, FilteredGroupedRules, Summary-[]),
	present_expansion_summary(Summary),
	clean_up_after_expand_and_filter_regulus_rules.

filter_grouped_regulus_rules(GroupedRulesIn, GroupedRulesOut) :-
	init_filter_grouped_rules(GroupedRulesIn, UnfilteredRuleIDs),
	filter_regulus_rules(UnfilteredRuleIDs, FilteredRuleIDs, _Summary),
	store_ungrounded_rules_after_filtering(FilteredRuleIDs),
	extract_grouped_rules_from_ungrounded_rules_database(GroupedRulesOut).

%---------------------------------------------------------------

init_expand_and_filter_regulus_rules(Rules, Schedule) :-
	feature_instantiation_schedule(Schedule),
	retractall(expansion_failed),
	retractall(ungrounded_rule(_)),
	store_ungrounded_rules(Rules),
	!.
init_expand_and_filter_regulus_rules(Rules, Schedule) :-
	regulus_error('~NInternal error: call ~w failed~n', [init_expand_and_filter_regulus_rules(Rules, Schedule)]).

store_ungrounded_rules([]) :-
	!.
store_ungrounded_rules([F | R]) :-
	assertz(ungrounded_rule(F)),
	!,
	store_ungrounded_rules(R).

%---------------------------------------------------------------

clean_up_after_expand_and_filter_regulus_rules :-
	retractall(expansion_failed),
	retractall(grounded_rule(_, _, _)),
	retractall(ungrounded_rule(_)),
	retractall(grounded_rule_id_counter(_)),
	retractall(mother_cat_to_id(_, _)).

%---------------------------------------------------------------

init_filter_grouped_rules(GroupedRules, UnfilteredRuleIDs) :-
	retractall(grounded_rule(_, _, _)),
	retractall(grounded_rule_id_counter(_)),
	assertz(grounded_rule_id_counter(1)),
	store_grouped_rules_as_ungrounded_rules(GroupedRules, UnfilteredRuleIDs-[]),
	!.
init_filter_grouped_rules(_GroupedRules, _UnfilteredRuleIDs) :-
	regulus_error('~NInternal error: call to init_filter_grouped_rules/2 failed~n', []).

store_grouped_rules_as_ungrounded_rules([], RuleIds-RuleIds) :-
	!.
store_grouped_rules_as_ungrounded_rules([_Cat-Rules | R], RuleIdsIn-RuleIdsOut) :-
	store_rules_as_ungrounded_rules(Rules, RuleIdsIn-RuleIdsNext),
	!,
	store_grouped_rules_as_ungrounded_rules(R, RuleIdsNext-RuleIdsOut).

store_rules_as_ungrounded_rules([], RuleIds-RuleIds) :-
	!.
store_rules_as_ungrounded_rules([F | R], [RuleId | RuleIdsNext]-RuleIdsOut) :-
	store_rule_as_ungrounded_rule(F, RuleId),
	!,
	store_rules_as_ungrounded_rules(R, RuleIdsNext-RuleIdsOut).

store_rule_as_ungrounded_rule(Rule, RuleId) :-
	copy_term(Rule, Rule1),
	make_ground(Rule1),
	assert_grounded_rule_into_database(Rule1, RuleId),
	!.

%---------------------------------------------------------------

expand_and_filter_regulus_rules1([LastGroup], FirstP, FilteredGroupedRules, SummaryIn-SummaryOut) :-
	!,
	expand_and_filter_regulus_rules2(LastGroup, FirstP, SummaryIn-SummaryOut),
	extract_grouped_rules_from_ungrounded_rules_database(FilteredGroupedRules).
expand_and_filter_regulus_rules1([FirstGroup | RestGroups], FirstP, FilteredGroupedRules, SummaryIn-SummaryOut) :-
	expand_and_filter_regulus_rules2(FirstGroup, FirstP, SummaryIn-SummaryNext),
	!,
	expand_and_filter_regulus_rules1(RestGroups, not_first, FilteredGroupedRules, SummaryNext-SummaryOut).
% This next clause should probably never be called, but just in case...
expand_and_filter_regulus_rules1([], _FirstP, FilteredGroupedRules, SummaryIn-SummaryIn) :-
	extract_grouped_rules_from_ungrounded_rules_database(FilteredGroupedRules).

expand_and_filter_regulus_rules2(FeatureGroup, FirstP, SummaryIn-SummaryIn) :-
	FirstP = not_first,
	all_features_in_list_are_ignored(FeatureGroup),
	format('~N -- Skipping ignored features: ~w.~n~n', [FeatureGroup]),
	!.
expand_and_filter_regulus_rules2(FeatureGroup, FirstP, SummaryIn-SummaryOut) :-
	format('~N -- Expanding and filtering using features: ~w.~n~n', [FeatureGroup]),
	timed_call(expand_and_filter_regulus_rules3(FeatureGroup, FirstP, RuleIDs, SummaryIn-SummaryOut), TimeTaken),
	format('~N -- Expanding and filtering done, ~1f secs~n~n', [TimeTaken]),
	present_expanded_rules(RuleIDs),
	!.

expand_and_filter_regulus_rules3(FeatureGroup, FirstP, FilteredRuleIDs, SummaryIn-SummaryOut) :-
	expand_regulus_rules(FeatureGroup, FirstP),
	extract_rule_ids_from_grounded_rules_database(UnfilteredRuleIDs),
	filter_regulus_rules(UnfilteredRuleIDs, FilteredRuleIDs, FilteringSummary),
	store_ungrounded_rules_after_filtering(FilteredRuleIDs),
	Summary = [features=FeatureGroup | FilteringSummary],
	SummaryIn = [Summary | SummaryOut],
	!.

all_features_in_list_are_ignored(FeatureGroup) :-
	\+ some_feature_in_list_is_not_ignored(FeatureGroup).

some_feature_in_list_is_not_ignored(FeatureGroup) :-
	member(Feature, FeatureGroup),
	\+ ignore_feature(Feature),
	% If we have a specialised grammar, we need to check that the feature hasn't already been removed.
	category_internal(_Cat, CatFeatures),
	member(Feature, CatFeatures),
	!.

%---------------------------------------------------------------

cat_and_rule_in_ungrounded_rules_database(HeadCat, Rule) :-
	ungrounded_rule(Rule),
	head_cat_and_line_info_for_rule(Rule, HeadCat, _LineInfo).

extract_rule_ids_from_grounded_rules_database(RuleIds) :-
	findall(RuleId, grounded_rule(RuleId, _MotherCat, _Rule), RuleIds).

store_ungrounded_rules_after_filtering(RuleIDs) :-
	retractall(ungrounded_rule(_)),
	store_ungrounded_rules_after_filtering1(RuleIDs).
	
store_ungrounded_rules_after_filtering1([]) :-
	!.
store_ungrounded_rules_after_filtering1([F | R]) :-
	store_ungrounded_rule_after_filtering(F),
	!,
	store_ungrounded_rules_after_filtering1(R).

store_ungrounded_rule_after_filtering(RuleId) :-
	grounded_rule(RuleId, _MotherCat, GroundedRule),
	unground(GroundedRule, UngroundedRule),
	assertz(ungrounded_rule(UngroundedRule)),
	!.
store_ungrounded_rule_after_filtering(RuleId) :-
	regulus_error('~NInternal error: call ~w failed~n', [store_ungrounded_rule_after_filtering(RuleId)]).

count_grounded_rules(NRules) :-
	findall(Id, grounded_rule(Id, _MotherCat, _Rule), Ids),
	length(Ids, NRules).

assert_grounded_rule_into_database(Rule) :-
	assert_grounded_rule_into_database(Rule, _RuleId).

assert_grounded_rule_into_database(Rule, RuleId) :-
	new_grounded_rule_id(RuleId),
	head_cat_and_line_info_for_rule(Rule, HeadCat, _LineInfo),
	assertz(grounded_rule(RuleId, HeadCat, Rule)),
	assertz(mother_cat_to_id(HeadCat, RuleId)),
	!.
assert_grounded_rule_into_database(Rule, RuleId) :-
	regulus_error('~NInternal error: call ~w failed~n', [assert_grounded_rule_into_database(Rule, RuleId)]).

new_grounded_rule_id(RuleId) :-
	grounded_rule_id_counter(RuleId),
	retractall(grounded_rule_id_counter(_)),
	NextRuleId is RuleId + 1,
	assertz(grounded_rule_id_counter(NextRuleId)),
	!.

%---------------------------------------------------------------

expand_regulus_rules(FeatureGroup, FirstP) :-
	format('~N   -- Expanding rules... ', []), flush_output(user),
	timed_call(expand_regulus_rules_main(FeatureGroup, FirstP), TimeTaken),
	count_grounded_rules(NRules),
	TimePerRule is 1000 * TimeTaken / NRules,
	format('~1f secs (~2f msecs/rule)~n~n', [TimeTaken, TimePerRule]).

expand_regulus_rules_main(FeatureGroup, FirstP) :-
	retractall(expansion_failed),
	retractall(grounded_rule(_, _, _)),
	retractall(mother_cat_to_id(_, _)),
	retractall(grounded_rule_id_counter(_)),
	assertz(grounded_rule_id_counter(1)),
	expand_regulus_rules1(FeatureGroup, FirstP).

expand_regulus_rules1(FeatureGroup, FirstP) :-
	expand_regulus_rule_in_db(ExpandedRule, FeatureGroup, FirstP),
	ExpandedRule \== failed_expansion,
	assert_grounded_rule_into_database(ExpandedRule),
	fail.
expand_regulus_rules1(_FeatureGroup, _FirstP) :-
	expansion_failed,
	!,
	format('~N--- Abandoning processing~n', []),
	fail.
expand_regulus_rules1(_FeatureGroup, _FirstP).

expand_regulus_rule_in_db(Result, FeatureGroup, FirstP) :-
	ungrounded_rule(rule(Rule, LineInfo)),
	regulus_debug_call(2, inform_about_expanding_rule(Rule, LineInfo)),
	expand_regulus_rule(Rule, InstantiatedRule, FeatureGroup, FirstP, LineInfo),
	(   InstantiatedRule = failed_expansion ->
	    Result = failed_expansion,
	    assertz(expansion_failed)
	;
	    Result = rule(InstantiatedRule, LineInfo)
	).

expand_regulus_rule(Rule, InstantiatedRule, FeatureGroup, FirstP, LineInfo) :-
	on_exception(
	Exception, 
	( expand_regulus_rule1(Rule, InstantiatedRule0, FeatureGroup, FirstP), 
	  InstantiatedRule = InstantiatedRule0 ),
	( inform_about_regulus_exception(Exception, LineInfo), 
	  InstantiatedRule = failed_expansion )
    ).
	
% If there's more than one feature group in the feature instantiation schedule
% (i.e. we are doing non-trivial iterative expansion), then there we only need
% to do most of the processing on the first group. In second and subsequent
% passes, we just instantiate features.

expand_regulus_rule1(Rule, InstantiatedRule, FeatureGroup, FirstP) :-
	FirstP = first,
	!,
	give_up_if_bad_rule(Rule),
	pre_process_rule(Rule, PreProcessedRule),
	give_up_if_bad_pre_processed_rule(Rule),
	instantiate_syntactic_features(PreProcessedRule, InstantiatedRule, FeatureGroup),
	make_ground(InstantiatedRule).
expand_regulus_rule1(Rule, InstantiatedRule, FeatureGroup, _NotFirst) :-
	instantiate_syntactic_features(Rule, InstantiatedRule, FeatureGroup),
	make_ground(InstantiatedRule).

%---------------------------------------------------------------

give_up_if_bad_rule(Rule) :-
	\+ using_prolog_semantics,
	sem_variable_in_rule_head_not_in_body(Rule),
	!,
	regulus_error('~NSemantic variable in rule head doesn''t appear in body~n', []).
give_up_if_bad_rule(_Rule).

sem_variable_in_rule_head_not_in_body( ( H --> B ) ) :-
	%H = cat(_CatName, _SynFeatsWithVals, Sem),
        H = _Cat:Features,
	member(sem=Sem, Features),
	term_variables(Sem, SemHeadVars),
	term_variables(B, BodyVars),
	member(V, SemHeadVars),
	\+ id_member(V, BodyVars),
	!.

%---------------------------------------------------------------

give_up_if_bad_pre_processed_rule(Rule) :-
	\+ using_prolog_semantics,
	sem_variable_appears_twice_in_body(Rule),
	!,
	regulus_error('~NSemantic variable appears more than once in rule body~n', []).
give_up_if_bad_pre_processed_rule(_Rule).

sem_variable_appears_twice_in_body( ( _H --> B ) ) :-
        sem_vars_in_body(B, SemVars-[]),
	repeated_var(SemVars, []).

sem_vars_in_body(Cat, [Var | R]-R) :-
	get_sem_var_from_cat(Cat, Var),
	!.
sem_vars_in_body((F, R), In-Out) :-
	sem_vars_in_body(F, In-Next),
	sem_vars_in_body(R, Next-Out),
	!.
sem_vars_in_body(_Other, In-In).

get_sem_var_from_cat(Cat, SemVar) :-
	nonvar(Cat),
	Cat = _CatName:Features,
	member(sem=SemVar, Features),
	var(SemVar),
	!.

repeated_var([F | _R], Previous) :-
	id_member(F, Previous),
	!.
repeated_var([F | R], Previous) :-
	!,
	repeated_var(R, [F | Previous]).

%---------------------------------------------------------------

inform_about_expanding_rule(Rule, LineInfo) :-
	Rule = ( Head --> _Tail ),
	copy_term(Head, Head1),
	numbervars(Head1, 0, _),
	LineInfo = line_info(_ItemNumber, From-To, File),
	format('~NExpanding rule ~w --> ... in lines ~d to ~d in ~w~n', [Head1, From, To, File]),
	!.
inform_about_expanding_rule(_Rule, _LineInfo).

%---------------------------------------------------------------

% pre_process_rule(+Rule, -PreProcessedRule)

pre_process_rule(Rule, PreProcessedRule) :-
	expand_disjunctions(Rule, ExpandedRule, nonhead),
	instantiate_null_sem_values(ExpandedRule),
	simplify_nulls_in_rule(ExpandedRule, ExpandedRule1),
	replace_anonymous_vars_with_any(ExpandedRule1, PreProcessedRule).

	%copy_term(pre_process_rule(Rule, PreProcessedRule), T),
	%numbervars(T, 1, _),
	%format('~n~w~n', [T]).

expand_disjunctions((H --> B), (H1 --> B1), _HeadP) :-
	!,
	expand_disjunctions(H, H1, head),
	expand_disjunctions(B, B1, nonhead).
expand_disjunctions((P, Q), (P1, Q1), _HeadP) :-
	!,
	expand_disjunctions(P, P1, nonhead),
	expand_disjunctions(Q, Q1, nonhead).
expand_disjunctions((P ; Q), Result, HeadP) :-
	!,
	(   expand_disjunctions(P, Result, HeadP) ;
	    expand_disjunctions(Q, Result, HeadP)
	).
expand_disjunctions((?P), Result, HeadP) :-
	!,
	(   expand_disjunctions(P, Result, HeadP) ;
	    Result = []
	).
expand_disjunctions(Atom, Atom, _HeadP) :-
	atomic(Atom),
	!.
expand_disjunctions(Cat:FeatsWithVals, InternalCat, HeadP) :-
	!,
	internalise_cat(Cat, FeatsWithVals, InternalCat, HeadP).
expand_disjunctions(Other, _, _HeadP) :-
	regulus_error('~NBad subterm in rule: ~q.~n', [Other]).

%---------------------------------------------------------------

simplify_nulls_in_rule((H --> B), (H1 --> B1)) :-
	!,
	simplify_nulls_in_rule(H, H1),
	simplify_nulls_in_rule(B, BNext),
	(   BNext = [] ->
	    B1 = '*empty*' ;
	    B1 = BNext
	). 
simplify_nulls_in_rule((P, Q), Result) :-
	!,
	simplify_nulls_in_rule(P, P1),
	simplify_nulls_in_rule(Q, Q1),
	simplify_nulls_in_conjunction((P1, Q1), Result).
simplify_nulls_in_rule(Atom, Atom) :-
	atomic(Atom),
	!.
simplify_nulls_in_rule(cat(Name, Feats, Sem), cat(Name, Feats, Sem1)) :-
	!,
	simplify_nulls_in_sem(Sem, Sem1).
simplify_nulls_in_rule(Other, _) :-
	regulus_error('~NBad subterm in expanded rule: ~q.~n', [Other]).

simplify_nulls_in_conjunction(([], P), P) :-
	!.
simplify_nulls_in_conjunction((P, []), P) :-
	!.
simplify_nulls_in_conjunction(Other, Other) :-
	!.

simplify_nulls_in_sem(V, V) :-
	var(V),
	!.
simplify_nulls_in_sem(Atom, Atom) :-
	atomic(Atom),
	!.
simplify_nulls_in_sem(TermIn, TermOut) :-
	functor(TermIn, F, N),
	functor(TermNext, F, N),
	simplify_nulls_in_sem_args(N, TermIn, TermNext),
	simplify_nulls_in_term(TermNext, TermOut),
	!.

simplify_nulls_in_sem_args(0, _TermIn, _TermOut) :-
	!.
simplify_nulls_in_sem_args(I, TermIn, TermOut) :-
	I > 0,
	arg(I, TermIn, ArgIn),
	arg(I, TermOut, ArgOut),
	simplify_nulls_in_sem(ArgIn, ArgOut),
	I1 is I - 1,
	!,
	simplify_nulls_in_sem_args(I1, TermIn, TermOut).

simplify_nulls_in_term(concat(Null, X), X) :-
	is_null_value(Null),
	!.
simplify_nulls_in_term(concat(X, Null), X) :-
	is_null_value(Null),
	!.
simplify_nulls_in_term(strcat(Null, X), X) :-
	is_null_value(Null),
	\+ using_strcat_semantics,
	!.
simplify_nulls_in_term(strcat(X, Null), X) :-
	is_null_value(Null),
	\+ using_strcat_semantics,
	!.
simplify_nulls_in_term(strcat(Null, X), strcat([], X)) :-
	is_null_value(Null),
	using_strcat_semantics,
	!.
simplify_nulls_in_term(strcat(X, Null), strcat(X, [])) :-
	is_null_value(Null),
	using_strcat_semantics,
	!.
simplify_nulls_in_term(add(Null, X), X) :-
	is_null_value(Null),
	!.
simplify_nulls_in_term(add(X, Null), X) :-
	is_null_value(Null),
	!.
simplify_nulls_in_term(sub(Null, X), neg(X)) :-
	is_null_value(Null),
	!.
simplify_nulls_in_term(sub(X, Null), X) :-
	is_null_value(Null),
	!.
simplify_nulls_in_term(mul(_X, Null), 0) :-
	is_null_value(Null),
	!.
simplify_nulls_in_term(mul(Null, _X), 0) :-
	is_null_value(Null),
	!.
simplify_nulls_in_term(div(X, Null), div(X, 0)) :-
	is_null_value(Null),
	!.
simplify_nulls_in_term(div(Null, _X), 0) :-
	is_null_value(Null),
	!.
simplify_nulls_in_term(neg(Null), 0) :-
	is_null_value(Null),
	!.
simplify_nulls_in_term(Other, Other).

% REF/NOREF
/*
is_null_value(Null) :-
	(   Null == ref('*null_value*') ;
	    Null == nonref('*null_value*')
	).
*/
is_null_value(Null) :-
	Null == '*null_value*'.

%---------------------------------------------------------------

% replace_anonymous_vars_with_any(+Rule, -RuleOut)

% This predicate carried out the Moore "ANY" transformation,
% described in Dowding (!) et al 2001. Let Val be a feature value in
% a category C in the body of Rule. Then if Val is a variable that
% doesn't occur anywhere else in Rule (an `anonymous variable'),
% we replace Val with the atom 'ANY'. Similarly, if Val is a disjunction,
% and the value is not shared with another feature elsewhere in the
% rule, we replace Val with the term 'ANY'(Val)
%
% If C is a non-head category in which at least one feature value has been
% replaced by 'ANY' or 'ANY'(Val), we also create a new rule of the form
%
%   C' --> C
%
% where C is the original version of C (i.e. with the anonymous
% variables) and C' is the version with the anonymous variables
% replaced by 'ANY' or 'ANY'(Val). We check first to make sure that we 
% haven't already created a rule of this type.
% 
% TRICKY POINT TO NOTE: for compactness, we arrange things so 
% that replace_anonymous_vars_with_any non-deterministically 
% returns EITHER the transformed rule OR any one of the new
% C --> C' rules, if any were created.

replace_anonymous_vars_with_any(Rule, RuleOut) :-
	copy_term(Rule, Rule1),
	consolidate_feat_vals_for_any_transform(Rule1, Rule2),
	replace_feat_val_vars_for_any_transform(Rule2),
	replace_feat_val_structs_for_any_transform(Rule2, Rule3, nonhead),
	extract_any_rules(Rule3, []-AnyRules),
	!,
	(   RuleOut = Rule3 ;
	    member(RuleOut, AnyRules)
	).
replace_anonymous_vars_with_any(Rule, RuleOut) :-
	regulus_error('~NInternal error: call ~w failed~n', [replace_anonymous_vars_with_any(Rule, RuleOut)]).

%---------------------------------------------------------------

consolidate_feat_vals_for_any_transform((H --> B), (H1 --> B1)) :-
	consolidate_feat_vals_for_any_transform(H, H1),
	consolidate_feat_vals_for_any_transform(B, B1),
	!.
consolidate_feat_vals_for_any_transform((P, Q), (P1, Q1)) :-
	consolidate_feat_vals_for_any_transform(P, P1),
	consolidate_feat_vals_for_any_transform(Q, Q1),
	!.
consolidate_feat_vals_for_any_transform(?(P), ?(P1)) :-
	consolidate_feat_vals_for_any_transform(P, P1),
	!.
consolidate_feat_vals_for_any_transform(Atom, Atom) :-
	atomic(Atom),
	!.
consolidate_feat_vals_for_any_transform(cat(CatName, SynFeatsWithVals, Sem), cat(CatName, SynFeatsWithVals1, Sem)) :-
	consolidate_feat_vals_in_feat_val_list(SynFeatsWithVals, SynFeatsWithVals1, '*no_current_feat_val*'),
	!.
consolidate_feat_vals_for_any_transform(X, Y) :-
	regulus_error('~NInternal error: call ~w failed~n', [consolidate_feat_vals_for_any_transform(X, Y)]).

% Exploit fact that features are ordered in list
consolidate_feat_vals_in_feat_val_list([], Result, LastFeat) :-
	(   LastFeat = '*no_current_feat_val*' ->
	    Result = [] ;
	    Result = [LastFeat]
	),
	!.
consolidate_feat_vals_in_feat_val_list([(Feat = Val) | R], Result, feat(Feat, Var, Constraint)) :-
	consolidate_single_feat_val((Feat = Val), feat(Feat, Var, Constraint), ConsolidatedFeat),
	!,
	consolidate_feat_vals_in_feat_val_list(R, Result, ConsolidatedFeat).
consolidate_feat_vals_in_feat_val_list([(Feat = Val) | R], Result, LastFeat) :-
	(   LastFeat = '*no_current_feat_val*' ->
	    Result = Result1 ;
	    Result = [LastFeat | Result1]
	),
	NextFeatVal = feat(Feat, Var, Constraint),
	(   var(Val) ->
	    Var = Val,
	    Constraint = '*no_constraint*' ;
	    Constraint = Val
	),
	consolidate_feat_vals_in_feat_val_list(R, Result1, NextFeatVal).

consolidate_single_feat_val((Feat = Val), feat(Feat, Var, Constraint), feat(Feat, Var, Constraint1)) :-
	(   var(Val) ->
	    Var = Val ;
	    consolidate_constraints(Constraint, Val, Constraint1)
	).

consolidate_constraints('*no_constraint*', Val, Val) :-
	!.
consolidate_constraints(Constraint, Val, (Constraint/\Val)) :-
	!.

%---------------------------------------------------------------

replace_feat_val_vars_for_any_transform((H --> B)) :-
	replace_feat_val_vars_for_any_transform(H),
	replace_feat_val_vars_for_any_transform(B),
	!.
replace_feat_val_vars_for_any_transform((P, Q)) :-
	replace_feat_val_vars_for_any_transform(P),
	replace_feat_val_vars_for_any_transform(Q),
	!.
replace_feat_val_vars_for_any_transform(?(P)) :-
	replace_feat_val_vars_for_any_transform(P),
	!.
replace_feat_val_vars_for_any_transform(Atom) :-
	atomic(Atom),
	!.
replace_feat_val_vars_for_any_transform(cat(_CatName, SynFeatsWithVals, _Sem)) :-
	replace_feat_val_vars_in_feat_val_list(SynFeatsWithVals),
	!.
replace_feat_val_vars_for_any_transform(Other) :-
	regulus_error('~NInternal error: call ~w failed~n', [replace_feat_val_vars_for_any_transform(Other)]).

replace_feat_val_vars_in_feat_val_list([]).
replace_feat_val_vars_in_feat_val_list([feat(_F, V, _Constraint) | R]) :-
	replace_feat_val_var(V),
	replace_feat_val_vars_in_feat_val_list(R).

replace_feat_val_var(V) :-
	var(V),
	!,
	V = v(_Singleton, _NewVar).
replace_feat_val_var(v(Singleton, _NewVar)) :-
	!,
	Singleton = not_singleton.
replace_feat_val_var(_Other) :-
	!.

%---------------------------------------------------------------

replace_feat_val_structs_for_any_transform((H --> B), (H1 --> B1), _Head) :-
	replace_feat_val_structs_for_any_transform(H, H1, head),
	replace_feat_val_structs_for_any_transform(B, B1, nonhead),
	!.
replace_feat_val_structs_for_any_transform((P, Q), (P1, Q1), nonhead) :-
	replace_feat_val_structs_for_any_transform(P, P1, nonhead),
	replace_feat_val_structs_for_any_transform(Q, Q1, nonhead),
	!.
replace_feat_val_structs_for_any_transform(?(P), ?(P1), nonhead) :-
	replace_feat_val_structs_for_any_transform(P, P1, nonhead),
	!.
replace_feat_val_structs_for_any_transform(Atom, Atom, nonhead) :-
	atomic(Atom),
	!.
replace_feat_val_structs_for_any_transform(cat(CatName, SynFeatsWithVals, Sem), cat(CatName, SynFeatsWithVals1, Sem), HeadP) :-
	replace_feat_val_structs_in_feat_val_list(SynFeatsWithVals, SynFeatsWithVals1, HeadP),
	!.
replace_feat_val_structs_for_any_transform(X, Y, Z) :-
	regulus_error('~NInternal error: call ~w failed~n', [replace_feat_val_structs_for_any_transform(X, Y, Z)]).

replace_feat_val_structs_in_feat_val_list([], [], _HeadP).
replace_feat_val_structs_in_feat_val_list([F | R], Result, HeadP) :-
	replace_feat_val_struct(F, HeadP, Result-RestResult),
	replace_feat_val_structs_in_feat_val_list(R, RestResult, HeadP).

% Unconstrained var in head; just use the var
replace_feat_val_struct(feat(F, v(_SingletonIfVar, Var), '*no_constraint*'), head, [(F=Var) | RestResult]-RestResult) :-
	!.
% Constrained singleton var in head; just use the constraint
replace_feat_val_struct(feat(F, v(SingletonIfVar, _Var), Constraint), head, [(F=Constraint) | RestResult]-RestResult) :-
	var(SingletonIfVar),
	!.
% Constrained non-singleton var in head; use the var and the constraint
replace_feat_val_struct(feat(F, v(SingletonIfVar, Var), Constraint), head, [(F=Var), (F=Constraint) | RestResult]-RestResult) :-
	nonvar(SingletonIfVar),
	!.
% Unconstrained singleton var in body; make into an 'ANY'
replace_feat_val_struct(feat(F, v(SingletonIfVar, _Var), '*no_constraint*'), nonhead, [(F='ANY') | RestResult]-RestResult) :-
	var(SingletonIfVar),
	!.
% Unconstrained non-singleton var in body; just use the var
replace_feat_val_struct(feat(F, v(SingletonIfVar, Var), '*no_constraint*'), nonhead, [(F=Var) | RestResult]-RestResult) :-
	nonvar(SingletonIfVar),
	!.
% Constrained singleton var in body; constraint allows several but not all values; make into an 'ANY'(Constraint)
replace_feat_val_struct(feat(F, v(SingletonIfVar, _Var), Constraint), nonhead, [(F='ANY'(Constraint)) | RestResult]-RestResult) :-
	var(SingletonIfVar),
	constraint_on_feat_allows_some_but_not_all_values(Constraint, F),
	!.
% Constrained singleton var in body; constraint allows all values; make into an 'ANY' 
replace_feat_val_struct(feat(F, v(SingletonIfVar, _Var), Constraint), nonhead, [(F='ANY') | RestResult]-RestResult) :-
	var(SingletonIfVar),
	constraint_on_feat_allows_all_values(Constraint, F),
	!.
% Constrained singleton var in body; remaining case: constrain allows just one value; just use the constraint
replace_feat_val_struct(feat(F, v(SingletonIfVar, _Var), Constraint), nonhead, [(F=Constraint) | RestResult]-RestResult) :-
	var(SingletonIfVar),
	!.
% Constrained non-singleton var in body; constraint allows all values; just use the var
replace_feat_val_struct(feat(F, v(SingletonIfVar, Var), Constraint), nonhead, [(F=Var) | RestResult]-RestResult) :-
	nonvar(SingletonIfVar),
	constraint_on_feat_allows_all_values(Constraint, F),
	!.
% Constrained non-singleton var in body; constraint disallows at least some values; use the var and the constraint
replace_feat_val_struct(feat(F, v(SingletonIfVar, Var), Constraint), nonhead, [(F=Var), (F=Constraint) | RestResult]-RestResult) :-
	nonvar(SingletonIfVar),
	!.

%---------------------------------------------------------------

extract_any_rules((_H --> B), In-Out) :-
	!,
	extract_any_rules(B, In-Out).
extract_any_rules((P, Q), In-Out) :-
	!,
	extract_any_rules(P, In-Next),
	extract_any_rules(Q, Next-Out).
extract_any_rules(?(P), In-Out) :-
	!,
	extract_any_rules(P, In-Out).
extract_any_rules(cat(CatName, SynFeats, Sem), In-[NewRule|In]) :-
	feat_val_list_contains_any_value(SynFeats),
	\+ existing_any_category(CatName, SynFeats),
	!,
	make_any_rule(cat(CatName, SynFeats, Sem), NewRule).
extract_any_rules(_Other, In-In) :-
	!.

feat_val_list_contains_any_value([(_Feat=Val) | _R]) :-
	nonvar(Val),
	(   Val = 'ANY' ;
	    Val = 'ANY'(_)
	),
	!.
feat_val_list_contains_any_value([_F | R]) :-
	feat_val_list_contains_any_value(R).

%---------------------------------------------------------------

make_any_rule(cat(CatName, SynFeats, Sem), Rule) :-
	replace_any_with_anonymous_var(SynFeats, SynFeats1, BodySynFeats),
	sem_values_for_any_rule(Sem, HeadSem, BodySem),
	Head = cat(CatName, SynFeats1, HeadSem),
	Body = cat(CatName, BodySynFeats, BodySem),
	Rule = ( Head --> Body ),
	remember_any_category(CatName, SynFeats),
	!.
make_any_rule(Cat, Rule) :-
	regulus_error('~NInternal error: call failed: ~w~n', [make_any_rule(Cat, Rule)]).

replace_any_with_anonymous_var([], [], []).
replace_any_with_anonymous_var([(F=V) | R], [(F=V) | R1], [(F=V1) | R2]) :-
	V == 'ANY',
	!,
	V1 = _NewVar,
	replace_any_with_anonymous_var(R, R1, R2).
replace_any_with_anonymous_var([(F=V) | R], [(F=V) | R1], [(F=V1) | R2]) :-
	nonvar(V),
	V = 'ANY'(Constraint),
	!,
	V1 = Constraint,
	replace_any_with_anonymous_var(R, R1, R2).
replace_any_with_anonymous_var([(F=V) | R], [(F=V) | R1], [(F=V) | R2]) :-
	(  atomic(V) ; var(V) ),
	!,
	replace_any_with_anonymous_var(R, R1, R2).
% If we have a disjunctive feature value, we need to add a second "variable" version so as to
% share the expansion of that value between mother and daughter in the "any" rule.
replace_any_with_anonymous_var([(F=V) | R], [(F=V), (F=_AnonVal) | R1], [(F=V), (F=_AnonVal) | R2]) :-
	!,
	replace_any_with_anonymous_var(R, R1, R2).

sem_values_for_any_rule(no_value, no_value, no_value) :-
	!.
% REF/NOREF
%sem_values_for_any_rule(value(ValueType, _Val), value(ValueType, ref(Val1)), value(ValueType, nonref(Val1))).
sem_values_for_any_rule(value(ValueType, _Val), value(ValueType, Val1), value(ValueType, Val1)).

%---------------------------------------------------------------

remember_any_category(CatName, SynFeats) :-
	copy_term(SynFeats, SynFeats1),
	numbervars(SynFeats1, 0, _),
	asserta(any_substituted_category(CatName, SynFeats1)).

existing_any_category(CatName, SynFeats) :-
	copy_term(SynFeats, SynFeats1),
	numbervars(SynFeats1, 0, _),
	any_substituted_category(CatName, SynFeats1).

%---------------------------------------------------------------

% Following code relies on the fact that we have sorted the features
% in the category declarations when we internalised them.

% It may be the case that a feature is specified more than once in
% a feature-set, e.g. cat:[f=X, f=(a\/b)]

internalise_cat(CatName, FeatsWithVals, _, _) :-
	top_level_category(CatName),
	member(sem=_SemVal, FeatsWithVals),
	!,
	regulus_error('~NMeaningless for top-level category to use sem feature. Probably gsem intended?~n', []).
% Note that we remove the ignored features before checking validity of
% the feat-val list, since otherwise we'll get an error - if there are
% any ignored features, they'll be missing from the internal category
% declaration too.
internalise_cat(CatName, FeatsWithVals, cat(CatName, SynFeatsWithVals, Sem), HeadP) :-
	atom(CatName),
	get_category_internal(CatName, AllFeats),
	give_up_if_undeclared_feats_in_feat_val_list(CatName, AllFeats, FeatsWithVals),
	remove_ignored_feat_val_pairs_from_list(FeatsWithVals, FeatsWithVals1),
	expand_cat_valued_feat_val_pairs(FeatsWithVals1, FeatsWithVals2),
	replace_vals_with_substitutes_in_feat_val_list(FeatsWithVals2, FeatsWithVals3),
	is_valid_feat_val_list(FeatsWithVals3, AllFeats),
	sort(FeatsWithVals3, FeatsWithVals4),
	internalise_cat_feats(AllFeats, FeatsWithVals4, SynFeatsWithVals, Sem0),
	internalise_cat_sem(HeadP, Sem0, Sem),
	!.
internalise_cat(CatName, FeatsWithVals, _, _) :-
	regulus_error('~NBad category: ~q~n', [CatName:FeatsWithVals]).

% internalise_cat_feats(+AllSynFeats, +FeatsWithVals, -SynFeatsWithVals, -Sem).

internalise_cat_feats(AllSynFeats, FeatsWithVals, SynFeatsWithVals, Sem) :-
	(   ( member(sem, AllSynFeats) ; member(gsem, AllSynFeats) ) ->
	    true ;
	    Sem = no_value
	),
	internalise_cat_feats1(AllSynFeats, FeatsWithVals, SynFeatsWithVals, Sem, '*dummy*').

internalise_cat_feats1([], [], [], _Sem, _LastFeat) :-
	!.
internalise_cat_feats1([Feat|R], [(Feat=Val)|R1], [(Feat=Val)|SynFeatsWithVals], Sem, _LastFeat) :-
	( Feat \== sem, Feat \== gsem),
	!,
	internalise_cat_feats1(R, R1, SynFeatsWithVals, Sem, Feat).
internalise_cat_feats1(R, [(LastFeat=Val)|R1], [(LastFeat=Val)|SynFeatsWithVals], Sem, LastFeat) :-
	( LastFeat \== sem, LastFeat \== gsem),
	!,
	internalise_cat_feats1(R, R1, SynFeatsWithVals, Sem, LastFeat).
internalise_cat_feats1([Feat|R], R1, [(Feat=_AnonVal)|SynFeatsWithVals], Sem, _LastFeat) :-
	( Feat \== sem, Feat \== gsem),
	!,
	internalise_cat_feats1(R, R1, SynFeatsWithVals, Sem, Feat).
internalise_cat_feats1([sem|R], [(sem=Val)|R1], SynFeatsWithVals, Sem, _LastFeat) :-
	!,
	Sem = value(return, Val),
	internalise_cat_feats1(R, R1, SynFeatsWithVals, Sem, sem).
internalise_cat_feats1([gsem|R], [(gsem=Val)|R1], SynFeatsWithVals, Sem, _LastFeat) :-
	!,
	Sem = value(global, Val),
	internalise_cat_feats1(R, R1, SynFeatsWithVals, Sem, sem).
internalise_cat_feats1([sem|R], R1, SynFeatsWithVals, Sem, _LastFeat) :-
	!,
	Sem = no_value,
	internalise_cat_feats1(R, R1, SynFeatsWithVals, Sem, sem).
internalise_cat_feats1([gsem|R], R1, SynFeatsWithVals, Sem, _LastFeat) :-
	!,
	Sem = no_value,
	internalise_cat_feats1(R, R1, SynFeatsWithVals, Sem, sem).

%---------------------------------------------------------------

% The point of this predicate is if necessary to substitute feature
% values deriving from inheritance, as defined by feature_value_space_substitution/3.

replace_vals_with_substitutes_in_feat_val_list([], []).
replace_vals_with_substitutes_in_feat_val_list([Feat=Val | R], [Feat=Val1 | R1]) :-
	replace_feat_with_substitute(Val, Feat, Val1),
	!,
	replace_vals_with_substitutes_in_feat_val_list(R, R1).

replace_feat_with_substitute(Val, Feat, Val) :-
	member(Feat, [sem, gsem]),
	!.
replace_feat_with_substitute(Val, Feat, Val1) :-
	replace_syn_feat_with_substitute(Val, Feat, Val1).
	
replace_syn_feat_with_substitute(Val, _Feat, Val) :-
	var(Val),
	!.
replace_syn_feat_with_substitute(Val, Feat, Val1) :-
	atomic(Val),
	feature(Feat, ValueSpaceId),
	feature_value_space_substitution(Val, Val1, ValueSpaceId),
	!.
replace_syn_feat_with_substitute(Val, _Feat, Val) :-
	atomic(Val),
	!.
replace_syn_feat_with_substitute((P/\Q), Feat, (P1/\Q1)) :-
	!,
	replace_syn_feat_with_substitute(P, Feat, P1),
	replace_syn_feat_with_substitute(Q, Feat, Q1).
replace_syn_feat_with_substitute((P\/Q), Feat, (P1\/Q1)) :-
	!,
	replace_syn_feat_with_substitute(P, Feat, P1),
	replace_syn_feat_with_substitute(Q, Feat, Q1).
replace_syn_feat_with_substitute((\(P)), Feat, (\(P1))) :-
	!,
	replace_syn_feat_with_substitute(P, Feat, P1).

%---------------------------------------------------------------

expand_cat_valued_feat_val_pairs([], []).
expand_cat_valued_feat_val_pairs([Feat=Val|R], Result) :-
	is_cat_valued_feature(Feat),
	!,
	expand_cat_valued_feat_val_pair(Feat=Val, F1),
	expand_cat_valued_feat_val_pairs(R, R1),
	append(F1, R1, Result).
expand_cat_valued_feat_val_pairs([Feat=Val|R], [Feat=Val|R1]) :-
	expand_cat_valued_feat_val_pairs(R, R1).

% If Val is an atom, then this is a non-cat value. Keep it as it is.
expand_cat_valued_feat_val_pair(Feat=Val, [Feat=Val]) :-
	atom(Val),
	!.
% If Val is a variable, then we need to make sure that all the subfeatures
% are shared with other features where Var occurs. So Result contains all
% the subfeatures, and we instantiate Var to this list to propagate the 
% values to the other places.
expand_cat_valued_feat_val_pair(Feat=Val, Result) :-
	var(Val),
	findall(SubFeat, subfeature(Feat, _, _, SubFeat), SubFeats),
	make_anonymous_featval_pairs(SubFeats, SubFeatPairs),
	Result = [(Feat = _NewAnonVal) | SubFeatPairs],
	Val = Result,
	!.
% Val should only be a list if it's been instantiated as a result of the operation 
% in the previous clause. Just use it as it is.
expand_cat_valued_feat_val_pair(_Feat=Val, Result) :-
	is_list(Val),
	Result = Val,
	!.
% If Val is a category, then expand it into a list of feature/value pairs
% and make the category the value of Feat
expand_cat_valued_feat_val_pair(Feat=(Cat:SubFeatVals), [(Feat=Cat) | Result]) :-
	expand_subfeats_in_feat_val_pair(SubFeatVals, Feat, Cat, Result).
expand_cat_valued_feat_val_pair(Other, Result) :-
	regulus_error('~NBad call: ~w', [expand_cat_valued_feat_val_pair(Other, Result)]).

expand_subfeats_in_feat_val_pair([], _Feat, _Cat, []).
expand_subfeats_in_feat_val_pair([(SubFeat=Val) | R], Feat, Cat, [(SubFeat1=Val) | R1]) :-
	(   subfeature(Feat, Cat, SubFeat, SubFeat1) ->
	    expand_subfeats_in_feat_val_pair(R, Feat, Cat, R1) ;
	    regulus_error('~NUnknown feature ~w in category ~w used as value of feature ~w', [SubFeat, Cat, Feat])
	).
	
make_anonymous_featval_pairs([], []).
make_anonymous_featval_pairs([F | R], [(F = _) | R1]) :-
	make_anonymous_featval_pairs(R, R1).

%---------------------------------------------------------------

give_up_if_undeclared_feats_in_feat_val_list(CatName, AllFeats, FeatsWithVals) :-
	undeclared_feats_in_feats_val_list(FeatsWithVals, AllFeats, []-UndeclaredFeats),
	UndeclaredFeats \== [],
	!,
	regulus_error('~NUndeclared feature(s) ~w in category ~w~n', [UndeclaredFeats, CatName]).
give_up_if_undeclared_feats_in_feat_val_list(_CatName, _AllFeats, _FeatsWithVals).

% undeclared_feats_in_feats_val_list(+FeatsWithVals, +AllFeats, InUndeclaredFeats-OutUndeclaredFeats)

undeclared_feats_in_feats_val_list([], _AllFeats, OutUndec-OutUndec).
undeclared_feats_in_feats_val_list([(Feat=_Val) | R], AllFeats, InUndec-OutUndec) :-
	\+ member(Feat, AllFeats),
	\+ ignore_feature(Feat),
	!,
	NextUndec = [Feat | InUndec],
	undeclared_feats_in_feats_val_list(R, AllFeats, NextUndec-OutUndec).
undeclared_feats_in_feats_val_list([_F | R], AllFeats, InUndec-OutUndec) :-
	!,
	undeclared_feats_in_feats_val_list(R, AllFeats, InUndec-OutUndec).

%---------------------------------------------------------------

% internalise_cat_sem(HeadP, Sem, Sem1)

% Internalise the semantic value of the category. We do rather different
% things for a category that is the head of a rule (HeadP = head) and 
% a category that occurs in the body (HeadP = nonhead).
%
% If Sem belongs to a head, we just keep it as it is, except that we mark
% all variables as references. This corresponds to passing up a structure 
% in a generated GSL rule, e.g.
%
% np:[sem=[dev=D, loc=L]] --> 
%      device:[sem=D], 
%      in, 
%      location:[sem=L].         (1)
% 
% might become something like
%
% NP ( device:a in location:b ) {return([<dev $a> <loc $b>])}
%
% If Sem belongs to a non-head, we again branch depending on whether it is a
% structure or a plain variable:
%
% If it's plain variable, we keep it as it is, except that we mark it as a 
% non-reference, as in example (1).
%
% If it's a structure, we replace it with a new variable V, also marked
% a non-reference, and instantiate all variables inside the structure as
% components of V. We use the notation a+b instead of the more usual a.b.

% This corresponds to GSL's Java-like way of accessing
% parts of daughter's structure, e.g.

% np:[sem=[spec=S, dev=D, loc=L]] --> 
%      device:[sem=[spec=S, dev=D]], 
%      in, 
%      location:[sem=L].         (2)
% 
% might become something like
%
% NP ( device:a in location:b ) {return([<spec $a.spec> <dev $a.dev> <loc $b>])}

internalise_cat_sem(nonhead, value(global, _Val), _Result) :-
	regulus_error('~Ngsem feature is meaningless except in head of rule.~n', []).
internalise_cat_sem(head, value(global, Val), _Result) :-
	(   var(Val) ;
	    \+ is_feat_val_list(Val)
	),
	regulus_error('~NValue of gsem feature can only be a feature/value list.~n', []).
internalise_cat_sem(HeadP, value(ValueType, Val), value(ValueType, Val1)) :-
	!,
	internalise_cat_sem1(HeadP, Val, Val1).
internalise_cat_sem(_HeadP, no_value, no_value) :-
	!.

internalise_cat_sem1(head, Sem, Sem1) :-
	mark_sem_variables(Sem, ref, Sem1).
internalise_cat_sem1(nonhead, Sem, Sem1) :-
	var(Sem),
	!,
	mark_sem_variables(Sem, nonref, Sem1).
internalise_cat_sem1(nonhead, Sem, _Sem1) :-
	is_semantic_component(Sem),
	!,
	regulus_error('~NSemantic variable occurs twice in body of rule.~n', []).
internalise_cat_sem1(nonhead, Sem, NewVar) :-
	instantiate_semantic_variables_as_components(Sem, NewVar).

% REF/NOREF
/*
mark_sem_variables(Sem, ref, Sem1) :-
	var(Sem),
	!,
	Sem1 = ref(Sem).
mark_sem_variables(Sem, nonref, Sem1) :-
	var(Sem),
	!,
	Sem1 = nonref(Sem).
*/
mark_sem_variables(Sem, _Type, Sem) :-
	var(Sem),
	!.
mark_sem_variables(Sem, _Type, Sem) :-
	atomic(Sem).
mark_sem_variables(F=V, Type, F=V1) :-
	!,
	mark_sem_variables(V, Type, V1).
mark_sem_variables([], _Type, []) :-
	!.
mark_sem_variables([F | R], Type, [F1 | R1]) :-
	!,
	mark_sem_variables(F, Type, F1),
	mark_sem_variables(R, Type, R1).
%mark_sem_variables((F, R), ref, (F1, R1)) :-
%	!,
%	mark_sem_variables(F, Type, F1),
%	mark_sem_variables(R, Type, R1).
%mark_sem_variables((_F, _R), nonref, _) :-
%	regulus_error('~NList in body of rule not allowed yet~n', []).
mark_sem_variables(FuncExpr, ref, FuncExpr1) :-
	functor(FuncExpr, F, N),
	gsl_function(F/N),
	!,
	functor(FuncExpr1, F, N),
	mark_sem_variables_args(N, FuncExpr, ref, FuncExpr1).
mark_sem_variables(FuncExpr, nonref, _FuncExpr1) :-
	functor(FuncExpr, F, N),
	gsl_function(F/N),
	!,
	regulus_error('~NNot meaningful to use GSL function ~w in body of rule~n', []).
mark_sem_variables(Other, Type, Other1) :-
	regulus_error('~NBad call: ~w~n', [mark_sem_variables(Other, Type, Other1)]).

mark_sem_variables_args(0, _FuncExpr, _Type, _FuncExpr1).
mark_sem_variables_args(I, FuncExpr, Type, FuncExpr1) :-
	I > 0,
	arg(I, FuncExpr, Arg),
	arg(I, FuncExpr1, Arg1),
	mark_sem_variables(Arg, Type, Arg1),
	I1 is I - 1,
	!,
	mark_sem_variables_args(I1, FuncExpr, Type, FuncExpr1).

instantiate_semantic_variables_as_components((F = V), StructureOver) :-
	var(V),
	!,
	V = StructureOver+F.
instantiate_semantic_variables_as_components((_F = Atom), _StructureOver) :-
	atom(Atom),
	!,
	regulus_error('~NSemantic variable assigned value in body of rule.~n', []).
instantiate_semantic_variables_as_components((_F = V), _StructureOver) :-
	is_semantic_component(V),
	!,
	regulus_error('~NSemantic variable occurs twice in body of rule.~n', []).
instantiate_semantic_variables_as_components(Atom, _StructureOver) :-	
	atomic(Atom),
	!.
instantiate_semantic_variables_as_components([], _StructureOver) :-	
	!.
instantiate_semantic_variables_as_components([First | Rest], StructureOver) :-
	!,
	instantiate_semantic_variables_as_components(First, StructureOver),
	instantiate_semantic_variables_as_components(Rest, StructureOver).
instantiate_semantic_variables_as_components(A, B) :-
	!,
	regulus_error('~NBad call: ~w.~n', [instantiate_semantic_variables_as_components(A, B)]).

%---------------------------------------------------------------

% instantiate_syntactic_features(+Rule, -InstantiatedRule, +FeatureGroup)

% We need two arguments (rather than a single argument which we instantiate,
% as the name suggests), to be able to deal with Boolean variables in 
% feature specs. So for example we might "instantiate" foo=(a\/b) to foo=a.

instantiate_syntactic_features((P --> Q), (P1 --> Q1), FeatureGroup) :-
	!,
	instantiate_syntactic_features(P, P1, FeatureGroup),
	instantiate_syntactic_features(Q, Q1, FeatureGroup).
instantiate_syntactic_features((P, Q), (P1, Q1), FeatureGroup) :-
	!,
	instantiate_syntactic_features(P, P1, FeatureGroup),
	instantiate_syntactic_features(Q, Q1, FeatureGroup).
instantiate_syntactic_features(?P, ?P1, FeatureGroup) :-
	!,
	instantiate_syntactic_features(P, P1, FeatureGroup).
instantiate_syntactic_features(Atom, Atom, _FeatureGroup) :-
	atomic(Atom),
	!.
instantiate_syntactic_features(cat(CatName, SynFeatsWithVals, Sem), cat(InstantiatedCatName, UnexpandedSynFeatsWithVals, Sem), FeatureGroup) :-
	!,
	instantiate_syntactic_features_in_list(SynFeatsWithVals, ExpandedSynFeatsWithVals, UnexpandedSynFeatsWithVals, FeatureGroup),
	instantiated_cat_name(CatName, ExpandedSynFeatsWithVals, InstantiatedCatName).
instantiate_syntactic_features(Other, Other1, FeatureGroup) :-
	regulus_error('~NBad call: ~q~n', [instantiate_syntactic_features(Other, Other1, FeatureGroup)]).

% instantiate_syntactic_features_in_list(SynFeatsWithVals, ExpandedSynFeatsWithVals, UnexpandedSynFeatsWithVals, FeatureGroup)

instantiate_syntactic_features_in_list(SynFeatsWithVals, ExpandedSynFeatsWithVals, UnexpandedSynFeatsWithVals, FeatureGroup) :-
	separate_syn_feats_with_vals(SynFeatsWithVals, SynFeatsWithValsToExpand, UnexpandedSynFeatsWithVals, FeatureGroup),
	instantiate_syntactic_features_in_list1(SynFeatsWithValsToExpand, ExpandedSynFeatsWithVals, '*dummy*', '*dummy*').

% separate_syn_feats_with_vals(+SynFeatsWithVals, -SynFeatsWithValsToExpand, -UnexpandedSynFeatsWithVals, +FeatureGroup)

separate_syn_feats_with_vals([], [], [], _FeatureGroup).
separate_syn_feats_with_vals([F | R], [F | R1], R2, FeatureGroup) :-
	F = (Feat = _Val),
	member(Feat, FeatureGroup),
	!,
	separate_syn_feats_with_vals(R, R1, R2, FeatureGroup).
separate_syn_feats_with_vals([F | R], R1, [F | R2], FeatureGroup) :-
	separate_syn_feats_with_vals(R, R1, R2, FeatureGroup).

instantiate_syntactic_features_in_list1([], [], _LastFeat, _LastVal).
instantiate_syntactic_features_in_list1([(Feat = Val) | R], R1, LastFeat, LastVal) :-
	Feat = LastFeat,
	!,
	possible_value_of_feature_consistent_with_spec(Feat, Val, LastVal),
	(   var(Val) ->
	    Val = LastVal ;
	    true
	),
	instantiate_syntactic_features_in_list1(R, R1, Feat, LastVal).
instantiate_syntactic_features_in_list1([(Feat = Val) | R], [(Feat = Val1) | R1], _LastFeat, _LastVal) :-
	possible_value_of_feature_consistent_with_spec(Feat, Val, Val1),
	(   var(Val) ->
	    Val = Val1 ;
	    true
	),
	instantiate_syntactic_features_in_list1(R, R1, Feat, Val1).

%---------------------------------------------------------------

extract_grouped_rules_from_ungrounded_rules_database(GroupedRules) :-
	%findall(Cat-Rule, ( cat_and_rule_in_ungrounded_rules_database(Cat, Rule), make_ground(Rule) ), Pairs),
	findall(Cat-Rule, cat_and_rule_in_ungrounded_rules_database(Cat, Rule), Pairs),
	keysort(Pairs, SortedTaggedPairs),
	group_sorted_tagged_rules(SortedTaggedPairs, GroupedRules0, _FirstHeadCat, []),
	sort_tagged_rules_by_item_number(GroupedRules0, GroupedRules).

/*
extract_grouped_rules_from_grounded_rules_database(GroupedRules) :-
	findall(Cat-Rule, grounded_rule(_RuleID, Cat, Rule), Pairs),
	keysort(Pairs, SortedTaggedPairs),
	group_sorted_tagged_rules(SortedTaggedPairs, GroupedRules0, _FirstHeadCat, []),
	sort_tagged_rules_by_item_number(GroupedRules0, GroupedRules).
*/

/*
group_regulus_rules(RulesIn, GroupedRules) :-
	findall(
	HeadCat-Rule, 
	( member(Rule, RulesIn), head_cat_and_line_info_for_rule(Rule, HeadCat, _LineInfo) ), 
	TaggedRules
    ),
	keysort(TaggedRules, SortedTaggedRules),
	group_sorted_tagged_rules(SortedTaggedRules, GroupedRules0, _FirstHeadCat, []),
	sort_tagged_rules_by_item_number(GroupedRules0, GroupedRules).
*/

% group_sorted_tagged_rules(+SortedTaggedRules, -GroupedRules, +CurrentHeadCat, +CurrentGroup)

group_sorted_tagged_rules([], [], _CurrentHeadCat, []) :-
	!.
group_sorted_tagged_rules([], [CurrentHeadCat-CurrentGroup], CurrentHeadCat, CurrentGroup) :-
	!.
group_sorted_tagged_rules([HeadCat-Rule | R], R1, CurrentHeadCat, CurrentGroup) :-
	HeadCat = CurrentHeadCat,
	!,
	group_sorted_tagged_rules(R, R1, HeadCat, [Rule | CurrentGroup]).
group_sorted_tagged_rules([HeadCat-Rule | R], [CurrentHeadCat-CurrentGroup | R1], CurrentHeadCat, CurrentGroup) :-
	HeadCat \== CurrentHeadCat,
	!,
	group_sorted_tagged_rules(R, R1, HeadCat, [Rule]).

% sort_tagged_rules_by_item_number(GroupedRules, SortedGroupedRules).

sort_tagged_rules_by_item_number(GroupedRules, SortedGroupedRules) :-
	findall(
	EarliestItemNumber-(HeadCat-Rules),
	( member(HeadCat-Rules, GroupedRules), earliest_item_number_in_rule_list(Rules, EarliestItemNumber) ),
	Pairs
    ),
	keysort(Pairs, SortedPairs),
	unkey_list(SortedPairs, SortedGroupedRules).

earliest_item_number_in_rule_list([], undefined).
earliest_item_number_in_rule_list([F | R], ItemNumber) :-
	earliest_item_number_in_rule_list(R, RItemNumber),
	head_cat_and_line_info_for_rule(F, _HeadCat, LineInfo),
	LineInfo = line_info(ItemNumberOrFreq, _FromLine-_ToLine, _File),
	(   RItemNumber = undefined, number(ItemNumberOrFreq)  ->
	    ItemNumber = ItemNumberOrFreq
	;
	    RItemNumber = undefined, ItemNumberOrFreq = frequency(Freq) ->
	    ItemNumber = Freq
	;
	    number(ItemNumberOrFreq) ->
	    ItemNumber is min(ItemNumberOrFreq, RItemNumber)
	;
	    ItemNumberOrFreq = frequency(Freq) ->
	    ItemNumber is max(Freq, RItemNumber)
	), 
	!.
earliest_item_number_in_rule_list(X, Y) :-
	regulus_error('~NBad call: ~w~n', [earliest_item_number_in_rule_list(X, Y)]).

%---------------------------------------------------------------

% filter_regulus_rules(+RuleIDsIn, -RuleIDsOut, -Summary)

filter_regulus_rules(RuleIDsIn, RuleIDsOut, Summary) :-
	safe_trimcore,
	format('~N   -- Start filtering...~n~n', []), flush_output(user),
	stats_for_rule_ids(RuleIDsIn, NRulesIn),
	
	format('~N   -- Removing duplicate rules: ~d rules... ', [NRulesIn]), flush_output(user),
	remove_duplicate_regulus_rules(RuleIDsIn, RuleIDsNoDuplicates, RemoveDuplicatesTime),
	RemoveDuplicatesTimePerRule is 1000 * RemoveDuplicatesTime / NRulesIn,
	format('~1f secs (~2f msecs/rule)~n~n', [RemoveDuplicatesTime, RemoveDuplicatesTimePerRule]), flush_output(user),
	!,

	safe_trimcore,
	stats_for_rule_ids(RuleIDsNoDuplicates, NRulesNoDuplicates),

	format('~N   -- Filtering bottom-up: ~d rules... ', [NRulesNoDuplicates]), flush_output(user),
	bup_filter_regulus_rules(RuleIDsNoDuplicates, RuleIDsBUPFiltered, NBUPCycles, BUPFilteringTime),
	BUPFilteringTimePerRule is 1000 * BUPFilteringTime / NRulesNoDuplicates,
	format('~d cycles, ~1f secs (~2f msecs/rule)~n~n', [NBUPCycles, BUPFilteringTime, BUPFilteringTimePerRule]), flush_output(user),
	!,

	safe_trimcore,
	stats_for_rule_ids(RuleIDsBUPFiltered, NRulesBUPFiltered),

	format('~N   -- Filtering top-down: ~d rules... ', [NRulesBUPFiltered]), flush_output(user),
	tdown_filter_regulus_rules(RuleIDsBUPFiltered, RuleIDsOut, TDownFilteringTime),
	TDownFilteringTimePerRule is 1000 * TDownFilteringTime / NRulesBUPFiltered,
	format('~1f secs (~2f msecs/rule)~n~n', [TDownFilteringTime, TDownFilteringTimePerRule]), flush_output(user),
	!,

	stats_for_rule_ids(RuleIDsOut, NRulesOut),

	Summary = [rules_after_expansion=NRulesIn,
		   rules_after_filtering=NRulesOut],

	format('~N   -- Filtering complete: ~d rules~n~n', [NRulesOut]),
	!.


%---------------------------------------------------------------

% remove_duplicate_regulus_rules(+RuleIDsIn, -RuleIDsOut)

remove_duplicate_regulus_rules(RuleIDsIn, RuleIDsOut, TimeTaken) :-
	timed_call(remove_duplicate_regulus_rules1(RuleIDsIn, RuleIDsOut), TimeTaken),
	!.
remove_duplicate_regulus_rules(_RuleIDsIn, _RuleIDsOut, _TimeTaken) :-
	regulus_error('~NInternal error: call to remove_duplicate_regulus_rules failed~n', []).

remove_duplicate_regulus_rules1(RuleIDsIn, RuleIDsOut) :-
	break_up_rule_ids_into_groups(RuleIDsIn, GroupedRulesIDs),
	remove_duplicates_from_grouped_rule_ids(GroupedRulesIDs, GroupedFilteredRuleIDs),
	append_list(GroupedFilteredRuleIDs, RuleIDsOut).

break_up_rule_ids_into_groups(RuleIds, GroupedRuleIds) :-
	findall(HeadCat-RuleId, ( member(RuleId, RuleIds), grounded_rule(RuleId, HeadCat, _Rule) ), Pairs),
	keysort(Pairs, SortedPairs),
	break_up_rule_ids_into_groups1(SortedPairs, GroupedRuleIds, _FirstHeadCat, []),
	!.

break_up_rule_ids_into_groups1([], [Group], _CurrentHeadCat, Group) :-
	!.
break_up_rule_ids_into_groups1([HeadCat-F | R], GroupedIds, CurrentHeadCat, CurrentGroup) :-
	HeadCat = CurrentHeadCat,
	!,
	break_up_rule_ids_into_groups1(R, GroupedIds, CurrentHeadCat, [F | CurrentGroup]).
break_up_rule_ids_into_groups1([HeadCat-F | R], [CurrentGroup | RestGroupedIds], _CurrentHeadCat, CurrentGroup) :-
	!,
	break_up_rule_ids_into_groups1(R, RestGroupedIds, HeadCat, [F]).		

remove_duplicates_from_grouped_rule_ids([], []).
remove_duplicates_from_grouped_rule_ids([F | R], [F1 | R1]) :-
	remove_duplicate_regulus_rule_ids_from_group(F, F1),
	remove_duplicates_from_grouped_rule_ids(R, R1).

remove_duplicate_regulus_rule_ids_from_group(Group, Group1) :-
	findall(Rule-RuleId, ( member(RuleId, Group), grounded_rule(RuleId, _HeadCat, Rule) ), Pairs),
	sort(Pairs, SortedPairs),
	remove_duplicate_regulus_rule_ids_from_group1(SortedPairs, Group1, '*dummy*').

remove_duplicate_regulus_rule_ids_from_group1([], [], _LastRule).
remove_duplicate_regulus_rule_ids_from_group1([Rule-_RuleId | R], R1, LastRule) :-
	same_rule_modulo_line_info(Rule, LastRule),
	!,
	remove_duplicate_regulus_rule_ids_from_group1(R, R1, LastRule).
remove_duplicate_regulus_rule_ids_from_group1([Rule-RuleId | R], [RuleId | R1], _LastRule) :-
	remove_duplicate_regulus_rule_ids_from_group1(R, R1, Rule).

same_rule_modulo_line_info(rule(R, _LineInfo1), rule(R1, _LineInfo2)) :-
	variant(R, R1).

%---------------------------------------------------------------

bup_filter_regulus_rules(RuleIDsIn, RuleIDsOut, NCycles, TimeTaken) :-
	timed_call(bup_filter_regulus_rules_top(RuleIDsIn, RuleIDsOut, NCycles), TimeTaken).

bup_filter_regulus_rules_top(RuleIDsIn, RuleIDsOut, 0) :-
	regulus_switch(bup_filtering, off),
	!,
	RuleIDsIn = RuleIDsOut.
bup_filter_regulus_rules_top(RuleIDsIn, RuleIDsOut, NCycles) :-
	linear_bup_filter_regulus_rules(RuleIDsIn, RuleIDsOut, NCycles).

%---------------------------------------------------------------

% linear_bup_filter_regulus_rules(+RuleIDsIn, -RuleIDsOut, -NCycles)
% 
% The basic idea of the algorithm is to find the set of all CFG
% categories which have support, in the following obvious sense. A
% category C has support if either i) there is a rule in which C is
% mother and all the daughters are terminals, or ii) there is a rule
% in which C is mother, and all nonterminal daughters have support.
% The algorithm just uses this definition to compute the set of categories 
% that have support. [NB: Assume that the grammar has been transformed 
% into Chomsky Normal Form, so each rule has at most two daughters].
% 
% We build a table called RulesByDaughters that indexes the
% rules by the daughter categories. So if C is a category,
% RulesByDaughters(C) returns the set of rules in which C is a daughter.
% This means that rules will be indexed as many times as they have
% daughters. If R is the number of rules, RulesByDaughters can evidently
% be built in O(R) time. We also build a table RuleSupport
% which associates each rule with the number of unsupported daughters
% (initially the number of distinct daughter non-terminals), and
% another table SupportedCategory which associates each
% category C with a boolean (initially false). Evidently,
% RuleSupport and SupportedCategory can also be built in O(R)
% time. We extract the set of all categories C such that C is mother of
% a rule with RuleSupport(R) = 0. We assign the variable CurrentCategories to
% this set.
% 
% We now start the main loop. On each pass through the loop, we do
% the following:
% 
% 1. Set SupportedCategory(C) = true for all C in CurrentCategories
%
% 2. Use RulesByDaughters to find the set of all rules R such that
% C' is the mother of a rule R with a daughter in CurrentCategories. 
% Call this set TmpRules.
% 
% 3. Set RuleSupport(R) = RuleSupport(R) - 1 for each of the TmpRules .
% 
% 4. Set CurrentCategories to the set of categories C such that 
%    i)   C is mother of a rule R in TmpRules 
%    ii)  RuleSupport(R) is now equal to zero.
%    iii) SupportedCategory(C) = false
%
% We iterate until we reach a fixed-point.

linear_bup_filter_regulus_rules(RulesIDsIn, RulesIDsOut, NCycles) :-
	safe_trimcore,
	init_bup_filter_regulus_rules(RulesIDsIn, SupportedCatsAS, DaughterMotherAS, RuleSupportAS, SupportedCatsOS),
	regulus_debug_call(1, inform_about_statistics('Init')),
	!,

	safe_trimcore,
        (   regulus_switch(filtering_language,cpp) -> % use findsupport.exe (C++ code)
	    bup_propagate_unsupported_cats_in_cpp(SupportedCatsOS, DaughterMotherAS, SupportedCatsAS, RuleSupportAS, FinalRuleSupportAS, NCycles);
	    bup_propagate_unsupported_cats(SupportedCatsOS, DaughterMotherAS, SupportedCatsAS, RuleSupportAS, _FinalSupportedCatsAS, FinalRuleSupportAS, 0-NCycles)
	),
	regulus_debug_call(1, inform_about_statistics('Propagate')),
	!,
	%assoc_generic_to_list(FinalRuleSupportAS, FRSList),
        %list_to_prolog_file(FRSList,'out_frs.txt'),
        %abort,
	safe_trimcore,
        format(' Removing unsupported rules...~n',[]),
	remove_unsupported_rules(RulesIDsIn, FinalRuleSupportAS, RulesIDsOut),
	regulus_debug_call(1, inform_about_statistics('Remove')).

%---------------------------------------------------------------

% remove_unsupported_rules(+RuleIDsIn, +RuleSupportAS, -RuleIDsOut)

remove_unsupported_rules([], _RuleSupportAS, []).
remove_unsupported_rules([RuleID | Rest], RuleSupportAS, RuleIDsOut) :-
	(   ( get_assoc_generic(RuleID, RuleSupportAS, Support), Support =< 0 ) ->
	    RuleIDsOut = [RuleID | RestOut] ;
	    RuleIDsOut = RestOut 
	),
	!,
	remove_unsupported_rules(Rest, RuleSupportAS, RestOut).

%---------------------------------------------------------------

init_bup_filter_regulus_rules(RuleIDs, SupportedCatsAS, DaughterMotherAS, RuleSupportAS, SupportedCatsOS) :-

	safe_trimcore,
	regulus_debug_call(1, inform_about_statistics('Mothers')),
	rule_ids_to_mother_categories(RuleIDs, MotherCatsOS),
	!,

	safe_trimcore,
	regulus_debug_call(1, inform_about_statistics('Daughters')),
	rule_ids_to_daughter_categories(RuleIDs, DaughterCatsOS),
	!,

	safe_trimcore,
	regulus_debug_call(1, inform_about_statistics('Union')),
	ord_union(MotherCatsOS, DaughterCatsOS, AllCatsOS),

	safe_trimcore,
	regulus_debug_call(1, inform_about_statistics('Supported Category')),
	cats_to_supported_cats_assoc_generic(AllCatsOS, SupportedCatsAS),

	safe_trimcore,
	regulus_debug_call(1, inform_about_statistics('Daughter/mother')),
	rule_ids_to_daughter_mother_assoc_generic(RuleIDs, DaughterMotherAS),
	!,

	safe_trimcore,
	regulus_debug_call(1, inform_about_statistics('Rule Support')),
	rule_ids_to_rule_support_assoc_generic(RuleIDs, RuleSupportAS),
	!,

	safe_trimcore,
	regulus_debug_call(1, inform_about_statistics('Rule Support')),
	rule_ids_to_supported_categories(RuleIDs, SupportedCatsOS),

	!.

rule_ids_to_mother_categories(RuleIDs, MotherCatsOS) :-
	findall(MotherCat, ( member(RuleID, RuleIDs), grounded_rule(RuleID, MotherCat, _Rule) ), MotherCats),
	list_to_ord_set(MotherCats, MotherCatsOS).

rule_ids_to_daughter_categories(RuleIDs, DaughterCatsOS) :-
	findall(Cat, daughter_cat_in_rule_ids(RuleIDs, Cat), Cats),
	list_to_ord_set(Cats, DaughterCatsOS).

daughter_cat_in_rule_ids(RuleIDs, Cat) :-
	member(RuleID, RuleIDs),
	grounded_rule(RuleID, _MotherCat, Rule),
	Rule = rule(RuleBody, _LineInfo),
	cat_in_body_of_expanded_rule(RuleBody, Cat).	

cats_to_supported_cats_assoc_generic(AllCatsOS, SupportedCatsAS) :-
	findall(Cat-false, member(Cat, AllCatsOS), List),
	list_to_assoc_generic(List, SupportedCatsAS).

rule_ids_to_daughter_mother_assoc_generic(RuleIDs, DaughterMotherAS) :-
	findall(
  	  Daughter-[Mother, RuleID], 
	  rule_id_mother_daughter_in_rule_ids(RuleID, Mother, Daughter, RuleIDs),
	  List),
	sort(List, SortedList), % we do want to remove duplicates
	group_sorted_tagged_rules(SortedList, GroupedSortedList, _FirstDaughterCat, []),
	list_to_assoc_generic(GroupedSortedList, DaughterMotherAS).

rule_id_mother_daughter_in_rule_ids(RuleID, MotherCatName, DaughterCatName, RuleIDs) :-
	member(RuleID, RuleIDs),
	grounded_rule(RuleID, MotherCatName, Rule),
	Rule = rule(RuleBody, _LineInfo),
	cat_in_body_of_expanded_rule(RuleBody, DaughterCatName).

rule_ids_to_rule_support_assoc_generic(RuleIDs, RuleSupportAS) :-
	findall(RuleID-Support, rule_id_with_support_in_grouped_rules(RuleIDs, RuleID, Support), List),
	list_to_assoc_generic(List, RuleSupportAS).

rule_id_with_support_in_grouped_rules(RuleIDs, RuleID, Support) :-
	member(RuleID, RuleIDs),
	grounded_rule(RuleID, _MotherCat, Rule),
	Rule = rule(RuleBody, _LineInfo),
	findall(DaughterCatName, cat_in_body_of_expanded_rule(RuleBody, DaughterCatName), DaughterCats0),
	safe_remove_duplicates(DaughterCats0, DaughterCats),
	length(DaughterCats, Support).

rule_ids_to_supported_categories(RuleIDs, SupportedCatsOS) :-
	findall(Cat, cat_with_lexical_rule_in_grouped_rules(RuleIDs, Cat), List),
	list_to_ord_set(List, SupportedCatsOS).

cat_with_lexical_rule_in_grouped_rules(RuleIDs, MotherCatName) :-
	member(RuleID, RuleIDs),
	grounded_rule(RuleID, MotherCatName, Rule),
	Rule = rule(RuleBody, _LineInfo),
	\+ cat_in_body_of_expanded_rule(RuleBody, _DaughterCatName).	

%---------------------------------------------------------------

% bup_propagate_unsupported_cats(SupportedCatsOS, DaughterMotherAS, SupportedCatsASIn, RuleSupportASIn, SupportedCatsASOut, RuleSupportASOut, NIn-NOut)

bup_propagate_unsupported_cats(SupportedCatsOS, _DaughterMotherAS, SupportedCatsASIn, RuleSupportASIn, SupportedCatsASOut, RuleSupportASOut, NIn-NOut) :-
	SupportedCatsOS = [],
	!,
	SupportedCatsASIn = SupportedCatsASOut,
	RuleSupportASIn = RuleSupportASOut,
	NIn = NOut.

bup_propagate_unsupported_cats(SupportedCatsIn, DaughterMotherAS, SupportedCatsASIn0, RuleSupportASIn, SupportedCatsASOut, RuleSupportASOut, NIn-NOut) :-
	add_cats_to_supported_cats(SupportedCatsIn, SupportedCatsASIn0, SupportedCatsASIn),
	bup_propagate_unsupported_cats1(SupportedCatsIn, DaughterMotherAS, SupportedCatsASIn, RuleSupportASIn, SupportedCatsASNext, RuleSupportASNext, []-SupportedCatsNext),
	NNext is NIn + 1,
	%format('~NStart new cycle~n', []),
	!,
	bup_propagate_unsupported_cats(SupportedCatsNext, DaughterMotherAS, SupportedCatsASNext, RuleSupportASNext, SupportedCatsASOut, RuleSupportASOut, NNext-NOut).
	
bup_propagate_unsupported_cats1([], _DaughterMotherAS, SupportedCatsASIn, RuleSupportASIn, SupportedCatsASOut, RuleSupportASOut, NewSupportedCatsIn-NewSupportedCatsOut) :-
	SupportedCatsASIn = SupportedCatsASOut,
	RuleSupportASIn = RuleSupportASOut,
	NewSupportedCatsIn = NewSupportedCatsOut,
	!.
bup_propagate_unsupported_cats1([Cat | Cats], DaughterMotherAS, SupportedCatsASIn, RuleSupportASIn, SupportedCatsASOut, RuleSupportASOut, NewSupportedCatsIn-NewSupportedCatsOut) :-
	bup_propagate_unsupported_single_cat(Cat, DaughterMotherAS, SupportedCatsASIn, RuleSupportASIn, SupportedCatsASNext, RuleSupportASNext, NewSupportedCatsIn-NewSupportedCatsNext),
	!,
	bup_propagate_unsupported_cats1(Cats, DaughterMotherAS, SupportedCatsASNext, RuleSupportASNext, SupportedCatsASOut, RuleSupportASOut, NewSupportedCatsNext-NewSupportedCatsOut).
   
bup_propagate_unsupported_single_cat(Cat, DaughterMotherAS, SupportedCatsASIn, RuleSupportASIn, SupportedCatsASOut, RuleSupportASOut, NewSupportedCatsIn-NewSupportedCatsOut) :-
	%format('~NPropagating from ~w~n', [Cat]), 
	get_assoc_generic(Cat, DaughterMotherAS, MotherAndRuleList),
	!,
	bup_propagate_mother_and_rule_list(MotherAndRuleList, SupportedCatsASIn, RuleSupportASIn, SupportedCatsASOut, RuleSupportASOut, NewSupportedCatsIn-NewSupportedCatsOut).
bup_propagate_unsupported_single_cat(_Cat, _DaughterMotherAS, SupportedCatsASIn, RuleSupportASIn, SupportedCatsASOut, RuleSupportASOut, NewSupportedCatsIn-NewSupportedCatsOut) :-
	SupportedCatsASIn = SupportedCatsASOut,
	RuleSupportASIn = RuleSupportASOut,
	NewSupportedCatsIn = NewSupportedCatsOut.

% bup_propagate_mother_and_rule_list(+MotherAndRuleList, +SupportedCatsASIn, +RuleSupportASIn, -SupportedCatsASOut, -RuleSupportASOut, +NewSupportedCatsIn-(-NewSupportedCatsOut)).

bup_propagate_mother_and_rule_list([], SupportedCatsASIn, RuleSupportASIn, SupportedCatsASIn, RuleSupportASIn, NewSupportedCatsIn-NewSupportedCatsIn).
bup_propagate_mother_and_rule_list([F | R], SupportedCatsASIn, RuleSupportASIn, SupportedCatsASOut, RuleSupportASOut, NewSupportedCatsIn-NewSupportedCatsOut) :-
	bup_propagate_mother_and_rule(F, SupportedCatsASIn, RuleSupportASIn, SupportedCatsASNext, RuleSupportASNext, NewSupportedCatsIn-NewSupportedCatsNext),
	!,
	bup_propagate_mother_and_rule_list(R, SupportedCatsASNext, RuleSupportASNext, SupportedCatsASOut, RuleSupportASOut, NewSupportedCatsNext-NewSupportedCatsOut).

/*
bup_propagate_mother_and_rule([Mother, _Rule], SupportedCatsASIn, RuleSupportASIn, SupportedCatsASOut, RuleSupportASOut, NewSupportedCatsIn-NewSupportedCatsOut) :-
	get_assoc_generic(Mother, SupportedCatsASIn, true),
	!,
	SupportedCatsASIn = SupportedCatsASOut,
	RuleSupportASIn = RuleSupportASOut,
	NewSupportedCatsIn = NewSupportedCatsOut.
*/
bup_propagate_mother_and_rule([Mother, Rule], SupportedCatsASIn, RuleSupportASIn, SupportedCatsASOut, RuleSupportASOut, NewSupportedCatsIn-NewSupportedCatsOut) :-
	get_assoc_generic(Rule, RuleSupportASIn, SupportForRuleIn),
	SupportForRuleOut is SupportForRuleIn - 1,
	%format('~NSupport for ~w = ~d~n', [Mother, SupportForRuleOut]), 
	put_assoc_generic(Rule, RuleSupportASIn, SupportForRuleOut, RuleSupportASOut),
	(   ( SupportForRuleOut = 0, get_assoc_generic(Mother, SupportedCatsASIn, false) ) ->
	    
	    NewSupportedCatsOut = [Mother | NewSupportedCatsIn],
	    put_assoc_generic(Mother, SupportedCatsASIn, true, SupportedCatsASOut) ;

	    SupportedCatsASIn = SupportedCatsASOut,
	    NewSupportedCatsOut = NewSupportedCatsIn
	).	

% add_cats_to_supported_cats(+SupportedCatsIn, +SupportedCatsASIn, -SupportedCatsASOut)

add_cats_to_supported_cats([], SupportedCatsASIn, SupportedCatsASIn).
add_cats_to_supported_cats([F | R], SupportedCatsASIn, SupportedCatsASOut) :-
	put_assoc_generic(F, SupportedCatsASIn, true, SupportedCatsASNext),
	add_cats_to_supported_cats(R, SupportedCatsASNext, SupportedCatsASOut).

%---------------------------------------------------------------

% In the following, exploit the fact that top level categories
% can't have syntactic features, and hence stay the same even after
% feature expansion.

tdown_filter_regulus_rules(RuleIDsIn, RuleIDsOut, TimeTaken) :-
	timed_call(tdown_filter_main(RuleIDsIn, RuleIDsOut), TimeTaken).

tdown_filter_main(RuleIDsIn, RuleIDsOut) :-
	regulus_switch(tdown_filtering, off),
	!,
	RuleIDsIn = RuleIDsOut.
tdown_filter_main(RuleIDsIn, RuleIDsOut) :-
	rule_ids_to_mother_daughters_assoc_generic(RuleIDsIn, MotherDaughtersAS),
	top_level_category_tags_for_rule_ids(RuleIDsIn, RelevantTopLevelCatList),
	(   RelevantTopLevelCatList = [] ->
	    regulus_error('~NNo top-level categories left at start of top-down filtering phase.\nProbably all removed by bottom-up filtering.~n', []) ;
	    tdown_filter1(RelevantTopLevelCatList, MotherDaughtersAS, RuleIDsIn, RuleIDsOut)
	).

rule_ids_to_mother_daughters_assoc_generic(RuleIDs, MotherDaughtersAS) :-
	findall(
  	  Mother-Daughter, 
	  rule_id_mother_daughter_in_rule_ids(_RuleID, Mother, Daughter, RuleIDs),
	  List),
	sort(List, SortedList), % we do want to remove duplicates
	group_sorted_tagged_rules(SortedList, GroupedSortedList, _FirstMotherCat, []),
	list_to_assoc_generic(GroupedSortedList, MotherDaughtersAS).

top_level_category_tags_for_rule_ids(RuleIDsIn, RelevantTopLevelCatList) :-
	findall(
	TopLevelCat, 
	( 
	    member(RuleID, RuleIDsIn),
	    grounded_rule(RuleID, TopLevelCat, _Rule),
	    top_level_category(TopLevelCat) 
	), 
	RelevantTopLevelCatList0),
	safe_remove_duplicates(RelevantTopLevelCatList0, RelevantTopLevelCatList).

tdown_filter1(CatsIn, MotherDaughtersAS, RuleIDsIn, RuleIDsOut) :-
	findall(Cat-true, member(Cat, CatsIn), CatsInPairs),
	list_to_assoc_generic(CatsInPairs, ASCatsIn),
	tdown_filter2(CatsIn, ASCatsIn, ASCatsOut, MotherDaughtersAS),
	rule_ids_with_mother_cats_in_list(RuleIDsIn, ASCatsOut, RuleIDsOut).

% tdown_filter2(+NewCats, +ASCatsIn, -ASCatsOut, +MotherDaughtersAS)
%
% NewCats is the list of new cats found by the previous iteration;
% ASCatsIn is the assoc set of cats found already (including NewCats);
% ASCatsOut is the final assoc set of cats;
% MotherDaughtersAS associates each cat C with the set of C' such that there a rule
%    which has C on the left and C' on the right.
%
% We compute ASCatsOut as the transitive closure of ASCatsIn under the relation R
% defined by 
%
%   C1 R C2 <=> C2 appears in a rule of which C1 is the head
%
% (R = directly_reachable_from_cat_in_list)

tdown_filter2([], ASCatsIn, ASCatsIn, _MotherDaughtersAS) :-
	!.
tdown_filter2(NewCatsIn, ASCatsIn, ASCatsOut, MotherDaughtersAS) :-
	tdown_filter3(NewCatsIn, ASCatsIn, []-NewCatsNext, ASCatsNext, MotherDaughtersAS),
	!,
	tdown_filter2(NewCatsNext, ASCatsNext, ASCatsOut, MotherDaughtersAS).

tdown_filter3([], ASCatsOut, NewCatsOut-NewCatsOut, ASCatsOut, _MotherDaughtersAS).
tdown_filter3([F | R], ASCatsIn, NewCatsIn-NewCatsOut, ASCatsOut, MotherDaughtersAS) :-
	(   get_assoc_generic(F, MotherDaughtersAS, Daughters) ->
	    true ;
	    Daughters = []
	),
	tdown_filter4(Daughters, ASCatsIn, NewCatsIn-NewCatsNext, ASCatsNext, MotherDaughtersAS),
	!,
	tdown_filter3(R, ASCatsNext, NewCatsNext-NewCatsOut, ASCatsOut, MotherDaughtersAS).

tdown_filter4([], ASCatsIn, NewCatsIn-NewCatsIn, ASCatsIn, _MotherDaughtersAS).
tdown_filter4([F | R], ASCatsIn, NewCatsIn-NewCatsOut, ASCatsOut, MotherDaughtersAS) :-
	(   get_assoc_generic(F, ASCatsIn, true) ->

	    ASCatsNext = ASCatsIn,
	    NewCatsNext = NewCatsIn ;

	    put_assoc_generic(F, ASCatsIn, true, ASCatsNext),
	    NewCatsNext = [F | NewCatsIn]
	),
	!,
	tdown_filter4(R, ASCatsNext, NewCatsNext-NewCatsOut, ASCatsOut, MotherDaughtersAS).

%---------------------------------------------------------------

% rule_ids_with_mother_cats_in_list(+RuleIDsIn, +ASCatsOut, -RuleIDsOut)

rule_ids_with_mother_cats_in_list([], _ASCatsOut, []) :-
	!.
rule_ids_with_mother_cats_in_list([RuleID | R], ASCats, [RuleID | R1]) :-
	grounded_rule(RuleID, MotherCat, _Rule),
	get_assoc_generic(MotherCat, ASCats, true),
	!,
	rule_ids_with_mother_cats_in_list(R, ASCats, R1).
rule_ids_with_mother_cats_in_list([_F | R], ASCats, R1) :-
	rule_ids_with_mother_cats_in_list(R, ASCats, R1).

%---------------------------------------------------------------

present_expansion_summary(Summary) :-
	format('~N *** SUMMARY FIGURES FOR RULE EXPANSION ***~n', []),
	find_largest_unfiltered_rule_set_size(Summary, LargestExpandedRuleSetSize),
	format('~N~n -- Largest size of unfiltered rule set: ~d~n', [LargestExpandedRuleSetSize]),

	find_largest_filtered_rule_set_size(Summary, LargestFilteredRuleSetSize),
	format('~N~n -- Largest size of filtered rule set: ~d~n', [LargestFilteredRuleSetSize]),

	add_growth_to_expansion_summary(Summary, '*no_last_rule_set_size*', Summary1),
	order_expansion_summary_by_growth(Summary1, Summary2),
	present_growth_figures_for_expansion_summary(Summary2),
	!.
present_expansion_summary(_Summary) :-
	format('~NWarning: unable to present expansion summary~n', []).

find_largest_unfiltered_rule_set_size(Summary, LargestRuleSetSize) :-
	findall(RuleSetSize,
		(   member(Record, Summary),
		    member(rules_after_expansion=RuleSetSize, Record)
		),
		RuleSetSizes),
	sort(RuleSetSizes, RuleSetSizes1),
	reverse(RuleSetSizes1, RuleSetSizes2),
	RuleSetSizes2 = [LargestRuleSetSize | _],
	!.
find_unlargest_unfiltered_rule_set_size(_Summary, _LargestRuleSetSize) :-
	format('~NWarning: unable to find largest unfiltered rule set size~n', []),
	fail.

find_largest_filtered_rule_set_size(Summary, LargestRuleSetSize) :-
	findall(RuleSetSize,
		(   member(Record, Summary),
		    member(rules_after_filtering=RuleSetSize, Record)
		),
		RuleSetSizes),
	sort(RuleSetSizes, RuleSetSizes1),
	reverse(RuleSetSizes1, RuleSetSizes2),
	RuleSetSizes2 = [LargestRuleSetSize | _],
	!.
find_largest_filtered_rule_set_size(_Summary, _LargestRuleSetSize) :-
	format('~NWarning: unable to find largest filtered rule set size~n', []),
	fail.

add_growth_to_expansion_summary([], _LastRuleSetSize, []) :-
	!.
add_growth_to_expansion_summary([F | R], LastRuleSetSize, [F1 | R1]) :-
	add_growth_to_expansion_summary_record(F, LastRuleSetSize, NextRuleSetSize, F1),
	!,
	add_growth_to_expansion_summary(R, NextRuleSetSize, R1).

add_growth_to_expansion_summary_record(Record, LastRuleSetSize, NextRuleSetSize, Record1) :-
	member(rules_after_filtering=NextRuleSetSize, Record),
	(   integer(LastRuleSetSize) ->
	    
	    Growth is NextRuleSetSize / LastRuleSetSize,
	    append(Record, [growth=Growth], Record1) ;

	    Record1 = Record
	),
	!.
add_growth_to_expansion_summary_record(Record, LastRuleSetSize, NextRuleSetSize, Record1) :-
	format('~NWarning: bad call ~w~n',
	       [add_growth_to_expansion_summary_record(Record, LastRuleSetSize, NextRuleSetSize, Record1)]),
	fail.

order_expansion_summary_by_growth(Summary, Summary1) :-
	findall(Growth-Record,
		(   member(Record, Summary),
		    member(growth=Growth, Record)
		),
		Pairs),
	keysort(Pairs, Pairs1),
	reverse(Pairs1, Pairs2),
	unkey_list(Pairs2, Summary1),
	!.
order_expansion_summary_by_growth(Summary, Summary1) :-
	format('~NWarning: bad call ~w~n',
	       [order_expansion_summary_by_growth(Summary, Summary1)]),
	fail.

present_growth_figures_for_expansion_summary(Summary) :-
	format('~N~n -- Feature groups ordered by proportional growth of rule-set:~n', []),
	present_growth_figures_for_expansion_summary1(Summary),
	!.
present_growth_figures_for_expansion_summary(Summary) :-
	format('~NWarning: bad call ~w~n',
	       [present_growth_figures_for_expansion_summary(Summary)]),
	fail.

present_growth_figures_for_expansion_summary1([]).
present_growth_figures_for_expansion_summary1([F | R]) :-
	present_growth_figures_for_expansion_summary_record(F),
	!,
	present_growth_figures_for_expansion_summary1(R).

present_growth_figures_for_expansion_summary_record(Record) :-
	member(features=Feats, Record),
	member(growth=Growth, Record),
	format('~N    ~2f ~10|~w~n', [Growth, Feats]),
	!.
present_growth_figures_for_expansion_summary_record(Record) :-
	format('~NWarning: bad call ~w~n',
	       [present_growth_figures_for_expansion_summary_record(Record)]),
	fail.
	
%---------------------------------------------------------------

present_expanded_rules(RuleIDs) :-
	present_most_expanded_rules(RuleIDs),
	regulus_debug_call(1, dump_expanded_rules_to_file(RuleIDs)).

dump_expanded_rules_to_file(RuleIDs) :-
	absolute_file_name(tmp(regulus_expanded_rules), File),
	format('~NDumping full expanded rules to ~w~n', [File]),
	open(File, write, S),
	dump_expanded_rules_to_file(S, RuleIDs),
	close(S).

dump_expanded_rules_to_file(S, RuleIDs) :-
	member(RuleID, RuleIDs),
	grounded_rule(RuleID, _MotherCat, Rule),
	format(S, '~N~n~q.~n', [Rule]),
	fail.
dump_expanded_rules_to_file(_S, _RuleIDs).

present_most_expanded_rules(RuleIDs) :-
	findall(LineInfo, line_info_in_rule_ids(LineInfo, RuleIDs), LineInfoList),
	list_to_ordered_multiset(LineInfoList, LineInfoMultiset),
	firstn_or_all(LineInfoMultiset, 5, FirstN),
	format('~NMost expanded rules:~n~n', []),
	present_most_expanded_rules1(FirstN),
	format('~n', []),
	!.
present_most_expanded_rules(RuleIDs) :-
	regulus_error('~NBad call: ~w~n', [present_most_expanded_rules(RuleIDs)]).

present_most_expanded_rules1([]).
present_most_expanded_rules1([F | R]) :-
	present_most_expanded_rules2(F),
	present_most_expanded_rules1(R).

present_most_expanded_rules2(Freq-line_info(_ItemNumber, FromLine-ToLine, File)) :-
	format('~N~d ~6|expanded rules: file ~w, lines ~d-~d~n', [Freq, File, FromLine, ToLine]),
	!.
present_most_expanded_rules2(Other) :-
	regulus_error('~NBad call: ~w~n', [present_most_expanded_rules2(Other)]).

line_info_in_rule_ids(LineInfo, RuleIDs) :-
	member(RuleID, RuleIDs),
	grounded_rule(RuleID, _MotherCat, Rule),
	Rule = rule(_RuleBody, LineInfo).

%---------------------------------------------------------------

stats_for_rule_ids(RuleIDs, NRules) :-
	n_rules_in_rule_ids(RuleIDs, NRules).

n_rules_in_rule_ids(RuleIDs, NRules) :-
	length(RuleIDs, NRules).

n_tags_in_rule_ids(RuleIDs, N) :-
	findall(MotherCat, ( member(RuleID, RuleIDs), grounded_rule(RuleID, MotherCat, _Rule) ), MotherCats0),
	safe_remove_duplicates(MotherCats0, MotherCats),
	length(MotherCats, N).

% cat_in_body_of_expanded_rule(+Rule, -Cat)

cat_in_body_of_expanded_rule( ( _H --> B ), Cat) :-
	!,
	cat_in_body_of_expanded_rule(B, Cat).
cat_in_body_of_expanded_rule((P, Q), Cat) :-
	!,
	(   cat_in_body_of_expanded_rule(P, Cat) ;
	    cat_in_body_of_expanded_rule(Q, Cat) 
	).
cat_in_body_of_expanded_rule(?P, Cat) :-
	!,
	cat_in_body_of_expanded_rule(P, Cat).
cat_in_body_of_expanded_rule(cat(Cat, _SynFeatsWithVals, _ReturnValue), Cat).

%---------------------------------------------------------------

instantiated_cat_name(CatName, SynFeatsWithVals, InstantiatedCatName) :-
	top_level_category(CatName),
	SynFeatsWithVals = [],
	!,
	InstantiatedCatName = CatName.
instantiated_cat_name(CatName, _SynFeatsWithVals, _InstantiatedCatName) :-
	top_level_category(CatName),
	!,
	regulus_error('~NTop-level category may not have syntactic features~n', []).
instantiated_cat_name(CatName, SynFeatsWithVals, InstantiatedCatName) :-
	syn_val_pairs_to_val_list(SynFeatsWithVals, Vals),
	flatten(Vals, FlattenedVals),
	cat_and_args_to_nuance_cat([CatName | FlattenedVals], InstantiatedCatName). 

syn_val_pairs_to_val_list([], []).
syn_val_pairs_to_val_list([_F=V | R], [V | R1]) :-
	syn_val_pairs_to_val_list(R, R1).

cat_and_args_to_nuance_cat(List, NuanceCatName) :-
	is_list(List),
	join_with_underscore(List, Atom),
	uppercase_atom(Atom, NuanceCatName),
	!.
cat_and_args_to_nuance_cat(List, NuanceCatName) :-
	regulus_error('~NBad call: ~w~n', [cat_and_args_to_nuance_cat(List, NuanceCatName)]).

is_cat_valued_feature(Feat) :-
	subfeature(Feat, _, _, _).

%---------------------------------------------------------------
/*
% Now taken from PrologLib/utilities.pl

firstn_or_all(List, N, List) :-
	length(List, Len),
	Len =< N,
	!.
firstn_or_all(List, N, FirstN) :-
	length(FirstN, N),
	append(FirstN, _Rest, List).
*/
%---------------------------------------------------------------

inform_about_statistics(Message) :-
	format('~N~n   -- ~w~n', [Message]), statistics, flush_output(user).

% --------------------------------------------------------------

bup_propagate_unsupported_cats_in_cpp(
	SupportedCatsOS, DaughterMotherAS, SupportedCatsAS, 
	RuleSupportAS, FinalRuleSupportAS, NIters) :-

        format('~NEntered bup_propagate_unsupported_cats_in_cpp...~n',[]),

	absolute_file_name(tmp('in_supported_os.txt'), InSupportedOSFile),
	%absolute_file_name(tmp('in_supported_as.txt'), InSupportedAsFile),
	absolute_file_name(tmp('in_rulesupport_as.txt'), InRuleSupportAsFile),
	absolute_file_name(tmp('in_daumother_as.txt'), InDauMotherAsFile),
	absolute_file_name(tmp('out_finalsupport.txt'), OutFinalSupportFile),
	absolute_file_name('$REGULUS/CPP/findsupport.exe', AbsExeFile),

	generate_cat_indices(SupportedCatsAS,CatIndexAS,MaxIndex),
	% Cats supported by all-lexical RHS's. We put the max cat index on the front.
	findall(Ind,(member(Cat,SupportedCatsOS),get_assoc_generic(Cat,CatIndexAS,Ind)), SupportedCatsIndOS),
	%write_atom_list_to_file([MaxIndex|SupportedCatsIndOS],'in_supported_os.txt'),
	write_atom_list_to_file([MaxIndex|SupportedCatsIndOS],InSupportedOSFile),
	% Category -> Supported; all false initially. Don't need to write this out, as we'll
	% just use it to reconvert from indices to cat names afterwards.
	%assoc_generic_to_prolog_file(SupportedCatsAS,'in_supported_as.txt'),
	% RulesByDaughters table: Daugher -> list of [Mother,RuleId] pairs
	%write_daughter_mother_triples(DaughterMotherAS,CatIndexAS,'in_daumother_as.txt'),
	write_daughter_mother_triples(DaughterMotherAS,CatIndexAS,InDauMotherAsFile),
	% RuleId (number) -> number of unsupported cats on its RHS
	%assoc_generic_values_to_prolog_file(RuleSupportAS,'in_rulesupport_as.txt'),
	assoc_generic_values_to_prolog_file(RuleSupportAS,InRuleSupportAsFile),
	%shell('C:/cygwin/home/carter/reg/findsupport.exe in_daumother_as.txt in_rulesupport_as.txt in_supported_os.txt > out_finalsupport.txt'),
	%system_on_list([AbsExeFile, 'in_daumother_as.txt', 'in_rulesupport_as.txt', 'in_supported_os.txt', '>', 'out_finalsupport.txt']),

        format('~NCalling ~w ...~n',[AbsExeFile]),
	system_on_list([AbsExeFile, InDauMotherAsFile, InRuleSupportAsFile, InSupportedOSFile, '>', OutFinalSupportFile]),
	%prolog_file_to_list('out_finalsupport.txt',[n(NIters)|SList]),
        format('~NReturned from ~w ...~n',[AbsExeFile]),
	prolog_file_to_list(OutFinalSupportFile,[n(NIters)|SList]),
	generate_final_rule_support(SList,FinalRuleSupportAS),
        format('~NReturning from bup_propagate_unsupported_cats_in_cpp...~n',[]).

generate_cat_indices(SupportedCatsAS,CatIndexAS,MaxIndex) :-
	empty_assoc_generic(CatIndexAS0),
	min_assoc_generic(SupportedCatsAS,Cat,_),
	generate_cat_indices1(SupportedCatsAS,Cat,0,CatIndexAS0,CatIndexAS,MaxIndex).

generate_cat_indices1(SupportedCatsAS,Cat,Index,CatIndexASIn,CatIndexASOut,MaxIndex) :-
	put_assoc_generic(Cat,CatIndexASIn,Index,CatIndexASMid),
	(   get_next_assoc_generic(Cat,SupportedCatsAS,CatNext,_) ->
	    IndexNext is Index+1,
	    generate_cat_indices1(SupportedCatsAS,CatNext,IndexNext,CatIndexASMid,CatIndexASOut,MaxIndex);
	    CatIndexASOut = CatIndexASMid,
	    MaxIndex = Index
	).

% Write the DaughterMotherAS to File, one line per daughter cat, in the form
% DaughterCatId N MotherCatId1 RuleId1 ... MotherCatIdN RuleIdN
write_daughter_mother_triples(DaughterMotherAS,CatIndexAS,File) :-
	min_assoc_generic(DaughterMotherAS,DCat,MRPairList),
	open(File,write,S),
	write_daughter_mother_triples1(DaughterMotherAS,CatIndexAS,DCat,MRPairList,S),
	close(S).

write_daughter_mother_triples1(DaughterMotherAS,CatIndexAS,DCat,MRPairList,S) :-
	get_assoc_generic(DCat,CatIndexAS,DCatInd),
	length(MRPairList,MRPLen),
	format(S,'~w ~w',[DCatInd,MRPLen]),
	write_daughter_mother_triples2(MRPairList,CatIndexAS,S),
	(   get_next_assoc_generic(DCat,DaughterMotherAS,DCatNext,MRPairListNext) ->
	    write_daughter_mother_triples1(DaughterMotherAS,CatIndexAS,DCatNext,MRPairListNext,S);
	    true
	).

write_daughter_mother_triples2([],_,S) :-
	nl(S).
write_daughter_mother_triples2([[MCat,RuleInd]|MRPairList],CatIndexAS,S) :-
	get_assoc_generic(MCat,CatIndexAS,MCatInd),
	format(S,' ~w ~w',[MCatInd,RuleInd]),
	write_daughter_mother_triples2(MRPairList,CatIndexAS,S).

assoc_generic_values_to_prolog_file(AS,File) :-
	assoc_generic_to_list(AS,List),
	open(File,write,S),
	write_pair_values_to_stream(List,S),
	close(S).

write_pair_values_to_stream([],_).
write_pair_values_to_stream([_Key-Val|List],S) :-
	format(S,'~w~n',[Val]),
	write_pair_values_to_stream(List,S).

generate_final_rule_support(SList,FinalRuleSupportAS) :-
	empty_assoc_generic(RuleSupportAS),
	generate_final_rule_support1(SList,RuleSupportAS,FinalRuleSupportAS).

generate_final_rule_support1([],FinalRuleSupportAS,FinalRuleSupportAS).
generate_final_rule_support1([s(Id,N)|SList],RuleSupportAS,FinalRuleSupportAS) :-
	put_assoc_generic(Id,RuleSupportAS,N,NewRuleSupportAS),
	generate_final_rule_support1(SList,NewRuleSupportAS,FinalRuleSupportAS).






