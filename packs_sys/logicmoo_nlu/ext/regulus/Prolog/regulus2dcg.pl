% regulus2dcg.pl
 
%---------------------------------------------------------------

:- module(regulus2dcg,
	  [regulus2dcg/2,
	   regulus2dcg_dynamic/2,
	   regulus2dcg_dynamic_for_generation/2,
	   regulus2dcg/3,
	   regulus2dcg_dynamic/3,
	   regulus2dcg_dynamic_for_generation/3,
	   regulus2dcg/4,

	   internalise_cat/4,
	   match_cats/2,
	   cat_matches_cat_in_list/2
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/Prolog/regulus_declarations').
:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/regulus_read').
:- use_module('$REGULUS/Prolog/strcat_semantics').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(ordsets)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).

%---------------------------------------------------------------

regulus2dcg(InFile, OutFile) :-
	regulus2dcg(InFile, OutFile, static, _SucceededOrFailed).

regulus2dcg_dynamic(InFile, OutFile) :-
	regulus2dcg(InFile, OutFile, dynamic(normal), _SucceededOrFailed).

regulus2dcg_dynamic_for_generation(InFile, OutFile) :-
	regulus2dcg(InFile, OutFile, dynamic(generation), _SucceededOrFailed).

regulus2dcg(InFile, OutFile, SucceededOrFailed) :-
	regulus2dcg(InFile, OutFile, static, SucceededOrFailed).

regulus2dcg_dynamic(InFile, OutFile, SucceededOrFailed) :-
	regulus2dcg(InFile, OutFile, dynamic(normal), SucceededOrFailed).

regulus2dcg_dynamic_for_generation(InFile, OutFile, SucceededOrFailed) :-
	regulus2dcg(InFile, OutFile, dynamic(generation), SucceededOrFailed).

regulus2dcg(InFile, OutFile, DCGType, SucceededOrFailed) :-
	timed_call(regulus2dcg1(InFile, OutFile, DCGType, SucceededOrFailed), TimeTaken),
	format('~NCompilation finished, ~2f secs~n', [TimeTaken]).

%---------------------------------------------------------------

regulus2dcg1(InFile, OutFile, DCGType, SucceededOrFailed) :-
	on_exception(
	Exception, 
	regulus2dcg2(InFile, OutFile, DCGType, SucceededOrFailed),
	( inform_about_top_level_regulus_error(Exception), SucceededOrFailed = failed )
    ).

regulus2dcg2(InFileOrFiles, OutFile, DCGType, SucceededOrFailed) :-
	regulus2dcg_init,
	read_regulus_file_or_files(InFileOrFiles, ReadRules0),
	(   DCGType = keep_line_info(static_for_generation) ->
	    assertz(category_internal(null_sem, [sem])) ;
	    true 
	),
	add_lexical_feature_defaults(ReadRules0, ReadRules1),
	convert_rules_to_strcat_semantics_if_necessary(ReadRules1, ReadRules2),
	expand_abbreviations_in_rules(ReadRules2, ReadRules),
	regulus_rules_to_dcg_rules(ReadRules, DCGType, DCGRules0),
	(   DCGType = stanford ->
	    DCGRules0 = DCGRules1 ;
	    add_dummy_rules_for_rhs_only_cats(DCGRules0, DCGRules1)
	),
	(   DCGType = keep_line_info(static_for_generation) ->
	    add_rule_for_null_sem_cat(DCGRules1, DCGRules2) ;
	    DCGRules1 = DCGRules2
	),
	%% we want to preserve the order between the rules:
	safe_remove_duplicates_preserving_order(DCGRules2, DCGRules3),
	%% sort(DCGRules2, DCGRules3),
	write_rules_dcg_file(DCGRules3, DCGType, OutFile),
	!,
	SucceededOrFailed = succeeded.
regulus2dcg2(_InFile, _OutFile, _DCGType, SucceededOrFailed) :-
	inform_about_top_level_regulus_error(failed),
	SucceededOrFailed = failed.

%---------------------------------------------------------------

regulus2dcg_init :-
	retract_all_regulus_preds.

%---------------------------------------------------------------

regulus_rules_to_dcg_rules(RegulusRules, DCGType, DCGRules) :-
	findall(
	DCGRule, 
	expand_regulus_rule_in_list(RegulusRules, DCGType, DCGRule), 
	DCGRules),

	\+ member(failed_expansion, DCGRules).

expand_regulus_rule_in_list(Rules, DCGType, Result) :-
	member(rule(Rule, LineInfo), Rules),
	regulus_debug_call(2, inform_about_expanding_rule(Rule, LineInfo)),
	expand_regulus_rule(Rule, DCGType, InstantiatedRule, LineInfo),
	(   InstantiatedRule = failed_expansion ->
	    Result = failed_expansion ;
	    Result = rule(InstantiatedRule, LineInfo)
	).

expand_regulus_rule(Rule, DCGType, InstantiatedRule, LineInfo) :-
	on_exception(
	Exception, 
	( expand_regulus_rule1(Rule, DCGType, InstantiatedRule0), 
	  InstantiatedRule = InstantiatedRule0 ),
	( inform_about_regulus_exception(Exception, LineInfo), 
	  InstantiatedRule = failed_expansion )
    ).
	
expand_regulus_rule1(Rule, DCGType, InstantiatedRule) :-
	give_up_if_bad_rule(Rule),
	pre_process_rule(Rule, DCGType, InstantiatedRule).

%---------------------------------------------------------------

give_up_if_bad_rule(Rule) :-
	\+ using_prolog_semantics,
	sem_variable_in_rule_head_not_in_body(Rule),
	!,
	regulus_error('~NSemantic variable in rule head doesn\'t appear in body~n', []).
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

pre_process_rule(Rule, DCGType, PreProcessedRule) :-
	pre_process_rule1(Rule, DCGType, PreProcessedRule, nonhead),
	instantiate_null_sem_values(PreProcessedRule).

pre_process_rule1(Var, DCGType, Var, HeadP) :-
	var(Var),
	!,
	HeadP = nonhead,
	DCGType = keep_line_info(static_for_generation),
	current_predicate(user:regulus_config/2),
	user:regulus_config(prolog_semantics, yes).
pre_process_rule1((H --> B), DCGType, (H1 --> B1), _HeadP) :-
	!,
	pre_process_rule1(H, DCGType, H1, head),
	pre_process_rule1(B, DCGType, B1, nonhead).
pre_process_rule1((P, Q), DCGType, (P1, Q1), _HeadP) :-
	!,
	pre_process_rule1(P, DCGType, P1, nonhead),
	pre_process_rule1(Q, DCGType, Q1, nonhead).
pre_process_rule1((P ; Q), DCGType, Result, _HeadP) :-
	DCGType = stanford,
	!,
	(   pre_process_rule1(P, DCGType, Result, nonhead) ;
	    pre_process_rule1(Q, DCGType, Result, nonhead)
	).
pre_process_rule1((P ; Q), DCGType, (P1 ; Q1), _HeadP) :-
	!,
	pre_process_rule1(P, DCGType, P1, nonhead),
	pre_process_rule1(Q, DCGType, Q1, nonhead).
pre_process_rule1((?P), DCGType, Result, HeadP) :-
	DCGType = stanford,
	!,
	(   pre_process_rule1(P, DCGType, Result, HeadP) ;

	    Result = []
	).
pre_process_rule1((?P), DCGType, (P1 ; NullP1), HeadP) :-
	DCGType = keep_line_info(static_for_generation),
	!,
	null_sem_version_of_constituent(P, DCGType, NullP),
	pre_process_rule1(P, DCGType, P1, HeadP),
	pre_process_rule1(NullP, DCGType, NullP1, HeadP).
pre_process_rule1((?P), DCGType, (P1 ; NullP1), HeadP) :-
	DCGType = dynamic(normal),
	!,
	null_sem_version_of_constituent(P, DCGType, NullP),
	pre_process_rule1(P, DCGType, P1, HeadP),
	pre_process_rule1(NullP, DCGType, NullP1, HeadP).
pre_process_rule1((?P), DCGType, (P1 ; []), HeadP) :-
	!,
	pre_process_rule1(P, DCGType, P1, HeadP).
pre_process_rule1(Atom, _DCGType, Atom, _HeadP) :-
	atomic(Atom),
	!.
pre_process_rule1(Cat:FeatsWithVals, _DCGType, InternalCat, HeadP) :-
	!,
	internalise_cat(Cat, FeatsWithVals, InternalCat, HeadP).
pre_process_rule1(Other, _DCGType, _, _HeadP) :-
	regulus_error('~NBad subterm in rule: ~q.~n', [Other]).

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
/*
internalise_cat(CatName, FeatsWithVals, cat(CatName, SynVals, Sem), _HeadP) :-
	CatName = null_sem,
	FeatsWithVals = [sem=Sem],
	SynVals = [],
	!.
*/
% Note that we remove the ignored features before checking validity of
% the feat-val list, since otherwise we'll get an error - if there are
% any ignored features, they'll be missing from the internal category
% declaration too.
internalise_cat(CatName, FeatsWithVals, cat(CatName, SynVals, Sem), HeadP) :-
	atom(CatName),
	get_category_internal(CatName, AllFeats),
	give_up_if_undeclared_feats_in_feat_val_list(CatName, AllFeats, FeatsWithVals),
	remove_ignored_feat_val_pairs_from_list(FeatsWithVals, FeatsWithVals1),
	replace_vals_with_substitutes_in_feat_val_list(FeatsWithVals1, FeatsWithVals2),
	is_valid_feat_val_list(FeatsWithVals2, AllFeats),
	sort(FeatsWithVals2, FeatsWithVals3),
	internalise_cat_feats(AllFeats, FeatsWithVals3, SynVals, Sem0),
	internalise_cat_sem(HeadP, Sem0, Sem),
	!.
internalise_cat(CatName, FeatsWithVals, _, _) :-
	regulus_error('~NBad category: ~q~n', [CatName:FeatsWithVals]).

% internalise_cat_feats(+AllSynFeats, +FeatsWithVals, -SynFeatsWithVals, -Sem).

internalise_cat_feats(AllSynFeats, FeatsWithVals, SynVals, Sem) :-
	(   ( member(sem, AllSynFeats) ; member(gsem, AllSynFeats) ) ->
	    true ;
	    Sem = no_value
	),
	internalise_cat_feats1(AllSynFeats, FeatsWithVals, SynVals, Sem, '*dummy*', '*dummy*').

internalise_cat_feats1([], [], [], _Sem, _LastFeat, _LastVal) :-
	!.
internalise_cat_feats1([Feat|R], [(Feat=Val)|R1], [(Feat=Val1)|SynVals], Sem, _LastFeat, _LastVal) :-
	( Feat \== sem, Feat \== gsem),
	!,
	internalise_feat_val(Feat, Val, Val1),
	internalise_cat_feats1(R, R1, SynVals, Sem, Feat, Val1).
internalise_cat_feats1(R, [(LastFeat=Val)|R1], SynVals, Sem, LastFeat, LastVal1) :-
	( LastFeat \== sem, LastFeat \== gsem),
	!,
	internalise_feat_val(LastFeat, Val, Val1),
	Val1 = LastVal1,
	internalise_cat_feats1(R, R1, SynVals, Sem, LastFeat, Val).
internalise_cat_feats1([Feat|R], R1, [(Feat=AnonVal)|SynVals], Sem, _LastFeat, _LastVal) :-
	( Feat \== sem, Feat \== gsem),
	!,
	internalise_cat_feats1(R, R1, SynVals, Sem, Feat, AnonVal).
internalise_cat_feats1([sem|R], [(sem=Val)|R1], SynVals, Sem, _LastFeat, _LastVal) :-
	!,
	Sem = value(return, Val),
	internalise_cat_feats1(R, R1, SynVals, Sem, sem, Val).
internalise_cat_feats1([gsem|R], [(gsem=Val)|R1], SynVals, Sem, _LastFeat, _LastVal) :-
	!,
	Sem = value(global, Val),
	internalise_cat_feats1(R, R1, SynVals, Sem, sem, Val).
internalise_cat_feats1([sem|R], R1, SynVals, Sem, _LastFeat, _LastVal) :-
	!,
	Sem = no_value,
	internalise_cat_feats1(R, R1, SynVals, Sem, sem, no_value).
internalise_cat_feats1([gsem|R], R1, SynVals, Sem, _LastFeat, _LastVal) :-
	!,
	Sem = no_value,
	internalise_cat_feats1(R, R1, SynVals, Sem, sem, no_value).

%---------------------------------------------------------------

% internalise_feat_val(+Feat, +Val, -InternalisedVal)

/*

The Mellish boolean vector encoding trick. If we have N possible values,
we represent the internalised value as a term with functor 'bv' and arity
N+1. The first arg is 0, the last arg is 1, and args I and I+1 are unified
if and only if the Ith possible value is NOT compatible with the specified value. 
Clever, isn't it?

*/

internalise_feat_val(_Feat, Val, InternalisedVal) :-
	var(Val),
	InternalisedVal = Val,
	!.
internalise_feat_val(_Feat, Val, InternalisedVal) :-
	functor(Val, bv, _),
	InternalisedVal = Val,
	!.
internalise_feat_val(Feat, Val, InternalisedVal) :-
	get_value_space_for_feat(Feat, ValueSpace),
	possible_values_for_value_space(ValueSpace, AllPossibleVals),
	length(AllPossibleVals, NVals),
	NVals1 is NVals + 1,
	functor(InternalisedVal, bv, NVals1),
	arg(1, InternalisedVal, 0),
	arg(NVals1, InternalisedVal, 1),
	internalise_feat_val1(AllPossibleVals, 1, Val, InternalisedVal).

internalise_feat_val1([], _NVals1, _Val, _InternalisedVal).
internalise_feat_val1([PossibleVal | RPossibleVals], I, Val, InternalisedVal) :-
	feature_value_compatible_with_spec(PossibleVal, Val),
	!,
	I1 is I + 1,
	internalise_feat_val1(RPossibleVals, I1, Val, InternalisedVal).
internalise_feat_val1([_PossibleVal | RPossibleVals], I, Val, InternalisedVal) :-
	I1 is I + 1,
	arg(I, InternalisedVal, ArgI),
	arg(I1, InternalisedVal, ArgI1),
	ArgI = ArgI1,
	internalise_feat_val1(RPossibleVals, I1, Val, InternalisedVal).

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
replace_syn_feat_with_substitute(Val, _Feat, Val) :-
	functor(Val, bv, _),
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
internalise_cat_sem(HeadP, value(ValueType, Val), value(ValueType, Val1)) :-
	!,
	internalise_cat_sem1(HeadP, Val, Val1).
internalise_cat_sem(_HeadP, no_value, no_value) :-
	!.

internalise_cat_sem1(head, Sem, Sem1) :-
	mark_sem_variables(Sem, ref, Sem1).
internalise_cat_sem1(nonhead, Sem, Sem1) :-
	using_prolog_semantics,
	!,
	mark_sem_variables(Sem, nonref, Sem1).
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

mark_sem_variables(Sem, ref, Sem1) :-
	var(Sem),
	!,
	Sem1 = ref(Sem).
mark_sem_variables(Sem, nonref, Sem1) :-
	var(Sem),
	!,
	Sem1 = nonref(Sem).
mark_sem_variables(Sem, _Type, Sem) :-
	atomic(Sem).
mark_sem_variables(FuncExpr, Type, FuncExpr1) :-
	using_prolog_semantics,
	compound(FuncExpr),
	!,
	functor(FuncExpr, F, N),
	functor(FuncExpr1, F, N),
	mark_sem_variables_args(N, FuncExpr, Type, FuncExpr1).
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
	regulus_error('~NNot meaningful to use GSL function ~w in body of rule~n', [F/N]).
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

% If we are using Prolog semantics,
%  a) don't do anything special with expressions of the form F = V
%  b) allow any kinds of semantic expressions on the RHS
instantiate_semantic_variables_as_components(_Term, _StructureOver) :-
	using_prolog_semantics,
	!.
instantiate_semantic_variables_as_components((F = V), StructureOver) :-
	var(V),
	!,
	V = StructureOver+F.
instantiate_semantic_variables_as_components((_F = Atom), _StructureOver) :-
	atomic(Atom),
	!,
	regulus_error('~NSemantic variable assigned value in body of rule.~n', []).
instantiate_semantic_variables_as_components((_F = V), _StructureOver) :-
	is_semantic_component(V),
	!,
	regulus_error('~NSemantic variable occurs twice in body of rule.~n', []).
%% Removed by peter ljunglöf, 2008-01-14:
%% instantiate_semantic_variables_as_components(Atom, _StructureOver) :-	
%% 	atomic(Atom),
%% 	!.
instantiate_semantic_variables_as_components([], _StructureOver) :-	
	!.
instantiate_semantic_variables_as_components([First | Rest], StructureOver) :-
	!,
	instantiate_semantic_variables_as_components(First, StructureOver),
	instantiate_semantic_variables_as_components(Rest, StructureOver).
%% Added by peter ljunglöf, 2008-01-14:
instantiate_semantic_variables_as_components(Atom, _StructureOver) :-
	atomic(Atom),
	!,
	regulus_error('~NSemantic variable assigned value in body of rule.~n', []).
instantiate_semantic_variables_as_components(A, B) :-
	!,
	regulus_error('~NBad call: ~w.~n', [instantiate_semantic_variables_as_components(A, B)]).

%---------------------------------------------------------------

add_dummy_rules_for_rhs_only_cats(DCGRules, DCGRules1) :-
	findall(LHSCat, lhs_cat_in_dcg_rules(DCGRules, LHSCat), LHSCats),
	findall(RHSCat, rhs_cat_in_dcg_rules(DCGRules, RHSCat), RHSCats),
	findall(UndefinedCat, ( member(UndefinedCat, RHSCats), \+ member(UndefinedCat, LHSCats) ), UndefinedCats0),
	safe_remove_duplicates(UndefinedCats0, UndefinedCats),
	dummy_rules_for_rhs_only_cats(UndefinedCats, DummyRules),
	append(DCGRules, DummyRules, DCGRules1).

lhs_cat_in_dcg_rules(DCGRules, LHSCat) :-
	member(Rule, DCGRules),
	lhs_cat_in_dcg_rule(Rule, LHSCat).

lhs_cat_in_dcg_rule(rule((Head --> _Body), _LineInfo), LHSCat) :-
	Head = cat(F, Feats, _Value),
	length(Feats, N),
	LHSCat = F/N.

rhs_cat_in_dcg_rules(DCGRules, RHSCat) :-
	member(Rule, DCGRules),
	rhs_cat_in_dcg_rule(Rule, RHSCat).

rhs_cat_in_dcg_rule(rule((_Head --> Body), _LineInfo), RHSCat) :-
	rhs_cat_in_dcg_body(Body, RHSCat).

rhs_cat_in_dcg_body(Var, _Cat) :-
	var(Var),
	!,
	fail.
rhs_cat_in_dcg_body((P, Q), Cat) :-
	!,
	(   rhs_cat_in_dcg_body(P, Cat) ;
	    rhs_cat_in_dcg_body(Q, Cat) 
	).
rhs_cat_in_dcg_body((P ; Q), Cat) :-
	!,
	(   rhs_cat_in_dcg_body(P, Cat) ;
	    rhs_cat_in_dcg_body(Q, Cat) 
	).
rhs_cat_in_dcg_body(cat(F, Feats, _Value), Cat) :-
	length(Feats, N),
	Cat = F/N.	

dummy_rules_for_rhs_only_cats([], []).
dummy_rules_for_rhs_only_cats([F | R], [F1 | R1]) :-
	dummy_rule_for_rhs_only_cat(F, F1),
	dummy_rules_for_rhs_only_cats(R, R1).

dummy_rule_for_rhs_only_cat(F/N, rule((cat(F, Feats, _Val) --> {fail}), no_line_info)) :-
	length(Feats, N).

%---------------------------------------------------------------

null_sem_version_of_constituent(P, keep_line_info(static_for_generation), NullP) :-
	P = _Cat:FeatValPairs,
	member(sem=SemVal, FeatValPairs),
	NullP = null_sem:[sem=SemVal],
	!.
null_sem_version_of_constituent(P, dynamic(normal), NullP) :-
	P = _Cat:FeatValPairs,
	member(sem=SemVal, FeatValPairs),
	NullP = null_sem_to_expand:[sem=SemVal],
	!.
null_sem_version_of_constituent(_P, _DCGType, NullP) :-
	NullP = [].

%---------------------------------------------------------------

add_rule_for_null_sem_cat(DCGRules, [NullSemCatRule | DCGRules]) :-
	null_sem_cat_rule(NullSemCatRule),
	!.

null_sem_cat_rule(rule((cat(null_sem, Feats, SemVal) --> []), no_line_info)) :-
	Feats = [],
	SemVal = no_value.

%---------------------------------------------------------------

write_rules_dcg_file(DCGRules, DCGType, File) :-
	format('~NWriting out rules...~n', []), flush_output(user),
	%trace,
	open_regulus_file(File, write, S),
	write_dcg_file_intro(S, DCGType),
	write_rules_dcg_stream(DCGRules, DCGType, S),
	close(S).

write_dcg_file_intro(S, DCGType) :-
	member(DCGType, [static, dynamic(_)]),
	!,
	format(S, '~N :- use_module(\'$REGULUS/Prolog/regulus_eval\').~n~n', []).
write_dcg_file_intro(_S, _DCGType).

%---------------------------------------------------------------

write_rules_dcg_stream([], _DCGType, _S).
write_rules_dcg_stream([F | R], DCGType, S) :-
	write_dcg_rule(F, DCGType, S),
	write_rules_dcg_stream(R, DCGType, S).

write_dcg_rule(Rule, DCGType, S) :-
	format_dcg_rule_for_writing0(Rule, DCGType, Rule1),
	portray_clause(S, Rule1).

format_dcg_rule_for_writing0(rule((H-->B), LineInfo), keep_line_info(DCGType), rule(OutputRule, LineInfo)) :-
	format_dcg_rule_for_writing0(rule((H-->B), LineInfo), DCGType, OutputRule),
	!.
format_dcg_rule_for_writing0(rule((H-->B), LineInfo), DCGType, OutputRule) :-
	on_exception(
	Exception, 
	format_dcg_rule_for_writing1((H-->B), LineInfo, DCGType, OutputRule),
	inform_about_regulus_exception(Exception, LineInfo)
    ),
	!.
format_dcg_rule_for_writing0(Rule, _DCGType, _OutputRule) :-
	regulus_error('~NUnable to format rule for DCG output: ~w~n', [Rule]).

format_dcg_rule_for_writing1((H-->B), LineInfo, DCGType, OutputRule) :-
	format_dcg_rule_for_writing2(B, B1, Tree, Globals, []-ExtraGoals0, nonhead),
	format_dcg_rule_for_writing2(H, H1, Tree, Globals, ExtraGoals0-ExtraGoals, head(LineInfo)),
	add_extra_goals_to_body(B1, ExtraGoals, B2),
	format_dcg_rule_for_grammar_type(DCGType, (H1-->B2), OutputRule),
	!.
format_dcg_rule_for_writing1(Rule, _DCGType, _OutputRule) :-
	regulus_error('~NUnable to format rule for DCG output: ~w~n', [Rule]).

format_dcg_rule_for_grammar_type(static, (H-->B), (H-->B)) :-
	!.
format_dcg_rule_for_grammar_type(static_for_generation, (H-->B), (H-->B)) :-
	!.
format_dcg_rule_for_grammar_type(dynamic(normal), (H-->B), OutputRule) :-
	expand_term((H-->B), (H1 :- B1)),
	OutputRule = dcg_clause(H1, B1),
	!.
format_dcg_rule_for_grammar_type(dynamic(generation), (H-->B), OutputRule) :-
	expand_term((H-->B), (H1 :- B1)),
	OutputRule = dcg_clause_for_generation(H1, B1),
	!.
format_dcg_rule_for_grammar_type(stanford, (H-->B), (HOut--->BOut)) :-
	flatten_comma_list_to_list(B, B1),
	turn_procedural_goals_into_return_values(B1, B2, H, H2),
	dcg_cats_to_stanford_cats_list([H2 | B2], [HOut | BOut]),
	!.
format_dcg_rule_for_grammar_type(DCGType, InputRule, OutputRule) :-
	regulus_error('~NBad call: ~w~n', [format_dcg_rule_for_grammar_type(DCGType, InputRule, OutputRule)]).

format_dcg_rule_for_writing2(Var, [Var], lex(Var), _Globals, GIn-GIn, nonhead) :-
	var(Var),
	!.
format_dcg_rule_for_writing2((A, B), (A1, B1), Tree, Globals, GIn-Gout, nonhead) :-
	!,
	format_dcg_rule_for_writing2(A, A1, ATree, Globals, GIn-GNext, nonhead),
	format_dcg_rule_for_writing2(B, B1, BTree, Globals, GNext-Gout, nonhead),
	Tree = (ATree, BTree).
	%Tree = [ATree, BTree].
%format_dcg_rule_for_writing2((A ; B), (( A1, {Tree=ATree} ) ; ( B1, {Tree=BTree} ) ), Tree, Globals, GIn-Gout, nonhead) :-
format_dcg_rule_for_writing2((A ; B), (( {Tree=ATree}, A1 ) ; ( {Tree=BTree}, B1 ) ), Tree, Globals, GIn-Gout, nonhead) :-
	!,
	format_dcg_rule_for_writing2(A, A1, ATree, Globals, GIn-GNext, nonhead),
	format_dcg_rule_for_writing2(B, B1, BTree, Globals, GNext-Gout, nonhead).
format_dcg_rule_for_writing2([], [], empty_constituent, _Globals, GIn-GIn, nonhead) :-
	!.
format_dcg_rule_for_writing2({Goal}, {Goal}, procedural_goal, _Globals, GIn-GIn, nonhead) :-
	!.
format_dcg_rule_for_writing2(Atom, [Atom], lex(Atom), _Globals, GIn-GIn, nonhead) :-
	atomic(Atom),
	!.
format_dcg_rule_for_writing2(cat(null_sem_to_expand, [], Value), {Local='*null_value*'}, empty_constituent, _Globals, GIn-GIn, nonhead) :-
	format_body_value(Value, Local),
	!.
format_dcg_rule_for_writing2(cat(Cat, SynFeats, Value), Goal, Tree, Globals, GIn-GIn, nonhead) :-
	format_body_value(Value, Local),
	Goal =.. [Cat, Tree, SynFeats, Local, Globals].
format_dcg_rule_for_writing2(cat(Cat, SynFeats, Value), Goal, BodyTree, Globals, GIn-GOut, head(LineInfo)) :-
	format_head_value(Value, Local, Globals, GIn-GOut),
	%Tree = [Cat, BodyTree],
	Tree = phrase(Cat, LineInfo, BodyTree),
	Goal =.. [Cat, Tree, SynFeats, Local, Globals].

%---------------------------------------------------------------

turn_procedural_goals_into_return_values([], [], H, H) :-
	!.
turn_procedural_goals_into_return_values([{merge_globals([_Feat=Val], _)}], [], H, H1) :-
	!,
	H =.. [CatName, Tree, SynFeats, _Local, Globals],
	H1 =.. [CatName, Tree, SynFeats, Val, Globals].
turn_procedural_goals_into_return_values([F | R], [F | R1], H, H1) :-
	turn_procedural_goals_into_return_values(R, R1, H, H1),
	!.

dcg_cats_to_stanford_cats_list([], []).
dcg_cats_to_stanford_cats_list([F | R], Result) :-
	dcg_cat_to_stanford_cat(F, F1),
	dcg_cats_to_stanford_cats_list(R, R1),
	(   is_list(F1) ->
	    append(F1, R1, Result) ;
	    Result = [F1 | R1]
	).

dcg_cat_to_stanford_cat(DCGCat, StanfordCat) :-
	is_list(DCGCat),
	StanfordCat = DCGCat,
	!.
dcg_cat_to_stanford_cat(DCGCat, StanfordCat) :-
	DCGCat =.. [CatName, Tree, SynFeatsVals, Local, _Globals],
	feat_vals_to_vals(SynFeatsVals, SynVals0),
	(   SynVals0 = [] ->
	    SynVals = [no_args] ;
	    SynVals = SynVals0
	),
	Syn =.. [CatName | SynVals],
	Sem = [Local, Tree],
	StanfordCat = Syn/Sem,
	!.

feat_vals_to_vals([], []) :-
	!.
feat_vals_to_vals([(_F=V) | R], [V | R1]) :-
	feat_vals_to_vals(R, R1),
	!.
feat_vals_to_vals(Other, Other1) :-
	regulus_error('~NBad call: ~w~n', [feat_vals_to_vals(Other, Other1)]).

%---------------------------------------------------------------

format_body_value(no_value, _NewVar) :-
	!.
format_body_value(value(return, Value), Local) :-
	format_body_value1(Value, Local).

format_head_value(no_value, _NewVar, _Globals, GIn-GIn) :-
	!.
format_head_value(value(return, Value), Local, _Globals, GIn-GOut) :-
	format_head_value1(Value, Local, GIn-GOut).
format_head_value(value(global, Value), Local, Globals, GIn-GOut) :-
	Local = _NewVar,
	format_head_value1(Value, Globals0, GIn-GNext),
	GOut = [merge_globals(Globals0, Globals) | GNext].

%---------------------------------------------------------------

format_body_value1(Var, Var) :-
	var(Var),
	!.
format_body_value1(no_value, _NewVar) :-
	!.
format_body_value1(Atom, Atom) :-
	atomic(Atom),
	!.
format_body_value1(nonref(T), T1) :-
	format_body_value1(T, T1),
	!.
format_body_value1(Term, Term1) :-
	functor(Term, F, N),
	functor(Term1, F, N),
	format_body_value1_args(N, Term, Term1).

format_body_value1_args(0, _Term, _Term1) :-
	!.
format_body_value1_args(I, Term, Term1) :-
	I > 0,
	arg(I, Term, Arg),
	arg(I, Term1, Arg1),
	format_body_value1(Arg, Arg1),
	I1 is I - 1,
	!,
	format_body_value1_args(I1, Term, Term1).

%---------------------------------------------------------------

format_head_value1(V, V, GIn-GIn) :-
	var(V),
	!.
format_head_value1(no_value, _NewVar, GIn-GIn) :-
	!.
format_head_value1(Atom, Atom, GIn-GIn) :-
	atomic(Atom),
	!.
format_head_value1(ref(V), V1, GIn-GOut) :-
	format_head_value1(V, V1, GIn-GOut),
	!.
format_head_value1([F | R], [F1 | R1], GIn-GOut) :-
	!,
	format_head_value1(F, F1, GIn-GNext),
	format_head_value1(R, R1, GNext-GOut).
format_head_value1(Val+Slot, slot_value(Val1, Slot), GIn-GOut) :-
	!,
	format_head_value1(Val, Val1, GIn-GOut).
format_head_value1(Expr, Expr1, GIn-GOut) :-
	functor(Expr, F, N),
	functor(Expr1, F, N),
	!,
	format_head_value1_args(N, Expr, Expr1, GIn-GOut).

format_head_value1_args(0, _Term, _Term1, GIn-GIn) :-
	!.
format_head_value1_args(I, Term, Term1, GIn-GOut) :-
	I > 0,
	arg(I, Term, Arg),
	arg(I, Term1, Arg1),
	format_head_value1(Arg, Arg1, GIn-GNext),
	I1 is I - 1,
	!,
	format_head_value1_args(I1, Term, Term1, GNext-GOut).

%---------------------------------------------------------------

add_extra_goals_to_body(Body, ExtraGoals, Body1) :-
	ExtraGoals = [],
	!,
	Body1 = Body.
add_extra_goals_to_body(Body, ExtraGoals, Body1) :-
	make_comma_list(ExtraGoals, ExtraGoal),
	Body1 = (Body, {ExtraGoal}).

make_comma_list([P], P) :-
	!.
make_comma_list([P, Q], (P, Q)) :-
	!.
make_comma_list([F | R], (F, R1)) :-
	make_comma_list(R, R1).

%---------------------------------------------------------------

flatten_comma_list_to_list(V, [V]) :-
	var(V),
	!.
flatten_comma_list_to_list((F, R), Result) :-
	!,
	flatten_comma_list_to_list(F, F1),
	flatten_comma_list_to_list(R, R1),
	append(F1, R1, Result).
flatten_comma_list_to_list([], []) :-
	!.
flatten_comma_list_to_list(Other, [Other]).

%---------------------------------------------------------------

comma_list_to_list_recursive(V, [V]) :-
	var(V),
	!.
comma_list_to_list_recursive((F, R), [F1 | R1]) :-
	!,
	comma_list_to_list_recursive1(F, F1),
	comma_list_to_list_recursive(R, R1).
comma_list_to_list_recursive(Other, [Other]).

comma_list_to_list_recursive1(V, V) :-
	var(V),
	!.
comma_list_to_list_recursive1((F, R), [F1 | R1]) :-
	!,
	comma_list_to_list_recursive1(F, F1),
	comma_list_to_list_recursive(R, R1).
comma_list_to_list_recursive1(Other, Other).

%---------------------------------------------------------------

match_cats(Cat1:Feats1, Cat2:Feats2) :-
	internalise_cat(Cat1, Feats1, InternalCat1, head),
	internalise_cat(Cat2, Feats2, InternalCat2, head),
	!,
	InternalCat1 = InternalCat2.

cat_matches_cat_in_list(Cat1:Feats1, List) :-
	internalise_cat(Cat1, Feats1, InternalCat1, head),
	member(Cat2:Feats2, List),
	internalise_cat(Cat2, Feats2, InternalCat2, head),
	InternalCat1 = InternalCat2,
	!.



