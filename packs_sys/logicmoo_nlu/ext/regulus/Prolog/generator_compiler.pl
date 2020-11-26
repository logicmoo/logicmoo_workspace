/*
- Basic strategy: transform DCG version into generation version. Complications:
  - Iterative deepening to avoid left recursion
  - Optional constituents won't work as currently impleremented. Initial solution: don't allow them for generation.
  - Need to do something about globals, at least top-level global
*/


:- module(generator_compiler,
	  [dcg2generator/7,
	   dcg2generator_role_marked/7
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module('$REGULUS/Prolog/regulus_declarations').
:- use_module('$REGULUS/Prolog/regulus2dcg').
:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/regulus_eval').

:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(ordsets)).

%---------------------------------------------------------------

dcg2generator(DCGFile, GeneratorFile, ModuleName, GenerationPred,
	      TopLevelCat, TopLevelFeatName, IncrementalDeepeningParams) :-
	dcg2generator1(DCGFile, GeneratorFile, ModuleName, GenerationPred,
		       TopLevelCat, TopLevelFeatName, IncrementalDeepeningParams,
		       normal).

dcg2generator_role_marked(DCGFile, GeneratorFile, ModuleName, GenerationPred,
			  TopLevelCat, TopLevelFeatName, IncrementalDeepeningParams) :-
	dcg2generator1(DCGFile, GeneratorFile, ModuleName, GenerationPred,
		       TopLevelCat, TopLevelFeatName, IncrementalDeepeningParams,
		       role_marked).
 
%---------------------------------------------------------------

% "sem_constants" are atoms. "sem_elements" are pairs of the form [Key, Value].

dcg2generator1(DCGFile, GeneratorFile, ModuleName, GenerationPred,
	       TopLevelCat, TopLevelFeatName, IncrementalDeepeningParams,
	       DCGType) :-
	read_dcg_file(DCGFile, Rules0),
	pre_process_rules(Rules0, Rules),
	check_rules_for_bad_semantic_values_and_get_sem_constants(Rules, SemConstants0-[]),
	sort(SemConstants0, SemConstants),
	remove_list_info_from_rules(Rules, RulesWithoutListInfo),
	find_d_list_sem_cats(RulesWithoutListInfo, DListSemCats),
	(   DCGType = role_marked ->
	    transform_role_marked_rules(RulesWithoutListInfo, DListSemCats, RulesWithoutListInfo1) ;
	    RulesWithoutListInfo1 = RulesWithoutListInfo
	),
	transform_semantics_into_d_lists(RulesWithoutListInfo1, DListSemCats, Rules2),
	post_process_rules(Rules2, FinalRules),
	get_semantic_elements_from_rules(FinalRules, SemElements0-[]),
	sort(SemElements0, SemElements),
	write_rules_to_file(FinalRules, SemConstants, SemElements,
			    ModuleName, GenerationPred, TopLevelCat, TopLevelFeatName, IncrementalDeepeningParams,
			    GeneratorFile).

%---------------------------------------------------------------

read_dcg_file(File, Rules) :-
	prolog_file_to_list(File, Rules).

%---------------------------------------------------------------

/*
  - Remove use_module declaration
  - Remove global argument (fourth one) in all non-terminals
  - Replace call to merge_globals in top-level rule. E.g. transform
    ('.MAIN'(phrase('.MAIN',A),[],_)-->utterance(A,[],C),{merge_globals([value=C],B)})
    into
    ('.MAIN'(phrase('.MAIN',A),[],[value=C])-->utterance(A,[],C)
*/

pre_process_rules([], []) :-
	!.
pre_process_rules([UseModuleDecl | R], R1) :-
	UseModuleDecl = ( :- ( use_module(_Module) ) ),
	!,
	pre_process_rules(R, R1).
pre_process_rules([F | R], [F1 | R1]) :-
	pre_process_rule(F, F1),
	!,
	pre_process_rules(R, R1).
pre_process_rules(X, Y) :-
	format2error('~N*** Error: bad call: ~w~n', [pre_process_rules(X, Y)]),
	fail.

pre_process_rule(rule(RuleIn, LineInfo), rule(RuleOut, LineInfo)) :-
	pre_process_rule(RuleIn, RuleOut),
	!.
pre_process_rule(RuleIn, RuleOut) :-
	remove_global_vars_arguments_from_non_terminals(RuleIn, RuleNext),
	handle_merge_globals(RuleNext, RuleOut),
	!.
pre_process_rule(RuleIn, RuleOut) :-
	format2error('~N*** Error: bad call: ~w~n', [pre_process_rule(RuleIn, RuleOut)]),
	fail.

remove_global_vars_arguments_from_non_terminals(Var, Var) :-
	var(Var),
	!.
remove_global_vars_arguments_from_non_terminals((H --> B), (H1 --> B1)) :-
	remove_global_vars_arguments_from_non_terminals(H, H1),
	remove_global_vars_arguments_from_non_terminals(B, B1),
	!.
remove_global_vars_arguments_from_non_terminals((P, Q), (P1, Q1)) :-
	remove_global_vars_arguments_from_non_terminals(P, P1),
	remove_global_vars_arguments_from_non_terminals(Q, Q1),
	!.
remove_global_vars_arguments_from_non_terminals((P ; Q), (P1 ; Q1)) :-
	remove_global_vars_arguments_from_non_terminals(P, P1),
	remove_global_vars_arguments_from_non_terminals(Q, Q1),
	!.
remove_global_vars_arguments_from_non_terminals(Terminals, Terminals) :-
	is_list(Terminals),
	!.
remove_global_vars_arguments_from_non_terminals({PrologCalls}, {PrologCalls}) :-
	!.
remove_global_vars_arguments_from_non_terminals(NonTerminal, NonTerminal1) :-
	compound(NonTerminal),
	NonTerminal =.. [CatName, Tree, Feats, Sem, _GlobalSem],
	NonTerminal1 =.. [CatName, Tree, Feats, Sem],
	!.
remove_global_vars_arguments_from_non_terminals(X, Y) :-
	format2error('~N*** Error: bad call: ~w~n', [remove_global_vars_arguments_from_non_terminals(X, Y)]),
	fail.

handle_merge_globals(Rule, Rule) :-
	\+ term_contains_functor(Rule, merge_globals/2),
	!.
handle_merge_globals((Head --> Body), (Head --> Body1)) :-
	arg(3, Head, HeadSem),
	handle_merge_globals1(Body, HeadSem, Body1),
	!.
handle_merge_globals(X, Y) :-
	format2error('~N*** Error: bad call: ~w~n', [handle_merge_globals(X, Y)]),
	fail.

handle_merge_globals1((P, Q), HeadSem, (P1, Q1)) :-
	!,
	handle_merge_globals1(P, HeadSem, P1),
	handle_merge_globals1(Q, HeadSem, Q1).
handle_merge_globals1({merge_globals(HeadSem, _)}, HeadSem, []) :-
	!.
handle_merge_globals1(Other, _HeadSem, Other) :-
	!.

%---------------------------------------------------------------

check_rules_for_bad_semantic_values_and_get_sem_constants([], Consts-Consts).
check_rules_for_bad_semantic_values_and_get_sem_constants([F | R], ConstsIn-ConstsOut) :-
	check_rule_for_bad_semantic_values_and_get_sem_constants(F, ConstsIn-ConstsNext),
	!,
	check_rules_for_bad_semantic_values_and_get_sem_constants(R, ConstsNext-ConstsOut).

check_rule_for_bad_semantic_values_and_get_sem_constants(Rule, ConstsIn-ConstsOut) :-
	Rule = rule((Head --> Body), LineInfo),
	semantic_variables_in_cat(Head, HeadSemVars),
	get_sem_constants_from_head(Head, ConstsIn-ConstsOut),
	rule_body_contains_bad_semantic_values(Body, HeadSemVars, [], _AllBodySemVars, Outcome),
	(   Outcome = bad(Reason) ->
	    report_bad_semantic_values_in_rule(LineInfo, Reason) ;
	    true
	),
	!.

report_bad_semantic_values_in_rule(LineInfo, Reason) :-
	inform_about_regulus_exception(regulus_exception(Reason, []), LineInfo),
	!.	

rule_body_contains_bad_semantic_values(Var, _HeadSemVars, _PreviousBodySemVars, AllBodySemVars, GoodOrBad) :-
	var(Var),
	AllBodySemVars = [],
	GoodOrBad = good,
	!.
rule_body_contains_bad_semantic_values((P, Q), HeadSemVars, PreviousBodySemVars, AllBodySemVars, GoodOrBad) :-
	rule_body_contains_bad_semantic_values(P, HeadSemVars, PreviousBodySemVars, PSemVars, PGoodOrBad),
	append(PreviousBodySemVars, PSemVars, NextBodySemVars),
	rule_body_contains_bad_semantic_values(Q, HeadSemVars, NextBodySemVars, QSemVars, QGoodOrBad),
	append(PSemVars, QSemVars, AllBodySemVars),
	combine_good_and_bad_semantic_value_results(PGoodOrBad, QGoodOrBad, GoodOrBad),
	!.
rule_body_contains_bad_semantic_values((P ; Q), HeadSemVars, PreviousBodySemVars, AllBodySemVars, GoodOrBad) :-
	rule_body_contains_bad_semantic_values(P, HeadSemVars, PreviousBodySemVars, PSemVars, PGoodOrBad),
	rule_body_contains_bad_semantic_values(Q, HeadSemVars, PreviousBodySemVars, QSemVars, QGoodOrBad),
	append(PSemVars, QSemVars, AllBodySemVars),
	combine_good_and_bad_semantic_value_results(PGoodOrBad, QGoodOrBad, GoodOrBad),
	!.
rule_body_contains_bad_semantic_values(Cat, HeadSemVars, PreviousBodySemVars, AllBodySemVars, GoodOrBad) :-
	is_cat(Cat),
	cat_has_nontrivial_semantics(Cat), 
	semantic_variables_in_cat(Cat, AllBodySemVars),
	(   \+ var_lists_share_member(HeadSemVars, AllBodySemVars) ->
	    GoodOrBad = bad('Warning: Semantic variable in body does not occur in head') ;
	    var_lists_share_member(PreviousBodySemVars, AllBodySemVars) ->
	    GoodOrBad = bad('Error: Same semantic variable occurs in two distinct daughters of rule')
	;
	    GoodOrBad = good
	),
	!.
rule_body_contains_bad_semantic_values(_Cat, _HeadSemVars, _PreviousBodySemVars, AllBodySemVars, GoodOrBad) :-
	AllBodySemVars = [],
	GoodOrBad = good,
	!.
rule_body_contains_bad_semantic_values(Cat, HeadSemVars, PreviousBodySemVars, AllBodySemVars, GoodOrBad) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [rule_body_contains_bad_semantic_values(Cat, HeadSemVars, PreviousBodySemVars, AllBodySemVars, GoodOrBad)]),
	fail.

combine_good_and_bad_semantic_value_results(PGoodOrBad, _QGoodOrBad, PGoodOrBad) :-
	PGoodOrBad = bad(_),
	!.
combine_good_and_bad_semantic_value_results(_PGoodOrBad, QGoodOrBad, QGoodOrBad) :-
	QGoodOrBad = bad(_),
	!.
combine_good_and_bad_semantic_value_results(_PGoodOrBad, _QGoodOrBad, good) :-
	!.

semantic_variables_in_cat(Cat, SemVars)	:-
	cat_name_and_sem_for_cat(Cat, _CatName, Sem),
	term_variables_bag(Sem, SemVars),
	!.
semantic_variables_in_cat(Cat, SemVars)	:-
	format2error('~N*** Error: bad call: ~w~n', [semantic_variables_in_cat(Cat, SemVars)]),
	fail.

get_sem_constants_from_head(Head, ConstsIn-ConstsOut) :-
	cat_name_and_sem_for_cat(Head, _CatName, Sem),
	get_sem_constants_from_sem(Sem, ConstsIn-ConstsOut).

%---------------------------------------------------------------

remove_list_info_from_rules([], []).
remove_list_info_from_rules([F | R], [F1 | R1]) :-
	remove_list_info_from_rule(F, F1),
	remove_list_info_from_rules(R, R1).

remove_list_info_from_rule(rule(Rule, _ListInfo), Rule) :-
	!.
remove_list_info_from_rule(Rule, Rule).

/*
Static analysis to work out which constituents need semantics to be a difference-list.
  - Anything whose LHS semantics is a concat needs a d-list
  - Anything appearing in an RHS is a d-list, if the LHS is a d-list and either
    1) the semantics of the LHS are or include a concat, and the semantics of the RHS item are one of the concatenated vars,
    2) the semantics of the LHS and the RHS element are identical variables
  - The LHS is a d-list, if both
    1) the semantics of the LHS are a variable
    2) there is an RHS variable whose semantics are the same variable, and which is a d-list
  - Find fixpoint
*/

find_d_list_sem_cats(Rules, DListSemCats) :-
	find_d_list_sem_cats_first_round(Rules, []-DListSemCats1),
	find_d_list_sem_cats_other_rounds(Rules, DListSemCats1, DListSemCats1-DListSemCats),
	format('~N~n--- Found d-list sem cats: ~w~n~n', [DListSemCats]).		      

find_d_list_sem_cats_first_round([], Cats-Cats) :-
	!.
find_d_list_sem_cats_first_round([F | R], CatsIn-CatsOut) :-
	find_d_list_sem_cats_first_round1(F, CatsIn-CatsNext),
	!,
	find_d_list_sem_cats_first_round(R, CatsNext-CatsOut).

find_d_list_sem_cats_first_round1((Head --> Body), CatsIn-CatsOut) :-
	cat_name_and_sem_for_cat(Head, HeadCatName, HeadSem),
	(   is_strict_concat(HeadSem, ExplicitConcatVars, _ConcatNonVars) ->
	    NewCats = [HeadCatName]
	;
	    ExplicitConcatVars = [],
	    NewCats = []
	),
	% Assume that any var which occurs as the argument in a lambda-application is a concat var.
	% This seems reasonably safe in practice - anything else would be odd.
	lambda_application_vars(HeadSem, LambdaApplicationVars-[]),
	append(ExplicitConcatVars, LambdaApplicationVars, ConcatVars),
	(   NewCats \== []
	;
	    ConcatVars \== []
	),
	!,
	ord_union(CatsIn, [HeadCatName], CatsNext),
	find_d_list_sem_cats_first_round2(Body, ConcatVars, CatsNext-CatsOut).
find_d_list_sem_cats_first_round1(_Rule, CatsIn-CatsIn) :-
	!.
find_d_list_sem_cats_first_round1(Rule, Cats) :-
	format2error('~N*** Error: bad call: ~w~n', [find_d_list_sem_cats_first_round1(Rule, Cats)]),
	fail.

find_d_list_sem_cats_first_round2((P, Q), ConcatVars, CatsIn-CatsOut) :-
	!,
	find_d_list_sem_cats_first_round2(P, ConcatVars, CatsIn-CatsNext),
	find_d_list_sem_cats_first_round2(Q, ConcatVars, CatsNext-CatsOut).
find_d_list_sem_cats_first_round2((P ; Q), ConcatVars, CatsIn-CatsOut) :-
	!,
	find_d_list_sem_cats_first_round2(P, ConcatVars, CatsIn-CatsNext),
	find_d_list_sem_cats_first_round2(Q, ConcatVars, CatsNext-CatsOut).
find_d_list_sem_cats_first_round2(Cat, ConcatVars, CatsIn-CatsOut) :-
	is_cat(Cat),
	cat_name_and_sem_for_cat(Cat, CatName, Sem),
	var(Sem),
	id_member(Sem, ConcatVars),
	!,
	ord_union(CatsIn, [CatName], CatsOut).
find_d_list_sem_cats_first_round2(_Other, _ConcatVars, CatsIn-CatsIn) :-
	!.
find_d_list_sem_cats_first_round2(Other, ConcatVars, Cats) :-
	format2error('~N*** Error: bad call: ~w~n', [find_d_list_sem_cats_first_round2(Other, ConcatVars, Cats)]),
	fail.

find_d_list_sem_cats_other_rounds(Rules, CurrentCats, DListSemCatsIn-DListSemCatsOut) :-
	find_d_list_sem_cats_other_round(Rules, CurrentCats, DListSemCatsIn-DListSemCatsNext, []-NewCats),
	!,
	(   NewCats == [] ->
	    DListSemCatsNext = DListSemCatsOut ;
	    find_d_list_sem_cats_other_rounds(Rules, NewCats, DListSemCatsNext-DListSemCatsOut)
	).
find_d_list_sem_cats_other_rounds(Rules, CurrentCats, DListSemCats) :-
	format2error('~N*** Error: bad call: ~w~n', [find_d_list_sem_cats_other_rounds(Rules, CurrentCats, DListSemCats)]),
	fail.

find_d_list_sem_cats_other_round([], _CurrentCats, DListSemCatsIn-DListSemCatsIn, NewCatsIn-NewCatsIn) :-
	!.
find_d_list_sem_cats_other_round([F | R], CurrentCats, DListSemCatsIn-DListSemCatsOut, NewCatsIn-NewCatsOut) :-
	find_d_list_sem_cats_other_round1(F, CurrentCats, DListSemCatsIn-DListSemCatsNext, NewCatsIn-NewCatsNext),
	!,
	find_d_list_sem_cats_other_round(R, CurrentCats, DListSemCatsNext-DListSemCatsOut, NewCatsNext-NewCatsOut).

find_d_list_sem_cats_other_round1((Head --> Body), CurrentCats, DListSemCatsIn-DListSemCatsOut, NewCatsIn-NewCatsOut) :-
	cat_name_and_sem_for_cat(Head, HeadCatName, HeadSem),
	member(HeadCatName, CurrentCats),
	(  ( var(HeadSem), HeadVars = [HeadSem] )
	%;  ( is_concat(HeadSem, HeadVars, _), HeadVars \== [] )
	;  ( all_concat_vars_in_term(HeadSem, HeadVars-[]), HeadVars \== [] )
	),
	!,
	find_d_list_sem_cats_other_round2(Body, HeadVars, DListSemCatsIn-DListSemCatsOut, NewCatsIn-NewCatsOut, testing_daughters).
find_d_list_sem_cats_other_round1((Head --> Body), _CurrentCats, DListSemCatsIn-DListSemCatsOut, NewCatsIn-NewCatsOut) :-
	cat_name_and_sem_for_cat(Head, HeadCatName, HeadSem),
	\+ member(HeadCatName, DListSemCatsIn),
	var(HeadSem),
	!,
	find_d_list_sem_cats_other_round2(Body, [HeadSem], DListSemCatsIn-DListSemCatsOut, NewCatsIn-NewCatsOut, testing_mother(HeadCatName)).
find_d_list_sem_cats_other_round1(_Rule, _CurrentCats, DListSemCatsIn-DListSemCatsIn, NewCatsIn-NewCatsIn) :-
	!.
find_d_list_sem_cats_other_round1(F, CurrentCats, DListSemCats, NewCats) :-
	format2error('~N*** Error: bad call: ~w~n', [find_d_list_sem_cats_other_round1(F, CurrentCats, DListSemCats, NewCats)]),
	fail.

find_d_list_sem_cats_other_round2(Var, _HeadVars, DListSemCatsIn-DListSemCatsIn, NewCatsIn-NewCatsIn, _Mode) :-
	var(Var),
	!.
find_d_list_sem_cats_other_round2((P, Q), HeadVars, DListSemCatsIn-DListSemCatsOut, NewCatsIn-NewCatsOut, Mode) :-
	find_d_list_sem_cats_other_round2(P, HeadVars, DListSemCatsIn-DListSemCatsNext, NewCatsIn-NewCatsNext, Mode),
	find_d_list_sem_cats_other_round2(Q, HeadVars, DListSemCatsNext-DListSemCatsOut, NewCatsNext-NewCatsOut, Mode).
find_d_list_sem_cats_other_round2((P ; Q), HeadVars, DListSemCatsIn-DListSemCatsOut, NewCatsIn-NewCatsOut, Mode) :-
	find_d_list_sem_cats_other_round2(P, HeadVars, DListSemCatsIn-DListSemCatsNext, NewCatsIn-NewCatsNext, Mode),
	find_d_list_sem_cats_other_round2(Q, HeadVars, DListSemCatsNext-DListSemCatsOut, NewCatsNext-NewCatsOut, Mode).
find_d_list_sem_cats_other_round2(Cat, HeadVars, DListSemCatsIn-DListSemCatsOut, NewCatsIn-NewCatsOut, Mode) :-
	is_cat(Cat),
	cat_name_and_sem_for_cat(Cat, CatName, Sem),
	var(Sem),
	id_member(Sem, HeadVars),
	(   Mode = testing_mother(MotherCatName) ->
	    member(CatName, DListSemCatsIn) ;
	    true
	),
	!,
	(   Mode = testing_daughters ->
	    NewCatName = CatName ;
	    Mode = testing_mother(MotherCatName) ->
	    NewCatName = MotherCatName
	),
	ord_union(DListSemCatsIn, [NewCatName], DListSemCatsOut, NewCats),
	ord_union(NewCatsIn, NewCats, NewCatsOut).	    
find_d_list_sem_cats_other_round2(_Cat, _HeadVars, DListSemCatsIn-DListSemCatsIn, NewCatsIn-NewCatsIn, _Mode) :-
	!.
find_d_list_sem_cats_other_round2(Cat, HeadVars, DListSemCats, NewCats, Mode) :-
	format2error('~N*** Error: bad call: ~w~n', [find_d_list_sem_cats_other_round2(Cat, HeadVars, DListSemCats, NewCats, Mode)]),
	fail.

%---------------------------------------------------------------

transform_role_marked_rules([], _DListSemCats, []).
transform_role_marked_rules([F | R], DListSemCats, [F1 | R1]) :-
	transform_role_marked_rule(F, DListSemCats, F1),
	!,
	transform_role_marked_rules(R, DListSemCats, R1).

transform_role_marked_rule((Head --> Body), DListSemCats, (Head1 --> Body1)) :-
	transform_role_marked_rule_head(Head, DListSemCats, Head1, TopRole, VarRolePairs),
	transform_role_marked_rule_body(Body, DListSemCats, TopRole, VarRolePairs, Body0),
	add_peek_declarations_to_body(Head, Body0, Body1).

/*
Transforming the head category:

- If this is not a d-list category, keep unchanged and pass role=top to the body

- If this is a d-list category:
   - Add a feature of the form '*role*'=TopRole to the head
   - Recurse down the logical form component of the head, initially using TopRole as the role:
     - Variable Var: keep it unchanged and add Var-Role to the VarRolePairs.
     - [role, Role, Body]: transform Body to Body1 using Role as the role, and replace by Body1
     - [clause, Role, Body]: transform Body to Body1 using 'top' as the role, and replace by Role-[clause, Body1]
     - [conj, Conj | Args]: transform each Arg using Role to make Arg1, and replace by [conj, Conj | Args1]
     - concat(X, Y): transform X and Y using Role to make X1 and Y1, and replace by concat(X1, Y1)
     - [Key, Value] with atomic Key: replace by Role-[Key, Value]
     - [F | R]: transform F and R using Role into F1 and R1, replace by [F1 | R1]
     - Other: keep unchanged

*/

transform_role_marked_rule_head(Head, DListSemCats, Head1, TopRole, VarRolePairs) :-
	cat_name_and_sem_for_cat(Head, CatName, _Sem),
	\+ member(CatName, DListSemCats),
	!,
	Head1 = Head,
	TopRole = null,
	VarRolePairs = [].
transform_role_marked_rule_head(Head, DListSemCats, Head1, TopRole, VarRolePairs) :-
	Head =.. [HeadCatName, HeadTree, HeadFeats, HeadSem],
	member(HeadCatName, DListSemCats),
	Head1 =.. [HeadCatName, HeadTree, HeadFeats1, HeadSem1],
	HeadFeats1 = ['*role*'=TopRole | HeadFeats],
	%reduce_lambda_applications_if_possible(HeadSem, ReducedHeadSem),
	%HeadSem = ReducedHeadSem,
	canonicalise_sem_for_generation(HeadSem, ReducedHeadSem),
	transform_role_marked_head_sem(ReducedHeadSem, TopRole, HeadSem1, []-VarRolePairs),
	!.
transform_role_marked_rule_head(Head, DListSemCats, Head1, TopRole, VarRolePairs) :-
	format2error('~N*** Error: bad call: ~q~n',
		     [transform_role_marked_rule_head(Head, DListSemCats, Head1, TopRole, VarRolePairs)]),
	fail.

reduce_lambda_applications_if_possible(HeadSem, ReducedHeadSem) :-
	term_contains_functor(HeadSem, apply/0),
	beta_reduce(HeadSem, ReducedHeadSem),
	!.
reduce_lambda_applications_if_possible(HeadSem, HeadSem).	

transform_role_marked_head_sem(Sem, Role, Sem1, VarRolePairsIn-VarRolePairsOut) :-
	var(Sem),
	Sem1 = Sem,
	VarRolePairsOut = [Sem-Role | VarRolePairsIn],
	!.
% This clause is unsatisfactory in two ways:
% 1) We are assuming that the extraction takes place in the same clause, so we can't e.g. get extraction
%    from an embedded clause.
% 2) The generated rule is underconstrained, since we should thread down the role for the Arg
%    and make it match the one above the associated gap.
transform_role_marked_head_sem(Sem, RoleAbove, Sem1, VarRolePairsIn-VarRolePairsOut) :-
	safe_subsumes_chk([apply, [lambda, _, _], _], Sem),
	Sem = [apply, [lambda, _Var, Body], Arg],
	transform_role_marked_head_sem(Body, RoleAbove, Body1, VarRolePairsIn-VarRolePairsNext),
	transform_role_marked_head_sem(Arg, _UndeterminedRole, Arg1, VarRolePairsNext-VarRolePairsOut),
	Sem1 = concat(Body1, Arg1),
	!.
% Corresponding to the clause immediately above for application of a lambda-bound form, we incorrectly
% ignore the semantic representations of the gaps. Both clauses need to be revised together.
transform_role_marked_head_sem(GapVar, _RoleAbove, [], VarRolePairsIn-VarRolePairsIn) :-
	is_gap_var(GapVar),
	!.
transform_role_marked_head_sem([[role, Role, Body]], _RoleAbove, Body1, VarRolePairsIn-VarRolePairsOut) :-
	transform_role_marked_head_sem(Body, Role, Body1, VarRolePairsIn-VarRolePairsOut),
	!.
transform_role_marked_head_sem([[clause, Body]], Role, [Role=[clause, Body1]], VarRolePairsIn-VarRolePairsOut) :-
	transform_role_marked_head_sem(Body, null, Body1, VarRolePairsIn-VarRolePairsOut),
	!.
transform_role_marked_head_sem([[conj, Conj | Args]], Role, [[conj, Conj | Args1]], VarRolePairsIn-VarRolePairsOut) :-
	transform_role_marked_head_sem_list(Args, Role, Args1, VarRolePairsIn-VarRolePairsOut),
	!.
transform_role_marked_head_sem(concat(X, Y), Role, concat(X1, Y1), VarRolePairsIn-VarRolePairsOut) :-
	transform_role_marked_head_sem(X, Role, X1, VarRolePairsIn-VarRolePairsNext),
	transform_role_marked_head_sem(Y, Role, Y1, VarRolePairsNext-VarRolePairsOut),
	!.
transform_role_marked_head_sem([[Key, Value]], Role, [Role=[Key, Value]], VarRolePairsIn-VarRolePairsIn) :-
	atom(Key),
	!.
transform_role_marked_head_sem([F | R], Role, Result, VarRolePairsIn-VarRolePairsOut) :-
	R \== [],
	transform_role_marked_head_sem(concat([F], R), Role, Result, VarRolePairsIn-VarRolePairsOut),
	!.
transform_role_marked_head_sem(Other, _Role, Other, VarRolePairsIn-VarRolePairsIn) :-
	!.

transform_role_marked_head_sem_list([], _Role, [], VarRolePairsIn-VarRolePairsIn).
transform_role_marked_head_sem_list([F | R], Role, [F1 | R1], VarRolePairsIn-VarRolePairsOut) :-
	transform_role_marked_head_sem(F, Role, F1, VarRolePairsIn-VarRolePairsNext),
	!,
	transform_role_marked_head_sem_list(R, Role, R1, VarRolePairsNext-VarRolePairsOut).

is_gap_var(GapVar) :-
	ground(GapVar),
	is_gap_var1(GapVar).

is_gap_var1(x).
is_gap_var1(y).
is_gap_var1([x]).
is_gap_var1([y]).

%---------------------------------------------------------------

/*
If the head sem contains elements of the form [role, R, B], where R is a variable, add a 'peek' declaration
at the beginning to link R to the input semantic form. This can have a large impact on efficiency in
rules like

PP --> P NP

where the role comes from the P.
*/

add_peek_declarations_to_body(Head, Body, Body1) :-
	cat_name_and_sem_for_cat(Head, _CatName, Sem),
	add_peek_declarations_to_body1(Sem, Body, Body1),
	!.
add_peek_declarations_to_body(Head, Body, Body1) :-
	format2error('~N*** Error: bad call: ~q~n',
		     [add_peek_declarations_to_body(Head, Body, Body1)]),
	fail.

add_peek_declarations_to_body1(Var, Body, Body) :-
	var(Var),
	!.
add_peek_declarations_to_body1(Atom, Body, Body) :-
	atomic(Atom),
	!.
add_peek_declarations_to_body1(Sem, Body, Body) :-
	safe_subsumes_chk([apply, [lambda, _, _], _], Sem),
	!.
add_peek_declarations_to_body1([role, Role, _X], Body, Body1) :-
	var(Role),
	Body1 = ( peek(Role=_), Body ),
	!.
add_peek_declarations_to_body1(concat(P, Q), Body, Body1) :-
	add_peek_declarations_to_body1(P, Body, BodyNext),
	add_peek_declarations_to_body1(Q, BodyNext, Body1),
	!.
add_peek_declarations_to_body1([P | Q], Body, Body1) :-
	add_peek_declarations_to_body1(P, Body, BodyNext),
	add_peek_declarations_to_body1(Q, BodyNext, Body1),
	!.
add_peek_declarations_to_body1(_Other, Body, Body) :-
	!.

%---------------------------------------------------------------

/*

Our strategy is

1) merge lists when possible
2) if no merge is possible, and one component of the concat is a list, move it to the left

*/

canonicalise_sem_for_generation(V, V) :-
	var(V),
	!.
canonicalise_sem_for_generation(concat(P, Q), Result) :-
	canonicalise_sem_for_generation(P, P1),
	canonicalise_sem_for_generation(Q, Q1),
	canonicalise_concat_sem_for_generation(concat(P1, Q1), Result),
	!.
canonicalise_sem_for_generation([F | R], [F1 | R1]) :-
	canonicalise_sem_for_generation(F, F1),
	canonicalise_sem_for_generation(R, R1),
	!.
canonicalise_sem_for_generation(X, X) :-
	!.
canonicalise_sem_for_generation(Other, Other1) :-
	format2error('~N*** Error: bad call: ~w~n', [canonicalise_sem_for_generation(Other, Other1)]),
	fail.

canonicalise_concat_sem_for_generation(concat(P, Q), Result) :-
	is_list(P),
	is_list(Q),
	append(P, Q, Result),
	!.
canonicalise_concat_sem_for_generation(concat(P, Q), Result) :-
	Q == [],
	P = Result,
	!.
canonicalise_concat_sem_for_generation(concat(P, Q), Result) :-
	P == [],
	Q = Result,
	!.
canonicalise_concat_sem_for_generation(concat(P, Q), Result) :-
	is_list(Q),
	Result = concat(Q, P),
	!.
canonicalise_concat_sem_for_generation(concat(P, Q), Result) :-
	is_list(P),
	nonvar(Q),
	Q = concat(List, Rest),
	is_list(List),
	append(P, List, PAndList),
	Result = concat(PAndList, Rest),
	!.
canonicalise_concat_sem_for_generation(X, X) :-
	!.
canonicalise_concat_sem_for_generation(Other, Other1) :-
	format2error('~N*** Error: bad call: ~w~n', [canonicalise_concat_sem_for_generation(Other, Other1)]),
	fail.
				
%---------------------------------------------------------------

/*

Transforming the body:

- If this is a disjunction, split.
- If this is a category with semantics Sem, and we have an item Sem-Role in the VarRolePairs, add feature '*role*'=Role.
- If this is any other d-list category, add a feature '*role*'=TopRole
- Otherwise, keep unchanged.

*/

transform_role_marked_rule_body((P ; Q), DListSemCats, TopRole, VarRolePairs, (P1 ; Q1)) :-
	transform_role_marked_rule_body(P, DListSemCats, TopRole, VarRolePairs, P1),
	transform_role_marked_rule_body(Q, DListSemCats, TopRole, VarRolePairs, Q1),
	!.
transform_role_marked_rule_body((P, Q), DListSemCats, TopRole, VarRolePairs, (P1, Q1)) :-
	transform_role_marked_rule_body(P, DListSemCats, TopRole, VarRolePairs, P1),
	transform_role_marked_rule_body(Q, DListSemCats, TopRole, VarRolePairs, Q1),
	!.
transform_role_marked_rule_body(Body, DListSemCats, TopRole, VarRolePairs, Body1) :-
	Body =.. [CatName, Tree, Feats, Sem],
	Body1 =.. [CatName, Tree, Feats1, Sem],
	member(CatName, DListSemCats),
	(   role_for_var_in_var_role_pairs(Sem, VarRolePairs, Role) ->
	    true ;
	    Role = TopRole
	),
	Feats1 = ['*role*'=Role | Feats],
	!.
transform_role_marked_rule_body(Body, _DListSemCats, _TopRole, _VarRolePairs, Body).

role_for_var_in_var_role_pairs(Var, [F | _R], Role) :-
	F = Var1-Role,
	Var == Var1,
	!.
role_for_var_in_var_role_pairs(Var, [_F | R], Role) :-
	role_for_var_in_var_role_pairs(Var, R, Role).
	
%---------------------------------------------------------------

/*
- Replacing semantic forms with d-lists
  - Find vars in LHS concat (call this ConcatVars)
    - Vars must appear directly as args to concat. E.g. in concat(A, [[foo, B]]) only count A.
  - Find non-variable forms in LHS concat
    - At start of new RHS, add calls to consume_item/3 for each non-variable term
  - New LHS sem-value is ArgsIn-ArgsOut
  - Recurse though RHS, passing ConcatVars, current ArgsIn, producing new ArgsOut
  - If RHS sem-value is a variable in ConcatVars, replace with ArgsIn-ArgsOut
  - If RHS sem-value V is a variable not in ConcatVars, and cat requires d-lists, replace with V-[]
  - If conjunction, thread
  - If disjunction, split
  - If other, ArgsIn=ArgsOut

  - Treat lexical rules specially: split them up to get better indexing
*/

transform_semantics_into_d_lists([], _DListSemCats, []) :-
	!.
transform_semantics_into_d_lists([F | R], DListSemCats, [F1General, F1Specific | R1]) :-
	lexical_rule(F, DListSemCats),
	transform_semantics_into_d_lists_for_lexical_rule(F, DListSemCats, F1General, F1Specific),
	!,
	transform_semantics_into_d_lists(R, DListSemCats, R1).
transform_semantics_into_d_lists([F | R], DListSemCats, [F1 | R1]) :-
	transform_semantics_into_d_lists_for_non_lexical_rule(F, DListSemCats, F1),
	!,
	transform_semantics_into_d_lists(R, DListSemCats, R1).

% Case 1: LHS is d-list pred, semantics is concat
transform_semantics_into_d_lists_for_non_lexical_rule((Head --> Body), DListSemCats, (Head1 --> Body1)) :-
	Head =.. [HeadCatName, HeadTree, HeadFeats, HeadSem],
	member(HeadCatName, DListSemCats),
	is_concat(HeadSem, ConcatVars, ItemsToConsume),
	transform_semantics_into_d_lists_in_items_to_consume(ItemsToConsume, ItemsToConsume1, 
							     NestedItemsToConsumeEtcIn-[], NestedItemsToConsumeEtcOut-[]),
	Head1 =.. [HeadCatName, HeadTree, HeadFeats, ArgsIn-ArgsOut],
	TopItemsToConsumeEtcIn = [ItemsToConsume1, ConcatVars, ArgsIn],
	TopItemsToConsumeEtcOut = [[], ConcatVars, ArgsOut],
	AllItemsToConsumeEtcIn = [TopItemsToConsumeEtcIn | NestedItemsToConsumeEtcIn],
	AllItemsToConsumeEtcOut = [TopItemsToConsumeEtcOut | NestedItemsToConsumeEtcOut],
	convert_peeks_in_body(Body, ArgsIn, BodyNext),
	transform_semantics_into_d_lists_for_body(BodyNext, DListSemCats, AllItemsToConsumeEtcIn-AllItemsToConsumeEtcOut, Body1),
	!.
% Case 2: LHS is d-list pred, semantics is simple variable
transform_semantics_into_d_lists_for_non_lexical_rule((Head --> Body), DListSemCats, (Head1 --> Body1)) :-
	Head =.. [HeadCatName, HeadTree, HeadFeats, HeadSem],
	member(HeadCatName, DListSemCats),
	var(HeadSem),
	ConcatVars = [HeadSem],
	ItemsToConsume = [],
	Head1 =.. [HeadCatName, HeadTree, HeadFeats, ArgsIn-ArgsOut],
	AllItemsToConsumeEtcIn = [[ItemsToConsume, ConcatVars, ArgsIn]],
	AllItemsToConsumeEtcOut = [[[], ConcatVars, ArgsOut]],
	convert_peeks_in_body(Body, ArgsIn, BodyNext),
	transform_semantics_into_d_lists_for_body(BodyNext, DListSemCats, AllItemsToConsumeEtcIn-AllItemsToConsumeEtcOut, Body1),
	!.
% Case 3: LHS is not d-list pred
transform_semantics_into_d_lists_for_non_lexical_rule((Head --> Body), DListSemCats, (Head1 --> Body1)) :-
	Head =.. [HeadCatName, HeadTree, HeadFeats, HeadSem],
	\+ member(HeadCatName, DListSemCats),
	ConcatVars = [],
	ItemsToConsume = [],
	Head1 =.. [HeadCatName, HeadTree, HeadFeats, HeadSem],
	ArgsIn = [],
	ArgsOut = [],
	AllItemsToConsumeEtcIn = [[ItemsToConsume, ConcatVars, ArgsIn]],
	AllItemsToConsumeEtcOut = [[[], ConcatVars, ArgsOut]],
	transform_semantics_into_d_lists_for_body(Body, DListSemCats, AllItemsToConsumeEtcIn-AllItemsToConsumeEtcOut, Body1).
transform_semantics_into_d_lists_for_non_lexical_rule(Rule, DListSemCats, Rule1) :-
	format2error('~N*** Error: bad call: ~w~n', [transform_semantics_into_d_lists_for_non_lexical_rule(Rule, DListSemCats, Rule1)]),
	fail.

transform_semantics_into_d_lists_in_items_to_consume([], [],
						     NestedItemsToConsumeEtcIn-NestedItemsToConsumeEtcIn,
						     NestedItemsToConsumeEtcOut-NestedItemsToConsumeEtcOut) :-
	!.
transform_semantics_into_d_lists_in_items_to_consume([F | R], [F1 | R1], 
						     NestedItemsToConsumeEtcInIn-NestedItemsToConsumeEtcInOut,
						     NestedItemsToConsumeEtcOutIn-NestedItemsToConsumeEtcOutOut) :-
	transform_semantics_into_d_lists_in_item_to_consume(F, F1, 
							    NestedItemsToConsumeEtcInIn-NestedItemsToConsumeEtcInNext,
							    NestedItemsToConsumeEtcOutIn-NestedItemsToConsumeEtcOutNext),
	!,
	transform_semantics_into_d_lists_in_items_to_consume(R, R1, 
							     NestedItemsToConsumeEtcInNext-NestedItemsToConsumeEtcInOut,
							     NestedItemsToConsumeEtcOutNext-NestedItemsToConsumeEtcOutOut).
transform_semantics_into_d_lists_in_items_to_consume(ItemsToConsume, ItemsToConsume1, 
						     NestedItemsToConsumeEtcIn, NestedItemsToConsumeEtcOut) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [transform_semantics_into_d_lists_in_items_to_consume(ItemsToConsume, ItemsToConsume1, 
								     NestedItemsToConsumeEtcIn, NestedItemsToConsumeEtcOut)]),
	fail.

% *** TO DO: SHOULD COVER RECURSIVE CASE HERE ***
%transform_semantics_into_d_lists_in_item_to_consume(Concat, Var, 
%						    ItemsToConsumeEtcInIn-ItemsToConsumeEtcInOut,
%						    ItemsToConsumeEtcOutIn-ItemsToConsumeEtcOutOut) :-
%	is_strict_concat(Concat, ConcatVars, ItemsToConsume),
%	NewItemToConsumeEtcIn = [ItemsToConsume, ConcatVars, Var],
%	NewItemToConsumeEtcOut = [[], ConcatVars, []],
%	ItemsToConsumeEtcInIn = [NewItemToConsumeEtcIn | ItemsToConsumeEtcInOut],
%	ItemsToConsumeEtcOutIn = [NewItemToConsumeEtcOut | ItemsToConsumeEtcOutOut],
%	!.
transform_semantics_into_d_lists_in_item_to_consume(Concat, Var, 
						    ItemsToConsumeEtcInIn-ItemsToConsumeEtcInOut,
						    ItemsToConsumeEtcOutIn-ItemsToConsumeEtcOutOut) :-
	is_strict_concat(Concat, ConcatVars, ItemsToConsume),
	transform_semantics_into_d_lists_in_items_to_consume(ItemsToConsume, ItemsToConsume1, 
							     ItemsToConsumeEtcInNext-ItemsToConsumeEtcInOut,
							     ItemsToConsumeEtcOutNext-ItemsToConsumeEtcOutOut),
	NewItemToConsumeEtcIn = [ItemsToConsume1, ConcatVars, Var],
	NewItemToConsumeEtcOut = [[], ConcatVars, []],
	ItemsToConsumeEtcInIn = [NewItemToConsumeEtcIn | ItemsToConsumeEtcInNext],
	ItemsToConsumeEtcOutIn = [NewItemToConsumeEtcOut | ItemsToConsumeEtcOutNext],
	!.
transform_semantics_into_d_lists_in_item_to_consume(Term, Term,
						    ItemsToConsumeEtcInIn-ItemsToConsumeEtcInIn,
						    ItemsToConsumeEtcOutIn-ItemsToConsumeEtcOutIn) :-
	\+ compound(Term),
	!.
transform_semantics_into_d_lists_in_item_to_consume(Term, Term1, 
						    ItemsToConsumeEtcInIn-ItemsToConsumeEtcInOut,
						    ItemsToConsumeEtcOutIn-ItemsToConsumeEtcOutOut) :-
	functor(Term, F, N),
	functor(Term1, F, N),
	transform_semantics_into_d_lists_in_item_to_consume_args(N, Term, Term1,
								 ItemsToConsumeEtcInIn-ItemsToConsumeEtcInOut,
								 ItemsToConsumeEtcOutIn-ItemsToConsumeEtcOutOut),
	!.

transform_semantics_into_d_lists_in_item_to_consume_args(0, _Term, _Term1,
							 ItemsToConsumeEtcInIn-ItemsToConsumeEtcInIn,
							 ItemsToConsumeEtcOutIn-ItemsToConsumeEtcOutIn) :-
	!.
transform_semantics_into_d_lists_in_item_to_consume_args(I, Term, Term1, 
							 ItemsToConsumeEtcInIn-ItemsToConsumeEtcInOut,
							 ItemsToConsumeEtcOutIn-ItemsToConsumeEtcOutOut) :-
	I > 0,
	arg(I, Term, Arg),
	arg(I, Term1, Arg1),
	transform_semantics_into_d_lists_in_item_to_consume(Arg, Arg1, 
							    ItemsToConsumeEtcInIn-ItemsToConsumeEtcInNext,
							    ItemsToConsumeEtcOutIn-ItemsToConsumeEtcOutNext),
	I1 is I - 1,
	transform_semantics_into_d_lists_in_item_to_consume_args(I1, Term, Term1, 
							 ItemsToConsumeEtcInNext-ItemsToConsumeEtcInOut,
							 ItemsToConsumeEtcOutNext-ItemsToConsumeEtcOutOut).


transform_semantics_into_d_lists_for_body(BodyIn, DListSemCats, ItemsToConsumeEtcIn-ItemsToConsumeEtcOut, BodyOut) :-
	add_consume_items_to_body(BodyOut, ItemsToConsumeEtcIn-ItemsToConsumeEtcNext, BodyMain),
	transform_semantics_into_d_lists_for_body1(BodyIn, DListSemCats, ItemsToConsumeEtcNext-ItemsToConsumeEtcOut, BodyMain).

convert_peeks_in_body(peek(Item), ArgsIn, {consume_item(ArgsIn, Item, _)}) :-
	!.
convert_peeks_in_body((P, Q), ArgsIn, (P1, Q1)) :-
	convert_peeks_in_body(P, ArgsIn, P1),
	convert_peeks_in_body(Q, ArgsIn, Q1),
	!.
convert_peeks_in_body((P ; Q), ArgsIn, (P1 ; Q1)) :-
	convert_peeks_in_body(P, ArgsIn, P1),
	convert_peeks_in_body(Q, ArgsIn, Q1),
	!.
convert_peeks_in_body(Other, _ArgsIn, Other) :-
	!.

add_consume_items_to_body(Body, []-[], Body) :-
	!.
add_consume_items_to_body(BodyIn,
			  [ItemsToConsumeEtcInF | ItemsToConsumeEtcInR]-[ItemsToConsumeEtcOutF | ItemsToConsumeEtcOutR],
			  BodyOut) :-
	add_consume_items_to_body1(BodyIn, ItemsToConsumeEtcInF, ItemsToConsumeEtcOutF, BodyNext),
	!,
	add_consume_items_to_body(BodyNext, ItemsToConsumeEtcInR-ItemsToConsumeEtcOutR, BodyOut).	

add_consume_items_to_body1(BodyIn, ItemsToConsumeEtcIn, ItemsToConsumeEtcOut, BodyOut) :-
	ItemsToConsumeEtcIn = [ItemsToConsume, ConcatVars, ArgsIn],
	ItemsToConsumeEtcOut = [[], ConcatVars, ArgsOut],
	add_consume_items_to_body2(ItemsToConsume, ArgsIn-ArgsOut, BodyIn-BodyOut),
	!.
add_consume_items_to_body1(BodyIn, ItemsToConsumeEtcIn, ItemsToConsumeEtcOut, BodyOut) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [add_consume_items_to_body1(BodyIn, ItemsToConsumeEtcIn, ItemsToConsumeEtcOut, BodyOut)]),
	fail.

add_consume_items_to_body2([], ArgsIn-ArgsIn, Body-Body) :-
	!.
add_consume_items_to_body2([F | R], ArgsIn-ArgsOut, BodyIn-BodyOut) :-
	ConsumeItem = {consume_item(ArgsIn, F, ArgsNext)},
	BodyIn = (ConsumeItem, BodyNext),
	add_consume_items_to_body2(R, ArgsNext-ArgsOut, BodyNext-BodyOut),
	!.

transform_semantics_into_d_lists_for_body1(Var, _DListSemCats, ItemsToConsumeEtcIn-ItemsToConsumeEtcIn, Var) :-
	var(Var),
	!.
% Conjunction: thread
transform_semantics_into_d_lists_for_body1((P, Q), DListSemCats, ItemsToConsumeEtcIn-ItemsToConsumeEtcOut, (P1, Q1)) :-
	transform_semantics_into_d_lists_for_body1(P, DListSemCats, ItemsToConsumeEtcIn-ItemsToConsumeEtcNext, P1),
	transform_semantics_into_d_lists_for_body1(Q, DListSemCats, ItemsToConsumeEtcNext-ItemsToConsumeEtcOut, Q1),
	!.
% Disjunction: split
transform_semantics_into_d_lists_for_body1((P ; Q), DListSemCats, ItemsToConsumeEtcIn-ItemsToConsumeEtcOut, (P1 ; Q1)) :-
	transform_semantics_into_d_lists_for_body1(P, DListSemCats, ItemsToConsumeEtcIn-ItemsToConsumeEtcOut, P1),
	transform_semantics_into_d_lists_for_body1(Q, DListSemCats, ItemsToConsumeEtcIn-ItemsToConsumeEtcOut, Q1),
	!.
% D-list cat, and part of concat in ItemsToConsumeEtcIn: thread
transform_semantics_into_d_lists_for_body1(Cat, DListSemCats, ItemsToConsumeEtcIn-ItemsToConsumeEtcOut, Cat1) :-
	Cat =.. [CatName, Tree, Feats, Sem],
	member(CatName, DListSemCats),
	var(Sem),
	extract_sem_from_items_to_consume_etc(ItemsToConsumeEtcIn, ItemsToConsumeEtcOut, Sem, ArgsIn, ArgsOut),
	Cat1 =.. [CatName, Tree, Feats, ArgsIn-ArgsOut],
	!.
% D-list cat, but not part of concat for mother: add null out-arg
transform_semantics_into_d_lists_for_body1(Cat, DListSemCats, ItemsToConsumeEtcIn-ItemsToConsumeEtcIn, Cat1) :-
	Cat =.. [CatName, Tree, Feats, Sem],
	member(CatName, DListSemCats),
	Cat1 =.. [CatName, Tree, Feats, Sem-[]],
	!.
% Other: keep unchanged
transform_semantics_into_d_lists_for_body1(Other, _DListSemCats, ItemsToConsumeEtcIn-ItemsToConsumeEtcIn, Other) :-
	!.
transform_semantics_into_d_lists_for_body1(Cat, DListSemCats, ConcatVars, Args, Cat1) :-
	format2error('~N*** Error: bad call: ~w~n', [transform_semantics_into_d_lists_for_body1(Cat, DListSemCats, ConcatVars, Args, Cat1)]),
	fail.

extract_sem_from_items_to_consume_etc([F | R], [F1 | R], Var, ArgsIn, ArgsOut) :-
	extract_sem_from_item_to_consume_etc(F, F1, Var, ArgsIn, ArgsOut),
	!.
extract_sem_from_items_to_consume_etc([F | R], [F | R1], Var, ArgsIn, ArgsOut) :-
	extract_sem_from_items_to_consume_etc(R, R1, Var, ArgsIn, ArgsOut),
	!.

extract_sem_from_item_to_consume_etc(ItemsToConsumeEtcIn, ItemsToConsumeEtcOut, Var, ArgsIn, ArgsOut) :-
	ItemsToConsumeEtcIn = [ItemsToConsume, ConcatVars, ArgsIn],
	id_member(Var, ConcatVars),
	ItemsToConsumeEtcOut = [ItemsToConsume, ConcatVars, ArgsOut],
	!.

% Case 1a: lexical rule, d-list cat, AFF semantics
transform_semantics_into_d_lists_for_lexical_rule((Head --> Body), DListSemCats, GeneralLexRule, SpecificLexRule) :-
	Head =.. [HeadCatName, HeadTree, HeadFeats, HeadSem],
	member(HeadCatName, DListSemCats),
	HeadSem = [FullSemItem],
	nonvar(FullSemItem),
	FullSemItem = (Tag = SemItem),
	GeneralHead =.. [HeadCatName, GenHeadTree, GenHeadFeats, ArgsIn-ArgsOut],
	ConsumeItem = {consume_item(ArgsIn, (GenTag = GenSemItem), ArgsOut)},
	GeneralBody = concat_lexicon(GenSemItem, GenTag, HeadCatName, GenHeadFeats, GenHeadTree),
	SpecificHead = concat_lexicon(SemItem, Tag, HeadCatName, HeadFeats, HeadTree),
	GeneralLexRule = ( GeneralHead --> ( ConsumeItem, GeneralBody ) ),
	SpecificLexRule = ( SpecificHead --> Body ),
	!.
% Case 1b: lexical rule, d-list cat
transform_semantics_into_d_lists_for_lexical_rule((Head --> Body), DListSemCats, GeneralLexRule, SpecificLexRule) :-
	Head =.. [HeadCatName, HeadTree, HeadFeats, HeadSem],
	member(HeadCatName, DListSemCats),
	HeadSem = [SemItem],
	GeneralHead =.. [HeadCatName, GenHeadTree, GenHeadFeats, ArgsIn-ArgsOut],
	ConsumeItem = {consume_item(ArgsIn, GenSemItem, ArgsOut)},
	GeneralBody = concat_lexicon(GenSemItem, HeadCatName, GenHeadFeats, GenHeadTree),
	SpecificHead = concat_lexicon(SemItem, HeadCatName, HeadFeats, HeadTree),
	GeneralLexRule = ( GeneralHead --> ( ConsumeItem, GeneralBody ) ),
	SpecificLexRule = ( SpecificHead --> Body ),
	!.
% Case 2: lexical rule, not d-list cat
transform_semantics_into_d_lists_for_lexical_rule((Head --> Body), DListSemCats, GeneralLexRule, SpecificLexRule) :-
	Head =.. [HeadCatName, HeadTree, HeadFeats, HeadSem],
	\+ member(HeadCatName, DListSemCats),
	HeadSem = SemItem,
	GeneralHead =.. [HeadCatName, GenHeadTree, GenHeadFeats, GenSemItem],
	GeneralBody = non_concat_lexicon(GenSemItem, HeadCatName, GenHeadFeats, GenHeadTree),
	SpecificHead = non_concat_lexicon(SemItem, HeadCatName, HeadFeats, HeadTree),
	GeneralLexRule = ( GeneralHead --> GeneralBody ),
	SpecificLexRule = ( SpecificHead --> Body ),
	!.
transform_semantics_into_d_lists_for_lexical_rule(Rule, DListSemCats, GeneralLexRule, SpecificLexRule) :-
	format2error('~N*** Error: bad call: ~w~n', [transform_semantics_into_d_lists_for_lexical_rule(Rule, DListSemCats, GeneralLexRule, SpecificLexRule)]),
	fail.	

%---------------------------------------------------------------

/*
- Post-process
  - Add extra args and clauses to support iterative deepening
  - Sort all rules and remove duplicates
*/

post_process_rules(RulesIn, RulesOut) :-
	add_depth_checking_to_rules(RulesIn, Rules1),
	make_all_rules_ground(Rules1, Rules2),
	%% we want to preserve the order between the rules:
	safe_remove_duplicates_preserving_order(Rules2, RulesOut).
	%% sort(Rules2, RulesOut).

add_depth_checking_to_rules([], []).
add_depth_checking_to_rules([F | R], [F1 | R1]) :-
	add_depth_checking_to_rule(F, F1),
	!,
	add_depth_checking_to_rules(R, R1).

add_depth_checking_to_rule(LexicalRule, LexicalRule) :-
	LexicalRule = (Head --> _Body),
	nonvar(Head),
	functor(Head, F, _N),
	( F = concat_lexicon ; F = non_concat_lexicon ),
	!.
add_depth_checking_to_rule((HIn --> BIn), (HOut --> BOut)) :-
	add_depth_checking_to_non_terminal(HIn, HOut, CostIn, CostOut, CostMax),
	add_depth_checking_to_rule_body(BIn, BOutMain, CostNext, CostOut, CostMax),
	rule_cost((HIn --> BIn), RuleCost),
	CostInc = { CostNext is CostIn + RuleCost },
	CostCheck = { CostNext =< CostMax },
	BOut = ( CostInc, CostCheck, BOutMain ),
	!.
add_depth_checking_to_rule(RuleIn, RuleOut) :-
	format2error('~N*** Error: bad call: ~w~n', [add_depth_checking_to_rule(RuleIn, RuleOut)]),
	fail.	

add_depth_checking_to_rule_body(Var, Var, CostIn, CostIn, _CostMax) :-
	var(Var),
	!.
add_depth_checking_to_rule_body((P, Q), (P1, Q1), CostIn, CostOut, CostMax) :-
	add_depth_checking_to_rule_body(P, P1, CostIn, CostNext, CostMax),
	add_depth_checking_to_rule_body(Q, Q1, CostNext, CostOut, CostMax),
	!.
%%%% Changes by Peter Ljunglöf, 11 dec 2007
%%%% Fixes a bug where one of the alternatives is lexical,
%%%% as described in bug 1845840 "Optional arguments don't work with generation"
add_depth_checking_to_rule_body((P ; Q), (P1,PPost ; Q1,QPost), CostIn, CostOut, CostMax) :-
	add_depth_checking_to_rule_body(P, P1, CostIn, PCostOut, CostMax),
	add_depth_checking_to_rule_body(Q, Q1, CostIn, QCostOut, CostMax),
	PPost = {CostOut = PCostOut},
	QPost = {CostOut = QCostOut},
	!.
%%%% Old code for previous clause:
%% add_depth_checking_to_rule_body((P ; Q), (P1 ; Q1), CostIn, CostOut, CostMax) :-
%% 	add_depth_checking_to_rule_body(P, P1, CostIn, CostOut, CostMax),
%% 	add_depth_checking_to_rule_body(Q, Q1, CostIn, CostOut, CostMax),
%% 	!.
%%%% End changes
add_depth_checking_to_rule_body(Cat, Cat1, CostIn, CostOut, CostMax) :-
	is_cat(Cat),
	add_depth_checking_to_non_terminal(Cat, Cat1, CostIn, CostOut, CostMax),
	!.
add_depth_checking_to_rule_body(Other, Other, CostIn, CostIn, _CostMax) :-
	!.

add_depth_checking_to_non_terminal(Cat, Cat1, CostIn, CostOut, CostMax) :-
	Cat =.. [CatName, Tree, Feats, Sem],
	Cat1 =.. [CatName, Tree, Feats, Sem, CostIn, CostOut, CostMax],
	!.
add_depth_checking_to_non_terminal(Cat, Cat1, CostIn, CostOut, CostMax) :-
	format2error('~N*** Error: bad call: ~w~n', [add_depth_checking_to_non_terminal(Cat, Cat1, CostIn, CostOut, CostMax)]),
	fail.

rule_cost((H --> B), RuleCost) :-
	cat_has_null_semantics(H),
	body_is_not_null(B),
	!,
	RuleCost = 5.
rule_cost(_Rule, RuleCost) :-
	RuleCost = 1.

%---------------------------------------------------------------

/*

Extract a declaration like

semantic_element([physical_object,fork]).

from an compiled lexicon entry like

(concat_lexicon([physical_object,fork],A,physical_object_type,['*role*'=A],phrase(physical_object_type,line_info(283,508-508,'d:/cygwin/home/speech/call-slt/int/regulus/interlingua.regulus'),lex(fork)))-->[fork]).

or a extract

semantic_element([utterance_type,state_possession])

and

semantic_element([seating,table])

from a compiled rule like

(request(phrase(request,line_info(79,113-119,'d:/cygwin/home/speech/call-slt/int/regulus/interlingua.regulus'),(A,lex('SAY'),lex('HAVE_RESERVATION'),B)),['*role*'=C],D-E,F,G,H)-->{I
is
F+1},{I=<H},{consume_item(D,C=[utterance_type,state_possession],J)},{consume_item(J,arg2=[seating,table],K)},optional_politeness(A,['*role*'=C],K-L,I,M,H),['SAY'],['HAVE_RESERVATION'],table_properties(B,['*role*'=C],L-E,M,G,H)).

*/

get_semantic_elements_from_rules([], In-In).
get_semantic_elements_from_rules([F | R], In-Out) :-
	get_semantic_elements_from_rule(F, In-Next),
	!,
	get_semantic_elements_from_rules(R, Next-Out).

% (concat_lexicon([onoff,allumé],onoff,[gender=bv(0,0,1),sem_np_type=A,singplur=bv(0,1,1)],phrase(onoff,line_info(38,82-82,'d:/cygwin/home/speech/regulus/examples/toy1/regulus/french.regulus'),lex(allumée)))-->[allumée]).

get_semantic_elements_from_rule(Rule, [Elt | Out]-Out) :-
	Rule = (concat_lexicon(Elt, _Cat, _Feats, _Tree) --> _Body),
	no_vars_or_grounded_vars_in_term(Elt),
	!.

% (concat_lexicon([physical_object,fork],A,physical_object_type,['*role*'=A],phrase(physical_object_type,line_info(283,508-508,'d:/cygwin/home/speech/call-slt/int/regulus/interlingua.regulus'),lex(fork)))-->[fork]).

get_semantic_elements_from_rule(Rule, [Elt | Out]-Out) :-
	Rule = (concat_lexicon(Elt, _Var, _Cat, _Feats, _Tree) --> _Body),
	no_vars_or_grounded_vars_in_term(Elt),
	!.
/*

(request(phrase(request,line_info(79,113-119,'d:/cygwin/home/speech/call-slt/int/regulus/interlingua.regulus'),(A,lex('SAY'),lex('HAVE_RESERVATION'),B)),['*role*'=C],D-E,F,G,H)-->{I
is
F+1},{I=<H},{consume_item(D,C=[utterance_type,state_possession],J)},{consume_item(J,arg2=[seating,table],K)},optional_politeness(A,['*role*'=C],K-L,I,M,H),['SAY'],['HAVE_RESERVATION'],table_properties(B,['*role*'=C],L-E,M,G,H)).

*/

get_semantic_elements_from_rule((_Head --> Body), In-Out) :-
	get_semantic_elements_from_rule_body(Body, In-Out),
	!.
get_semantic_elements_from_rule(_Other, In-In).

get_semantic_elements_from_rule_body((F, R), In-Out) :-
	get_semantic_elements_from_rule_body(F, In-Next),
	!,
	get_semantic_elements_from_rule_body(R, Next-Out).
get_semantic_elements_from_rule_body({consume_item(_, [Key, Value], _)},
				     [[Key, Value] | Out]-Out) :-
	no_vars_or_grounded_vars_in_term([Key, Value]),
	!.
get_semantic_elements_from_rule_body({consume_item(_, (_Role=[Key, Value]), _)},
				     [[Key, Value] | Out]-Out) :-
	no_vars_or_grounded_vars_in_term([Key, Value]),
	!.
get_semantic_elements_from_rule_body(_Other, In-In).

%---------------------------------------------------------------
 
/*
  - Add module declaration
  - Add support for consume_item/3
  - Write to file
*/

write_rules_to_file(Rules, SemConstants, SemElements,
		    ModuleName, GenerationPred, TopLevelCat, TopLevelFeatName, IncrementalDeepeningParams, File) :-
	construct_module_declarations(ModuleName, GenerationPred, ModuleDeclarations),
	construct_top_level_rules(GenerationPred, TopLevelCat, TopLevelFeatName, IncrementalDeepeningParams, TopLevelRules),
	construct_sem_constant_decls(SemConstants, SemConstantDecls),
	construct_sem_element_decls(SemElements, SemElementDecls),
	append_list([ModuleDeclarations, TopLevelRules, Rules, SemConstantDecls, SemElementDecls], AllItems),
	absolute_file_name(File, AbsoluteFile),
	%list_to_prolog_file_with_encoding(AllItems, AbsoluteFile, 'UTF-16LE'),
	(   ( current_predicate(user:regulus_config/2), user:regulus_config(generator_grammar_encoding, Encoding) ) ->
	    list_to_prolog_file_with_encoding(AllItems, AbsoluteFile, Encoding)
	;
	    otherwise ->
	    list_to_prolog_file(AllItems, AbsoluteFile)
	),
	format('~NGenerator written to file ~w~n', [AbsoluteFile]).

construct_module_declarations(ModuleName, GenerationPred, ModuleDeclarations) :-
	ModuleDecl = ( :- module(ModuleName, [GenerationPred/3]) ),
	UseModuleDecls = 
    [   ( :- use_module('$REGULUS/Prolog/generator_runtime') ),
	( :- use_module(library(lists)) )
    ],
	ModuleDeclarations = [ ModuleDecl | UseModuleDecls ],
	!.
construct_module_declarations(ModuleName, ModuleDeclarations) :-
	format2error('~N*** Error: bad call: ~w~n', [construct_module_declarations(ModuleName, ModuleDeclarations)]),
	fail.

construct_sem_constant_decls([], []) :-
	!.
construct_sem_constant_decls([F | R], [F1 | R1]) :-
	construct_sem_constant_decl(F, F1),
	!,
	construct_sem_constant_decls(R, R1).
construct_sem_constant_decls(X, Y) :-
	format2error('~N*** Error: bad call: ~w~n', [construct_sem_constant_decls(X, Y)]),
	fail.

construct_sem_constant_decl(Const, generation_sem_constant(Const)) :-
	!.

construct_sem_element_decls([], []) :-
	!.
construct_sem_element_decls([F | R], [F1 | R1]) :-
	construct_sem_element_decl(F, F1),
	!,
	construct_sem_element_decls(R, R1).
construct_sem_element_decls(X, Y) :-
	format2error('~N*** Error: bad call: ~w~n', [construct_sem_element_decls(X, Y)]),
	fail.

construct_sem_element_decl(Const, sem_element(Const)) :-
	!.

construct_top_level_rules(GenerationPred, TopLevelCat, TopLevelFeatName, IncrementalDeepeningParams, TopLevelRules) :-
	construct_generate_up_to_depth_rule(TopLevelCat, TopLevelFeatName, GenerateUpToDepthRule),
	construct_iterative_deepening_rules(GenerationPred, IncrementalDeepeningParams, IterativeDeepeningRules),
	append(IterativeDeepeningRules, [GenerateUpToDepthRule], TopLevelRules0),
	make_all_rules_ground(TopLevelRules0, TopLevelRules),
	!.
construct_top_level_rules(GenerationPred, TopLevelCat, TopLevelFeatName, TopLevelRules) :-
	format2error('~N*** Error: bad call: ~w~n', [construct_top_level_rules(GenerationPred, TopLevelCat, TopLevelFeatName, TopLevelRules)]),
	fail.	

construct_generate_up_to_depth_rule(TopLevelCat, TopLevelFeatName, GenerateUpToDepthRule) :-
	RuleHead = generate_up_to_depth(Sem, Tree, Words, MaxDepth),
	RuleBody =.. [TopLevelCat, Tree, [], [TopLevelFeatName=Sem], 0, _Depth, MaxDepth, Words, []],
	GenerateUpToDepthRule = ( RuleHead :- RuleBody ).

construct_iterative_deepening_rules(GenerationPred, IncrementalDeepeningParams, IterativeDeepeningRules) :-
	construct_top_level_rule(GenerationPred, IncrementalDeepeningParams, TopRule),
	construct_non_top_iterative_deepening_rules(NonTopIterativeDeepeningRules),
	IterativeDeepeningRules = [TopRule | NonTopIterativeDeepeningRules],
	!.
construct_iterative_deepening_rules(GenerationPred, IncrementalDeepeningParams, IterativeDeepeningRules) :-
	format2error('~N*** Error: bad call: ~w~n', [construct_iterative_deepening_rules(GenerationPred, IncrementalDeepeningParams, IterativeDeepeningRules)]),
	fail.	

construct_top_level_rule(GenerationPred, IncrementalDeepeningParams, TopRule) :-
	check_incremental_deepening_parameters(IncrementalDeepeningParams),
	RuleHead =.. [GenerationPred, Sem, Tree, Words],
	IncrementalDeepeningParams = [InitDepth, Incr, MaxDepth],
	RuleBody = iteratively_generate_up_to_depth(Sem, Tree, Words, InitDepth, Incr, MaxDepth),
	TopRule = ( RuleHead :- RuleBody ),
	!.
construct_top_level_rule(GenerationPred, IncrementalDeepeningParams, TopRule) :-
	format2error('~N*** Error: bad call: ~w~n', [construct_top_level_rule(GenerationPred, IncrementalDeepeningParams, TopRule)]),
	fail.

check_incremental_deepening_parameters(IncrementalDeepeningParams) :-
	nonvar(IncrementalDeepeningParams),
	IncrementalDeepeningParams =  [InitDepth, Incr, MaxDepth],
	number(InitDepth),
	number(Incr),
	number(MaxDepth),
	MaxDepth >= InitDepth,
	MaxDepth >= Incr,
	Incr > 0,
	!.
check_incremental_deepening_parameters(IncrementalDeepeningParams) :-
	format2error('~N*** Error: bad value of incremental deepening parameters: ~w.', [IncrementalDeepeningParams]),
	format2error('~N    Should be of form [<Start>, <Incr>, <Max>], with <Start>, <Incr> =< <Max> and <Incr> > 0', []),
	fail.

construct_non_top_iterative_deepening_rules([Clause1, Clause2]) :-
	non_top_iterative_deepening_rules_clause1(Clause1),
	non_top_iterative_deepening_rules_clause2(Clause2),
	!.

non_top_iterative_deepening_rules_clause1(Clause1) :-
	Clause1 = 
    (   iteratively_generate_up_to_depth(Sem, Tree, Words, Depth, _Incr, MaxDepth) :-
	Depth =< MaxDepth,
	copy_term([Sem, Tree], [SemCopy, TreeCopy]),
	findall([Tree1, Words1, Sem1],
		(   [SemCopy, TreeCopy] = [Sem1, Tree1],
		    generate_up_to_depth(Sem, Tree1, Words1, Depth)
		),
		Tuples),
	Tuples \== [],
	!,
	member([Tree, Words, Sem], Tuples)
    ).
non_top_iterative_deepening_rules_clause2(Clause2) :-
	Clause2 = 
    (   iteratively_generate_up_to_depth(Sem, Tree, Words, Depth, Incr, MaxDepth) :-
	NextDepth is Depth + Incr,
	NextDepth =< MaxDepth,
	iteratively_generate_up_to_depth(Sem, Tree, Words, NextDepth, Incr, MaxDepth)
    ).

%---------------------------------------------------------------

% Utilities

is_cat(Cat) :-
	compound(Cat),
	functor(Cat, _CatName, 3),
	!.

cat_name_and_sem_for_cat(Cat, CatName, Sem) :-
	compound(Cat),
	functor(Cat, CatName, 3),
	arg(3, Cat, Sem),
	!.
cat_name_and_sem_for_cat(Cat, CatName, Sem) :-
	format2error('~N*** Error: bad call: ~w~n', [cat_name_and_sem_for_cat(Cat, CatName, Sem)]),
	fail.

cat_has_nontrivial_semantics(Cat) :-
	current_predicate(regulus_pred:category_internal/2),
	compound(Cat),
	functor(Cat, CatName, 3),
	regulus_pred:category_internal(CatName, Feats),
	member(sem, Feats),
	!.

is_strict_concat(Sem, ConcatVars, ConcatNonVars) :-
	compound(Sem),
	functor(Sem, concat, 2),
	is_concat1(Sem, ConcatVars-[], ConcatNonVars-[]).

all_concat_vars_in_term(Sem, AllVarsIn-AllVarsOut) :-
	is_concat1(Sem, AllVarsIn-IndirectVars, ConcatNonVars-[]),
	indirect_concat_vars(ConcatNonVars, IndirectVars-AllVarsOut),
	!.
all_concat_vars_in_term(_Other, AllVarsIn-AllVarsIn).

indirect_concat_vars(Concat, VarsIn-VarsOut) :-
	compound(Concat),
	functor(Concat, concat, 2),
	!,
	all_concat_vars_in_term(Concat, VarsIn-VarsOut).
indirect_concat_vars(Atom, Vars-Vars) :-
	atomic(Atom),
	!.
indirect_concat_vars(Var, Vars-Vars) :-
	var(Var),
	!.
indirect_concat_vars([F | R], VarsIn-VarsOut) :-
	indirect_concat_vars(F, VarsIn-VarsNext),
	!,
	indirect_concat_vars(R, VarsNext-VarsOut).

is_concat(Sem, ConcatVars, ConcatNonVars) :-
	(   ( compound(Sem), functor(Sem, concat, 2) ) ;
	    is_list(Sem)
	),
	is_concat1(Sem, ConcatVars-[], ConcatNonVars-[]).

is_concat1(Var, [Var | VarsOut]-VarsOut, NonVarsIn-NonVarsIn) :-
	var(Var),
	!.
is_concat1(concat(L, R), VarsIn-VarsOut, NonVarsIn-NonVarsOut) :-
	is_concat1(L, VarsIn-VarsNext, NonVarsIn-NonVarsNext),
	is_concat1(R, VarsNext-VarsOut, NonVarsNext-NonVarsOut),
	!.
is_concat1([Role | R], VarsIn-VarsOut, NonVarsIn-NonVarsOut) :-
	nonvar(Role),
	Role = [role, _RoleName, RoleBody],
	is_concat1(RoleBody, VarsIn-VarsNext, NonVarsIn-NonVarsNext),
	is_concat1(R, VarsNext-VarsOut, NonVarsNext-NonVarsOut),
	!.
is_concat1([F | R], VarsIn-VarsOut, [F | NonVarsNext]-NonVarsOut) :-
	is_concat1(R, VarsIn-VarsOut, NonVarsNext-NonVarsOut),
	!.
is_concat1([], VarsIn-VarsIn, NonVarsIn-NonVarsIn) :-
	!.

% Look for vars occuring as arguments of lambda-applications.
lambda_application_vars(Var, VarsIn-VarsIn) :-
	var(Var),
	!.
lambda_application_vars(Atom, VarsIn-VarsIn) :-
	atomic(Atom),
	!.
lambda_application_vars([apply, LambdaForm, Var], [Var | VarsOut]-VarsOut) :-
	safe_subsumes_chk([lambda, _, _], LambdaForm),
	var(Var),
	!.
lambda_application_vars(Term, VarsIn-VarsOut) :-
	compound(Term),
	functor(Term, _F, N),
	lambda_application_vars_args(N, Term, VarsIn-VarsOut).

lambda_application_vars_args(0, _Term, VarsIn-VarsIn).
lambda_application_vars_args(I, Term, VarsIn-VarsOut) :-
	I > 0,
	arg(I, Term, Arg),
	lambda_application_vars(Arg, VarsIn-VarsNext),
	I1 is I - 1,
	!,
	lambda_application_vars_args(I1, Term, VarsNext-VarsOut).

lexical_rule(Rule, DListSemCats) :-
	nonvar(Rule),
	Rule = ( Head --> _Body ),
	cat_name_and_sem_for_cat(Head, HeadCatName, HeadSem),
	(   ground(HeadSem)
	;
	    nonvar(HeadSem),
	    HeadSem = [_Tag = Pair],
	    ground(Pair)
	),
	(   member(HeadCatName, DListSemCats) ->
	    is_list(HeadSem),
	    length(HeadSem, 1),
	    \+ is_gap_var(HeadSem)
	;
	    true
	).

cat_has_null_semantics(Cat) :-
	cat_name_and_sem_for_cat(Cat, _CatName, Sem),
	nonvar(Sem),
	Sem = In-Out,
	In == Out,
	!.

body_is_not_null(Var) :-
	var(Var),
	!.
body_is_not_null((P, Q)) :-
	!,
	(   body_is_not_null(P) ;
	    body_is_not_null(Q)
	).
body_is_not_null((P ; Q)) :-
	!,
	(   body_is_not_null(P) ;
	    body_is_not_null(Q)
	).
body_is_not_null(Cat) :-
	is_cat(Cat),
	!.
body_is_not_null(List) :-
	is_list(List),
	List \== [].

make_all_rules_ground([], []).
make_all_rules_ground([F | R], [F1 | R1]) :-
	copy_term(F, F1),
	make_ground(F1),
	!,
	make_all_rules_ground(R, R1).	

var_lists_share_member(L1, L2) :-
	member(X, L1),
	id_member(X, L2),
	!.

	
	
	
