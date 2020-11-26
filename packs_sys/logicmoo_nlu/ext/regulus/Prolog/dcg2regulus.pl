% dcg2regulus.pl

%---------------------------------------------------------------

:- module(dcg2regulus,
	  [prolog_dcg_rule_to_regulus/2]
      ).

%---------------------------------------------------------------

:- use_module(library(lists)).
:- use_module(library(terms)).

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/regulus_declarations').

%---------------------------------------------------------------
 
prolog_dcg_rule_to_regulus((Head :- Body), (RegulusHead --> RegulusBody)) :-
	(   remove_merge_globals_from_body(Body, Value, Body1) ->
	    GSemValue = Value
	;
	    otherwise ->
	    GSemValue = no_value,
	    Body1 = Body
	),
	add_extra_feats_for_shared_vars_in_booleans((Head, Body1), (Head2, Body2)),
	normalise_prolog_dcg_to_c_form((Head2 :- Body2), (Head3 :- Body3)),
	prolog_dcg_head_to_regulus(Head3, GSemValue, Body3, RegulusHead),
	prolog_dcg_body_to_regulus(Body3, RegulusBody0),
	compact_empty_lists_in_regulus_body(RegulusBody0, RegulusBody1),
	flatten_regulus_body(RegulusBody1, RegulusBody),
	!.
prolog_dcg_rule_to_regulus(X, Y) :-
	format2error('~N*** Error: call failed: ~w~n', [prolog_dcg_rule_to_regulus(X, Y)]),
	fail.

%---------------------------------------------------------------
 
normalise_prolog_dcg_to_c_form((HeadIn :- BodyIn), (HeadOut :- BodyOut)) :-
	normalise_prolog_dcg_cat_to_c_form(HeadIn, HeadOut, HeadCGoals, BeforeOrAfter),
	normalise_prolog_dcg_body_to_c_form(BodyIn, BodyNext),
	(   BeforeOrAfter = before ->
	    BodyOut = (HeadCGoals, BodyNext)
	;
	    otherwise ->
	    BodyOut = (BodyNext, HeadCGoals)
	),
	!.
normalise_prolog_dcg_to_c_form(X, Y) :-
	format2error('~N*** Error: call failed: ~w~n', [normalise_prolog_dcg_to_c_form(X, Y)]),
	fail.

normalise_prolog_dcg_body_to_c_form((F, R), (F1, R1)) :-
	!,
	normalise_prolog_dcg_body_to_c_form(F, F1),
	normalise_prolog_dcg_body_to_c_form(R, R1).
normalise_prolog_dcg_body_to_c_form('C'(In, LexItem, Out), 'C'(In, LexItem, Out)) :-
	!.
normalise_prolog_dcg_body_to_c_form(true, true) :-
	!.
normalise_prolog_dcg_body_to_c_form(Cat, CatOut) :-
	compound(Cat),
	functor(Cat, _CatName, N),
	member(N, [6, 7]),
	normalise_prolog_dcg_cat_to_c_form(Cat, CatNext, CatCGoals, BeforeOrAfter),
	(   BeforeOrAfter = before ->
	    CatOut = (CatCGoals, CatNext)
	;
	    otherwise ->
	    CatOut = (CatNext, CatCGoals)
	),
	!.
normalise_prolog_dcg_body_to_c_form(X, Y) :-
	format2error('~N*** Error: call failed: ~w~n', [normalise_prolog_dcg_body_to_c_form(X, Y)]),
	fail.

normalise_prolog_dcg_cat_to_c_form(CatIn, CatOut, CatCGoals, BeforeOrAfter) :-
	CatIn =.. [CatName, Tree, Feats, SemValue, In, Out],
	CatOut =.. [CatName, Tree, Feats, SemValue, In1, Out1],
	normalise_dlist_to_c_form(In-Out, In1-Out1, CatCGoals, BeforeOrAfter),
	!.
normalise_prolog_dcg_cat_to_c_form(CatIn, CatOut, CatCGoals, BeforeOrAfter) :-
	CatIn =.. [CatName, Tree, Feats, SemValue, GSem, In, Out],
	CatOut =.. [CatName, Tree, Feats, SemValue, GSem, In1, Out1],
	normalise_dlist_to_c_form(In-Out, In1-Out1, CatCGoals, BeforeOrAfter),
	!.
normalise_prolog_dcg_cat_to_c_form(CatIn, CatOut, CatCGoals, BeforeOrAfter) :-
	format2error('~N*** Error: call failed: ~w~n',
	       [normalise_prolog_dcg_cat_to_c_form(CatIn, CatOut, CatCGoals, BeforeOrAfter)]),
	fail.

normalise_dlist_to_c_form(In-Out, In-Out, true, _BeforeOrAfter) :-
	var(In),
	var(Out),
	!.
normalise_dlist_to_c_form([Word | R]-Out, NewInVar-Out1, (CGoal, RestCGoals), before) :-
	atom(Word),
	CGoal = 'C'(NewInVar, Word, NextInVar),
	!,
	normalise_dlist_to_c_form(R-Out, NextInVar-Out1, RestCGoals, before).
normalise_dlist_to_c_form(In-[Word | R], In1-NewOutVar, (CGoal, RestCGoals), after) :-
	atom(Word),
	CGoal = 'C'(NextOutVar, Word, NewOutVar),
	!,
	normalise_dlist_to_c_form(In-R, In1-NextOutVar, RestCGoals, after).

%---------------------------------------------------------------

prolog_dcg_head_to_regulus(Head, GSemValue, Body1, RegulusHead) :-
	Head =.. [CatName, _Tree, Feats, SemValue, _GSem, _In, _Out],
	dcg_head_sem_feats(CatName, SemValue, GSemValue, Body1, HeadSemFeats),
	print_form_for_syn_feats(Feats, SynFeats),
	append(HeadSemFeats, SynFeats, RegulusFeats),
	RegulusHead = CatName:RegulusFeats.

prolog_dcg_body_to_regulus((F, R), (F1, R1)) :-
	!,
	prolog_dcg_body_to_regulus(F, F1),
	prolog_dcg_body_to_regulus(R, R1).
prolog_dcg_body_to_regulus('C'(_In, LexItem, _Out), LexItem) :-
	!.
prolog_dcg_body_to_regulus(true, []) :-
	!.
prolog_dcg_body_to_regulus(BodyGoal, RegulusCat) :-
	BodyGoal =.. [CatName, _Tree, Feats, SemValue, _GSem, _In, _Out],
	print_form_for_syn_feats(Feats, SynFeats),
	(   category_has_sem_feat(CatName) ->
	    RegulusCat = CatName:[sem=SemValue | SynFeats] ;
	    RegulusCat = CatName:SynFeats
	).

%---------------------------------------------------------------

dcg_head_sem_feats(_CatName, _SemValue, GSemValue, _Body1, HeadSemFeats) :-
	dif(GSemValue, no_value),
	!,
	HeadSemFeats = [gsem=GSemValue].
dcg_head_sem_feats(CatName, _SemValue, _GSemValue, _Body1, HeadSemFeats) :-
	\+ category_has_sem_feat(CatName),
	!,
	HeadSemFeats = [].
dcg_head_sem_feats(_CatName, SemValue, GSemValue, Body1, HeadSemFeats) :-
	GSemValue = no_value,
	var(SemValue),
	term_variables(Body1, BodyVars),
	!,
	(   id_member(SemValue, BodyVars) ->
	    HeadSemFeats = [sem=SemValue] ;
	    HeadSemFeats = []
	).
dcg_head_sem_feats(_CatName, SemValue, GSemValue, _Body1, HeadSemFeats) :-
	GSemValue = no_value,
	!,
	HeadSemFeats = [sem=SemValue].

%---------------------------------------------------------------

remove_merge_globals_from_body((F, merge_globals(Value, _)), Value, F) :-
	!.
remove_merge_globals_from_body((F, R), Value, (F, R1)) :-
	remove_merge_globals_from_body(R, Value, R1).

%---------------------------------------------------------------

add_extra_feats_for_shared_vars_in_booleans(GoalsIn, GoalsOut) :-
	link_vars_for_booleans(GoalsIn, BooleanLinkVarAssoc),
	add_extra_feats_for_shared_vars_in_booleans1(GoalsIn, BooleanLinkVarAssoc, GoalsOut).

link_vars_for_booleans(Goals, BooleanLinkVarAssoc) :-
	link_vars_for_booleans1(Goals, []-_Booleans, []-SharedBooleans),
	pair_shared_booleans_with_link_vars(SharedBooleans, BooleanLinkVarAssoc).

link_vars_for_booleans1(Var, BooleansIn-BooleansIn, SharedBooleansIn-SharedBooleansIn) :-
	var(Var),
	!.
link_vars_for_booleans1(GroundTerm, BooleansIn-BooleansIn, SharedBooleansIn-SharedBooleansIn) :-
	ground(GroundTerm),
	!.
link_vars_for_booleans1(Boolean, BooleansIn-BooleansOut, SharedBooleansIn-SharedBooleansOut) :-
	functor(Boolean, bv, _),
	!,
	(   id_member(Boolean, SharedBooleansIn) ->
	    BooleansOut = BooleansIn,
	    SharedBooleansOut = SharedBooleansIn ;

	    id_member(Boolean, BooleansIn) ->
	    SharedBooleansOut = [Boolean | SharedBooleansIn],
	    BooleansOut = BooleansIn ;
	    
	    BooleansOut = [Boolean | BooleansIn],
	    SharedBooleansOut = SharedBooleansIn
	).
link_vars_for_booleans1(T, BooleansIn-BooleansOut, SharedBooleansIn-SharedBooleansOut) :-
	functor(T, _F, N),
	link_vars_for_booleans1_args(N, T, BooleansIn-BooleansOut, SharedBooleansIn-SharedBooleansOut).

link_vars_for_booleans1_args(0, _T, BooleansIn-BooleansIn, SharedBooleansIn-SharedBooleansIn).
link_vars_for_booleans1_args(I, T, BooleansIn-BooleansOut, SharedBooleansIn-SharedBooleansOut) :-
	I > 0,
	arg(I, T, Arg),
	link_vars_for_booleans1(Arg, BooleansIn-BooleansNext, SharedBooleansIn-SharedBooleansNext),
	I1 is I - 1,
	!,
	link_vars_for_booleans1_args(I1, T, BooleansNext-BooleansOut, SharedBooleansNext-SharedBooleansOut).

pair_shared_booleans_with_link_vars([], []).
pair_shared_booleans_with_link_vars([F | R], [(F-_NewVar) | R1]) :-
	pair_shared_booleans_with_link_vars(R, R1).

add_extra_feats_for_shared_vars_in_booleans1((F, R), BooleanLinkVarAssoc, (F1, R1)) :-
	!,
	add_extra_feats_for_shared_vars_in_booleans1(F, BooleanLinkVarAssoc, F1),
	add_extra_feats_for_shared_vars_in_booleans1(R, BooleanLinkVarAssoc, R1).
add_extra_feats_for_shared_vars_in_booleans1('C'(In, LexItem, Out), _BooleanLinkVarAssoc, 'C'(In, LexItem, Out)) :-
	!.
add_extra_feats_for_shared_vars_in_booleans1(true, _BooleanLinkVarAssoc, true) :-
	!.
add_extra_feats_for_shared_vars_in_booleans1(Goal, BooleanLinkVarAssoc, Goal1) :-
	compound(Goal),
	functor(Goal, CatName, 6),
	!,
	Goal =.. [CatName, Tree, Feats, SemValue, GSem, In, Out],
	Goal1 =.. [CatName, Tree, Feats1, SemValue, GSem, In, Out],
	add_extra_feats_for_shared_vars_in_booleans1_feats(Feats, BooleanLinkVarAssoc, Feats1).

add_extra_feats_for_shared_vars_in_booleans1_feats([], _BooleanLinkVarAssoc, []).
add_extra_feats_for_shared_vars_in_booleans1_feats([(F=B) | R], BooleanLinkVarAssoc, [(F=B), (F=V) | R1]) :-
	get_link_var_from_boolean_link_var_assoc(B, BooleanLinkVarAssoc, V),
	!,
	add_extra_feats_for_shared_vars_in_booleans1_feats(R, BooleanLinkVarAssoc, R1).
add_extra_feats_for_shared_vars_in_booleans1_feats([(F=B) | R], BooleanLinkVarAssoc, [(F=B) | R1]) :-
	add_extra_feats_for_shared_vars_in_booleans1_feats(R, BooleanLinkVarAssoc, R1).

get_link_var_from_boolean_link_var_assoc(B, [B1-V | _R], V) :-
	B == B1,
	!.
get_link_var_from_boolean_link_var_assoc(B, [_F | R], V) :-
	get_link_var_from_boolean_link_var_assoc(B, R, V).

%---------------------------------------------------------------

compact_empty_lists_in_regulus_body((F, R), Result) :-
	!,
	compact_empty_lists_in_regulus_body(F, F1),
	compact_empty_lists_in_regulus_body(R, R1),
	(   F1 = [] ->
	    Result = R1
	;
	    R1 = [] ->
	    Result = F1
	;
	    otherwise ->
	    Result = (F1, R1)
	).
compact_empty_lists_in_regulus_body(Other, Other).	  
 
%---------------------------------------------------------------

flatten_regulus_body((F, R), Result) :-
	flatten_regulus_body(F, F1),
	flatten_regulus_body(R, R1),
	comma_list_to_list(F1, F1List),
	comma_list_to_list(R1, R1List),
	append(F1List, R1List, ResultList),
	list_to_comma_list(ResultList, Result),
	!.
flatten_regulus_body(Other, Other) :-
	!.

%---------------------------------------------------------------

category_has_sem_feat(CatName) :-
	category_internal(CatName, AllCatFeats),
	member(sem, AllCatFeats),
	!.
