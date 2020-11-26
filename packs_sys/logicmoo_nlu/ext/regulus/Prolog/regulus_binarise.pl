% regulus2dcg.pl

%---------------------------------------------------------------

:- module(regulus_binarise,
	  [binarise_grammar_alist/4]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/regulus_declarations').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(ordsets)).


:- dynamic cat_counter/1.

binarise_grammar_alist(InGrammarUnits, OutGrammarUnits, NewCats, NewFeats) :-
	binarise_grammar_init,
	binarise_grammar_units(InGrammarUnits, OutGrammarUnits-[], NewCats-[], NewFeats0-[]),
	safe_remove_duplicates(NewFeats0, NewFeats).

%---------------------------------------------------------------

binarise_grammar_init :-
	retractall(cat_counter(_)),
	assertz(cat_counter(0)).

%---------------------------------------------------------------

binarise_grammar_units([], GrammarIn-GrammarIn, CatsIn-CatsIn, FeatsIn-FeatsIn).
binarise_grammar_units([ignore(_F) | R], GrammarIn-GrammarOut, CatsIn-CatsOut, FeatsIn-FeatsOut) :-
	!,
	binarise_grammar_units(R, GrammarIn-GrammarOut, CatsIn-CatsOut, FeatsIn-FeatsOut).
binarise_grammar_units([F | R], GrammarIn-GrammarOut, CatsIn-CatsOut, FeatsIn-FeatsOut) :-
	binarise_grammar_unit(F, GrammarIn-GrammarNext, CatsIn-CatsNext, FeatsIn-FeatsNext),
	!,
	binarise_grammar_units(R, GrammarNext-GrammarOut, CatsNext-CatsOut, FeatsNext-FeatsOut).

binarise_grammar_unit(examples(RuleSummary, Examples), RulesIn-RulesOut, CatsIn-CatsIn, FeatsIn-FeatsIn) :-
	RulesIn = [examples(RuleSummary, Examples) | RulesOut],
	!.
binarise_grammar_unit(frequency_labelled_rule(Freq, Rule), RulesIn-RulesOut, CatsIn-CatsOut, FeatsIn-FeatsOut) :-
	binarise_grammar_unit1(Rule, RulesIn-RulesOut, CatsIn-CatsOut, FeatsIn-FeatsOut, Freq),
	!.
binarise_grammar_unit(GrammarUnit, Rules, Cats, Feats) :-
	format2error('~N*** Error: bad call: ~w~n', [binarise_grammar_unit(GrammarUnit, Rules, Cats, Feats)]),
	fail.

binarise_grammar_unit1(Rule, RulesIn-RulesOut, CatsIn-CatsOut, FeatsIn-FeatsOut, Freq) :-
	is_non_lexical_regulus_rule(Rule),
	!,
	binarise_non_lexical_rule(Rule, RulesIn-RulesOut, CatsIn-CatsOut, FeatsIn-FeatsOut, Freq).
binarise_grammar_unit1(Other, [frequency_labelled_rule(Freq, Other) | RulesOut]-RulesOut,
		       CatsIn-CatsIn, FeatsIn-FeatsIn, Freq).

binarise_non_lexical_rule((Head --> Body), RulesIn-RulesOut, CatsIn-CatsOut, FeatsIn-FeatsOut, Freq) :-
	length_of_comma_list(Body, 0-Length),
	Length =< 2,
	!,
	RulesIn = [frequency_labelled_rule(Freq, (Head --> Body)) | RulesOut],
	CatsIn = CatsOut,
	FeatsIn = FeatsOut.
binarise_non_lexical_rule((Head --> Body), RulesIn-RulesOut, CatsIn-CatsOut, FeatsIn-FeatsOut, Freq) :-
	comma_list_to_list(Body, BodyList),
	binarise_non_lexical_rule1([Head | BodyList], BinarisedBodies-[], CatsIn-CatsOut, FeatsIn-FeatsOut),
	binarised_bodies_to_new_rules(BinarisedBodies, RulesIn-RulesOut, Freq).

%---------------------------------------------------------------

% binarise_non_lexical_rule1(Body, BinarisedIn-BinarisedOut, CatsIn-CatsOut, FeatsIn-FeatsOut)

binarise_non_lexical_rule1(Body, BinarisedIn-BinarisedOut, CatsIn-CatsIn, FeatsIn-FeatsIn) :-
	length(Body, Length),
	Length =< 3,
	BinarisedIn = [Body | BinarisedOut],
	!.
binarise_non_lexical_rule1(Body, BinarisedIn-BinarisedOut, CatsIn-CatsOut, FeatsIn-FeatsOut) :-
	Body = [Mother, FirstDaughter | RestDaughters],
	new_mother_cat(Mother, FirstDaughter, RestDaughters, NewMother, FeatsIn-FeatsNext),
	cat_name_and_features_in_cat(NewMother, CatName, Features),
	NewCatSpec = category(CatName, Features),
	CatsIn = [NewCatSpec | CatsNext],
	BinarisedBody = [Mother, FirstDaughter, NewMother],
	BinarisedIn = [BinarisedBody | BinarisedNext],
	binarise_non_lexical_rule1([NewMother | RestDaughters], BinarisedNext-BinarisedOut, CatsNext-CatsOut, FeatsNext-FeatsOut).

%---------------------------------------------------------------

new_mother_cat(Mother, FirstDaughter, RestDaughters, NewMother, FeatsIn-FeatsOut) :-
	term_variables([Mother, FirstDaughter], MotherFirstDaughterVars),
	term_variables(RestDaughters, RestDaughtersVars),
	sem_variables_in_cat(Mother, MotherSemVars),

	list_to_ord_set(MotherFirstDaughterVars, MotherFirstDaughterVarsOS),
	list_to_ord_set(RestDaughtersVars, RestDaughtersVarsOS),
	list_to_ord_set(MotherSemVars, MotherSemVarsOS),

	ord_intersection(MotherFirstDaughterVarsOS, RestDaughtersVarsOS, CommonVarsOS),
	ord_intersection(CommonVarsOS, MotherSemVarsOS, CommonSemVarsOS),
	ord_subtract(CommonVarsOS, MotherSemVarsOS, CommonSynVarsOS),

	assign_feats_to_syn_vars(CommonSynVarsOS, RestDaughters, SynFeatValList, [], FeatsIn-FeatsOut),
	assign_feats_to_sem_vars(CommonSemVarsOS, SemFeatValList),

	(   SemFeatValList = [] ->
	    FeatValList = SynFeatValList ;
	    FeatValList = [sem=SemFeatValList | SynFeatValList]
	),

	new_cat_id(NewCat),
	NewMother = NewCat:FeatValList.

%---------------------------------------------------------------

% assign_feats_to_syn_vars(Vars, Cats, FeatVals, PreviousFeats, FeatsIn-FeatsOut)

assign_feats_to_syn_vars([], _Cats, [], _PreviousFeats, FeatsIn-FeatsIn).
assign_feats_to_syn_vars([Var | RVars], Cats, [(Feat = Var) | RFeatVals], PreviousFeats, FeatsIn-FeatsOut) :-
	assign_feat_to_syn_var_from_cat_list(Var, Cats, Feat, PreviousFeats, FeatsIn-FeatsNext),
	assign_feats_to_syn_vars(RVars, Cats, RFeatVals, [Feat | PreviousFeats], FeatsNext-FeatsOut).

assign_feat_to_syn_var_from_cat_list(Var, Cats, Feat, PreviousFeats, FeatsIn-FeatsOut) :-
	assign_feat_to_syn_var_from_cat_list1(Var, Cats, Feat, PreviousFeats, FeatsIn-FeatsOut),
	!.
assign_feat_to_syn_var_from_cat_list(Var, Cats, Feat, Prev, Feats) :-
	format2error('~N*** Error: bad call: ~q.~n', [assign_feat_to_syn_var_from_cat_list(Var, Cats, Feat, Prev, Feats)]),
	fail.

assign_feat_to_syn_var_from_cat_list1(Var, [Cat | _Cats], Feat, PreviousFeats, FeatsIn-FeatsOut) :-
	assign_feat_to_syn_var_from_cat(Var, Cat, Feat, PreviousFeats, FeatsIn-FeatsOut),
	!.
assign_feat_to_syn_var_from_cat_list1(Var, [_Cat | Cats], Feat, PreviousFeats, FeatsIn-FeatsOut) :-
	assign_feat_to_syn_var_from_cat_list1(Var, Cats, Feat, PreviousFeats, FeatsIn-FeatsOut).

assign_feat_to_syn_var_from_cat(Var, _Cat:FeatVals, Feat, PreviousFeats, FeatsIn-FeatsOut) :-
	assign_feat_to_syn_var_from_feat_vals(Var, FeatVals, Feat, PreviousFeats, FeatsIn-FeatsOut).

assign_feat_to_syn_var_from_feat_vals(Var, [(Feat1 = Var1) | R], Feat, PreviousFeats, FeatsIn-FeatsOut) :-
	(   Var == Var1 ->
	    assign_feat_to_syn_var_from_feat_val(Feat1, PreviousFeats, Feat, FeatsIn-FeatsOut) ;
	    assign_feat_to_syn_var_from_feat_vals(Var, R, Feat, PreviousFeats, FeatsIn-FeatsOut)
	).

assign_feat_to_syn_var_from_feat_val(Feat1, PreviousFeats, Feat, FeatsIn-FeatsOut) :-
	\+ member(Feat1, PreviousFeats),
	!,
	Feat = Feat1,
	FeatsOut = FeatsIn.
assign_feat_to_syn_var_from_feat_val(Feat1, PreviousFeats, Feat, FeatsIn-FeatsOut) :-
	feat_variant_not_in_list(Feat1, PreviousFeats, 1, Feat),
	get_feat_val_space_for_feat(Feat1, FeatValSpaceId),
	NewFeatDecl = feature(Feat, FeatValSpaceId),
	(   ignored_feat(Feat1) ->
	    NewIgnoreDecl = ignore_feature(Feat),
	    FeatsIn = [NewFeatDecl, NewIgnoreDecl | FeatsOut]
	;
	    otherwise ->
	    FeatsIn = [NewFeatDecl | FeatsOut]
	).

get_feat_val_space_for_feat(Feat, FeatValSpaceId) :-
	feature(Feat, FeatValSpaceId),
	!.
get_feat_val_space_for_feat(Feat, _FeatValSpaceId) :-
	format2error('~NError: unable to find feature declaration for feature "~w"~n', [Feat]),
	fail.

feat_variant_not_in_list(Feat1, PreviousFeats, I, Feat) :-
	join_with_underscore([Feat1, I], FeatI),
	(   \+ member(FeatI, PreviousFeats) ->
	    Feat = FeatI ;
	    I1 is I + 1,
	    feat_variant_not_in_list(Feat1, PreviousFeats, I1, Feat)
	).

%---------------------------------------------------------------

assign_feats_to_sem_vars(Vars, FeatVals) :-
	assign_feats_to_sem_vars1(Vars, FeatVals, 1).

assign_feats_to_sem_vars1([], [], _Counter).
assign_feats_to_sem_vars1([Var | RestVars], [(SemFeatName = Var) | RestFeatVars], Counter) :-
	join_with_underscore([sem, Counter], SemFeatName),
	Counter1 is Counter + 1,
	!,
	assign_feats_to_sem_vars1(RestVars, RestFeatVars, Counter1).

%---------------------------------------------------------------

% binarised_bodies_to_new_rules(BinarisedBodies, RulesIn-RulesOut, Freq)

binarised_bodies_to_new_rules([], RulesIn-RulesIn, _Freq).
binarised_bodies_to_new_rules([F | R], [Rule | RulesNext]-RulesOut, Freq) :-
	binarised_body_to_new_rule(F, Rule, Freq),
	!,
	binarised_bodies_to_new_rules(R, RulesNext-RulesOut, Freq).

binarised_body_to_new_rule(RuleBody, frequency_labelled_rule(Freq, Rule), Freq) :-
	RuleBody = [Mother | Daughters],
	list_to_comma_list(Daughters, Daughters1),
	copy_term( ( Mother --> Daughters1 ), Rule).

%---------------------------------------------------------------

new_cat_id(NewId) :-
	new_cat_counter(Counter),
	safe_number_codes(Counter, CounterChars),
	append_list(["tmp_cat_", CounterChars], NewIdChars),
	atom_codes(NewId, NewIdChars).

%---------------------------------------------------------------

new_cat_counter(Counter) :-
	cat_counter(Counter),
	NextCounter is Counter + 1,
	retractall(cat_counter(_)),
	assertz(cat_counter(NextCounter)).

%---------------------------------------------------------------

cat_name_and_features_in_cat(CatName:FeatVals, CatName, Features) :-
	features_in_feat_val_list(FeatVals, Features).

features_in_feat_val_list([], []).
features_in_feat_val_list([(Feat=_Val) | R], [Feat | R1]) :-
	features_in_feat_val_list(R, R1).

%---------------------------------------------------------------

sem_variables_in_cat(Cat, AllSemVars) :-
	Cat = _CatName:FeatVals,
	(   member((sem=SemVal), FeatVals) ->
	    term_variables(SemVal, SemVars) ;
	    SemVars = []),
	(   member((gsem=GSemVal), FeatVals) ->
	    term_variables(GSemVal, GSemVars) ;
	    GSemVars = []),
	append(SemVars, GSemVars, AllSemVars),
	!.

%---------------------------------------------------------------

ignored_feat(Feat) :-
	regulus_preds:ignore_feature(Feat),
	!.
ignored_feat(Feat) :-
	get_ebl_ignore_feats(IgnoredFeats),
	member(Feat, IgnoredFeats),
	!.

get_ebl_ignore_feats(IgnoredFeats) :-	
	current_predicate(user:regulus_config/2),
	user:regulus_config(ebl_ignore_feats, IgnoredFeats),
	!.
	
			    
