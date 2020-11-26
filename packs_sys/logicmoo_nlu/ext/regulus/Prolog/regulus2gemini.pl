% regulus2gemini

%---------------------------------------------------------------

:- module(regulus2gemini,
	  [regulus2gemini/2,
	   regulus2gemini_for_multiple_grammars/4
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/Prolog/regulus_declarations').
:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/ebl_postprocess').
:- use_module('$REGULUS/Prolog/regulus_read').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
%:- use_module(library(terms)).
%:- use_module(library(ordsets)).
%:- use_module(library(assoc)).
%:- use_module(library(system)).

%---------------------------------------------------------------

regulus2gemini(InFile, OutFile) :-
	timed_call(regulus2gemini1(InFile, OutFile), TimeTaken),
	format('~NTranslation finished, ~2f secs~n', [TimeTaken]).

%---------------------------------------------------------------

regulus2gemini_for_multiple_grammars(_BaseInFile, _IgnoreFeatsFile, [], _OutFile).
regulus2gemini_for_multiple_grammars(BaseInFile, IgnoreFeatsFile, [Tag | Tags], OutFile) :-
	regulus2gemini_for_multiple_grammars1(BaseInFile, IgnoreFeatsFile, Tag, OutFile),
	!,
	regulus2gemini_for_multiple_grammars(BaseInFile, IgnoreFeatsFile, Tags, OutFile).

regulus2gemini_for_multiple_grammars1(BaseInFile, IgnoreFeatsFile, Tag, BaseOutFile) :-
	format('~N~n==========================================================================~n', []),
	format('~N~nCompiling grammar for tag "~w"~n~n', [Tag]),
	add_tag_to_files_in_list([BaseInFile, BaseOutFile], Tag, [InFile, OutFile]),
	regulus2gemini([InFile, IgnoreFeatsFile], OutFile),
	!.	

%---------------------------------------------------------------


regulus2gemini1(InFile, OutFile) :-
	on_exception(
	Exception, 
	regulus2gemini2(InFile, OutFile),
	inform_about_top_level_regulus_error(Exception)
    ).

regulus2gemini2(InFiles, OutFile) :-
	retract_all_regulus_preds,
	read_regulus_file_or_files(InFiles, ReadRules, ReadDeclarations),
	pre_process_regulus_rules_for_gemini(ReadRules, PreProcessedRules),

	regulus_decls_to_gemini(ReadDeclarations, GeminiDeclarations, TopLevelCats),
	gemini_value_space_decls(GeminiValueSpaceDecls),
	
	regulus_rules_to_gemini(PreProcessedRules, GeminiSynRules, GeminiSemRules0, GeminiLexEntries),
	make_gemini_sem_rules_compositional(GeminiSemRules0, GeminiSemRules, NewSemFeatDecls, CatsAndNewFeats, TopLevelCats),
	adjust_category_decls_to_add_new_feats(GeminiDeclarations, GeminiDeclarations1, CatsAndNewFeats),

	% Value space decls need to go first...
	append_list([GeminiValueSpaceDecls, NewSemFeatDecls, GeminiDeclarations1], AllGeminiDeclarations),

	gemini_top_rules(TopLevelCats, PrefDecl, TopSynRules, TopSemRules),

	append([PrefDecl | TopSynRules], GeminiSynRules, AllGeminiSynRules),
	append(TopSemRules, GeminiSemRules, AllGeminiSemRules),
	write_rules_and_decls_to_gemini_files(AllGeminiDeclarations,
					      AllGeminiSynRules, AllGeminiSemRules, GeminiLexEntries,
					      OutFile),
	!.
regulus2gemini2(_InFile, _OutFile) :-
	inform_about_top_level_regulus_error(failed).

%---------------------------------------------------------------

pre_process_regulus_rules_for_gemini(ReadRules, PreProcessedRules) :-
	format('~N -- Pre_processing rules... ', []), flush_output(user),
	timed_call(pre_process_regulus_rules_for_gemini_main(ReadRules, PreProcessedRules), TimeTaken),
	format('~1f secs~n~n', [TimeTaken]).

pre_process_regulus_rules_for_gemini_main(ReadRules, ExpandedRules) :-
	findall(
	ExpandedRule, 
	pre_process_regulus_rule_for_gemini_in_list(ReadRules, ExpandedRule), 
	ExpandedRules),

	\+ member(failed_expansion, ExpandedRules).

pre_process_regulus_rule_for_gemini_in_list(Rules, Result) :-
	member(rule(Rule, _LineInfo), Rules),
	regulus_debug_call(1, inform_about_expanding_rule(Rule, LineInfo)),
	on_exception(
	Exception, 
	pre_process_rule_for_gemini(Rule, Result), 
	( inform_about_regulus_exception(Exception, LineInfo), Result = failed_expansion )
    ).

%---------------------------------------------------------------

% pre_process_rule_for_gemini(+Rule, -PreProcessedRule)

pre_process_rule_for_gemini(Rule, PreProcessedRule) :-
	expand_disjunctions_for_gemini(Rule, ExpandedRule),
	remove_nulls_for_gemini(ExpandedRule, PreProcessedRule).

expand_disjunctions_for_gemini((H --> B), (H1 --> B1)) :-
	!,
	expand_disjunctions_for_gemini(H, H1),
	expand_disjunctions_for_gemini(B, B1).
expand_disjunctions_for_gemini((P, Q), (P1, Q1)) :-
	!,
	expand_disjunctions_for_gemini(P, P1),
	expand_disjunctions_for_gemini(Q, Q1).
expand_disjunctions_for_gemini((P ; Q), Result) :-
	!,
	(   expand_disjunctions_for_gemini(P, Result) ;
	    expand_disjunctions_for_gemini(Q, Result)
	).
expand_disjunctions_for_gemini((?P), Result) :-
	!,
	(   expand_disjunctions_for_gemini(P, Result) ;
	    Result = '*null*',
	    instantiate_sem_vars_to_null_values(P)
	).
expand_disjunctions_for_gemini(Atom, Atom) :-
	atomic(Atom),
	!.
expand_disjunctions_for_gemini(Cat:FeatsWithVals, GeminiCat) :-
	!,
	internalise_cat_for_gemini(Cat, FeatsWithVals, GeminiCat).
expand_disjunctions_for_gemini(Other, _) :-
	regulus_error('NBad subterm in rule: ~q.~n', [Other]).


%---------------------------------------------------------------

instantiate_sem_vars_to_null_values((P, Q)) :-
	!,
	instantiate_sem_vars_to_null_values(P),
	instantiate_sem_vars_to_null_values(Q).
instantiate_sem_vars_to_null_values((P ; Q)) :-
	!,
	instantiate_sem_vars_to_null_values(P),
	instantiate_sem_vars_to_null_values(Q).
instantiate_sem_vars_to_null_values((?P)) :-
	!,
	instantiate_sem_vars_to_null_values(P).
instantiate_sem_vars_to_null_values(Atom) :-
	atomic(Atom),
	!.
instantiate_sem_vars_to_null_values(_Cat:FeatsWithVals) :-
	!,
	instantiate_sem_vars_to_null_values_in_feats_list(FeatsWithVals).
instantiate_sem_vars_to_null_values(Other) :-
	regulus_error('NBad call: ~q.~n', [instantiate_sem_vars_to_null_values(Other)]).

instantiate_sem_vars_to_null_values_in_feats_list([]).
instantiate_sem_vars_to_null_values_in_feats_list([(Feat=Val) | R]) :-
	(   member(Feat, [sem, gsem]) ->
	    instantiate_sem_vars_to_null_values_in_semval(Val) ;
	    true
	),
	instantiate_sem_vars_to_null_values_in_feats_list(R).

instantiate_sem_vars_to_null_values_in_semval(V) :-
	var(V),
	V = '*null_value*',
	!.
instantiate_sem_vars_to_null_values_in_semval(A) :-
	atomic(A),
	!.
instantiate_sem_vars_to_null_values_in_semval(T) :-
	functor(T, _F, N),
	instantiate_sem_vars_to_null_values_in_semval_args(N, T).

instantiate_sem_vars_to_null_values_in_semval_args(0, _T).
instantiate_sem_vars_to_null_values_in_semval_args(I, T) :-
	arg(I, T, Arg),
	instantiate_sem_vars_to_null_values_in_semval(Arg),
	I1 is I - 1,
	!,
	instantiate_sem_vars_to_null_values_in_semval_args(I1, T).

%---------------------------------------------------------------
% Modified version of internalise_cat/4 from regulus.pl

% Note that we remove the ignored features before checking validity of
% the feat-val list, since otherwise we'll get an error - if there are
% any ignored features, they'll be missing from the internal category
% declaration too.

internalise_cat_for_gemini(CatName, FeatsWithVals, cat(SemVal, CatName1:GeminiFeatsWithVals)) :-
	atom(CatName),
	regulus_cat_name_to_gemini(CatName, CatName1),
	get_category_internal(CatName, AllFeats),
	give_up_if_undeclared_feats_in_feat_val_list(CatName, AllFeats, FeatsWithVals),
	remove_ignored_feat_val_pairs_from_list(FeatsWithVals, FeatsWithVals1),
	replace_vals_with_substitutes_in_feat_val_list(FeatsWithVals1, FeatsWithVals2),
	is_valid_feat_val_list(FeatsWithVals2, AllFeats),
	remove_sem_and_gsem_feats_from_feat_val_list(FeatsWithVals2, GeminiFeatsWithVals, SemVal),
	!.
internalise_cat_for_gemini(CatName, FeatsWithVals, _) :-
	regulus_error('~NCouldn''t translate category: ~q~n', [CatName:FeatsWithVals]).

%---------------------------------------------------------------

regulus_cat_name_to_gemini(CatName, CatName1) :-
	atom_codes(CatName, Chars),
	(   Chars = [0'. | RestChars] ->
	    atom_codes(CatName1, RestChars) ;
	    CatName1 = CatName
	).

%---------------------------------------------------------------

remove_sem_and_gsem_feats_from_feat_val_list([], [], '*null_value*').
remove_sem_and_gsem_feats_from_feat_val_list([(SemFeat = SemVal) | R], R, SemVal) :-
	member(SemFeat, [sem, gsem]),
	!.
remove_sem_and_gsem_feats_from_feat_val_list([F | R], [F | R1], SemVal) :-
	!,
	remove_sem_and_gsem_feats_from_feat_val_list(R, R1, SemVal).

%---------------------------------------------------------------

remove_nulls_for_gemini((H --> B), (H1 --> B1)) :-
	!,
	remove_nulls_for_gemini(H, H1),
	remove_nulls_for_gemini(B, B1).
remove_nulls_for_gemini((P, '*null*'), P1) :-
	!,
	remove_nulls_for_gemini(P, P1).
remove_nulls_for_gemini(('*null*', P), P1) :-
	!,
	remove_nulls_for_gemini(P, P1).
remove_nulls_for_gemini((P, Q), (P1, Q1)) :-
	!,
	remove_nulls_for_gemini(P, P1),
	remove_nulls_for_gemini(Q, Q1).
remove_nulls_for_gemini(Atom, Atom) :-
	atomic(Atom),
	!.
remove_nulls_for_gemini(cat(SemVal, Cat:FeatsWithVals), cat(SemVal, Cat:FeatsWithVals)) :-
	!.
remove_nulls_for_gemini(Other, _) :-
	regulus_error('NBad subterm in rule: ~q.~n', [Other]).

%---------------------------------------------------------------

regulus_decls_to_gemini([], [], []).
regulus_decls_to_gemini([F0 | R], Out, TopLevelCats) :-
	F0 = declaration(_Label, F, _LineInfo),
	regulus_decl_to_gemini(F, F1),
	(   F1 = discard ->
	    [Out, TopLevelCats] = [Out1, TopLevelCats1] ;

	    F1 = top_level_category(Cat) ->
	    [Out, TopLevelCats] = [Out1, [Cat | TopLevelCats1]] ;

	    [Out, TopLevelCats] = [[F1 | Out1], TopLevelCats1]
	),
	!,
	regulus_decls_to_gemini(R, Out1, TopLevelCats1).

% The feature_value_space declarations may have been combined from several files.
regulus_decl_to_gemini(feature_value_space(_Name, _Space), discard) :-
	!.
regulus_decl_to_gemini(feature_value_space_substitution(_Val1, _Val2, _SpaceId), discard) :-
	!.
regulus_decl_to_gemini(specialises(_Val1, _Val2, _SpaceId), discard) :-
	!.
regulus_decl_to_gemini(ignore_specialises(_Val1, _Val2, _SpaceId), discard) :-
	!.
regulus_decl_to_gemini(macro(_Val1, _Val2), discard) :-
	!.
regulus_decl_to_gemini(default_macro(_Val1, _Val2), discard) :-
	!.
regulus_decl_to_gemini(external_grammar(_Val1, _Val2), discard) :-
	!.
regulus_decl_to_gemini(feature(Feat, Space), feature(Feat, Space)) :-
	!.
regulus_decl_to_gemini(lexical_feature_default(_Feat, _Val), discard) :-
	!.
regulus_decl_to_gemini(feature_instantiation_schedule(_Schedule), discard) :-
	!.
regulus_decl_to_gemini(ignore_feature(_Feat), discard) :-
	!.
% Categories decls may be macro-expanded
regulus_decl_to_gemini(category(CatName, _OriginalFeats), category(CatName1, Feats1)) :-
	!,
	regulus_cat_name_to_gemini(CatName, CatName1),
	category_internal(CatName, Feats),
	remove_sem_and_gsem_from_list(Feats, Feats1).
regulus_decl_to_gemini(top_level_category(CatName), top_level_category(CatName1)) :-
	regulus_cat_name_to_gemini(CatName, CatName1),
	!.
regulus_decl_to_gemini( Other, _) :-
	regulus_error('~NUnable to translate regulus declaration ~q~n', [Other]).

remove_sem_and_gsem_from_list([], []).
remove_sem_and_gsem_from_list([F | R], Out) :-
	(   member(F, [sem, gsem]) ->
	    Out = R1 ;
	    Out = [F | R1]
	),
	remove_sem_and_gsem_from_list(R, R1).

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

% The feature_value_space declarations may have been combined from several files.

% We also need to define lf_fragment as a logical_form_valueset, in case make_gemini_sem_rules_compositional
% adds new semantic features.

gemini_value_space_decls(GeminiValueSpaceDecls) :-
	findall(feature_value_space(Name, Space), feature_value_space(Name, Space), GeminiValueSpaceDecls0),

	LFFragmentDecl1 = sem_feature_value_space(lf_fragment, [atom, concat(_,_), [], [_|_]]),
	LFFragmentDecl2 = logical_form_valueset(lf_fragment),

	GeminiValueSpaceDecls = [LFFragmentDecl1, LFFragmentDecl2 | GeminiValueSpaceDecls0].

%---------------------------------------------------------------

regulus_rules_to_gemini(RegulusRules, GeminiSynRules, GeminiSemRules, GeminiLexEntries) :-
	regulus_rules_to_gemini1(RegulusRules, []-GeminiSynRules0, []-GeminiSemRules0, []-GeminiLexEntries0, 0),
	reverse(GeminiSynRules0, GeminiSynRules),
	reverse(GeminiSemRules0, GeminiSemRules),
	reverse(GeminiLexEntries0, GeminiLexEntries).

regulus_rules_to_gemini1([], Syn-Syn, Sem-Sem, Lex-Lex, _Counter) :-
	!.
regulus_rules_to_gemini1([F | R], SynIn-SynOut, SemIn-SemOut, LexIn-LexOut, Counter) :-
	is_lexical_regulus_rule(F),
	!,
	regulus_lex_entry_to_gemini(F, F1),
	LexNext = [F1 | LexIn],
	regulus_rules_to_gemini1(R, SynIn-SynOut, SemIn-SemOut, LexNext-LexOut, Counter).
regulus_rules_to_gemini1([F | R], SynIn-SynOut, SemIn-SemOut, LexIn-LexOut, Counter) :-
	!,
	regulus_rule_to_gemini(F, Syn, Sem, Counter, Counter1),	
	SynNext = [Syn | SynIn],
	SemNext = [Sem | SemIn],
	regulus_rules_to_gemini1(R, SynNext-SynOut, SemNext-SemOut, LexIn-LexOut, Counter1).

%---------------------------------------------------------------

regulus_rule_to_gemini(RegulusRule, Syn, Sem, Counter, Counter1) :-
	gemini_rule_id(Counter, RuleId, Counter1),

	Syn = syn(RuleId, rule_group, SynBody),
	Sem = sem(RuleId, SemBody),
	
	regulus_rule_body_to_gemini(RegulusRule, SynBody, syn),
	regulus_rule_body_to_gemini(RegulusRule, SemBody, sem),
	!.
regulus_rule_to_gemini(RegulusRule, _Syn, _Sem, _Counter, _Counter1) :-
	regulus_error('~NUnable to convert rule to Gemini format:~n~n~w~n', [RegulusRule]).	

regulus_rule_body_to_gemini((H --> B), Out, SynOrSem) :-
	!,
	regulus_rule_body_to_gemini(H, H1, SynOrSem),
	regulus_rule_body_to_gemini(B, B1, SynOrSem),
	append(H1, B1, Out).
regulus_rule_body_to_gemini((F, R), Out, SynOrSem) :-
	!,
	regulus_rule_body_to_gemini(F, F1, SynOrSem),
	regulus_rule_body_to_gemini(R, R1, SynOrSem),
	append(F1, R1, Out).
regulus_rule_body_to_gemini(cat(_SemVal, Cat:Feats), [Cat:Feats], syn) :-
	!.
regulus_rule_body_to_gemini(cat(SemVal, Cat:_Feats), [(SemVal, Cat:[])], sem) :-
	!.
regulus_rule_body_to_gemini(Atom, [Atom], syn) :-
	atomic(Atom),
	!.
regulus_rule_body_to_gemini(Atom, [(_SemVal, Atom)], sem) :-
	atomic(Atom),
	!.

gemini_rule_id(Counter, RuleId, Counter1) :-
	join_with_underscore([rule, Counter], RuleId),
	Counter1 is Counter + 1.

%---------------------------------------------------------------

make_gemini_sem_rules_compositional(GeminiSemRulesIn, GeminiSemRulesOut, NewSemFeatDecls, CatsAndNewFeats, TopLevelCats) :-
	make_gemini_sem_rules_compositional1(GeminiSemRulesIn, GeminiSemRulesOut, []-NewSemFeats0, []-CatsAndNewFeats0, TopLevelCats),
	safe_remove_duplicates(NewSemFeats0, NewSemFeats),
	safe_remove_duplicates(CatsAndNewFeats0, CatsAndNewFeats),
	feats_to_sem_feat_decls(NewSemFeats, NewSemFeatDecls),
	!.
make_gemini_sem_rules_compositional(GeminiSemRulesIn, GeminiSemRulesOut, NewSemFeatDecls, CatsAndNewFeats, TopLevelCats) :-
	regulus_error('~NBad call: ~w~n', [make_gemini_sem_rules_compositional(GeminiSemRulesIn, GeminiSemRulesOut, NewSemFeatDecls, CatsAndNewFeats, TopLevelCats)]).

make_gemini_sem_rules_compositional1([], [], NewSemFeatsIn-NewSemFeatsIn, CatsAndNewFeatsIn-CatsAndNewFeatsIn, _TopLevelCats) :-
	!.
make_gemini_sem_rules_compositional1([F | R], [F1 | R1], NewSemFeatsIn-NewSemFeatsOut, CatsAndNewFeatsIn-CatsAndNewFeatsOut, TopLevelCats) :-
	make_gemini_sem_rule_compositional(F, F1, NewSemFeatsIn-NewSemFeatsNext, CatsAndNewFeatsIn-CatsAndNewFeatsNext, TopLevelCats),
	make_gemini_sem_rules_compositional1(R, R1, NewSemFeatsNext-NewSemFeatsOut, CatsAndNewFeatsNext-CatsAndNewFeatsOut, TopLevelCats),
	!.

make_gemini_sem_rule_compositional(Rule, Rule1, NewSemFeatsIn-NewSemFeatsOut, CatsAndNewFeatsIn-CatsAndNewFeatsOut, TopLevelCats) :-
	Rule = sem(RuleId, SemBody),
	Rule1 = sem(RuleId, SemBody1),
	make_gemini_sem_rule_body_compositional(SemBody, SemBody1, NewSemFeatsIn-NewSemFeatsOut, CatsAndNewFeatsIn-CatsAndNewFeatsOut, TopLevelCats),
	!.
make_gemini_sem_rule_compositional(Rule, Rule1, NewSemFeats, CatsAndNewFeats, TopLevelCats) :-
	regulus_error('~NBad call: ~w~n', [make_gemini_sem_rule_compositional(Rule, Rule1, NewSemFeats, CatsAndNewFeats, TopLevelCats)]).

make_gemini_sem_rule_body_compositional([], [], NewSemFeatsIn-NewSemFeatsIn, CatsAndNewFeatsIn-CatsAndNewFeatsIn, _TopLevelCats) :-
	!.
make_gemini_sem_rule_body_compositional([F | R], [F1 | R1], NewSemFeatsIn-NewSemFeatsOut, CatsAndNewFeatsIn-CatsAndNewFeatsOut, TopLevelCats) :-
	make_gemini_sem_rule_cat_compositional(F, F1, NewSemFeatsIn-NewSemFeatsNext, CatsAndNewFeatsIn-CatsAndNewFeatsNext, TopLevelCats),
	make_gemini_sem_rule_body_compositional(R, R1, NewSemFeatsNext-NewSemFeatsOut, CatsAndNewFeatsNext-CatsAndNewFeatsOut, TopLevelCats),
	!.
make_gemini_sem_rule_body_compositional(SemBody, SemBody1, NewSemFeats, CatsAndNewFeats, TopLevelCats) :-
	regulus_error('~NBad call: ~w~n', [make_gemini_sem_rule_body_compositional(SemBody, SemBody1, NewSemFeats, CatsAndNewFeats, TopLevelCats)]).

make_gemini_sem_rule_cat_compositional(Cat, Cat1, NewSemFeatsIn-NewSemFeatsIn, CatsAndNewFeatsIn-CatsAndNewFeatsIn, TopLevelCats) :-
	Cat = (SemVal, CatName:Feats),
	member(CatName, TopLevelCats),
	Cat1 = (SemVal, CatName:Feats),
	!.
make_gemini_sem_rule_cat_compositional(Cat, Cat1, NewSemFeatsIn-NewSemFeatsIn, CatsAndNewFeatsIn-CatsAndNewFeatsIn, _TopLevelCats) :-
	Cat = (SemVal, Atom),
	atomic(Atom),
	Cat1 = (SemVal, Atom),
	!.
make_gemini_sem_rule_cat_compositional(Cat, Cat1, NewSemFeatsIn-NewSemFeatsOut, CatsAndNewFeatsIn-CatsAndNewFeatsOut, _TopLevelCats) :-
	Cat = (SemVal, CatName:Feats),
	Cat1 = (SemVal1, CatName:Feats1),
	make_gemini_sem_rule_cat_compositional(CatName, SemVal, SemVal1, Feats, Feats1, NewSemFeatsIn-NewSemFeatsOut, CatsAndNewFeatsIn-CatsAndNewFeatsOut),
	!.
make_gemini_sem_rule_cat_compositional(Cat, Cat1, NewSemFeats, CatsAndNewFeats, TopLevelCats) :-
	regulus_error('~NBad call: ~w~n', [make_gemini_sem_rule_cat_compositional(Cat, Cat1, NewSemFeats, CatsAndNewFeats, TopLevelCats)]).

make_gemini_sem_rule_cat_compositional(_CatName, SemVal, SemVal, Feats, Feats, NewSemFeatsIn-NewSemFeatsIn, CatsAndNewFeatsIn-CatsAndNewFeatsIn) :-
	var(SemVal),
	!.
make_gemini_sem_rule_cat_compositional(CatName, SemVal, SemVal1, Feats, Feats1, NewSemFeatsIn-NewSemFeatsOut, CatsAndNewFeatsIn-CatsAndNewFeatsOut) :-
	is_feat_val_list(SemVal),
	SemVal1 = _NewVar,

	append(Feats, SemVal, Feats1),

	feats_in_feat_val_list(SemVal, SemFeatNames),
	append(NewSemFeatsIn, SemFeatNames, NewSemFeatsOut),

	CatsAndNewFeatsOut = [category(CatName, SemFeatNames) | CatsAndNewFeatsIn],
	!.
make_gemini_sem_rule_cat_compositional(_CatName, SemVal, SemVal, Feats, Feats, NewSemFeatsIn-NewSemFeatsIn, CatsAndNewFeatsIn-CatsAndNewFeatsIn) :-
	!.
make_gemini_sem_rule_cat_compositional(CatName, SemVal, SemVal1, Feats, Feats1, NewSemFeats, CatsAndNewFeats) :-
	regulus_error('~NBad call: ~w~n', [make_gemini_sem_rule_cat_compositional(CatName, SemVal, SemVal1, Feats, Feats1, NewSemFeats, CatsAndNewFeats)]).

%---------------------------------------------------------------

feats_to_sem_feat_decls([], []).
feats_to_sem_feat_decls([F | R], [F1 | R1]) :-
	feat_to_sem_feat_decl(F, F1),
	feats_to_sem_feat_decls(R, R1).

feat_to_sem_feat_decl(FeatName, sem_feature(FeatName, lf_fragment)).

%---------------------------------------------------------------

adjust_category_decls_to_add_new_feats([], [], _CatsAndNewFeats) :-
	!.
adjust_category_decls_to_add_new_feats([F | R], [F1 | R1], CatsAndNewFeats) :-
	adjust_category_decl_to_add_new_feats(F, F1, CatsAndNewFeats),
	adjust_category_decls_to_add_new_feats(R, R1, CatsAndNewFeats),
	!.
adjust_category_decls_to_add_new_feats(Decls, Decls1, CatsAndNewFeats) :-
	regulus_error('~NBad call: ~w~n', [adjust_category_decls_to_add_new_feats(Decls, Decls1, CatsAndNewFeats)]).

adjust_category_decl_to_add_new_feats(category(CatName, Feats), category(CatName, Feats1), CatsAndNewFeats) :-
	(   member(category(CatName, NewFeats), CatsAndNewFeats) ->
	    append(Feats, NewFeats, Feats1) ;
	    Feats1 = Feats
	),
	!.
adjust_category_decl_to_add_new_feats(Other, Other, _CatsAndNewFeats).

%---------------------------------------------------------------

regulus_lex_entry_to_gemini(RegulusRule, GeminiLexEntry) :-
	RegulusRule = (H --> B),
	H = cat(SemVal, Cat:Feats),
	(   SemVal = '*null_value*' ->
	    GeminiLexEntry = le(WordList, Cat, features:Feats) ;
	    GeminiLexEntry = le(WordList, Cat, features:Feats, logical_form:SemVal)
	),
	regulus_rule_body_to_word_list(B, WordList),
	!.
regulus_lex_entry_to_gemini(RegulusRule, _GeminiLexEntry) :-
	regulus_error('~NUnable to convert lex entry to Gemini format:~n~n~w~n', [RegulusRule]).	

regulus_rule_body_to_word_list(Atom, Atom) :-
	atomic(Atom),
	!.
regulus_rule_body_to_word_list(Body, List) :-
	regulus_rule_body_to_word_list1(Body, List).

regulus_rule_body_to_word_list1(Atom, [Atom]) :-
	atomic(Atom),
	!.
regulus_rule_body_to_word_list1((F, R), [F | R1]) :-
	regulus_rule_body_to_word_list1(R, R1).

%---------------------------------------------------------------

write_rules_and_decls_to_gemini_files(Declarations, SynRules, SemRules, LexEntries, OutFile) :-
	write_gemini_item_list_to_file(Declarations, OutFile, features),
	write_gemini_item_list_to_file(SynRules, OutFile, syn),
	write_gemini_item_list_to_file(SemRules, OutFile, sem),
	write_gemini_item_list_to_file(LexEntries, OutFile, lex).

write_gemini_item_list_to_file(Items, FileName, Extension) :-
	gemini_file(FileName, Extension, File),
	absolute_file_name(File, AbsoluteFile),
	open(AbsoluteFile, write, S),
	write_gemini_item_list(S, Items),
	close(S),
	length(Items, Length),
	format('~NWritten ~d items to ~w~n', [Length, AbsoluteFile]).		       

gemini_file(BaseFile, Extension, FullFile) :-
	atomic(BaseFile),
	!,
	append_atoms([BaseFile, Extension], 0'., FullFile).
gemini_file(BaseFile, Extension, FullFile) :-
	BaseFile =.. [FileSearchPath, BaseFile1],
	!,
	gemini_file(BaseFile1, Extension, FullFile1),
	FullFile =.. [FileSearchPath, FullFile1].
gemini_file(BaseFile, _Extension, _FullFile) :-
	regulus_error('~NUnable to interpret ~w as name of Gemini output file~n', BaseFile).

%---------------------------------------------------------------

gemini_top_rules(TopLevelCats, PrefDecl, TopSynRules, TopSemRules) :-
	gemini_top_rules1(TopLevelCats, TopSynRules, TopSemRules, TopRuleIds),
	PrefDecl = sigma_pref([TopRuleIds]).

gemini_top_rules1([], [], [], []).
gemini_top_rules1([FCat | RCats], [FSyn | RSyn], [FSem | RSem], [FId | RIds]) :-
	join_with_underscore([sigma, FCat], FId),
	gemini_top_syn_rule_for_cat(FCat, FSyn, FId),
	gemini_top_sem_rule_for_cat(FCat, FSem, FId),	
	gemini_top_rules1(RCats, RSyn, RSem, RIds).

gemini_top_syn_rule_for_cat(Cat, Rule, Id) :-
	Rule = top_down_syn(Id, top_down, [sigma, Cat:[]]).

sem(sigma_top, 
    [(LF,sigma),
     (LF,top:[])
    ]
   ).

gemini_top_sem_rule_for_cat(Cat, Rule, Id) :-
	Rule = sem(Id, [(LF, sigma), (LF, Cat:[])]).

%---------------------------------------------------------------

write_gemini_item_list(_S, []).
write_gemini_item_list(S, [F | R]) :-
	write_gemini_item(S, F),
	write_gemini_item_list(S, R).

write_gemini_item(S, feature_value_space(ValueSpaceName, ValueSpace)) :-
	!,
	format(S, '~N~q def_value_set ~q ; enable_boolean_ops.~n~n', [ValueSpaceName, ValueSpace]).
write_gemini_item(S, sem_feature_value_space(ValueSpaceName, ValueSpace)) :-
	!,
	format(S, '~N~q def_value_set ~q.~n~n', [ValueSpaceName, ValueSpace]).
write_gemini_item(S, feature(FeatName, ValueSpaceName)) :-
	!,	
	format(S, '~Nsyn def_feature ~q with_value_set ~q.~n~n', [FeatName, ValueSpaceName]).
write_gemini_item(S, sem_feature(FeatName, ValueSpaceName)) :-
	!,	
	format(S, '~Nsem def_feature ~q with_value_set ~q.~n~n', [FeatName, ValueSpaceName]).
write_gemini_item(S, category(CatName, Feats)) :-
	!,	
	format(S, '~Ndef_category ~q with_features ~q enable_lexicon.~n~n', [CatName, Feats]).
write_gemini_item(S, logical_form_valueset(Feat)) :-
	!,	
	format(S, '~N~q.~n~n', [logical_form_valueset(Feat)]).
write_gemini_item(S, SigmaPref) :-
	functor(SigmaPref, sigma_pref, 1),
	!,	
	format(S, '~N~q.~n~n', [SigmaPref]).
write_gemini_item(S, Atom) :-
	atomic(Atom),
	!,
	format(S, '~N~q.~n~n', [Atom]).	
write_gemini_item(S, SynRule) :-
	(   functor(SynRule, syn, 3) ;
	    functor(SynRule, top_down_syn, 3)
	),
	!,
	make_ground(SynRule),
	format(S, '~N~q.~n~n', [SynRule]).	
write_gemini_item(S, SemRule) :-
	functor(SemRule, sem, 2),
	!,
	make_ground(SemRule),
	format(S, '~N~q.~n~n', [SemRule]).	
write_gemini_item(S, LexEntry) :-
	(   functor(LexEntry, le, 3) ;
	    functor(LexEntry, le, 4)
	),
	!,
	make_ground(LexEntry),
	format(S, '~N~q.~n~n', [LexEntry]).	
write_gemini_item(S, Other) :-
	format(S, '~N*** UNKNOWN ITEM TYPE ***~q~n~n', [Other]).	

%---------------------------------------------------------------

feats_in_feat_val_list([], []).
feats_in_feat_val_list([(Feat=_Val) | R], [Feat | R1]) :-
	feats_in_feat_val_list(R, R1).
