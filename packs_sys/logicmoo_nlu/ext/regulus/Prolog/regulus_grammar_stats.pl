
/*

Needs to be loaded after $REGULUS/Prolog/load.pl

   print_regulus_grammar_stats(+ConfigFile, +GrammarFiles)

Prints rough measures of grammar complexity. Typical call:

print_regulus_grammar_stats('$MED_SLT2/Jap/scripts/japanese_recognition.cfg',
			    [japanese_generation_grammars(japanese_recognition_rules),
			     japanese_generation_grammars(japanese_recognition_lex)])

*/

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- use_module('$REGULUS/Prolog/regulus_utilities').

%---------------------------------------------------------------

print_regulus_grammar_stats(ConfigFile, GrammarFiles) :-
	load_regulus_config_file(ConfigFile),
	read_regulus_files(GrammarFiles, Rules, _Declarations),
	find_categories_in_rules(Rules, Cats),
	print_cat_stats(Cats).

%---------------------------------------------------------------

print_cat_stats(Cats) :-
	length(Cats, NCats),
	format('~N -- Number of categories used in grammar: ~d~n', [NCats]),
	
	count_cat_feature_sum(Cats, 0-CatFeatureSum),
	format('~N -- Sum of number of syntactic features in each category: ~d~n', [CatFeatureSum]),

	count_cat_expansions_sum(Cats, 0-CatExpansionSum, [null,0]-[MostImportantCat,MostImportantExpansion]),
	format('~N -- Sum of number of possible expansions of each category: ~4E (Most important cat: ~w, ~4E expansions)~n',
	       [CatExpansionSum, MostImportantCat, MostImportantExpansion]),
	!.

%---------------------------------------------------------------

count_cat_feature_sum([], In-In) :-
	!.
count_cat_feature_sum([F | R], In-Out) :-
	count_cat_features(F, Count),
	Next is In + Count,
	!,
	count_cat_feature_sum(R, Next-Out).
count_cat_feature_sum(Cats, Pair) :-
	format2error('~N*** Error: bad call: ~w~n', [count_cat_feature_sum(Cats, Pair)]),
	fail.

count_cat_features(CatName, Count) :-
	category_internal(CatName, Feats),
	findall(Feat,
		( member(Feat, Feats), \+ member(Feat, [sem, gsem]) ),
		SynFeats),
	length(SynFeats, Count),
	!.
count_cat_features(CatName, Count) :-
	format2error('~N*** Error: bad call: ~w~n', [count_cat_features(CatName, Count)]),
	fail.

%---------------------------------------------------------------	

count_cat_expansions_sum([], In-In, Biggest-Biggest) :-
	!.
count_cat_expansions_sum([F | R], In-Out, [CatIn, CatExpIn]-BiggestOut) :-
	count_cat_expansions(F, NExpansions),
	(   NExpansions >= CatExpIn ->
	    BiggestNext = [F, NExpansions] ;
	    BiggestNext = [CatIn, CatExpIn]
	),
	Next is In + NExpansions,
	!,
	count_cat_expansions_sum(R, Next-Out, BiggestNext-BiggestOut).
count_cat_expansions_sum(Cats, Pair1, Pair2) :-
	format2error('~N*** Error: bad call: ~w~n', [count_cat_expansions_sum(Cats, Pair1, Pair2)]),
	fail.

count_cat_expansions(CatName, NExpansions) :-
	category_internal(CatName, Feats),
	count_feat_expansions(Feats, 1-NExpansions),
	!.
count_cat_expansions(F, NExpansions) :-
	format2error('~N*** Error: bad call: ~w~n', [count_cat_expansions(F, NExpansions)]),
	fail.

count_feat_expansions([], In-In) :-
	!.
count_feat_expansions([F | R], In-Out) :-
	count_feat_value_space_for_feat(F, Count),
	Next is In * Count,
	!,
	count_feat_expansions(R, Next-Out).

count_feat_value_space_for_feat(sem, 1) :-
	!.
count_feat_value_space_for_feat(gsem, 1) :-
	!.
count_feat_value_space_for_feat(Feat, Count) :-
	feature(Feat, ValueSpaceId),
	feature_value_space(ValueSpaceId, ValueSpace),
	count_value_space(ValueSpace, 1-Count),
	!.
count_feat_value_space_for_feat(F, Count) :-
	format2error('~N*** Error: bad call: ~w~n', [count_feat_value_space_for_feat(F, Count)]),
	fail.

count_value_space([], In-In) :-
	!.
count_value_space([F | R], In-Out) :-
	length(F, Count),
	Next is In * Count,
	!,
	count_value_space(R, Next-Out).
count_value_space(Space, Pair) :-
	format2error('~N*** Error: bad call: ~w~n', [count_value_space(Space, Pair)]),
	fail.

%---------------------------------------------------------------	

find_categories_in_rules(Rules, Cats) :-
	findall(Cat, cat_name_in_rules(Cat, Rules), Cats0),
	sort(Cats0, Cats).

cat_name_in_rules(CatName, Rules) :-
	member(rule(Rule, _LineInfo), Rules),
	cat_name_in_rule(Rule, CatName).

cat_name_in_rule((Head --> Body), CatName) :-
	(   cat_name_in_cat(Head, CatName) ;
	    cat_name_in_body(Body, CatName)
	).

cat_name_in_cat(CatName:_Feats, CatName).

cat_name_in_body((P, Q), CatName) :-
	(   cat_name_in_body(P, CatName) ;
	    cat_name_in_body(Q, CatName)
	).
cat_name_in_body((P ; Q), CatName) :-
	(   cat_name_in_body(P, CatName) ;
	    cat_name_in_body(Q, CatName)
	).
cat_name_in_body(?P, CatName) :-
	cat_name_in_body(P, CatName).
cat_name_in_body(CatName:_Feats, CatName).





