
:- ensure_loaded(library(regulus)).

%---------------------------------------------------------------

regulus_to_gemini(InFile, OutFile) :-
	timed_call(regulus_to_gemini1(InFile, OutFile), TimeTaken),
	format('~NTranslation finished, ~2f secs~n', [TimeTaken]).

%---------------------------------------------------------------

regulus_to_gemini1(InFile, OutFile) :-
	on_exception(
	Exception, 
	regulus_to_gemini2(InFile, OutFile),
	inform_about_top_level_regulus_error(Exception)
    ).

regulus_to_gemini2(InFile, OutFile) :-
	regulus_compile_init,
	read_regulus_file(InFile, ReadRules, ReadDeclarations),
	internalise_regulus_declarations(ReadDeclarations),
	pre_process_regulus_rules_for_gemini(ReadRules, PreProcessedRules),

	regulus_decls_to_gemini(ReadDeclarations, GeminiDeclarations, TopLevelCats),
	gemini_value_space_decls(GeminiValueSpaceDecls),
	% Value space decls need to go first...
	append(GeminiValueSpaceDecls, GeminiDeclarations, AllGeminiDeclarations),

	regulus_rules_to_gemini(PreProcessedRules, GeminiRules, GeminiLexEntries),
	gemini_top_rules(TopLevelCats, PrefDecl, TopRules),

	append([PrefDecl | TopRules], GeminiRules, AllGeminiRules),
	write_rules_and_decls_to_gemini_files(AllGeminiDeclarations, AllGeminiRules, GeminiLexEntries, OutFile),
	!.
regulus_to_gemini2(_InFile, _OutFile) :-
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
	    Result = '*null*'
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
% Modified version of internalise_cat/4 from regulus.pl

% Note that we remove the ignored features before checking validity of
% the feat-val list, since otherwise we'll get an error - if there are
% any ignored features, they'll be missing from the internal category
% declaration too.

internalise_cat_for_gemini(CatName, FeatsWithVals, CatName:GeminiFeatsWithVals) :-
	atom(CatName),
	get_category_internal(CatName, AllFeats),
	give_up_if_undeclared_feats_in_feat_val_list(CatName, AllFeats, FeatsWithVals),
	remove_ignored_feat_val_pairs_from_list(FeatsWithVals, FeatsWithVals1),
	replace_vals_with_substitutes_in_feat_val_list(FeatsWithVals1, FeatsWithVals2),
	is_valid_feat_val_list(FeatsWithVals2, AllFeats),
	remove_sem_and_gsem_feats_from_feat_val_list(FeatsWithVals2, GeminiFeatsWithVals),
	!.
internalise_cat_for_gemini(CatName, FeatsWithVals, _) :-
	regulus_error('~NCouldn''t translate category: ~q~n', [CatName:FeatsWithVals]).

%---------------------------------------------------------------

remove_sem_and_gsem_feats_from_feat_val_list([], []).
remove_sem_and_gsem_feats_from_feat_val_list([(Feat = Val) | R], Out) :-
	(   member(Feat, [sem, gsem]) ->
	    Out = R1 ;
	    Out = [(Feat = Val) | R1]
	),
	!,
	remove_sem_and_gsem_feats_from_feat_val_list(R, R1).

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
remove_nulls_for_gemini(Cat:FeatsWithVals, Cat:FeatsWithVals) :-
	!.
remove_nulls_for_gemini(Other, _) :-
	regulus_error('NBad subterm in rule: ~q.~n', [Other]).

%---------------------------------------------------------------

regulus_decls_to_gemini([], [], []).
regulus_decls_to_gemini([F0 | R], Out, TopLevelCats) :-
	F0 = declaration(F, _LineInfo),
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
regulus_decl_to_gemini(specialises(_Val1, _Val2, _SpaceId), discard) :-
	!.
regulus_decl_to_gemini(feature(Feat, Space), feature(Feat, Space)) :-
	!.
regulus_decl_to_gemini(ignore_feature(_Feat), discard) :-
	!.
regulus_decl_to_gemini(category(CatName, Feats), category(CatName, Feats1)) :-
	!,
	remove_sem_and_gsem_from_list(Feats, Feats1).
regulus_decl_to_gemini(top_level_category(Cat), top_level_category(Cat)) :-
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

% The feature_value_space declarations may have been combined from several files.
gemini_value_space_decls(GeminiValueSpaceDecls) :-
	findall(feature_value_space(Name, Space), feature_value_space(Name, Space), GeminiValueSpaceDecls).

%---------------------------------------------------------------

regulus_rules_to_gemini(RegulusRules, GeminiRules, GeminiLexEntries) :-
	regulus_rules_to_gemini1(RegulusRules, []-GeminiRules0, []-GeminiLexEntries0, 0),
	reverse(GeminiRules0, GeminiRules),
	reverse(GeminiLexEntries0, GeminiLexEntries).

regulus_rules_to_gemini1([], Rules-Rules, Lex-Lex, _Counter) :-
	!.
regulus_rules_to_gemini1([F | R], RulesIn-RulesOut, LexIn-LexOut, Counter) :-
	is_lexical_regulus_rule(F),
	!,
	regulus_lex_entry_to_gemini(F, F1),
	LexNext = [F1 | LexIn],
	regulus_rules_to_gemini1(R, RulesIn-RulesOut, LexNext-LexOut, Counter).
regulus_rules_to_gemini1([F | R], RulesIn-RulesOut, LexIn-LexOut, Counter) :-
	!,
	regulus_rule_to_gemini(F, F1, Counter, Counter1),	
	RulesNext = [F1 | RulesIn],
	regulus_rules_to_gemini1(R, RulesNext-RulesOut, LexIn-LexOut, Counter1).

%---------------------------------------------------------------

regulus_rule_to_gemini(RegulusRule, GeminiRule, Counter, Counter1) :-
	RegulusRule = (H --> B),
	GeminiRule = syn(RuleId, rule_group, [H | B1]),
	gemini_rule_id(Counter, RuleId, Counter1),
	regulus_cats_to_gemini_cats(B, B1),
	!.
regulus_rule_to_gemini(RegulusRule, _GeminiRule, _Counter, _Counter1) :-
	regulus_error('~NUnable to convert rule to Gemini format:~n~n~w~n', [RegulusRule]).	

regulus_cats_to_gemini_cats((F, R), Out) :-
	!,
	regulus_cats_to_gemini_cats(F, F1),
	regulus_cats_to_gemini_cats(R, R1),
	append(F1, R1, Out).
regulus_cats_to_gemini_cats(F, [F]).

gemini_rule_id(Counter, RuleId, Counter1) :-
	join_with_underscore([rule, Counter], RuleId),
	Counter1 is Counter + 1.

%---------------------------------------------------------------

regulus_lex_entry_to_gemini(RegulusRule, GeminiLexEntry) :-
	RegulusRule = (H --> B),
	H = Cat:Feats,
	GeminiLexEntry = le(WordList, Cat, features:Feats),
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

write_rules_and_decls_to_gemini_files(GeminiDeclarations, AllGeminiRules, GeminiLexEntries, OutFile) :-
	OutFile = user,
	!,
	format('~N~nGeminiDeclarations: ~n~n', []),
	write_gemini_item_list(user, GeminiDeclarations),
	format('~N~nGeminiRules: ~n~n', []),
	write_gemini_item_list(user, AllGeminiRules),
	format('~N~nGeminiLexEntries: ~n~n', []),
	write_gemini_item_list(user, GeminiLexEntries).
write_rules_and_decls_to_gemini_files(GeminiDeclarations, AllGeminiRules, GeminiLexEntries, OutFile) :-
	gemini_file(OutFile, features, DeclarationsFile),
	gemini_file(OutFile, syn, SynFile),
	gemini_file(OutFile, lex, LexFile),
	
	open(DeclarationsFile, write, S1),
	write_gemini_item_list(S1, GeminiDeclarations),
	close(S1),

	open(SynFile, write, S2),
	write_gemini_item_list(S2, AllGeminiRules),
	close(S2),

	open(LexFile, write, S3),
	write_gemini_item_list(S3, GeminiLexEntries),
	close(S3).	

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

gemini_top_rules(TopLevelCats, PrefDecl, TopRules) :-
	gemini_top_rules1(TopLevelCats, TopRules, TopRuleIds),
	PrefDecl = sigma_pref([TopRuleIds]).

gemini_top_rules1([], [], []).
gemini_top_rules1([FCat | RCats], [FRule | RRules], [FId | RIds]) :-
	gemini_top_rule_for_cat(FCat, FRule, FId),
	gemini_top_rules1(RCats, RRules, RIds).

gemini_top_rule_for_cat(Cat, Rule, Id) :-
	join_with_underscore([sigma, Cat], Id),
	Rule = top_down_syn(Id, top_down, [sigma, Cat:[]]).

%---------------------------------------------------------------

write_gemini_item_list(_S, []).
write_gemini_item_list(S, [F | R]) :-
	write_gemini_item(S, F),
	write_gemini_item_list(S, R).

write_gemini_item(S, feature_value_space(ValueSpaceName, ValueSpace)) :-
	!,
	format(S, '~N~q def_value_set ~q ; enable_boolean_ops.~n~n', [ValueSpaceName, ValueSpace]).
write_gemini_item(S, feature(FeatName, ValueSpaceName)) :-
	!,	
	format(S, '~Nsyn def_feature ~q with_value_set ~q.~n~n', [FeatName, ValueSpaceName]).
write_gemini_item(S, category(CatName, Feats)) :-
	!,	
	format(S, '~Ndef_category ~q with_features ~q enable_lexicon.~n~n', [CatName, Feats]).
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
write_gemini_item(S, LexEntry) :-
	functor(LexEntry, le, 3),
	!,
	make_ground(LexEntry),
	format(S, '~N~q.~n~n', [LexEntry]).	
write_gemini_item(S, Other) :-
	format(S, '~N*** UNKNOWN ITEM TYPE ***~q~n~n', [Other]).	
