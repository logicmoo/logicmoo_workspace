/* this compiler is intended to work in a gemini.qof compiled
   image.  

   If we use this enough, it could be promoted to being a full-fledged
   gemini compiler.
*/

gemini2regulus(OutputFile):-
	determine_regulus_feature_value_spaces,
	determine_regulus_features,
	determine_regulus_categories,
	determine_regulus_top_level_category,
	determine_regulus_rules,
	write_regulus_file(OutputFile).


determine_regulus_feature_value_spaces:-
	clear_dynamic_predicates([regulus_feature_value_space/2]),
	feature_type_def(ValueSpaceID, Values, EnableBooleanOps),
	\+ logical_form_valueset(ValueSpaceID), % exclude LF valued features
	\+ ValueSpaceID = lexform_types, % exclude lexform_types
	(EnableBooleanOps = no ->
	     ValueSpaces = [Values]
	;    ValueSpaces = Values),
	assert(regulus_feature_value_space(ValueSpaceID, ValueSpaces)),
	fail.
determine_regulus_feature_value_spaces.


determine_regulus_features :-
	clear_dynamic_predicates([regulus_feature/2]),
        % only even consider syntactic features
	feature_def(_Category, FeatureName, syn, _, ValueSpaceID, _, _,_),
	% should be a known ValueSpaceID,
	regulus_feature_value_space(ValueSpaceID, _),
	assert_once(regulus_feature(FeatureName, ValueSpaceID)),
	fail.
determine_regulus_features.
	
determine_regulus_categories :-
	clear_dynamic_predicates([regulus_category/2]),
	category(CategoryName, _Lexical),
	findall(Feature, 
		(feature_def(CategoryName, Feature, syn, _, _, _, _, _),
		 regulus_feature(Feature, _)),
		FeatsList),
	assert(regulus_category(CategoryName, FeatsList)),
	fail.
determine_regulus_categories.

determine_regulus_top_level_category:-
	clear_dynamic_predicates([regulus_top_level_category/1]),
	assert(regulus_top_level_category(sigma)).


determine_regulus_rules:-
	clear_dynamic_predicates([regulus_rule/2]),
	determine_regulus_syntax_rules,
	determine_regulus_lexicon.



determine_regulus_lexicon:-
	cat(Word, GeminiCategory),
	gemini_category2regulus_category(GeminiCategory, RegulusCategory),
	assert(regulus_rule(RegulusCategory, Word)),
	fail.
% multi-words are stored in multi_word_cat in reverse order,
% so they are easier to match against the chart.
determine_regulus_lexicon:-
	multi_word_cat(LastWord, RestWords, GeminiCategory),
	gemini_category2regulus_category(GeminiCategory, RegulusCategory),
	words_to_comma_tuples(RestWords, LastWord, RHS),
	assert(regulus_rule(RegulusCategory, RHS)),
	fail.
determine_regulus_lexicon.


% top level rules using application_sigma_rule/5 are 
% required to to non-branching.  This code should be generalized
% to allow other top_down rules (none in PSA, but...)
determine_regulus_syntax_rules:-
	top_down_rule(GeminiCategory, [GeminiRHS], RuleName),
	application_sigma_rule(_Handle, _Feats, RuleName, yes, _) ,
	decode_cat((GeminiCategory,GeminiRHS),(RegulusCategory, RegulusRHS)),
	assert(regulus_rule(RegulusCategory, RegulusRHS)),
	fail.
determine_regulus_syntax_rules:-
	rule(GeminiCategory, GeminiRHS, _RuleName),
	my_decode_cat([GeminiCategory|GeminiRHS], [RegulusCategory|RegulusRHS]),
	list_to_comma_tuples(RegulusRHS, RHS),
	assert(regulus_rule(RegulusCategory, RHS)),
	fail.

determine_regulus_syntax_rules.

gemini_rhs2regulus_rhs([GeminiCategory], RegulusCategory):-
	!,
        gemini_category2regulus_category(GeminiCategory, RegulusCategory).
gemini_rhs2regulus_rhs([GeminiCategory|RestGeminiRHS], 
		       (RegulusCategory,RestRegulusRHS)):-
        gemini_category2regulus_category(GeminiCategory, RegulusCategory),
	gemini_rhs2regulus_rhs(RestGeminiRHS, RestRegulusRHS).
	

% words_to_comma_tuples does a reverse on the way down
% multi-words are stored in reverse order
words_to_comma_tuples([], Partial, Partial):-
	!.
words_to_comma_tuples([Term], Partial, (Term,Partial)):-
	!.
words_to_comma_tuples([Term|RestWords], Partial, Result):-
	words_to_comma_tuples(RestWords, (Term,Partial), Result).


% like words_to_comma_tuples, but does not do the reverse
list_to_comma_tuples([Term], Term):-
	!.
list_to_comma_tuples([Term|Rest], (Term,Result)):-
	list_to_comma_tuples(Rest, Result).


gemini_category2regulus_category(Category, Category):-
	atomic(Category),
	!.
gemini_category2regulus_category(GeminiCategory, 
				 CategoryName:PrunedFeatureValues):-
	% decode_cat/2 translates from Gemini internal form
	% to MajorCat:FeatureValues form.
	decode_cat(GeminiCategory, CategoryName:FeatureValues),
	prune_nonregulus_features(FeatureValues, PrunedFeatureValues).


prune_nonregulus_features([], []).
prune_nonregulus_features([(FeatName=Val)|RestFeats], [(FeatName=Val)|ResultFeats]):-
	regulus_feature(FeatName, _),
	!,
	prune_nonregulus_features(RestFeats, ResultFeats).
prune_nonregulus_features([(_FeatName=_Val)|RestFeats], ResultFeats):-
	prune_nonregulus_features(RestFeats, ResultFeats).

/*
 my_decode_cat/2 is a varient of decode_cat/2 that preserves
 variables across pretty printing of (for instance) boolean valued
 features.  That is, the source grammar may have something like

 syn(utterance_s, basic,
        [utterance:[stype=SType],
         s:[stype=SType, stype=(dcl\/imp\/ynq\/whq), operator_wrapped=y,
	    gapsin=null, gapsout=null]]).

Where the stype is explicitly shared between the utterance and the s.
Internal processing forces those to have shared variables, but
decode_cat/2 will lose those sharings.  

*/
my_decode_cat(Source, Result):-
	note_shared_terms(Source, Shares),
	substitute_shares_with_vars(Source, Shares, NextSource),
	convert_shares(Shares, NextSource, Target, TargetShares),
	add_shares_back_in(Target, TargetShares, Result).


% this predicate is a mess.  It seems to work, but I need
% to work through this from first principles again.
convert_shares(Shares, Source, Target, ResultShares):-
	copy_term((Shares,Source),(CopyShares,CopySource)),
	unify_shares_in(CopyShares),
	decode_cat((Shares,Source), (TempShares,NextSource)),
	decode_cat((CopyShares,CopySource), (NextShares,NextSource)),
	pair_shares_back_up(TempShares, Shares, Shares1),
	decode_cat((Shares1,Source), (ResultShares,Target)).

pair_shares_back_up([], [], []).
pair_shares_back_up([pair(_, R)|Rest],[pair(_, Var)|RestSource],[pair(R,Var)|RestResults]):-
	pair_shares_back_up(Rest,RestSource, RestResults).

unify(X,X).

unify_shares_in([]).
unify_shares_in([pair(Val, Val)|RestShares]):-
	unify_shares_in(RestShares).

add_shares_back_in([], [], []).
add_shares_back_in([Cat|RestCats], Shares, [ResultCat|RestResultCats]):-
	add_shares_back_in_cat(Cat, Shares, ResultCat, RestShares),
	add_shares_back_in(RestCats, RestShares, RestResultCats).

add_shares_back_in_cat(Cat, Shares, Cat, Shares):-
	atomic(Cat),
	!.
add_shares_back_in_cat(Cat, Shares, Cat, Shares):-
	var(Cat), % shouldn't happen
	!.
add_shares_back_in_cat((MajorCat:Feats), Shares, (MajorCat:NewFeats), RestShares):-
	add_shares_back_in_feats(Feats, Shares, NewFeats, RestShares).

add_shares_back_in_feats([], Shares, [], Shares).
add_shares_back_in_feats([(FeatName=FeatValue)|RestFeats], Shares, [(FeatName=RealValue),(FeatName=FeatValue)|RestResultFeats], ResultShares):-
	matches_with_term(Shares, FeatValue, RealValue, RestShares),
	!,
	add_shares_back_in_feats(RestFeats, RestShares,RestResultFeats, ResultShares).
add_shares_back_in_feats([(FeatName=FeatValue)|RestFeats], Shares, [(FeatName=NewFeatValue)|RestResultFeats], ResultShares):-
	add_shares_back_in_value(FeatValue, Shares, NewFeatValue, RestShares),
	!,
	add_shares_back_in_feats(RestFeats, RestShares,RestResultFeats, ResultShares).

add_shares_back_in_value(FeatValue, Shares, FeatValue, Shares):-
	atomic(FeatValue),
	!.
add_shares_back_in_value(FeatValue, Shares, FeatValue, Shares):-
	var(FeatValue),
	!.
add_shares_back_in_value((MajorCat:Feats), Shares, (MajorCat:NewFeats), RestShares):-      
	!,
	add_shares_back_in_feats(Feats, Shares, NewFeats, RestShares).
add_shares_back_in_value(FeatValue, Shares, FeatValue, Shares).
	
	

substitute_shares_with_vars(Source, _Shares, Source):-
	atomic(Source),
	!.
substitute_shares_with_vars(Source, _Shares, Source):-
	var(Source),
	!.
substitute_shares_with_vars(Source, Shares, Var):-
	matches_with_var(Shares, Source, Var),
	!.
substitute_shares_with_vars(Source, Shares, Result):-
	functor(Source, Functor, Arity),
	functor(Result, Functor, Arity),
	substitute_shares_with_var_helper(Arity, Source, Shares, Result).


substitute_shares_with_var_helper(0, _Source, _Shares, _Result).
substitute_shares_with_var_helper(Arity, Source, Shares, Result):-
	arg(Arity, Source, SourceArg),
	arg(Arity, Result, ResultArg),
	NextArity is Arity - 1,
	substitute_shares_with_vars(SourceArg, Shares, ResultArg),
	substitute_shares_with_var_helper(NextArity, Source, Shares, Result).

note_shared_terms(Source, Shares):-
	find_all_terms(Source, [], Terms),
	remove_non_shared(Terms, Shares).

find_all_terms(Var, Terms, Terms):-
	var(Var),
	!. % not interested in isolated vars, only non-ground compound terms
find_all_terms(Atomic, Terms, Terms):-
	atomic(Atomic),
	!.
find_all_terms(CompoundTerm, Terms, Terms):-
	ground(CompoundTerm),
	!.
find_all_terms(CompoundTerm, Terms, Terms):-
	already_seen_term(Terms, CompoundTerm),
	!.
find_all_terms(CompoundTerm, Terms, RestTerms):-
	functor(CompoundTerm, _, Arity),
	find_all_terms_helper(Arity, CompoundTerm, [term(CompoundTerm,_)|Terms], RestTerms).

find_all_terms_helper(0, _CompoundTerm, Terms, Terms).
find_all_terms_helper(Arity, CompoundTerm, Terms, RestTerms):-
	Arity > 0,
	arg(Arity, CompoundTerm, Arg),
	find_all_terms(Arg, Terms, TempTerms),
	NextArity is Arity - 1,
	find_all_terms_helper(NextArity, CompoundTerm, TempTerms, RestTerms).

matches_with_term([pair(Term, Var)|RestShares], TargetVar, Term, RestShares):-
	Var == TargetVar,
	!.
matches_with_var([This|Rest], TargetVar, Term, [This|RestShares]):-
	matches_with_var(Rest, TargetVar, Term, RestShares).


matches_with_var([pair(Term, Var)|_], TargetTerm, Var):-
	Term == TargetTerm,
	!.
matches_with_var([_|Rest], TargetTerm, Var):-
	matches_with_var(Rest, TargetTerm, Var).


already_seen_term([term(Term, true)|_], TargetTerm):-
	Term == TargetTerm,
	!.
already_seen_term([_|Rest], TargetTerm):-
	already_seen_term(Rest, TargetTerm).

remove_non_shared([], []).
remove_non_shared([term(Term, Var)|RestTerms], [pair(Term, _NewVar)|Result]):-
	Var == true,
	!,
	remove_non_shared(RestTerms, Result).
remove_non_shared([_|RestTerms], Result):-
	remove_non_shared(RestTerms, Result).

write_regulus_file(OutputFile):-
	open(OutputFile, write, Stream),
	write_regulus_stream(Stream),
	close(Stream).
	
write_regulus_stream(Stream):-
	regulus_feature_value_space(ValueSpaceID, Values),
	format(Stream, '~q.~n', [feature_value_space(ValueSpaceID, Values)]),
	fail.
write_regulus_stream(Stream):-
	nl(Stream),
	regulus_feature(FeatureName, ValueSpaceID),
	format(Stream, '~q.~n', [feature(FeatureName, ValueSpaceID)]),
	fail.
write_regulus_stream(Stream):-
	nl(Stream),
	regulus_category(Category, FeatureList),
	format(Stream, '~q.~n', [category(Category, FeatureList)]),
	fail.
write_regulus_stream(Stream):-
	nl(Stream),
	regulus_top_level_category(Category),
	format(Stream, '~q.~n', [top_level_category(Category)]),
	fail.
write_regulus_stream(Stream):-
	nl(Stream),
	regulus_rule(Category, RHS),
	numbervars((Category --> RHS), 0, _),
	format(Stream, '~q.~n', [(Category --> RHS)]),
	fail.
write_regulus_stream(_Stream).



