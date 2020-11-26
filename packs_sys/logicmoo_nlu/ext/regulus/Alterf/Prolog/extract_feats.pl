
%---------------------------------------------------------------

:- module(extract_feats,
	  [rec_results_to_feature_vectors/3,
	   feat_for_surface_words/3,
	   feat_for_nl_value/3,
	   words_covered_by_surface_pattern/3,
	   hand_coded_surface_pattern_match_in_string/3]
      ).

%---------------------------------------------------------------


:- use_module('$REGULUS/Alterf/Prolog/classifier_utilities').

:- use_module('$REGULUS/Prolog/regulus_eval').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(system)).
:- use_module(library(lists)).

%---------------------------------------------------------------

/*

rec_results_to_feature_vectors(+RecResultsAlist, +FeatureExtractionSpecAlist, +FeatVectors)

Create feature vector file.

1. RecResultsAlist

List of one or more terms of the form Key-File, containing Prolog-readable rec results produced
by real or text batchrec. See batchrec.pl and home/speech/CambridgePrologLib/batchrec_tools.pl.

2. FeatureExtractionSpecAlist

List of one or more terms, in format defined in classifier_trainer.pl.

3. FeatVectors

File of feat vectors, in format

wavfile_with_words_confidence_and_feats(Id, WordsConfidenceFeatsList)

where Id is the wavfile ID, and WordsConfidenceFeatsList is a list of one or more items of the form

[Words, Confidence, Feats]

where Words is a list of words (Prolog atoms), Confidence is the confidence score (integer) and
Feats is a list of features.

------------------------------------------------------------------------------------------

feat_for_surface_words(-Feature, +FeatureExtractionSpec, +Words)

Find a feature based on surface words.

1. FeatureExtractionSpec is a term in format defined in classifier_trainer.pl.

2. Words is a list of surface words.

3. Feature is a feature of a type permitted by FeatureExtractionSpec occurring in Words.

------------------------------------------------------------------------------------------

feat_for_nl_value(-Feature, +FeatureExtractionSpec, +NLVal)

Find a feature based on NLValue (aka LF).

1. FeatureExtractionSpec is a term in format defined in classifier_trainer.pl.

2. NLVal is a Nuance NLValue encoded as a Prolog term

3. Feature is a feature of a type permitted by FeatureExtractionSpec occurring in NLVal.

------------------------------------------------------------------------------------------

words_covered_by_surface_pattern(+Words, +SemanticAtom, -PossibleWordsCovered)

Find the words on which a surface pattern match was based. There could be more than one such set of words.

1. Words is a list of words.

2. SemanticAtom is the atom for which the surface pattern match is being carried out. This could be a number, decimal number or similar

3. PossibleWordsCovered is a (possibly empty) list of lists. Each list gives the words on which one possible match was based.

------------------------------------------------------------------------------------------

hand_coded_surface_pattern_match_in_string(+Pattern, +String, -WordsMatched)

Match a surface pattern to a string.

1. Pattern is a surface pattern.

2. Words is a list of words.

3. WordsMatched is the list of words (a subset of Words) that matches the pattern, if such a subset exists.

*/


rec_results_to_feature_vectors(RecResultsAlist, FeatureExtractionSpecAlist, FeatVectors) :-
	absolute_file_name(FeatVectors, FeatVectors1),
	format('~N-- Creating file of features ~w... ', [FeatVectors1]),

	open_alist(RecResultsAlist, read, SInAlist),
	open(FeatVectors1, write, SOut),
	rec_results_to_feature_vectors_stream(SInAlist, SOut, FeatureExtractionSpecAlist, 0),
	close_alist(SInAlist),
	close(SOut),

	format('done~n', []).

%---------------------------------------------------------------

words_covered_by_surface_pattern(Words, SemanticAtom, PossibleWordsCovered) :-
	apply_tagging_grammar(Words, Words1),
	append(['*start*' | Words1], ['*end*'], Words2),

	findall(
	ExpandedWordsMatched, 
	expanded_hand_coded_surface_pattern_match_in_string(Words2, SemanticAtom, ExpandedWordsMatched), 
	PossibleWordsCovered0),

	remove_duplicates(PossibleWordsCovered0, PossibleWordsCovered).

expanded_hand_coded_surface_pattern_match_in_string(Words, SemanticAtom, ExpandedWordsMatched) :-
	hand_coded_surface_pattern_match_in_string(Words, SemanticAtom, WordsMatched, atom),
	expand_using_tagging_grammar(WordsMatched, ExpandedWordsMatched).

%---------------------------------------------------------------

rec_results_to_feature_vectors_stream(SInAlist, SOut, FeatureExtractionSpecAlist, Counter) :-
	read_alist(SInAlist, ItemAlist),
	Counter1 is Counter + 1,
	Mod is Counter1 mod 100,
	(   Mod = 0 ->
	    format('~d ', [Counter1]), 
	    flush_output(user) ;
	    true
	),
	rec_results_to_feature_vectors_stream1(ItemAlist, SInAlist, SOut, FeatureExtractionSpecAlist, Counter1).

rec_results_to_feature_vectors_stream1(ItemAlist, _SInAlist, _SOut, _FeatureExtractionSpecAlist, _Counter) :-
	is_end_of_file_alist(ItemAlist),
	!.
rec_results_to_feature_vectors_stream1(ItemAlist, SInAlist, SOut, FeatureExtractionSpecAlist, Counter) :-
	rec_results_item_alist_to_feature_vectors(ItemAlist, SOut, FeatureExtractionSpecAlist),
	!,
	rec_results_to_feature_vectors_stream(SInAlist, SOut, FeatureExtractionSpecAlist, Counter).

rec_results_item_alist_to_feature_vectors(ItemAlist, SOut, FeatureExtractionSpecAlist) :-
	rec_result_alist_to_id_words_confidence_and_features_list(ItemAlist, FeatureExtractionSpecAlist, Id, WordsConfidenceFeatsList),
	format(SOut, "~N~q.~n", [wavfile_with_words_confidence_and_feats(Id, WordsConfidenceFeatsList)]),
	!.
rec_results_item_alist_to_feature_vectors(ItemAlist, SOut, FeatureExtractionSpecAlist) :-
	format('~N*** Error: bad call: ~w~n', [rec_results_item_alist_to_feature_vectors(ItemAlist, SOut, FeatureExtractionSpecAlist)]).

rec_result_alist_to_id_words_confidence_and_features_list([], _FeatureExtractionSpecAlist, _Id, []) :-
	!.
rec_result_alist_to_id_words_confidence_and_features_list([Key-Item | ItemAlistR], FeatureExtractionSpecAlist, Id, [[Words, Confidence, Feats] | ListR]) :-
	member(Key-FeatureExtractionSpec, FeatureExtractionSpecAlist),
	rec_result_to_id_words_confidence_and_features(Item, FeatureExtractionSpec, Id, Words, Confidence, Feats),
	!,
	rec_result_alist_to_id_words_confidence_and_features_list(ItemAlistR, FeatureExtractionSpecAlist, _SameId, ListR).

%---------------------------------------------------------------

rec_result_to_id_words_confidence_and_features(batchrec_item(Pairs), FeatureExtractionSpec, Id, Words, Confidence, Feats) :-
	member(wavfile=Id, Pairs),
	member(words=Words, Pairs),
	member(confidence=Confidence, Pairs),
	findall(Feat, feat_for_batchrec_pairs(Feat, FeatureExtractionSpec, Pairs), Feats),
	!.
rec_result_to_id_words_confidence_and_features(Item, FeatureExtractionSpec, Id, Words, Confidence, Feats) :-
	format('*** Error: bad call: ~w~n', [rec_result_to_id_words_confidence_and_features(Item, FeatureExtractionSpec, Id, Words, Confidence, Feats)]),
	fail.

% Every utterance has the 'no_info' feature - we want this for normalisation.
feat_for_batchrec_pairs(no_info, _FeatureExtractionSpec, _Pairs).
feat_for_batchrec_pairs(Feat, FeatureExtractionSpec, Pairs) :-
	member(confidence=Confidence, Pairs),
	feat_for_confidence_score(Feat, FeatureExtractionSpec, Confidence).
feat_for_batchrec_pairs(Feat, FeatureExtractionSpec, Pairs) :-
	member(words=Words, Pairs),
	feat_for_surface_words(Feat, FeatureExtractionSpec, Words).
feat_for_batchrec_pairs(Feat, FeatureExtractionSpec, Pairs) :-
	member(nl_value=NLVal, Pairs),
	feat_for_nl_value(Feat, FeatureExtractionSpec, NLVal).

%---------------------------------------------------------------

feat_for_confidence_score(Feat, FeatureExtractionSpec, Confidence) :-
	member(confidence, FeatureExtractionSpec),
	confidence_score_feat(Confidence, Feat).

confidence_score_feat(Confidence, confidence(From, To)) :-
	reference_confidence_scores(List),
	interval_in_list(List, Confidence, From, To).

reference_confidence_scores([0, 10, 20, 25, 30, 35, 40, 43, 45, 47, 50, 55, 60, 65, 70, 80, 101]).

interval_in_list([From, To | _R], Confidence, From, To) :-
	From =< Confidence, Confidence < To,
	!.
interval_in_list([_F | R], Confidence, From, To) :-
	interval_in_list(R, Confidence, From, To).

%---------------------------------------------------------------

feat_for_surface_words(unigram(Word), FeatureExtractionSpec, Words) :-
	member(unigrams, FeatureExtractionSpec),
	member(Word, Words).
feat_for_surface_words(bigram(Bigram), FeatureExtractionSpec, Words) :-
	member(bigrams, FeatureExtractionSpec),
	append(['*start*' | Words], ['*end*'], Words1),
	bigram_in_list(Bigram, Words1).
feat_for_surface_words(trigram(Bigram), FeatureExtractionSpec, Words) :-
	member(trigrams, FeatureExtractionSpec),
	append(['*start*' | Words], ['*end*'], Words1),
	trigram_in_list(Bigram, Words1).

feat_for_surface_words(class_unigram(Word), FeatureExtractionSpec, Words0) :-
	member(class_unigrams, FeatureExtractionSpec),
	apply_tagging_grammar(Words0, Words),
	member(Word, Words).
feat_for_surface_words(class_bigram(Bigram), FeatureExtractionSpec, Words0) :-
	member(class_bigrams, FeatureExtractionSpec),
	apply_tagging_grammar(Words0, Words),
	append(['*start*' | Words], ['*end*'], Words1),
	bigram_in_list(Bigram, Words1).
feat_for_surface_words(class_trigram(Bigram), FeatureExtractionSpec, Words0) :-
	member(class_trigrams, FeatureExtractionSpec),
	apply_tagging_grammar(Words0, Words),
	append(['*start*' | Words], ['*end*'], Words1),
	trigram_in_list(Bigram, Words1).
feat_for_surface_words(hand_coded_surface_pattern_match(Atom), FeatureExtractionSpec, Words0) :-
	member(hand_coded_surface_patterns, FeatureExtractionSpec),
	apply_tagging_grammar(Words0, Words),
	append(['*start*' | Words], ['*end*'], Words1),
	findall(Atom0, hand_coded_surface_pattern_match_in_string(Words1, Atom0, _WordsMatched, atom), Atoms),
	remove_duplicates(Atoms, UniqueAtoms),
	member(Atom, UniqueAtoms).	
feat_for_surface_words(hand_coded_surface_pattern(Rule), FeatureExtractionSpec, Words0) :-
	member(hand_coded_surface_pattern_rules, FeatureExtractionSpec),
	apply_tagging_grammar(Words0, Words),
	append(['*start*' | Words], ['*end*'], Words1),
	findall(Rule0, hand_coded_surface_pattern_match_in_string(Words1, Rule0, _WordsMatched, rule), Rules),
	remove_duplicates(Rules, UniqueRules),
	member(Rule, UniqueRules).	

bigram_in_list([A, B], [A, B | _Rest]).
bigram_in_list(Bigram, [_F | R]) :-
	bigram_in_list(Bigram, R).

trigram_in_list([A, B, C], [A, B, C | _Rest]).
trigram_in_list(Trigram, [_F | R]) :-
	trigram_in_list(Trigram, R).

%---------------------------------------------------------------

apply_tagging_grammar([], []) :-
	!.
apply_tagging_grammar(In, [Tag | NextOut]) :-
	tag_spanning_longest_substring(Tag, In, Next),
	!,
	apply_tagging_grammar(Next, NextOut).
apply_tagging_grammar([F | Next], [F | NextOut]) :-
	apply_tagging_grammar(Next, NextOut),
	!.
apply_tagging_grammar(In, Out) :-
	format('~N*** Error: bad call: ~w~n', [apply_tagging_grammar(In, Out)]),
	fail.

%---------------------------------------------------------------

expand_using_tagging_grammar([], []) :-
	!.
expand_using_tagging_grammar([F | Next], [F | NextOut]) :-
	atom(F),
	expand_using_tagging_grammar(Next, NextOut),
	!.
expand_using_tagging_grammar([Tag | R], Out) :-
	( number(Tag) ; \+ atomic(Tag) ),
	user:tagging_grammar(Tag, Out, RestOut),
	expand_using_tagging_grammar(R, RestOut),
	!.
expand_using_tagging_grammar(In, Out) :-
	format('~N*** Error: bad call: ~w~n', [expand_using_tagging_grammar(In, Out)]),
	fail.

%---------------------------------------------------------------

tag_spanning_longest_substring(BestTag, In, BestOut) :-
	findall(LengthOut-[Tag, Out], tag_and_remaining_length(Tag, In, Out, LengthOut), Triples),
	Triples \== [],
	keysort(Triples, SortedTriples),
	SortedTriples = [_BestLengthOut-[BestTag, BestOut] | _Rest],
	!.

tag_and_remaining_length(Tag, In, Out, LengthOut) :-
	user:tagging_grammar(Tag, In, Out),
	length(Out, LengthOut).

%---------------------------------------------------------------

feat_for_nl_value(Feat, FeatureExtractionSpec, NLVal) :-
	nl_val_to_lf_according_to_feature_extraction_spec(NLVal, FeatureExtractionSpec, LF),
	feat_for_lf(Feat, FeatureExtractionSpec, LF).

nl_val_to_lf_according_to_feature_extraction_spec(NLVal, FeatureExtractionSpec, LF) :-
	member(lf_postproc_pred=PostprocPred, FeatureExtractionSpec),
	(   member(nl_val_type=text, FeatureExtractionSpec) ->
	    regulus_eval_text(NLVal, LF, PostprocPred) ;
	    regulus_eval_speech(NLVal, LF, PostprocPred)
	),
	!.
nl_val_to_lf_according_to_feature_extraction_spec(NLVal, _FeatureExtractionSpec, LF) :-
	LF = NLVal,
	!.
nl_val_to_lf_according_to_feature_extraction_spec(NLVal, FeatureExtractionSpec, LF) :-
	format('~N*** Error: bad call: ~w~n', [nl_val_to_lf_according_to_feature_extraction_spec(NLVal, FeatureExtractionSpec, LF)]),
	fail.

feat_for_lf(post_processed_lf(LF), FeatureExtractionSpec, LF) :-
	member(post_processed_lf, FeatureExtractionSpec).

feat_for_lf(sem_atom(Atom), FeatureExtractionSpec, LF) :-
	member(sem_atoms, FeatureExtractionSpec),
	findall(Atom0, sem_atom_in_lf(LF, Atom0), Atoms),
	remove_duplicates(Atoms, UniqueAtoms),
	member(Atom, UniqueAtoms).

feat_for_lf(sem_triple(Triple), FeatureExtractionSpec, LF) :-
	member(sem_triples, FeatureExtractionSpec),
	sem_triple_in_lf(LF, Triple).

feat_for_lf(hand_coded_pattern_match(Atom), FeatureExtractionSpec, LF) :-
	member(hand_coded_patterns, FeatureExtractionSpec),
	findall(Atom0, hand_coded_pattern_match_in_lf(LF, Atom0, atom), Atoms),
	remove_duplicates(Atoms, UniqueAtoms),
	member(Atom, UniqueAtoms).	

feat_for_lf(hand_coded_pattern(Rule), FeatureExtractionSpec, LF) :-
	member(hand_coded_pattern_rules, FeatureExtractionSpec),
	findall(Rule0, hand_coded_pattern_match_in_lf(LF, Rule0, rule), Rules),
	remove_duplicates(Rules, UniqueRules),
	member(Rule, UniqueRules).	

%---------------------------------------------------------------

hand_coded_pattern_match_in_lf(Var, _TargetAtom, _AtomOrRule) :-
	var(Var),
	!,
	fail.
hand_coded_pattern_match_in_lf(Atom, TargetAtom, AtomOrRule) :-
	atomic(Atom),
	!,
	hand_coded_pattern_match_on_term(Atom, TargetAtom, AtomOrRule).
hand_coded_pattern_match_in_lf(T, TargetAtom, AtomOrRule) :-
	hand_coded_pattern_match_on_term(T, TargetAtom, AtomOrRule).
hand_coded_pattern_match_in_lf(T, TargetAtom, AtomOrRule) :-
	functor(T, _F, N),
	hand_coded_pattern_match_in_lf_args(N, T, TargetAtom, AtomOrRule).

hand_coded_pattern_match_in_lf_args(I, T, TargetAtom, AtomOrRule) :-
	I > 0,
	arg(I, T, Arg),
	hand_coded_pattern_match_in_lf(Arg, TargetAtom, AtomOrRule).
hand_coded_pattern_match_in_lf_args(I, T, TargetAtom, AtomOrRule) :-
	I > 1,
	I1 is I - 1,
	hand_coded_pattern_match_in_lf_args(I1, T, TargetAtom, AtomOrRule).

hand_coded_pattern_match_on_term(T, TargetAtom, atom) :-
	current_predicate(user:alterf_pattern/3),
	user:alterf_pattern(T, TargetAtom, _Example).
hand_coded_pattern_match_on_term(T, Rule1, rule) :-
	current_predicate(user:alterf_pattern/3),
	user:alterf_pattern(T1, TargetAtom1, Example),
	Rule1 = alterf_pattern(T1, TargetAtom1, Example),
	copy_term(Rule1, Rule),
	make_ground(Rule1),
	Rule = alterf_pattern(T, _TargetAtom, Example).

%---------------------------------------------------------------

hand_coded_surface_pattern_match_in_string(String, TargetAtom, WordsMatched, atom) :-
	current_predicate(user:alterf_surface_pattern/3),
	user:alterf_surface_pattern(Pattern, TargetAtom, Exceptions),
	hand_coded_surface_pattern_match_in_string_checking_exceptions(Pattern, String, WordsMatched, Exceptions).
hand_coded_surface_pattern_match_in_string(String, Rule1, WordsMatched, rule) :-
	current_predicate(user:alterf_surface_pattern/3),
	user:alterf_surface_pattern(Pattern1, TargetAtom1, Exceptions),
	Rule1 = alterf_surface_pattern(Pattern1, TargetAtom1, Exceptions),
	copy_term(Rule1, Rule),
	make_ground(Rule1),
	Rule = alterf_surface_pattern(Pattern, _TargetAtom, Exceptions),
	hand_coded_surface_pattern_match_in_string_checking_exceptions(Pattern, String, WordsMatched, Exceptions).

hand_coded_surface_pattern_match_in_string_checking_exceptions(Pattern, String, WordsMatched, Exceptions) :-
	hand_coded_surface_pattern_match_in_string(Pattern, String, WordsMatched),
	check_exceptions(Exceptions, Pattern, String).

hand_coded_surface_pattern_match_in_string(Pattern, String, WordsMatched) :-
	hand_coded_surface_pattern_match_in_string2(Pattern, String-_StringOut, WordsMatched-[]).
hand_coded_surface_pattern_match_in_string(Pattern, [_F | R], WordsMatched) :-
	hand_coded_surface_pattern_match_in_string(Pattern, R, WordsMatched).

hand_coded_surface_pattern_match_in_string2([], _String, WordsMatched-WordsMatched) :-
	!.
hand_coded_surface_pattern_match_in_string2(['...' | R], [F | R1]-StringOut, WordsMatched) :-
	(   hand_coded_surface_pattern_match_in_string2(R, [F | R1]-StringOut, WordsMatched) ;
	    hand_coded_surface_pattern_match_in_string2(['...' | R], R1-StringOut, WordsMatched)
	).
hand_coded_surface_pattern_match_in_string2([F | R], StringIn-StringOut, WordsMatchedIn-WordsMatchedOut) :-
	hand_coded_surface_pattern_match_in_string2(F, StringIn-StringNext, WordsMatchedIn-WordsMatchedNext),
	hand_coded_surface_pattern_match_in_string2(R, StringNext-StringOut, WordsMatchedNext-WordsMatchedOut).
hand_coded_surface_pattern_match_in_string2(Elt1/Elt2, String, WordsMatched) :-
	(   hand_coded_surface_pattern_match_in_string2(Elt1, String, WordsMatched) ;
	    hand_coded_surface_pattern_match_in_string2(Elt2, String, WordsMatched)
	).
% Don't count a match if it's in the scope of a 'not'.
hand_coded_surface_pattern_match_in_string2(not(Elt), StringIn-StringIn, WordsMatched-WordsMatched) :-
	\+ hand_coded_surface_pattern_match_in_string2(Elt, StringIn-_StringOut, _DiscardedWordsMatched-[]).
hand_coded_surface_pattern_match_in_string2(not_word(Elt), [F | R]-R, WordsMatchedOut-WordsMatchedOut) :-
	\+ hand_coded_surface_pattern_match_in_string2(Elt, [F]-[], _WordsMatched).
hand_coded_surface_pattern_match_in_string2(number(N), [N | R]-R, [N | WordsMatchedOut]-WordsMatchedOut) :-
	number(N).
hand_coded_surface_pattern_match_in_string2(Atom, [Atom | R]-R, [Atom | WordsMatchedOut]-WordsMatchedOut).

%---------------------------------------------------------------

check_exceptions(unless(List), Pattern, String) :-
	!,
	check_exceptions1(List, Pattern, String).
check_exceptions(_Exceptions, _Pattern, _String).

check_exceptions1([], _Pattern, _String).
check_exceptions1([F | R], Pattern, String) :-
	\+ hand_coded_surface_pattern_match_in_string(F, String, _WordsMatched),
	!,
	check_exceptions1(R, Pattern, String).

