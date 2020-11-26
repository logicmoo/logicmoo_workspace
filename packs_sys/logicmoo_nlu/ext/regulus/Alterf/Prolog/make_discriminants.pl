
%---------------------------------------------------------------

:- module(make_discriminants,
	  [feature_vectors_to_discriminant_scores/4,
	   present_discriminant_spec/2]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/Alterf/Prolog/generic_numbers').
:- use_module('$REGULUS/Alterf/Prolog/classifier_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(random)).

%---------------------------------------------------------------

:- dynamic no_info_score/2.

%---------------------------------------------------------------

/*

feature_vectors_to_discriminant_scores(+AnnotatedWavfiles, +FeatVectors, +DiscriminantSpec, +DiscriminantScores)

Create the file of discriminant scores out of the annotated, parsed and feature-extracted data.

1. AnnotatedWavfiles

Prolog-readable file in format 

wavfile_and_annotations(Wavfile, SemanticAtoms, Transcription)

as produced by parse_annotated_wavfiles/5 in parse_annotated_wavfiles

- Wavfile is the wavfile (Prolog atom)

- SemanticAtoms is a list of semantic atoms

- Transcription is a list of atoms.

2. FeatVectors 

Prolog-readable file associating training examples with feature bundles. Produced
by rec_results_to_feature_vectors/3 in extract_feats.pl

3. DiscriminantSpec

Specifies how to produce the discriminant values. See documentation in classifier_trainer.pl.

4. DiscriminantScores

The output file of discriminants. See documentation in classifier_trainer.pl.

*/

%---------------------------------------------------------------
% Discriminant spec features and default values

% We can use naive Bayes, or normalise with respect to the value given no information.
% Naive Bayes by default.
discriminant_spec_feature_value(bayes_version, Value) :-
	member(Value, [naive, normalised]).

% We can use only a portion of the training data.
% Use all the data by default
discriminant_spec_feature_value(use_proportion_of_data, Value) :-
	number(Value),
	0.0 =< Value, Value =< 1.0.

% We can weight the correctly recognised training examples more heavily.
% No weighting by default.
discriminant_spec_feature_value(rec_ok_weight, Value) :-
	number(Value),
	Value > 0.

% We can discard training examples with low confidence scores.
% Use only examples with confidence score >= 45 by default.
discriminant_spec_feature_value(confidence_threshold_ignore, Value) :-
	number(Value),
	Value > 0.
discriminant_spec_feature_value(confidence_threshold_non_ignore, Value) :-
	number(Value),
	Value > 0.

% We can discard discriminants with values less than a given threshold.
% Threshold = 0.5 by default.
discriminant_spec_feature_value(discriminant_score_threshold, Value) :-
	number(Value).

% We can discard discriminants based on insufficient positive examples.
% More than one positive example by default.
discriminant_spec_feature_value(discriminant_n_good_examples_threshold, Value) :-
	number(Value).

% We can calculate the MLE formula using either the standard formula or the modified one from Carter 2000
% Standard formula by default.
discriminant_spec_feature_value(mle_formula, Value) :-
	member(Value, [standard, carter]).

% We can add a bonus/penalty to discriminants for hand-coded rules
% No bonus by default
discriminant_spec_feature_value(hand_coded_rule_bonus, Value) :-
	number(Value).

% We can add a bonus/penalty to discriminants for confidence scores
% No bonus by default
discriminant_spec_feature_value(confidence_score_bonus, Value) :-
	number(Value).

% We can assume we have more positive examples for a rule-based feature if we want
% Minimum of 10 by default
discriminant_spec_feature_value(assumed_minimum_n_good_examples_for_rule, Value) :-
	number(Value),
	Value > 0.

% Default values

discriminant_spec_feature_value_default(bayes_version, naive).
discriminant_spec_feature_value_default(use_proportion_of_data, 1.0).
discriminant_spec_feature_value_default(rec_ok_weight, 1).
discriminant_spec_feature_value_default(confidence_threshold_ignore, Threshold) :-
	get_default_confidence_threshold(Threshold).
discriminant_spec_feature_value_default(confidence_threshold_non_ignore, Threshold) :-
	get_default_confidence_threshold(Threshold).
discriminant_spec_feature_value_default(discriminant_score_threshold, 0.5).
discriminant_spec_feature_value_default(discriminant_n_good_examples_threshold, 1).
discriminant_spec_feature_value_default(mle_formula, standard).
discriminant_spec_feature_value_default(hand_coded_rule_bonus, 0.0).
discriminant_spec_feature_value_default(confidence_score_bonus, 0.0).
discriminant_spec_feature_value_default(assumed_minimum_n_good_examples_for_rule, 10).

%---------------------------------------------------------------

feature_vectors_to_discriminant_scores(AnnotatedWavfiles, FeatVectors, DiscriminantSpec, DiscriminantScores) :-
	present_discriminant_spec(user, DiscriminantSpec),
	feature_vectors_to_discriminant_scores1(AnnotatedWavfiles, FeatVectors, DiscriminantSpec, DiscriminantScores).

feature_vectors_to_discriminant_scores1(AnnotatedWavfiles, FeatVectors, DiscriminantSpec, DiscriminantScores) :-
	get_discriminant_spec_feature_value(bayes_version, DiscriminantSpec, naive),
	feature_vectors_to_discriminant_scores2(AnnotatedWavfiles, FeatVectors, DiscriminantSpec, DiscriminantScores),
	!.
feature_vectors_to_discriminant_scores1(AnnotatedWavfiles, FeatVectors, DiscriminantSpec, DiscriminantScores) :-
	get_discriminant_spec_feature_value(bayes_version, DiscriminantSpec, normalised),
	make_tmp_file('unnormalised_discriminants.pl', UnnormalisedDiscriminants),
	feature_vectors_to_discriminant_scores2(AnnotatedWavfiles, FeatVectors, DiscriminantSpec, UnnormalisedDiscriminants),
	normalise_discriminants_file(UnnormalisedDiscriminants, DiscriminantScores),
	!.
feature_vectors_to_discriminant_scores1(AnnotatedWavfiles, FeatVectors, DiscriminantSpec, DiscriminantScores) :-
	format('~N*** Error: bad call: ~w~n', [feature_vectors_to_discriminant_scores1(AnnotatedWavfiles, FeatVectors, DiscriminantSpec, DiscriminantScores)]),
	fail.

feature_vectors_to_discriminant_scores2(AnnotatedWavfiles, FeatVectors, DiscriminantSpec, DiscriminantScores) :-
	check_lengths_of_annotated_wavfiles_and_feat_vectors_files(AnnotatedWavfiles, FeatVectors),
	retractall(no_info_score(_, _)),
	
	make_tmp_file(raw_discriminant_data, TmpRawDiscriminants),
	absolute_file_name(DiscriminantScores, DiscriminantScores1),

	get_all_semantic_atoms_from_annotated_wavfiles(AnnotatedWavfiles, AllSemanticAtomsOS),
	length(AllSemanticAtomsOS, NAtoms),
	format('~N-- Creating discriminant file (~d target atoms: ~w)~n', [NAtoms, AllSemanticAtomsOS]),

	load_annotated_wavfiles_file(AnnotatedWavfiles),
	feature_vectors_to_raw_discriminant_data(FeatVectors, AllSemanticAtomsOS, DiscriminantSpec, TmpRawDiscriminants),
	raw_discriminant_data_to_discriminants(TmpRawDiscriminants, DiscriminantSpec, DiscriminantScores),
	format('~N-- Discriminants file ~w created~n', [DiscriminantScores1]).

%---------------------------------------------------------------

present_discriminant_spec(S, DiscriminantSpec) :-
	findall(Feature, discriminant_spec_feature_value_default(Feature, _Default), FeatureList),
	format(S, '~N% Feature spec:~n', []),
	present_discriminant_spec1(FeatureList, DiscriminantSpec, S),
	!.
present_discriminant_spec(S, DiscriminantSpec) :-
	format('~N*** Error: bad call: ~w~n', [present_discriminant_spec(S, DiscriminantSpec)]),
	fail.

present_discriminant_spec1([], _DiscriminantSpec, _S).
present_discriminant_spec1([F | R], DiscriminantSpec, S) :-
	present_discriminant_spec_feature(F, DiscriminantSpec, S),
	present_discriminant_spec1(R, DiscriminantSpec, S).

present_discriminant_spec_feature(Feat, DiscriminantSpec, S) :-
	get_discriminant_spec_feature_value(Feat, DiscriminantSpec, Value),
	format(S, '~N%    ~w = ~w~n', [Feat, Value]),
	!.

%---------------------------------------------------------------

check_lengths_of_annotated_wavfiles_and_feat_vectors_files(AnnotatedWavfiles, FeatVectors) :-
	number_of_prolog_records_in_file(AnnotatedWavfiles, LengthAnnotatedWavfiles),
	number_of_prolog_records_in_file(FeatVectors, LengthFeatVectors),
	!,
	(   LengthAnnotatedWavfiles = LengthFeatVectors ->
	    true ;
	    format('~N*** Warning: annotated wavfiles file has ~d records, feature vectors file has ~d records. Wavfile corpus data probably missing or in the wrong place.~n', [LengthAnnotatedWavfiles, LengthFeatVectors])
	).
check_lengths_of_annotated_wavfiles_and_feat_vectors_files(AnnotatedWavfiles, FeatVectors) :-
	format('~N*** Error: bad call: ~w~n', [check_lengths_of_annotated_wavfiles_and_feat_vectors_files(AnnotatedWavfiles, FeatVectors)]),
	fail.

%---------------------------------------------------------------

get_all_semantic_atoms_from_annotated_wavfiles(AnnotatedWavfiles, AllSemanticAtomsOS) :-
	open(AnnotatedWavfiles, read, S),
	get_all_semantic_atoms_from_annotated_wavfiles_stream(S, []-AllSemanticAtoms),
	remove_numbers_from_list(AllSemanticAtoms, AllSemanticAtoms1),
	findall(GenericNumber, generic_replacement_for_number_type(_Type, other, GenericNumber), GenericNumbers),
	append(GenericNumbers, AllSemanticAtoms1, AllSemanticAtoms2),
	list_to_ord_set(AllSemanticAtoms2, AllSemanticAtomsOS),
	warn_about_undeclared_atoms(AllSemanticAtomsOS),
	close(S).

get_all_semantic_atoms_from_annotated_wavfiles_stream(S, In-Out) :-
	read(S, Term),
	get_all_semantic_atoms_from_annotated_wavfiles_stream1(Term, S, In-Out).

get_all_semantic_atoms_from_annotated_wavfiles_stream1(end_of_file, _S, In-In) :-
	!.
get_all_semantic_atoms_from_annotated_wavfiles_stream1(Term, S, In-Out) :-
	Term = wavfile_and_annotations(_Wavfile, Atoms, _Transcription),
	!,
	append(In, Atoms, Next),
	get_all_semantic_atoms_from_annotated_wavfiles_stream(S, Next-Out).
get_all_semantic_atoms_from_annotated_wavfiles_stream1(Term, _S, _Other) :-
	format('~N*** Error: unknown term ~q in annotated wav file~n', [Term]),
	fail.

%---------------------------------------------------------------

warn_about_undeclared_atoms(AllSemanticAtomsOS) :-
	findall(UndeclaredAtom, ( member(UndeclaredAtom, AllSemanticAtomsOS), \+ user:target_atom(UndeclaredAtom, _Type) ), UndeclaredAtoms),
	(   UndeclaredAtoms = [] ->
	    true ;
	    format('~N*** Warning: following atoms not declared in target language model: ~w~n', [UndeclaredAtoms])
	).	
 
%---------------------------------------------------------------

load_annotated_wavfiles_file(AnnotatedWavfiles) :-
	(   current_predicate(wavfile_and_annotations, wavfile_and_annotations(_, _, _)) ->
	    abolish(wavfile_and_annotations/3) ;
	    true
	),
	compile(AnnotatedWavfiles).

%---------------------------------------------------------------

feature_vectors_to_raw_discriminant_data(FeatVectors, AllSemanticAtomsOS, DiscriminantSpec, RawDiscriminants) :-
	absolute_file_name(RawDiscriminants, RawDiscriminants1),
	format('~N  -- Creating raw discriminants data file ~w... ', [RawDiscriminants1]),

	open(FeatVectors, read, SFeats),
	open(RawDiscriminants, write, SOut),

	feature_vectors_to_raw_discriminant_stream(SFeats, AllSemanticAtomsOS, DiscriminantSpec, SOut, 0-Records, 0-RecordsUsed),

	close(SFeats),
	close(SOut),

	format('done (~d/~d records used)~n', [RecordsUsed, Records]),
	flush_output(user).

feature_vectors_to_raw_discriminant_stream(SFeats, AllSemanticAtomsOS, DiscriminantSpec, SOut, CountIn-CountOut, UsedIn-UsedOut) :-
	read(SFeats, FeatsTerm),

	feature_vectors_to_raw_discriminant_stream1(FeatsTerm, SFeats, AllSemanticAtomsOS, DiscriminantSpec, SOut, CountIn-CountOut, UsedIn-UsedOut).

feature_vectors_to_raw_discriminant_stream1(end_of_file, _SFeats, _AllSemanticAtomsOS, _DiscriminantSpec, _SOut, Count-Count, Used-Used) :-
	!.
feature_vectors_to_raw_discriminant_stream1(FeatsTerm, SFeats, AllSemanticAtomsOS, DiscriminantSpec, SOut, CountIn-CountOut, UsedIn-UsedOut) :-
	feature_vector_item_to_raw_discriminant_stream(FeatsTerm, AllSemanticAtomsOS, DiscriminantSpec, SOut, CountIn-CountNext, UsedIn-UsedNext),
	Mod is CountNext mod 100,
	(   Mod = 0 ->
	    format('~d ', [CountNext]), 
	    flush_output(user) ;
	    true
	),	
	!,
	feature_vectors_to_raw_discriminant_stream(SFeats, AllSemanticAtomsOS, DiscriminantSpec, SOut, CountNext-CountOut, UsedNext-UsedOut).

%---------------------------------------------------------------

feature_vector_item_to_raw_discriminant_stream(FeatsTerm, AllSemanticAtomsOS, DiscriminantSpec, SOut, CountIn-CountOut, UsedIn-UsedOut) :-
	(   FeatsTerm = wavfile_with_words_confidence_and_feats(Wavfile, WordsConfidenceFeatsList) ->
	    true ;
	    format('~N*** Error: unknown term in feat vectors file ~q~n', [FeatsTerm]),
	    fail
	),

	get_discriminant_spec_feature_value(use_proportion_of_data, DiscriminantSpec, Proportion),
	get_discriminant_spec_feature_value(confidence_threshold_non_ignore, DiscriminantSpec, ConfidenceThresholdNonIgnore),
	get_discriminant_spec_feature_value(confidence_threshold_ignore, DiscriminantSpec, ConfidenceThresholdIgnore),

	(   wavfile_and_annotations(Wavfile, PositiveSemanticAtomsOS, Transcription) ->

	    CountOut is CountIn + 1,

	    (   PositiveSemanticAtomsOS = [ignore] ->
		ConfidenceThreshold = ConfidenceThresholdIgnore ;
		ConfidenceThreshold = ConfidenceThresholdNonIgnore
	    ),

	    (   record_not_in_proportion_used(CountIn, Proportion) ->
		UsedOut is UsedIn ;

		all_elements_in_words_confidence_feats_list_under_confidence_threshold(WordsConfidenceFeatsList, ConfidenceThreshold) ->
		UsedOut is UsedIn ;

		words_confidence_feats_list_to_raw_discriminant_stream(WordsConfidenceFeatsList, Transcription, ConfidenceThreshold,
		                                                       AllSemanticAtomsOS, PositiveSemanticAtomsOS, SOut),
		UsedOut is UsedIn + 1 
	    ) ;

	    format('~N*** Warning: no wavfile_and_annotations record found for wavfile ~w referred to in features file.~n', [Wavfile]),
	    CountOut is CountIn,
	    UsedOut is UsedIn	    
	),
	!.
feature_vector_item_to_raw_discriminant_stream(FeatsTerm, AllSemanticAtomsOS, SOut, Count, Used) :-
	format('~NError: bad call: ~w~n', [feature_vector_item_to_raw_discriminant_stream(FeatsTerm, AllSemanticAtomsOS, SOut, Count, Used)]),
	fail.

%---------------------------------------------------------------

record_not_in_proportion_used(Count, Proportion) :-
	CountUnits is Count mod 10,
	CountTens is ( Count // 10 ) mod 10,

	ReversedCountLastTwoDigitsProportion is ( 10 * CountUnits + CountTens ) / 100,

	ReversedCountLastTwoDigitsProportion >= Proportion.

%---------------------------------------------------------------

all_elements_in_words_confidence_feats_list_under_confidence_threshold([], _ConfidenceThreshold).
all_elements_in_words_confidence_feats_list_under_confidence_threshold([F | R], ConfidenceThreshold) :-
	F = [_RecognisedWords, Confidence, _Feats],
	Confidence < ConfidenceThreshold,
	!,
	all_elements_in_words_confidence_feats_list_under_confidence_threshold(R, ConfidenceThreshold).

%---------------------------------------------------------------

words_confidence_feats_list_to_raw_discriminant_stream([], _Transcription, _Threshold, _AllSemanticAtomsOS, _PositiveSemanticAtomsOS, _SOut) :-
	!.
words_confidence_feats_list_to_raw_discriminant_stream([F | R], Transcription, Threshold, AllSemanticAtomsOS, PositiveSemanticAtomsOS, SOut) :-
	words_confidence_feats_item_to_raw_discriminant_stream(F, Transcription, Threshold, AllSemanticAtomsOS, PositiveSemanticAtomsOS, SOut),
	!,
	words_confidence_feats_list_to_raw_discriminant_stream(R, Transcription, Threshold, AllSemanticAtomsOS, PositiveSemanticAtomsOS, SOut).

words_confidence_feats_item_to_raw_discriminant_stream(Item, Transcription, Threshold, AllSemanticAtomsOS, PositiveSemanticAtomsOS, SOut) :-
	Item = [RecognisedWords, Confidence, Feats],
	(   Confidence < Threshold ->
	    true ;
	    feature_vector_item_to_raw_discriminant_stream1(Feats, RecognisedWords, Transcription, AllSemanticAtomsOS, PositiveSemanticAtomsOS, SOut)
	),
	!.
words_confidence_feats_item_to_raw_discriminant_stream(Item, Transcription, Threshold, AllSemanticAtomsOS, PositiveSemanticAtomsOS, SOut) :-
	format('~N*** Error: bad call: ~w~n', [words_confidence_feats_item_to_raw_discriminant_stream(Item, Transcription, Threshold, AllSemanticAtomsOS, PositiveSemanticAtomsOS, SOut)]),
	fail.

%---------------------------------------------------------------

feature_vector_item_to_raw_discriminant_stream1(Feats, RecognisedWords, Transcription, AllSemanticAtomsOS, PositiveSemanticAtomsOS, SOut) :-
	(   RecognisedWords = Transcription ->
	    RecCorrect = rec_g ;
	    RecCorrect = rec_b
	),
	ord_subtract(AllSemanticAtomsOS, PositiveSemanticAtomsOS, NegativeSemanticAtomsOS),
	features_atoms_and_polarity_to_raw_discriminant_stream(Feats, PositiveSemanticAtomsOS, g, RecCorrect, SOut),
	features_atoms_and_polarity_to_raw_discriminant_stream(Feats, NegativeSemanticAtomsOS, b, RecCorrect, SOut),
	!.
feature_vector_item_to_raw_discriminant_stream1(Feats, RecognisedWords, Transcription, AllSemanticAtomsOS, PositiveSemanticAtomsOS, SOut) :-
	format('~N*** Error: bad call: ~w~n', [feature_vector_item_to_raw_discriminant_stream1(Feats, RecognisedWords, Transcription, AllSemanticAtomsOS, PositiveSemanticAtomsOS, SOut)]),
	fail.

features_atoms_and_polarity_to_raw_discriminant_stream([], _SemanticAtoms, _Polarity, _RecCorrect, _S).
features_atoms_and_polarity_to_raw_discriminant_stream([Feat | RestFeats], SemanticAtoms, Polarity, RecCorrect, S) :-
	feature_atoms_and_polarity_to_raw_discriminant_stream(SemanticAtoms, Feat, Polarity, RecCorrect, S),
	!,
	features_atoms_and_polarity_to_raw_discriminant_stream(RestFeats, SemanticAtoms, Polarity, RecCorrect, S).

feature_atoms_and_polarity_to_raw_discriminant_stream([], _Feat, _Polarity, _RecCorrect, _S).
feature_atoms_and_polarity_to_raw_discriminant_stream([Atom | RestAtoms], Feat, Polarity, RecCorrect, S) :-
	atom_feature_and_polarity_to_raw_discriminant_stream(Atom, Feat, Polarity, RecCorrect, S),
	!,
	feature_atoms_and_polarity_to_raw_discriminant_stream(RestAtoms, Feat, Polarity, RecCorrect, S).

atom_feature_and_polarity_to_raw_discriminant_stream(Atom, Feat, Polarity, RecCorrect, S) :-
	make_numbers_in_atom_and_feat_generic(Atom, Feat, Atom1, Feat1),
	format(S, '~N~q.~n', [t(Feat1, Atom1, Polarity, RecCorrect)]).

%---------------------------------------------------------------

raw_discriminant_data_to_discriminants(RawDiscriminants, DiscriminantSpec, Discriminants) :-
	make_tmp_file(sorted_raw_discriminant_data, TmpSortedRawDiscriminants),
	format('~N  -- Sorting file... ', []),
	sort_file(RawDiscriminants, TmpSortedRawDiscriminants),
	format('done~n', []),
	sorted_raw_discriminant_data_to_discriminants(TmpSortedRawDiscriminants, DiscriminantSpec, Discriminants).

sorted_raw_discriminant_data_to_discriminants(SortedRawDiscriminants, DiscriminantSpec, Discriminants) :-
	open(SortedRawDiscriminants, read, SIn),
	open(Discriminants, write, SOut),

	format('~N  -- Creating final discriminants file... ', []),
	%write_discriminant_file_intro(SOut),

	sorted_raw_discriminant_stream_to_discriminants(SIn, SOut, '*start*', DiscriminantSpec, 0, 0, 0),

	close(SIn),
	close(SOut).

write_discriminant_file_intro(SOut) :-
	format(SOut, '~N:- module(discriminants, [d/5]).~n~n', []).

sorted_raw_discriminant_stream_to_discriminants(SIn, SOut, CurrentFeatAndAtom, DiscriminantSpec, CurrentG, CurrentB, Count) :-
	read(SIn, Term),
	Count1 is Count + 1,
	Mod is Count1 mod 10000,
	(   Mod = 0 ->
	    format('~d ', [Count1]), 
	    flush_output(user) ;
	    true
	),	
	!,
	sorted_raw_discriminant_stream_to_discriminants1(Term, SIn, SOut, CurrentFeatAndAtom, DiscriminantSpec, CurrentG, CurrentB, Count1).

sorted_raw_discriminant_stream_to_discriminants1(end_of_file, _SIn, SOut, CurrentFeatAndAtom, DiscriminantSpec, CurrentG, CurrentB, _Count) :-
	(   CurrentFeatAndAtom = '*start*' ->
	    true ;
	    store_discriminant(SOut, CurrentFeatAndAtom, CurrentG, CurrentB, DiscriminantSpec)
	),
	!.
sorted_raw_discriminant_stream_to_discriminants1(Term, SIn, SOut, CurrentFeatAndAtom, DiscriminantSpec, CurrentG, CurrentB, Count) :-
	Term = t(Feat, Atom, Polarity, RecGood),
	CurrentFeatAndAtom = [Feat, Atom],
	get_discriminant_spec_feature_value(rec_ok_weight, DiscriminantSpec, RecOKWeight),
	(   RecGood = rec_g ->
	    Inc = RecOKWeight ;
	    Inc = 1
	),
	(   Polarity = g ->

	    NextG is CurrentG + Inc,
	    NextB is CurrentB ;
	    
	    NextG is CurrentG,
	    NextB is CurrentB + Inc
	),
	!,
	sorted_raw_discriminant_stream_to_discriminants(SIn, SOut, CurrentFeatAndAtom, DiscriminantSpec, NextG, NextB, Count).
sorted_raw_discriminant_stream_to_discriminants1(Term, SIn, SOut, CurrentFeatAndAtom, DiscriminantSpec, CurrentG, CurrentB, Count) :-
	(   CurrentFeatAndAtom = '*start*' ->
	    true ;
	    store_discriminant(SOut, CurrentFeatAndAtom, CurrentG, CurrentB, DiscriminantSpec)
	),
	Term = t(NewFeat, NewAtom, Polarity, RecGood),
	get_discriminant_spec_feature_value(rec_ok_weight, DiscriminantSpec, RecOKWeight),
	(   RecGood = rec_g ->
	    Inc = RecOKWeight ;
	    Inc = 1
	),
	(   Polarity = g ->

	    NextG is Inc,
	    NextB is 0 ;
	    
	    NextG is 0,
	    NextB is Inc
	),	
	!,
	sorted_raw_discriminant_stream_to_discriminants(SIn, SOut, [NewFeat, NewAtom], DiscriminantSpec, NextG, NextB, Count).
sorted_raw_discriminant_stream_to_discriminants1(Term, SIn, SOut, CurrentFeatAndAtom, DiscriminantSpec, CurrentG, CurrentB, Count) :-
	format('~NError: bad call: ~w~n', [sorted_raw_discriminant_stream_to_discriminants1(Term, SIn, SOut, CurrentFeatAndAtom, DiscriminantSpec, CurrentG, CurrentB, Count)]),
	fail.

store_discriminant(SOut, [Feat, Atom], G, B, DiscriminantSpec) :-
	discriminant_score(Feat, G, B, DiscriminantSpec, Score0),
	apply_discriminant_bonuses(Feat, Score0, DiscriminantSpec, Score),

	(   Feat = no_info ->
	    assertz(no_info_score(Atom, Score)) ;

	    useful_discriminant(Feat, Atom, G, B, DiscriminantSpec, Score) ->
	    format(SOut, '~N~q.~n', [d(Feat, Atom, G, B, Score)]) ;

	    true
	).

apply_discriminant_bonuses(hand_coded_pattern_match(_Atom), Score0, DiscriminantSpec, Score) :-
	get_discriminant_spec_feature_value(hand_coded_rule_bonus, DiscriminantSpec, Bonus),
	Score is Score0 + Bonus,
	!.
apply_discriminant_bonuses(confidence(_From, _To), Score0, DiscriminantSpec, Score) :-
	get_discriminant_spec_feature_value(confidence_score_bonus, DiscriminantSpec, Bonus),
	Score is Score0 + Bonus,
	!.
apply_discriminant_bonuses(_Feat, Score, _DiscriminantSpec, Score).

%---------------------------------------------------------------

% We only want the hand-coded rules to predict the atoms explicitly mentioned...
% can argue for the other option, but what we have seems more in tune with the political agenda.
% We however want to keep ALL of these discriminants (not just the ones with sufficiently
% high score) since again this seems more consonant with the idea of hand-coded rules.
useful_discriminant(hand_coded_pattern_match(Atom), OtherAtom, _G, _B, _DiscriminantSpec, _Score) :-
	!,
	Atom = OtherAtom.
% Except for the hand-coded rule case, we have no current use for discriminants with negative scores.
% It is also in general useless to predict '*other_number*' etc, since these tags can 
% never be correct.
useful_discriminant(_Feat, Atom, G, _B, DiscriminantSpec, Score) :-
	get_discriminant_spec_feature_value(discriminant_score_threshold, DiscriminantSpec, ScoreThreshold),
	get_discriminant_spec_feature_value(discriminant_n_good_examples_threshold, DiscriminantSpec, NGoodExamplesThreshold),
	Score > ScoreThreshold,
	G > NGoodExamplesThreshold,
	\+ generic_replacement_for_number_type(_Type, other, Atom),
	!.

%---------------------------------------------------------------

discriminant_score(Feat, G, B, DiscriminantSpec, Score) :-
	adjust_g_and_b_counts_on_hand_coded_rule_feats(Feat, DiscriminantSpec, G, B, G1, B1),
	probability_estimate(G1, B1, DiscriminantSpec, Prob),
	(   Prob < 0.5 ->
	    Score0 is 1 + log(2, Prob) ;
	    Score0 is -1 * ( 1 + log(2, (1 - Prob)) )
	),
	Score is round( 100 * Score0 ) / 100.

adjust_g_and_b_counts_on_hand_coded_rule_feats(Feat, DiscriminantSpec, G, B, G1, B1) :-
	(   Feat = hand_coded_pattern_match(_Atom);
	    Feat = hand_coded_surface_pattern_match(_Atom)
	),
	B = 0,
	get_discriminant_spec_feature_value(assumed_minimum_n_good_examples_for_rule, DiscriminantSpec, AssumedMinG),
	G < AssumedMinG,
	G1 = AssumedMinG,
	B1 = B,
	!.
adjust_g_and_b_counts_on_hand_coded_rule_feats(_Feat, _DiscriminantSpec, G, B, G, B) :-
	!.
adjust_g_and_b_counts_on_hand_coded_rule_feats(Feat, DiscriminantSpec, G, B, G1, B1) :-
	format('*** Error: bad call: ~w~n', [adjust_g_and_b_counts_on_hand_coded_rule_feats(Feat, DiscriminantSpec, G, B, G1, B1)]),
	fail.

probability_estimate(G, B, DiscriminantSpec, Prob) :-
	get_discriminant_spec_feature_value(mle_formula, DiscriminantSpec, FormulaType),
	probability_estimate1(FormulaType, G, B, Prob).

% The standard Naive Bayes formula 
probability_estimate1(standard, G, B, Prob) :-
	Prob is ( G + 1 ) / ( G + B + 2).
% Dave's complicated formula (see appendix to Spoken Language Translator book)
probability_estimate1(carter, G, B, Prob) :-
	Prob is ( G * (G + B + 3 ) + 1 ) / ( (G + B ) * (G + B + 3 ) + 2 ).

%---------------------------------------------------------------

normalise_discriminants_file(InFile, OutFile) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),
	format('~N  -- Normalising scores in discriminants file ~w... ', [AbsInFile]),
	
	open(AbsInFile, read, SIn),
	open(AbsOutFile, write, SOut),

	normalise_discriminants_stream(SIn, SOut),

	close(SIn),
	close(SOut),
	format('~N  -- Output written to file ~w ', [AbsOutFile]),
	!.
normalise_discriminants_file(InFile, OutFile) :-
	format('~N*** Error: bad call: ~w~n', [normalise_discriminants_file(InFile, OutFile)]),
	fail.

normalise_discriminants_stream(SIn, SOut) :-
	read(SIn, Term),
	normalise_discriminants_stream1(Term, SIn, SOut).

normalise_discriminants_stream1(end_of_file, _SIn, _SOut) :-
	!.
normalise_discriminants_stream1(Term, SIn, SOut) :-
	normalise_discriminant_file_entry(Term, Term1),
	format(SOut, '~N~q.~n', Term1),
	!,
	normalise_discriminants_stream(SIn, SOut).

normalise_discriminant_file_entry(Term, Term1) :-
	Term = d(_Feat, _Atom, _G, _B, _Score),
	!,
	normalise_discriminant_file_entry1(Term, Term1).
normalise_discriminant_file_entry(Term, Term).

normalise_discriminant_file_entry1(Term, Term1) :-
	Term = d(Feat, Atom, G, B, Score),
	(   generic_replacement_for_number_type(Type, same, Atom) ->
	    generic_replacement_for_number_type(Type, other, OtherAtom),
	    no_info_score(OtherAtom, ScoreNoInfo) ;
	    no_info_score(Atom, ScoreNoInfo)
	),
	NormalisedScore0 is Score - ScoreNoInfo,
	NormalisedScore is round( 100 * NormalisedScore0 ) / 100,
	Term1 = d(Feat, Atom, G, B, NormalisedScore),
	!.
normalise_discriminant_file_entry1(Term, Term1) :-
	format('~N*** Error: bad call: ~q~n', [normalise_discriminant_file_entry1(Term, Term1)]),
	fail.

%---------------------------------------------------------------

get_discriminant_spec_feature_value(Feature, DiscriminantSpec, Value) :-
	nonvar(DiscriminantSpec),
	member((Feature = SpecifiedValue), DiscriminantSpec),
	!,
	(   discriminant_spec_feature_value(Feature, SpecifiedValue) ->
	    Value = SpecifiedValue ;
	    format('~N*** Error: bad value of discriminant spec feature "~w": ~w~n', [Feature, Value]),
	    fail
	).
get_discriminant_spec_feature_value(Feature, DiscriminantSpec, Value) :-
	nonvar(DiscriminantSpec),
	discriminant_spec_feature_value_default(Feature, Default),
	!,
	Value = Default.
get_discriminant_spec_feature_value(Feature, DiscriminantSpec, Value) :-
	format('~N*** Error: bad call: ~w~n', [get_discriminant_spec_feature_value(Feature, DiscriminantSpec, Value)]),
	fail.
