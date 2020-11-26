% surface_parse.pl

% Interface to surface parsing using Alterf
 
:- module(surface_parse,
	  [surface_patterns_parse/2,
	   compile_surface_constituents_file/2]
      ).

:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/Alterf/Prolog/classifier_decoder').
:- use_module('$REGULUS/Alterf/Prolog/extract_feats').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

current_feature_extraction_spec([%class_unigrams,
				 %class_bigrams, class_trigrams, 
	                         %sem_triples, 
                                 %hand_coded_patterns,
				 hand_coded_surface_patterns
                                 %lf_postproc_pred=riacs_postproc_lf
                                 %nl_val_type=text]).
				 %nl_val_type=speech]).
				]).
 
%----------------------------------------------------------------------

compile_surface_constituents_file(InFile, OutFile) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),
	
	format('~N~n--- Compiling surface constituents rule file: ~w~n', [AbsInFile]),
	prolog_file_to_list(AbsInFile, RawRules),

	compile_surface_constituents_rules(RawRules, CompiledRules),
	length(CompiledRules, NRules),

	list_to_prolog_file(CompiledRules, AbsOutFile),
	format('~N~n--- Compiled surface constituents rule file (~d items) written to: ~w~n~n', [NRules, AbsOutFile]),
	!.
compile_surface_constituents_file(InFile, _OutFile) :-
	format2error('~N~n*** Error: unable to compile surface constituents rule file ~w~n', [InFile]),
	fail.

compile_surface_constituents_rules([], []).
compile_surface_constituents_rules([F | R], [F1 | R1]) :-
	compile_surface_constituents_rule(F, F1),
	!,
	compile_surface_constituents_rules(R, R1).

compile_surface_constituents_rule(surface_constituent_boundary(PreviousWords, FollowingWords, StartOrEnd, Tag),
				  compiled_surface_constituent_boundary(ReversedPreviousWords1, FollowingWords1, StartOrEnd, Tag)) :-
	(   is_list(PreviousWords) ->
	    reverse(PreviousWords, ReversedPreviousWords) ;
	    ReversedPreviousWords = PreviousWords
	),
	add_start_element_to_list(ReversedPreviousWords, ReversedPreviousWords1),
	add_start_element_to_list(FollowingWords, FollowingWords1),
	(   member(StartOrEnd, [start, end]) ->
	    true
	;
	    format2error('~*** Error: rule type "~w" in surface_constituent_boundary rule must be "start" or "end"', [StartOrEnd]),
	    fail
	),
	(   atomic(Tag) ->
	    true
	;
	    format2error('~*** Error: tag "~w" in surface_constituent_boundary rule must be atomic', [Tag]),
	    fail
	),
	!.
compile_surface_constituents_rule(F, _F1) :-
	format2error('~N~n*** Error: unable to compile rule in surface constituents rule file: ~w~n',
		     [F]),
	fail.

%----------------------------------------------------------------------

surface_patterns_parse(Words, Representation) :-
	extract_surface_constitutents(Words, Words1, SurfaceConstituents),
	surface_patterns_parse1(Words1, TopLevelPartOfRepresentation),
	surface_patterns_parse1_on_surface_constituents(SurfaceConstituents, SurfaceConstituentsPartOfRepresentation),
	append(TopLevelPartOfRepresentation, SurfaceConstituentsPartOfRepresentation, Representation0),
	postprocess_after_surface_parse(Representation0, Representation, top_level),
	!.
surface_patterns_parse(Words, Representation) :-
	format2error('~N*** Error: bad call: ~w', [surface_patterns_parse(Words, Representation)]),
	fail.
	
%----------------------------------------------------------------------

% extract_surface_constitutents(+Words, -Words1, -SurfaceConstituents)

extract_surface_constitutents(Words, Words, []) :-
	\+ current_predicate(user:compiled_surface_constituent_boundary/4),
	!.
extract_surface_constitutents(Words, WordsLeft, SurfaceConstituents) :-
	extract_surface_constitutents1(Words, [], [], top_level, WordsLeft, SurfaceConstituents),
	!.

%----------------------------------------------------------------------

% extract_surface_constitutents1(+Words, +ReversedPrevious, +ReversedWordsInCurrentConstituent, +Tag, -WordsLeft, -Constituents).

% End of string, at top-level
extract_surface_constitutents1([], _ReversedPrevious, [], top_level, [], []) :-
	!.
% End of string, not at top-level
extract_surface_constitutents1([], _ReversedPrevious, ReversedCurrent, Tag, [], [Tag-Current]) :-
	Tag \== top_level,
	reverse(ReversedCurrent, Current),
	!.
% Start of constituent, at top-level
extract_surface_constitutents1(Words, ReversedPrevious, [], top_level, WordsLeft, Constituents) :-
	match_constituent_boundary(ReversedPrevious, Words, start, Tag),
	!,
	extract_surface_constitutents1(Words, ReversedPrevious, [], Tag, WordsLeft, Constituents).
% Not start of constituent, at top-level
extract_surface_constitutents1([F | R], ReversedPrevious, [], top_level, WordsLeft, Constituents) :-
	!,
	NextReversedPrevious = [F | ReversedPrevious],
	WordsLeft = [F | NextWordsLeft], 
	extract_surface_constitutents1(R, NextReversedPrevious, [], top_level, NextWordsLeft, Constituents).
% End of constituent, not at top-level
extract_surface_constitutents1([F | R], ReversedPrevious, ReversedCurrent, Tag, WordsLeft, [Tag-Constitutent | RestConstituents]) :-
	match_constituent_boundary([F | ReversedPrevious], R, end, Tag),
	!,
	NextReversedPrevious = [F | ReversedPrevious],
	reverse([F | ReversedCurrent], Constitutent),
	extract_surface_constitutents1(R, NextReversedPrevious, [], top_level, WordsLeft, RestConstituents).
% Not start of constituent, not at top-level
extract_surface_constitutents1([F | R], ReversedPrevious, ReversedCurrent, Tag, WordsLeft, Constituents) :-
	Tag \== top_level,
	!,
	NextReversedPrevious = [F | ReversedPrevious],
	NextCurrent = [F | ReversedCurrent],
	extract_surface_constitutents1(R, NextReversedPrevious, NextCurrent, Tag, WordsLeft, Constituents).
% Other case, error
extract_surface_constitutents1(Words, ReversedPrevious, ReversedCurrent, Tag, WordsLeft, Constituents) :-
	format2error('~N*** Error: bad call: ~w',
		     [extract_surface_constitutents1(Words, ReversedPrevious, ReversedCurrent, Tag, WordsLeft, Constituents)]),
	fail.

%----------------------------------------------------------------------

match_constituent_boundary(ReversedPreviousWords, RemainingWords, StartOrEnd, Tag) :-
	add_start_element_to_list(ReversedPreviousWords, ReversedPreviousWords1),
	add_start_element_to_list(RemainingWords, RemainingWords1),
	user:compiled_surface_constituent_boundary(ReversedPreviousPattern, RemainingPattern, StartOrEnd, Tag),
	hand_coded_surface_pattern_match_in_string(ReversedPreviousPattern, ReversedPreviousWords1, _Matched1),
	hand_coded_surface_pattern_match_in_string(RemainingPattern, RemainingWords1, _Matched2),	
	!.

%----------------------------------------------------------------------

surface_patterns_parse1_on_surface_constituents([], []) :-
	!.
surface_patterns_parse1_on_surface_constituents([Tag-Words | R], [[Tag, Representation] | R1]) :-
	surface_patterns_parse1(Words, Representation0),
	postprocess_after_surface_parse(Representation0, Representation, Tag),
	!,
	surface_patterns_parse1_on_surface_constituents(R, R1).
surface_patterns_parse1_on_surface_constituents(Constituents, Representations) :-
	format2error('~N*** Error: bad call: ~w', [surface_patterns_parse1_on_surface_constituents(Constituents, Representations)]),
	fail.

%----------------------------------------------------------------------

surface_patterns_parse1(Words, Representation) :-
	current_feature_extraction_spec(FeatureExtractionSpec),
	decode_words_and_nl_value([1-[Words, null_lf]], [1-FeatureExtractionSpec], Representation, _DecoderPairs),
	!.

% Downward compatibility
postprocess_after_surface_parse(Representation0, Representation0, _Tag) :-
	\+ current_predicate(user:surface_postprocess/3),
	\+ current_predicate(user:surface_postprocess/2),
	!.
% Downward compatibility
postprocess_after_surface_parse(Representation0, Representation, _Tag) :-
	\+ current_predicate(user:surface_postprocess/3),
	current_predicate(user:surface_postprocess/2),
	user:surface_postprocess(Representation0, Representation),
	!.
% Should use this clause
postprocess_after_surface_parse(Representation0, Representation, Tag) :-
	user:surface_postprocess(Representation0, Representation, Tag),
	!.

%----------------------------------------------------------------------

add_start_element_to_list(Words, ['*start*' | Words]).

/*
add_start_and_end_tags_to_words(Words, Words1) :-
	append(['*start*' | Words], ['*end*'], Words1),
	!.

remove_start_and_end_tags_from_words([], []) :-
	!.
remove_start_and_end_tags_from_words([F | R], R1) :-
	member(F, ['*start*', '*end*']),
	!,
	remove_start_and_end_tags_from_words(R, R1).
remove_start_and_end_tags_from_words([F | R], [F | R1]) :-
	!,
	remove_start_and_end_tags_from_words(R, R1).

remove_start_and_end_tags_from_constitutents([], []) :-
	!.
remove_start_and_end_tags_from_constitutents([Tag-F | R], [Tag-F1 | R1]) :-
	remove_start_and_end_tags_from_words(F, F1),
	!,
	remove_start_and_end_tags_from_constitutents(R, R1).
*/
