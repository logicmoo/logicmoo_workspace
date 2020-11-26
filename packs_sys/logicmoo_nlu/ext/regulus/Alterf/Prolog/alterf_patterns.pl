% alterf_patterns.pl

%--------------------------------------------------------------------------------------------

:- module(alterf_patterns,
	  [make_blank_alterf_patterns/2,
	   check_alterf_patterns/3,
	   check_alterf_surface_patterns/3,
	   check_alterf_patterns/4,
	   check_alterf_surface_patterns/4,
	   add_alterf_tags/3,
	   filter_alterf_patterns/3,
	   filter_alterf_surface_patterns/3]
      ). 

%--------------------------------------------------------------------------------------------

:- use_module('$REGULUS/Alterf/Prolog/generic_numbers').
:- use_module('$REGULUS/Alterf/Prolog/parse_annotated_wavfiles').
:- use_module('$REGULUS/Alterf/Prolog/batchrec').
:- use_module('$REGULUS/Alterf/Prolog/extract_feats').
:- use_module('$REGULUS/Alterf/Prolog/classifier_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

%:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

%--------------------------------------------------------------------------------------------

make_blank_alterf_patterns(EBLTreebankFile, BlankPatterns) :-
	absolute_file_name(EBLTreebankFile, AbsEBLTreebankFile),
	absolute_file_name(BlankPatterns, AbsBlankPatterns),

	format('~NCreating blank alterf_patterns file from EBL treebank file ~w~n', [AbsEBLTreebankFile]),
	load_ebl_treebank_file(AbsEBLTreebankFile),
	findall([Tag, WordList]-LF, good_example_and_tag(Tag, WordList, LF), Pairs1),
	remove_duplicates(Pairs1, Pairs2),
	keysort(Pairs2, Pairs3),
	
	open(AbsBlankPatterns, write, S),
	make_blank_alterf_patterns_stream(Pairs3, S, '*no_tag*'),
	close(S),
	format('~NCreated blank alterf_patterns file ~w~n', [AbsBlankPatterns]).

check_alterf_patterns(AnnotatedWavfiles, RecParams, TraceFile) :-
	check_alterf_patterns(AnnotatedWavfiles, RecParams, lf, TraceFile).
check_alterf_surface_patterns(AnnotatedWavfiles, RecParams, TraceFile) :-
	check_alterf_patterns(AnnotatedWavfiles, RecParams, surface, TraceFile).

filter_alterf_patterns(AnnotatedWavfiles, RecParams, OutFile) :-
	filter_alterf_patterns(AnnotatedWavfiles, RecParams, lf, OutFile).
filter_alterf_surface_patterns(AnnotatedWavfiles, RecParams, OutFile) :-
	filter_alterf_patterns(AnnotatedWavfiles, RecParams, surface, OutFile).

add_alterf_tags(AnnotatedWavfiles, RecParams, OutFile) :-
	add_alterf_tags(AnnotatedWavfiles, RecParams, lf_and_surface, OutFile).

%--------------------------------------------------------------------------------------------

make_blank_alterf_patterns_stream([], _S, _PreviousTag).
make_blank_alterf_patterns_stream([F | R], S, PreviousTag) :-
	make_blank_alterf_patterns_stream_item(F, S, PreviousTag-NextTag),
	!,
	make_blank_alterf_patterns_stream(R, S, NextTag).
make_blank_alterf_patterns_stream(Items, S, PreviousTag) :-
	format('~NError: bad call: ~w~n', [make_blank_alterf_patterns_stream(Items, S, PreviousTag)]),
	fail.

make_blank_alterf_patterns_stream_item([Tag, WordList]-LF, S, PreviousTag-NextTag) :-
	join_with_spaces(WordList, WordsAtom),
	(   Tag = PreviousTag ->
	    NextTag = PreviousTag ;
	    format(S, '~N~n% ALTERF PATTERNS FOR "~w"~n~n', [Tag]),
	    NextTag = Tag
	),
	format(S, '~N%~q.~n', [alterf_pattern(LF, Tag, WordsAtom)]),
	!.
make_blank_alterf_patterns_stream_item(F, S, Tags) :-
	format('~NError: bad call: ~w~n', [make_blank_alterf_patterns_stream_item(F, S, Tags)]),
	fail.
	
%--------------------------------------------------------------------------------------------

good_example_and_tag(GenericTag, WordList, LF) :-
	example(_GrammarAtom, _Tree, LF, WordList, TagList),
	LF \== 'NO_LF',
	member(Tag, TagList),
	make_numbers_in_feat_generic(Tag, GenericTag, _Substitution).

%--------------------------------------------------------------------------------------------

filter_alterf_patterns(AnnotatedWavfiles, RecParams, LFOrSurface, OutFile) :-
	absolute_file_name(AnnotatedWavfiles, AbsAnnotatedWavfiles),
	absolute_file_name(OutFile, AbsOutFile),

	format('~N~n*** FILTERING LOADED ALTERF PATTERNS AGAINST CORPUS FILE ~w ***~n~n', [AbsAnnotatedWavfiles]),

	make_check_patterns_tmp_files(TmpRecResults, TmpFeatVectors, TmpWavfiles, TmpTranscriptions, TmpSents, TmpAnnotatedWavfiles),
	parse_annotated_wavfiles(AbsAnnotatedWavfiles, TmpWavfiles, TmpTranscriptions, TmpSents, TmpAnnotatedWavfiles),
	do_text_batchrec(TmpTranscriptions, RecParams, TmpRecResults),
	(   LFOrSurface = lf ->
	    FeatureExtractionSpec = [hand_coded_pattern_rules, post_processed_lf, lf_postproc_pred=riacs_postproc_lf] ;
	    FeatureExtractionSpec = [hand_coded_surface_pattern_rules]
	),
	rec_results_to_feature_vectors([1-TmpRecResults], [1-FeatureExtractionSpec], TmpFeatVectors),
	load_annotated_wavfiles_file(TmpAnnotatedWavfiles),

	filter_patterns1(TmpFeatVectors, LFOrSurface, FilteredPatterns),
	length(FilteredPatterns, NFilteredPatterns),
	sort_filtered_patterns(FilteredPatterns, SortedFilteredPatterns),
	
	list_to_prolog_file(SortedFilteredPatterns, AbsOutFile),

	count_loaded_patterns_of_type(LFOrSurface, NOriginalPatterns),
	format('~N~nDone: ~d/~d patterns left after filtering~n', [NFilteredPatterns, NOriginalPatterns]),
	format('~N~nUnused patterns:~n', []),
	list_unused_patterns(LFOrSurface, SortedFilteredPatterns),
	format('~N~nResults written to ~w~n~n', [AbsOutFile]).

sort_filtered_patterns(FilteredPatterns, SortedFilteredPatterns) :-
	findall(
	Atom-FilteredPattern,
	( 
	    member(FilteredPattern, FilteredPatterns),
	    arg(2, FilteredPattern, Atom)
	),
	KeyedFilteredPatterns
    ),
	keysort(KeyedFilteredPatterns, SortedKeyedFilteredPatterns),
	unkey_list(SortedKeyedFilteredPatterns, SortedFilteredPatterns).

count_loaded_patterns_of_type(lf, N) :-
	current_predicate(user:alterf_pattern/3),
	findall(x, user:alterf_pattern(_, _, _), Xs),
	length(Xs, N),
	!.
count_loaded_patterns_of_type(surface, N) :-
	current_predicate(user:alterf_surface_pattern/3),
	findall(x, user:alterf_surface_pattern(_, _, _), Xs),
	length(Xs, N),
	!.
count_loaded_patterns_of_type(_, 0).

list_unused_patterns(lf, UsedPatterns) :-
	current_predicate(user:alterf_pattern/3),
	user:alterf_pattern(A, B, C),
	\+ member(alterf_pattern(A, B, C), UsedPatterns),
	make_ground(alterf_pattern(A, B, C)),
	format('~N~q~n', [alterf_pattern(A, B, C)]),
	fail.
list_unused_patterns(surface, UsedPatterns) :-
	current_predicate(user:alterf_surface_pattern/3),
	user:alterf_surface_pattern(A, B, C),
	\+ member(alterf_surface_pattern(A, B, C), UsedPatterns),
	format('~N~q~n', [alterf_surface_pattern(A, B, C)]),
	fail.
list_unused_patterns(_, _).
	
%--------------------------------------------------------------------------------------------

filter_patterns1(FeatVectors, LFOrSurface, FilteredPatterns) :-
	open(FeatVectors, read, S),
	filter_patterns2(S, LFOrSurface, []-FilteredPatterns),
	close(S).

filter_patterns2(S, LFOrSurface, PatternsIn-PatternsOut) :-
	read(S, T),
	filter_patterns3(T, S, LFOrSurface, PatternsIn-PatternsOut).

filter_patterns3(end_of_file, _S, _LFOrSurface, PatternsIn-PatternsIn) :-
	!.
filter_patterns3(T, S, LFOrSurface, PatternsIn-PatternsOut) :-
	filter_patterns_for_single_item(T, LFOrSurface, PatternsIn-PatternsNext),
	!,
	filter_patterns2(S, LFOrSurface, PatternsNext-PatternsOut).

filter_patterns_for_single_item(FeatsTerm, LFOrSurface, PatternsIn-PatternsOut) :-
	(   FeatsTerm = wavfile_with_words_confidence_and_feats(Wavfile, [[_RecognisedWords, _Confidence, Feats]]) ->
	    true ;
	    format('~N*** Error: unknown term in feat vectors file ~q~n', [FeatsTerm]),
	    fail
	),
	
	(   wavfile_and_annotations(Wavfile, _GivenSemanticAtoms, _Transcription) ->
	    filter_patterns_for_single_item1(Feats, LFOrSurface, PatternsIn-PatternsOut) ;
	    format('~N*** Warning: no wavfile_and_annotations record found for wavfile ~w referred to in features file.~n', [Wavfile]),
	    fail
	).

filter_patterns_for_single_item1(Feats, surface, PatternsIn-PatternsOut) :-
	findall(NewPattern, ( member(hand_coded_surface_pattern(NewPattern), Feats), make_ground(NewPattern) ), NewPatterns),
	list_to_ord_set(NewPatterns, NewPatternsOS),
	ord_union(PatternsIn, NewPatternsOS, PatternsOut),
	!.
filter_patterns_for_single_item1(Feats, lf, PatternsIn-PatternsOut) :-
	findall(NewPattern, ( member(hand_coded_pattern(NewPattern), Feats), make_ground(NewPattern) ), NewPatterns),
	list_to_ord_set(NewPatterns, NewPatternsOS),
	ord_union(PatternsIn, NewPatternsOS, PatternsOut),
	!.

%--------------------------------------------------------------------------------------------

add_alterf_tags(AnnotatedWavfiles, RecParams, LFOrSurface, OutFile) :-
	absolute_file_name(AnnotatedWavfiles, AbsAnnotatedWavfiles),
	absolute_file_name(OutFile, AbsOutFile),

	format('~NAdding tags to file ~w~n', [AbsAnnotatedWavfiles]),

	make_check_patterns_tmp_files(TmpRecResults, TmpFeatVectors, TmpWavfiles, TmpTranscriptions, TmpSents, TmpAnnotatedWavfiles),
	parse_annotated_wavfiles(AbsAnnotatedWavfiles, TmpWavfiles, TmpTranscriptions, TmpSents, TmpAnnotatedWavfiles),
	do_text_batchrec(TmpTranscriptions, RecParams, TmpRecResults),
	(   LFOrSurface = lf ->
	    FeatureExtractionSpec = [hand_coded_patterns, post_processed_lf, lf_postproc_pred=riacs_postproc_lf] ;
	    LFOrSurface = surface ->
	    FeatureExtractionSpec = [hand_coded_surface_patterns] ;
	    LFOrSurface = lf_and_surface ->
	    FeatureExtractionSpec = [hand_coded_patterns, post_processed_lf, lf_postproc_pred=riacs_postproc_lf,
				     hand_coded_surface_patterns] ;
	    format('~N*** Error: bad value of arg 3 in add_alterf_tags: ~w~n', [LFOrSurface]),
	    fail
	),
	rec_results_to_feature_vectors([1-TmpRecResults], [1-FeatureExtractionSpec], TmpFeatVectors),

	load_annotated_wavfiles_file(TmpAnnotatedWavfiles),

	open(AbsOutFile, write, S),
	add_alterf_tags1(TmpFeatVectors, S),
	close(S),

	format('~NResults written to ~w~n~n', [AbsOutFile]).

%--------------------------------------------------------------------------------------------

add_alterf_tags1(FeatVectors, SOut) :-
	open(FeatVectors, read, S),
	add_alterf_tags2(S, SOut),
	close(S).

add_alterf_tags2(S, SOut) :-
	read(S, T),
	add_alterf_tags3(T, S, SOut).

add_alterf_tags3(end_of_file, _S, _SOut) :-
	!.
add_alterf_tags3(T, S, SOut) :-
	add_alterf_tags_for_single_item(T, SOut),
	!,
	add_alterf_tags2(S, SOut).

add_alterf_tags_for_single_item(FeatsTerm, SOut) :-
	(   FeatsTerm = wavfile_with_words_confidence_and_feats(Wavfile, [[_RecognisedWords, _Confidence, Feats]]) ->
	    true ;
	    format('~N*** Error: unknown term in feat vectors file ~q~n', [FeatsTerm]),
	    fail
	),

	(   wavfile_and_annotations(Wavfile, _GivenSemanticAtoms, Transcription) ->
	    add_alterf_tags_for_single_item1(Wavfile, Transcription, Feats, SOut) ;
	    format('~N*** Warning: no wavfile_and_annotations record found for wavfile ~w referred to in features file.~n', [Wavfile])
	).

add_alterf_tags_for_single_item1(Wavfile, Transcription, Feats, SOut) :-
	findall(Atom, member(hand_coded_surface_pattern_match(Atom), Feats), SurfaceAtoms),
	findall(Atom, member(hand_coded_pattern_match(Atom), Feats), LFAtoms),
	append(SurfaceAtoms, LFAtoms, Atoms),
	
	sort(SurfaceAtoms, SortedSurfaceAtoms),
	sort(LFAtoms, SortedLFAtoms),
	sort(Atoms, SortedAtoms),
	
	coerce_list_elements_to_atoms_if_necessary(SortedAtoms, SortedAtoms1),
	(   SortedAtoms1 = [] ->
	    SortedAtomsToken = ignore ;
	    join_with_spaces(SortedAtoms1, SortedAtomsToken)
	),
	join_with_spaces(Transcription, TranscriptionToken),
	(   SortedSurfaceAtoms = SortedLFAtoms ->
	    Annotation = '' ;
	    
	    \+ member(post_processed_lf(_LF), Feats) ->
	    Annotation = '# LF AND SURFACE ATOMS DIFFER + OUT OF COVERAGE' ;
	
	    Annotation = '# LF AND SURFACE ATOMS DIFFER' 
	),
	format(SOut, '~N~w | ~w | ~w ~w~n', [Wavfile, SortedAtomsToken, TranscriptionToken, Annotation]),
	!.
add_alterf_tags_for_single_item1(Wavfile, Transcription, Feats, SOut) :-
	format('~N*** Error: bad call: ~q~n', [add_alterf_tags_for_single_item1(Wavfile, Transcription, Feats, SOut)]),
	fail.

%--------------------------------------------------------------------------------------------

check_alterf_patterns(AnnotatedWavfiles, RecParams, LFOrSurface, TraceFile) :-
	absolute_file_name(AnnotatedWavfiles, AbsAnnotatedWavfiles),
	absolute_file_name(TraceFile, AbsTraceFile),

	format('~NChecking loaded Alterf patterns against corpus file ~w~n', [AbsAnnotatedWavfiles]),

	make_check_patterns_tmp_files(TmpRecResults, TmpFeatVectors, TmpWavfiles, TmpTranscriptions, TmpSents, TmpAnnotatedWavfiles),
	parse_annotated_wavfiles(AbsAnnotatedWavfiles, TmpWavfiles, TmpTranscriptions, TmpSents, TmpAnnotatedWavfiles),
	do_text_batchrec(TmpTranscriptions, RecParams, TmpRecResults),
	(   LFOrSurface = lf ->
	    FeatureExtractionSpec = [hand_coded_patterns, post_processed_lf, lf_postproc_pred=riacs_postproc_lf] ;
	    FeatureExtractionSpec = [hand_coded_surface_patterns]
	),
	rec_results_to_feature_vectors([1-TmpRecResults], [1-FeatureExtractionSpec], TmpFeatVectors),
	load_annotated_wavfiles_file(TmpAnnotatedWavfiles),

	open(AbsTraceFile, write, S),
	check_patterns1(TmpFeatVectors, LFOrSurface, NGood, NBad, S),
	close(S),

	format('~N~nDone: ~d examples OK, ~d examples not OK~n', [NGood, NBad]),
	format('~NResults written to ~w~n~n', [AbsTraceFile]).
	
%--------------------------------------------------------------------------------------------

check_patterns1(FeatVectors, LFOrSurface, NGood, NBad, SOut) :-
	open(FeatVectors, read, S),
	check_patterns2(S, LFOrSurface, 0-NGood, 0-NBad, SOut),
	close(S).

check_patterns2(S, LFOrSurface, NGoodIn-NGoodOut, NBadIn-NBadOut, SOut) :-
	read(S, T),
	check_patterns3(T, S, LFOrSurface, NGoodIn-NGoodOut, NBadIn-NBadOut, SOut).

check_patterns3(end_of_file, _S, _LFOrSurface, NGoodIn-NGoodIn, NBadIn-NBadIn, _SOut) :-
	!.
check_patterns3(T, S, LFOrSurface, NGoodIn-NGoodOut, NBadIn-NBadOut, SOut) :-
	check_patterns_for_single_item(T, LFOrSurface, NGoodIn-NGoodNext, NBadIn-NBadNext, SOut),
	!,
	check_patterns2(S, LFOrSurface, NGoodNext-NGoodOut, NBadNext-NBadOut, SOut).

check_patterns_for_single_item(FeatsTerm, LFOrSurface, NGoodIn-NGoodOut, NBadIn-NBadOut, SOut) :-
	(   FeatsTerm = wavfile_with_words_confidence_and_feats(Wavfile, [[_RecognisedWords, _Confidence, Feats]]) ->
	    true ;
	    format('~N*** Error: unknown term in feat vectors file ~q~n', [FeatsTerm]),
	    fail
	),
	
	(   wavfile_and_annotations(Wavfile, GivenSemanticAtoms, Transcription) ->
	    check_patterns_for_single_item1(Wavfile, Transcription, GivenSemanticAtoms, Feats, LFOrSurface, NGoodIn-NGoodOut, NBadIn-NBadOut, SOut) ;
	    format('~N*** Warning: no wavfile_and_annotations record found for wavfile ~w referred to in features file.~n', [Wavfile]),
	    NGoodIn = NGoodOut,
	    NBadIn = NBadOut
	).

check_patterns_for_single_item1(Wavfile, Transcription, GivenSemanticAtoms, Feats, surface, NGoodIn-NGoodOut, NBadIn-NBadOut, SOut) :-
	findall(RuleBasedSemanticAtom, member(hand_coded_surface_pattern_match(RuleBasedSemanticAtom), Feats), RuleBasedSemanticAtoms),
	check_patterns_for_single_item2(Wavfile, Transcription, 'LF irrelevant', RuleBasedSemanticAtoms, GivenSemanticAtoms, NGoodIn-NGoodOut, NBadIn-NBadOut, SOut),
	!.
check_patterns_for_single_item1(_Wavfile, _Transcription, GivenSemanticAtoms, Feats, lf, NGoodIn-NGoodIn, NBadIn-NBadIn, _SOut) :-
	(   \+ member(post_processed_lf(_LF), Feats) ;
	    member(post_processed_lf(no_lf), Feats) ;
	    GivenSemanticAtoms = [ignore]
	),
	!.  
check_patterns_for_single_item1(Wavfile, Transcription, GivenSemanticAtoms, Feats, lf, NGoodIn-NGoodOut, NBadIn-NBadOut, SOut) :-
	member(post_processed_lf(LF), Feats),
	findall(RuleBasedSemanticAtom, member(hand_coded_pattern_match(RuleBasedSemanticAtom), Feats), RuleBasedSemanticAtoms),
	check_patterns_for_single_item2(Wavfile, Transcription, LF, RuleBasedSemanticAtoms, GivenSemanticAtoms, NGoodIn-NGoodOut, NBadIn-NBadOut, SOut),
	!.

check_patterns_for_single_item2(Wavfile, Transcription, LF, RuleBasedSemanticAtoms, GivenSemanticAtoms, NGoodIn-NGoodOut, NBadIn-NBadOut, SOut) :-
	findall(Atom1,
		(   member(Atom1, GivenSemanticAtoms),
		    Atom1 \== ignore,
		    \+ member(Atom1, RuleBasedSemanticAtoms)
		),
		NotFoundAtoms),
	findall(Atom2,
		(   member(Atom2, RuleBasedSemanticAtoms),
		    \+ member(Atom2, GivenSemanticAtoms)
		),
		BadAtoms),
	pairs_of_atoms_in_list_inconsistent_according_to_target_model(RuleBasedSemanticAtoms, InconsistentAtomPairs),
	(   ( NotFoundAtoms = [], BadAtoms = [], InconsistentAtomPairs = [] ) ->
	    NGoodOut is NGoodIn + 1,
	    NBadOut is NBadIn ;
	    report_bad_patterns_for_single_item(Wavfile, Transcription, LF, NotFoundAtoms, BadAtoms, InconsistentAtomPairs, SOut),
	    NBadOut is NBadIn + 1,
	    NGoodOut is NGoodIn
	).

report_bad_patterns_for_single_item(Wavfile, Transcription, LF, NotFoundAtoms, BadAtoms, InconsistentAtomPairs, SOut) :-
	join_with_spaces(Transcription, TranscriptionAtom),
	format(SOut, '~N~n----------------------------------------------------', []),
	format(SOut, '~N~nID: ~w', [Wavfile]),
	format(SOut, '~NUtterance: ~w', [TranscriptionAtom]),
	format(SOut, '~N~nLF: ~q~n~n', [LF]),
	report_not_found_atoms(SOut, NotFoundAtoms),
	report_bad_atoms(SOut, BadAtoms),
	(   ( NotFoundAtoms = [], BadAtoms = [] ) ->
	    report_inconsistent_atom_pairs(SOut, InconsistentAtomPairs) ;
	    true
	).

report_not_found_atoms(_SOut, []).
report_not_found_atoms(SOut, [F | R]) :-
	format(SOut, '~NMissed: ~w~n', [F]),
	report_not_found_atoms(SOut, R).

report_bad_atoms(_SOut, []).
report_bad_atoms(SOut, [F | R]) :-
	format(SOut, '~NIncorrect match: ~w~n', [F]),
	report_bad_atoms(SOut, R).

report_inconsistent_atom_pairs(_SOut, []).
report_inconsistent_atom_pairs(SOut, [F | R]) :-
	format(SOut, '~NInconsistent atom pair: ~w~n', [F]),
	report_inconsistent_atom_pairs(SOut, R).

%--------------------------------------------------------------------------------------------

pairs_of_atoms_in_list_inconsistent_according_to_target_model(SemanticAtoms, InconsistentAtomPairs) :-
	findall([Atom1, Atom2], ( member(Atom1, SemanticAtoms), member(Atom2, SemanticAtoms), inconsistent_atom_pair(Atom1, Atom2) ), InconsistentAtomPairs).

inconsistent_atom_pair(Atom1, Atom2) :-
	make_numbers_in_feat_generic(Atom1, GenAtom1, _Subst1),
	make_numbers_in_feat_generic(Atom2, GenAtom2, _Subst2),
	current_predicate(user:target_atom_excludes/2),
	user:target_atom_excludes(GenAtom1, ExcludedAtoms),
	member(GenAtom2, ExcludedAtoms).

%--------------------------------------------------------------------------------------------

make_check_patterns_tmp_files(TmpRecResults, TmpFeatVectors, TmpWavfiles, TmpTranscriptions, TmpSents, TmpAnnotatedWavfiles) :-
	make_tmp_file('text_rec_results.pl', TmpRecResults),
	make_tmp_file(check_patterns_feat_vectors, TmpFeatVectors),
	make_tmp_file(wavfiles, TmpWavfiles),
	make_tmp_file(transcriptions, TmpTranscriptions),
	make_tmp_file('sents.pl', TmpSents),
	make_tmp_file('annotated_wavfiles.pl', TmpAnnotatedWavfiles).

%--------------------------------------------------------------------------------------------

load_ebl_treebank_file(EBLTreebankFile) :-
	(   current_predicate(example, example(_, _, _, _, _)) ->
	    abolish(example/5) ;
	    true
	),
	compile(EBLTreebankFile).

%---------------------------------------------------------------

load_annotated_wavfiles_file(AnnotatedWavfiles) :-
	(   current_predicate(wavfile_and_annotations, wavfile_and_annotations(_, _, _)) ->
	    abolish(wavfile_and_annotations/3) ;
	    true
	),
	compile(AnnotatedWavfiles).

%--------------------------------------------------------------------------------------------

 
