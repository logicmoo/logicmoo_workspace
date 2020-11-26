 
%--------------------------------------------------------------------------------------------
 
:- module(batch_decoder,
	  [batch_decode/6,
	   batch_decode/7,
	   compare_trace_files/4,
	   print_cumulative_batch_decoder_results/2,
	   print_cumulative_batch_decoder_results/3]
      ).
 
%--------------------------------------------------------------------------------------------

:- use_module('$REGULUS/Alterf/Prolog/parse_annotated_wavfiles').
:- use_module('$REGULUS/Alterf/Prolog/classifier_decoder').
:- use_module('$REGULUS/Alterf/Prolog/batchrec').
:- use_module('$REGULUS/Alterf/Prolog/extract_feats').
:- use_module('$REGULUS/Alterf/Prolog/classifier_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(assoc)).

%--------------------------------------------------------------------------------------------

/*

batch_decode(+AnnotatedWavfiles, +RecParamsAlist, +FeatureExtractionSpecAlist, +OutFile, +RecOrNoRec, +Threshold, -Summary)

Sample call:

batch_decode(
	corpora(annotated_wavfiles_test1),
	[1-[package=ebllm_gemini(recogniser_sem), grammar='.MAIN', 'rec.Pruning'=1200], 
         2-[package=slm(package), grammar='.TOP', 'rec.Pruning'=1200]], 
	[1-[hand_coded_patterns, sem_triples, lf_postproc_pred=riacs_postproc_lf], 
	 2-[unigrams, bigrams, trigrams]],
	alterf_generated_files('batch_decode_results_slm.txt'),
	rec([1-alterf_generated_files('batchrec_results_test1.pl'), 2-alterf_generated_files('batchrec_results_test2.pl')]),
	55,
	Results

Top-level call for batch testing. Most of the parameters are the same as those for train_classifier/6 in classifier_trainer.pl.

1. AnnotatedWavfiles. 

File of annotated wavfile data. Same format as AnnotatedWavfiles in train_classifier/6.

2. RecParamsAlist

How to do recognition. Same format as RecParamsAlist in train_classifier/6.

3. FeatureExtractionSpecAlist

How to extract features. Same format as FeatureExtractionSpecAlist in train_classifier/6.

4. OutFile

Results. An example of a result file is in home/speech/rialist/checklist/GeneratedFiles/batch_decode_results.txt.
Note that the file is Prolog-readable, with human-readable text as Prolog comments.

5. RecOrNoRec

Where to get data from. Same format as RecOrNoRec in train_classifier/6.

6. Threshold

Number between 0 and 100. Recognition results under Threshold are rejected.

7. Summary

Prolog assoc structure summarising result of test run. Keys are the following:

    good_reject.              Tagged as "ignore" and rejected.
    misfire.                  Tagged as "ignore" but accepted.
    bad_reject_acoustic.      Not tagged as "ignore" but rejected due to rec score being too low.
    bad_reject_semantic.      Not tagged as "ignore" but rejected due Alterf not finding any atoms.
    good_accept.              Not tagged as "ignore", accepted with correct semantic atoms.
    bad_accept.               Not tagged as "ignore", accepted with incorrect semantic atoms.

%--------------------------------------------------------------------------------------------

compare_trace_files(+File1, +File2, +OutFile, -Summary)

File1 and File2 are batchrec output files for the same input data.

OutFile is a file describing the differences.

Summary give a good/bad count.

*/

batch_decode(AnnotatedWavfiles, RecParamsAlist, FeatureExtractionSpecAlist, OutFile, RecOrNoRec, Threshold) :-
	batch_decode(AnnotatedWavfiles, RecParamsAlist, FeatureExtractionSpecAlist, OutFile, RecOrNoRec, Threshold, _Summary).

batch_decode(AnnotatedWavfiles, RecParamsAlist, FeatureExtractionSpecAlist, OutFile, RecOrNoRec, Threshold, Summary) :-

	check_alists_are_compatible([RecParamsAlist, FeatureExtractionSpecAlist]),

	absolute_file_name(AnnotatedWavfiles, AbsAnnotatedWavfiles),
	absolute_file_name(OutFile, AbsOutFile),

	format('~N~nDoing batch decoding from file: ~w~n', [AbsAnnotatedWavfiles]),

	make_batch_decode_tmp_files(TmpWavfiles, TmpTranscriptions, TmpSents, TmpAnnotatedWavfiles),
	parse_annotated_wavfiles(AnnotatedWavfiles, TmpWavfiles, TmpTranscriptions, TmpSents, TmpAnnotatedWavfiles),

	(   RecOrNoRec = rec(RecResultsAlist) ->
	    check_alists_are_compatible([RecParamsAlist, RecResultsAlist]),
	    do_batchrec_multiple(TmpWavfiles, TmpTranscriptions, RecParamsAlist, RecResultsAlist) ;

	    RecOrNoRec = text ->
	    do_text_batchrec_multiple(TmpTranscriptions, RecParamsAlist, RecResultsAlist) ;

	    RecOrNoRec = saved_rec(RecResultsAlist) ->
	    check_alists_are_compatible([RecParamsAlist, RecResultsAlist]),
	    check_saved_rec_results_alist(RecResultsAlist) ;

	    format('~N*** Error: bad value "~w" for last arg to batch_decode/6. Must be "rec(<RecResultAlist>)", "text" or "saved_rec(<RecResultAlist>)".~n', [RecOrNoRec]),
	    fail
	),
	
	batch_decode1(RecResultsAlist, TmpAnnotatedWavfiles, FeatureExtractionSpecAlist, Threshold, AbsOutFile, Summary),
	format('~N~nResults written to file ~w~n', [AbsOutFile]).

batch_decode1(RecResultsAlist, TmpAnnotatedWavfiles, FeatureExtractionSpecAlist, Threshold, OutFile, FinalAssoc) :-
	load_annotated_wavfiles_file(TmpAnnotatedWavfiles),	

	open_alist(RecResultsAlist, read, SResultsAlist),
	open(OutFile, write, SOut),
	
	empty_assoc(InitAssoc),
	batch_decode_rec_results_stream(SResultsAlist, FeatureExtractionSpecAlist, Threshold, SOut, InitAssoc-FinalAssoc),

	close_alist(SResultsAlist),
	close(SOut).

batch_decode_rec_results_stream(SResultsAlist, FeatureExtractionSpecAlist, Threshold, SOut, AssocIn-AssocOut) :-
	read_alist(SResultsAlist, ResultAlist),
	batch_decode_rec_results_stream1(ResultAlist, SResultsAlist, FeatureExtractionSpecAlist, Threshold, SOut, AssocIn-AssocOut).

batch_decode_rec_results_stream1(ResultAlist, _SResultsAlist, _FeatureExtractionSpecAlist, Threshold, _SOut, AssocIn-AssocIn) :-
	is_end_of_file_alist(ResultAlist),

	format(user, '~N~nSummary (threshold = ~d):~n', [Threshold]),
	print_cumulative_batch_decoder_results(user, AssocIn),
	!.
batch_decode_rec_results_stream1(ResultAlist, SResultsAlist, FeatureExtractionSpecAlist, Threshold, SOut, AssocIn-AssocOut) :-
	(   batch_decode_rec_result(ResultAlist, FeatureExtractionSpecAlist, Threshold, SOut, AssocIn-AssocNext) ->
	    true ;
	    AssocIn = AssocNext
	),
	!,
	batch_decode_rec_results_stream(SResultsAlist, FeatureExtractionSpecAlist, Threshold, SOut, AssocNext-AssocOut).

%---------------------------------------------------------------

make_batch_decode_tmp_files(TmpWavfiles, TmpTranscriptions, TmpSents, TmpAnnotatedWavfiles) :-
	make_tmp_file(wavfiles_batch, TmpWavfiles),
	make_tmp_file(transcriptions_batch, TmpTranscriptions),
	make_tmp_file(sents_batch, TmpSents),
	make_tmp_file('annotated_wavfiles_batch.pl', TmpAnnotatedWavfiles).

%---------------------------------------------------------------

batch_decode_rec_result(ResultAlist, _FeatureExtractionSpecAlist, _Threshold, _SOut, AssocIn-AssocIn) :-
	member(_Key-Result, ResultAlist),
	Result = batchrec_item(Pairs),
	member(wavfile=Id, Pairs),
	\+ wavfile_and_annotations(Id, _RefTargetAtoms, _RefWordList),
	format('~N*** Warning: skipping record: no wavfile_and_annotations data for batchrec ID ~w~n', [Id]),
	!.
batch_decode_rec_result(ResultAlist, FeatureExtractionSpecAlist, Threshold, SOut, AssocIn-AssocOut) :-
	result_alist_to_id_and_words_and_nl_value_alist(ResultAlist, Id, WordsAndNLValueAlist),
	wavfile_and_annotations(Id, RefTargetAtoms, RefWordList),
	decode_words_and_nl_value(WordsAndNLValueAlist, FeatureExtractionSpecAlist, TargetAtoms, PairsUsed),
	combined_confidence_value_in_result_alist(ResultAlist, CombinedConfidence),
	all_words_in_result_alist(ResultAlist, Words),
	all_relevant_words_in_result_alist(ResultAlist, TargetAtoms, RelevantWords),
	AcceptRejectInfo = [confidence=CombinedConfidence,
			    target_atoms=TargetAtoms,
			    threshold=Threshold,
			    words=Words,
			    relevant_words=RelevantWords],
	categorise_atoms_as_in_or_out_of_domain(RefTargetAtoms, InOrOutOfDomain),
	decide_whether_to_reject(AcceptRejectInfo, AcceptOrReject),
	
	(   InOrOutOfDomain = out_of_domain ->
	    inc_assoc_multiple(AssocIn, [out_of_domain], AssocNext) ;
	    inc_assoc_multiple(AssocIn, [in_domain], AssocNext) 
	),

	(   (  InOrOutOfDomain = out_of_domain, AcceptOrReject = reject ) ->
	    Status = 'Correctly rejected',
	    inc_assoc_multiple(AssocNext, [good_reject], AssocOut) ;

	    AcceptOrReject = reject ->
	    Status = 'Incorrectly rejected',
	    inc_assoc_multiple(AssocNext, [bad_reject], AssocOut) ;

	    target_atoms_match(TargetAtoms, RefTargetAtoms) ->
	    Status = 'Correct',
	    inc_assoc_multiple(AssocNext, [good_accept], AssocOut) ;

	    InOrOutOfDomain = out_of_domain ->
	    Status = 'Misfire',
	    inc_assoc_multiple(AssocNext, [misfire], AssocOut) ;
	    
	    Status = 'Incorrect',
	    inc_assoc_multiple(AssocNext, [bad_accept], AssocOut)
	),

	format(SOut, '~N~n% -----------------------------------------------------------------~n', []),
	format(SOut, '~N% ID: ~w~n~n', [Id]),
	format(SOut, '~N% Reference words: ~w~n', [RefWordList]),
	format(SOut, '~N% Reference target atoms: ~w~n', [RefTargetAtoms]),

	format(SOut, '~N% Recognised words: ~w~n', [Words]),
	format(SOut, '~N% Relevant words: ~w~n', [RelevantWords]),
	format(SOut, '~N% Decoded target atoms: ~w~n~n', [TargetAtoms]),
	format(SOut, '~N% Confidence: ~d~n~n', [CombinedConfidence]),

	present_decoder_pairs_used(SOut, PairsUsed),

	format(SOut, '~N~n% Status: ~w~n', [Status]),
	print_cumulative_batch_decoder_results(SOut, AssocOut),

	format(SOut, '~N~n~q.~n', [result(Id, Status, RefWordList, RefTargetAtoms, Words, TargetAtoms, CombinedConfidence, PairsUsed)]),
	!.
batch_decode_rec_result(ResultAlist, FeatureExtractionSpecAlist, Threshold, SOut, Assoc) :-
	format('*** Warning: bad call: ~w~n', [batch_decode_rec_result(ResultAlist, FeatureExtractionSpecAlist, Threshold, SOut, Assoc)]),
	fail.

%---------------------------------------------------------------

categorise_atoms_as_in_or_out_of_domain([], InOrOutOfDomain) :-
	!,
	InOrOutOfDomain = out_of_domain.
categorise_atoms_as_in_or_out_of_domain(TargetAtoms, InOrOutOfDomain) :-
	member(ignore, TargetAtoms),
	!,
	InOrOutOfDomain = out_of_domain.
categorise_atoms_as_in_or_out_of_domain(TargetAtoms, InOrOutOfDomain) :-
	current_predicate(post_process_alterf:post_process_alterf/2),
	\+ post_process_alterf:post_process_alterf(TargetAtoms, _TargetDialogMove),
	!,
	InOrOutOfDomain = out_of_domain.
categorise_atoms_as_in_or_out_of_domain(_TargetAtoms, InOrOutOfDomain) :-
	InOrOutOfDomain = in_domain.

%---------------------------------------------------------------

target_atoms_match(TargetAtoms, RefTargetAtoms) :-
	current_predicate(post_process_alterf:post_process_alterf/2),
	post_process_alterf:post_process_alterf(TargetAtoms, TargetDialogMove),
	post_process_alterf:post_process_alterf(RefTargetAtoms, RefTargetDialogMove),
	!,
	TargetDialogMove = RefTargetDialogMove.
target_atoms_match(TargetAtoms, RefTargetAtoms) :-
	TargetAtoms = RefTargetAtoms.

%---------------------------------------------------------------

decide_whether_to_reject(AcceptRejectInfo, reject) :-
	member(confidence=CombinedConfidence, AcceptRejectInfo),
	member(threshold=Threshold, AcceptRejectInfo),
	CombinedConfidence < Threshold,
	!.
decide_whether_to_reject(AcceptRejectInfo, reject) :-
	member(target_atoms=TargetAtoms, AcceptRejectInfo),
	member(ignore, TargetAtoms),
	!.
decide_whether_to_reject(AcceptRejectInfo, reject) :-
	member(target_atoms=TargetAtoms, AcceptRejectInfo),
	TargetAtoms = [],
	!.
% Not completely clear we want this... the problem is that the reference labelling
% can fail to translate to a dialogue move. But if we say that in that case
% the utterance is really out of domain, things seem to work out OK.
decide_whether_to_reject(AcceptRejectInfo, reject) :-
	current_predicate(post_process_alterf:post_process_alterf/2),
	member(target_atoms=TargetAtoms, AcceptRejectInfo),
	\+ post_process_alterf:post_process_alterf(TargetAtoms, _TargetDialogMove),
	!.
/*
% This idea of Nikos doesn't yet give us any improvement...
decide_whether_to_reject(AcceptRejectInfo, reject) :-
	member(confidence=CombinedConfidence, AcceptRejectInfo),
	member(words=Words, AcceptRejectInfo),
	member(relevant_words=RelevantWords, AcceptRejectInfo),
	low_confidence_relevant_word_in_recognised_words(Words, RelevantWords, CombinedConfidence),
	!.
*/
decide_whether_to_reject(_AcceptRejectInfo, accept) :-
	!.

low_confidence_relevant_word_in_recognised_words(Words, RelevantWords, CombinedConfidence) :-
	member(WordsWithConfidence, Words),
	member(Word/WordConfidence, WordsWithConfidence),
	member(Word, RelevantWords),
	WordConfidence < CombinedConfidence * 0.5,
	!.

%---------------------------------------------------------------

result_alist_to_id_and_words_and_nl_value_alist([], _Id, []) :-
	!.
result_alist_to_id_and_words_and_nl_value_alist([Key-Result | ResultAlist], Id, [Key-[Words, NLVal] | WordsAndNLValueAlist]) :-
	result_to_id_words_and_nl_value(Result, Id, Words, NLVal),
	result_alist_to_id_and_words_and_nl_value_alist(ResultAlist, _OtherId, WordsAndNLValueAlist),
	!.

result_to_id_words_and_nl_value(Result, Id, Words, NLVal) :-
	Result = batchrec_item(Pairs),
	member(wavfile=Id, Pairs),
	member(words=Words, Pairs),
	(   member(nl_value=NLVal, Pairs) ->
	    true ;
	    NLVal = '*no_nl_value*'
	),
	!.

%---------------------------------------------------------------

combined_confidence_value_in_result_alist(ResultAlist, CombinedConfidence) :-
	all_confidence_values_in_result_alist(ResultAlist, ConfidenceScores),
	%max_list(ConfidenceScores, CombinedConfidence),
	min_list(ConfidenceScores, CombinedConfidence),
	!.
combined_confidence_value_in_result_alist(ResultAlist, CombinedConfidence) :-
	format('*** Error: bad call: ~w~n', [combined_confidence_value_in_result_alist(ResultAlist, CombinedConfidence)]),
	fail.

%---------------------------------------------------------------

all_words_in_result_alist([], []).
all_words_in_result_alist([_Key-Result | Results], [Words | RestWords]) :-
	words_in_result(Result, Words),
	!,
	all_words_in_result_alist(Results, RestWords).

words_in_result(batchrec_item(Pairs), WordsWithConfidence) :-
	member(words=Words, Pairs),
	member(word_confidence=WordConfidence, Pairs),
	combine_words_and_word_confidence(Words, WordConfidence, WordsWithConfidence),
	!.
words_in_result(batchrec_item(Pairs), Words) :-
	member(words=Words, Pairs),
	!.

%---------------------------------------------------------------

all_relevant_words_in_result_alist(ResultAlist, TargetAtoms, RelevantWords) :-
	findall(SurfaceMatch,
		surface_match_for_result_alist_and_target_atoms(ResultAlist, TargetAtoms, SurfaceMatch),
		SurfaceMatches),
	append_list(SurfaceMatches, RelevantWords),
	!.

surface_match_for_result_alist_and_target_atoms(ResultAlist, TargetAtoms, SurfaceMatch) :-
	member(_Key-batchrec_item(Pairs), ResultAlist),
	member(words=Words, Pairs),
	member(SemanticAtom, TargetAtoms),
	words_covered_by_surface_pattern(Words, SemanticAtom, PossibleWordsCovered),
	member(SurfaceMatch, PossibleWordsCovered).

%---------------------------------------------------------------

all_confidence_values_in_result_alist([], []).
all_confidence_values_in_result_alist([_Key-Result | Results], [Confidence | RestConfidence]) :-
	Result = batchrec_item(Pairs),
	member(confidence=Confidence, Pairs),
	!,
	all_confidence_values_in_result_alist(Results, RestConfidence).

combine_words_and_word_confidence([], [], []).
combine_words_and_word_confidence([F | R], [F1 | R1], [F/F1 | R2]) :-
	combine_words_and_word_confidence(R, R1, R2).

%---------------------------------------------------------------

print_cumulative_batch_decoder_results(SOut, Assoc) :-
	print_cumulative_batch_decoder_results(SOut, no_tag, Assoc).

print_cumulative_batch_decoder_results(SOut, Tag, Assoc) :-
	get_assoc_or_zero(out_of_domain, Assoc, CountOutOfDomain),
	get_assoc_or_zero(in_domain, Assoc, CountInDomain),

	get_assoc_or_zero(misfire, Assoc, CountMisfires),
	get_assoc_or_zero(bad_reject, Assoc, CountBadReject),
	get_assoc_or_zero(bad_accept, Assoc, CountBadAccept),

	(   CountOutOfDomain > 0 ->
	    MisfireRate is ( 100 * CountMisfires ) / CountOutOfDomain ;
	    MisfireRate is 0
	),

	(   CountInDomain > 0 ->
	    BadRejectRate is ( 100 * CountBadReject ) / CountInDomain ;
	    BadRejectRate is 0
	),

	(   CountInDomain > 0 ->
	    BadAcceptRate is ( 100 * CountBadAccept ) / CountInDomain ;
	    BadAcceptRate is 0
	),

	format(SOut, '~N~n% OUT OF DOMAIN~n', []),
	format(SOut, '~N%      Misfires: ~d/~d (~1f%)~n', [CountMisfires, CountOutOfDomain, MisfireRate]),
	format(SOut, '~N~n% IN DOMAIN~n', []),
	format(SOut, '~N%   Bad rejects: ~d/~d (~1f%)~n', [CountBadReject, CountInDomain, BadRejectRate]),
	format(SOut, '~N% Misclassified: ~d/~d (~1f%)~n', [CountBadAccept, CountInDomain, BadAcceptRate]),

	(   Tag = no_tag ->
	    true ;
	    format(SOut, '~N~ntest_results(~q, ~1f, ~1f, ~1f).~n', [Tag, MisfireRate, BadRejectRate, BadAcceptRate])
	).
	
%--------------------------------------------------------------------------------------------

compare_trace_files(File1, File2, OutFile, Summary) :-
	prolog_file_to_list(File1, List1),
	prolog_file_to_list(File2, List2),

	length(List1, Len1),
	length(List2, Len2),

	(   Len1 \== Len2 ->
	    format('~N*** Error: files ~w has ~d records, ~w has ~d records.~n', [File1, Len1, File2, Len2]) ;

	    open(OutFile, write, S),
	    compare_trace_lists(List1, List2, S, 0-FirstGood, 0-SecondGood),
	    file_trace_summary(FirstGood, SecondGood, Summary),
	    close(S)
	).

compare_trace_lists([], [], _S, First-First, Second-Second).
compare_trace_lists([F | R], [F1 | R1], S, FirstIn-FirstOut, SecondIn-SecondOut) :-
	compare_trace_list_items(F, F1, S, FirstIn-FirstNext, SecondIn-SecondNext),
	!,
	compare_trace_lists(R, R1, S, FirstNext-FirstOut, SecondNext-SecondOut).

compare_trace_list_items(Item1, Item2, S, FirstIn-FirstOut, SecondIn-SecondOut) :-
	Item1 = result(Id, Status1, RefWords, RefAtoms, Words1, Atoms1, Confidence1, Pairs1),
	Item2 = result(Id, Status2, RefWords, RefAtoms, Words2, Atoms2, Confidence2, Pairs2),

	(   ( Status1 = 'Correct', Status2 \== 'Correct' ) ->
	    FirstOut is FirstIn + 1,
	    SecondOut = SecondIn,
	    ComparisonStatus = 'First better' ;
	    
	    ( Status2 = 'Correct', Status1 \== 'Correct' ) ->
	    FirstOut = FirstIn,
	    SecondOut is SecondIn + 1,
	    ComparisonStatus = 'Second better' ;

	    FirstOut = FirstIn,
	    SecondOut = SecondIn,
	    ComparisonStatus = same
	),

	(   ComparisonStatus = same ->
	    true ;
	    
	    format(S, '~N~n% -----------------------------------------------------------------~n', []),
	    format(S, '~N% ID: ~w~n', [Id]),
	    format(S, '~N% ~w~n~n', [ComparisonStatus]),
	    format(S, '~N% Reference words: ~w~n', [RefWords]),
	    format(S, '~N% Reference target atoms: ~w~n', [RefAtoms]),

	    format(S, '~N~n% FIRST FILE:', []),
	    format(S, '~N% Status: ~w~n', [Status1]),
	    format(S, '~N% Recognised words: ~w~n', [Words1]),
	    format(S, '~N% Decoded target atoms: ~w~n~n', [Atoms1]),
	    format(S, '~N% Confidence: ~d~n~n', [Confidence1]),
	    present_decoder_pairs_used(S, Pairs1),
	
	    format(S, '~N~n% SECOND FILE:', []),
	    format(S, '~N% Status: ~w~n', [Status2]),
	    format(S, '~N% Recognised words: ~w~n', [Words2]),
	    format(S, '~N% Decoded target atoms: ~w~n~n', [Atoms2]),
	    format(S, '~N% Confidence: ~d~n~n', [Confidence2]),
	    present_decoder_pairs_used(S, Pairs2)
	),
	!.
compare_trace_list_items(Item1, Item2, S, First, Second) :-
	format('~N*** Error: bad call: ~w~n', [compare_trace_list_items(Item1, Item2, S, First, Second)]),
	fail.
	    
file_trace_summary(FirstGood, SecondGood, Summary) :-
	Summary = [first_good=FirstGood, second_good=SecondGood].

%---------------------------------------------------------------

load_annotated_wavfiles_file(AnnotatedWavfiles) :-
	(   current_predicate(wavfile_and_annotations, wavfile_and_annotations(_, _, _)) ->
	    abolish(wavfile_and_annotations/3) ;
	    true
	),
	compile(AnnotatedWavfiles).

%---------------------------------------------------------------

inc_assoc_multiple(AssocIn, [], AssocIn) :-
	!.
inc_assoc_multiple(AssocIn, [F | R], AssocOut) :-
	inc_assoc(AssocIn, F, AssocNext),
	!,
	inc_assoc_multiple(AssocNext, R, AssocOut).

