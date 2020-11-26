
:- module(nbest,
	[unpack_nbest_batchrec_list/2,
	 unpack_nbest_batchrec_list_for_dialogue/2,

	 notional_batchrec_item_for_transcription_in_nbest_list/2,
	 
	 nbest_preferences/4,
	 print_nbest_trace/3,
	 maybe_extract_and_print_training_data/2,
	 nbest_preferences_score_for_dialogue_record/3,
	 print_nbest_preference_info/2,

	 init_stored_nbest_translation_summary/0,
	 store_nbest_translation_summary/6,
	 print_stored_nbest_translation_summary/1,

	 zero_nbest_training_data_file_if_necessary/1,
	 print_nbest_training_data/0,
	 dont_print_nbest_training_data/0,

	 set_nbest_n/1,
	 get_nbest_n/1
	]).

:- use_module('$REGULUS/Prolog/paraphrases').

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

/*
batchrec_item([wavfile='c:/cygwin/home/speech/regulus/examples/toy1/corpora/wavfiles/utt01.wav',
	       transcription=[switch,on,the,light],
	       
	       words=[switch,on,the,lights],
	       confidence=39,
	       nl_confidence=39,
	       nl_value=[[utterance_type,command],[action,switch],[onoff,on],[device,light]],
	       
	       words=[switch,off,the,lights],
	       confidence=38,
	       nl_confidence=39,
	       nl_value=[[utterance_type,command],[action,switch],[onoff,off],[device,light]],
	       
	       transcription=[switch,on,the,light]
	      ]).
*/

unpack_nbest_batchrec_list(BatchrecList, NBestBatchrecLists) :-
	select_pairs(BatchrecList, words, WordsList, NWordsLists),
	select_pairs(BatchrecList, confidence, ConfidenceList, NConfidenceList),
	select_pairs(BatchrecList, nl_confidence, NLConfidenceList, NNLConfidenceList),
	select_pairs(BatchrecList, nl_value, NLValueList, NNLValueList),
	member(wavfile=Wavfile, BatchrecList),
	(   member(transcription=Transcription, BatchrecList) ->
	    true
	;
	    otherwise ->
	    transcription = ['*no_transcription*']
	),
	% Check all lists have same length and that that length is greater than 1
	sort([NWordsLists, NConfidenceList, NNLConfidenceList, NNLValueList], [Length]),
	Length > 1,
	unpack_nbest_batchrec_list1(1, WordsList, ConfidenceList, NLConfidenceList, NLValueList, Wavfile, Transcription,
				    NBestBatchrecLists0),
	(   get_nbest_n(N) ->
	    firstn_or_all(NBestBatchrecLists0, N, NBestBatchrecLists)
	;
	    otherwise ->
	    NBestBatchrecLists0 = NBestBatchrecLists
	).

unpack_nbest_batchrec_list1(_N, [], [], [], [], _Wavfile, _Transcription, []).
unpack_nbest_batchrec_list1(I, [F | R], [F1 | R1], [F2 | R2], [F3 | R3], Wavfile, Transcription, [FCombined | RCombined]) :-
	create_nbest_item(I, F, F1, F2, F3, Wavfile, Transcription, FCombined),
	I1 is I + 1,
	!,
	unpack_nbest_batchrec_list1(I1, R, R1, R2, R3, Wavfile, Transcription, RCombined).

create_nbest_item(I, F, F1, F2, F3, Wavfile, Transcription, FCombined) :-
	FCombined = [wavfile=Wavfile,
		     transcription=Transcription,
		     rank=I,
		     F, F1, F2, F3].

unpack_nbest_batchrec_list_for_dialogue(BatchrecList, NBestBatchrecLists) :-
	select_pairs(BatchrecList, words, WordsList, NWordsLists),
	select_pairs(BatchrecList, confidence, ConfidenceList, NConfidenceList),
	member(wavfile=Wavfile, BatchrecList),
	(   member(transcription=Transcription, BatchrecList) ->
	    true
	;
	    otherwise ->
	    transcription = ['*no_transcription*']
	),
	% Check both lists have same length and that that length is greater than 1
	sort([NWordsLists, NConfidenceList], [Length]),
	Length > 1,
	unpack_nbest_batchrec_list_for_dialogue1(1, WordsList, ConfidenceList, Wavfile, Transcription,
						 NBestBatchrecLists0),
	(   get_nbest_n(N) ->
	    firstn_or_all(NBestBatchrecLists0, N, NBestBatchrecLists)
	;
	    otherwise ->
	    NBestBatchrecLists0 = NBestBatchrecLists
	).

unpack_nbest_batchrec_list_for_dialogue1(_N, [], [], _Wavfile, _Transcription, []).
unpack_nbest_batchrec_list_for_dialogue1(I, [F | R], [F1 | R1], Wavfile, Transcription, [FCombined | RCombined]) :-
	create_nbest_item_for_dialogue(I, F, F1, Wavfile, Transcription, FCombined),
	I1 is I + 1,
	!,
	unpack_nbest_batchrec_list_for_dialogue1(I1, R, R1, Wavfile, Transcription, RCombined).

create_nbest_item_for_dialogue(I, F, F1, Wavfile, Transcription, FCombined) :-
	FCombined = [wavfile=Wavfile,
		     transcription=Transcription,
		     rank=I,
		     F,
		     F1].

select_pairs(List, Key, SelectedList, N) :-
	select_pairs1(List, Key, SelectedList),
	length(SelectedList, N).

select_pairs1([], _Key, []).
select_pairs1([Key=Value | R], Key, [Key=Value | R1]) :-
	!,
	select_pairs1(R, Key, R1).
select_pairs1([_F | R], Key, R1) :-
	!,
	select_pairs1(R, Key, R1).

%---------------------------------------------------------------

notional_batchrec_item_for_transcription_in_nbest_list(BatchrecList, TranscriptionBatchrecList) :-
	member(wavfile=Wavfile, BatchrecList),
	member(transcription=SourceWords, BatchrecList),
	\+ SourceWords = '*no_transcription*',
	\+ member('(guessed)', SourceWords),
	(   paraphrase_for_sentence_words(SourceWords, ParaphraseWords) ->
	    TranscriptionBatchrecList = [wavfile=Wavfile,
					 words=ParaphraseWords,
					 transcription=SourceWords,
					 paraphrase=ParaphraseWords,
					 rank=0,
					 confidence=100]
	;
	    otherwise ->
	    TranscriptionBatchrecList = [wavfile=Wavfile,
					 words=SourceWords,
					 transcription=SourceWords,
					 rank=0,
					 confidence=100]
	),
	!.

%---------------------------------------------------------------

/*

Tuples is a list of elements of the form [record=Record | OtherMaterial]

Each Record is an alist produced by dialogue:dialogue_process_item_normal. It must contain an element of the form rank=Rank.

*/

nbest_preferences(Tuples, ChosenTuple, Trace, RankOfChosenElement) :-
	findall(Score-[Tuple, PrefTrace],
		( member(Tuple, Tuples), nbest_preferences_score(Tuple, Score, PrefTrace) ),
		ScoredTuples),
	keysort(ScoredTuples, SortedScoredTuples),
	reverse(SortedScoredTuples, ReversedSortedScoredTuples),
	ReversedSortedScoredTuples = [BestElement | _Rest],
	get_rank_for_trace_element(BestElement, RankOfChosenElement),
	BestElement = _BestScore-[ChosenTuple, _ChosenPrefTrace],
	Trace = ScoredTuples,
	!.
nbest_preferences(_Tuples, _ChosenTuple, _Trace) :-
	format2error('~N*** Error in nbest_preferences/3~n', []),
	fail.

nbest_preferences_score(Tuple, TotalScore, PrefTrace) :-
	member(record=Record, Tuple),
	nbest_preferences_score_for_dialogue_record(Record, TotalScore, PrefTrace).

nbest_preferences_score_for_dialogue_record(Record, TotalScore, PrefTrace) :-
	remove_judgements_from_record(Record, Record1),
	findall([Feature, Weight, Score],
		(   nbest_features:feature_weight(Feature, Weight),
		    nbest_features:feature_value_for_record(Feature, Record1, Score)
		),
		Triples),
	total_score(Triples, 0-TotalScore),
	PrefTrace = Triples.

total_score([], In-In).
total_score([[_Feature, Weight, Score] | R], In-Out) :-
	Next is In + ( Score * Weight ),
	!,
	total_score(R, Next-Out).

%---------------------------------------------------------------

print_nbest_trace(Trace, RankOfChosenElement, S) :-
	(   RankOfChosenElement = 1 ->
	    Comment = ''
	;
	    otherwise ->
	    Comment = 'NON-TOP '
	),
	format(S, '~N~n/*~n', []),
	format(S, '~NN-best trace (~whypothesis #~d selected):~n~n', [Comment, RankOfChosenElement]),
	print_nbest_trace1(Trace, S, 0),
	format(S, '~N*/~n', []),
	
	maybe_extract_and_print_training_data(Trace, RankOfChosenElement),
	!.
print_nbest_trace(_Trace, _S) :-
	format2error('~N*** Error: unable to print N-best trace~n', []),
	fail.

print_nbest_trace1([], _S, _N).
print_nbest_trace1([F | R], S, N) :-
	get_rec_result_and_transcription_for_trace_element(F, RecResult, Transcription),
	(   N = 0 ->
	    format(S, '~NHypothesis #~d~n"~w" (transcription - not real speech hypothesis)~n', [N, RecResult])
	;
	    otherwise ->
	    format(S, '~NHypothesis #~d~n"~w" (correct: "~w")~n', [N, RecResult, Transcription])
	),
	print_nbest_trace_element(F, S),
	N1 is N + 1,
	!,
	print_nbest_trace1(R, S, N1).

print_nbest_trace_element(TraceElement, S) :-
	TraceElement = Score-[Tuple, PrefTrace],
	member(record=Record, Tuple),
	remove_judgements_from_record(Record, Record1),
	format(S, '~N~n', []),
	prettyprint_to_stream(S, Record1),
	format(S, '~N~nPreference info:~n~n', []),
	%prettyprint_to_stream(S, PrefTrace),
	print_nbest_preference_info(PrefTrace, S),
	FloatScore is float(Score),
	format(S, '~NScore: ~2f~n~n', [FloatScore]),
	!.
print_nbest_trace_element(TraceElement, _S) :-
	format2error('~N*** Error: unable to print N-best trace element~n', []),
	prettyprint(TraceElement),
	fail.

print_nbest_preference_info([], _S).
print_nbest_preference_info([F | R], S) :-
	print_nbest_preference_info_item(F, S),
	!,
	print_nbest_preference_info(R, S).

print_nbest_preference_info_item(Item, S) :-
	Item = [Feature, Weight, Score],
	WeightedScore is Weight * Score,
	FloatWeight is float(Weight),
	FloatScore is float(Score),
	FloatWeightedScore is float(WeightedScore),
	format(S, '~N~w~40|~2f * ~2f = ~2f~n', [Feature, FloatWeight, FloatScore, FloatWeightedScore]).

%---------------------------------------------------------------

:- dynamic stored_nbest_translation_summary/1.

init_stored_nbest_translation_summary :-
	retractall(stored_nbest_translation_summary(_)).

store_nbest_translation_summary(Source, Recognised, Wavfile, Target, Stats, Judgement) :-
	member(non_top_hyp=yes, Stats),
	member(selected_hyp=Rank, Stats),
	member(interlingua_surface=InterlinguaSurface, Stats),
	assertz(stored_nbest_translation_summary([wavfile=Wavfile,
						  source=Source,
						  recognised=Recognised,
						  selected_hyp=Rank,
						  interlingua_surface=InterlinguaSurface,
						  target=Target,
						  judgement=Judgement])),
	!.
store_nbest_translation_summary(_Source, _Recognised, _Wavfile, _Target, _Stats, _Judgement).
						  
print_stored_nbest_translation_summary(_S) :-
	\+ stored_nbest_translation_summary(_),
	!.
print_stored_nbest_translation_summary(S) :-
	format(S, '~N~n~n/*', []),
	format(S, '~N~n*** N-BEST TRANSLATION SUMMARY ***~n', []),
	print_stored_nbest_translation_summary1(S),
	format(S, '~N~n~n*/~n', []),
	!.

print_stored_nbest_translation_summary1(S) :-
	stored_nbest_translation_summary(List),
	format(S, '~N~n', []),
	prettyprintq_to_stream(S, nbest_example(List), 0, 150),
	format(S, '.~n', []),
	fail.
print_stored_nbest_translation_summary1(_S).

%---------------------------------------------------------------

maybe_extract_and_print_training_data(Trace, RankOfChosenElement) :-
	(   printing_nbest_training_data ->
	    extract_and_print_training_data(Trace, RankOfChosenElement)
	;
	    otherwise ->
	    true
	).

extract_and_print_training_data(Trace, _RankOfChosenElement) :-
	get_nbest_training_data_file(File),
	extract_nbest_training_data(Trace, Data),
	open(File, append, S),
	format(S, '~N~n', []),
	prettyprintq_to_stream(S, Data),
	%format(S, '.~N~n', []),
	%print_semantic_recognition_info_as_comment(S, Trace, RankOfChosenElement),
	close(S),
	!.
extract_and_print_training_data(Trace, RankOfChosenElement) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [extract_and_print_training_data(Trace, RankOfChosenElement)]),
	fail.

zero_nbest_training_data_file_if_necessary(File) :-
	printing_nbest_training_data,
	get_nbest_training_data_file(File),
	open(File, write, S),
	close(S),
	!.
zero_nbest_training_data_file_if_necessary(no_file).

get_nbest_training_data_file(AbsFile) :-
	user:get_regulus_config_item(nbest_training_data_file, File),
	safe_absolute_file_name(File, AbsFile),
	!.

%---------------------------------------------------------------

print_semantic_recognition_info_as_comment(S, Trace, RankOfChosenElement) :-
	format(S, '~N%~n', []),
	format(S, '~N% ---------------------------------~n', []),
	format(S, '~N%~n', []),
	(   \+ trace_shows_good_semantic_recognition_exists(Trace) ->
	    format(S, '~N% [ALL CHOICES ARE BAD]~n', []),
	    format(S, '~N%~n', [])
	;
	    trace_shows_bad_semantic_recognition(Trace, RankOfChosenElement) ->
	    format(S, '~N% [BAD N-BEST CHOICE]~n', []),
	    format(S, '~N%~n', [])
	;
	    RankOfChosenElement > 1 ->
	    format(S, '~N% [GOOD NON-TOP HYPOTHESIS CHOSEN]~n', []),
	    format(S, '~N%~n', [])
	;
	    otherwise ->
	    format(S, '~N%~n', [])
	),
	print_semantic_recognition_as_comment(S, Trace, RankOfChosenElement),
	!.
print_semantic_recognition_info_as_comment(_S, _Trace, _RankOfChosenElement).

trace_shows_good_semantic_recognition_exists(Trace) :-
	member(_Score-[Tuple, _PrefTrace], Trace),
	member(record=Record, Tuple),
	member(sem_recognition=GoodOrUnclear, Record),
	good_or_unclear(GoodOrUnclear),
	member(rank=Rank, Record),
	Rank > 0,
	!.

good_or_unclear(good).
good_or_unclear(unclear).

trace_shows_bad_semantic_recognition(Trace, RankOfChosenElement) :-
	member(_Score-[Tuple, _PrefTrace], Trace),
	member(record=Record, Tuple),
	member(rank=RankOfChosenElement, Record),
	member(sem_recognition=bad, Record),
	!.

print_semantic_recognition_as_comment(_S, [], _RankOfChosenElement).
print_semantic_recognition_as_comment(S, [F | R], RankOfChosenElement) :-
	print_semantic_recognition_hyp_as_comment(S, F, RankOfChosenElement),
	!,
	print_semantic_recognition_as_comment(S, R, RankOfChosenElement).

print_semantic_recognition_hyp_as_comment(S, TraceElement, RankOfChosenElement) :-
	get_rec_result_and_transcription_for_trace_element(TraceElement, RecSent, TranscriptionSent),
	get_rank_for_trace_element(TraceElement, Rank),
	(   Rank = 0 ->
	    format(S, '~N% Transcription: "~w"~n~n', [TranscriptionSent])
	;
	    otherwise ->
	    (   Rank = RankOfChosenElement ->
		Tag = ' (selected)'
	    ;
		Tag = ''
	    ),
	    format(S, '~N% ~d: "~w"~w~n', [Rank, RecSent, Tag])
	).
print_semantic_recognition_hyp_as_comment(S, TraceElement, RankOfChosenElement) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [print_semantic_recognition_hyp_as_comment(S, TraceElement, RankOfChosenElement)]),
	fail.

%---------------------------------------------------------------

get_rec_result_and_transcription_for_trace_element(TraceElement, RecSent, TranscriptionSent) :-
	TraceElement = _Score-[Tuple, _PrefTrace],
	member(record=Record, Tuple),
	member(sent=TranscriptionSent, Record),
	member(recognised=RecWords, Record),
	join_with_spaces(RecWords, RecSent),
	!.
get_rec_result_and_transcription_for_trace_element(TraceElement, RecSent, TranscriptionSent) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [get_rec_result_and_transcription_for_trace_element(TraceElement, RecSent, TranscriptionSent)]),
	fail.

get_rank_for_trace_element(TraceElement, Rank) :-
	TraceElement = _Score-[Tuple, _PrefTrace],
	member(record=Record, Tuple),	
	member(rank=Rank, Record),
	!.
get_rank_for_trace_element(TraceElement, Rank) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [get_rank_for_trace_element(TraceElement, Rank)]),
	fail.

%---------------------------------------------------------------

:- dynamic printing_nbest_training_data/0.

print_nbest_training_data :-
	(   user:get_regulus_config_item(nbest_training_data_file, _File) ->
	    retractall(printing_nbest_training_data),
	    assertz(printing_nbest_training_data)
	;
	    otherwise ->
	    format2error('~N*** Error: meaningless to switch on printing of N-best training data without nbest_training_data_file entry~n', []),
	    fail
	).	

dont_print_nbest_training_data :-
	retractall(printing_nbest_training_data).

%---------------------------------------------------------------

extract_nbest_training_data(Trace, Data) :-
	Data = nbest_data([wavfile=Wavfile, correct_words=Sent],
			  HypsData),
	get_wavfile_and_correct_words_from_trace(Trace, Wavfile, Sent),
	extract_nbest_training_data1(Trace, HypsData),
	!.

get_wavfile_and_correct_words_from_trace(Trace, Wavfile, SentWords) :-
	Trace = [FirstHyp | _Rest],
	FirstHyp = _Score-[Tuple, _PrefTrace],
	member(record=Record, Tuple),
	(   member(wavfile=Wavfile, Record) ->
	    true
	;
	    otherwise ->
	    Wavfile = unknown_wavfile
	),
	member(sent=SentAtom, Record),
	split_atom_into_words(SentAtom, SentWords),
	!.
get_wavfile_and_correct_words_from_trace(Trace, Wavfile, SentWords) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [get_wavfile_and_correct_words_from_trace(Trace, Wavfile, SentWords)]),
	fail.

extract_nbest_training_data1([], []).
extract_nbest_training_data1([F | R], [F1 | R1]) :-
	extract_training_data_from_hyp(F, F1),
	!,
	extract_nbest_training_data1(R, R1).

extract_training_data_from_hyp(Hyp, FeatValPairs) :-
	Hyp = _Score-[Tuple, PrefTrace],
	member(record=Record, Tuple),
	remove_judgements_from_record(Record, Record1),
	findall(Feat=Val,
		(   extract_feat_val_from_record_if_possible(Record1, Feat, Val)
		;   extract_feat_val_from_pref_trace(PrefTrace, Feat, Val)
		),
		FeatValPairs0),
	safe_remove_duplicates(FeatValPairs0, FeatValPairs).

extract_feat_val_from_pref_trace(PrefTrace, Feat, Val) :-
	member([Feat, _Weight, Val], PrefTrace).

extract_feat_val_from_record_if_possible(Record, Feat, Val) :-
	current_predicate(nbest_features:extract_feat_val_from_record/3),
	nbest_features:extract_feat_val_from_record(Record, Feat, Val).

%---------------------------------------------------------------

:- dynamic nbest_n/1.

set_nbest_n(N) :-
	integer(N),
	retractall(nbest_n(_)),
	assertz(nbest_n(N)),
	!.
set_nbest_n(N) :-
	format2error('~N*** Error: bad call: ~w~n', [set_nbest_n(N)]),
	fail.

get_nbest_n(N) :-
	nbest_n(N).
