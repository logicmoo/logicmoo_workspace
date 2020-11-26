% Top-level for translation

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(split_corpus,
	[split_speech_corpus_with_respect_to_training_corpus/4,
	 split_speech_corpus_with_respect_to_current_grammar/4
	]).

:- use_module('$REGULUS/PrologLib/batchrec_tools').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

split_speech_corpus_with_respect_to_training_corpus(MainTranscriptionsFile,
						    CorpusFile,
						    TrainingTranscriptionsFile,
						    NonTrainingTranscriptionsFile) :-
	safe_absolute_file_name(MainTranscriptionsFile, AbsMainTranscriptionsFile),
	safe_absolute_file_name(CorpusFile, AbsCorpusFile),
	safe_absolute_file_name(TrainingTranscriptionsFile, AbsTrainingTranscriptionsFile),
	safe_absolute_file_name(NonTrainingTranscriptionsFile, AbsNonTrainingTranscriptionsFile),

	parse_transcriptions_file_to_list(AbsMainTranscriptionsFile, MainList),
	length(MainList, NMainList),

	record_training_corpus_sentences_for_splitting_corpus(AbsCorpusFile),

	format('~N~n--- Splitting transcription file with respect to current grammar (~d entries): ~w~n',
	       [NMainList, AbsMainTranscriptionsFile]),
	split_transcriptions_list_with_respect_to_training_corpus(MainList, InList, OutList),
	format('~N--- Done~n', []),
	
	length(InList, NInList),
	length(OutList, NOutList),

	write_out_transcription_file_list(InList, AbsTrainingTranscriptionsFile),
	write_out_transcription_file_list(OutList, AbsNonTrainingTranscriptionsFile),

	format('~N---  Written in-training part (~d entries) ~w~n', [NInList, AbsTrainingTranscriptionsFile]),
	format('~N---  and out-of-training part (~d entries) ~w~n', [NOutList, AbsNonTrainingTranscriptionsFile]),
	!.
split_speech_corpus_with_respect_to_training_corpus(MainTranscriptionsFile,
						    CorpusFile,
						    TrainingTranscriptionsFile,
						    NonTrainingTranscriptionsFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [split_speech_corpus_with_respect_to_training_corpus(MainTranscriptionsFile,
									  CorpusFile,
									  TrainingTranscriptionsFile,
									  NonTrainingTranscriptionsFile)]),
	fail.

%---------------------------------------------------------------

split_transcriptions_list_with_respect_to_training_corpus([], [], []) :-
	!.
split_transcriptions_list_with_respect_to_training_corpus([F | R], InList, OutList) :-
	F = transcription(_Wavfile, Words),
	(   words_list_is_in_training_corpus(Words) ->
	    
	    InList = [F | InListNext],
	    OutList = OutListNext ;

	    InList = InListNext,
	    OutList = [F | OutListNext]
	),
	format('.', []), flush_output(user),
	!,
	split_transcriptions_list_with_respect_to_training_corpus(R, InListNext, OutListNext).

%---------------------------------------------------------------

:- dynamic in_training_corpus/1.

record_training_corpus_sentences_for_splitting_corpus(AbsCorpusFile) :-
	retractall(in_training_corpus(_)),
	prolog_file_to_list(AbsCorpusFile, List),
	length(List, N),
	record_training_corpus_sentences_for_splitting_corpus1(List),
	format('~N--- Read training corpus (~d entries) ~w~n', [N, AbsCorpusFile]),
	!.

record_training_corpus_sentences_for_splitting_corpus1([]).
record_training_corpus_sentences_for_splitting_corpus1([F | R]) :-
	record_training_corpus_sentence_record_for_splitting_corpus(F),
	!,
	record_training_corpus_sentences_for_splitting_corpus1(R).

record_training_corpus_sentence_record_for_splitting_corpus(Record) :-
	functor(Record, sent, _),
	arg(1, Record, Sent),
	assertz(in_training_corpus(Sent)),
	!.
record_training_corpus_sentence_record_for_splitting_corpus(Record) :-
	format('~N*** Warning: couldn\'t handle record in training corpus ~w~n', [Record]).

words_list_is_in_training_corpus(Words) :-
	join_with_spaces(Words, WordsAtom),
	in_training_corpus(WordsAtom).

%---------------------------------------------------------------

split_speech_corpus_with_respect_to_current_grammar(MainTranscriptionsFile,
						    GrammarAtom,
						    InCoverageTranscriptionsFile,
						    OutOfCoverageTranscriptionsFile) :-
	safe_absolute_file_name(MainTranscriptionsFile, AbsMainTranscriptionsFile),
	safe_absolute_file_name(InCoverageTranscriptionsFile, AbsInCoverageTranscriptionsFile),
	safe_absolute_file_name(OutOfCoverageTranscriptionsFile, AbsOutOfCoverageTranscriptionsFile),

	parse_transcriptions_file_to_list(AbsMainTranscriptionsFile, MainList),
	length(MainList, NMainList),

	format('~N~n--- Splitting transcription file with respect to current grammar (~d entries): ~w~n',
	       [NMainList, AbsMainTranscriptionsFile]),
	split_transcriptions_list_with_respect_to_current_grammar(MainList, GrammarAtom, InList, OutList),
	format('~N--- Done~n', []),
	
	length(InList, NInList),
	length(OutList, NOutList),

	write_out_transcription_file_list(InList, AbsInCoverageTranscriptionsFile),
	write_out_transcription_file_list(OutList, OutOfCoverageTranscriptionsFile),

	format('~N---  Written in-coverage part (~d entries) ~w~n', [NInList, AbsInCoverageTranscriptionsFile]),
	format('~N---  and out-of-coverage part (~d entries) ~w~n', [NOutList, AbsOutOfCoverageTranscriptionsFile]),
	!.
split_speech_corpus_with_respect_to_current_grammar(MainTranscriptionsFile,
						    GrammarAtom,
						    InCoverageTranscriptionsFile,
						    OutOfCoverageTranscriptionsFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [split_speech_corpus_with_respect_to_current_grammar(MainTranscriptionsFile,
									  GrammarAtom,
									  InCoverageTranscriptionsFile,
									  OutOfCoverageTranscriptionsFile)]),
	fail.

split_transcriptions_list_with_respect_to_current_grammar([], _GrammarAtom, [], []) :-
	!.
split_transcriptions_list_with_respect_to_current_grammar([F | R], GrammarAtom, InList, OutList) :-
	F = transcription(_Wavfile, Words),
	(   words_list_is_in_coverage(Words, GrammarAtom) ->
	    
	    InList = [F | InListNext],
	    OutList = OutListNext ;

	    InList = InListNext,
	    OutList = [F | OutListNext]
	),
	format('.', []), flush_output(user),
	!,
	split_transcriptions_list_with_respect_to_current_grammar(R, GrammarAtom, InListNext, OutListNext).

words_list_is_in_coverage(WordList, GrammarAtom) :-
	user:words_to_lf_and_tree_with_current_parser(WordList, GrammarAtom, _LF, _Tree),
	!.

write_out_transcription_file_list(List, File) :-
	transcription_file_list_to_atoms(List, List1),
	write_atom_list_to_file(List1, File),
	!.

transcription_file_list_to_atoms([], []) :-
	!.
transcription_file_list_to_atoms([F | R], [F1 | R1]) :-
	transcription_file_list_item_to_atom(F, F1),
	!,
	transcription_file_list_to_atoms(R, R1).

transcription_file_list_item_to_atom(transcription(Wavfile, Words), Atom) :-
	join_with_spaces([Wavfile | Words], Atom),
	!.
transcription_file_list_item_to_atom(F, F1) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [transcription_file_list_item_to_atom(F, F1)]),
	fail.

