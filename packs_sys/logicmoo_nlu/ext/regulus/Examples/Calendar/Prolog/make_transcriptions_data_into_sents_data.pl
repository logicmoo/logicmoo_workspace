
:- use_module('$REGULUS/PrologLib/batchrec_tools.pl').
:- use_module('$REGULUS/PrologLib/utilities.pl').

:- use_module(library(lists)).

test :-
	transcriptions2sents('$REGULUS/Examples/Calendar/corpora/calendar_transcriptions.txt',
			     '$REGULUS/Examples/Calendar/corpora/dev_corpus_from_transcriptions.pl').

transcriptions2sents(InFile, OutFile) :-
	parse_transcriptions_file_to_list(InFile, InList),
	transcriptions_list2sents_list(InList, '*start*', OutList),
	length(OutList, NOut),

	absolute_file_name(OutFile, AbsOutFile),
	list_to_prolog_file(OutList, AbsOutFile),
	format('~N~n--- Written sents file (~d records) ~w~n', [NOut, AbsOutFile]),
	!.
transcriptions2sents(InFile, OutFile) :-
	format('~N*** Error: bad call: ~w~n', [transcriptions2sents(InFile, OutFile)]),
	fail.

transcriptions_list2sents_list([], _PreviousDir, []).
transcriptions_list2sents_list([F | R], PreviousDir, OutList) :-
	transcriptions_list2sents_list_single(F, PreviousDir-NextDir, OutList-NextList),
	!,
	transcriptions_list2sents_list(R, NextDir, NextList).

transcriptions_list2sents_list_single(transcription(Wavfile, WordList), PreviousDir-NextDir, OutList-NextList) :-
	join_with_spaces(WordList, Sent),
	SentRecord = sent(Sent),
	directory_for_wavfile(Wavfile, NextDir),
	(   NextDir \== PreviousDir ->
	    OutList = [set_notional_time(NextDir), init_dialogue | NextList0]
	;
	    otherwise ->
	    OutList = NextList0
	),
	(   WordList = [] ->
	    NextList0 = NextList
	;
	    otherwise ->
	    NextList0 = [SentRecord | NextList]
	),
	!.
transcriptions_list2sents_list_single(Transcription, Dirs, Lists) :-
	format('~N*** Error: bad call: ~w~n', [transcriptions_list2sents_list_single(Transcription, Dirs, Lists)]),
	fail.

directory_for_wavfile(Wavfile, Dir) :-
	split_atom_into_words(Wavfile, 0'/, List),
	last_but_one_element_of_list(List, Dir),
	!.
directory_for_wavfile(Wavfile, Dir) :-
	format('~N*** Error: bad call: ~w~n', [directory_for_wavfile(Wavfile, Dir)]),
	fail.

last_but_one_element_of_list([X, _], X) :-
	!.
last_but_one_element_of_list([_F | R], X) :-
	last_but_one_element_of_list(R, X).
