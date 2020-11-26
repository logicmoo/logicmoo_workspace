
:- use_module('$REGULUS/PrologLib/batchrec_tools.pl').
:- use_module('$REGULUS/PrologLib/utilities.pl').

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).

test :-
	randomly_split_transcriptions('$REGULUS/Examples/Calendar/corpora/calendar_transcriptions.txt',
				      '$REGULUS/Examples/Calendar/corpora/calendar_transcriptions_training.txt',
				      '$REGULUS/Examples/Calendar/corpora/calendar_transcriptions_test.txt',
				      0.5).

randomly_split_transcriptions(InFile, OutFile1, OutFile2, Prob) :-
	set_random_generator_from_time,
	safe_absolute_file_name(InFile, AbsInFile),
	parse_transcriptions_file_to_list(InFile, InList),
	length(InList, NIn),
	format('~N~n--- Read sents file (~d records) ~w~n', [NIn, AbsInFile]),
	
	transcriptions_list2sents_lists(InList, '*start*', '*undefined*', Prob, OutList1, OutList2),

	length(OutList1, NOut1),
	safe_absolute_file_name(OutFile1, AbsOutFile1),
	write_atom_list_to_file(OutList1, AbsOutFile1),
	format('~N~n--- Written sents file (~d records) ~w~n', [NOut1, AbsOutFile1]),

	length(OutList2, NOut2),
	safe_absolute_file_name(OutFile2, AbsOutFile2),
	write_atom_list_to_file(OutList2, AbsOutFile2),
	format('~N~n--- Written sents file (~d records) ~w~n', [NOut2, AbsOutFile2]),
	!.
randomly_split_transcriptions(InFile, OutFile1, OutFile2, Prob) :-
	format('~N*** Error: bad call: ~w~n', [randomly_split_transcriptions(InFile, OutFile1, OutFile2, Prob)]),
	fail.

transcriptions_list2sents_lists([], _PreviousDir, _PreviousChoice, _Prob, [], []).
transcriptions_list2sents_lists([F | R], PreviousDir, PreviousChoice, Prob, OutList1, OutList2) :-
	transcriptions_list2sents_lists_single(F, PreviousDir-NextDir, PreviousChoice-NextChoice, Prob, OutList1-NextList1, OutList2-NextList2),
	!,
	transcriptions_list2sents_lists(R, NextDir, NextChoice, Prob, NextList1, NextList2).

transcriptions_list2sents_lists_single(transcription(Wavfile, WordList),
				      PreviousDir-NextDir, PreviousChoice-NextChoice, Prob,
				      OutList1-NextList1, OutList2-NextList2) :-
	join_with_spaces([Wavfile | WordList], Transcription),
	directory_for_wavfile(Wavfile, NextDir),
	(   NextDir \== PreviousDir ->
	    random_choice(Prob, NextChoice)  
	;
	    otherwise ->
	    NextChoice = PreviousChoice
	),
	(   NextChoice = first ->
	    OutList1 = [Transcription | NextList1],
	    OutList2 = NextList2
	;
	    OutList1 = NextList1,
	    OutList2 = [Transcription | NextList2]
	),
	!.
transcriptions_list2sents_lists_single(TranscriptionRecord, Dirs, Choices, Prob, List1, List2) :-
	format('~N*** Error: bad call: ~w~n', [transcriptions_list2sents_lists_single(TranscriptionRecord, Dirs, Choices, Prob, List1, List2)]),
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

set_random_generator_from_time :-
	datime(Datime),
	Datime = datime(Y, Mo, D, H, Mi, S),
	YMo is 12 * Y + Mo,
	DH is 24 * D + H,
	MiS is 60 * Mi + S,
	setrand(rand(YMo, DH, MiS)),
	!.

random_choice(Prob, Choice) :-
	random(X),
	(   X =< Prob ->
	    Choice = first
	;
	    Choice = second
	).
