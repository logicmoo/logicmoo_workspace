
:- use_module('$REGULUS/PrologLib/utilities').
:- use_module('$REGULUS/PrologLib/batchrec_tools').

:- use_module(library(lists)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).

update_transcriptions(TranscriptionsFile, ListingsFile, RootDir, NewTranscriptionsFile) :-
	absolute_file_name(TranscriptionsFile, AbsTranscriptionsFile),
	absolute_file_name(ListingsFile, AbsListingsFile),
	absolute_file_name(NewTranscriptionsFile, AbsNewTranscriptionsFile),
	
	read_file_to_atom_list(AbsListingsFile, WavfilesList),
	length(WavfilesList, NWavfiles),
	format('~N--- Read wavfile listings file (~d elements) ~w~n', [NWavfiles, AbsListingsFile]),

	(   file_exists(AbsTranscriptionsFile) ->
	    parse_transcriptions_file_to_list(AbsTranscriptionsFile, TranscriptionsListIn),
	    length(TranscriptionsListIn, NTranscriptions),
	    format('~N--- Read transcriptions file (~d elements) ~w~n', [NTranscriptions, AbsTranscriptionsFile])
	;
	    otherwise ->
	    TranscriptionsListIn = [],
	    format('~N--- No initial transcriptions file~n', [])
	    
	),

	fill_in_transcriptions_in_wavfile_list(WavfilesList, TranscriptionsListIn, RootDir, TranscriptionsListOut, 0-NOld, 0-NNew),
	
	write_atom_list_to_file(TranscriptionsListOut, AbsNewTranscriptionsFile),
	format('~N--- Written transcriptions file (~d old elements, ~d new elements) ~w~n', [NOld, NNew, AbsNewTranscriptionsFile]),
	!.

fill_in_transcriptions_in_wavfile_list([], _TranscriptionsListIn, _RootDir, [], OldIn-OldIn, NewIn-NewIn).
fill_in_transcriptions_in_wavfile_list([F | R], TranscriptionsListIn, RootDir, [F1 | R1], OldIn-OldOut, NewIn-NewOut) :-
	fill_in_transcriptions_in_wavfile_list_single(F, TranscriptionsListIn, RootDir, F1, OldIn-OldNext, NewIn-NewNext),
	!,
	fill_in_transcriptions_in_wavfile_list(R, TranscriptionsListIn, RootDir, R1, OldNext-OldOut, NewNext-NewOut).

fill_in_transcriptions_in_wavfile_list_single(Wavfile0, TranscriptionsListIn, RootDir, OutAtom, OldIn-OldOut, NewIn-NewOut) :-
	format_to_atom('~w/~w', [RootDir, Wavfile0], Wavfile1),
	absolute_file_name(Wavfile1, Wavfile),
	(   ( member(transcription(Wavfile, Transcription), TranscriptionsListIn), Transcription \== [] ) ->
	    join_with_spaces([Wavfile1 | Transcription], OutAtom),
	    OldOut is OldIn + 1,
	    NewOut = NewIn
	;
	    get_transcription_from_wavfile(Wavfile, WavfileTranscription) ->
	    join_with_spaces([Wavfile1 | WavfileTranscription], OutAtom),
	    OldOut = OldIn,
	    NewOut is NewIn + 1
	;
	    otherwise ->
	    OutAtom = Wavfile1,
	    OldOut = OldIn,
	    NewOut is NewIn + 1
	),
	!.

	    