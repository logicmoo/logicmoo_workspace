
:- use_module('$REGULUS/PrologLib/utilities').
:- use_module('$REGULUS/PrologLib/batchrec_tools').

:- use_module(library(lists)).
:- use_module(library(system)).

update_transcriptions(TranscriptionsFile, ListingsFile, NewTranscriptionsFile) :-
	absolute_file_name(TranscriptionsFile, AbsTranscriptionsFile),
	absolute_file_name(ListingsFile, AbsListingsFile),
	absolute_file_name(NewTranscriptionsFile, AbsNewTranscriptionsFile),
	
	read_file_to_atom_list(AbsListingsFile, WavfilesList),
	parse_transcriptions_file_to_list(AbsTranscriptionsFile, TranscriptionsListIn),

	length(WavfilesList, NWavfiles),
	length(TranscriptionsListIn, NTranscriptions),

	format('~N--- Read wavfile listings file (~d elements) ~w~n', [NWavfiles, AbsListingsFile]),
	format('~N--- Read transcriptions file (~d elements) ~w~n', [NTranscriptions, AbsTranscriptionsFile]),

	fill_in_transcriptions_in_wavfile_list(WavfilesList, TranscriptionsListIn, TranscriptionsListOut, 0-NOld, 0-NNew),
	
	write_atom_list_to_file(TranscriptionsListOut, AbsNewTranscriptionsFile),
	format('~N--- Written transcriptions file (~d old elements, ~d new elements) ~w~n', [NOld, NNew, AbsNewTranscriptionsFile]),
	!.

fill_in_transcriptions_in_wavfile_list([], _TranscriptionsListIn, [], OldIn-OldIn, NewIn-NewIn).
fill_in_transcriptions_in_wavfile_list([F | R], TranscriptionsListIn, [F1 | R1], OldIn-OldOut, NewIn-NewOut) :-
	fill_in_transcriptions_in_wavfile_list_single(F, TranscriptionsListIn, F1, OldIn-OldNext, NewIn-NewNext),
	!,
	fill_in_transcriptions_in_wavfile_list(R, TranscriptionsListIn, R1, OldNext-OldOut, NewNext-NewOut).

fill_in_transcriptions_in_wavfile_list_single(Wavfile0, TranscriptionsListIn, OutAtom, OldIn-OldOut, NewIn-NewOut) :-
	format_to_atom('$REGULUS/Examples/Calendar/corpora/speech/~w', [Wavfile0], Wavfile1),
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

get_transcription_from_wavfile(Wavfile, WavfileTranscription) :-
	file_exists(Wavfile),
	open(Wavfile, read, S),
	get_transcription_from_wavfile_stream(S, WavfileTranscription),
	close(S),
	!,
	WavfileTranscription \== '*no_transcription*'.

/*
NIST_1A
   1024
channel_count -i 1
sample_count -i 18240
sample_rate -i 8000
sample_n_bytes -i 2
sample_sig_bits -i 16
sample_coding -s3 pcm
sample_byte_format -s2 01
recording_date -s11 09-Sep-2007
recording_time -s11 11:04:18.00
abs_safe_start -i 1760
rel_safe_start -i 0
rel_actual_start -i 4800
rel_actual_end -i 12880
rel_safe_end -i 18239
transcription -s22 is elisabeth attending
end_head
*/

get_transcription_from_wavfile_stream(S, WavfileTranscription) :-
	read_line(S, Line),
	(   Line = end_of_file ->
	    WavfileTranscription = '*no_transcription*'
	;
	    otherwise ->
	    split_string_into_words(Line, Words),
	    !,
	    (   Words = [end_head] ->
		WavfileTranscription = '*no_transcription*'
	    ;
		Words = [transcription, _ | WavfileTranscription] ->
		true
	    ;
		get_transcription_from_wavfile_stream(S, WavfileTranscription)
	    )
	).

	
	    