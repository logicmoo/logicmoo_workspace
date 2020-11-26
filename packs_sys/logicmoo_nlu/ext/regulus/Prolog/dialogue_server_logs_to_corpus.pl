
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(dialogue_server_logs_to_corpus,
	  [dialogue_server_dir_to_corpus/2,
	   dialogue_server_dir_to_corpus/3,
	   dialogue_server_dir_to_corpus/4
	  ]).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).

%------------------------------------------------------------------------------------

dialogue_server_dir_to_corpus(Dir, CorpusFile) :-
	dialogue_server_dir_to_corpus(Dir, CorpusFile, '*none*', text).

dialogue_server_dir_to_corpus(Dir, CorpusFile, Mode) :-
	dialogue_server_dir_to_corpus(Dir, CorpusFile, '*none*', Mode).

dialogue_server_dir_to_corpus(Dir, CorpusFile, CorpusFileForLastDir, Mode) :-
	get_dialogue_server_files_in_directory(Dir, Files),
	dialogue_server_files_to_corpus(Files, CorpusFile, Mode),
	(   ( last(Files, LastFile), CorpusFileForLastDir \== '*none*' ) ->
	    dialogue_server_files_to_corpus([LastFile], CorpusFileForLastDir, Mode)
	;
	    true
	),
	!.
dialogue_server_dir_to_corpus(Dir, CorpusFile, CorpusFileForLastDir, Mode) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [dialogue_server_dir_to_corpus(Dir, CorpusFile, CorpusFileForLastDir, Mode)]),
	fail.

dialogue_server_files_to_corpus(Files, CorpusFile, Mode) :-
	safe_absolute_file_name(CorpusFile, AbsCorpusFile),
	open(AbsCorpusFile, write, S),
	files_to_corpus_stream(Files, S, Mode, 0-N),
	close(S),
	format('~N--- Written ~w corpus file (~d entries) ~w~n', [Mode, N, AbsCorpusFile]),
	!.

%------------------------------------------------------------------------------------

get_dialogue_server_files_in_directory(Dir, Files) :-
	(   ( safe_absolute_file_name(Dir, AbsDir), safe_directory_exists(AbsDir) ) ->
	    safe_directory_files(AbsDir, AllFiles0),
	    sort(AllFiles0, AllFiles)
	;
	    format2error('~N*** Error: ~w is not a directory~n', [Dir]),
	    fail
	),
	dialogue_server_files_in_list(AllFiles, AbsDir, Files),
	!.
get_dialogue_server_files_in_directory(Dir, Files) :-
	format2error('~N*** Error: bad call: ~w~n', [get_dialogue_server_files_in_directory(Dir, Files)]),
	fail.
			 
dialogue_server_files_in_list([], _Dir, []).
dialogue_server_files_in_list([F | R], Dir, [F1 | R1]) :-
	dialogue_server_file(F, Dir, F1),
	!,
	dialogue_server_files_in_list(R, Dir, R1).
dialogue_server_files_in_list([_F | R], Dir, R1) :-
	!,
	dialogue_server_files_in_list(R, Dir, R1).

dialogue_server_file(File, Dir, AbsFile) :-
	atom_codes(File, Chars),
	safe_prefix("dialogue_server_log", Chars),
	safe_suffix(".pl", Chars),
	format_to_atom('~w/~w', [Dir, File], AbsFile),
	!.

%------------------------------------------------------------------------------------

files_to_corpus_stream([], _S, _Mode, N-N) :-
	!.
files_to_corpus_stream([F | R], S, Mode, In-Out) :-
	file_to_corpus_stream(F, S, Mode, In-Next),
	!,
	files_to_corpus_stream(R, S, Mode, Next-Out).

file_to_corpus_stream(File, S, Mode, In-Out) :-
	prolog_file_to_list(File, List),
	length(List, N),
	format('~N--- Extracting from ~w (~d elements)~n', [File, N]),
	format(S, '~N~n%-----------------------------------------------~N%Taken from ~w~n', [File]),
	format(S, '~N~ninit_dialogue.~n', []),
	list_to_corpus_stream(List, S, Mode, In-Out),
	!.

list_to_corpus_stream([], _S, _Mode, In-In).
list_to_corpus_stream([F | R], S, Mode, In-Out) :-
	element_to_corpus_stream(F, S, Mode, In-Next),
	!,
	list_to_corpus_stream(R, S, Mode, Next-Out).

element_to_corpus_stream(Element, S, _Mode, In-Out) :-
	Element = dialogue_event(_Timestamp, received(action(process_non_speech_input(Content)))),
	\+ dubious_non_speech_input(Content),
	format(S, '~N~n~q.~n', [sent(Content)]),
	Out is In + 1,
	!.
element_to_corpus_stream(Element, S, text, In-Out) :-
	(   Element = dialogue_event(_Timestamp, received(action(process_nbest_list(Wavfile, Hyps))))
	;
	    Element = dialogue_event(_Timestamp, received(action(process_nbest_list(Hyps)))),
	    Wavfile = unknown_wavfile
	),
	(   get_transcription_from_wavfile(Wavfile, TranscriptionWords) ->
	    join_with_spaces(TranscriptionWords, TranscriptionAtom),
	    format(S, '~N~n~q.~n', [sent(TranscriptionAtom)]),
	    Out is In + 1
	;
	    Hyps = [nbest_hyp(_Conf, TopHyp) | _RestHyps] ->
	    format_to_atom('Guessed from ~w', [Wavfile], Comment),
	    format(S, '~N~n~q.', [comment(Comment)]),
	    format(S, '~N~q.~n', [sent(TopHyp)]),
	    Out is In + 1
	;
	    otherwise ->
	    In = Out
	),
	!.
element_to_corpus_stream(Element, S, speech, In-Out) :-
	(   Element = dialogue_event(_Timestamp, received(action(process_nbest_list(Wavfile, Hyps))))
	;
	    Element = dialogue_event(_Timestamp, received(action(process_nbest_list(Hyps)))),
	    Wavfile = unknown_wavfile
	),
	(   get_transcription_from_wavfile(Wavfile, TranscriptionWords) ->
	    join_with_spaces(TranscriptionWords, TranscriptionAtom),
	    format_to_atom('"~w"', [TranscriptionAtom], Comment),
	    format(S, '~N~n~q.', [comment(Comment)]),
	    format(S, '~N~q.~n', [wavfile(Wavfile)]),
	    Out is In + 1
	;
	    Hyps = [nbest_hyp(_Conf, TopHyp) | _RestHyps] ->
	    format_to_atom('"~w" (rec result)', [TopHyp], Comment),
	    format(S, '~N~n~q.', [comment(Comment)]),
	    format(S, '~N~q.~n', [wavfile(Wavfile)]),
	    Out is In + 1
	;
	    otherwise ->
	    In = Out
	),
	!.
element_to_corpus_stream(Element, S, text, In-Out) :-
	(   Element = dialogue_event(_Timestamp, received(action(process_rec_string(Wavfile, TopHyp))))
	;
	    Element = dialogue_event(_Timestamp, received(action(process_rec_string(TopHyp)))),
	    Wavfile = unknown_wavfile
	),
	(   get_transcription_from_wavfile(Wavfile, TranscriptionWords) ->
	    join_with_spaces(TranscriptionWords, TranscriptionAtom),
	    format(S, '~N~n~q.~n', [sent(TranscriptionAtom)]),
	    Out is In + 1
	;
	    format_to_atom('Guessed from ~w', [Wavfile], Comment),
	    format(S, '~N~n~q.', [comment(Comment)]),
	    format(S, '~N~q.~n', [sent(TopHyp)]),
	    Out is In + 1
	;
	    otherwise ->
	    In = Out
	),
	!.
element_to_corpus_stream(Element, S, speech, In-Out) :-
	(   Element = dialogue_event(_Timestamp, received(action(process_rec_string(Wavfile, TopHyp))))
	;
	    Element = dialogue_event(_Timestamp, received(action(process_rec_string(TopHyp)))),
	    Wavfile = unknown_wavfile
	),
	(   get_transcription_from_wavfile(Wavfile, TranscriptionWords) ->
	    join_with_spaces(TranscriptionWords, TranscriptionAtom),
	    format_to_atom('"~w"', [TranscriptionAtom], Comment),
	    format(S, '~N~n~q.', [comment(Comment)]),
	    format(S, '~N~q.~n', [wavfile(Wavfile)]),
	    Out is In + 1
	;
	    format_to_atom('"~w" (rec result)', [TopHyp], Comment),
	    format(S, '~N~n~q.', [comment(Comment)]),
	    format(S, '~N~q.~n', [wavfile(Wavfile)]),
	    Out is In + 1
	;
	    otherwise ->
	    In = Out
	),
	!.
element_to_corpus_stream(_Element, _S, _Mode, In-In).

%dubious_non_speech_input('LF: ok') :-
%	!.
%dubious_non_speech_input('LF: end_of_file') :-
%	!.
%dubious_non_speech_input('LF: failure') :-
%	!.
dubious_non_speech_input(Atom) :-
	atom_codes(Atom, Chars),
	dubious_starting_chars_for_non_speech_input(Start),
	safe_prefix(Start, Chars), 
	!.

dubious_starting_chars_for_non_speech_input("LF: recognition_succeeded").
dubious_starting_chars_for_non_speech_input("LF: recognition_failed").

