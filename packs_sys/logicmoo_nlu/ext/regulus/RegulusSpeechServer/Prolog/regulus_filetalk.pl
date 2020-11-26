
:- module(regulus_filetalk,
	[regulus_filetalk_init/1,
	 regulus_filetalk_clean_up/0,
	 regulus_filetalk_say_file/1,
	 regulus_filetalk_say_number/1,
	 regulus_filetalk_say_tts/1,
	 regulus_filetalk_set_output_volume/1,
	 regulus_filetalk_post_recognition_request/1,
	 regulus_filetalk_post_record_request/1,
	 wait_for_regulus_server_response/1,
	 wait_for_regulus_server_response_or_time/2]
    ).

%----------------------------------------------------------------------

:- use_module(library(system)).
:- use_module(library(lists)).

:- use_module('$REGULUS/PrologLib/utilities').

%----------------------------------------------------------------------
%
%   regulus_filetalk_init(ConfigFile)
%
% Initialise Regulus filetalk system; call before invoking any of the 
% other calls.
%
% Sample call:
%
% regulus_filetalk_init
%
%----------------------------------------------------------------------
%
%   regulus_filetalk_clean_up
%
% Causes all queued prompts to be played.
%
% Sample call:
%
% regulus_filetalk_clean_up
%
%----------------------------------------------------------------------
%
%   regulus_filetalk_say_file(+File)
%
% where 
%
%   File is an atom whose print name is the name of a .wav file in the current
%   Regulus server prompt directory
%
% The wavfile is appended to the prompt queue
%
% Sample call:
%
% regulus_filetalk_say_file(hello)
%
%----------------------------------------------------------------------
%
%   regulus_filetalk_say_number(+Number)
%
% where 
%
%   Number is an integer
%
% A request to say Number using .wav files is appended to the prompt queue
%
% Sample call:
%
% regulus_filetalk_say_number(50)
%
%----------------------------------------------------------------------
%
%   regulus_filetalk_say_tts(+String)
%
% where 
%
%   String is a Prolog string
%
% A request to say String using TTS is appended to the prompt queue
%
% Sample call:
%
% regulus_filetalk_say_number(50)
%
%----------------------------------------------------------------------
%
%   regulus_filetalk_set_output_volume(+Number)
%
% where 
%
%   Number is an integer between 0 and 255 inclusive
%
% A request to set the output volume to Number is sent to the server
%
% Sample call:
%
% regulus_filetalk_set_output_volume(255)
%
%----------------------------------------------------------------------
%
%   regulus_filetalk_post_recognition_request(+Grammar)
%
% where 
%
%   Grammar is a Prolog string representing a grammar name in the current package
%
% A request to do recognition using Grammar is sent to the server.
% The response has to be fetched using wait_for_regulus_server_response/1.
%
% Sample call:
% 
% regulus_filetalk_post_recognition_request(".Utterance")
%
%----------------------------------------------------------------------
%
%   regulus_filetalk_post_record_request(+File)
%
% where 
%
%   File is a Prolog string representing a file name
%
% A request to record speech onto the file File is sent to the server.
% A confirmation is sent on completion, which should be fetched using wait_for_regulus_server_response/1.
%
% Sample call:
% 
% regulus_filetalk_post_record_request("C:\Temp\recorded_file1.wav")
%
%----------------------------------------------------------------------
%
%   wait_for_regulus_server_response(-Response)
%
% The process waits for a recognition response from the server, following
% a call to regulus_filetalk_post_recognition_request/1.
%
% The response can be one of the following:
%
%   recognition_succeeded(RecString, Result) where 
%        - RecString is the recognition string, expressed as a Prolog atom
%        - Result is a Regulus semantic expression, expressed as a Prolog term.
%
%   file_recorded(File), where 
%        - File is the name of the file recorded, expressed as a Prolog string.
%
%   recognition_failed(FailureType) where FailureType is one of the following atoms:
%        - 'UTT_SESSION_ABORTED'
%        - 'UTT_REJECT'
%        - 'UTT_BEGIN_SPEECH_TIMEOUT'
%        - 'UTT_END_SPEECH_TIMEOUT'
%        - 'UTT_END_RECOGNITION_TIMEOUT'
%        - 'UTT_ERROR'
%        - 'UNKNOWN_STATUS'
%
%   The meanings of the failure types are defined in the Nuance documentation.
%
% Sample call:
%
% wait_for_regulus_server_response(X)
%
%----------------------------------------------------------------------
%
%   wait_for_regulus_server_response_or_time(+Time, -Response)
%
% where
%
%   Time is a time in standard SICStus format, i.e. datime(Year, Month, Day, Hour, Minute, Second)
%
% Like wait_for_regulus_server_response/1 above, except that the process
% also waits for the time Time to be reached or passed. If Time is reached before
% the next recognition response is received, the call returns with Response
% instantiated to 
%
%    time_reached(Time)
%
% If the next recognition response is received first, the response is as
% for wait_for_regulus_server_response/1.
%
%----------------------------------------------------------------------

regulus_server_process_name('regulusserver.e').

:- dynamic regulus_client_file_pointer_file/1.
:- dynamic regulus_server_file_pointer_file/1.
:- dynamic regulus_client_file_prefix/1.
:- dynamic regulus_server_file_prefix/1.

:- dynamic client_file_serial_number/1, server_file_serial_number/1.

%----------------------------------------------------------------------

regulus_filetalk_init(ConfigFile) :-
	init_from_config_file(ConfigFile),
	%wait_for_regulus_server_to_start,
	set_client_file_pointer_from_file,
	set_server_file_pointer_from_file.

wait_for_regulus_server_to_start :-
	format('~NWaiting for Regulus server to start... ~n', []), flush_output(user),
	regulus_server_process_name(ProcessName),
	wait_for_regulus_server_to_start1(ProcessName),
	format('~NRegulus server started OK~n', []).

wait_for_regulus_server_to_start1(ProcessName) :-
	get_pulist(PUList),
	member(process_and_pid(ProcessName, _PID), PUList),
	!.
wait_for_regulus_server_to_start1(ProcessName) :-
	sleep(1),
	!,
	wait_for_regulus_server_to_start1(ProcessName).

%----------------------------------------------------------------------

init_from_config_file(ConfigFile) :-
	absolute_file_name(ConfigFile, AbsoluteConfigFile),
	format('~N~nInitialising Regulus filetalk from file ~w... ~n~n', [AbsoluteConfigFile]),

	zero_all_filetalk_config_predicates,
	read_file_to_atom_list(AbsoluteConfigFile, ConfigFileAtoms),
	init_from_atom_list(ConfigFileAtoms),
	format('~N~nFiletalk initialised successfully~n~n', []),
	!.
init_from_config_file(_ConfigFile) :-
	format('~NFiletalk initialisation failed~n~n', []),
	fail.

zero_all_filetalk_config_predicates :-
	retractall(regulus_client_file_pointer_file(_)),
	retractall(regulus_server_file_pointer_file(_)),
	retractall(regulus_client_file_prefix(_)),
	retractall(regulus_server_file_prefix(_)).	

init_from_atom_list([]).
init_from_atom_list([F | R]) :-
	init_from_atom(F),
	init_from_atom_list(R).

init_from_atom(Atom) :-
	split_atom_into_words(Atom, Words),
	Words = [Key, Value],
	init_from_key_and_value(Key, Value),
	format('~N~w = "~w"~n', [Key, Value]),
	!.
init_from_atom(Atom) :-
	format('~N*** Error: unable to interpret line "~w" in Regulus filetalk config file~n', [Atom]),
	fail.

init_from_key_and_value(client_file_pointer_file, Atom) :-
	asserta(regulus_client_file_pointer_file(Atom)),
	!.
init_from_key_and_value(server_file_pointer_file, Atom) :-
	asserta(regulus_server_file_pointer_file(Atom)),
	!.
init_from_key_and_value(client_file_prefix, Atom) :-
	atom_chars(Atom, Chars),
	asserta(regulus_client_file_prefix(Chars)),
	!.
init_from_key_and_value(server_file_prefix, Atom) :-
	atom_chars(Atom, Chars),
	asserta(regulus_server_file_prefix(Chars)),
	!.

%----------------------------------------------------------------------

set_client_file_pointer_from_file :-
	retractall(client_file_serial_number(_)),
	regulus_client_file_pointer_file(File),
	safe_open(File, read, S, 20),
	read(S, T),
	T = next_client_file_serial_number(N),
	assert(client_file_serial_number(N)),
	current_client_file(CurrentClientFile),
	format('~NFirst client file written will be ~w~n', [CurrentClientFile]),
	!.
set_client_file_pointer_from_file :-
	format('~NError: bad call: ~w~n', [set_client_file_pointer_from_file]),
	fail.

set_server_file_pointer_from_file :-
	retractall(server_file_serial_number(_)),
	regulus_server_file_pointer_file(File),
	safe_open(File, read, S, 20),
	read(S, T),
	T = next_server_file_serial_number(N),
	assert(server_file_serial_number(N)),
	current_server_file(CurrentServerFile),
	format('~NFirst server file read will be ~w~n', [CurrentServerFile]),
	!.
set_server_file_pointer_from_file :-
	format('~NError: bad call: ~w~n', [set_server_file_pointer_from_file]),
	fail.

%----------------------------------------------------------------------

regulus_filetalk_clean_up :-
	current_client_file(ClientFile),
	open(ClientFile, write, S),
	format(S, 'CLEAN_UP\r~n', []),
	close(S),
	mark_current_client_file_as_processed,
	!.
regulus_filetalk_clean_up :-
	format('~NError: bad call: ~w~n', [regulus_filetalk_clean_up]),
	fail.

%----------------------------------------------------------------------

regulus_filetalk_say_file(PromptFile) :-
	current_client_file(ClientFile),
	open(ClientFile, write, S),
	format(S, 'SAY_FILE\r~n', []),
	format(S, '~w\r~n', [PromptFile]),
	close(S),
	mark_current_client_file_as_processed,
	!.
regulus_filetalk_say_file(PromptFile) :-
	format('~NError: bad call: ~w~n', [regulus_filetalk_say_file(PromptFile)]),
	fail.

regulus_filetalk_say_tts(String) :-
	current_client_file(ClientFile),
	open(ClientFile, write, S),
	format(S, 'SAY_TTS\r~n', []),
	format(S, '~s\r~n', [String]),
	close(S),
	mark_current_client_file_as_processed,
	!.
regulus_filetalk_say_tts(String) :-
	format('~NError: bad call: ~w~n', [regulus_filetalk_say_tts(String)]),
	fail.

regulus_filetalk_say_number(N) :-
	current_client_file(ClientFile),
	open(ClientFile, write, S),
	format(S, 'SAY_NUMBER\r~n', []),
	format(S, '~d\r~n', [N]),
	close(S),
	mark_current_client_file_as_processed,
	!.
regulus_filetalk_say_number(N) :-
	format('~NError: bad call: ~w~n', [regulus_filetalk_say_number(N)]),
	fail.

regulus_filetalk_set_output_volume(N) :-
	integer(N),
	0 =< N,
	N =< 255,
	current_client_file(ClientFile),
	open(ClientFile, write, S),
	format(S, 'SET_OUTPUT_VOLUME\r~n', []),
	format(S, '~d\r~n', [N]),
	close(S),
	mark_current_client_file_as_processed,
	!.
regulus_filetalk_set_output_volume(N) :-
	format('~NError: bad call: ~w~n', [regulus_filetalk_set_output_volume(N)]),
	fail.

regulus_filetalk_post_recognition_request(Grammar) :-
	current_client_file(ClientFile),
	open(ClientFile, write, S),
	format(S, 'RECOGNISE\r~n', []),
	format(S, '~w\r~n', [Grammar]),
	close(S),
	mark_current_client_file_as_processed,
	!.
regulus_filetalk_post_recognition_request(Grammar) :-
	format('~NError: bad call: ~w~n', [regulus_filetalk_post_recognition_request(Grammar)]),
	fail.

regulus_filetalk_post_record_request(File) :-
	current_client_file(ClientFile),
	open(ClientFile, write, S),
	format(S, 'RECORD\r~n', []),
	format(S, '~w\r~n', [File]),
	close(S),
	mark_current_client_file_as_processed,
	!.
regulus_filetalk_post_record_request(File) :-
	format('~NError: bad call: ~w~n', [regulus_filetalk_post_record_request(File)]),
	fail.	

%----------------------------------------------------------------------

wait_for_regulus_server_response(Response) :-
	current_server_file(File),
	wait_for_regulus_server_response1(File, Response),
	mark_current_server_file_as_processed.

wait_for_regulus_server_response1(File, Response) :-
	file_exists(File),
	!,
	open(File, read, S),
	read(S, Response),
	close(S).
wait_for_regulus_server_response1(File, Response) :-
	sleep(1),
	!,
	wait_for_regulus_server_response1(File, Response).

%----------------------------------------------------------------------

wait_for_regulus_server_response_or_time(Time, Response) :-
	current_server_file(File),
	wait_for_regulus_server_response_or_time1(File, Time, Response).

wait_for_regulus_server_response_or_time1(_File, Time, Response) :-
	datime(CurrentTime),
	(   Time = CurrentTime ;
	    earlier_datime(Time, CurrentTime)
	),
	!,
	Response = time_reached(Time).
wait_for_regulus_server_response_or_time1(File, _Time, Response) :-
	file_exists(File),
	!,
	open(File, read, S),
	read(S, Response),
	close(S),
	mark_current_server_file_as_processed.
wait_for_regulus_server_response_or_time1(File, Time, Response) :-
	sleep(1),
	!,
	wait_for_regulus_server_response_or_time1(File, Time, Response).

%----------------------------------------------------------------------

current_client_file(File) :-
	regulus_client_file_prefix(PrefixString),
	client_file_serial_number(SerialNumber),
	number_to_suffix_string(SerialNumber, SuffixString),
	append(PrefixString, SuffixString, Chars),
	atom_chars(File, Chars),
	!.
current_client_file(_File) :-
	format('~NError: call to current_client_file/1 failed~n', []),
	fail.

current_server_file(File) :-
	regulus_server_file_prefix(PrefixString),
	server_file_serial_number(SerialNumber),
	number_to_suffix_string(SerialNumber, SuffixString),
	append(PrefixString, SuffixString, Chars),
	atom_chars(File, Chars),
	!.
current_server_file(_File) :-
	format('~NError: call to current_server_file/1 failed~n', []),
	fail.

mark_current_client_file_as_processed :-
	client_file_serial_number(N),
	retractall(client_file_serial_number(_)),
	N1 is N + 1,
	asserta(client_file_serial_number(N1)),
	!.
mark_current_client_file_as_processed :-
	format('~NError: call to mark_current_client_file_as_processed failed~n', []),
	fail.

mark_current_server_file_as_processed :-
	server_file_serial_number(N),
	retractall(server_file_serial_number(_)),
	N1 is N + 1,
	asserta(server_file_serial_number(N1)),
	!.
mark_current_server_file_as_processed :-
	format('~NError: call to mark_current_server_file_as_processed failed~n', []),
	fail.

%----------------------------------------------------------------------

number_to_suffix_string(N, _) :-
	N > 9999,
	!,
	format('~NError: argument to number_to_suffix_string is over 9999~n', []),
	fail.
number_to_suffix_string(N, String) :-
	A is 0'0 + ( N // 1000 ),
	B is 0'0 + ( N mod 1000) // 100,
	C is 0'0 + ( N mod 100) // 10,
	D is 0'0 + ( N mod 10),
	append([A, B, C, D], ".txt", String).

%----------------------------------------------------------------------

safe_open(File, Mode, S, NRetries) :-
	on_exception(
	_Exception, 
	open(File, Mode, S),
	retry_safe_open(File, Mode, S, NRetries)
    ).

retry_safe_open(File, Mode, S, NRetries) :-
	format('~nUnable to open file ~w, retrying ', [File]),
	(   retry_safe_open1(File, Mode, S, NRetries) ->
	    format(' OK~n', []) ;
          format('~nGiving up, sorry~n', []),
	    fail
	).


retry_safe_open1(File, Mode, S, NRetries) :-
	sleep(1),
	on_exception(
	_Exception, 
	open(File, Mode, S),
	(   NRetries =< 0 ->

	    fail ;

	    NRetries1 is NRetries - 1,
	    format('.', []), flush_output(user),
	    retry_safe_open1(File, Mode, S, NRetries1)
	)
    ).












