
:- module(open_mic_client,
	  [start_open_mic_server/1,
	   shut_down_open_mic_server/0,
	   
	   check_for_open_mic_recognition_result/1,
	   
	   regulus_sockettalk_say_file/1,
	   regulus_sockettalk_say_tts/1,
	   regulus_sockettalk_say_list/1,
	   regulus_sockettalk_set_parameter/2,
	   regulus_sockettalk_get_parameter/2,
	   regulus_sockettalk_recognise_file/2
	  ]
	 ).
	  
/*

Open mic client library.

Predicates with names starting "regulus_sockettalk..." have the same
functionality as those defined in RegulusSpeechServer/Prolog/regulus_sockettalk.pl.
The corresponding call is executed on the open mic server.

*/

%======================================================================

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(sockets)).

%======================================================================

start_open_mic_server(Port) :-
	current_host(Host),
	socket('AF_INET', Socket),
	socket_connect(Socket, 'AF_INET'(Host, Port), Stream),
	store_open_mic_server_socket_and_stream(Socket, Stream),
	format('~N~n--- Started open mic server on port ~d~n', [Port]),
	!.
connect_to_open_mic_server(Port, _RegulusPort, _Package, _NuanceParams) :-
	format('~N~n*** Error: Unable to connect to open mic server on port ~d~n', [Port]),
	!.

shut_down_open_mic_server :-
	get_open_mic_server_socket_and_stream(Socket, _Stream),
	format('~N~n--- Closing down connection to open_mic server...~n', []),
	send_request_to_server(end, _ReplyTerm),
	socket_close(Socket),
	format('~N~n--- Closed down connection to open mic server.~n', []).

%======================================================================

check_for_open_mic_recognition_result(RecognitionResult) :-
	get_open_mic_server_socket_and_stream(_Socket, Stream),
	poll_for_waiting_reply(Stream),
	read(Stream, RecognitionResult),
	RecognitionResult \== end_of_file,
	!.

% Wait 100 microseconds for result
poll_for_waiting_reply(Stream) :-	
	socket_select([], [], 0:100, [Stream], StreamsWithInput),
	StreamsWithInput \== [],
	!.

%======================================================================

regulus_sockettalk_say_file(File) :-
	send_request_to_server_no_reply(regulus_sockettalk_say_file(File)),
	!.
	
regulus_sockettalk_say_tts(String) :-
	send_request_to_server_no_reply(regulus_sockettalk_say_tts(String)),
	!.

regulus_sockettalk_say_list(ItemList) :-
	send_request_to_server_no_reply(regulus_sockettalk_say_list(ItemList)),
	!.

regulus_sockettalk_set_parameter(ParamName, Value) :-
	send_request_to_server_no_reply(regulus_sockettalk_set_parameter(ParamName, Value)),
	!.

regulus_sockettalk_get_parameter(ParamName, Value) :-
	send_request_to_server(regulus_sockettalk_get_parameter(ParamName), ReplyTerm),
	ReplyTerm \== '*error*',
	Value = ReplyTerm.

regulus_sockettalk_recognise_file(Wavfile, GrammarName, Result) :-
	send_request_to_server(regulus_sockettalk_recognise_file(Wavfile, GrammarName), ReplyTerm),
	ReplyTerm \== '*error*',
	Result = ReplyTerm.

%======================================================================

send_request_to_server_no_reply(Message) :-
	get_open_mic_server_socket_and_stream(_Socket, Stream),
	format(Stream, '~q.~n', [Message]),
	flush_output(Stream),
	!.

send_request_to_server(Message, ReplyTerm) :-
	get_open_mic_server_socket_and_stream(_Socket, Stream),
	format(Stream, '~q.~n', [Message]),
	flush_output(Stream),
	read(Stream, ReplyTerm),
	ReplyTerm \== end_of_file,
	!.

%======================================================================

check_reply_term_is_ok(Call, ReplyTerm) :-
	(   ReplyTerm = ok ->
	    true
	;
	    otherwise ->
	    format('~N*** Error: bad call: ~w~n', [Call]),
	    fail
	).

%======================================================================

:- dynamic open_mic_server_socket_and_stream/2.

store_open_mic_server_socket_and_stream(Socket, Stream) :-
	retractall(open_mic_server_socket_and_stream(_, _)),
	assertz(open_mic_server_stream(Socket, Stream)),
	!.

get_open_mic_server_socket_and_stream(Socket, Stream) :-
	open_mic_server_stream(Socket, Stream),
	!.
get_open_mic_server_stream(_Socket, _Stream) :-
	format('~N~n*** Error: No open mic socket and stream defined~n', []),
	fail.

%======================================================================




	