
:- module(backend_server_client,
	  [connect_to_backend_server_over_socket/1,
	   connect_to_backend_server_over_socket/2,
	   close_backend_server_socket/0,
	   check_for_reply_from_backend_server/1,
	   send_request_to_backend_server/1
	  ]
	 ).

%======================================================================

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(sockets)).

%======================================================================

connect_to_backend_server_over_socket(Port) :-
	current_host(Host),
	connect_to_backend_server_over_socket(Port, Host).

%======================================================================

connect_to_backend_server_over_socket(Port, Host) :-
	socket('AF_INET', Socket),
	socket_connect(Socket, 'AF_INET'(Host, Port), Stream),
	store_backend_server_socket_and_stream(Socket, Stream),
	format('~N~n--- Connected to backend server on port ~d~n', [Port]),
	!.

connect_to_backend_server_over_socket(Port) :-
	current_host(Host),
	socket('AF_INET', Socket),
	socket_connect(Socket, 'AF_INET'(Host, Port), Stream),
	store_backend_server_socket_and_stream(Socket, Stream),
	format('~N~n--- Connected to backend server on port ~d~n', [Port]),
	!.
connect_to_backend_server_over_socket(Port) :-
	format('~N~n*** Error: Unable to connect to backend server on port ~d~n', [Port]),
	!.
/*
close_backend_server_socket :-
	get_backend_server_socket_and_stream(Socket, _Stream),
	format('~N~n--- Closing down connection to backend server...~n', []),
	send_request_to_backend_server(client_shutdown, _ReplyTerm),
	socket_close(Socket),
	format('~N~n--- Closed down connection to backend server.~n', []).
*/
close_backend_server_socket :-
	get_backend_server_socket_and_stream(Socket, _Stream),
	format('~N~n--- Closing down connection to backend server...~n', []),
	send_request_to_backend_server(client_shutdown),
	socket_close(Socket),
	format('~N~n--- Closed down connection to backend server.~n', []).

send_request_to_backend_server(Action) :-
	get_backend_server_socket_and_stream(_Socket, Stream),
	format('~N~n--- Sending message to backend server: ~w~n', [Action]),
	format(Stream, '~q.~n', [Action]),
	flush_output(Stream),
	!.
send_request_to_backend_server(Action) :-
	format('~N~n*** Error: Couldn\'t send message to backend server: ~w~n', [Action]),
	fail.

check_for_reply_from_backend_server(ReplyTerm) :-
	get_backend_server_socket_and_stream(_Socket, Stream),
	poll_for_waiting_reply(Stream),
	read(Stream, ReplyTerm),
	!.

poll_for_waiting_reply(Stream) :-	
	socket_select([], [], 0:100, [Stream], StreamsWithInput),
	StreamsWithInput \== [],
	!.

%======================================================================

:- dynamic backend_server_socket_and_stream/2.

store_backend_server_socket_and_stream(Socket, Stream) :-
	retractall(backend_server_socket_and_stream(_, _)),
	assertz(backend_server_stream(Socket, Stream)),
	!.

get_backend_server_socket_and_stream(Socket, Stream) :-
	backend_server_stream(Socket, Stream),
	!.
get_backend_server_stream(_Socket, _Stream) :-
	format('~N~n*** Error: No backend socket and stream defined~n', []),
	fail.

