
:- use_module('$REGULUS/RegulusSpeechServer/Prolog/regulus_sockettalk').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(system)).
:- use_module(library(sockets)).

%----------------------------------------------------------------------------------

% OpenMicServerPort = 4547

server :-
	server(4547,
	       4549,
	       '$REGULUS/Examples/Toy1/Generated/recogniser',
	       'audio.OutputVolume=200').

%----------------------------------------------------------------------------------

/*

run_server(+ServerPort, +RegserverPort, Package, NuanceParams)

Start an Open Mic server loop, communicating with the client on ServerPort,
and starting a Regserver on RegserverPort, using Package and NuanceParams.

*/

server(ServerPort, RegulusPort, Package, NuanceParameters) :-
	% Open connection to client
	initialise_server_socket(ServerPort),
	% Get the package name
	absolute_file_name(Package, AbsPackage),
	% Switch on tracing for messages to and from RegServer
	regulus_sockettalk_debug,
	% Start Regserver
	regulus_sockettalk_init(RegulusPort, AbsPackage, NuanceParameters),
	!,
	% Enter main loop
	loop.

% Main loop
loop :-
	% Check to see if the client has sent a message
	check_for_client_message(Message),

	(   Message = end ->

	    % Message = 'end': close down the connection to the client
	    close_server_socket,
	    % and close down the Regserver
	    regulus_sockettalk_exit_server
	;
	    Message = '*no_message*' ->
	    % No message: issue a recognise command and wait for the reply
	    regulus_sockettalk_recognise('.MAIN', Recognised),
	    format('~N~nRecognised: ~w~n~n', [Recognised]),
	    % If anything got recognised, send it back to the client
	    (   Recognised = recognition_failed(_) ->
		format('~N~n--- Nothing recognised~n', [])
	    ;
		send_client_message(Recognised),
		% Sleep a little to wait for possible client message (not an elegant solution...)
		sleep(0.1)
	    ),
	    !,
	    % and carry on looping
	    loop
	;
	    otherwise ->

	    % Normal client request: process it
	    process_non_end_client_message(Message),
	    % and carry on looping
	    loop
	).

%----------------------------------------------------------------------------------

process_non_end_client_message(regulus_sockettalk_say_file(File)) :-
	regulus_sockettalk_say_file(File),
	!.
process_non_end_client_message(regulus_sockettalk_say_tts(String)) :-
	regulus_sockettalk_say_tts(String),
	!.
process_non_end_client_message(regulus_sockettalk_say_list(ItemList)) :-
	regulus_sockettalk_say_list(ItemList),
	!.
process_non_end_client_message(regulus_sockettalk_set_parameter(ParamName, Value)) :-
	regulus_sockettalk_set_parameter(ParamName, Value),
	!.
process_non_end_client_message(regulus_sockettalk_get_parameter(ParamName)) :-
	(   regulus_sockettalk_get_parameter(ParamName, Value) ->
	    send_client_message(Value)
	;
	    otherwise ->
	    send_client_message('*error*')
	),
	!.
process_non_end_client_message(regulus_sockettalk_recognise_file(Wavfile, GrammarName)) :-
	(   regulus_sockettalk_recognise_file(Wavfile, GrammarName, Result) ->
	    send_client_message(Result)
	;
	    otherwise ->
	    send_client_message('*error*')
	),
	!.
process_non_end_client_message(Message) :-
	format('~N*** Error: unable to process message: ~w~n', [Message]),
	close_server_socket,
	regulus_sockettalk_exit_server.

%----------------------------------------------------------------------------------

% Open a socket to the client
initialise_server_socket(Port) :-
	current_host(Host),
	socket('AF_INET', Socket),
	socket_bind(Socket, 'AF_INET'(Host, Port)),
	socket_listen(Socket, 5),
	socket_accept(Socket, _Client, Stream),
	store_socket_and_stream(Socket, Stream),
	format('~N--- Initialised server socket~n', []),
	!.

% Close the socket to the client
close_server_socket :-
	get_socket_and_stream(Socket, _Stream),
	socket_close(Socket).

% Wait for a message from the client that says 'start'
wait_for_start_message(RegulusPort, Package, NuanceParameters) :-
	(   wait_for_term_from_client(1:0, start(RegulusPort, Package, NuanceParameters))  ->
	    true ;
	    wait_for_start_message(RegulusPort, Package, NuanceParameters)
	).	

% Wait 5000 microseconds to see if there is a term from the client
check_for_client_message(Message) :-
	wait_for_term_from_client(0:5000, Term),
	!,
	% If we got it, that's the message
	Message = Term.
% Otherwise, notional message is '*no_message*'
check_for_client_message(Message) :-
	Message = '*no_message*'.

% Send message to the client 
send_client_message(Message) :-
	get_socket_and_stream(_Socket, Stream),
	format(Stream, '~q.~n', [Message]),
	flush_output(Stream),
	!.

%----------------------------------------------------------------------------------

% Wait until you get something from the client.
% TimeOut is a term of the form S:M,
% where S is the number of Seconds and M is the number of microseconds
wait_for_term_from_client(TimeOut, Term) :-
	get_socket_and_stream(_Socket, Stream),
	% Wait TimeOut to see if Stream has anything to read
	socket_select([], [], TimeOut, [Stream], StreamsWithInput),
	% StreamsWithInput will be null if there is nothing to read on Stream
	StreamsWithInput \== [],
	read(Stream, Term),
	!.

%----------------------------------------------------------------------------------

:- dynamic server_socket_and_stream/2.

store_socket_and_stream(Socket, Stream) :-
	retractall(server_socket_and_stream(_, _)),
	assertz(server_socket_and_stream(Socket, Stream)),
	!.

get_socket_and_stream(Socket, Stream) :-
	server_socket_and_stream(Socket, Stream),
	!.

