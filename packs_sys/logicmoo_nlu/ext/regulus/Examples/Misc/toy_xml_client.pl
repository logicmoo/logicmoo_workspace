
/*

Toy client to match server in toy_xml_server. Open a socket and go into a loop.
At each iteration of the loop, ask user for message ID, and print appropriate text to the socket.
Possible messages:

- xml1. Print text in $REGULUS/Examples/Misc/xml_example1.txt
- xml2. Print text in $REGULUS/Examples/Misc/xml_example2.txt
- exit. End loop and close down server.

*/

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(sockets)).

client :-
	client(4325).

client(Port) :-
	current_host(Host),
	safe_socket('AF_INET', Socket),
	safe_socket_connect(Socket, Host, Port, Stream),
	client_loop(Stream),
	close(Stream),
	format('Exit client~n', []).

client_loop(Stream) :-
	repeat,
	get_client_message(Message),
	format('Sending message:~n~n~s~n~n', [Message]),
	format(Stream, '~s~n', [Message]),
	flush_output(Stream),
	Message = "EXIT".

%------------------------------------------------------------------------

get_client_message(Message) :-
	format('~N~nChoose message ID: ', []),
	read(Id),
	(   client_message(Id, Message) ->
	    true ;
	    format('~NSorry, unknown ID~n', []),
	    get_client_message(Message)
	).

client_message(exit, "EXIT").
client_message(xml1, String) :-
	File = '$REGULUS/Examples/Misc/xml_example1.txt',
	absolute_file_name(File, AbsFile),
	read_file_to_string(AbsFile, String).
client_message(xml2, String) :-
	File = '$REGULUS/Examples/Misc/xml_example2.txt',
	absolute_file_name(File, AbsFile),
	read_file_to_string(AbsFile, String).
