
/*

Toy XML server in Prolog. The server recieves "login", "request", or "exit" messages from
the client. If it gets "login" it reads in xml_example1.txt from the file and sends it out
the socket. If it gets "request" it does the same thing with xml_example2.txt. If it gets 
"exit" it shuts down. 

*/

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%----------------------------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(xml)).
:- use_module(library(sockets)).
:- use_module(library(lists)).

%----------------------------------------------------------------------------------
%convenient way to start main predicate in sicstus by typing "server."
server :-
	server(4335).

%----------------------------------------------------------------------------------

%Open the socket
server(Port) :-
	safe_socket_server_open(Port, Socket),
	server1(Socket).

server1(Socket) :-
	safe_socket_server_accept(Socket, _Client, Stream),  %connect to client
	server_loop(Stream),   %main loop
	close(Stream),         %done with loop -- close down
	safe_socket_server_close(Socket),
	!.

%main loop
server_loop(Stream) :-
	read(Stream, ClientRequest),    %get client request from socket
	format('~NReceived from client: "~w"~n', [ClientRequest]),
	(   server_input(ClientRequest, ServerReply) ->     %if server has a reply
	    format('  ~nServer response: ~s~n', [ServerReply]),
	    format(Stream, '~s~n', [ServerReply]),     %send the reply to the client
	    flush_output(Stream)
	;
	    otherwise ->
	    format('Unknown client request~n', [])  %otherwise complain
	),
	(   ClientRequest == exit ->        %if the client request is exit, leave the loop
	    format('~NServer exit~n', [])
	;
	    otherwise ->
	    server_loop(Stream)       %if the client request is not exit, go through the loop again
	).


%=================================================================================
/*
Replies for client requests.
-Pick the right file
-resolve variables
-read the file into a string
*/
server_input(login, String):-
	File = '$REGULUS/Examples/Misc/xml_example1.txt',
	absolute_file_name(File, AbsFile),
	read_file_to_string(AbsFile, String),
	!.
server_input(request, String):-
	File = '$REGULUS/Examples/Misc/xml_example2.txt',
	absolute_file_name(File, AbsFile),
	read_file_to_string(AbsFile, String),
	!.
