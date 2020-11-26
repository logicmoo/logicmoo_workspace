
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

/*

Translation server. The call

  server(<PortNumber>)

starts a translation server that accepts requests on the port <PortNumber>.

The following requests are supported:

 Request: client(ClientHost).
Response: accept(ClientHost).

 Request: action(load_package(ConfigFile)).
Response: ok.     [Server has been (re)initialised using config file ConfigFile]
          error.  [Something went wrong]

 Request: client_shutdown.
Response: thread_shutdown.

Note the periods after the requests and responses.


Socket code adapted from code at http://dlp.cs.vu.nl/~ctv/dlpinfo/srcs/tcp/sicsock.pl.html

*/

%----------------------------------------------------------------------------------

:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(sockets)).

:- use_module('$REGULUS/PrologLib/utilities').

%----------------------------------------------------------------------------------

server(Port) :-
	current_host(Host),
	server(Host, Port).

%----------------------------------------------------------------------------------

server(Host, Port) :-
	socket('AF_INET', Socket),
	socket_bind(Socket, 'AF_INET'(Host, Port)),
	socket_listen(Socket, 5),
	socket_accept(Socket, _Client, Stream),
	server_loop(Stream),
	socket_close(Socket),
	format('Exit server~n', []).

server_loop(Stream) :-
	read(Stream, ClientRequest),
	format('~N~nServer received: ~q~n', [ClientRequest]),
	server_input(ClientRequest, ServerReply),
	format('  ~nServer response: ~q~n', [ServerReply]),
	server_reply(ServerReply, Stream),
	flush_output(user),
	!,
	(   ClientRequest == client_shutdown ->
	    true ;

	    server_loop(Stream)
	),
	!.

server_input(ClientRequest, ServerReply) :-
	ClientRequest = client(ClientHost),
	!,
	ServerReply = accept(ClientHost).
server_input(ClientRequest, ServerReply) :-
	ClientRequest = action(Request),
	!,
	handle_action_request(Request, Response),
	ServerReply = Response.
server_input(ClientRequest, ServerReply) :-
	ClientRequest = end_of_file,
	!,
	ServerReply = client_shutdown.
server_input(ClientRequest, ServerReply) :-
	ClientRequest = client_shutdown,
	!,
	ServerReply = thread_shutdown.
server_input(ClientRequest, ServerReply) :-
	ServerReply = unknown_request(ClientRequest),
	format('Unknown client request: ~w~n', [ClientRequest]).

%----------------------------------------------------------------------------------

server_reply(client_shutdown, _ServerOut) :-
	format('Server: end of file.~n'),
	!.
server_reply(ServerReply, ServerOut) :-
	format(ServerOut, '~q.~n', [ServerReply]),
	flush_output(ServerOut).

%----------------------------------------------------------------------------------

handle_action_request(Request, Response) :-
	on_exception(
	_Exception, 
	handle_action_request1(Request, Response),
	Response = error
    ),
	!.
handle_action_request(_Request, error) :-
	!.

%----------------------------------------------------------------------------------

handle_action_request1(tts(String), printed(Atom)) :-
	atom_chars(Atom, String),
	format('~N~n"~w"~n', [Atom]),
	!.

