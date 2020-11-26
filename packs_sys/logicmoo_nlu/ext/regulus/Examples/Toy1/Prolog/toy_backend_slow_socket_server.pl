
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

/*

Server. The call

  server(<PortNumber>)

starts a server that accepts requests on the port <PortNumber>.

Socket code adapted from code at http://dlp.cs.vu.nl/~ctv/dlpinfo/srcs/tcp/sicsock.pl.html

*/

%----------------------------------------------------------------------------------

:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(sockets)).

:- use_module('$REGULUS/PrologLib/utilities').

%:- ensure_loaded('$REGULUS/Examples/Toy1/Prolog/polling.pl').

%----------------------------------------------------------------------------------

% BackendServerPort = 8062

server :-
	server(7022).

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
	flush_output(Stream),
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
	get_approval_to_complete_response,
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

handle_action_request1(Action, Action) :-
	!.

%----------------------------------------------------------------------------------

get_approval_to_complete_response :-
	format('~N~nHit return to send response: ', []),
	read_line(_Line),
	!.
