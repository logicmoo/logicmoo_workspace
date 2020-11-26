
/*

Translation server. The call

  regulus_language_server(<PortNumber>)

starts a translation server that accepts requests on the port <PortNumber>.

The following requests are supported:

 Request: client(ClientHost).
Response: accept(ClientHost).

 Request: call(Call).
Response: Call.   [Call completed, instantiated version returned].
          error.  [Something went wrong]
 
 Request: client_shutdown.
Response: thread_shutdown.

Note the periods after the requests and responses.

EXAMPLE SESSION

Client: client_shutdown.
Server: thread_shutdown

Socket code adapted from code at http://dlp.cs.vu.nl/~ctv/dlpinfo/srcs/tcp/sicsock.pl.html

*/

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%----------------------------------------------------------------------------------

:- asserta(library_directory('$REGULUS/PrologLib')).
:- asserta(library_directory('$REGULUS/Prolog')).
:- asserta(library_directory('$REGULUS/Alterf/Prolog')).
:- asserta(library_directory('.')).

%----------------------------------------------------------------------------------

:- use_module(library(regulus_top)).
:- use_module(library(ebl_make_training_data)).
:- use_module(library(ebl_train)).
:- use_module(library(ebl_postprocess)).

:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(sockets)).

:- use_module(library(utilities)).

%----------------------------------------------------------------------------------

% Don't use analysis timeouts in server for now - weird problems with interaction between sockets and timeout (?)
% Manny Rayner, Mar 29 2007

:- disable_timeouts.

%---------------------------------------------------------------

regulus_language_server :-
	regulus_language_server(4321).

regulus_language_server(Port) :-
	current_host(Host),
	regulus_language_server(Host, Port).

%----------------------------------------------------------------------------------

regulus_language_server(_Host, Port) :-
	%socket('AF_INET', Socket),
	%socket_bind(Socket, 'AF_INET'(Host, Port)),
	%socket_listen(Socket, 5),
	safe_socket_server_open(Port, Socket),
	%socket_accept(Socket, _Client, Stream),
	safe_socket_server_accept(Socket, _Client, Stream),
	regulus_language_server_loop(Stream),
	socket_close(Socket),
	format('Exit server~n', []),
	halt.

regulus_language_server_loop(Stream) :-
	repeat,
		read(Stream, ClientRequest),
		format('~N~nServer received: ~q~n', [ClientRequest]),
		regulus_language_server_input(ClientRequest, ServerReply),
		format('  ~nServer response: ~q~n', [ServerReply]),
		regulus_language_server_reply(ServerReply, Stream),
	ClientRequest == client_shutdown,
	!.

regulus_language_server_input(ClientRequest, ServerReply) :-
	ClientRequest = client(ClientHost),
	!,
	ServerReply = accept(ClientHost).
regulus_language_server_input(ClientRequest, ServerReply) :-
	ClientRequest = call(Request),
	!,
	handle_regulus_language_action_request(Request, Response),
	ServerReply = Response.
regulus_language_server_input(ClientRequest, ServerReply) :-
	ClientRequest = end_of_file,
	!,
	ServerReply = client_shutdown.
regulus_language_server_input(ClientRequest, ServerReply) :-
	ClientRequest = client_shutdown,
	!,
	ServerReply = thread_shutdown.
regulus_language_server_input(ClientRequest, ServerReply) :-
	ServerReply = unknown_request(ClientRequest),
	format('Unknown client request: ~w~n', [ClientRequest]).

%----------------------------------------------------------------------------------

regulus_language_server_reply(client_shutdown, _ServerOut) :-
	format('Server: end of file.~n'),
	!.
regulus_language_server_reply(ServerReply, ServerOut) :-
	format(ServerOut, '~q.~n', [ServerReply]),
	flush_output(ServerOut).

%----------------------------------------------------------------------------------

handle_regulus_language_action_request(Request, Response) :-
	on_exception(
	_Exception, 
	handle_regulus_language_action_request1(Request, Response),
	Response = error
    ),
	!.
handle_regulus_language_action_request(_Request, error) :-
	!.

%----------------------------------------------------------------------------------

handle_regulus_language_action_request1(Call, Call) :-
	call(Call),
	!.

