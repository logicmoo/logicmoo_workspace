
/*

Simple code for testing the translation server. Start client as follows:

> sicstus

?- ['$REGULUS/RegulusLanguageServer/Prolog/client'].

?- client(<PortNumber>).       [same <PortNumber> as for server]

You are now in a loop, where the client will prompt you for one of several standard messages.
You type in the message ID, followed by a period. Here are the defined message IDs:

--------------


revert - send a 'revert_discourse_context' message

shutdown - close down server

--------------

The client will print out the actual message it sends to the server, and the server's response.

Socket communication code taken from http://dlp.cs.vu.nl/~ctv/dlpinfo/srcs/tcp/sicsock.pl.html.

*/

:- use_module(library(sockets)).

%port(4321).

client :-
	client(4321).

client(Port)  :-
	current_host(Host),
	client(Host, Port).

client(Host, Port) :-
	socket('AF_INET', Socket),
	socket_connect(Socket, 'AF_INET'(Host, Port), Stream),
	client_loop(Stream),
	socket_close(Socket),
	format('Exit client~n', []).

client_loop(Stream) :-
	repeat,
	get_client_message(Message),
	client_msg(Stream, Message),
	Message == client_shutdown.

client_msg(Stream, Mesg) :-
	format('Client and msg: ~q.~n', [Mesg]),
	format(Stream, '~q.~n', [Mesg]),
	flush_output(Stream),
	read(Stream, StreamTerm),
	format('Client rcv msg: ~q~n', [StreamTerm]).

%------------------------------------------------------------------------

get_client_message(Message) :-
	format('~N~nChoose message ID: ', []),
	read(Id),
	(   client_message(Id, Message) ->
	    true ;
	    format('~NSorry, unknown ID~n', []),
	    get_client_message(Message)
	).

client_message(shutdown, client_shutdown).

client_message(1, call((_X is 1 + 1))).
client_message(2, call(member(_X, [a, b]))).
client_message(3, call(findall(X,
			       member(X, [a, b]),
			       _Xs))).

client_message(start_eng_eng_specialised,
	       call(user:regulus_batch('$MED_SLT2/EngEng/Prolog/med_new_interlingua.cfg', ["EBL_LOAD"]))
	      ).
client_message(parse1,
	       call(user:words_to_lf_and_tree_with_current_parser([where, 'is', the, pain], _GrammarAtom, _LF, _Tree))
	      ).

client_message(revert, action(revert_discourse_context)).

