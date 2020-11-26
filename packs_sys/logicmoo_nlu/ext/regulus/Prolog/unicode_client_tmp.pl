
:- use_module(library(sockets)).

client :-
	client(1985,
	       [type(text), encoding('UTF-16LE')]).

client(Port, OpenStreamParameters) :-
	current_host(Host),
	socket_client_open(Host:Port, Stream, OpenStreamParameters),
	read(Stream, Term),
	format('~N--- Read from stream: ~q~n', [Term]),
	close(Stream),
	!.
