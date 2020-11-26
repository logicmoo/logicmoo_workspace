
/*

Toy XML server in Prolog. Read lines, discarding anything that doesn't start with a < and end with a >,
exclusing whitespace. When we read a line containing the string </message>, parse everything
we've read so far as XML, and print out the result in Prolog form. The line "EXIT" finishes.

*/

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%----------------------------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(xml)).
:- use_module(library(sockets)).
:- use_module(library(lists)).

%----------------------------------------------------------------------------------

server :-
	server(4325).

%----------------------------------------------------------------------------------

server(Port) :-
	safe_socket_server_open(Port, Socket),
	server1(Socket).

server1(Socket) :-
	safe_socket_server_accept(Socket, _Client, Stream),
	server_loop(Stream, []),
	close(Stream),
	safe_socket_server_close(Socket),
	!.

server_loop(Stream, OldLines) :-
	read_line(Stream, Line),
	%format('~NRead line: "~s"~n', [Line]),
	flush_output(user),
	(   Line == "EXIT" ->
	    format('~NServer exit~n', [])
	;
	    otherwise ->
	    process_line(Line, OldLines-NewLines),
	    !,
	    server_loop(Stream, NewLines)
	).

%----------------------------------------------------------------------------------

process_line(Line, OldLines-OldLines) :-
	\+ is_xml_line(Line),
	!.
process_line(Line, OldLines-NewLines) :-
	is_contiguous_sublist("</message>", Line),
	!,
	append(OldLines, Line, CompleteString),
	(   xml_parse(CompleteString, XMLTerm) ->
	    format('~N--- Read XML structure. Prolog form:~n~n', []),
	    prettyprintq(XMLTerm),
	    format('~N~n', [])
	;
	    otherwise ->
	    format('~N*** Error: unable to parse XML string to Prolog: ~s~n', [CompleteString])
	),
	NewLines = [].
process_line(Line, OldLines-NewLines) :-
	append(OldLines, Line, NewLines),
	!.

%----------------------------------------------------------------------------------

% To find out if we have an XML line, remove whitespaces, and see if it starts with < and ends with >

is_xml_line(Line) :-
	remove_whitespaces(Line, LineWithoutWhiteSpaces),
	LineWithoutWhiteSpaces = [0'< | _],
	last(LineWithoutWhiteSpaces, 0'>).

remove_whitespaces([], []).
remove_whitespaces([F | R], R1) :-
	whitespace_char(F),
	!,
	remove_whitespaces(R, R1).
remove_whitespaces([F | R], [F | R1]) :-
	!,
	remove_whitespaces(R, R1).
