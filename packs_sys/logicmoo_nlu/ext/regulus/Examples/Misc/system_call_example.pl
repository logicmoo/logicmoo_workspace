
/*

Example of sending and recieving material with cURL access. Behaviour is:


*/

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module('$REGULUS/Examples/Misc/xml_info').  %this is needed for cleaning up the xml

:- use_module(library(sockets)).
:- use_module(library(lists)).
:- use_module(library(xml)).   % this is needed for parsing the xml


reqall_log_in :-
	process_create('C:/cygwin/bin/curl.exe', ['-i ', '-k ', '-X ', 'POST ',  '-H ', '\'Accept: application/xml\' ', '-d ', '\'accessid=ucsc_ling160\' ', '-d ', '\'password=onesixty160\' ', 'https://www.reqall.com'], [stdout(pipe(Stream))]),
	read_xml(Stream, XMLReply),
	prettyprint(XMLReply).

/*
reqall_post :-
	process_create(
*/
%------------------------------------------------------------------------
/*
  this gets the message ("login." or "request." or "end.") from command line. This is a test/example structure only. 
  An app would get the message from the input/dialogue/output_manager processing and send that.
*/
get_client_message(Message) :-
	format('~N~nChoose message ID: ', []),
	read(Id),
	(   client_message(Id, Message) ->
	    true ;
	    format('~NSorry, unknown ID~n', []),
	    get_client_message(Message)
	).

%These are the possible messages
client_message(exit, exit).
client_message(login, login).
client_message(request, request).

%--------------------------------------------------------------------------
%read xml from the socket, then tidy it up with beautify_prolog_xml.
read_xml(Stream, XMLReply) :-
	read_xml_loop(Stream, [], MessyXMLReply),
	beautify_prolog_xml(MessyXMLReply, XMLReply).  

/*
  This loop repeatedly reads a line from the socket. It keeps the xml lines and 
  gets rid of the others. It appends all the lines together. When you run out of 
  xml it exits
*/
read_xml_loop(Stream, OldLines, XMLReply) :-
	read_line(Stream, Line),           %
	%format('~NRead line: "~s"~n', [Line]),
	%flush_output(user),
	(   Line == end_of_file ->
	    XMLReply = '*** Error: no closing </message> tag'
	;
	    otherwise ->
	    process_line(Line, OldLines-NewLines, ResultForLine),
	    !,
	    (   ResultForLine = finished(XMLReply) ->
		true
	    ;
		otherwise ->
		read_xml_loop(Stream, NewLines, XMLReply)
	    )
	).

%---------------------------------------------------------------------------
/* 
  This predicate actually does the work of checking whether
  lines are xml or not and appending all the xml lines 
  together. It works through the list of lines using
  difference lists
*/

% not an xml line and not at the end yet
process_line(Line, OldLines-OldLines, not_finished) :-
	\+ is_xml_line(Line),
	!.
%Either this all xml lines have been read successfully or the xml is bad
process_line(Line, OldLines-[], ResultForLine) :-
	is_contiguous_sublist("</message>", Line),
	!,
	append(OldLines, Line, CompleteString),
	(   xml_parse(CompleteString, XMLTerm) ->
	    ResultForLine = finished(XMLTerm)
	;
	    otherwise ->
	    ResultForLine = finished('*** Error: unable to parse XML')
	).
%There are still more lines to process
process_line(Line, OldLines-NewLines, not_finished) :-
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

