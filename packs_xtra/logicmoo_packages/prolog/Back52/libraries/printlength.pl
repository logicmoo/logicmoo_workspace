/******************************************************************************
    This file is being distributed, by written permission of Quintus 
    Corporation, for use with the BACK system only.  This file may not
    be used with Prolog implementations other than Quintus Prolog except
    (a) as part of the BACK system, or (b) with the written permission
    of Quintus Corporation.  To obtain such written permission, please
    contact:

	Quintus Corporation
	2100 Geng Road
	Palo Alto,
	California  94303
	USA
	415-813-3800
	marketing@quintus.com
******************************************************************************/

%
%bianchi - 5/6/96 introdotta una modifica - vedi piu' avanti.
%

%   Package: print_length
%   Author : Richard A. O'Keefe
%   Updated: 8/29/89
%   Purpose: determining how wide a term would be if written

%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

:- module(print_length, [
	print_length/2,
	print_length/3,
	print_lines/2,
	tab_to/1
   ]).
:- meta_predicate
	print_length(0, ?),
	print_length(0, ?, ?),
	print_lines(0, ?),
	tab_to(+).
:- mode
	'print length'(+, +, +).

sccs_id('"@(#)89/08/29 printlength.pl    33.1"').


%   print_length(+Command, ?StartColumn, ?EndColumn)
%   is true when executing Command would write EndColumn-StartColumn
%   characters.  Either StartColumn or EndColumn should be instantiated
%   to an integer.  The other argument can then be solved for.  Quintus
%   Prolog numbers columns starting from 0 (think of 'line_position' as
%   "the number of characters which have already been read from/written
%   on this line"), so print_length/3 will fail if StartColumn is negative.
%   print_length/3 fails if Command fails.

print_length(Command, StartColumn, EndColumn) :-
	print_length(Command, Length),
    %	plus(Length, StartColumn, EndColumn),
	(   integer(StartColumn) ->
	    EndColumn is StartColumn+Length
	;   integer(EndColumn) ->
	    StartColumn is EndColumn-Length
	),
	StartColumn >= 0.


%   print_length(+Command, -Length)
%   is true when Command would write Length characters to the current
%   output stream, none of them being new-line characters.  The point
%   of this predicate is to let you work out how many columns an atom
%   (or some other term), would take if printed according to Command.
%   The reason that it fails if a new-line character would be written
%   is that your calculations about columns are likely assume that no
%   new-lines are written.  I don't know exactly what you need, hence
%   this may be more useful to you as a model than as it stands.  The
%   technique is to actually execute the command with output directed
%   to a new null stream, and see how far it got.  The length of the
%   output of any command at all can be determined this way, provided
%   that it writes to the current stream, and not to a stream argument.
%   Help stamp out stream arguments!
%   print_length/2 fails if Command fails.

% bianchi - cambiato il controllo di LineNumber .
% forse il SICSstus numera le linee diversamente. 
print_length(Command, Length) :-
	open_null_stream(Stream),
	'print length'(Command, user, Stream),
	character_count(Stream, CharsWritten),
	line_count(Stream, LineNumber),
	close(Stream),
% versione originale
%	LineNumber = 1,
% per il SICSTUS o altro
        (che_prolog('SICS')-> 
         LineNumber = 0;
         LineNumber =1), 
	Length = CharsWritten.


%   print_lines(+Command, -Lines)
%   is true when Command would write Lines new-line characters to the
%   current output stream.  One use of this is to tell whether there
%   would be any point in calling print_length/2.

print_lines(Command, Lines) :-
	open_null_stream(Stream),
	'print length'(Command, user, Stream),
	line_count(Stream, LineNumber),
	close(Stream),
	Lines is LineNumber-1.


%   'print length' only really needs its first and last clauses.
%   The other clauses are accelerators to avoid dynamic lookup of
%   some common cases (which are visible in all modules).  Note
%   that display/1 is defined to write to 'user_output', NOT to
%   the current output stream.  So you should never use it as an
%   argument to print_length/[2,3].  Just in case you do, I have
%   mapped it to write_canonical/1.  But write_canonical/1 will
%   quote some atoms, which display/1 never will.  Caveat scriptor!

'print length'(Module:Command, _, Stream) :- !,
	'print length'(Command, Module, Stream).
'print length'(write(X), _, Stream) :- !,
	write(Stream, X).
'print length'(writeq(X), _, Stream) :- !,
	writeq(Stream, X).
'print length'(display(X), _, Stream) :- !,	% not quite right
	write_canonical(Stream, X).
'print length'(write_canonical(X), _, Stream) :- !,
	write_canonical(Stream, X).
'print length'(print(X), _, Stream) :- !,
	print(Stream, X).
'print length'(format(F,L), _, Stream) :- !,
	format(Stream, F, L).
'print length'(Command, Module, Stream) :-
	current_output(OldOutput),
	set_output(Stream),
	(   Module:Command -> true
	;   set_output(OldOutput),
	    close(Stream),
	    fail
	),
	set_output(OldOutput).


%   tab_to(+Column)
%   ensures that line_position(Current output, Column) is true
%   by writing 0 or 1 newlines and at most Column spaces.

tab_to(Column) :-
	current_output(Stream),
	line_position(Stream, OldCol),
	(   OldCol > Column ->
	    nl,
	    Spaces is Column
	;   Spaces is Column-OldCol
	),
	tab(Spaces).

