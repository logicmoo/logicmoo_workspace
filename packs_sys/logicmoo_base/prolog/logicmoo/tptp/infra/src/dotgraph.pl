/*
* Copyright (C) 2002, 2007 Christoph Wernhard
* 
* This program is free software; you can redistribute it and/or modify it
* under the terms of the GNU General Public License as published by the Free
* Software Foundation; either version 2 of the License, or (at your option)
* any later version.
* 
* This program is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
* more details.
* 
* You should have received a copy of the GNU General Public License along with
* this program; if not, see <http://www.gnu.org/licenses/>.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Dotgraph
%%%% 
%%%% Interface to the Dot Graph Drawing Program
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(dotgraph, [process_dotgraph/4,
		     print_dotgraph/1,
		     write_gif_dotgraph/2]).

:- use_module('swilib/fromonto').
:- use_module('swilib/err').
:- use_module(library(readutil)).

%
% *** TODO - Security: accessibility of the DotFile
%          - currently only implemented as far as we need it
%            (i.e. only gif files)     

%%%% 
%%%% Tested with graphviz-1.8.5
%%%% 

%% Windows/Unix versions behave differently with 0'\ and 0'\\, so
%% we use 92 instead.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Graph Term Syntax
%%%% =================
%%%%
%%%% dotgraph(Name, Statements)
%%%% Statement ::= graph_attributes(AVs)       |
%%%%               edge_defaults(AVs)          |
%%%% 	           node_defaults(AVs)          |
%%%% 	           node(Id, AVs)               |
%%%% 	           edge(Id1, Id2, AVs)         |
%%%%               subgraph(Name, Statements)
%%%%
%%%% AV ::= Attribute=Value | label=Label | label=RLabel
%%%% Label ::= Value | Lines
%%%% Line ::= Value | n(Value) | l(Value) | r(Value)
%%%% RLabel ::= Label | Port:Label | rec(RLabel [, RLabel]*)
%%%%
%%%% Name, Attribute and Value are atoms. The optional funtors
%%%% in Line specify the justification of the line.
%%%% RLabel is used for record labels. Id is an atom or a term NodeName:Port,
%%%% where NodeName and Port are atoms.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Print Dotgraph
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_dotgraph(dotgraph(Name, Statements)) :-
	!,
	format('digraph ~w {~n', [Name]),
	( member(Stmt, Statements),
	  write('\t'), print_statement(Stmt),
	  fail
	; true
	),
	format('}~n').
print_dotgraph(Object) :-
	err('Not a dotgraph: ~q.', [Object]).

print_statement(edge(Id1, Id2, AVs)) :-
	!,
	print_id(Id1),
	format(' -> '),
	print_id(Id2),
	( AVs = [] ->
	  format(';~n')
	; format(' ['),
	  print_avs(AVs),
	  format('];~n')
	).
print_statement(node(Id, AVs)) :-
	!,
	print_id(Id),
	( AVs = [] ->
	  format(';~n')
	; format(' ['),
	  print_avs(AVs),
	  format('];~n')
	).
print_statement(edge_defaults(AVs)) :-
	!,
	( AVs = [] ->
	  true
	; format('edge ['),
	  print_avs(AVs),
	  format('];~n')
	).
print_statement(node_defaults(AVs)) :-
	!,
	( AVs = [] ->
	  true
	; format('node ['),
	  print_avs(AVs),
	  format('];~n')
	).
print_statement(graph_attributes(AVs)) :-
	!,
	( member(A=V, AVs),
	  format('~w=', [A]),
	  print_value(V),
	  format(';~n'),
	  fail
	; true
	).
print_statement(subgraph(Name, Statements)) :-
	!,
	%% For now indentation corresponds always to the first subgraph
	%% level, also for nested subgraphs.
	write('\t'),
	format('subgraph ~w {~n', [Name]),
	( member(Stmt, Statements),
	  write('\t\t'), print_statement(Stmt),
	  fail
	; true
	),
	write('\t'),
	format('}~n').
print_statement(S) :-
	err('Bad graph statement term: ~q.', [S]).

print_avs([A=V]) :-
	!,
	print_av(A, V).
print_avs([A=V|AVs]) :-
	print_av(A, V),
	format(', '),
	print_avs(AVs).

print_av(label, Label) :-
	!,
	format('label='),
	%% Seems that the escapes required for record labels are also
	%% handled propery by labels for other shapes.
	print_reclabel(Label).
print_av(A, V) :-
	format('~w=', [A]),
	print_value(V).

% print_label(Label) :-
% 	atom(Label),
% 	!,
% 	print_value(Label).
% print_label(Lines) :-
% 	put_code(0'"),
% 	( member(Line, Lines),
% 	  ( atom(Line) -> Value = Line, NL = '\\n'
% 	  ; Line = n(Value) -> NL = '\\n'
% 	  ; Line = l(Value) -> NL = '\\l'
% 	  ; Line = r(Value) -> NL = '\\r'
% 	  ; err('Bad label line: ~q.', [Line])
% 	  ),
% 	  print_escaped(Value),
% 	  write(NL),
% 	  fail
%         ; true
% 	),
% 	put_code(0'").
	    
print_escaped(Value) :-	       
	atom_codes(Value, Codes),
	( member(C, Codes),
	  ( print_value_escape(C) ->
	    put_code(92)
	  ; true
	  ),
	  put_code(C),
	  fail
	; true
	).

print_id(NodeName:Port) :-
	!,
	print_value(NodeName),
	put_code(0':),
	print_value(Port).
print_id(NodeName) :-
	print_value(NodeName).

print_value(Value) :-
	put_code(0'"),
	print_escaped(Value),
	put_code(0'").

print_value_escape(0'").
print_value_escape(92).

print_reclabel(Label) :-
	put_code(0'"),
	print_reclabel_0(Label),
	put_code(0'").

print_reclabel_0(Label) :-
	compound(Label),
	Label =.. [rec,Field1|Fields],
	!,
        print_reclabel_1(Field1),		
	( member(Field, Fields),
	  put_code(0'|),
	  print_reclabel_1(Field),
	  fail
	; true
	).
print_reclabel_0(Label) :-
	print_reclabel_1(Label).

print_reclabel_1(Label) :-
	compound(Label),
	Label =.. [rec,Field1|Fields],
	!,
	put_code(0'{),
        print_reclabel_1(Field1),		
	( member(Field, Fields),
	  put_code(0'|),
	  print_reclabel_1(Field),
	  fail
	; true
	),
	put_code(0'}).
print_reclabel_1(Port:Label) :-
	!,
	put_code(0'<),
	print_reclabel_escaped(Port),
	put_code(0'>),
	print_reclabel_2(Label).
print_reclabel_1(Label) :-
	print_reclabel_2(Label).

print_reclabel_2(Label) :-
	atom(Label),
	!,
	print_reclabel_escaped(Label).
print_reclabel_2(Lines) :-
	Lines = [_|_],
	!,
	( member(Line, Lines),
	  ( atom(Line) -> Value = Line, NL = '\\n'
	  ; Line = n(Value) -> NL = '\\n'
	  ; Line = l(Value) -> NL = '\\l'
	  ; Line = r(Value) -> NL = '\\r'
	  ; err('Bad label line: ~q.', [Line])
	  ),
	  print_reclabel_escaped(Value),
	  write(NL),
	  fail
        ; true
	).
print_reclabel_2(Label) :-
	err('Bad label: ~q.', [Label]).

print_reclabel_escaped(Value) :-	       
	atom_codes(Value, Codes),
	( member(C, Codes),
	  ( C = 10 ->
	    put_code(92),
	    put_code(0'n)
	  ; ( print_reclabel_escape(C) ->
	      put_code(92)
	    ; true
	    ),
	    put_code(C)
	  ),
	  fail
	; true
	).

print_reclabel_escape(0'").
print_reclabel_escape(92).
print_reclabel_escape(0' ).
print_reclabel_escape(0'{).
print_reclabel_escape(0'}).
print_reclabel_escape(0'|).
print_reclabel_escape(0'<).
print_reclabel_escape(0'>).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% ISMAP Files
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ismap_file_to_map(File, Areas) :-
	from_file( read_lines(Lines), File ),
	map_ismap_to_area(Lines, Areas).

ismap_to_area(Line, E) :-
	E = element(area,
		    [alt=Alt, shape=Shape, coords=Coords, href=Href], []),
	concat_atom([Shape1 | Args], ' ', Line),
	( Shape1 = rectangle ->
	  Shape = rect,  
	  Args = [Cs1, Cs2, Href | _],
	  Alt = Href,
	  term_to_atom((X1,Y1), Cs1),
	  term_to_atom((X2,Y2), Cs2),
	  XX1 is min(X1, X2),
	  XX2 is max(X1, X2),
	  YY1 is min(Y1, Y2),
	  YY2 is max(Y1, Y2),
	  format(atom(Coords), '~d,~d,~d,~d', [XX1,YY1,XX2,YY2])
	; err('Unsupported ismap line: ~q.', [Line])
	).
	
map_ismap_to_area([X|Xs], [X1|Xs1]) :-
	ismap_to_area(X, X1),
	map_ismap_to_area(Xs, Xs1).
map_ismap_to_area([], []).

read_lines([]) :-
	current_input(S),
	at_end_of_stream(S),
	!.
read_lines([L|Ls]) :-
	current_input(S),
	read_line_to_codes(S, C),
	atom_codes(L, C),
	read_lines(Ls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Process Dotgraph
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_gif_dotgraph(Dotgraph, ImageFile) :-
	process_dotgraph_1(Dotgraph, 'image/gif', ImageFile, _).

%%%% 
%%%% process_dotgraph(+Dotgraph, +MimeType, -ImageFile, -IsmapAreas)
%%%%
process_dotgraph(Dotgraph, MimeType, ImageFile, IsmapAreas) :-
	tmp_file('dotgraph', ImageFile1),
	atom_concat(ImageFile1, '.gif', ImageFile),
	process_dotgraph_1(Dotgraph, MimeType, ImageFile, IsmapAreas).

process_dotgraph_1(Dotgraph, 'image/gif', ImageFile, IsmapAreas) :-
	tmp_file('dotgraph', DotFile),
	tmp_file('dotgraph', IsmapFile),
	format(atom(Cmd1), 'umask 0077 ; dot -Tgif ~w -o ~w',
		[DotFile, ImageFile]),
	format(atom(Cmd2), 'umask 0077 ; dot -Tismap ~w -o ~w',
		[DotFile, IsmapFile]),
	catch(( onto_file( print_dotgraph(Dotgraph), DotFile ),
		( shell(Cmd1, 0) ->
		  true
		; err('Graph image creation failed.')
		),
		( shell(Cmd2, 0) ->
		  true
		; err('Graph map creation failed.')
		),
		ismap_file_to_map(IsmapFile, IsmapAreas)
	      ),
	      E,
	      ( ( delete_file(DotFile) -> true ; true ),
		( delete_file(ImageFile) -> true ; true ),
		( delete_file(IsmapFile) -> true ; true ),
		throw(E)
	      )),
	delete_file(DotFile),
	delete_file(IsmapFile).
