/*
* Copyright (C) 2002, 2003, 2016 Christoph Wernhard
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
* this program; if not, write to the Free Software Foundation, Inc., 59 Temple
* Place, Suite 330, Boston, MA 02111-1307 USA
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
		     write_png_dotgraph/2,
		     write_ps2_dotgraph/2,
		     write_gif_dotgraph/2]).

:- use_module(fromonto).
:- use_module(err).
:- use_module(library(readutil)).

/** <module> Interface to the dot graph drawing program of Graphviz 

  A graph to be processed by =dot= is represented by a =Dotgraph=, that
  is, a term of the following form:

==
     Dotgraph ::= dotgraph(Name, Statements)
     Statement ::= graph_attributes(AVs)       |
                   edge_defaults(AVs)          |
     	           node_defaults(AVs)          |
     	           node(Id, AVs)               |
     	           edge(Id1, Id2, AVs)         |
                   subgraph(Name, Statements)  |
                   statements(Statements) 
    
     AV ::= Attribute=Value | label=Label | label=RLabel
     Label ::= Value | Lines
     Line ::= Value | n(Value) | l(Value) | r(Value)
     RLabel ::= Label | Port:Label | rec(RLabel [, RLabel]*)
==
  
     =Name=, =Attribute= and =Value= are atoms. The optional functors in
     =Line= specify the justification of the line.  =RLabel= is used for
     record labels. =Id= is an atom or a term =NodeName:Port=, where
     =NodeName= and =Port= are atoms.

@author Christoph Wernhard  
  
@tbd Security: accessibility of the generated files should perhaps be
  restricted. This is for now implemented incompletely with umask.

*/

debug_keep_intermediate_files :- fail.
% debug_keep_intermediate_files.

%% 
%% Tested with graphviz-1.8.5
%% 

%% Windows/Unix versions behave differently with 0'\ and 0'\\, so
%% we use 92 instead.

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
	atomic_list_concat([Shape1 | Args], ' ', Line),
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
	  sformat(Coords1, '~d,~d,~d,~d', [XX1,YY1,XX2,YY2]),
	  string_to_atom(Coords1, Coords)
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

%!   write_gif_dotgraph(++Dotgraph, ++ImageFile) is det.
%
%     Invokes process_dotgraph/4 as `process_dotgraph(Dotgraph, 'image/gif',
%     ImageFile, none)`.
%
write_gif_dotgraph(Dotgraph, ImageFile) :-
	process_dotgraph(Dotgraph, 'image/gif', ImageFile, none).

%!   write_png_dotgraph(++Dotgraph, ++ImageFile) is det.
%
%     Invokes process_dotgraph/4 as `process_dotgraph(Dotgraph, 'image/png',
%     ImageFile, none)`.
%
write_png_dotgraph(Dotgraph, ImageFile) :-
	process_dotgraph(Dotgraph, 'image/png', ImageFile, none).

%!   write_ps2_dotgraph(++Dotgraph, ++ImageFile) is det.
%
%     Invokes process_dotgraph/4 as `process_dotgraph(Dotgraph,
%     'application/postscript',
%     ImageFile, none)`.
%
write_ps2_dotgraph(Dotgraph, ImageFile) :-
	process_dotgraph(Dotgraph, 'application/postscript', ImageFile, none).


%!   process_dotgraph(++Dotgraph, ++MimeType, --ImageFile, --IsmapAreas) is det.
%!   process_dotgraph(++Dotgraph, ++MimeType, ++ImageFile, --IsmapAreas) is det.
%!   process_dotgraph(++Dotgraph, ++MimeType, ++ImageFile, --IsmapAreas) is det.
%!   process_dotgraph(++Dotgraph, ++MimeType, ++ImageFile, ++IsmapAreas) is det.
%
%    Generate an image file and optionally a corresponding HTML area map
%    structure from a given dotgraph representation.
%
%    @arg Dotgraph A =Dotgraph= as specified in the module description.
%    
%    @arg MimeType An atom that specifies the mime type of the
%    generated image. One of `'image/gif'`, `'image/png'`, or
%    `'application/postscript'`.
%
%    @arg ImageFile The file name of the image file as atom. Either given
%    as input or, if the input is a variable, bound to the name of
%    a freshly generated temporary file.
%
%    @arg IsmapAreas If a variable is given as input, it is bound to
%    an HTML area structure represented with `element/3` terms
%    (if the mime type is `image/gif` or `image/png`).
%    If the atom `none` is given as input, no area map creation is performed.
%    
process_dotgraph(Dotgraph, MimeType, ImageFile, IsmapAreas) :-
	( var(ImageFile) ->
	  tmp_file('dotgraph', ImageFile1),
	  dot_mimetype(MimeType, Extension, _),
	  atom_concat(ImageFile1, Extension, ImageFile)
	; true
	),
	process_dotgraph_1(Dotgraph, MimeType, ImageFile, IsmapAreas).

dot_mimetype('image/gif', '.gif', gif).
dot_mimetype('image/png', '.png', png).
dot_mimetype('application/postscript', '.ps', ps2).

process_dotgraph_1(Dotgraph, MimeType, ImageFile, IsmapAreas) :-
	tmp_file('dotgraph', DotFile),
	tmp_file('dotgraph', IsmapFile),
	dot_mimetype(MimeType, _, DotType),
	sformat(Cmd1, 'umask 0077 ; dot -T~w ~w -o ~w',
		[DotType,DotFile, ImageFile]),
	( IsmapAreas \== none,
	  memberchk(MimeType, ['image/gif', 'image/png']) ->
	  sformat(Cmd2, 'umask 0077 ; dot -Tismap ~w -o ~w',
		  [DotFile, IsmapFile])
	; true
	),
	catch(( onto_file( print_dotgraph(Dotgraph), DotFile ),
		( shell(Cmd1, 0) ->
		  true
		; err('Graph image creation failed.')
		),
		( var(Cmd2) ->
		  true
		; shell(Cmd2, 0) ->
		  ismap_file_to_map(IsmapFile, IsmapAreas)
		; err('Graph map creation failed.')
		)
	      ),
	      E,
	      ( ( debug_keep_intermediate_files -> true
		; ( delete_file(DotFile) -> true ; true ),
		  ( delete_file(ImageFile) -> true ; true ),
		  ( nonvar(Cmd2), delete_file(IsmapFile) -> true ; true )
		),
		throw(E)
	      )),
	( debug_keep_intermediate_files ->
	  true
	; delete_file(DotFile),
	  (nonvar(Cmd2) -> delete_file(IsmapFile) ; true )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Print Dotgraph
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!   print_dotgraph(++Dotgraph) is det.
%
%    Prints the given dotgraph in the format accepted by =dot= to the standard
%    output.
%
%    @arg Dotgraph A =Dotgraph= as specified in the module description.
% 
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

print_statement(statements(Statements)) :-
	!,
	write('\t'),
	format('{~n'),
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
	format(atom(Value1), '~w', Value),
	atom_codes(Value1, Codes),
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
	%% HTML labels
	atom(Label),
	sub_atom(Label, 0, 1, _, '<'),
	sub_atom(Label, _, 1, 0, '>'),
	!,
	write(Label).
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
print_reclabel_2(Label) :-
	number(Label),
	!,
	atom_number(Label1, Label),
	print_reclabel_escaped(Label1).
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
	format(atom(Value1), '~w', Value),
	atom_codes(Value1, Codes),
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

