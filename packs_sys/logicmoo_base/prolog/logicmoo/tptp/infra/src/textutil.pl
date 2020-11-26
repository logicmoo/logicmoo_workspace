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

:- module(textutil, [abbreviate/3, write_lines/3, indent/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Abbreviate
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abbreviate(Text, ApproxMaxLen, Text1) :-
	( atom(Text) -> C2 = Text ; term_to_atom(Text, C2) ),
	( sub_atom(C2, B, _, _, '. ') ->
	  B1 is B + 1,
	  sub_atom(C2, 0, B1, _, C3)
	; C3 = C2
	),
	atom_length(C3, L),
	( L < ApproxMaxLen ->
	  Text1 = C3
	; sub_atom(C3, 0, ApproxMaxLen, _, C4),
	  ( atom_codes(C4, Cs),
	    reverse(Cs, Cs1),
	    memlist(Cs1, [S|Cs2]),
	    code_type(S, space) ->
	    reverse([0'., 0'., 0'.| Cs2], Cs3),
	    atom_codes(Text1, Cs3)
	  ; atom_concat(C4, '...', Text1)
	  )
	).  

memlist(X, X).
memlist([_|Xs], Y) :-
	memlist(Xs, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Write Lines
%%%% 
%%%% Write the atom as lines indented by Indent with a right margin of Margin.
%%%% At first a newline is written. No trailing newline is written.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_lines(Atom, Indent, Margin) :-
	concat_atom(Atoms, ' ', Atom),
	write_lines_1(Atoms, Margin, Indent, Margin).

write_lines_1([''|Atoms], Pos, Indent, Margin) :-
	!,
	write_lines_1(Atoms, Pos, Indent, Margin).
write_lines_1([Atom|Atoms], Pos, Indent, Margin) :-
	atom_length(Atom, L),
	( Pos+L >= Margin ->
  	  nl,
	  indent(Indent),
	  Pos1 is Indent+L
	; write(' '),
	  Pos1 is Pos+L+1
	),
	write(Atom),
	write_lines_1(Atoms, Pos1, Indent, Margin).
write_lines_1([], _, _, _).


indent(N) :-
	    N > 0,
	    !,
	    write(' '),
	    N1 is N-1,
	    indent(N1).
indent(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
