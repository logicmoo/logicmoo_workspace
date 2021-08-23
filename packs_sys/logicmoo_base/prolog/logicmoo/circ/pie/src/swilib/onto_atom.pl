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

:- module(onto_atom, [onto_atom/2]).

:- module_transparent(onto_atom/2).

onto_atom(Call, Atom) :-
	with_output_to(atom(Atom), Call).

% :- use_module(fromonto).
% 
% :- dynamic(onto_atom_via_file).
% 
% onto_atom_via_file_file('c:/tmp_onto_atom').
% 	
% :- catch( use_module(library(pce)),
% 	  _,
% 	  ( onto_atom_via_file_file(File),
% 	    assert(onto_atom_via_file),
% 	    sformat(S, 'Warning: no library(pce) - using file ~w for onto_atom', [File]),
% 	    write(user_error, S),
% 	    nl(user_error) )).
% 
% onto_atom(Call, Atom) :-
% 	onto_atom_via_file,
% 	!,
% 	onto_atom_via_file(Call, Atom).
% 
% onto_atom(Call, Atom) :-
%  	new(TB, text_buffer),
%  	send(TB, undo_buffer_size, 0),
%  	pce_open(TB, write, Out),
%  	catch( fromonto:onto_stream(Call, Out),
% 	       E,
% 	       ( close(Out), free(TB), throw(E) )),
%  	close(Out),
%  	pce_open(TB, read, In),
%  	read_codes(In, Codes, []),
%  	close(In),
%  	free(TB),
%  	atom_codes(Atom, Codes).
% 
% read_codes(Stream, Diff, Diff) :-
% 	at_end_of_stream(Stream),
% 	!.
% read_codes(Stream, [C|Cs], Diff) :-
% 	get_code(Stream, C),
% 	read_codes(Stream, Cs, Diff).
% 
% onto_atom_via_file(Call, Atom) :-
% 	onto_atom_via_file_file(File),
% 	onto_file(Call, File),
% 	from_file(onto_atom:read_codes(Codes, []), File),
% 	delete_file(File),
% 	atom_codes(Atom, Codes).
% 
% read_codes(Diff, Diff) :-
% 	at_end_of_stream,
% 	!.
% read_codes([C|Cs], Diff) :-
% 	get_code(C),
% 	read_codes(Cs, Diff).
% 
% 
