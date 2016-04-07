/*  Part of Extended libraries for Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2014, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(infer_alias,
	  [infer_alias/3,
	   fastest_alias/2,
	   library_alias/2,
	   smallest_alias/2,
	   current_alias/2,
	   pretty_path/2
	  ]).

:- use_module(library(term_size)).

infer_alias(File, CAlias, OptionL) :-
    select_option(sort(SortL), OptionL, _, []),
    findall(SortTerm-Alias,
	    ( current_alias(File, Alias),
	      Alias =.. [AName, _],
	      maplist(sort_field(Alias, AName), SortL, SortTerm)
	    ), SA),
    sort(SA, [_-CAlias|_]).

sort_field(_, A, alias(L), N) :-
    ( nth0(N, L, A)
    ->true
    ; length(L, N)
    ).
sort_field(_, A,       sols, N) :-
    findall(A, user:file_search_path(A, _), L),
    length(L, N).
sort_field(Alias, _, size, S) :-
    term_size(Alias, S).
sort_field(Alias, _, length, N) :-
    term_to_atom(Alias, Atom),
    atom_length(Atom, N).

fastest_alias(File, Alias) :-
    infer_alias(File, Alias, [sort([sols, size, length])]).

library_alias(File, Alias) :-
    infer_alias(File, Alias, [sort([alias([library]), sols, size, length])]).

smallest_alias(File, Alias) :-
    infer_alias(File, Alias, [sort([length, size, sols])]).

current_alias(File, Alias) :-
    user:file_search_path(A0, ADir),
    A0 \= autoload,
    absolute_file_name(ADir, Dir, [file_type(directory), solutions(all)]),
    directory_file_path(Dir, Base, File),
    file_name_extension(Name, _Ext, Base),
    pretty_path(Name, Path),
    Alias =.. [A0, Path].

pretty_path(Name0, Path/F) :-
    directory_file_path(Dir, F, Name0),
    Dir \= '.',
    !,
    pretty_path(Dir, Path).
pretty_path(Name, Name).
