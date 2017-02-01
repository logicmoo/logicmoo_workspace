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

:- module(atomics_string, [atomics_string/2]).

%% atomics_string(+Atms:list(atomic), -Atm:string) is det.
%% atomics_string(?Atms:list(string), +Atm:string)
%
% Like atomics_atom, but with strings.

atomics_string(Atomics, String) :-
    ( var(String)->F=1 ; F=2 ),
    atomics_string(Atomics, F, String).

atomics_string([], _, "").
atomics_string([A|L], F, String) :-
    atomics_string(L, F, A, String).

atomics_string([],          _, String, String).
atomics_string([B|Atomics], F, A,      String) :-
    atomics_string(F, Atomics, A, B, String).

atomics_string(1, Atomics, A, B, String) :- atomics_to_string(Atomics, A, B, String).
atomics_string(2, Atomics, A, B, String) :- string_to_atomics(String, A, B, Atomics).

string_to_atomics(String, A, B, Atomics) :-
    nonvar(A), !,
    string_concat(A, Atom2, String),
    atomics_string(Atomics, 2, B, Atom2).
string_to_atomics(String, A, B, Atomics) :-
    nonvar(B), !,
    sub_string(String, Before, _, After, B),
    sub_string(String, 0, Before, _, A),
    sub_string(String, _, After, 0, Atom2),
    atomics_string(Atomics, 2, Atom2).
string_to_atomics(String, A, B, Atomics) :-
    atomics_string(Atomics, 2, C, String),
    string_concat(A, B, C).

atomics_to_string(Atomics, A, B, String) :-
    ( nonvar(A),
      nonvar(B)
    ->string_concat(A, B, C),
      atomics_string(Atomics, 1, C, String)
    ; throw(error(instantiation_error,
                  context(atomics_string:atomics_string/2,_)))
    ).
