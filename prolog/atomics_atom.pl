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

:- module(atomics_atom, [atomics_atom/2]).

%% atomics_atom(+Atms:list(atomic), -Atm:atom) is det.
%% atomics_atom(?Atms:list(atom), +Atm:atomic)
%
% Atm is the atom resulting from concatenating all atomics in the list Atms in the
% order in which they appear. If Atm is an atom at call then Atms can contain
% free variables, and multiple solutions can be found on backtracking.
%
% Based on atom_concat/2 of Ciao-Prolog, but without performance issues.

atomics_atom(Atomics, Atom) :-
    ( var(Atom)->F=1 ; F=2 ),
    atomics_atom(Atomics, F, Atom).

atomics_atom([], _, '').
atomics_atom([A|L], F, Atom) :-
    atomics_atom(L, F, A, Atom).

atomics_atom([],          _, Atom, Atom).
atomics_atom([B|Atomics], F, A,    Atom) :-
    atomics_atom(F, Atomics, A, B, Atom).

% Non-optimized/naive version:
%
% atomics_atom(Atoms, A, B, Atom) :-
%     nonvar(A),
%     nonvar(B), !,
%     atom_concat(A, B, C),
%     atomics_atom(Atoms, C, Atom).
% atomics_atom(Atoms, A, B, Atom) :-
%     atomics_atom(Atoms, C, Atom),
%     atom_concat(A, B, C).

atomics_atom(1, Atomics, A, B, Atom) :- atomics_to_atom(Atomics, A, B, Atom).
atomics_atom(2, Atomics, A, B, Atom) :- atom_to_atomics(Atom, A, B, Atomics).

atom_to_atomics(Atom, A, B, Atomics) :-
    nonvar(A), !,
    atom_concat(A, Atom2, Atom),
    atomics_atom(Atomics, 2, B, Atom2).
atom_to_atomics(Atom, A, B, Atomics) :-
    nonvar(B), !,
    sub_atom(Atom, Before, _, After, B),
    sub_atom(Atom, 0, Before, _, A),
    sub_atom(Atom, _, After, 0, Atom2),
    atomics_atom(Atomics, 2, Atom2).
atom_to_atomics(Atom, A, B, Atomics) :-
    atomics_atom(Atomics, 2, C, Atom),
    atom_concat(A, B, C).

atomics_to_atom(Atomics, A, B, Atom) :-
    ( nonvar(A),
      nonvar(B)
    ->atom_concat(A, B, C),
      atomics_atom(Atomics, 1, C, Atom)
    ; throw(error(instantiation_error,
		  context(atomics_atom:atomics_atom/2,_)))
    ).
