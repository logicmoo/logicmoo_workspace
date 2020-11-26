/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2014, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(atomics_atom, [atomics_atom/2]).

%!  atomics_atom(+Atms:list(atomic), -Atm:atom) is det.
%!  atomics_atom(?Atms:list(atom), +Atm:atomic)
%
%   Atm is the atom resulting from concatenating all atomics in the list Atms in
%   the order in which they appear. If Atm is an atom at call then Atms can
%   contain free variables, and multiple solutions can be found on backtracking.
%
%   Based on atom_concat/2 of Ciao-Prolog, but without performance issues.

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
