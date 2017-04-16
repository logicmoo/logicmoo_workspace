/*  Part of Extended libraries for Prolog

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
