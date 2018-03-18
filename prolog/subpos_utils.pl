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

:- module(subpos_utils,
          [subpos_location/3,
           subterm_location/3,
           subterm_location_eq/3
          ]).

:- use_module(library(lists)).

location_subpos(PPos, N, SPos) :-
    nonvar(PPos),
    PPos = parentheses_term_position(_, _, Pos), !,
    location_subpos(Pos, N, SPos).
location_subpos(term_position(_, _, _, _, PosL), N, Pos) :-
    nth1(N, PosL, Pos).
location_subpos(list_position(From, To, PosL, Tail), N, Pos) :-
    ( N = 1
    ->PosL = [Pos|_]
    ; N = 2
    ->( PosL = [_]
      ->Pos = Tail
      ; PosL = [_|PosL1],
        Pos = list_position(From, To, PosL1, Tail)
      )
    ).
location_subpos(brace_term_position(_, _, Pos), 1, Pos).

subpos_location([],    Pos,    Pos).
subpos_location([N|L], SubPos, Pos) :-
    location_subpos(SubPos, N, Pos1),
    subpos_location(L, Pos1, Pos).

subterm_location([],    Term, Term).
subterm_location([N|L], Find, Term) :-
    compound(Term),
    arg(N, Term, SubTerm),
    subterm_location(L, Find, SubTerm).

subterm_location_eq([],    Find, Term) :- Find==Term.
subterm_location_eq([N|L], Find, Term) :-
    compound(Term),
    arg(N, Term, SubTerm),
    subterm_location_eq(L, Find, SubTerm).
