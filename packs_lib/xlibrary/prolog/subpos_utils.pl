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
           location_subterm/3,
           location_subterm/4,
           location_subterm_un/3,
           location_subterm_un/4,
           location_subterm_eq/3
          ]).

:- use_module(library(lists)).
:- use_module(library(neck)).

:- meta_predicate
        subterm_location(1,+,?),
        location_subterm(+,1,+),
        location_subterm(+,1,1,+),
        location_subterm_un(+,+,1,+).

location_subpos(PPos, N, SPos) :-
    nonvar(PPos),
    PPos = parentheses_term_position(_, _, Pos), !,
    location_subpos(Pos, N, SPos).
location_subpos(term_position(_, _, _, _, PosL), N, Pos) :-
    nth1(N, PosL, Pos).
location_subpos(PPos, N, Pos) :-
    member(IniP-PPos, [inip(Pos1)-list_position(From, To, PosL, Tail),
                       frto(BTo)-sub_list_position(From, To, BTo, _, PosL, Tail)
                      ]),
    neck,
    ( N = 1
    ->( PosL = [Pos|_]
      ->true
      ; PosL = []
      ->Pos = Tail
      )
    ; N = 2
    ->( PosL = [_]
      ->Pos = Tail
      ; PosL = [Pos1|PosL1],
        lspi(IniP, BTo1),
        PosL1 = [Pos2|_],
        arg(1, Pos2, PTo),
        Pos = sub_list_position(From, To, BTo1, PTo, PosL1, Tail)
      )
    ).
location_subpos(brace_term_position(_, _, Pos), 1, Pos).

lspi(inip(Pos), BTo) :- arg(1, Pos, BTo).
lspi(frto(BTo), BTo).

subpos_location([],    Pos,    Pos).
subpos_location([N|L], SubPos, Pos) :-
    location_subpos(SubPos, N, Pos1),
    subpos_location(L, Pos1, Pos).

location_subterm_un(L, Term, Find) :- location_subterm(L, =(Find), Term).

location_subterm_un(L, Term, Tester, Find) :- location_subterm(L, =(Find), Tester, Term).

location_subterm_eq(L, Term, Find) :- subterm_location(==(Find), Term, L).

subterm_location(Comparator, Term, []) :- call(Comparator, Term), !.
subterm_location(Comparator, Term, [N|L]) :-
    compound(Term),
    arg(N, Term, SubTerm),
    subterm_location(Comparator, SubTerm, L).

location_subterm(L, Comparator, Term) :-
    location_subterm(L, Comparator, compound, Term).

location_subterm([],    Comparator, _, Term) :- call(Comparator, Term).
location_subterm([N|L], Comparator, Tester, Term) :-
    call(Tester, Term),
    arg(N, Term, SubTerm),
    location_subterm(L, Comparator, Tester, SubTerm).
