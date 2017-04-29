/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
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

:- module(gcover, [gcover/2, covered_db/6, reset_cover/0, reset_cover/1]).

:- use_module(library(ontrace)).

:- meta_predicate gcover(0,+).

gcover(Goal, OptL0 ) :-
    select_option(tag(Tag), OptL0, OptL, user),
    ontrace(Goal, gcover_port(Tag), OptL).

:- dynamic covered_db/6.

gcover_port(Tag, Port, _Frame, _PC, _ParentL, Loc, continue) :-
    record_cover(Loc, Port, Tag).

loc_file_range(file_term_position(File, TermPos), File, Fr, To) :-
    arg(1, TermPos, Fr),
    arg(2, TermPos, To).

record_cover(Loc, Port, Tag) :-
    loc_file_range(Loc, File, Fr, To),
    ( retract(covered_db(Fr, To, File, Port, Tag, Count1))
    ->succ(Count1, Count)
    ; Count=1
    ),
    assertz(covered_db(Fr, To, File, Port, Tag, Count)).

reset_cover :- reset_cover(_).

reset_cover(Tag) :-
    retractall(covered_db(_, _, Tag, _, _, _)).
