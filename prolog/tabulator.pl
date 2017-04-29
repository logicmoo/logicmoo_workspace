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

:- module(tabulator, [tabulate/3, align/4]).

tabulate(FillChar, Table0, Table) :-
    maplist(tabulate_row(Lists), Table0, Table),
    maplist(close_list(FillChar), Lists).

close_list(FillChar, List) :-
    maplist('='(FillChar), List),
    !.

tabulate_row(Lengths, Row0, Row) :-
    maplist(tabulate_element, Lengths, Row0, Row).

tabulate_element(List, Column0, Column) :-
    length(Column0, Length0),
    length(List0,   Length0),
    append(Column0, Tail0, Column),
    append(List0,   Tail0, List).

align(FillChar, Scheme, Table0, Table) :-
    maplist(align_row(FillChar, Scheme), Table0, Table).

align_row(FillChar, Scheme, Row0, Row) :-
    maplist(align_cell_(FillChar), Scheme, Row0, Row).

align_cell_(FillChar, Align, Cell0, Cell) :-
    align_cell(Align, FillChar, Cell0, Cell).

align_cell(left, FillChar, Cell0, Cell) :-
    align_cell_left(Cell0, FillChar, Cell).
align_cell(right, FillChar, Cell0, Cell) :-
    align_cell_right(Cell0, FillChar, Cell).
align_cell(center, FillChar, Cell0, Cell) :-
    align_cell_center(Cell0, FillChar, Cell).
align_cell(none, _, Cell, Cell).

align_cell_left(Cell0, FillChar, Cell) :-
    discompose_cell(Cell0, FillChar, [], FillStr, Cell1),
    append(Cell1, FillStr, Cell).

discompose_cell([],           _,    FillStr,  FillStr, []).
discompose_cell([Char|Cell0], Char, FillStr0, FillStr, Cell) :- !,
    discompose_cell(Cell0, Char, [Char|FillStr0], FillStr, Cell).
discompose_cell(Cell, _, FillStr, FillStr, Cell).

align_cell_right(Cell0, FillChar, Cell) :-
    reverse(Cell0, Cell1),
    discompose_cell(Cell1, FillChar, [], FillStr, Cell2),
    reverse(Cell2, Cell3),
    append(FillStr, Cell3, Cell).

align_cell_center(Cell0, FillChar, Cell) :-
    discompose_cell(Cell0, FillChar, [], Fill1, Cell1),
    reverse(Cell1, Cell2),
    discompose_cell(Cell2, FillChar, [], Fill2, Cell3),
    reverse(Cell3, Cell4),
    length(Fill1, N1),
    length(Fill2, N2),
    NL is (N1 + N2) // 2,
    length(FillL, NL),
    append(Fill1, Fill2, Fill),
    append(FillL, FillR, Fill),
    append([FillL, Cell4, FillR], Cell).
