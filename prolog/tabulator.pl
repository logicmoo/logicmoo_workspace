/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.

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
