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
