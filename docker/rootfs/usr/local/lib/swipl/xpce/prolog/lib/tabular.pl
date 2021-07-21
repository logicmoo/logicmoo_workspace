/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2001-2011, University of Amsterdam
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

:- module(tabular, []).
:- use_module(library(pce)).

:- pce_begin_class(tabular, device,
                   "Device with associated table <-layout_manager").

delegate_to(layout_manager).

initialise(TD, Table:[table]) :->
    send_super(TD, initialise),
    (   Table == @default
    ->  send(TD, layout_manager, new(_, tabular_table))
    ;   send(TD, layout_manager, Table)
    ).

:- pce_group(appearance).


:- pce_group(event).

:- pce_global(@tabular_device_recogniser,
              new(resize_table_slice_gesture(column, left))).

event(RT, Ev:event) :->
    get(RT, table, Table),
    (   get(Table, cell_from_position, Ev, Cell),
        send(Cell, instance_of, table_cell),
        get(Cell, image, Gr),
        (   send(Gr, has_send_method, on_mark_clicked),
            send(Ev, is_a, button),
            get(Cell, note_mark, Mark),
            Mark \== @nil,
            get(Cell, area, area(X, Y, W, _)),
            get(Ev, position, RT, point(EX, EY)),
            get(Mark, size, size(MW, MH)),
            EX > X+W-MW,
            EY < Y+MH
        ->  (   send(Ev, is_up)
            ->  send(Gr, on_mark_clicked)
            ;   true
            )
        ;   send(Ev, post, Gr)
        )
    ;   send(@tabular_device_recogniser, event, Ev)
    ).

:- pce_group(geometry).

table_width(TD, W:int) :->
    "Set width of the table"::
    send(TD?table, width, W).

:- pce_group(parts).

table(TD, Table:table) :<-
    "Get the table layout_manager"::
    get(TD, layout_manager, Table).

:- pce_group(fill).

append(TD,
       Label:label='name|graphical|table_cell',
       Font:font=[font],
       HAlign:halign=[{left,center,right}],
       VAlign:valign=[{top,center,bottom}],
       Span:colspan='[1..]',
       RSpan:rowspan='[1..]',
       BG:background=[colour],
       FG:colour=[colour]) :->
    "Append a cell to the table"::
    get(TD, table, Table),
    (   atom(Label)
    ->  new(TC, table_cell(text(Label, @default, Font)))
    ;   send(Label, instance_of, graphical)
    ->  new(TC, table_cell(Label)),
        (   Font \== @default
        ->  send(Label, font, Font)
        ;   true
        )
    ;   TC = Label,
        (   Font \== @default
        ->  send(Label, font, Font)
        ;   true
        )
    ),
    (   HAlign \== @default
    ->  send(TC, halign, HAlign)
    ;   true
    ),
    send(TC, background, BG),
    (   FG \== @default
    ->  send(TC?image, colour, FG)
    ;   true
    ),
    (   Span \== @default
    ->  send(TC, col_span, Span)
    ;   true
    ),
    (   RSpan \== @default
    ->  send(TC, row_span, RSpan)
    ;   true
    ),
    (   VAlign \== @default
    ->  send(TC, valign, VAlign)
    ;   true
    ),
    send(Table, append, TC).

clear(TD) :->
    "Delete all rows"::
    get(TD, table, Table),
    send(Table, delete_rows).

:- pce_group(label).

append_label_button(TD, Field:name) :->
    "Append a button to sort the field"::
    get(TD, layout_manager, Table),
    get(Table, current, point(X, Y)),
    send(Table, append,
         new(TC, table_cell(new(B, button(Field,
                                          message(TD, sort_rows,
                                                  X, Y+1)))))),
    send(B, radius, 0),
    get(class(dialog), class_variable, background, BGVar),
    send(TC, background, BGVar?value),
    send(TC, cell_padding, size(0,0)),
    send(TC, halign, stretch).

:- pce_group(sort).

sort_rows(TD, Col:int, FromRow:int) :->
    "Sort rows starting at FromRow on the indicated column"::
    format('~p: Sorting rows below ~w on column ~w~n', [TD, FromRow, Col]).

:- pce_end_class(tabular).


                 /*******************************
                 *          THE TABLE           *
                 *******************************/

:- pce_begin_class(tabular_table, table,
                   "The layout manager class tabular").

stretched_column(Table, Col:table_column, W:int) :->
    "Adjust the size of cells holding a wrapped text"::
    get(Col, index, Index),
    send(Col, for_all, message(Table, stretched_cell, @arg1, W, Index)),
    send_super(Table, stretched_column, Col, W).

stretched_cell(T, Cell:table_cell, W:int, ColN:int) :->
    (   get(Cell, image, Graphical),
        send(Graphical, instance_of, graphical)
    ->  (   send(Graphical, has_send_method, margin),
            get(Graphical, send_method, margin,
                tuple(Graphical, Method)),
            get(Method, argument_type, 2, T2),
            send(T2, validate, wrap)
        ->  spanned_cell_width(Cell, ColN, W, T, TextW),
            send(Graphical, margin, TextW, wrap)
        ;   send(Graphical, instance_of, device),
            get(Graphical, format, Format),
            Format \== @nil,
            get(Format, columns, @off)
        ->  spanned_cell_width(Cell, ColN, W, T, TextW),
            send(Graphical, format, width, TextW)
        ;   send(Graphical, has_get_method, auto_align),
            get(Graphical, auto_align, @on)
        ->  spanned_cell_width(Cell, ColN, W, T, TextW),
            send(Graphical, do_set, width := TextW)
        ;   true
        )
    ;   true
    ).

spanned_cell_width(Cell, ColN, W, T, TextW) :-
    get(Cell, col_span, Span),
    get(Cell, column, Col0),
    EndCol is Col0+Span,
    cell_width(Col0, EndCol, ColN, W, T, 0, TotalW),
    (   get(Cell, cell_padding, size(PW, _))
    ->  TextW is TotalW - PW*2
    ;   get(Cell?table, cell_padding, size(PW, _))
    ->  TextW is TotalW - PW*2
    ;   TextW is TotalW
    ).

%       Determine the width of a spanned cell.

cell_width(End, End, _, _, _, W, W) :- !.
cell_width(C, End, N, W, T, W0, Width) :-
    (   C == N
    ->  W1 is W0 + W
    ;   get(T, column, C, Col),
        get(Col, width, WC),
        W1 is W0 + WC
    ),
    C2 is C + 1,
    cell_width(C2, End, N, W, T, W1, Width).

:- pce_end_class(tabular_table).
