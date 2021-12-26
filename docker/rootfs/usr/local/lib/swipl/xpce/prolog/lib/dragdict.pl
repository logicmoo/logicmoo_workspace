/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
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

:- module(pce_drag_and_drop_browser, []).
:- use_module(library(pce)).
:- use_module(library(dragdrop)).       % no autoload, we need it now

:- pce_begin_class(drag_and_drop_dict_item_gesture,
                   drag_and_drop_gesture,
                   "Drag and drop items from a browser").

class_variable(button, button_name, left,
               "By default drag-and-drop from left").

set_source(G, Ev:event) :->
    "Set <-source to dict_item or <-get_source(dict_item)"::
    get(Ev, receiver, LB),
    get(LB, dict_item, Ev, DI),
    get(G, get_source, Function),
    (   Function == @nil
    ->  send(G, slot, source, DI)
    ;   get(Function, '_forward', DI, Source),
        send(G, slot, source, Source)
    ).


cursor(G, LB:list_browser, Ev:event, Cursor:cursor) :<-
    "Make cursor for the dict_item"::
    (   get(G, class_variable_value, cursor, Cursor), Cursor \== @nil
    ->  send(G?offset, set, 0, 0)
    ;   get(LB, dict_item, Ev, DI),
        get(DI, label, Label),
        font(DI, Font),
        new(T, text(Label, left, Font)),
        get(T, size, size(W, H)),
        new(BM, image(@nil, W, H)),
        send(BM, draw_in, T),
        get(DI, image, LB),
        get(DI, position, DiPos),
        (   get(G, warp, @on)
        ->  new(HotSpot, point(W/2, H/2)),
            send(DiPos, plus, HotSpot),
            send(LB, pointer, DiPos)
        ;   get(Ev, position, LB, EvPos),
            get(EvPos, difference, DiPos, HotSpot),
            get(HotSpot, x, HX),
            get(HotSpot, y, HY),
            ( HX > W-8 -> send(HotSpot, x, W-8) ; true ),
            ( HY > H -> send(HotSpot, y, H) ; true )
        ),
        send(BM, or, image('cross.bm'), point(HotSpot?x-8, HotSpot?y-8)),
        new(Cursor, cursor(@nil, BM, @default, HotSpot))
    ).

font(DI, Font) :-
    get(DI, style, StyleName),
    atom(StyleName),
    get(DI, image, Browser),
    get(Browser?styles, StyleName, Style),
    get(Style, font, Font),
    !.
font(DI, Font) :-
    get(DI, style, @default),
    get(DI, image, Browser),
    get(Browser, font, Font).


:- pce_end_class.

/*
test :-
        send(new(B, browser), open),
        send(@classes, for_all, message(B, append, @arg1)),
        send(B, sort),
        send(B?list_browser, recogniser,
             new(G, drag_and_drop_dict_item_gesture)),
        send(G, warp, @off).
*/
