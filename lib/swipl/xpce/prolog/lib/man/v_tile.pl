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

:- module(pce_tile_hierarchy, []).
:- use_module(library(pce)).

:- pce_begin_class(tile_hierarchy, frame).

initialise(F, Tile:tile) :->
    "Display hierarchy of tile"::
    send(F, send_super, initialise, 'Tile Hierarchy'),
    send(F, append, new(P, picture)),
    send(new(D, dialog), below, P),
    fill_dialog(D),
    send(F, display_tile, Tile),
    send(F, open).


fill_dialog(D) :-
    get(D, frame, Frame),
    send(D, append, button(quit, message(Frame, destroy))).


display_tile(F, Tile:tile) :->
    "Display hierarchy below tile"::
    get(F, member, picture, P),
    send(P, clear),
    send(P, display, tree(new(R, tile_node(Tile)))),
    send(R, expand).

:- pce_end_class.

:- pce_begin_class(tile_node, node).

initialise(N, Tile:tile) :->
    "Create node for tile"::
    (   get(Tile, object, Obj),
        Obj \== @nil
    ->  get(Obj, name, Label)
    ;   get(Tile, orientation, Label)
    ),

    new(S, string('%s %dx%d\n-%dx+%d -%dy+%d',
                  Label,
                  Tile?ideal_width, Tile?ideal_height,
                  Tile?hor_shrink, Tile?hor_stretch,
                  Tile?ver_shrink, Tile?ver_stretch)),

    new(_, hyper(N, Tile, tile)),

    send(N, send_super, initialise, text(S, center)).

expand(N) :->
    "Expand tile sub-tree"::
    get(N, hypered, tile, Tile),
    (   get(Tile, members, SubTiles),
        SubTiles \== @nil
    ->  send(Tile?members, for_all,
             and(assign(new(SubNode, var), ?(N?class, instance, @arg1)),
                 message(N, son, SubNode),
                 message(SubNode, expand)))
    ;   true
    ).

:- pce_end_class.
