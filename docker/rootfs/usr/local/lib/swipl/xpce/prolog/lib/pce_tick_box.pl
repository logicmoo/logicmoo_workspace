/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1996-2011, University of Amsterdam
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

:- module(pce_tick_box, []).
:- use_module(library(pce)).
:- require([ default/3
           ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class `tick_box' defines a label with a `tick-box' displayed left of the
label. The selection is expressed as a boolean.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(tick_box, menu, "Simple boolean tick-box").

variable(align_with, {left,value} := left, both, "How to align").

class_variable(item_elevation, elevation*, @nil, "Elevation of the label").

initialise(TB, Name:name, Value:[bool], Message:[code]*) :->
    default(Value, @off, Def),
    send_super(TB, initialise, Name, marked, Message),
    send(TB, multiple_selection, @on),
    send_super(TB, show_label, @off),
    get(TB, label_font, Font),
    send(TB, value_font, Font),
    send(TB, append, menu_item(Name,
                               message(@receiver, forward))),
    send(TB, default, Def).

:- pce_group(appearance).

label_width(TB, LW:int) :->
    "Honour label alignment if we align with value of <-above"::
    (   get(TB, align_with, value)
    ->  send_super(TB, label_width, LW)
    ;   true
    ).

label(TB, Label:'name|image') :->
    "Set the label"::
    (   get(TB, members, Members),
        Members \== @nil
    ->  get(Members, head, Item),
        send(Item, label, Label)
    ;   send(TB, send_super, label, Label) % during initialise
    ).

show_label(TB, Show:bool) :->
    "Show the label"::
    get(TB?members, head, Item),
    (   Show == @on
    ->  send(Item, label, Item?value?label_name)
    ;   send(Item, label, '')
    ).

:- pce_group(selection).

selection(TB, Val:bool) :->
    "Set selection as boolean"::
    get(TB?members, head, Item),
    send(Item, selected, Val).
selection(TB, Val:bool) :<-
    "Get selection as boolean"::
    get(TB?members, head, Item),
    get(Item, selected, Val).

forward(TB) :->
    "Execute the message"::
    get(TB, message, Msg),
    get(TB, selection, Val),
    (   Msg == @default
    ->  get(TB, name, Selector),
        send(TB?device, Selector, Val)
    ;   Msg == @nil
    ->  true
    ;   send(Msg, forward, Val)
    ).

:- pce_end_class.

