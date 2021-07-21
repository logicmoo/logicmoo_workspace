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

:- module(editor_buttons, []).
:- use_module(pce_boot(pce_principal)).
:- use_module(pce_boot(pce_realise),
              [ pce_register_class/1,
                pce_begin_class_definition/4
              ]).

make_editor_recogniser(G) :-
    new(Editor, @event?receiver),
    new(G, handler_group(new(select_editor_text_gesture),
                         click_gesture(middle, '', single,
                                       and(message(Editor, paste, primary),
                                           message(Editor, mark_undo))))).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines @editor_recogniser, a recogniser called from

Parts of the specs by Uwe Lesta.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(select_editor_text_gesture, gesture,
                   "Select text in an editor").

variable(selecting,     bool := @off,   get, "Are we making a selection").
variable(down_position, point*,         get, "Position of down-event").
variable(origin,        int*,           get, "Index of down event").
variable(unit,          {character,word,line}, get, "Selection unit").
variable(editor,        editor*,        get, "Client object").

initialise(G) :->
    send_super(G, initialise),
    send(G, slot, unit, character),
    send(G, drag_scroll, self).


initiate(G, Ev:event) :->
    "Set caret and prepare for selectiong"::
    send(G, slot, down_position, Ev?position),
    get(Ev, receiver, Editor),
    send(G, slot, editor, Editor),
    get(Editor, image, Image),
    get(Image, index, Ev, Index),
    send(Editor, caret, Index),
    get(Ev, multiclick, Multi),
    selection_unit(Multi, Unit),
    send(G, slot, unit, Unit),
    (   Multi == single
    ->  send(G, slot, origin, Index),
        send(G, selecting, @off)
    ;   send(G, selecting, @on)
    ).

selection_unit(single, character).
selection_unit(double, word).
selection_unit(triple, line).


selecting(G, Val:bool) :->
    "Start/stop selecting"::
    send(G, slot, selecting, Val),
    get(G, editor, Editor),
    (   Val == @on
    ->  get(G, origin, Origin), Origin \== @nil,
        send(Editor, selection_unit, G?unit),
        send(Editor, selection_origin, Origin)
    ;   send(Editor, mark_status, inactive)
    ).


drag(G, Ev:event) :->
    "Extend the selection if selecting"::
    (   (   get(G, selecting, @on)
        ->  true
        ;   get(G, down_position, DownPos),
            get(Ev, position, EvPos),
            get(DownPos, distance, EvPos, D),
            D > 25
        ->  send(G, selecting, @on)
        )
    ->  get(Ev, receiver, Editor),
        get(Editor, image, Image),
        (   get(Image, index, Ev, Index)
        ->  send(Editor, selection_extend, Index)
        ;   true
        )
    ;   true
    ).

terminate(G, _Ev:event) :->
    "If we are selecting, copy the selection"::
    get(G, editor, Editor),
    send(G, slot, editor, @nil),
    (   get(G, selecting, @on),
        get(Editor, class_variable_value, auto_copy, @on)
    ->  send(Editor, copy)
    ;   true
    ).

:- pce_end_class.

:- free(@editor_recogniser).
:- initialization
    make_editor_recogniser(@editor_recogniser).
