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

:- module(pce_editable_text, []).
:- use_module(library(pce)).


:- pce_begin_class(editable_text, text, "Editable short graphical text").

variable(editable,      bool := @on,    get,    "Text is editable").
variable(message,       'name|code*',   both,   "Action on enter").

:- pce_global(@editable_text_gesture, make_edit_text_recogniser).
:- pce_global(@editable_text_key_binding, make_key_binding).

make_edit_text_recogniser(R) :-
    new(R, edit_text_gesture).


make_key_binding(KB) :-
    new(KB, key_binding(editable_text, text)),
    send(KB, function, '\\C-a', select_all),
    send(KB, function, 'TAB', advance),
    send(KB, function, '\\e',  enter),
    send(KB, function, 'RET', enter).


editable(T, Val:bool) :->
    "Remove caret when switched to @off"::
    (   Val == @off
    ->  send(T?window, keyboard_focus, @nil)
    ;   true
    ),
    send(T, slot, editable, Val).


cancel(T) :->
    "Stop editing"::
    send(T?window, keyboard_focus, @nil).


show_caret(T, Val:bool) :->
    "Show/hide caret"::
    (   Val == @on
    ->  send(T, obtain_focus)
    ;   send(T, release_focus)
    ),
    send_super(T, show_caret, Val).


obtain_focus(T) :->
    "Called when focus is obtained: show the caret"::
    (   get(T, attribute, edit_saved_parms, _)  % pointer in/out of window
    ->  true
    ;   send(T, save_parameter, pen),
        send(T, save_parameter, border),
        get(T, pen, OldPen),
        get(T, border, OldBorder),
        NewPen is OldPen+1,
        send(T, pen, NewPen),
        NewBorder is max(2, OldBorder),
        send(T, border, NewBorder)
    ).


save_parameter(T, Parm:name) :->
    "Save some property to be restored after edit"::
    get(T, Parm, Value),
    (   get(T, attribute, edit_saved_parms, Sheet)
    ->  true
    ;   send(T, attribute, edit_saved_parms, new(Sheet, sheet))
    ),
    send(Sheet, value, Parm, Value).


release_focus(T) :->
    "Called when focus is lost: remove the caret"::
    (   get(T, attribute, edit_saved_parms, Sheet)
    ->  send(Sheet, for_all,
             message(T, @arg1?name, @arg1?value)),
        send(T, delete_attribute, edit_saved_parms)
    ;   true
    ).


enter(T) :->
    "Stop typing"::
    send(T?window, keyboard_focus, @nil),
    send(T, forward).

forward(T) :->
    "Typing has completed; forward change"::
    get(T, device, Dev),
    get(T, message, Msg),
    (   Msg == @nil
    ->  true
    ;   send(Msg, instance_of, name)
    ->  send(Dev, Msg, T?string?value)
    ;   send(Msg, forward, T?string?value)
    ).


advance(T) :->
    "Advance to next editable item"::
    send(T?device, advance, T).


'_wants_keyboard_focus'(T) :->
    "True if text is <-editable"::
    get(T, editable, @on).


typed(T, Id:event_id) :->
    "Handle keyboard input"::
    get(T, show_caret, @on),
    send(@editable_text_key_binding, typed, Id, T).


select_all(T) :->
    "Select the whole text"::
    get(T?string, size, End),
    send(T, selection, 0, End).


event(T, Ev:event) :->
    (   send(@editable_text_gesture, event, Ev)
    ;   send_super(T, event, Ev)
    ).

:- pce_end_class.

