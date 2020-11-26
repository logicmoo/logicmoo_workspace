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

:- module(emacs_hit_list, []).
:- use_module(library(pce)).
:- require([ default/3
           ]).

/** <module> PceEmacs class to show error search location
*/

:- pce_begin_class(emacs_hit_list, frame,
                   "Represent result of find, errors, etc.").

variable(expose_on_append, bool := @off, both,
         "->expose on ->append_hit").
variable(clear_on_append,  bool := @off, both,
         "Clear on ->append_hit after usage").
variable(used,             bool := @off, both,
         "->goto has been used").
variable(message,          name := caret, both,
         "Method to apply").

class_variable(confirm_done, bool, @off).

initialise(L, Label:[string]) :->
    "Create from label"::
    default(Label, 'Compilation errors', FrameLabel),
    send_super(L, initialise, FrameLabel),
    send(L, append, new(B, browser('', size(60, 6)))),
    send(B, select_message, message(L, goto, @arg1?object)),
    send(new(D, dialog), below, B),
    send(D, pen, 0),
    send(D, gap, size(10, 5)),
    send(D, append, button(quit, message(L, destroy))),
    send(D, append, label(reporter), right).


unlink(L) :->
    "Remove fragments from the buffers"::
    send(L, clear),
    send_super(L, unlink).

open(L) :->
    "Open, if possible as a transient window for PceEmacs"::
    debug(emacs, 'Open ~p~n', [L]),
    (   object(@emacs),
        get(@emacs, current_frame, Frame)
    ->  get(Frame, area, area(X,Y,W,_H)),
        send(L, transient_for, Frame),
        send(L, create),
        get(L, size, size(BW,_)),
        send_super(L, open, point(X+W-BW,Y+20))
    ;   send_super(L, open)
    ).

browser(L, Browser:list_browser) :<-
    get(L, member, browser, B),
    get(B, list_browser, Browser).


clear(L) :->
    "Clear browser and delete fragments"::
    get(L?browser, dict, Dict),
    send(Dict, for_all, message(@arg1?object, free)),
    send(Dict, clear),
    send(L, used, @off).


append_hit(L, Buffer:emacs_buffer, Start:int, Len:[int], Msg:[char_array]) :->
    "Append a hit to the hit-list"::
    (   get(L, expose_on_append, @on)
    ->  send(L, expose)
    ;   true
    ),
    (   get(L, clear_on_append, @on),
        get(L, used, @on)
    ->  send(L, clear)
    ;   true
    ),
    (   Len == @default
    ->  get(Buffer, scan, Start, line, 0, end, EOL),
        FragLength is EOL - Start
    ;   FragLength = Len
    ),
    get(Buffer, line_number, Start, LineNo),
    (   Msg == @default
    ->  get(Buffer, contents, Start, FragLength, String)
    ;   String = Msg
    ),
    get(Buffer, name, BufName),
    get(L, browser, ListBrowser),
    send(ListBrowser, append,
         new(DI, dict_item('',
                           string('%s:%d: %s',
                                  BufName, LineNo, String),
                           new(F, fragment(Buffer, Start, FragLength))))),
    new(_, emacs_mark_hyper(DI, F, dict_item, fragment)),
    send(ListBrowser, normalise, DI).

goto(L, Fragment:fragment) :->
    "Indicate the fragment"::
    send(L, used, @on),
    get(Fragment, text_buffer, TB),
    get(TB, open, tab, Frame),
    get(Frame, editor, Editor),
    get(L, message, Method),
    send(Editor, Method, Fragment?start).

:- pce_end_class.

:- pce_begin_class(emacs_mark_hyper, hyper).

unlink_to(H) :->
    get(H, from, From),
    free(From),
    free(H).
unlink_from(H) :->
    get(H, to, To),
    free(To),
    free(H).

:- pce_end_class.


