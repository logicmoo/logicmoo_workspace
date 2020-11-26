/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2003-2011, University of Amsterdam
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

:- module(pce_password_item,
          [
          ]).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This class realises a GUI password  item, visualising the typed password
as a list of stars. The returned value   is  an XPCE string to avoid the
password entering the XPCE symbol table where it would be much easier to
find.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


                 /*******************************
                 *       CLASS PASSWD_ITEM      *
                 *******************************/

:- pce_begin_class(password_item, text_item, "text-item for entering a passwd").

variable(shadow,        text_item,      get, "The real (invisible) item").

initialise(I, Name:[name], Message:[message]) :->
    default(Name, password, TheName),
    send_super(I, initialise, TheName, string('')),
    send(I, slot, shadow, text_item(TheName, string(''), Message)).


unlink(I) :->
    get(I, shadow, Shadow),
    free(Shadow),
    send_super(I, unlink).


event(I, Ev:event) :->
    get(I, shadow, Shadow),
    (   get(Shadow, message, @default),
        get(Ev, id, 13)             % RET
    ->  send_super(I, event)
    ;   get(Ev, id, 9)              % TAB
    ->  send_super(I, event, Ev)
    ;   send(Shadow, event, Ev),
        send(I, update),
        (   send(Ev, is_a, keyboard)
        ->  true
        ;   send_super(I, event, Ev)
        )
    ).


update(I) :->
    "Update visual representation"::
    get(I, shadow, Shadow),
    get(Shadow, selection, String),
    get(Shadow, caret, Caret),
    get(String, size, Size),
    make_star_string(Size, Stars),
    send_super(I, selection, Stars),
    send(I, caret, Caret).


selection(I, Passwd:string) :<-
    get(I, shadow, Shadow),
    get(Shadow, selection, Passwd).

selection(I, Passwd:string) :->
    get(I, shadow, Shadow),
    send(Shadow, selection, Passwd),
    send(I, update).

make_star_string(Size, S) :-
    new(S, string),
    forall(between(1, Size, _), send(S, append, '*')).

:- pce_end_class(password_item).
