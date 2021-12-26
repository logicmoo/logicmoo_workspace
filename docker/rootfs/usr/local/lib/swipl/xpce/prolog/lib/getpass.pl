/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2011, University of Amsterdam
                         VU University Amsterdam
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

:- module(getpass,
          [ getpass/1
          ]).
:- use_module(library(pce)).
:- require([ between/3
           , default/3
           , forall/2
           ]).

/** <module> Request passwords using XPCE

This module provides getpass/1 as a simple  way to prompt for a password
and  the  class  =passwd_item=,  which  defines  a  text_item  with  ***
feedback.
*/

%!  getpass(-Passwd)
%
%   Asks the user for a password.  Provides feedback as `*' characters
%   The password typed is returned as a Prolog list.  All intermediate
%   results use XPCE strings rather than atoms to avoid finding the
%   typed password by inspecting the Prolog or XPCE symbol-table.

getpass(Pass) :-
    send(new(D, dialog('Enter Password')), append, new(I, passwd_item)),
    send(D, append, button(ok, message(D, return, I?selection))),
    send(D, append, button(cancel, message(D, return, @nil))),
    send(D, default_button, ok),
    (   send(@event, instance_of, event)
    ->  get(@event, position, @display, Pointer)
    ;   Pointer = @default
    ),
    get(D, confirm_centered, Pointer, RVal),
    (   RVal == @nil
    ->  send(D, destroy),
        fail
    ;   pce_string_to_list(RVal, RawPass),
        send(D, destroy),
        Pass = RawPass
    ).

pce_string_to_list(String, List) :-
    get(String, size, Size),
    pce_string_to_list(0, Size, String, List).

pce_string_to_list(N, N, _, []) :- !.
pce_string_to_list(I, N, S, [C|T]) :-
    get(S, character, I, C),
    NI is I + 1,
    pce_string_to_list(NI, N, S, T).


                 /*******************************
                 *       CLASS PASSWD_ITEM      *
                 *******************************/

:- pce_begin_class(passwd_item, text_item, "text-item for entering a passwd").

variable(shadow,        text_item,      get, "The real (invisible) item").

initialise(I, Name:[name], Message:[message]) :->
    default(Name, password, TheName),
    send(I, send_super, initialise, TheName, string('')),
    send(I, slot, shadow, text_item(TheName, string(''), Message)).


unlink(I) :->
    get(I, shadow, Shadow),
    free(Shadow),
    send(I, send_super, unlink).


event(I, Ev:event) :->
    get(I, shadow, Shadow),
    (   get(Shadow, message, @default),
        get(Ev, id, 13)
    ->  send(I, send_super, event)
    ;   send(Shadow, event, Ev),
        get(Shadow, selection, String),
        get(Shadow, caret, Caret),
        get(String, size, Size),
        make_star_string(Size, Stars),
        send(I, selection, Stars),
        send(I, caret, Caret),
        (   send(Ev, is_a, keyboard)
        ->  true
        ;   send(I, send_super, event, Ev)
        )
    ).


selection(I, Passwd:string) :<-
    get(I, shadow, Shadow),
    get(Shadow, selection, Passwd).


make_star_string(Size, S) :-
    new(S, string),
    forall(between(1, Size, _), send(S, append, '*')).

:- pce_end_class.
