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

:- module(pce_statistics, []).

:- use_module(library(pce)).
:- require([ send_list/3
           ]).

:- pce_begin_class(man_statistics, man_frame,
                   "Statistics tool").


variable(timer, timer*, get,
         "Timer that forces updates").

initialise(S, Manual:man_manual) :->
    "Create from manual"::
    send(S, send_super, initialise, Manual, 'PCE Statistics'),

    send(S, append, new(D, dialog)),
    send(D, append, new(CU, text_item(core_in_use,    0))),
    send(D, append, new(CW, text_item(core_wasted,    0))),
    send(D, append, new(OU, text_item(objects_in_use, 0))),
    Items = [CU, CW, OU],
    send_list(Items, length, 10),
    send_list(Items, editable, @off),

    new(T, timer(5,
                 and(message(CU, selection, @pce?core_usage),
                     message(CW, selection, @pce?core_wasted),
                     message(OU, selection, @pce?objects_allocated -
                                            @pce?objects_freed)))),

    send(S, slot, timer, T),
    send(T, execute),
    send(T, start),

    send(D, append, button(update, message(T, execute))),
    send(D, append, button(help,   message(S, help))),
    send(D, append, button(quit,   message(S, quit))),

    send(S, open).


unlink(S) :->
    send(S?timer, stop),
    send(S, send_super, unlink).

:- pce_end_class.
