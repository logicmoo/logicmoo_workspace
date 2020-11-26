/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2011, University of Amsterdam
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

:- module(juggler,
          [ juggle_demo/0
          ]).

:- use_module(library(pce)).
:- require([ atomic_list_concat/2
           , forall/2
           , member/2
           , send_list/3
           ]).

juggle_demo :-
    new(_, juggler).

:- pce_begin_class(juggler, frame).

variable(timer, timer, get, "Timer for animation").
variable(speed, int,   get, "Animations/second").

class_variable(geometry, geometry,      '72x72+0+0',    "Default geometry").
class_variable(speed,    int,           10,             "Animations/second").

:- pce_global(@juggler_popup, make_juggler_popup).

make_juggler_popup(P) :-
    new(P, popup),
    new(J, @arg1?frame),
    send_list(P, append,
              [ menu_item(stop,  message(J, stop))
              , menu_item(start, message(J, start))
              , menu_item(speed, message(J, set_speed), @default, @on)
              , menu_item(quit,  message(J, free))
              ]).


initialise(F) :->
    "Create a juggler-window"::
    send(F, send_super, initialise, 'Juggler', popup),
    send(F, append, new(P, picture)),
    send(P, scrollbars, none),
    send(P, popup, @juggler_popup),

    send(P, icon, 'juggler1.bm', 'Juggler'),

    send(P, display, new(Fig, figure)),
    send(Fig, status, 1),
    forall(member(N, [1,2,3,4,5]),
           (atomic_list_concat([juggler, N, '.bm'], IconName),
            new(I, bitmap(IconName)),
            send(I, name, N),
            send(Fig, display, I))),

    send(F, slot, timer, new(T, timer(0.1, message(Fig, next_status)))),
    send(T, start),
    get(F, class_variable_value, speed, Speed),
    send(F, speed, Speed),

    send(F, open).

unlink(F) :->
    send(F?timer, stop),
    send(F, send_super, unlink).

speed(F, Speed:int) :->
    "Set animations/second"::
    Interval is 1 / Speed,
    send(F?timer, interval, Interval),
    send(F, slot, speed, Speed).

stop(F) :->
    "Stop the juggler"::
    send(F?timer, stop).

start(F) :->
    "(Re)Start the juggler"::
    send(F?timer, start).

set_speed(F) :->
    "Popup a dialog to set the speed"::
    new(D, dialog('Speed of juggler')),
    send(D, append, new(S, slider(speed, 1, 50, F?speed,
                                  message(F, speed, @arg1)))),
    send(S, drag, @on),
    send(D, append, button(quit, message(D, free))),
    send(D, open).

:- pce_end_class.
