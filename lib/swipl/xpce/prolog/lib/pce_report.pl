/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1999-2011, University of Amsterdam
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

:- module(pce_reporter, []).
:- use_module(library(pce)).

:- pce_begin_class(reporter, label,
                   "Label for reporting").

variable(hor_stretch,   int := 100,     get, "Stretch-ability").
variable(hor_shrink,    int := 100,     get, "Shrink-ability").
variable(error_delay,   int := 5,       both, "Delay after error").
variable(warning_delay, int := 2,       both, "Delay after warning").
variable(delay_next_to, date*,          get, "Delay errors and warnings").

initialise(R) :->
    send_super(R, initialise, reporter, ''),
    send(R, elevation, -1),
    send(R, border, 2),
    send(R, reference, point(0, R?height)).

report(R, Status:name, Fmt:[char_array], Args:any ...) :->
    (   get(R, delay_next_to, DelayTo),
        DelayTo \== @nil,
        get(DelayTo, difference, new(date), ToGo),
        ToGo > 0,
        (   vital(Status)
        ->  send(timer(ToGo), delay),
            fail
        ;   true
        )
    ->  true
    ;   Msg =.. [report, Status, Fmt | Args],
        colour(Status, Colour),
        send(R, colour, Colour),
        delay(R, Status, Date),
        send(R, slot, delay_next_to, Date),
        send_super(R, Msg)
    ).

colour(error, red) :- !.
colour(_, @default).

delay(R, warning, Date) :-
    get(R, warning_delay, Delay),
    Delay > 0,
    !,
    new(Date, date),
    send(Date, advance, Delay).
delay(R, error, Date) :-
    get(R, error_delay, Delay),
    Delay > 0,
    !,
    new(Date, date),
    send(Date, advance, Delay).
delay(_, _, @nil).

vital(warning).
vital(error).
vital(inform).

:- pce_end_class.


:- pce_begin_class(report_dialog, dialog,
                   "Dialog window holding reporter").

variable(reporter, reporter, get, "Associated reporter").
delegate_to(reporter).

initialise(D) :->
    send_super(D, initialise),
    send(D, gap, size(0, 0)),
    send(D, resize_message, message(D, layout, @arg2)),
    send(D, append, new(R, reporter)),
    send(D, slot, reporter, R).

:- pce_end_class.
