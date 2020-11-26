/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

:- module(pce_splash_screen, []).
:- use_module(library(pce)).
:- use_module(library(autowin)).
:- use_module(library(hyper)).
:- use_module(library(help_message)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Provide a splash-screen for an application.  The simplest operation is
to open it with a timer that destroys it:

        send(new(splash_screen(Image)), open, Time)
        <continue initialisation>

Note that the window will only disappear after the elapsed time if
events are dispatched (see @display->synchronise).

You can also open it permanently and   associate active regions to parts
of the image:

        new(S, splash_screen(Image)),
        send(S, map, graphical(25,50,100,20),
             message(@prolog, start_my_app),
             'Start application'),
        send(S, map, graphical(25,70,100,20),
             message(@prolog, browse_manual),
             'Browse manual'),
        send(S, open).

The graphical is used to define the sensitive area as well as the cursor
that is visible in the area. It is not displayed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(splash_screen, auto_sized_picture,
                   "Show splash window").

variable(maps,  chain*,  get, "Event-handling map").

initialise(S, Img:image, Label:[char_array]) :->
    send_super(S, initialise, Label),
    get(@display, size, MaxSize),
    send(S, max_size, MaxSize),
    send(S, border, 0),
    (   Label == @default
    ->  send(S, kind, popup)        % do not show border
    ;   true
    ),
    send(S, display, bitmap(Img)).

open(S, Time:[real]*) :->
    send(S, open_centered),
    send(S, wait),
    (   number(Time)
    ->  new(T, timer(Time, message(S, destroy))),
        new(_, partof_hyper(S, T, timer)),
        send(T, start)
    ;   true
    ).

map(S, Gr:[graphical]*, Msg:code, Alt:[char_array]) :->
    "Extend the event-handling map"::
    default(Gr, @nil, TheGr),
    (   get(S, maps, Maps),
        Maps \== @nil
    ->  true
    ;   send(S, slot, maps, new(Maps, chain))
    ),
    send(Maps, append, splash_map(TheGr, Msg, Alt)).

map(S, At:point, Map:splash_map) :<-
    "Find map from position"::
    get(S, maps, Maps), Maps \== @nil,
    object(At, point(X, Y)),
    get(Maps, find,
        or(@arg1?graphical == @nil,
           message(@arg1?graphical, in_event_area, X, Y)),
        Map).

:- pce_group(event).

:- pce_global(@spash_screen_recogniser,
              new(click_gesture(left, '', single,
                                message(@receiver, clicked,
                                        @event?position)))).

event(S, Ev:event) :->
    (   send_super(S, event, Ev)
    ->  true
    ;   send(@spash_screen_recogniser, event, Ev)
    ;   send(Ev, is_a, loc_move),
        (   get(S, map, ?(Ev, position, S), Map)
        ->  get(Map, graphical, Gr),
            (   Gr \== @nil,
                get(Gr, cursor, Cursor),
                Cursor \== @nil
            ->  send(S, cursor, Cursor)
            ;   send(S, cursor, hand2)
            )
        ;   get(S, class_variable_value, cursor, DefCursor),
            send(S, cursor, DefCursor)
        )
    ).

help_message(S, What:{tag,summary}, Ev:event, Alt:string) :<-
    "Show balloon"::
    What == tag,
    get(S, map, ?(Ev, position, S), Map),
    get(Map, alt, Alt),
    Alt \== @nil.

clicked(S, At:point) :->
    "Handle clicked"::
    send(S, expose),
    get(S, map, At, Map),
    get(Map, message, Code),
    get(S, display, Display),
    send(Display, busy_cursor),
    call_cleanup(send(Code, forward, S),
                 send(Display, busy_cursor, @nil)).

:- pce_end_class(splash_screen).

:- pce_begin_class(splash_map, object, "Area in splash screen").

variable(graphical,     graphical*,     get, "Graphical defining area").
variable(message,       code,           get, "Associated code").
variable(alt,           char_array*,    get, "Balloon text").

initialise(SM, Gr:graphical*, Msg:code, Alt:[char_array]*) :->
    send_super(SM, initialise),
    send(SM, slot, graphical, Gr),
    send(SM, slot, message, Msg),
    default(Alt, @nil, TheAlt),
    send(SM, slot, alt, TheAlt).

:- pce_end_class(splash_map).
