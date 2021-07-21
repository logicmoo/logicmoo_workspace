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

:- module(pce_unclip, []).
:- use_module(library(pce)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library deals with showing graphicals   that  are partly clipped by
the window on which  they  are  displayed.   It  is  used  by  the class
toc_image from library(pce_toc) to  show   nodes  that  (typically) have
their right-side clipped and provides a  convient mechanism to deal with
a few long labels in a relatively small window.

It is upto the clipped graphical to  detect the mouse is positioned over
it   and   part   of   the   graphical     is    clipped.   The   method
`graphical->clipped_by_window' can be used to   detect  the graphical is
(partly) obscured.

For an example, please start  the   SWI-Prolog  manual  browser using ?-
help.   The   source-code   that   attaches     this   library   is   in
`toc_image->entered'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_extend_class(graphical).

clipped_by_window(Gr) :->
    "Test if graphical is clipped by window border"::
    get(Gr, window, Window),
    get(Window, visible, Visible),
    get(Gr, absolute_position, Window, point(X,Y)),
    get(Gr, area, area(_,_,W,H)),
    \+ send(Visible, inside, area(X,Y,W,H)).

:- pce_end_class(graphical).


                 /*******************************
                 *           INVISIBLE          *
                 *******************************/

:- pce_global(@unclip_window, new(pce_unclip_window)).

:- pce_begin_class(pce_unclip_window, window).

variable(handler, handler, get, "Handler used to fetch all events").
variable(busy,    bool := @off, none, "Handling attach/detach?").

class_variable(background, colour, azure).

initialise(W) :->
    send_super(W, initialise),
    get(W, frame, Fr),
    send(Fr, kind, popup),
    send(Fr, sensitive, @off),
    send(W, pen, 0),
    send(Fr, border, 1),
    send(Fr?tile, border, 0),
    send(W, slot, handler,
         handler(any, message(W, unclipped_event, @event))).

attach(W, To:graphical) :->
    "Attach to graphical"::
    (   get(W, slot, busy, @off)
    ->  send(W, slot, busy, @on),
        call_cleanup(attach(W, To),
                     send(W, slot, busy, @off))
    ;   true
    ).

attach(W, To) :-
    get(To, window, ToWindow),
    (   get(W, hypered, mirroring, Old)
    ->  send(W, delete_hypers, mirroring),
        (   get(Old, window, ToWindow)
        ->  true
        ;   send(Old, grab_pointer, @off),
            send(ToWindow, grab_pointer, @on)
        )
    ;   get(W, handler, H),
        send(ToWindow, grab_pointer, @on),
        send(@display?inspect_handlers, prepend, H)
    ),
    new(_, hyper(To, W, mirror, mirroring)),
    send(W, update),
    get(To, display_position, point(X,Y)),
    (   get(@pce, window_system, windows)
    ->  Border = 0                  % TBD: Fix inside kernel
    ;   get(W, border, Border)
    ),
    send(W, open, point(X-Border,Y-Border)),
    send(W, expose).

update(W) :->
    "Update for changed receiver"::
    send(W, clear),
    (   get(W, hypered, mirroring, Gr)
    ->  get(Gr, clone, Clone),
        (   get(@pce, window_system, windows)
        ->  get(Clone, size, size(W0, H0)),
            send(W, size, size(W0+1, H0+1))
        ;   get(Clone, size, Size),
            send(W, size, Size)
        ),
        send(Clone, set, 0, 0),
        send(W, display, Clone)
    ;   true
    ).


detach(W) :->
    "Detach and hide"::
    (   get(W, slot, busy, @off)
    ->  send(W, slot, busy, @on),
        call_cleanup(detach(W),
                     send(W, slot, busy, @off))
    ;   true
    ).

detach(W) :-
    (   get(W, hypered, mirroring, Gr)
    ->  send(W, delete_hypers, mirroring),
        send(W, clear),
        send(W, show, @off),
        get(W, handler, H),
        send(Gr?window, grab_pointer, @off),
        send(@display?inspect_handlers, delete, H)
    ;   true
    ).


unclipped_event(W, Ev:event) :->
    (   send(Ev, is_a, loc_move),
        (   \+ send(Ev, inside, W)
        ;   get(W, hypered, mirroring, Gr),
            \+ send(Ev, inside, Gr?window)
        )
    ->  send(W, detach)
    ;   (   send(Ev, is_a, button)
        ;   send(Ev, is_a, keyboard)
        ;   send(Ev, is_a, wheel)
        )
    ->  send(W, detach),
        fail                        % normal event-processing
    ).

:- pce_end_class(pce_unclip_window).
