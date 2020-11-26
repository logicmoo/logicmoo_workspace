/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2001-2011, University of Amsterdam
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

:- module(man_select_graphical, []).
:- use_module(library(pce)).
:- require([ default/3
           ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library lets the user select an   XPCE graphical by pointing at it.
It is an example of low-level focus and cursor management and is used by
the visual hierarchy and inspector  tools   to  select  objects from the
screen.

A typical sequence using this class is:

select_graphical(Cond, Gr) :-
        get(new(D, select_graphical), select, Cond, Gr),
        send(D, destroy),
        Gr \== @nil.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(select_graphical, dialog,
                   "Select a graphical by clicking it").

variable(condition,      [code],  get, "Condition for target").
variable(current_target, visual*, get, "Current target").

initialise(D, Label:[char_array]) :->
    default(Label, 'Please select an object', TheLabel),
    send_super(D, initialise, 'Selector'),
    send(D, kind, popup),
    send(D, append, label(help, TheLabel)),
    send(D, append, new(Cancel, button(cancel))),
    send(Cancel, alignment, right),
    send(D, resize_message, message(D, layout, @arg2)).

cancel(D) :->
    send(D, grab_pointer, @off),
    send(D, return, @nil).

select(D, Condition:[code], Pos:[point], V:visual) :<-
    "Actually select the target"::
    send(D, slot, condition, Condition),
    send(D, open_centered, Pos),
    send(D, wait),
    send(D, grab_pointer, @on),
    send(D, cursor, crosshair),
    get(D, confirm, V),
    send(D, grab_pointer, @off).

event(D, Ev:event) :->
    "Process and event"::
    (   send_super(D, event, Ev)
    ->  true
    ;   send(Ev, is_a, 27)          % ESC
    ->  send(D, cancel)
    ;   send(Ev, is_a, loc_move)
    ->  send(D, indicate, Ev)
    ;   send(Ev, is_a, ms_left_up),
        send(D, done, Ev)
    ).


indicate(D, Ev:event) :->
    "Indicate current target"::
    (   get(D, target, Ev, Target)
    ->  send(D, cursor, hand2),
        (   get(D, current_target, Target)
        ->  true
        ;   send(D, slot, current_target, Target),
            send(D, indicate_target, Target)
        )
    ;   send(D, cursor, crosshair)
    ).


indicate_target(D, Target:visual) :->
    "Report the current target to the user"::
    portray_object(Target, Term),
    term_to_atom(Term, Text),
    send(D, report, status, '%s', Text).


done(D, Ev:event) :->
    "User has left-clicked"::
    get(Ev, click_displacement, Displacement),
    (   Displacement > 5
    ->  send(D, cancel)
    ;   get(D, target, Ev, Target),
        send(D, return, Target)
    ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is a sequence to find a   graphical satisfying <-condition from the
current cursor location. A similar algorithm is by library(dragdrop).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

target(D, Ev:event, Target:visual) :<-
    "Get current target from event"::
    get(D, condition, Cond),
    get(Ev, inside_sub_window, Frame),
    get(Ev, inside_sub_window, Frame, Window),
    (   Window == D
    ->  !, fail
    ;   get(Window, find, Ev, Cond, Target0),
        target(Target0, Ev, Cond, Target)
    ->  true
    ;   (   Cond == @default
        ;   send(Cond, forward, Frame)
        )
    ->  Target = Frame
    ).

%       Deal with windows displayed on windows

target(Here, Ev, Cond, Target) :-
    send(Here, instance_of, window),
    get(Here, find, Ev, Cond, Target0),
    Target0 \== Here,
    !,
    target(Target0, Ev, Cond, Target).
target(Here, _, @default, Here) :- !.
target(Here, _, Cond, Here) :-
    send(Cond, forward, Here).

:- pce_end_class.

