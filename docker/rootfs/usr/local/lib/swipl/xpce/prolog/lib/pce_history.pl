/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2012, University of Amsterdam
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

:- module(pce_history, []).
:- use_module(library(pce)).
:- use_module(library(toolbar)).
:- use_module(library(hyper)).
:- require([ default/3
           ]).

resource(back, image, image('16x16/back.xpm')).
resource(forw, image, image('16x16/forward.xpm')).

:- pce_begin_class(history, object,
                   "Manage a location history").

variable(backward_list, chain,               get,  "Backward history").
variable(forward_list,  chain,               get,  "Forward history").
variable(message,       code*,               both, "Message executed").
variable(action,        {forward,backward}*, get,  "Current action").


initialise(H, Msg:[code]*) :->
    default(Msg, @nil, Message),
    send_super(H, initialise),
    send(H, slot, message, Message),
    send(H, slot, backward_list, new(chain)),
    send(H, slot, forward_list, new(chain)).

:- pce_group(history).

location(H, Loc:any) :->
    "Tell history we go some location"::
    (   get(H, action, @nil)
    ->  get(H, forward_list, Forward),
        get(H, backward_list, Backward),
        (   send(Backward?head, equal, Loc)
        ->  true
        ;   send(Forward, clear),
            send(Backward, prepend, Loc)
        )
    ;   true
    ),
    ignore(send(H, send_hyper, button, activate)).

current(H, Loc:any) :<-
    "Return current location"::
    get(H?backward_list, head, Loc).

delete(H, Loc:any) :->
    "Delete object from history"::
    send(H?backward_list, delete_all, Loc),
    send(H?forward_list, delete_all, Loc).

forward(DW, Obj:[any]) :->
    "Forward into history"::
    get(DW, forward_list, Forward),
    get(DW, backward_list, Backward),
    (   Obj == @default
    ->  Roll = 1
    ;   get(Forward, index, Obj, Roll)
    ),
    roll(Roll, Forward, Backward),
    get(Backward, head, Here),
    send(DW, goto, forward, Here).

backward(DW, Obj:[any]) :->
    "Backward into history"::
    get(DW, forward_list, Forward),
    get(DW, backward_list, Backward),
    (   Obj == @default
    ->  Roll = 1
    ;   get(Backward, index, Obj, I),
        Roll is I-1
    ),
    roll(Roll, Backward, Forward),
    get(Backward, head, Here),
    send(DW, goto, backward, Here).

roll(N, Ch1, Ch2) :-
    succ(N2, N),
    !,
    get(Ch1, delete_head, Obj),
    send(Ch2, prepend, Obj),
    roll(N2, Ch1, Ch2).
roll(_, _, _).

goto(DW, Dir:{forward,backward}, Obj:any) :->
    get(DW, message, Msg),
    Msg \== @nil,
    setup_call_cleanup(
        send(DW, slot, action, Dir),
        send(Msg, forward, Obj),
        send(DW, slot, action, @nil)).

can_backward(H) :->
    "Test whether there is backward history available"::
    get(H, backward_list, Backward),
    get(Backward, size, Count),
    Count > 1.

can_forward(H) :->
    "Test whether there is forward history available"::
    \+ send(H?forward_list, empty).

:- pce_group(gui).

update_menu(DW, Popup:popup, MaxLen:[int]) :->
    "Update forward/backward popup"::
    default(MaxLen, 10, Max),
    get(Popup, name, Dir),
    (   Dir == forward
    ->  get(DW, forward_list, Targets)
    ;   get(DW, backward_list, Targets)
    ),
    get(Targets, size, Len),
    (   Len > Max
    ->  get(Targets, sub, 0, Max, MenuTargets)
    ;   MenuTargets = Targets
    ),
    send(Popup, clear),
    send(MenuTargets, for_all,
         message(Popup, append,
                 create(menu_item,
                        @arg1, @default, @arg1?print_name))).


button(DW, Dir:{forward,backward}, B:tool_button) :<-
    "Create tool-buttons for manipulating the history"::
    (   Dir == forward
    ->  Img = forw,
        Tag = forward_history
    ;   Img = back,
        Tag = backward_history
    ),
    atom_concat(can_, Dir, Can),
    new(B, tool_button(message(DW, Dir),
                       image(resource(Img)),
                       Tag,
                       message(DW, Can))),
    send(B, popup,
         new(P, popup(Dir, message(DW, Dir, @arg1)))),
    send(P, update_message, message(DW, update_menu, P)),
    send(B, recogniser,
         handler_group(timed_click_gesture(
                           left, @default,
                           message(@receiver, execute),
                           message(@receiver, status, preview),
                           message(B, cancel)),
                       new(timed_popup_gesture(P, left)))),
    new(_, partof_hyper(DW, B, button, history)).

:- pce_end_class(history).

:- pce_begin_class(timed_click_gesture, click_gesture,
                   "Click gesture that cancels").

variable(timer,  timer,   get, "Associated timer").
variable(window, window*, get, "Window I'm associated to").

initialise(G,
           Button:button=[button_name],
           Modifier:modifier=[modifier],
           Message:message=[code]*,
           Preview:preview=[code]*,
           Cancel:cancel=[code]*,
           Time:time=[float]) :->
    "Initialise as click_gesture with timeout"::
    default(Time, 0.3, TheTime),
    send_super(G, initialise,
               Button, Modifier, single,
               Message, Preview, Cancel),
    send(G, slot, timer, timer(TheTime, message(G, timeout))).

initiate(G, Ev:event) :->
    send_super(G, initiate, Ev),
    send(G, slot, window, Ev?window),
    send(G?timer, start, once).

timeout(G) :->
    "Button pressed too long; cancel"::
    get(G, window, Window), Window \== @nil,
    get(Window, focus_event, Ev), Ev \== @nil,
    send(G, slot, window, @nil),
    send(G, cancel, Ev).

terminate(G, Ev:event) :->
    send(G?timer, stop),
    send(G, slot, window, @nil),
    send_super(G, terminate, Ev).

:- pce_end_class(timed_click_gesture).

:- pce_begin_class(timed_popup_gesture, popup_gesture,
               "Complement timed_click_gesture").

initiate(G, Ev:event) :->
    get(Ev, receiver, Gr),
    get(Gr, height, Y),
    get(G, slot, current, Popup),
    send(Popup, open, Gr, point(0,Y), @off, @off),
    send(Ev, post, Popup).

:- pce_end_class.
