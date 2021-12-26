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

:- module(drag_and_drop, []).
:- use_module(library(pce)).
:- require([ default/3
           , ignore/1
           ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Define a gesture that allows to  `drag-and-drop' objects.  The target on
which to drop should understand the method  ->drop, which will be called
with the dropped graphical  as  an   argument.   If  may  also implement
->preview_drop, which will be called to   provide visual feedback of the
drop that will take place when the button is released here.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(drag_and_drop_gesture, gesture,
                   "Drag and drop command-gesture").

variable(target,        visual*,        get,  "Drop target").
variable(warp,          bool,           both, "Pointer in center?").
variable(offset,        point,          get,  "Offset X-y <->caret pointer").
variable(get_source,    function*,      both, "Function to map the source").
variable(source,        any,            get,  "Current source").
variable(active_cursor, cursor*,        get,  "@nil: activated").
variable(select_popup,  popup*,         get,  "Popup for selecting command").

class_variable(warp,   bool,        @on,
               "Pointer in center?").
class_variable(button, button_name, left,
               "Button on which gesture operates").
class_variable(cursor, [cursor],    cross_reverse,
               "Cursor to display.  @default: use graphical").

active_distance(_G, D) :-
    D > 5.

initialise(G, But:button=[button_name],
           M:modifier=[modifier], W:warp=[bool],
           S:get_source=[function]*) :->
    "Create from button, modifiers and warp"::
    send_super(G, initialise, But, M),
    default(W, class_variable(G, warp), Warp),
    default(S, @nil, GS),
    send(G, warp, Warp),
    send(G, get_source, GS),
    send(G, slot, offset, new(point)),
    get(G, class_variable_value, cursor, Cursor),
    send(G, cursor, Cursor).


event(G, Ev:event) :->
    "Pass events to <-select_popup"::
    (   get(G, select_popup, P),
        P \== @nil
    ->  send(P, event, Ev)
    ;   send_super(G, event, Ev)
    ).


verify(_G, Ev:event) :->
    "Only accept as single-click"::
    get(Ev, multiclick, single).


initiate(G, Ev:event) :->
    "Change the cursor"::
    get(Ev, receiver, Gr),
    get(Ev, position, Gr, Offset),
    send(G?offset, copy, Offset),
    send(G, set_source, Ev),
    get(G, cursor, Gr, Cursor),
    send(G, slot, active_cursor, Cursor).


set_source(G, Ev:event) :->
    "Find source from event"::
    get(Ev, receiver, Gr),
    get(G, get_source, Function),
    (   Function == @nil
    ->  send(G, slot, source, Gr)
    ;   get(Function, '_forward', Gr, Source),
        send(G, slot, source, Source)
    ).


cursor(G, Gr:graphical, Cursor:cursor) :<-
    "Create cursor from the graphical"::
    (   get_super(G, cursor, Cursor),
        send(Cursor, instance_of, cursor)
    ->  true
    ;   get(Gr?area, size, size(W, H)),
        (   get(G, warp, @on)
        ->  new(HotSpot, point(W/2, H/2)),
            send(Gr, pointer, HotSpot),
            send(G?offset, copy, HotSpot)
        ;   get(G, offset, HotSpot)
        ),
        new(BM, image(@nil, W, H)),
        send(BM, draw_in, Gr, point(0,0)),
        send(BM, or, image('cross.bm'), point(HotSpot?x-8, HotSpot?y-8)),
        new(Cursor, cursor(@nil, BM, @default, HotSpot))
    ).


activate(G, Ev:event) :->
    "Activate if dragged far enough"::
    (   get(G, active_cursor, Cursor),
        Cursor \== @nil             % still not activated
    ->  (   get(Ev, click_displacement, D),
            active_distance(G, D)   % far enough: activate
        ->  send(Ev?window, focus_cursor, Cursor),
            send(G, slot, active_cursor, @nil)
        )
    ;   true
    ).


drag(G, Ev:event) :->
    "Find possible ->drop target"::
    (   send(G, activate)
    ->  get(G, source, Source),
        (   get(Ev, inside_sub_window, Frame),
            get(Ev, inside_sub_window, Frame, Window),
            get(Window, find, Ev,
                and(@arg1 \== Source,
                    or(and(G?target == @arg1,
                           message(G, move_target, Ev)),
                       message(G, target, Source, Ev, @arg1))),
                _Gr)
        ->  true
        ;   send(G, target, Source, @nil, @nil)
        )
    ;   true
    ).


:- pce_global(@dd_dummy_point, new(point)).

move_target(G, Ev:event) :->
    "The user is dragging the object over a drop-zone"::
    get(G, target, Target),
    get(G, source, Source),
    (   get(Target, send_method, preview_drop, tuple(_, Method)),
        get(Method, argument_type, 1, Type),
        get(Type, check, Source, Src),
        get(Method, argument_type, 2, PosType),
        send(PosType, validate, @dd_dummy_point)
    ->  get(Ev, position, Target, Pos),
        get(Pos, copy, P2),
        send(P2, minus, G?offset),
        send(Target, preview_drop, Src, P2)
    ;   true
    ).


target(G, Source:any, Ev:event*, Gr:graphical*) :->
    "Make the given object the target"::
    (   Gr == @nil
    ->  Target = Gr
    ;   get(Gr, is_displayed, @on),
        container_with_send_method(Gr, drop, Target)
    ->  true
    ),
    ignore((get(G, target, Old),
            send(Old, has_send_method, preview_drop),
            send(Old, preview_drop, @nil))),
    (   get(Target, send_method, preview_drop, tuple(_, Method)),
        get(Method, argument_type, 1, Type),
        get(Type, check, Source, Src)
    ->  (   get(Method, argument_type, 2, PosType),
            send(PosType, validate, @dd_dummy_point)
        ->  get(Ev, position, Target, Pos),
            get(Pos, copy, P2),
            send(P2, minus, G?offset),
            send(Target, preview_drop, Src, P2)
        ;   send(Target, preview_drop, Src)
        )
    ;   true
    ),
    send(G, slot, target, Target).

container_with_send_method(Obj, Method, Obj) :-
    send(Obj, has_send_method, Method).
container_with_send_method(Obj, Method, Container) :-
    get(Obj, contained_in, C0),
    container_with_send_method(C0, Method, Container).


terminate(G, Ev:event) :->
    "->drop to <-target"::
    (   get(G, active_cursor, Cursor),
        Cursor \== @nil
    ->  send(G, slot, active_cursor, @nil),
        send(G, cancel)
    ;   get(G, slot, target, Target),
        send(Ev?window, focus_cursor, @nil),
%       send(G, cursor, @default),
        get(G, source, Source),
        (   Target == @nil
        ->  true
        ;   send(G, target, Source, @nil, @nil),
            get(Target, send_method, drop, tuple(_, Method)),
            get(Method, argument_type, 1, T1),
            get(T1, check, Source, Src),
            get(Target, display, Display),
            (   get(Method, argument_type, 2, Type),
                send(Type, validate, @dd_dummy_point)
            ->  get(Ev, position, Target, Pos),
                get(Pos, copy, P2),
                send(P2, minus, G?offset),
                send(Display, busy_cursor),
                forward(G, Target, Src, P2),
                send(Display, busy_cursor, @nil)
            ;   send(Display, busy_cursor, @default),
                forward(G, Target, Src),
                send(Display, busy_cursor, @nil)
            )
        ),
        send(G, slot, source, @nil)
    ).

%!  forward(+G, +Target, +Src, +Pos)

forward(G, Target, Src, Pos) :-
    (   catch(send(message(@arg1, drop, @arg2, @arg3),
                   forward_receiver, G, Target, Src, Pos), E, true)
    ->  (   nonvar(E)
        ->  print_message(error, E)
        ;   true
        )
    ;   true
    ).

forward(G, Target, Src) :-
    (   catch(send(message(@arg1, drop, @arg2),
                   forward_receiver, G, Target, Src), E, true)
    ->  (   nonvar(E)
        ->  print_message(error, E)
        ;   true
        )
    ;   true
    ).


:- pce_group(command).


%       <-select_command: Commands:chain --> Cmd:name
%
%       This method is to support menu selection of a command often
%       associated with right-dragging instead of left-dragging. It
%       is called from the ->drop at the receiving graphical:
%
%%              drop(Me, Obj:any) :->
%                       (   send(@event, is_a, ms_right_up)
%                       ->  get(@receiver, select_command,
%%                              chain(move, copy), Command),
%                           <move or copy Obj>
%                       ;   <Perform default action>
%                       ).

select_command(G, Commands:chain, Cmd:name) :<-
    "Select a command (normally right-button drag)"::
    send(@display, busy_cursor, @nil),
    new(P, popup(command)),
    send(P, members, Commands),
    send_list(P, append,
              [ gap,
                cancel
              ]),
    get(@event, receiver, Gr),
    get(@event, position, Gr, Pos),
    send(P, open, Gr, Pos),
    send(G, slot, select_popup, P),
%   get(P, window, Window),
%   send(Window, grab_pointer, @on),
    repeat,
        send(@display, dispatch),
        get(P, displayed, @off),
        !,
%       send(Window, grab_pointer, @off),
    (   get(P, selected_item, SI),
        SI \== @nil
    ->  get(SI, value, Cmd)
    ;   Cmd = @nil
    ),
    send(G, slot, select_popup, @nil),
    Cmd \== @nil,
    Cmd \== cancel.

:- pce_end_class.
