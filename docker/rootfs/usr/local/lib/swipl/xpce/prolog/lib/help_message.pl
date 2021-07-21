/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1997-2011, University of Amsterdam
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

:- module(pce_help_messages, []).
:- use_module(library(pce)).

:- pce_global(@help_message_window, new(help_message_window)).

:- pce_begin_class(help_message_window, dialog,
                   "Window to display <-help_message").

class_variable(background, colour,
               when(@pce?window_system == windows,
                    win_infobk,
                    burlywood1),
               "Ballon background").
class_variable(foreground, colour,
               when(@pce?window_system == windows,
                    win_infotext,
                    black)).

variable(handler,       handler,        get, "Handler for intercept").
variable(message,       string*,        get, "Currently displayed message").

initialise(W) :->
    send(W, slot, handler,
         handler(mouse, message(W, try_hide, @event))),
    send_super(W, initialise),
    get(W, frame, Frame),
    send(Frame, kind, popup),
    send(Frame, sensitive, @off),
    send(Frame, border, 0),
    send(Frame?tile, border, 0),
    send(W, gap, size(5, 2)),
    (   get(@pce, window_system, windows) % Hack
    ->  send(W, pen, 1)
    ;   send(W, pen, 0)
    ),
    send(W, append, new(L, label(feedback, '', normal))),
    send(L, length, 0),
    send(Frame, create).

owner(W, Owner:[any]*) :->
    "Maintain hyperlink to the owner"::
    (   Owner == @nil
    ->  send(W, delete_hypers, owner)
    ;   Owner == @default
    ->  true                        % no change
    ;   new(_, help_hyper(Owner, W, help_baloon, owner))
    ).
owner(W, Owner:any) :<-
    get(W, hypered, owner, Owner).


try_hide(W, Ev:event) :->
    get(W, owner, Owner),
    (   send(Ev, inside, Owner),
        (   send(Ev, is_a, loc_move)
        ;   send(Ev, is_a, loc_still)
        )
    ->  %send(@pce, format, '%O: Move/still event\n', Owner),
        get(W, message, OldMsg),
        (   get(Owner, help_message, tag, Ev, Msg)
        ->  %send(@pce, format, '%O: yielding %s\n', Owner, Msg),
            (   OldMsg \== @nil,
                send(Msg, equal, OldMsg)
            ->  (   send(Ev, is_a, loc_still)
                ->  send(W, adjust_position, Ev)
                ;   true
                )
            ;   send(W, feedback, Msg, Ev)
            )
        ;   (   get(W, message, @nil)
            ->  true
            ;   send(W, feedback, @nil, Ev)
            )
        )
    ;   send(W, owner, @nil),
        send(W, hide),
        fail                        % normal event-processing
    ).


hide(W) :->
    "Remove from the display"::
    send(W, show, @off),
    get(W, handler, H),
    send(W?display?inspect_handlers, delete, H).


feedback(W, S:string*, Ev:event, For:[any]*) :->
    "Display window holding string and grab pointer"::
    send(W, owner, For),
    send(W, slot, message, S),
    (   S == @nil
    ->  send(W, show, @off)
    ;   get(W, member, feedback, L),
        send(L, selection, S),
        send(W, layout),
        send(W?frame, fit),
        send(W, adjust_position, Ev),
        send(W?display, inspect_handler, W?handler)
    ).


adjust_position(W, Ev:event) :->
    "Fix the position of the feedback window"::
    get(Ev, position, W?display, P),
    get(P, plus, point(5,5), point(FX, FY)),
    send(W?frame, set, FX, FY),
    send(W?frame, expose).

:- pce_end_class.


attribute_name(tag,     help_tag).
attribute_name(summary, help_summary).

:- pce_extend_class(visual).

help_message(Gr, What:{tag,summary}, Msg:string*) :->
    "Associate a help message"::
    attribute_name(What, AttName),
    (   Msg == @nil
    ->  send(Gr, delete_attribute, AttName)
    ;   send(Gr, attribute, AttName, Msg)
    ).
help_message(V, What:{tag,summary}, _Ev:[event], Msg:string) :<-
    attribute_name(What, AttName),
    get(V, attribute, AttName, Msg).

:- pce_end_class.


:- pce_extend_class(graphical).

show_help_message(Gr, What:name, Ev:event) :->
    find_help_message(Gr, What, Ev, Owner, Msg),
    send(@help_message_window, feedback, Msg, Ev, Owner).


find_help_message(Gr, What, Ev, Gr, Msg) :-
    get(Gr, help_message, What, Ev, Msg),
    !.
find_help_message(Gr, What, Ev, Owner, Msg) :-
    get(Gr, contained_in, Container),
    find_help_message(Container, What, Ev, Owner, Msg).

:- pce_end_class.


:- pce_extend_class(menu).

help_message(Gr, What:{tag,summary}, Ev:[event], Msg:string) :<-
    "Fetch associated help message"::
    (   get(Gr, item_from_event, Ev, Item),
        get(Item, help_message, What, Msg)
    ->  true
    ;   get(Gr, get_super, help_message, What, Ev, Msg)
    ).

:- pce_end_class.


:- pce_begin_class(help_hyper, hyper,
                   "Hyper between help-balloon and owner").

unlink_from(H) :->
    "->hide the <-to part"::
    get(H, to, Part),
    (   object(Part)
    ->  send(Part, hide)
    ;   free(Part)
    ),
    free(H).

:- pce_end_class.

                 /*******************************
                 *           REGISTER           *
                 *******************************/

register_help_message_window :-
    send(@display, inspect_handler,
         handler(loc_still,
                 message(@receiver, show_help_message, tag, @event))),
    send(@display, inspect_handler,
         handler(help,
                 message(@receiver, show_help_message, summary, @event))).

:- initialization
   register_help_message_window.
