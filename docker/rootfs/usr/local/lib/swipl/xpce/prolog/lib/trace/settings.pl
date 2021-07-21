/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2001-2013, University of Amsterdam
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

:- module(trace_settings,
          [ trace_settings/0
          ]).
:- use_module(library(pce)).
:- use_module(util).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setting(?Name, ?ValueSet, ?Comment)
        Defines the settable attributes for the GUI based tracer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

setting(show_unbound,
        [true, false],
        '`Bindings'' window shows unbound variables').
setting(cluster_variables,
        [true, false],
        '`Bindings'' window clusters variables with the same value').
setting(portray_codes,
        [ true, false ],
        'Portray code-lists as text').
setting(stack_depth,
        int(2, infinite),
        'Number of stack-frames displayed').
setting(choice_depth,
        int(0, infinite),
        'Number of choice-points displayed').
setting(list_max_clauses,
        int(2, infinite),
        'Maximum number of clauses decompiled when listing dynamic code').
setting(auto_raise,
        [true, false],
        'Automatically raise the tracer window').
setting(auto_close,
        [true, false],
        'Close window on n(odebug) and a(abort)').
setting(use_pce_emacs,
        [true, false],
        'Use Built-in PceEmacs editor').

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
trace_setting/0
        Show the current settings, and allows for editing them.  There
        isn't a help yet.  Modal.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

trace_settings :-
    new(D, dialog('Trace Settings')),
    send(D, application, @prolog_gui),
    forall(setting(Name, _, _), make_item(D, Name)),
    send(D, append, new(A, button(apply, and(message(D, apply),
                                             message(D, destroy))))),
    send(D, append, button(reset, message(D, restore))),
    send(D, append, button(cancel, message(D, destroy))),
    send(D, default_button, A),
    send(A, active, @off),
    send(D, modal, application),
    (   send(@event, instance_of, event),
        get(@event?receiver, frame, Frame)
    ->  send(D, transient_for, Frame),
        get(Frame?area, center, Pos)
    ;   Pos = @default
    ),
    send(D, open_centered, Pos).


make_item(D, Name) :-
    setting(Name, ValueSet, Comment),
    is_list(ValueSet),
    !,
    send(D, append, new(M, menu(Name, marked,
                                message(@prolog, set_trace_setting,
                                        Name, @arg1)))),
    send(M, layout, horizontal),
    send_list(M, append, ValueSet),
    send(M, default, ?(@prolog, trace_setting, Name)),
    send(M, help_message, tag, Comment).
make_item(D, Name) :-
    setting(Name, int(Low, infinite), Comment),
    send(D, append, new(TI, text_item(Name, Low,
                                      message(@prolog, set_trace_setting,
                                              Name, @arg1)))),
    send(TI, length, 5),
    send(TI, default, ?(@prolog, trace_setting, Name)),
    send(TI, help_message, tag, Comment).

set_trace_setting(Name, Value) :-
    trace_setting(Name, _, Value).

