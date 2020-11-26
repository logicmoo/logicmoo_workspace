/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1995-2013, University of Amsterdam
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

:- module(pce_prompt,
          [ prompter/2
          ]).
:- use_module(library(pce)).
:- require([ checklist/2
           ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines a standard prompter-box for PCE applications.  It is
invoked with:

    prompter(+Tile, +ListOfAttributes)

where each attribute is a term of the form

   +Label:+Type = -Value[/+Default]

Examples:

prompter('Create class',
       [ name:name = Name
       , super:name = Super
       ]).

NOTE:   Package is under development.  Needs support for more types;
        optional/obligatory fields and better error-messages.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@prompter, new(dialog)).
:- pce_global(@prompter_execute_message,
              new(message(@receiver?window?ok_member, execute))).

prompter(Title, Attributes) :-
    send(@prompter, clear),
    checklist(append_prompter(@prompter), Attributes),
    send(@prompter, append,
         button(ok, message(@prompter, return, ok)), next_row),
    send(@prompter, append,
         button(cancel, message(@prompter, return, cancel))),
    send(@prompter?frame, label, Title),
    send(@prompter, fit),
    repeat,
        get(@prompter, confirm_centered, ?(@event, position, @display), OK),
        (   OK == ok
        ->  checklist(read_prompter(@prompter), Attributes),
            !,
            send(@prompter, show, @off)
        ;   !,
            send(@prompter, show, @off),
            fail
        ).


                /********************************
                *      CREATE DIALOG ITEMS      *
                ********************************/

append_prompter(P, Label:Type = Value) :-
    make_dialog_item(DI, Label, Type),
    set_default(Value, DI),
    send(P, append, DI).

                                                  % TBD: specialised types
make_dialog_item(DI, Label, _) :-
    !,
    new(DI, text_item(Label, '', @prompter_execute_message)).


                /********************************
                *          SET DEFAULTS         *
                ********************************/

set_default(Value, DI) :-
    nonvar(Value),
    Value = _RVal/Default,
    !,
    send(DI, selection, Default).
set_default(_, _).

                /********************************
                *           READ VALUES         *
                ********************************/

read_prompter(P, Label:Type = Value) :-
    get(P, member, Label, DI),
    get(DI, selection, V0),
    canonicalise(DI, V0, V1),
    (   get(@pce, convert, V1, Type, Val)
    ->  (   nonvar(Value),
            Value = RVal/_
        ->  RVal = Val
        ;   Value = Val
        )
    ;   send(@display, inform, '%s should be a %s', Label, Type),
        fail
    ).


canonicalise(DI, A, B) :-
    send(DI?class, is_a, text_item),
    !,
    new(S, string(A)),
    send(S, strip),
    get(S, value, B),
    send(S, done).
canonicalise(_, Val, Val).                                % TBD
