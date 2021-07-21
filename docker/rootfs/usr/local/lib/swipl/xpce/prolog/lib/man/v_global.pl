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

:- module(man_object_browser, []).

:- use_module(library(pce)).
:- use_module(util).
:- require([ default/3
           , send_list/3
           ]).

:- pce_begin_class(man_object_browser, man_frame,
                   "Gobal object browser").

variable(objects,       man_module,             get).

initialise(OB, Manual:man_manual) :->
    "Create from Manual"::
    send(OB, send_super, initialise, Manual, 'Object Browser'),

    get(Manual, module, objects, @on, Module),
    send(OB, slot, objects, Module),

    new(B, man_summary_browser(man_summary, size(70, 15))),
    send(B?image, tab_stops, vector(20, 200)),
    send(B, name, browser),
    dialog(Dialog),

    send(OB, append, B),
    send(Dialog, below, B),
    send(OB, fill, ''),

    send(OB, open).


                /********************************
                *            DIALOG             *
                ********************************/

dialog(D) :-
    new(D, dialog),
    new(OB, D?frame),
    send(D, append, new(A, menu(show, marked, @nil))),
    send(A, layout, horizontal),
    send_list(A, append, [documented, all]),
    send(D, append, new(SS, text_item(search, regex(''),
                                      message(D?apply_member, execute))),
         right),
    send(SS, length, 15),
    send(D, append, button(apply, message(OB, fill,
                                          SS?selection, A?selection))),

    send(D, append, button(help,  message(OB, help))),
    send(D, append, button(quit,  message(OB, quit))).


                /********************************
                *             FILL              *
                ********************************/

fill(OB, Pattern:regex, What:[name]) :->
    "Fill with all global objects matching pattern"::
    default(What, documented, Show),
    get(OB, member, browser, B),
    send(B, clear),
    new(Chain, chain),
    (   Show == documented
    ->  get(@manual, module, objects, @on, ObjModule),
        send(ObjModule?id_table, for_some,
             message(@prolog, append_card, Chain, Pattern, @arg2))
    ;   send(@pce, for_name_reference,
             message(@prolog, append_object, Chain, Pattern, @arg1))
    ),
    get(Chain, size, S),
    send(OB, report, progress, 'Sorting %d objects ...', S),
    send(Chain, sort, ?(@arg1?reference, compare, @arg2?reference)),
    send(OB, report, done),
    send(B, members, Chain).


append_object(Chain, Pattern, Ref) :-
    new(G, man_global(Ref)),
    (   get(G, man_summary, Summary),
        send(Pattern, search, Summary)
    ->  send(Chain, append, G)
    ;   true
    ).


append_card(Chain, Pattern, Card) :-
    get(Card, identifier, Id),
    atom_concat('O.', Name, Id),
    get(Card, summary, S0),
    (   S0 == @nil
    ->  S1 = @default
    ;   S1 = S0
    ),
    new(G, man_global(Name, S1)),
    (   get(G, man_summary, Summary),
        send(Pattern, search, Summary)
    ->  send(Chain, append, G)
    ;   true
    ).

                /********************************
                *          COMMUNICATION        *
                ********************************/

selected(OB, Obj:object*) :->
    "Set the selection"::
    get(OB, member, browser, B),
    send(B, selected, Obj).


release_selection(OB) :->
    "Clear the selection"::
    get(OB, member, browser, B),
    send(B, release_selection).

:- pce_end_class.

