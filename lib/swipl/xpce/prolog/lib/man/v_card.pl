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

:- module(man_card_editor,
        [
        ]).

:- use_module(library(pce)).
:- use_module(util).

:- pce_autoload(behaviour_item, library('man/behaviour_item')).

:- pce_begin_class(man_card_editor,     man_frame,
                   "Display textual and relational attributes").

                /********************************
                *            CREATE             *
                ********************************/

initialise(CE, Manual:man_manual) :->
    "Create from manual"::
    send(CE, send_super, initialise, Manual, 'Card Viewer'),

    send(CE, append, new(TE, man_editor)),
    send(TE, name, editor),
    send(new(D, dialog), below, TE),
    fill_dialog(D),

    send(CE, create),                         % compute layout before
                                              % setting selection
    send(CE, edit_mode, Manual?edit_mode),
    send(CE, selected, Manual?selection).


fill_dialog(D) :-
    send(D, name, dialog),
    get(D, frame, CE),

    send(D, append, button(help, message(CE, help))),
    send(D, append, button(quit, message(CE, quit))),
    send(D, append,
         new(Goto, behaviour_item(goto, '',
                                  if(@arg1 \== @nil,
                                     message(CE, request_selection,
                                             @arg1)))),
         right),
    send(Goto, advance, clear),
    send(D, append, new(label), right).     % reporter


                /********************************
                *          READ STATUS          *
                ********************************/

editor(CE, Editor) :<-
    "Text attribute editor"::
    get(CE, member, editor, Editor).


                /********************************
                *           EDIT MODE           *
                ********************************/

edit_mode(CE, Val:bool) :->
    "Switch editors edit_mode"::
    get(CE, editor, Editor),
    (   Val == @off
    ->  send(Editor, save_if_modified),
        send(CE, label, 'Card Viewer', 'Card Viewer')
    ;   send(CE, label, 'Card Editor', 'Card Editor')
    ),
    send(Editor, edit_mode, Val).

                 /*******************************
                 *           SELECTION          *
                 *******************************/

selected(CE, Obj:object) :->
    "Display selected object"::
    send(CE?editor, selection, Obj).

:- pce_end_class.

