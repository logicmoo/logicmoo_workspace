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

:- module(pce_make_dialog,
          [ make_dialog/2
          ]).
:- meta_predicate make_dialog(-, :).

:- use_module(library(pce)).
:- require([ forall/2
           , member/2
           , memberchk/2
           , send_list/3
           , strip_module/3
           ]).


make_dialog(Dialog, Id) :-
    strip_module(Id, Module, TheId),
    make_dialog(Dialog, Module, TheId).

make_dialog(Dialog, Module, TheId) :-
    Module:dialog(TheId, Attributes),
    memberchk(object := Dialog, Attributes),
    do(make_dialog_item,   parts,         Attributes),
    do(modify,             modifications, Attributes),
    do(popups,             popups,        Attributes),
    do(behaviour(Module),  behaviour,     Attributes),
    do(layout(Dialog),     layout,        Attributes),
    do(initialise,         initialise,    Attributes).

do(Goal, Attribute, List) :-
    memberchk(Attribute := Value, List),
    !,
    maplist(Goal, Value).
do(_, _, _).


                 /*******************************
                 *            PARTS             *
                 *******************************/

make_dialog_item(Var := NewTerm) :-
    new(Var, NewTerm).

                 /*******************************
                 *        MODIFICATIONS         *
                 *******************************/

modify(Ref := List) :-
    modify(List, Ref).

modify([], _).
modify([Attr := Value|T], Ref) :-
    send_list(Ref, Attr, Value),
    modify(T, Ref).


                 /*******************************
                 *            POPUPS            *
                 *******************************/

popups(Ref := [ PopupSelector := NewTerm, Attributes ]) :-
    new(Popup, NewTerm),
    modify(Popup := Attributes),
    send(Ref, PopupSelector, Popup).


                 /*******************************
                 *            LAYOUT            *
                 *******************************/

layout(Dialog, below(I1, I2)) :-
    !,
    attach(Dialog, I1, I2),
    send(I1, below, I2).
layout(Dialog, right(I1, I2)) :-
    !,
    attach(Dialog, I1, I2),
    send(I1, right, I2).
layout(Dialog, position(I1, Pos)) :-
    send(Dialog, display, I1, Pos).
layout(Dialog, area(I1, area(X,Y,W,H))) :-
    send(I1, auto_align, @off),
    send(I1, do_set, X, Y, W, H),
    send(Dialog, display, I1).

attach(Dialog, I1, _I2) :-
    get(I1, device, Dialog),
    !.
attach(Dialog, _I1, I2) :-
    get(I2, device, Dialog),
    !.
attach(Dialog, _I1, I2) :-
    send(Dialog, append, I2).


                 /*******************************
                 *            DYNAMICS          *
                 *******************************/

behaviour(Module, Ref := List) :-
    forall(member(Attr := Value, List), Module:send(Ref, Attr, Value)).


                 /*******************************
                 *           INITIALISE         *
                 *******************************/

initialise(_Name := Code) :-           % compatibility
    !,
    send(Code, forward).
initialise(Goal) :-
    Goal.
