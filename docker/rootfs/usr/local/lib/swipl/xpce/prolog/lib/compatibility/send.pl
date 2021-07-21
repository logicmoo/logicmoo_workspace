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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module redefines send/[2,3] such  that they accepts lists, as was
the case upto version 4.3.4 of  xpce/swi-prolog.  This module can only
be loaded on top  of SWI-Prolog as it  uses some rather dirty  tricks.
Portable    code  should  use   the new   send_list/2  and send_list/3
predicates.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- abolish(send, 2).
:- abolish(send, 3).

:- export(pce_principal:'$pce_send'(_,_,_)).
:- import(pce_principal:'$pce_send'(_,_,_)).

:- module_transparent
    send/2,
    send/3.

:- index(send(1, 1)).
:- index(send(1, 1, 1)).

%       send_list(+ListOfObjs, +ListOfSels, [+Arg])
%
%       Send a messages to the carthesian product of ListOfObjs and
%       ListOfSels.

send([], _) :- !.
send(_, []) :- !.
send([Object|Objects], Selectors) :-
    !,
    send(Object, Selectors),
    send(Objects, Selectors).
send(Object, [Selector|Selectors]) :-
    !,
    send(Object, Selector),
    send(Object, Selectors).
send(Object, Selector) :-
    '$pce_send'(Object, Selector, arguments).


send([], _,  _) :- !.
send(_, [], _) :- !.
send(_, _, []) :- !.
send([Object|Objects], Selectors, Arguments) :-
    !,
    send(Object, Selectors, Arguments),
    send(Objects, Selectors, Arguments).
send(Objects, [Selector|Selectors], Arguments) :-
    !,
    send(Objects, Selector, Arguments),
    send(Objects, Selectors, Arguments).
send(Object, Selector, [Argument|Arguments]) :-
    !,
    send(Object, Selector, Argument),
    send(Object, Selector, Arguments).
send(Object, Selector, Argument) :-
    '$pce_send'(Object, Selector, arguments(Argument)).

