/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1999-2011, University of Amsterdam
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

:- module(pce_by_operator,
          [ (->>)/2,
            op(800, yfx, ->>),          % send/get
            op(800, xfx, *>>),          % send/get super
            op(800, xfx, =>>)
          ]).
:- use_module(library(pce)).

:- meta_predicate
    ->>(+, :).


                 /*******************************
                 *            SEND/GET          *
                 *******************************/

%!  ->>(Object, Message) is semidet.
%
%   Send an XPCE message.
%
%   @tbd make this a goal-expansion too.

Obj->>M:Msg :-
    action(Obj, [Msg], M).

action(A, _, _) :-
    var(A),
    !,
    throw(error(instantiation_error, (->>)/2)).
action(A = Obj, Sels, M) :-
    !,
    gets(Sels, Obj, A, M).
action(Obj->>Sel1, Sel, M) :-
    !,
    action(Obj, [Sel1|Sel], M).
action(Obj, Sels, M) :-
    !,
    sends(Sels, Obj, M).

gets([Sel], Obj, A, M) :-
    !,
    get(Obj, M:Sel, A).
gets([S1|Sels], Obj, A, M) :-
    get(Obj, M:S1, O1),
    gets(Sels, O1, A, M).

sends([Sel], Obj, M) :-
    !,
    send(Obj, M:Sel).
sends([S1|Sels], Obj, M) :-
    get(Obj, M:S1, O1),
    sends(Sels, O1, M).


                 /*******************************
                 *        SEND/GET-SUPER        *
                 *******************************/

expand(Rec*>>Msg, Expanded) :-
    !,
    (   nonvar(Rec),
        Rec = (A = Obj)
    ->  Expanded = get_super(Obj, Msg, A)
    ;   Expanded = send_super(Rec, Msg)
    ).

                 /*******************************
                 *          SLOT ACCESS         *
                 *******************************/

expand(Rec=>>Msg, Expanded) :-
    !,
    (   nonvar(Rec),
        Rec = (A = Obj)
    ->  Expanded = get(Obj, slot(Msg, A))
    ;   Msg =.. List,
        EMsg =.. [slot|List],
        Expanded = send(Rec, EMsg)
    ).

pce_ifhostproperty(prolog(sicstus),
[(:- multifile(user:goal_expansion/3)),
 (user:goal_expansion(G, M, E) :-
        M \== pce_by_operator,
        expand(G, E)
 )
],
[(:- multifile(system:goal_expansion/2)),
 (system:goal_expansion(G, E) :-
        expand(G, E)            )
]).
