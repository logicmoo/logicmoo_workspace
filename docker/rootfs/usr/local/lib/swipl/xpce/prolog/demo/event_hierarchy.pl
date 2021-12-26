/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2011, University of Amsterdam
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

:- module(pce_event_hierarchy,
          [ event_hierarchy/0
          ]).

:- use_module(library(pce)).
:- require([ chain_list/2
           , forall/2
           , member/2
           ]).

event_hierarchy :-
    new(P, picture('PCE Event Hierarchy')),
    new(D, dialog),
    send(D, below, P),
    send(D, append, button(quit, message(P, destroy))),
    get(@event_tree, root, Root),
    new(T, tree(new(RootNode, node(text(Root?value, left, normal))))),
    fill_event_hierarchy(Root, RootNode),
    send(P, display, T),
    send(P, open).


fill_event_hierarchy(Node, TreeNode) :-
    get(Node, sons, Sons),
    Sons \== @nil,
    !,
    chain_list(Sons, List),
    forall(member(S, List),
           (send(TreeNode, son, new(N, node(text(S?value, left, normal)))),
            fill_event_hierarchy(S, N))).
fill_event_hierarchy(_, _).
