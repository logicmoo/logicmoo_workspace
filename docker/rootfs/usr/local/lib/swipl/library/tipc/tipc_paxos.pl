/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, CWI, Amsterdam
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

:- module(tipc_paxos,
          [ tipc_paxos_get/1,         % ?Term
            tipc_paxos_get/2,         % ?Term, +Options
            tipc_paxos_set/1,         % ?Term
            tipc_paxos_set/2,         % ?Term, +Options
            tipc_paxos_on_change/2,   % ?Term, +Goal
            tipc_initialize/0
          ]).
:- use_module(library(tipc/tipc_broadcast), [tipc_initialize/0]).
:- autoload(library(paxos),[paxos_set/2,paxos_get/2,paxos_on_change/2]).

/** <module> Paxos on TIPC

This module provides compatibility for  using   paxos  over  TIPC. As of
SWI-Prolog 7.7.15 the core of this  module   has  been moved to the core
library as library(paxos)  and  can  be   used  with  other  distributed
implementations of library(broadcast) such as library(udb_broadcast).
*/

:- meta_predicate
    tipc_paxos_on_change(?, 0).

%!  tipc_paxos_set(?Term) is semidet.
%!  tipc_paxos_get(?Term) is semidet.
%!  tipc_paxos_get(?Term, +Options) is semidet.
%!  tipc_paxos_set(?Term, +Options) is semidet.
%!  tipc_paxos_on_change(?Term, :Goal) is det.

tipc_paxos_set(Term) :-             paxos_set(Term, []).
tipc_paxos_set(Term, Options) :-    paxos_set(Term, Options).
tipc_paxos_get(Term) :-             paxos_get(Term, []).
tipc_paxos_get(Term, Options) :-    paxos_get(Term, Options).
tipc_paxos_on_change(Term, Goal) :- paxos_on_change(Term, Goal).

:- multifile
    paxos:paxos_message_hook/3.

paxos:paxos_message_hook(Paxos, -,   tipc_cluster(Paxos)) :- !.
paxos:paxos_message_hook(Paxos, TMO, tipc_cluster(Paxos, TMO)).
