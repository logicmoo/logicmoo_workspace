/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
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

:- module(implemented_in, [implemented_in/1,
                           implemented_in/3]).

:- use_module(library(lists)).
:- use_module(library(extra_location)).
:- use_module(library(extra_messages)).
:- use_module(library(prolog_codewalk), []). % for message_location//1
:- use_module(library(normalize_head)).
:- use_module(library(dynamic_locations)).

:- multifile
    prolog:message//1.

prolog:message(implemented_in(Args)) -->
    ['Implements ~w'-Args].

implemented_in(MGoal0, From, Args) :-
    normalize_head(MGoal0, MGoal),
    M:Goal = MGoal,
    functor(Goal, F, A),
    findall(MI, ( current_module(M),
                  \+ predicate_property(MGoal, imported_from(_)),
                  MI = M
                ; predicate_property(MGoal, imported_from(MI))
                ), UML),
    sort(UML, ML),
    member(M, ML),
    ( ( loc_declaration(Goal, M, Declaration, From),
        Declaration \= goal
      ; loc_dynamic(Goal, M, Declaration, From),
        Declaration \= dynamic(query, _, _)
      ),
      Args = [M:F/A-Declaration]
    ; From = clause(ClauseRef),
      catch(( clause(MGoal, _, ClauseRef),
              nth_clause(MGoal, N, ClauseRef)
            ), _, fail),
      Args = [M:F/A-N]
    ).

:- dynamic prepared/0.

prepare :-
    dynamic_locations([source(false),
                       infer_meta_predicates(false),
                       autoload(false),
                       evaluate(false),
                       trace_reference(_),
                       module_class([user, system, library])]),
    retractall(prepared),
    assertz(prepared).

implemented_in(MGoal) :-
    ( prepared
    ->true
    ; prepare
    ),
    forall(implemented_in(MGoal, From, Args),
           print_message(information, at_location(From,
                                                  implemented_in(Args)))).
