/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
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

:- module(qualify_meta_goal, [meta_call_goal/4,
                              qualify_meta_goal/2,
                              qualify_meta_goal/3,
                              qualify_meta_goal/4,
                              qualify_meta_call/5]).

:- use_module(library(mapargs)).
:- use_module(library(check), []). % for add_module/3

qualify_meta_goal(Goal1, M, CM, Goal) :-
    qualify_meta_call(Goal1, M, CM, true, Goal).

:- meta_predicate meta_call_goal(+, +, 0, -).
meta_call_goal(Goal, M, MCaller, Meta) :-
    predicate_property(M:Goal,  meta_predicate(GMeta)),
    ( predicate_property(MCaller, meta_predicate(CMeta))
    ->true
    ; CMeta = true
    ),
    functor(Goal, F, A),
    functor(Meta, F, A),
    strip_module(MCaller, _, Caller),
    mapargs(meta_call_goal_arg(Caller, CMeta), Goal, GMeta, Meta).

meta_call_goal_arg(Caller, CMeta, _, Arg, Spec1, Spec) :-
    ( module_qualified(Spec1),
      ( nonvar(Arg),
        Arg = _:_
      ; compound(CMeta),
        arg(N, CMeta, CSpec),
        module_qualified(CSpec),
        arg(N, Caller, CArg),
        CArg == Arg
      )
    ->Spec = +
    ; Spec = Spec1
    ).

:- meta_predicate qualify_meta_call(+, +, ?, 0, -).
qualify_meta_call(Goal1, M, CM, Caller, Goal) :-
    meta_call_goal(Goal1, M, Caller, Meta), !,
    qualify_meta_goal(CM:Goal1, Meta, Goal).
qualify_meta_call(Goal, _, _, _, Goal).

:- meta_predicate qualify_meta_goal(0, -).
qualify_meta_goal(Goal0, Goal) :-
    predicate_property(Goal0, meta_predicate(Meta)), !,
    qualify_meta_goal(Goal0, Meta, Goal).
qualify_meta_goal(_:Goal, Goal).

qualify_meta_goal(M:Goal0, Meta, Goal) :-
    functor(Goal0, F, N),
    functor(Goal, F, N),
    mapargs(meta_goal(M), Meta, Goal0, Goal).

module_qualified(:) :- !.
module_qualified(N) :- integer(N), N >= 0.

meta_goal(M, _, ArgM, Arg0, Arg) :-
    ( module_qualified(ArgM)
    ->check:add_module(Arg0, M, Arg)
    ; Arg = Arg0
    ).
