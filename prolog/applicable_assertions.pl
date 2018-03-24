/*  Part of Run-Time Checker for Assertions

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
    Copyright (C): 2018, Process Design Center, Breda, The Netherlands.
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

:- module(applicable_assertions,
          [applicable_prop_check/3,
           applicable_assertions/3]).

:- use_module(library(assrt_lib)).
:- use_module(library(ctrtchecks)).
:- use_module(library(intercept)).

:- meta_predicate
       applicable_assertions(0, -, -).

assrt_lib:asr_aprop(spec(Asr), Key, Prop, From) :-
    curr_prop_asr(Key, Prop, From, Asr).

applicable_prop_check(T, Part, Asr) :-
    part_time(Part, T),
    asr_aprop(Asr, type, Type, _),
    assrt_op(Part, step2, inner, Type),
    asr_aprop(Asr, stat, Status, _),
    applicable_status(Status).

applicable_status(true).
applicable_status(trust).
applicable_status(check).

%%  applicable_assertions(:Call, -AsrGlobL:list, -AsrSuccL:list) is det.
%
%   For a given Call, AsrGlobL gives the applicable assertions with global
%   properties and AsrSuccL gives the applicable assertions with success
%   properties. This predicate is intended to be used at run-time.

applicable_assertions(Head, AsrGlobL, AsrSuccL) :-
    findall(spec(Asr), prop_asr(head, Head, _, Asr), AsrL),
    intercept(check_asrs_pre(rt, applicable_prop_check, AsrL, AsrGlobPVL, AsrSuccPVL),
              assrchk(_, _), true),
    findall(Asr,
            ( member(Asr-PVL, AsrGlobPVL),
              memberchk(PVL, [[], [[]]])
            ), AsrGlobL),
    findall(Asr,
            ( member(Asr-PVL, AsrSuccPVL),
              memberchk(PVL, [[], [[]]])
            ), AsrSuccL).
