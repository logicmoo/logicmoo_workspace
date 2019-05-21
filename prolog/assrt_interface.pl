/*  Part of Assertion Reader for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
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

:- module(assrt_interface, []).

:- use_module(library(assertions), []).
:- use_module(library(interface), []).
:- use_module(library(context_values)).

% Propagate assertions in an interface to the implementation

assertions:asr_head_prop(in_asr(IM, Head, Asr), IM, Head, Status, Type, Dict, Loc) :-
    head_prop_asr_intf(Head, IM, Status, Type, Dict, Loc, Asr).

head_prop_asr_intf(Head, IM, Status, Type, Dict, Loc, Asr) :-
    \+ current_context_value(evaluating, true),
    interface:'$implementation'(IM, Interface),
    with_context_value(assertions:asr_head_prop(Asr, Interface, Head,
                                               Status, Type, Dict, Loc),
                       evaluating, true).

assertions:asr_comm(in_asr(IM, Head, Asr), Comm, Loc) :-
    head_prop_asr_intf(Head, IM, _, _, _, _, Asr),
    assertions:asr_comm(Asr, Comm, Loc).
assertions:asr_comp(in_asr(IM, Head, Asr), M, Comp, Loc) :-
    head_prop_asr_intf(Head, IM, _, _, _, _, Asr),
    assertions:asr_comp(Asr, M, Comp, Loc).
assertions:asr_call(in_asr(IM, Head, Asr), M, Call, Loc) :-
    head_prop_asr_intf(Head, IM, _, _, _, _, Asr),
    assertions:asr_call(Asr, M, Call, Loc).
assertions:asr_succ(in_asr(IM, Head, Asr), M, Succ, Loc) :-
    head_prop_asr_intf(Head, IM, _, _, _, _, Asr),
    assertions:asr_succ(Asr, M, Succ, Loc).
assertions:asr_glob(in_asr(IM, Head, Asr), M, Glob, Loc) :-
    head_prop_asr_intf(Head, IM, _, _, _, _, Asr),
    assertions:asr_glob(Asr, M, Glob, Loc).
