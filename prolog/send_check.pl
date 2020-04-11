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

:- module(send_check, [send_check/4,
                       send_comp_rtcheck/3]).

:- use_module(library(assertions)).
:- use_module(library(intercept)).

get_comp_rtcheck_info(Goal, Name, ALoc) :-
    ( nb_current('$with_asr', Asr)
    ->asr_aprop(Asr, head, Name, ALoc)
    ; Name = Goal
    ).

:- meta_predicate send_comp_rtcheck(0, +, +).

send_comp_rtcheck(M:Goal, Prop, Fail) :-
    get_comp_rtcheck_info(M:Goal, Name, ALoc),
    ( nb_current('$with_gloc', GLoc)
    ->true
    ; GLoc = []
    ),
    send_check([GLoc/Prop-[Fail]], comp, Name, ALoc).

send_check([], _, _, _) :- !.
send_check(Props, ErrType, Name, ALoc) :-
    ( nb_current('$with_ploc', PLoc)
    ->true
    ; PLoc = []
    ),
    send_signal(assrchk(asr, error(ErrType, Name, Props, PLoc, ALoc))).
