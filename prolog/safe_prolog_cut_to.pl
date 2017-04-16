/*  Part of Extended libraries for Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2016, Process Design Center, Breda, The Netherlands.
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

:- module(safe_prolog_cut_to,
          [call_decreasing_cp/2,
           fix_choice/3,
           safe_prolog_cut_to/1,
           safe_prolog_cut_to/3
          ]).

:- meta_predicate
    call_decreasing_cp(0, ?).

safe_prolog_cut_to(CP) :-
    prolog_current_choice(CPC),
    safe_prolog_cut_to(CPC, CP, _).

safe_prolog_cut_to(CPC, CP, CPF) :-
    fix_choice(CPC, CP, CPF),
    prolog_cut_to(CPF).

fix_choice(CPC, CP, CPC) :-
    CPC =< CP, !.
fix_choice(CPC, CP, CPF) :-
    prolog_choice_attribute(CPC, parent, CPP),
    fix_choice(CPP, CP, CPF).

call_decreasing_cp(Call, Arg) :-
    prolog_current_choice(CP1),
    copy_term(Arg, PArg),
    S = s([CP1-PArg]),
    call(Call),
    prolog_current_choice(CP2),
    S = s(SolL1),
    ( select(CP-Sol, SolL1, SolL),
      Sol =@= Arg,
      CP2 > CP
    ->safe_prolog_cut_to(CP2, CP, CPN)
    ; SolL = SolL1,
      CPN = CP2
    ),
    nb_setarg(1, S, [CPN-Arg|SolL]).
