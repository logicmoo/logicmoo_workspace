/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
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

:- dynamic
    decreasing_cp_db/2.
        
:- volatile
    decreasing_cp_db/2.

call_decreasing_cp(Call, Arg) :-
    variant_sha1(Arg, H1),
    prolog_current_choice(CP1),
    setup_call_cleanup(
        assertz(decreasing_cp_db(H1, CP1)),
        do_call_decreasing_cp(Call, Arg),
        retractall(decreasing_cp_db(_, _))).

do_call_decreasing_cp(Call, Arg) :-
    ( call(Call),
      prolog_current_choice(CP2),
      variant_sha1(Arg, H2),
      ( clause(decreasing_cp_db(H2, CP), _, Ref),
        CP2 > CP
      ->erase(Ref),
        safe_prolog_cut_to(CP2, CP, CPN)
      ; CPN = CP2
      ),
      assertz(decreasing_cp_db(H2, CPN))
    ).
