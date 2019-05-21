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

:- module(assrt_meta, []).

:- use_module(library(assertions)).
:- use_module(library(assertions_op)).
:- use_module(library(location_utils)).
:- use_module(library(predicate_from)).
:- use_module(library(globprops)).

:- create_prolog_flag(assrt_meta_pred, none, [type(atom)]).

:- meta_predicate
    with_amp(0, +, +).

:- table
    am_head_prop_idx/5.

meta_has_mode_info(Meta) :-
    arg(_, Meta, Spec),
    memberchk(Spec, [+,-]),
    !.

with_amp(Goal, OldFlag, NewFlag) :-
    setup_call_cleanup(
        set_prolog_flag(assrt_meta_pred, NewFlag),
        Goal,
        set_prolog_flag(assrt_meta_pred, OldFlag)).

am_head_prop_idx(Head, M, Meta, From) :-
    current_prolog_flag(assrt_meta_pred, Flag),
    Flag \= none,
    copy_term_nat(Head, Term),
    with_amp(am_head_prop_idx(Flag, Term, M, Meta, From), Flag, none),
    Head = Term.

am_head_prop_idx(Flag, Head, M, Meta, From) :-
    var(Meta), !,
    Pred = M:Head,
    ( var(Head)
    ->module_property(M, class(user)),
      current_predicate(M:F/A),
      functor(Head, F, A)
    ; functor(Head, F, A),
      module_property(M, class(user)),
      current_predicate(M:F/A) % Narrow answer set for M
    ),
    \+ predicate_property(Pred, imported_from(_)),
    % if something can not be debugged, can not be rtchecked either:
    \+ predicate_property(Pred, nodebug),
    '$predicate_property'(meta_predicate(Meta), Pred),
    % predicate_property(Pred, meta_predicate(Meta)),
    meta_has_mode_info(Meta),
    ( Flag = all
    ->
      \+ ( prop_asr(Head, M, check, _, _, _, Asr),
           prop_asr(glob, no_meta_modes(_), _, Asr)
         )
    ; Flag = specific
    ->once(( prop_asr(Head, M, check, _, _, _, Asr),
             prop_asr(glob, meta_modes(_), _, Asr)
           ))
    ),
    once(( property_from(M:Pred, meta_predicate, From)
         ; predicate_from(Pred, From)
         )).
am_head_prop_idx(_, _, _, _, _).

assertions:asr_head_prop(am_asr(M, H, S, F), M, H, check, (comp), [], F) :-
    am_head_prop_idx(H, M, S, F).
assertions:asr_glob(am_asr(M, H, S, F), assrt_meta,
                   check_call(rt, [am_asr2(M, H, S, F)], _), F) :-
    am_head_prop_idx(H, M, S, F).

assertions:asr_aprop(am_asr2(M, H, _, From), head,   M:H, From).
assertions:asr_aprop(am_asr2(_, _, _, From), stat, check, From).
assertions:asr_aprop(am_asr2(_, _, _, From), type,  pred, From).
assertions:asr_aprop(am_asr2(M, H, Meta, From), Type,  Prop, From) :-
    (nonvar(Type) -> memberchk(Type, [call, succ]) ; true),
    assertions:current_normalized_assertion(pred Meta, M, _, M:H, _,
                                           _, _, CaL, SuL, _, _, _, _),
    member(Type-PropL, [call-CaL, succ-SuL]),
    member(Prop-_, PropL).
