/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
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

:- module(meta_args,
          [mark_meta_arguments/1]).

:- use_module(library(intercept)).
:- use_module(library(applicable_assertions)).
:- use_module(library(assrt_lib)).
:- use_module(library(mapargs)).
:- use_module(library(ctrtchecks)).
:- use_module(library(rtchecks_rt)).
:- use_module(library(rtchecks_utils), []).

:- meta_predicate
        mark_meta_arguments(0).

attr_unify_hook(AttValue1, VarValue2) :-
    ( var(VarValue2)
    ->( get_attr(VarValue2, meta_args, AttValue2)
      ->once(narrow_meta_spec(AttValue1, AttValue2, AttValue))
      ; AttValue = AttValue1
      ),
      put_attr(VarValue2, meta_args, AttValue)
    ; true
    ).

narrow_meta_spec(X, X, X).
narrow_meta_spec(M, N, :) :-
    integer(M),
    integer(N),
    M =\= N.
narrow_meta_spec(:, _, :).
narrow_meta_spec(_, :, :).
narrow_meta_spec(N, _, N) :- integer(N).
narrow_meta_spec(_, N, N) :- integer(N).
narrow_meta_spec(_, _, ?).

mark_meta_arg(_, Arg, Spec) :- mark_meta_arg(Arg, Spec).

mark_meta_arg(Arg, Spec) :-
    put_attr(Var, meta_args, Spec),
    Arg = Var.

assrt_lib:asr_aprop(ctspec(Asr), CKey, Prop, From) :-
    ckey_akey(CKey, AKey),
    curr_prop_asr(AKey, Prop, From, Asr).

ckey_akey(head, head).
ckey_akey(stat, stat).
ckey_akey(type, type).
ckey_akey(dict, dict).
ckey_akey(comm, comm).
ckey_akey(comp, AKey) :- cc(AKey).
ckey_akey(succ, succ).

cc(call).
cc(comp).

mark_meta_arguments(Head) :-
    ( ( predicate_property(Head, meta_predicate(Meta))
      ; inferred_meta_predicate(Head, Meta)
      )
    ->strip_module(Head, _, H),
      mapargs(mark_meta_arg, H, Meta)
    ; true
    ),
    findall(ctspec(Asr), prop_asr(head, Head, _, Asr), AsrL),
    intercept(check_asrs_pre(ct, applicable_prop_check,
                             AsrL, _, AsrSuccPVL), assrchk(_, _), true),
    findall(Head-Spec-Arg,
            ( member(Asr-PVL, AsrSuccPVL),
              memberchk(PVL, [[], [[]]]),
              aprop_asr(head, Head, _, Asr),
              meta_prop(Succ, Spec, Arg),
              aprop_asr(succ, Succ, _, Asr)
            ), HeadSpecArgL),
    maplist(mark_head_spec(Head), HeadSpecArgL).

meta_prop(typeprops:goal(Var),     0, Var).
meta_prop(typeprops:goal(N, Var),  N, Var).
meta_prop(typeprops:mod_qual(Var), :, Var).

mark_head_spec(Head, Head-Spec-Arg) :-
    mark_meta_arg(Arg, Spec).
