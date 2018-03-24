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

:- module(assrt_metainference, [infer_meta_assertions/0]).

:- use_module(library(assrt_lib)).
:- use_module(library(transpose)).
:- use_module(library(implementation_module)).
:- use_module(library(prolog_metainference), []).

applicable_type(calls).
applicable_type(success).
applicable_type(pred).
applicable_type(prop).

meta_prop(typeprops:goal(Var),     0, Var).
meta_prop(typeprops:goal(N, Var),  N, Var).
meta_prop(typeprops:mod_qual(Var), :, Var).

%!  infer_meta_assertions is det
%
%   Collects meta-predicate specifiers that can be inferred from assertions and
%   make them available through prolog_metainterfece:inferred_meta_predicate/2.
%   This is intended to be executed before the inference of meta predicates via
%   prolog_codewalk.

infer_meta_assertions :-
    findall((M:F/A)-Meta,
            ( asr_head_prop(Asr, CM, Head, Status, Type, _, _From),
              memberchk(Status, [true, trust, check]),
              applicable_type(Type),
              functor(Head, F, A),
              implementation_module(CM:Head, M),
              findall([Head, Arg, Spec],
                      ( member(Key, [comp, call, succ]),
                        meta_prop(Prop, Spec, Arg),
                        prop_asr(Key, Prop, _, Asr)
                      ), HeadArgSpecLL),
              transpose(HeadArgSpecLL, [HeadL, ArgL, SpecL]),
              maplist(=(Head), HeadL),
              prolog_metainference:( maplist(annotate, ArgL, SpecL),
                                     meta_annotation(Head, Meta))
            ), KeyMetaU),
    keysort(KeyMetaU, KeyMetaL),
    group_pairs_by_key(KeyMetaL, KeyMetaG),
    forall(member((M:F/A)-MetaL1, KeyMetaG),
           ( functor(Head, F, A),
             ( predicate_property(M:Head, meta_predicate(Meta1))
             ->MetaL = [Meta1|MetaL1]
             ; MetaL = MetaL1
             ),
             prolog_metainference:combine_meta_args(MetaL, Meta),
             assertz(prolog_metainference:inferred_meta_pred(Head, M, Meta))
           )).
