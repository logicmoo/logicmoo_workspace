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

:- use_module(library(assrt_lib)).
:- use_module(library(applicable_assertions)).
:- use_module(library(mapargs)).

:- meta_predicate
        mark_meta_arguments(0).

meta_args:attr_unify_hook(AttValue1, VarValue2) :-
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

mark_meta_arg(_, Arg, Spec) :-
    put_attr(Var, meta_args, Spec),
    Arg = Var.

mark_meta_arguments(Head) :-
    ( ( predicate_property(Head, meta_predicate(Meta))
      ; inferred_meta_predicate(Head, Meta)
      )
    ->strip_module(Head, _, H),
      mapargs(mark_meta_arg, H, Meta)
    ; true
    ),
    ( applicable_assertions(Head, _, AsrSuccL),
      AsrSuccL \= []
    ->findall(Head-Spec-Arg,
              ( member(AsrSucc, AsrSuccL),
                asr_aprop(AsrSucc, head, Head, _),
                meta_prop(Succ, Spec, Arg),
                asr_aprop(AsrSucc, succ, Succ, _)
              ), HeadSpecArgL),
      maplist(mark_head_spec(Head), HeadSpecArgL)
    ; true
    ).

meta_prop(typeprops:is_pred(N, Var), N,   Var).
meta_prop(typeprops:mod_qual(  Var), (:), Var).

mark_head_spec(Head1, Head-Spec-Arg) :-
    put_attr(Arg, meta_args, Spec),
    Head1 = Head.

