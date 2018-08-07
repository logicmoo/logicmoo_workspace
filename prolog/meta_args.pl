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

:- use_module(library(mapargs)).
:- use_module(library(prolog_metainference), []).

:- meta_predicate mark_meta_arguments(0 ).

annotate(Var, Annotation) :-
    get_attr(Var, meta_args, Annot0),
    !,
    prolog_metainference:join_annotation(Annot0, Annotation, Joined),
    put_attr(Var, meta_args, Joined).
annotate(Var, Annotation) :-
    put_attr(Var, meta_args, Annotation).

attr_unify_hook(A1, Other) :-
    ( get_attr(Other, meta_args, A2)
    ->prolog_metainference:join_annotation(A1, A2, A),
      put_attr(Other, meta_args, A)
    ; var(Other)
    ->put_attr(Other, meta_args, A1)
    ; true
    ).

mark_meta_arg(Arg, Spec) :-
    ( var(Arg)
    ->annotate(Arg, Spec)
    ; Arg = M:A
    ->( var(A)
      ->annotate(A, Spec)
      ; true
      ),
      ( var(M)
      ->annotate(M, m)
      ; true
      )
    ; true
    ).

mark_meta_arguments(Head) :-
    ( ( inferred_meta_predicate(Head, Meta)
      ; predicate_property(Head, meta_predicate(Meta))
      )
    ->strip_module(Head, _, H),
      mapargs(mark_meta_arg, H, Meta)
    ; true
    ).
