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

:- module(codewalk, [walk_code/1]).

:- use_module(library(option_utils)).

:- multifile
    walk_code/2.

:- public
    true_3/3,
    true_2/2.

:- use_module(library(codewalk_prolog)).
:- use_module(library(codewalk_source)).
:- use_module(library(codewalk_clause)).
% :- use_module(library(codewalk_hybrid)).

true_3(_, _, _).
true_2(_, _).

/*
true_3(Goal, Caller, From) :-
    print_message(information,
                  at_location(From, format("~w :- ~w", [Caller, Goal]))).
*/

is_meta(on_trace).
is_meta(on_head).

:- meta_predicate
    walk_code(:).

%!  walk_code(:Options) is det.

walk_code(MOptions) :-
    meta_options(is_meta, MOptions, Options1),
    foldl(select_option_default,
          [method(Method)-clause],
          Options1, Options),
    walk_code(Method, Options).
