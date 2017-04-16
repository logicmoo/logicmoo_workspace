/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
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

:- module(called_by_body, [called_by_body/4]).

:- use_module(library(extend_args)).
:- use_module(library(implementation_module)).

called_by_body(Body, CM, Body, CM) :- var(Body), !, fail.
called_by_body(CM:Body, _, H, M) :- called_by_body(Body, CM, H, M).
called_by_body((A,B),  CM, H, M) :- !,
    ( called_by_body(A, CM, H, M)
    ; called_by_body(B, CM, H, M)
    ).
called_by_body((A;B),  CM, H, M) :- !,
    ( called_by_body(A, CM, H, M)
    ; called_by_body(B, CM, H, M)
    ).
called_by_body(Goal, CM, H, M) :-
    predicate_property(CM:Goal, meta_predicate(Spec)),
    called_by_args(Goal, Spec, CM, H, M).
called_by_body(Goal, CM, Goal, M) :-
    implementation_module(CM:Goal, M).

called_by_args(Goal, Spec, CM, H, M) :-
    arg(N, Goal, Arg),
    arg(N, Spec, SA),
    called_by_arg(SA, Arg, CM, H, M).

called_by_arg(0, Goal, CM, H, M) :- !, called_by_body(Goal, CM, H, M).
called_by_arg(^, Goal, CM, H, M) :- !, called_by_body(Goal, CM, H, M).
called_by_arg(N, Goal, CM, H, M) :-
    integer(N),
    length(Extra, N),
    extend_args(Goal, Extra, Goal1),
    called_by_body(Goal1, CM, H, M).
