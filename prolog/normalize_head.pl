/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2014, Process Design Center, Breda, The Netherlands.
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

:- module(normalize_head, [normalize_head/2]).

:- use_module(library(implementation_module)).

current_predicate_ext(M:F/A) :-
    ( nonvar(M) ->
      findall(M:F/A, current_predicate(M:F/A), PIL1)
    ; findall(M:F/A, (current_predicate(CM:F/A),
                      functor(H, F, A),
                      implementation_module(CM:H, M)), PIL1)
    ),
    sort(PIL1, PIL),
    member(M:F/A, PIL),
    current_predicate(M:F/A).

:- meta_predicate normalize_head(?, ?).
normalize_head(P,     P)   :- var(P), !.
normalize_head(M:P,   M:P) :- var(P), !.
normalize_head(M:F/A, M:H) :- !, normalize_head_from_pi(M, F, A, H).
normalize_head(P,     MH) :-
    ( P = F/A *->
      MH = M:H,
      normalize_head_from_pi(M, F, A, H)
    ; MH = P
    ).

normalize_head_from_pi(M, F, A, H) :-
    ( atom(F), integer(A) -> true
    ; current_predicate_ext(M:F/A)
    ),
    functor(H, F, A).
