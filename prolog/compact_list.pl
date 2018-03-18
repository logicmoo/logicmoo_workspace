/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
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

:- module(compact_list, [compact_list/2]).

:- use_module(library(lists)).

compact_list_n(L, N, R) :-
        length(L1, N),
        append(L1, R1, L),
        compact_list_n_(L, L1, R1, N, R).

compact_list_n_(L, L2, R2, N, R) :-
        length(L3, N),
        append(L3, R3, R2),
        (
            L2 == L3 ->
            (compact_list_n_(R2, L3, R3, N, R) -> true ; R2 = R)
        ;
            L = [E|L1],
            (compact_list_n(L1, N, R1) -> R = [E|R1] ; R = L)
        ).

% :- test compact_list(A, B) :
%       (A = [1, 2, 2, 2, 2, 3, 3, 4, 3, 4, 3, 4, 3, 4, 1, 5, 7, 1, 5, 7])
%       => (B = [1, 2, 3, 4, 1, 5, 7]) + not_fails.

% :- pred compact_list(L, R) : list(L) => list(R)
%       # "Predicate that delete repeated sequences in a list.".

compact_list(L, R) :-
        compact_list_(L, 1, R).

compact_list_(L, N, R) :-
        compact_list_n(L, N, R1) ->
        N1 is N + 1,
        compact_list_(R1, N1, R)
    ;
        L = R.
