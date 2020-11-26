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

:- module(group_pairs_or_sort, [group_pairs_or_sort/2]).

group_pairs_by_key_u([], []).
group_pairs_by_key_u([M-N|T0], [M-[N|TN]|T]) :-
    same_key_u(M, T0, TN, T1),
    group_pairs_by_key_u(T1, T).

same_key_u(M, [M-N|T0], [N|TN], T) :-
    !,
    same_key_u(M, T0, TN, T).
same_key_u(_, L, [], L).

group_pairs_or_sort(Pairs, Grouped) :-
    Pairs = [_-_|_],
    !,
    keysort(Pairs, Sorted),
    group_pairs_by_key_u(Sorted, UnGrouped),
    maplist(group_pairs_or_sort_into, UnGrouped, Grouped).
group_pairs_or_sort(Unsorted, Sorted) :-
    sort(Unsorted, Sorted).

group_pairs_or_sort_into(K-U, K-S) :- group_pairs_or_sort(U, S).
