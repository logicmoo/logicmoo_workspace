/*  Part of Extended libraries for Prolog

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

:- module(ungroup_keys_values,
          [ungroup_keys_values/2,
           ungroup_keys_values/3
          ]).

% :- use_module(library(apply)).

% ungroup_keys_values(Groups) -->
%     foldl(ungroup_key_values, Groups).

% ungroup_key_values(K-VL) -->
%     foldl(ungroup_key_value(K), VL).

% ungroup_key_value(K, V) --> [K-V].

ungroup_keys_values(Groups, Pairs) :-
    ungroup_keys_values(Groups, Pairs, []).

ungroup_keys_values([]) --> [].
ungroup_keys_values([M-G|T]) -->
    ungroup_keys_values(G, M),
    ungroup_keys_values(T).

ungroup_keys_values([N|TN], M) -->
    [M-N],
    same_key(TN, M).
ungroup_keys_values([], _) --> [].

same_key([N|TN], M) -->
    [M-N],
    same_key(TN, M).
same_key([], _) --> [].
