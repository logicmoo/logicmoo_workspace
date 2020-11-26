/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
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

:- module(foldnl,
          [foldnl/5,
           foldnl/6,
           foldnl/7,
           foldnl/8]).

:- use_module(library(apply)).

:- meta_predicate
    foldnl(4, +, +, ?, ?),
    foldnl(5, +, +, +, ?, ?),
    foldnl(6, +, +, +, ?, ?, ?),
    foldnl(7, +, +, +, ?, ?, ?, ?).

foldnl(Goal, I1, List, V1, V) :-
    foldl(goaln(Goal), List, I1-V1, _-V).

foldnl(Goal, I1, List1, List2, V1, V) :-
    foldl(goaln(Goal), List1, List2, I1-V1, _-V).

foldnl(Goal, I1, List1, List2, List3, V1, V) :-
    foldl(goaln(Goal), List1, List2, List3, I1-V1, _-V).

foldnl(Goal, I1, List1, List2, List3, List4, V1, V) :-
    foldl(goaln(Goal), List1, List2, List3, List4, I1-V1, _-V).

goaln(Goal, H, I1-V1, I-V) :-
    call(Goal, I1, H, V1, V),
    succ(I1, I).

goaln(Goal, H1, H2, I1-V1, I-V) :-
    call(Goal, I1, H1, H2, V1, V),
    succ(I1, I).

goaln(Goal, H1, H2, H3, I1-V1, I-V) :-
    call(Goal, I1, H1, H2, H3, V1, V),
    succ(I1, I).

goaln(Goal, H1, H2, H3, H4, I1-V1, I-V) :-
    call(Goal, I1, H1, H2, H3, H4, V1, V),
    succ(I1, I).
