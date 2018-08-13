/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2016, Process Design Center, Breda, The Netherlands.
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

:- module(mapnargs, [mapnargs/2,
                     mapnargs/3,
                     mapnargs/4,
                     mapnargs/5,
                     mapnargs/6,
                     mapnargs/7]).

mapnargs_(N, Goal, T) :-
    arg(N, T, A),
    !,
    call(Goal, N, A),
    succ(N, N1),
    mapnargs_(N1, Goal, T).
mapnargs_(_, _, _).

mapnargs_(N, Goal, T1, T2) :-
    arg(N, T1, A1),
    arg(N, T2, A2),
    !,
    call(Goal, N, A1, A2),
    succ(N, N1),
    mapnargs_(N1, Goal, T1, T2).
mapnargs_(_, _, _, _).

mapnargs_(N, Goal, T1, T2, T3) :-
    arg(N, T1, A1),
    arg(N, T2, A2),
    arg(N, T3, A3),
    !,
    call(Goal, N, A1, A2, A3),
    succ(N, N1),
    mapnargs_(N1, Goal, T1, T2, T3).
mapnargs_(_, _, _, _, _).

mapnargs_(N, Goal, T1, T2, T3, T4) :-
    arg(N, T1, A1),
    arg(N, T2, A2),
    arg(N, T3, A3),
    arg(N, T4, A4),
    !,
    call(Goal, N, A1, A2, A3, A4),
    succ(N, N1),
    mapnargs_(N1, Goal, T1, T2, T3, T4).
mapnargs_(_, _, _, _, _, _).

mapnargs_(N, Goal, T1, T2, T3, T4, T5) :-
    arg(N, T1, A1),
    arg(N, T2, A2),
    arg(N, T3, A3),
    arg(N, T4, A4),
    arg(N, T5, A5),
    !,
    call(Goal, N, A1, A2, A3, A4, A5),
    succ(N, N1),
    mapnargs_(N1, Goal, T1, T2, T3, T4, T5).
mapnargs_(_, _, _, _, _, _, _).

mapnargs_(N, Goal, T1, T2, T3, T4, T5, T6) :-
    arg(N, T1, A1),
    arg(N, T2, A2),
    arg(N, T3, A3),
    arg(N, T4, A4),
    arg(N, T5, A5),
    arg(N, T6, A6),
    !,
    call(Goal, N, A1, A2, A3, A4, A5, A6),
    succ(N, N1),
    mapnargs_(N1, Goal, T1, T2, T3, T4, T5, T6).
mapnargs_(_, _, _, _, _, _, _, _).

:- meta_predicate
    mapnargs(2,?),
    mapnargs(3,?,?),
    mapnargs(4,?,?,?),
    mapnargs(5,?,?,?,?),
    mapnargs(6,?,?,?,?,?),
    mapnargs(7,?,?,?,?,?,?).

mapnargs(Goal, Term) :-
    compound(Term),
    !,
    mapnargs_(1, Goal, Term).
mapnargs(_, _).

mapnargs(Goal, T1, T2) :-
    compound(T1),
    !,
    functor(T1, N, A),
    functor(T2, N, A),
    mapnargs_(1, Goal, T1, T2).
mapnargs(_, T, T).

mapnargs(Goal, T1, T2, T3) :-
    compound(T1),
    !,
    functor(T1, N, A),
    functor(T2, N, A),
    functor(T3, N, A),
    mapnargs_(1, Goal, T1, T2, T3).
mapnargs(_, T, T, T).

mapnargs(Goal, T1, T2, T3, T4) :-
    compound(T1),
    !,
    functor(T1, N, A),
    functor(T2, N, A),
    functor(T3, N, A),
    functor(T4, N, A),
    mapnargs_(1, Goal, T1, T2, T3, T4).
mapnargs(_, T, T, T, T).

mapnargs(Goal, T1, T2, T3, T4, T5) :-
    compound(T1),
    !,
    functor(T1, N, A),
    functor(T2, N, A),
    functor(T3, N, A),
    functor(T4, N, A),
    functor(T5, N, A),
    mapnargs_(1, Goal, T1, T2, T3, T4, T5).
mapnargs(_, T, T, T, T, T).

mapnargs(Goal, T1, T2, T3, T4, T5, T6) :-
    compound(T1),
    !,
    functor(T1, N, A),
    functor(T2, N, A),
    functor(T3, N, A),
    functor(T4, N, A),
    functor(T5, N, A),
    functor(T6, N, A),
    mapnargs_(1, Goal, T1, T2, T3, T4, T5, T6).
mapnargs(_, T, T, T, T, T, T).
