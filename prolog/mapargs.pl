/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
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

:- module(mapargs, [mapargs/2,
                    mapargs/3,
                    mapargs/4,
                    mapargs/5,
                    mapargs/6,
                    mapargs/7]).

mapargs_(N, Goal, T) :-
    arg(N, T, A),
    !,
    call(Goal, N, A),
    succ(N, N1),
    mapargs_(N1, Goal, T).
mapargs_(_, _, _).

mapargs_(N, Goal, T1, T2) :-
    arg(N, T1, A1),
    arg(N, T2, A2),
    !,
    call(Goal, N, A1, A2),
    succ(N, N1),
    mapargs_(N1, Goal, T1, T2).
mapargs_(_, _, _, _).

mapargs_(N, Goal, T1, T2, T3) :-
    arg(N, T1, A1),
    arg(N, T2, A2),
    arg(N, T3, A3),
    !,
    call(Goal, N, A1, A2, A3),
    succ(N, N1),
    mapargs_(N1, Goal, T1, T2, T3).
mapargs_(_, _, _, _, _).

mapargs_(N, Goal, T1, T2, T3, T4) :-
    arg(N, T1, A1),
    arg(N, T2, A2),
    arg(N, T3, A3),
    arg(N, T4, A4),
    !,
    call(Goal, N, A1, A2, A3, A4),
    succ(N, N1),
    mapargs_(N1, Goal, T1, T2, T3, T4).
mapargs_(_, _, _, _, _, _).

mapargs_(N, Goal, T1, T2, T3, T4, T5) :-
    arg(N, T1, A1),
    arg(N, T2, A2),
    arg(N, T3, A3),
    arg(N, T4, A4),
    arg(N, T5, A5),
    !,
    call(Goal, N, A1, A2, A3, A4, A5),
    succ(N, N1),
    mapargs_(N1, Goal, T1, T2, T3, T4, T5).
mapargs_(_, _, _, _, _, _, _).

mapargs_(N, Goal, T1, T2, T3, T4, T5, T6) :-
    arg(N, T1, A1),
    arg(N, T2, A2),
    arg(N, T3, A3),
    arg(N, T4, A4),
    arg(N, T5, A5),
    arg(N, T6, A6),
    !,
    call(Goal, N, A1, A2, A3, A4, A5, A6),
    succ(N, N1),
    mapargs_(N1, Goal, T1, T2, T3, T4, T5, T6).
mapargs_(_, _, _, _, _, _, _, _).

:- meta_predicate
    mapargs(2,?),
    mapargs(3,?,?),
    mapargs(4,?,?,?),
    mapargs(5,?,?,?,?),
    mapargs(6,?,?,?,?,?),
    mapargs(7,?,?,?,?,?,?).

mapargs(Goal, Term)                   :- mapargs_(1, Goal, Term).
mapargs(Goal, T1, T2)                 :- mapargs_(1, Goal, T1, T2).
mapargs(Goal, T1, T2, T3)             :- mapargs_(1, Goal, T1, T2, T3).
mapargs(Goal, T1, T2, T3, T4)         :- mapargs_(1, Goal, T1, T2, T3, T4).
mapargs(Goal, T1, T2, T3, T4, T5)     :- mapargs_(1, Goal, T1, T2, T3, T4, T5).
mapargs(Goal, T1, T2, T3, T4, T5, T6) :- mapargs_(1, Goal, T1, T2, T3, T4, T5, T6).
