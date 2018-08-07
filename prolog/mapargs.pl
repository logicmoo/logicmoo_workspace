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

:- module(mapargs, [mapargs/2,
                    mapargs/3,
                    mapargs/4,
                    mapargs/5,
                    mapargs/6,
                    mapargs/7
                   ]).

:- use_module(library(clambda)).
:- use_module(library(mapnargs)).

:- meta_predicate
    mapargs(1,+),
    mapargs(2,+,?),
    mapargs(3,+,?,?),
    mapargs(4,+,?,?,?),
    mapargs(5,+,?,?,?,?),
    mapargs(6,+,?,?,?,?,?).

mapargs(G, T)                      :- mapnargs([G] +\ _^call(G), T).
mapargs(G, T1, T2)                 :- mapnargs([G] +\ _^call(G), T1, T2).
mapargs(G, T1, T2, T3)             :- mapnargs([G] +\ _^call(G), T1, T2, T3).
mapargs(G, T1, T2, T3, T4)         :- mapnargs([G] +\ _^call(G), T1, T2, T3, T4).
mapargs(G, T1, T2, T3, T4, T5)     :- mapnargs([G] +\ _^call(G), T1, T2, T3, T4, T5).
mapargs(G, T1, T2, T3, T4, T5, T6) :- mapnargs([G] +\ _^call(G), T1, T2, T3, T4, T5, T6).
