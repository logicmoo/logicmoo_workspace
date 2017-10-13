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

:- module(foldargs, [foldargs//2,
                     foldargs//3,
                     foldargs//4,
                     foldargs//5
                    ]).

:- meta_predicate
   foldargs(4, +, +, -),
   foldargs(5, +, +, +, -),
   foldargs(6, +, +, +, +, -),
   foldargs(7, +, +, +, +, +, -).

foldargs(Goal, Term)                   --> foldargs_(1, Goal, Term).
foldargs(Goal, T1, T2)                 --> foldargs_(1, Goal, T1, T2).
foldargs(Goal, T1, T2, T3)             --> foldargs_(1, Goal, T1, T2, T3).
foldargs(Goal, T1, T2, T3, T4)         --> foldargs_(1, Goal, T1, T2, T3, T4).

foldargs_(N, Goal, T1) -->
    {arg(N, T1, A1)},
    !,
    call(Goal, N, A1),
    {succ(N, N1)},
    foldargs_(N1, Goal, T1).
foldargs_(_, _, _) --> [].

foldargs_(N, Goal, T1, T2) -->
    { arg(N, T1, A1),
      arg(N, T2, A2)
    },
    !,
    call(Goal, N, A1, A2),
    {succ(N, N1)},
    foldargs_(N1, Goal, T1, T2).
foldargs_(_, _, _, _) --> [].

foldargs_(N, Goal, T1, T2, T3) -->
    { arg(N, T1, A1),
      arg(N, T2, A2),
      arg(N, T3, A3)
    },
    !,
    call(Goal, N, A1, A2, A3),
    {succ(N, N1)},
    foldargs_(N1, Goal, T1, T2, T3).
foldargs_(_, _, _, _, _) --> [].

foldargs_(N, Goal, T1, T2, T3, T4) -->
    { arg(N, T1, A1),
      arg(N, T2, A2),
      arg(N, T3, A3),
      arg(N, T3, A4)
    },
    !,
    call(Goal, N, A1, A2, A3, A4),
    {succ(N, N1)},
    foldargs_(N1, Goal, T1, T2, T3, T4).
foldargs_(_, _, _, _, _, _) --> [].
