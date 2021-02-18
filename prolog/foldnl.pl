/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2021, Process Design Center, Breda, The Netherlands.
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

%!  foldnl(:Goal, +NList, +V0, -V).
%!  foldnl(:Goal, +NList1, +NList2, +V0, -V).
%!  foldnl(:Goal, +NList1, +NList2, +NList3, +V0, -V).
%!  foldnl(:Goal, +NList1, +NList2, +NList3, +NList4, +V0, -V).

foldnl(Goal, List) -->
    foldnl_(List, Goal).

foldnl_([], _) -->
    !.
foldnl_([H|T], Goal) -->
    !,
    foldnl_(H, Goal),
    foldnl_(T, Goal).
foldnl_(E, Goal) -->
    call(Goal, E).

foldnl(Goal, List1, List2) -->
    foldnl_(List1, List2, Goal).

foldnl_([], [], _) -->
    !.
foldnl_([H1|T1], [H2|T2], Goal) -->
    !,
    foldnl_(H1, H2, Goal),
    foldnl_(T1, T2, Goal).
foldnl_(E1, E2, Goal) -->
    call(Goal, E1, E2).

foldnl(Goal, List1, List2, List3) -->
    foldnl_(List1, List2, List3, Goal).

foldnl_([], [], [], _) -->
    !.
foldnl_([H1|T1], [H2|T2], [H3|T3], Goal) -->
    !,
    foldnl_(H1, H2, H3, Goal),
    foldnl_(T1, T2, T3, Goal).
foldnl_(E1, E2, E3, Goal) -->
    call(Goal, E1, E2, E3).

foldnl(Goal, List1, List2, List3, List4) -->
    foldnl_(List1, List2, List3, List4, Goal).

foldnl_([], [], [], [], _) -->
    !.
foldnl_([H1|T1], [H2|T2], [H3|T3], [H4|T4], Goal) -->
    !,
    foldnl_(H1, H2, H3, H4, Goal),
    foldnl_(T1, T2, T3, T4, Goal).
foldnl_(E1, E2, E3, E4, Goal) -->
    call(Goal, E1, E2, E3, E4).
