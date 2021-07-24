/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera
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

:- module(mapnlist,
          [mapnlist/2,
           mapnlist/3,
           mapnlist/4,
           mapnlist/5,
           mapnlist/6]).

:- meta_predicate
    mapnlist(1, ?),
    mapnlist(2, ?, ?),
    mapnlist(3, ?, ?, ?),
    mapnlist(4, ?, ?, ?, ?),
    mapnlist(5, ?, ?, ?, ?, ?).

mapnlist(Goal, List) :-
    mapnlist_(List, Goal).

mapnlist_([], _) :- !.
mapnlist_([H|T], Goal) :-
    !,
    mapnlist_(H, Goal),
    mapnlist_(T, Goal).
mapnlist_(E1, Goal) :-
    call(Goal, E1).

mapnlist(Goal, List1, List2) :-
    mapnlist_(List1, List2, Goal).

mapnlist_([], [], _) :- !.
mapnlist_([H1|T1], [H2|T2], Goal) :-
    !,
    mapnlist_(H1, H2, Goal),
    mapnlist_(T1, T2, Goal).
mapnlist_(E1, E2, Goal) :-
    call(Goal, E1, E2).

mapnlist(Goal, List1, List2, List3) :-
    mapnlist_(List1, List2, List3, Goal).

mapnlist_([], [], [], _) :- !.
mapnlist_([H1|T1], [H2|T2], [H3|T3], Goal) :-
    !,
    mapnlist_(H1, H2, H3, Goal),
    mapnlist_(T1, T2, T3, Goal).
mapnlist_(E1, E2, E3, Goal) :-
    call(Goal, E1, E2, E3).

mapnlist(Goal, List1, List2, List3, List4) :-
    mapnlist_(List1, List2, List3, List4, Goal).

mapnlist_([], [], [], [], _) :- !.
mapnlist_([H1|T1], [H2|T2], [H3|T3], [H4|T4], Goal) :-
    !,
    mapnlist_(H1, H2, H3, H4, Goal),
    mapnlist_(T1, T2, T3, T4, Goal).
mapnlist_(E1, E2, E3, E4, Goal) :-
    call(Goal, E1, E2, E3, E4).

mapnlist(Goal, List1, List2, List3, List4, List5) :-
    mapnlist_(List1, List2, List3, List4, List5, Goal).

mapnlist_([], [], [], [], [], _) :- !.
mapnlist_([H1|T1], [H2|T2], [H3|T3], [H4|T4], [H5|T5], Goal) :-
    !,
    mapnlist_(H1, H2, H3, H4, H5, Goal),
    mapnlist_(T1, T2, T3, T4, T5, Goal).
mapnlist_(E1, E2, E3, E4, E5, Goal) :-
    call(Goal, E1, E2, E3, E4, E5).
