/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera
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

:- module(mapilist,
          [mapilist/3,
           mapilist/4,
           mapilist/5,
           mapilist/6,
           mapilist/7,
           mapilist/8]).

:- meta_predicate
    mapilist(2, +, ?),
    mapilist(3, +, ?, ?),
    mapilist(4, +, ?, ?, ?),
    mapilist(5, +, ?, ?, ?, ?),
    mapilist(6, +, ?, ?, ?, ?, ?),
    mapilist(7, +, ?, ?, ?, ?, ?, ?).

mapilist(Goal, I1, List) :-
    mapilist_(List, I1, Goal).

mapilist_([], _, _).
mapilist_([Elem|Tail], I1, Goal) :-
    call(Goal, I1, Elem),
    succ(I1, I),
    mapilist_(Tail, I, Goal).

mapilist(Goal, I1, List1, List2) :-
    mapilist_(List1, List2, I1, Goal).

mapilist_([], [], _, _).
mapilist_([Elem1|Tail1], [Elem2|Tail2], I1, Goal) :-
    call(Goal, I1, Elem1, Elem2),
    succ(I1, I),
    mapilist_(Tail1, Tail2, I, Goal).

mapilist(Goal, I, List1, List2, List3) :-
        mapilist_(List1, List2, List3, I, Goal).

mapilist_([], [], [], _, _).
mapilist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], I1, Goal) :-
        call(Goal, I1, Elem1, Elem2, Elem3),
        succ(I1, I),
        mapilist_(Tail1, Tail2, Tail3, I, Goal).

mapilist(Goal, I, List1, List2, List3, List4) :-
        mapilist_(List1, List2, List3, List4, I, Goal).

mapilist_([], [], [], [], _, _).
mapilist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], [Elem4|Tail4], I1, Goal) :-
        call(Goal, I1, Elem1, Elem2, Elem3, Elem4),
        succ(I1, I),
        mapilist_(Tail1, Tail2, Tail3, Tail4, I, Goal).

mapilist(Goal, I, List1, List2, List3, List4, List5) :-
        mapilist_(List1, List2, List3, List4, List5, I, Goal).

mapilist_([], [], [], [], [], _, _).
mapilist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], [Elem4|Tail4], [Elem5|Tail5], I1, Goal) :-
        call(Goal, I1, Elem1, Elem2, Elem3, Elem4, Elem5),
        succ(I1, I),
        mapilist_(Tail1, Tail2, Tail3, Tail4, Tail5, I, Goal).

mapilist(Goal, I, List1, List2, List3, List4, List5, List6) :-
        mapilist_(List1, List2, List3, List4, List5, List6, I, Goal).

mapilist_([], [], [], [], [], [], _, _).
mapilist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], [Elem4|Tail4], [Elem5|Tail5], [Elem6|Tail6], I1, Goal) :-
        call(Goal, I1, Elem1, Elem2, Elem3, Elem4, Elem5, Elem6),
        succ(I1, I),
        mapilist_(Tail1, Tail2, Tail3, Tail4, Tail5, Tail6, I, Goal).
