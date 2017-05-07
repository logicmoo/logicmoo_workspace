/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2013, Process Design Center, Breda, The Netherlands.
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

:- module(list_intervals, [list_intervals/2,
                           compact_intervals/2,
                           repack_list/2]).

%!  list_intervals(+From:list,-Pairs:list(pair)) is det.
%!  list_intervals(-From:list,+Pairs:list(pair)) is det.
%
% :- pred list_intervals(?sorted, ?sorted).
%
list_intervals([], []).
list_intervals([From|L], [From-To|PairL]) :-
    list_intervals(L, From, To, PairL).

list_intervals([Elem|L], From, To, PairL) :-
    (nonvar(To) -> From < To ; true), % make it reversible
    succ(From, Elem), !,
    list_intervals(L, Elem, To, PairL).
list_intervals(L, To, To, PairL) :-
    list_intervals(L, PairL).

compact_intervals(L, R) :-
    maplist(compact_interval, L, R).

compact_interval(To-To,  To) :- To \= _-_, !.
compact_interval(FromTo, FromTo).

repack_list(List0, List) :-
    compact_intervals(List1, List0),
    list_intervals(List2, List1),
    sort(List2, List3),
    list_intervals(List3, List4),
    compact_intervals(List4, List).
