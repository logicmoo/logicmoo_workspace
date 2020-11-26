/*  Part of Extended Libraries for SWI-Prolog

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

:- module(transpose, [transpose/2]).

:- use_module(library(apply)).

%% transpose(+Matrix, ?Transpose)
%
%  Transpose a list of lists of the same length.
%
%  Its implementation is more general than the one in library/clp/clpfd.pl,
%  since it allows to work with incomplete double lists, provided that the
%  transpose have sense. Example:
%
%  ==
%  ?- transpose([[1,2,3,4],[5,6,7,8],[9,10],[11,12],[13]], Ts).
%  Ts = [[1, 5, 9, 11, 13], [2, 6, 10, 12], [3, 7], [4, 8]].
%  ==


transpose([], L) :-
    once(maplist(=([]), L)).
transpose([C|Cs], L) :-
    deal_column(C, L, R),
    transpose(Cs, R).

deal_column([], L, L) :-
    once(maplist(=([]), L)).
deal_column([E|Es], [[E|R1]|L], [R1|R]) :-
    deal_column(Es, L, R).
