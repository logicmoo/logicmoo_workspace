/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
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

:- module(near, [near/3]).

% :- prop near(A, B, Eps) # "Verifies that abs(@var{B} -
%    @var{A})/(abs(@var{B}) + (@var{A})) =< @var{Eps}.".

near(A, B, _) :-
    A==B, !.
near(A, _, _) :-
    var(A),
    !,
    fail.
near(_, B, _) :-
    var(B),
    !,
    fail.
near(A, B, Eps) :-
    number(A),
    number(B),
    !,
    near_num(A, B, Eps).
near(A, B, Eps) :-
    functor(A, F, N),
    functor(B, F, N),
    near_args(N, A, B, Eps).

near_args(0, _, _, _) :- !.
near_args(N, A, B, Eps) :-
    arg(N, A, ArgA),
    arg(N, B, ArgB),
    !,
    near(ArgA, ArgB, Eps),
    N1 is N - 1,
    near_args(N1, A, B, Eps).
near_args(_, _, _, _).

near_num(A,      B,      Eps) :- A =:= 0, !, abs(B) =< Eps.
near_num(A,      B,      Eps) :- B =:= 0, !, abs(A) =< Eps.
near_num(A,      B,      Eps) :-
    2 * abs(B - A) / (abs(A) + abs(B)) =< Eps.
