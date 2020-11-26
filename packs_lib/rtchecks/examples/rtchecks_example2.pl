/*  Part of Run-Time Checker for Assertions

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
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

:- module(rtchecks_example2,
          [pred1/2, pred2/2, pred3/1, pred4/2, aconcat/3,
           bad_concat/3, ppp1/0, test_all/2, test_atm/1]).

:- use_module(library(assertions)).
:- use_module(library(plprops)).

/** <module> Examples of assertions for processing by the run-time checker.
*/

:- pred pred1/2 : (int * int).
:- pred pred1/2 : (atm * atm).

pred1(X, Y) :-
        display(pred1(X, Y)),
        nl.

pred2(X, Y) :-
        pred1(X, Y).

:- pred pred3(X) : int(X).

pred3(X) :-
        display(X),
        nl.

:- check comp pred4/2 : int * int + (not_fails, not_fails).
:- check comp pred4/2 : atm * atm + not_fails.

pred4(X, Y) :-
        display(p(X, Y)),
        nl.

:- export(pred5/2).

:- pred pred5/2.

pred5(a, b).

:- check success aconcat(A, B, X) : (A = [1, 2], B = [3]) => (X == [1, 2, 4]).

:- check success aconcat/3 : (list * list * var) => (list * list * list).

aconcat([],    X, X).
aconcat([X|Y], Z, [X|T]) :-
        aconcat(Y, Z, T).

:- check success bad_concat/3
        : (list * list * var) => (list * list * list).

bad_concat(_A, _X, a).

:- pred test_all(A, B) :: int(A) : int(A) => int(B) + not_fails.
:- pred test_all(A, B) :: atm(A) : atm(A) => atm(B) + not_fails.

test_all(A, A).

:- pred test_atm(A) : atm(A).
test_atm(A) :-
        test_atm2(A).

:- pred test_atm2(A) : atm(A).
test_atm2(_) :- fail.
test_atm2(A) :-
        test_atm3(A),
        display(done),
        nl.

:- pred test_atm3(A) : int(A).
test_atm3(A) :-
        display(A),
        nl.

:- check comp ppp1/0 + not_fails.

ppp1.
