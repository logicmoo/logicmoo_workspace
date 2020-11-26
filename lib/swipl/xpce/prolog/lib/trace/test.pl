/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2001-2011, University of Amsterdam
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
test file for the tracer package.  This file contains specific test
situations.  Test 1 2 3 4.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%:- ensure_loaded(trace).

:- dynamic status/1.

t1 :-
    trace,
    forall(between(0, 10, X), assert(status(X))),
    (   status(S),
        S == 5
    ).

t2 :-
    (   \+ foo = foo
    ->  writeln(yes)
    ;   writeln(no)
    ).

t3 :-
    A = foo(X),
    B = bar(X),
    writeln((A=B)),
    true.

t4 :-
    a(X),
    b(Y),
    !,
    format('X=~w, Y=~w~n', [X, Y]).
t4.

a(1).
a(2).

b(1).
b(2).

t5 :-
    a(_A),
    forall(a(X), b(X)),
    b(_B).

t6 :-
    findall(A, a(A), As),
    writeln(As).

t7 :-
    format('Please enter your name, followed by a dot~n', []),
    read(Name),
    format('Hello ~w~n', [Name]).
