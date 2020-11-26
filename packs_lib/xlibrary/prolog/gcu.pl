/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2019, Process Design Center, Breda, The Netherlands.
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

:- module(gcu,
          [greatest_common_unifier/3]).

greatest_common_unifier(Term1, Term2, Term1) :-
    Term1 == Term2,
    !.
greatest_common_unifier(Term1, Term2, Term) :-
    nonvar(Term1),
    nonvar(Term2),
    functor(Term1, F, A),
    functor(Term2, F, A),
    !,
    functor(Term, F, A),
    greatest_common_unifier(1, Term1, Term2, Term).
greatest_common_unifier(_, _, _).

greatest_common_unifier(N, Term1, Term2, Term) :-
    arg(N, Term1, Arg1),
    !,
    arg(N, Term2, Arg2),
    greatest_common_unifier(Arg1, Arg2, Arg),
    arg(N, Term,  Arg),
    succ(N, N1),
    greatest_common_unifier(N1, Term1, Term2, Term).
greatest_common_unifier(_, _, _, _).
