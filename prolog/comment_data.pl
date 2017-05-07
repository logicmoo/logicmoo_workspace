/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
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

:- module(comment_data, [comment_data/2, enable/0, disable/0]).

:- dynamic comment_data/2.

%!  process_comment_data(+Comments, +Term) is semidet
%
%   This comment_hook hack allow us to write copy-pasteable data as comment, to
%   facilitate output comparisons:

process_comment_data(Comments, Term) :-
    Term = (test(_Test) :- _),
    ( member(_-Comment, Comments),
      get_comment_data(Comment, Name, Out),
      retractall(comment_data(Name, _)),
      assertz(comment_data(Name, Out)),
      fail
    ; true
    ).

get_comment_data(Comment, Name, Out) :-
    string_concat("/* $", Out0, Comment),
    sub_string(Out0, Before, Length, After, "$\n"),
    sub_string(Out0, 0, Before, _, SName),
    atom_string(Name, SName),
    OutPos is Before + Length,
    sub_string(Out0, OutPos, After, _, Out1),
    string_concat(Out, "*/", Out1).

:- dynamic enabled/0.

enable :-
    retractall(enabled),
    assertz(enabled).

disable :-
    retractall(enabled).

:- multifile prolog:comment_hook/3.
% :- dynamic prolog:comment_hook/3.

prolog:comment_hook(Comments, _TermPos, Term) :-
    enabled,
    process_comment_data(Comments, Term).
