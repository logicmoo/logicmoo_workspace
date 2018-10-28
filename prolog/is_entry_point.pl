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

:- module(is_entry_point, [is_entry_point/2]).

is_entry_property(exported).
is_entry_property((public)).
is_entry_property(imported_from(_)).

:- dynamic http_dispatch:handler/4.
:- multifile http_dispatch:handler/4.

:- multifile
    is_entry_point_hook/2.

is_entry_point_hook(attribute_goals(_, _, _), _).
is_entry_point_hook(term_expansion(_, _), _).
is_entry_point_hook(goal_expansion(_, _), _).
is_entry_point_hook(term_expansion(_, _, _, _), _).
is_entry_point_hook(goal_expansion(_, _, _, _), _).
is_entry_point_hook(thread_message_hook(_, _, _), user).
is_entry_point_hook(attr_unify_hook(_, _), _).
is_entry_point_hook(prolog_exception_hook(_, _, _, _), user).
is_entry_point_hook(prolog_load_file(_, _), user).
is_entry_point_hook(message_hook(_, _, _), user).
is_entry_point_hook(prolog_trace_interception(_, _, _, _), user).
is_entry_point_hook(_, prolog).
is_entry_point_hook(doc_db(_, _, _, _), assrt_lib).
is_entry_point_hook(H, sandbox) :- predicate_property(sandbox:H, multifile).
is_entry_point_hook(goal_colours(_, _, _), prolog_colour).

is_entry_point(H, M) :- is_entry_point_hook(H, M), !.
is_entry_point(H, M) :-
    functor(H, Name, A),
    A>0,
    succ(A2, A),
    functor(H2, Name, A2),
    http_dispatch:handler(_, M:H2, _, _), !.
is_entry_point(H, M) :-
    is_entry_property(Prop),
    predicate_property(M:H, Prop), !.
