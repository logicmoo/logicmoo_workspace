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

:- module(context_values, [context_name/2,
                           with_value/3,
                           with_value/4,
                           get_context_value/2,
                           set_context_value/2,
                           current_context_value/2,
                           with_context_values/3,
                           with_context_values/4,
                           with_context_value/3,
                           with_context_value/4,
                           without_context_value/2,
                           without_context_value/3
                           ]).

context_name(M:Name, ContextName) :-
    context_name(M, Name, ContextName).

context_name(M, Name, ContextName) :-
    atomic_list_concat([M, Name], ':', ContextName).

:- meta_predicate get_context_value(:, ?).
get_context_value(Name, Value) :-
    context_name(Name, ContextName),
    b_getval(ContextName, Value).

:- meta_predicate set_context_value(:, ?).
set_context_value(Name, Value) :-
    context_name(Name, ContextName),
    b_setval(ContextName, Value).

:- meta_predicate current_context_value(:, ?).
current_context_value(Name, Value) :-
    context_name(Name, ContextName),
    nb_current(ContextName, Value).

:- meta_predicate with_value(0, +, +).
with_value(Goal, Name, NewValue) :-
    with_value(Goal, Name, _, NewValue).

:- meta_predicate with_context_value(0, :, +).
with_context_value(Goal, Name, Value) :-
    context_name(Name, ContextName),
    with_value(Goal, ContextName, Value).

:- meta_predicate with_context_value(0, :, ?, +).
with_context_value(Goal, Name, OldValue, NewValue) :-
    context_name(Name, ContextName),
    with_value(Goal, ContextName, OldValue, NewValue).

:- meta_predicate with_value(0, +, ?, +).
with_value(Goal, Name, OldValue0, NewValue) :-
    ( nb_current(Name, OldValue)
    ->OldValue0 = OldValue,
      b_setval(Name, NewValue),
      Goal,
      b_setval(Name, OldValue)
    ; b_setval(Name, NewValue),
      Goal,
      nb_delete(Name)
    ).

:- meta_predicate without_context_value(0, :).
without_context_value(Goal, Name) :-
    without_context_value(Goal, Name, _).

:- meta_predicate without_context_value(0, :, ?).
without_context_value(Goal, Name, Value) :-
    context_name(Name, ContextName),
    without_value(Goal, ContextName, Value).

without_value(Goal, Name, Value) :-
    ( nb_current(Name, Value)
    ->setup_call_cleanup(nb_delete(Name),
                         (Goal, nb_setval(Name, Value)),
                         nb_setval(Name, Value))
    ; Goal
    ).

update_value(Name, OldValue1, NewValue, Cleanup) :-
    ( nb_current(Name, OldValue)
    ->OldValue1 = OldValue,
      Cleanup = set(Name, OldValue)
    ; Cleanup = del(Name)
    ),
    b_setval(Name, NewValue).

cleanup(set(Name, OldValue)) :- b_setval(Name, OldValue).
cleanup(del(Name)) :- nb_delete(Name).

:- meta_predicate with_values(0, +, ?, +).
with_values(Goal, Names, OldValues, NewValues) :-
    maplist(update_value, Names, OldValues, NewValues, Cleanups),
    Goal,
    maplist(cleanup, Cleanups).

:- meta_predicate with_context_values(0, :, ?, +).
with_context_values(Goal, M:Names, OldValues, NewValues) :-
    maplist(context_name(M), Names, ContextNames),
    with_values(Goal, ContextNames, OldValues, NewValues).

with_values(Goal, Names, Values) :- with_values_(Names, Values, Goal).

with_values_([], [], Goal) :- Goal.
with_values_([Name|Names], [Value|Values], Goal) :-
    with_values_(Names, Values, with_value(Goal, Name, Value)).

:- meta_predicate with_context_values(0, :, +).
with_context_values(Goal, M:Names, Values) :-
    maplist(context_name(M), Names, ContextNames),
    with_values(Goal, ContextNames, Values).

