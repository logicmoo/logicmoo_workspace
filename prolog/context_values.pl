/*  Part of Extended libraries for Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2014, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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
    ( nb_current(Name, OldValue) ->
      OldValue0 = OldValue,
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
    ( nb_current(Name, Value) ->
      setup_call_cleanup(nb_delete(Name),
                         (Goal, nb_setval(Name, Value)),
                         nb_setval(Name, Value))
    ; Goal
    ).

:- meta_predicate with_values(0, +, ?, +).
with_values(Goal, Names, OldValues, NewValues) :-
    maplist(b_getval, Names, OldValues),
    maplist(b_setval, Names, NewValues),
    Goal,
    maplist(b_setval, Names, OldValues).

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

