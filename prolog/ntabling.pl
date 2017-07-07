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

:- module(ntabling,
          [ (table)/1,
            abolish_all_tables/0,
            abolish_table_subgoals/1,
            start_tabling/2,

            op(1150, fx, table)]).

:- use_module(library(tabling), []).

/** <module> Naive Tabled execution

This library provides tabled execution, in the same way as library(tabling), but
is implemented in a naive way, just collecting all the solutions and later on
asserting them in the prolog database.
*/

:- dynamic
       '$table_data'/1,
       '$table_data'/2.

:- volatile
       '$table_data'/1,
       '$table_data'/2.

:- meta_predicate
       start_tabling(+, 0),
       abolish_table_subgoals(:).

table(PIList) :-
    throw(error(context_error(nodirective, table(PIList)), _)).

start_tabling(M:Goal, WrappedHead) :-
    collect_sols(M:Goal, WrappedHead, Hash),
    '$table_data'(Hash, Goal).

goal_hash(M:Goal, Hash) :-
    ( '$expand':is_meta_call(Goal, M, _)
    ->Meta = M:Goal
    ; Meta = Goal
    ),
    variant_hash(Meta, Hash).

collect_sols(M:Goal, WrappedHead, Hash) :-
    goal_hash(M:Goal, Hash),
    (   '$table_data'(Hash)
    *-> true
    ;   forall(WrappedHead, assertz('$table_data'(Hash, Goal))),
        assertz('$table_data'(Hash))
    ).

abolish_all_tables :-
    abolish_table_hash(_).

:- meta_predicate
       abolish_ntable_subgoals(0).

abolish_table_subgoals(M:Goal) :-
    goal_hash(M:Goal, Hash),
    abolish_table_hash(Hash).

abolish_table_hash(Hash) :-
    retractall('$table_data'(Hash)),
    retractall('$table_data'(Hash, _)).
