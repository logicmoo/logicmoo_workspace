/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
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

:- module(test_pcache,
          [ test_pcache/0
          ]).
:- use_module(user:'../prolog/cache_rocks').
:- use_module('../prolog/cache_rocks').
:- use_module('../prolog/signature').
:- use_module(library(plunit)).
:- use_module(library(filesex)).
:- use_module(library(debug)).

test_pcache :-
    run_tests([ pcache
              ]).

:- begin_tests(pcache,
               [ setup(create_test_db(Dir)),
                 cleanup(clean_db(Dir))
               ]).

test(basic, Xs == [8]) :-
    add_table(2),
    findall(X, cached(table(2, 4, X)), Xs),
    assertion((this_cache_property(table(2, 4, _), count(Count)), Count =:= 1)),
    findall(Y, cached(table(2, 4, Y)), Ys),
    Xs =@= Ys.
test(subsumes, Xs == As) :-
    add_table(3),
    findall(A, table(3, _, A), As),
    findall(X, cached(table(3, _, X)), Xs),
    assertion((this_cache_property(table(3, _, _), count(C1)), C1 =:= 10)),
    findall(Y, cached(table(3, _, Y)), Ys),
    Xs =@= Ys,
    findall(Z, cached(table(3, 4, Z)), Zs),
    assertion(Zs =:= 12),
    assertion((this_cache_property(table(3, 4, _), count(C2)), C2 =:= 1)).
test(resume, Xs == [5,10,15]) :-
    add_table(5),
    findall(X, limit(3, cached(table(5, _, X))), Xs),
    assert_property(table(5, _, _), count(3)),
    assert_property(table(5, _, _), state(partial)),
    findall(X, limit(5, cached(table(5, _, X))), Ys),
    assertion(Ys == [5,10,15,20,25]),
    forall(cached(table(5, _, X)), true),
    assert_property(table(5, _, _), state(complete)).
test(modify) :-
    add_table(4),
    forall(cached(table(4, 4, _)), true),
    predicate_property(table(_,_,_), last_modified_generation(G0)),
    deep_predicate_hash(table(_,_,_), Hash0),
    assert_property(table(4, 4, _), count(1)),
    add_table(6),
    predicate_property(table(_,_,_), last_modified_generation(G1)),
    assertion(G1 > G0),
    deep_predicate_hash(table(_,_,_), Hash1),
    assertion(Hash1 \== Hash0),
    assertion(\+ this_cache_property(table(4, 4, _), _)),
    forall(cached(table(4, 4, _)), true),
    assert_property(table(4, 4, _), count(1)).

:- end_tests(pcache).

:- meta_predicate
    assert_property(:, ?).

assert_property(Goal, P) :-
    P =.. [Name,Value],
    Gen =.. [Name,Value0],
    assertion((this_cache_property(Goal, Gen), Value == Value0)).


		 /*******************************
		 *              DATA		*
		 *******************************/

:- dynamic table/3.

add_table(N) :-
    forall(between(1, 10, I),
           (   V is N*I,
               assertz(table(N, I, V))
           )).

clean_db :-
    retractall(table(_,_,_)).


		 /*******************************
		 *         DB MANAGEMENT	*
		 *******************************/

create_test_db(Dir) :-
    current_prolog_flag(pid, P),
    atom_concat('test-db-', P, Dir),
    cache_open(Dir).

clean_db(Dir) :-
    cache_close,
    delete_directory_and_contents(Dir),
    clean_db.
