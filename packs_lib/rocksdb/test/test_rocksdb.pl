/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
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

:- module(test_rocksdb,
	  [ test_rocksdb/0
	  ]).
:- use_module(library(rocksdb)).
:- use_module(library(plunit)).
:- use_module(library(debug)).

test_rocksdb :-
	run_tests([ rocks,
		    terms,
		    types,
		    merge,
                    properties
		  ]).

:- begin_tests(rocks, [cleanup(delete_db)]).

test(basic, Noot == noot) :-
	test_db(Dir),
	rocks_open(Dir, RocksDB, []),
	rocks_put(RocksDB, aap, noot),
	rocks_get(RocksDB, aap, Noot),
	rocks_delete(RocksDB, aap),
	assertion(\+ rocks_get(RocksDB, aap, _)),
	rocks_close(RocksDB).
test(basic, Noot == noot) :-
	test_db(Dir),
	rocks_open(Dir, RocksDB, []),
	rocks_put(RocksDB, aap, noot),
	rocks_close(RocksDB),
        rocks_open(Dir, RocksDB2, [mode(read_only)]),
	rocks_get(RocksDB2, aap, Noot),
	rocks_close(RocksDB2).
test(open_twice, error(rocks_error(_))) :-
	test_db(Dir),
	setup_call_cleanup(
	    rocks_open(Dir, RocksDB1, []),
	    call_cleanup(rocks_open(Dir, RocksDB2, []),
			 rocks_close(RocksDB2)),
	    rocks_close(RocksDB1)).
test(batch, Pairs == [zus-noot]) :-
	test_db(Dir),
	rocks_open(Dir, RocksDB, []),
	rocks_put(RocksDB, aap, noot),
	rocks_get(RocksDB, aap, Value),
	rocks_batch(RocksDB,
		    [ delete(aap),
		      put(zus, Value)
		    ]),
	findall(K-V, rocks_enum(RocksDB, K, V), Pairs),
	rocks_close(RocksDB).

:- end_tests(rocks).

:- begin_tests(terms, [cleanup(delete_db)]).

test(basic, Noot-Noot1 == noot(mies)-noot(1)) :-
	test_db(Dir),
	rocks_open(Dir, RocksDB,
		   [ key(term),
		     value(term)
		   ]),
	rocks_put(RocksDB, aap, noot(mies)),
	rocks_get(RocksDB, aap, Noot),
	rocks_put(RocksDB, aap(1), noot(1)),
	rocks_get(RocksDB, aap(1), Noot1),
	rocks_close(RocksDB).

:- end_tests(terms).

:- begin_tests(types, [cleanup(delete_db)]).

test(int32) :-
	Min = -100, Max = 100,
	test_db(Dir),
	rocks_open(Dir, RocksDB,
		   [ key(atom),
		     value(int32)
		   ]),
	forall(between(Min, Max, I),
	       ( rocks_put(RocksDB, key, I),
		 assertion(rocks_get(RocksDB, key, I)))),
	rocks_close(RocksDB).

test(int64) :-
	Min = -100, Max = 100,
	test_db(Dir),
	rocks_open(Dir, RocksDB,
		   [ key(atom),
		     value(int64)
		   ]),
	forall(between(Min, Max, I),
	       ( rocks_put(RocksDB, key, I),
		 assertion(rocks_get(RocksDB, key, I)))),
	rocks_close(RocksDB).

test(float) :-
	Min = -100, Max = 100,
	test_db(Dir),
	rocks_open(Dir, RocksDB,
		   [ key(atom),
		     value(float)
		   ]),
	forall(between(Min, Max, I),
	       ( F is sin(I),
		 rocks_put(RocksDB, key, F),
		 rocks_get(RocksDB, key, F2),
		 assertion(abs(F-F2) < 0.00001))),
	rocks_close(RocksDB).

test(double) :-
	Min = -100, Max = 100,
	test_db(Dir),
	rocks_open(Dir, RocksDB,
		   [ key(atom),
		     value(double)
		   ]),
	forall(between(Min, Max, I),
	       ( F is sin(I),
		 rocks_put(RocksDB, key, F),
		 rocks_get(RocksDB, key, F2),
		 assertion(F=:=F2))),
	rocks_close(RocksDB).

:- end_tests(types).

:- begin_tests(merge, [cleanup(delete_db)]).

test(set, FinalOk == Final) :-
	N = 100,
	numlist(1, N, FinalOk),
	test_db(Dir),
	rocks_open(Dir, DB,
		   [ merge(merge),
		     value(term)
		   ]),
	rocks_put(DB, set, []),
	forall(between(1, N, I),
	       (   rocks_merge(DB, set, [I]),
		   (   I mod 10 =:= 0
		   ->  rocks_get(DB, set, Set),
		       assertion(numlist(1, I, Set))
		   ;   true
		   )
	       )),
	rocks_get(DB, set, Final),
	rocks_close(DB).
test(new, Final == [1]) :-
	test_db(Dir),
	rocks_open(Dir, DB,
		   [ merge(merge),
		     value(term)
		   ]),
	rocks_merge(DB, empty, [1]),
	rocks_get(DB, empty, Final),
	rocks_close(DB).

merge(partial, _Key, Left, Right, Result) :-
	debug(merge, 'Merge partial ~p ~p', [Left, Right]),
	ord_union(Left, Right, Result).
merge(full, _Key, Initial, Additions, Result) :-
	debug(merge, 'Merge full ~p ~p', [Initial, Additions]),
	append([Initial|Additions], List),
	sort(List, Result).

:- end_tests(merge).

:- begin_tests(properties, [cleanup(delete_db)]).

test(basic) :-
	test_db(Dir),
	rocks_open(Dir, RocksDB,
		   [ key(term),
		     value(term)
		   ]),
	rocks_put(RocksDB, aap, noot(mies)),
	rocks_put(RocksDB, aap(1), noot(1)),
	rocks_property(RocksDB, estimate_num_keys(Num)),
        assertion(integer(Num)).

:- end_tests(properties).


		 /*******************************
		 *	       UTIL		*
		 *******************************/

test_db('/tmp/test_rocksdb').

delete_db :-
	test_db(DB),
	delete_db(DB).

delete_db(DB) :-
	exists_directory(DB), !,
	delete_directory_and_contents(DB).
delete_db(_).
