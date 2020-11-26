/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, VU University Amsterdam
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

:- use_module(library(rocksdb)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(apply_macros)).

:- debug(merge).

open :-
	open("test_merge").
open(DB) :-
	rocks_open(DB, _DB,
		   [ alias(mdb),
		     open(once),
		     merge(merge),
		     value(term)
		   ]).

close :-
	rocks_close(mdb).

merge(partial, _Key, Left, Right, Result) :-
	debug(merge, 'Merge partial ~p ~p', [Left, Right]),
	ord_union(Left, Right, Result).
merge(full, _Key, Initial, Additions, Result) :-
	debug(merge, 'Merge full ~p ~p', [Initial, Additions]),
	append([Initial|Additions], List),
	sort(List, Result).

set(Key, Values) :-
	rocks_put(mdb, Key, Values).

merge(Key, Values) :-
	rocks_merge(mdb, Key, Values).

fetch(Key, Values) :-
	rocks_get(mdb, Key, Values).

t(Set) :-
	open,
	set(t1, [1]),
	merge(t1, [2]),
	fetch(t1, Set).

demo(N) :-
	open("test_merge"),
	set(int_set, [1]),
	forall(between(1, N, I),
	       (   merge(int_set, [I]),
		   (   I mod 10 =:= 0
		   ->  fetch(int_set, Set),
		       assertion(numlist(1, I, Set))
		   ;   true
		   )
	       )).

demo2(N) :-
	open("test_merge"),
	set(int_set, [1]),
	forall(between(1, N, I),
	       (   fetch(int_set, Set0),
		   ord_add_element(Set0, I, Set1),
		   set(int_set, Set1),
		   (   I mod 10 =:= 0
		   ->  fetch(int_set, Set),
		       assertion(numlist(1, I, Set))
		   ;   true
		   )
	       )).
