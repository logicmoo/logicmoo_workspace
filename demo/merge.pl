:- use_module(rocksdb).
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
