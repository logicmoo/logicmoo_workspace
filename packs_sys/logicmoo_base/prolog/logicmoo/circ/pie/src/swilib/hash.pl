%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2005, 2016, 2020 Christoph Wernhard
%%%%
%%%% This program is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%% 
%%%% This program is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%% 
%%%% You should have received a copy of the GNU General Public License
%%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(hash, [mk_ht/2,
 		 ht_put/3,
		 ht_del/2,
 		 ht_get/3,
 		 ht_flag/4,
 		 ht_key/2,
		 ht_capacity/2,
		 mk_htm/2,
		 htm_add/3,
		 htm_del/3,
		 htm_get/3,
		 htm_key/2,
		 htm_capacity/2,
		 mk_htb/2,
		 htb_put/3,
		 htb_get/3,
		 htb_del/2,
		 htb_key/2,
		 htb_capacity/2]).

/** <module> Hash tables implemented with destructive operations

Three types of hash tables, indicated by predicate pre- or postfix,
respectively:

  * ht
    non-backtrackable, associate a single value with a key
  * htm
    non-backtrackable, associate a multiset of values with a key
  * htb
    backtrackable, associate a single value with a key

We call hash tables of these types *HT hash table*, *HTM hash table* and *HTB
hash table*, respectively.

The hash tables are created with a specified capacity.  This is usually an
estimation of the expected number of entries. It is no limitation, but if
chosen too small, access is slowed.
  
The non-backtrable versions are implemented using nb_setarg/3 and
nb_linkarg/3.  For these versions, values are copied at insertion and the
state of the table is not reversed at backtracking.

@author Christoph Wernhard
  
@tbd Resizing if the capacity is exceeded by some load factor.
  
*/  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% NON-BACKTRACKABLE BASIC HASHTABLES
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! mk_ht(++Capacity, --HashTable) is det.
%
%  Create a HT hash table.
%
%  @arg Capacity A natural number specifying the capacity of
%  the created hash table.
%
%  @arg HashTable The newly created HT hash table.
%
mk_ht(Capacity, HashTable) :-
	functor(Table, k, Capacity),
	init_table(Capacity, Table),
	HashTable = ht(Table, Capacity).

init_table(N, Table) :-
	N > 0,
	arg(N, Table, []),
	N1 is N - 1,
	init_table(N1, Table).
init_table(0, _).


%!  ht_capacity(+HashTable, -Capacity) is semidet.
%
% Unify `Capacity` with the capacity of `HashTable`.
%
% @arg HashTable A HT hash table.
%
% @arg Capacity A natural number > 0.
% 
ht_capacity(ht(_, Size), Size).


%! ht_get(+HashTable, +Key, -Value) is semidet.
%
%  Binds `Value` to the value associated with `Key` in `HashTable`.  Fails if
%  no value or a value that does not unify with `Value` is associated with
%  `Key`.
%
%  @arg HashTable A HT hash table.
%
%  @arg Key A Term.
%
%  @arg Value A Term.
%
ht_get(ht(Table, Size), Key, Value) :-
	term_hash(Key, N),
	Index is 1 + (N mod Size),
	arg(Index, Table, Entries),
	memberchk(Key-Value1, Entries),
	Value1 = Value.

%! ht_put(+HashTable, +Key, +Value) is det.
%
%  Associates `Value` with `Key` in `HashTable`. 
%
%  @arg HashTable A HT hash table.
%
%  @arg Key A Term.
%
%  @arg Value A Term.
%
ht_put(ht(Table, Size), Key, Value) :-
	term_hash(Key, N),
	Index is 1 + (N mod Size),
	arg(Index, Table, Entries),
	( member(KV, Entries), arg(1, KV, Key1), Key1 == Key ->
	  nb_setarg(2, KV, Value)  
	; Entries == [] ->
	  nb_setarg(Index, Table, [Key-Value])
	; duplicate_term([Key-Value|_], KVE1),
	  nb_linkarg(2, KVE1, Entries),
	  nb_linkarg(Index, Table, KVE1)
        ).

%! ht_flag(+HashTable, +Key, --OldValue, +NewValueSpec) is det.
%
%  Get and put numeric values in one step, similar to flag/3: The value
%  associated with `Key` in `HashTable` is set to the result of arithmetically
%  evaluating `NewValueSpec`, where the variable `OldValue` is bound to the
%  value associated with `Key`, or 0 in case there is no associated value.
%  Example: `ht_put(HT, mykey, V, V+1)`.
%
%  @arg HashTable A HT hash table.
%
%  @arg Key A term.
%
%  @arg OldValue A variable.
%
%  @arg NewValueSpec An arithmetic expression.
%
ht_flag(ht(Table, Size), Key, OldValue, NewValueExpression) :-
	term_hash(Key, N),
	Index is 1 + (N mod Size),
	arg(Index, Table, Entries),
	( member(KV, Entries), arg(1, KV, Key1), Key1 == Key ->
	  arg(2, KV, OldValue),
	  NewValue is NewValueExpression,	    
	  nb_setarg(2, KV, NewValue)  
	; Entries == [] ->
	  OldValue = 0,
	  NewValue is NewValueExpression,	    
	  nb_setarg(Index, Table, [Key-NewValue])
	; OldValue = 0,
	  NewValue is NewValueExpression,	    
	  duplicate_term([Key-NewValue|_], KVE1),
	  nb_linkarg(2, KVE1, Entries),
	  nb_linkarg(Index, Table, KVE1)
        ).

%! ht_del(+HashTable, +Key) is semidet.
%
%  Deletes the association with `Key` in `HashTable`. 
%  Fails if there is no such association.
%
%  @arg HashTable A HT hash table.
%
%  @arg Key A term.
%
ht_del(ht(Table, Size), Key) :-
	term_hash(Key, N),
	Index is 1 + (N mod Size),
	arg(Index, Table, Entries),
	del_unifiable_nb_linkarg(Entries, Key-_, Index, Table).

%! ht_key(+HashTable, -Key) is nondet.
%
%  Enumerate as `Key` the keys with associated value in `HashTable`.
%
%  @arg HashTable A HT hash table.
%
%  @arg Key A term.
%
ht_key(ht(Table, Size), Key) :-
	ht_key_1(Table, Size, Key).

ht_key_1(T, N, K) :-
	N > 0,
	arg(N, T, E),
	member(K-_, E).
ht_key_1(T, N, K) :-
	N > 0,	
	N1 is N - 1,
	ht_key_1(T, N1, K).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% NON-BACKTRACKABLE HASHTABLES WITH MULTISETS OF VALUES 
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! mk_htm(++Capacity, --HashTable) is det.
%
%  Create a HTM hash table.
%
%  @arg Capacity A natural number specifying the capacity of
%  the created hash table.
%
%  @arg HashTable The newly created HTM hash table.
%
mk_htm(Capacity, HashTable) :-
	functor(Table, k, Capacity),
	init_table(Capacity, Table),
	HashTable = htm(Table, Capacity).

%!  htm_capacity(+HashTable, -Capacity) is semidet.
%
% Unify `Capacity` with the capacity of `HashTable`.
%
% @arg HashTable A HTM hash table.
%
% @arg Capacity A natural number > 0.
% 
htm_capacity(htm(_, Size), Size).

%! htm_get(+HashTable, +Key, -Value) is nondet.
%
%  Enumerate as `Value` the occurrences of values associated with `Key` in
%  `HashTable`.
% 
%  @arg HashTable A HTM hash table.
%
%  @arg Key A Term.
%
%  @arg Value A Term.
%
htm_get(htm(Table, Size), Key, Value) :-
	term_hash(Key, N),
	Index is 1 + (N mod Size),
	arg(Index, Table, Entries),
	member(Key-Value, Entries).

%! htm_add(+HashTable, +Key, +Value) is det.
%
%  Add an occurrence of `Value` to the occurrences associated with `Key` in
%  `HashTable`.
% 
%  @arg HashTable A HTM hash table.
%
%  @arg Key A Term.
%
%  @arg Value A Term.
%
htm_add(htm(Table, Size), Key, Value) :-
	term_hash(Key, N),
	Index is 1 + (N mod Size),
	arg(Index, Table, Entries),
	( Entries == [] ->
	  nb_setarg(Index, Table, [Key-Value])
	; duplicate_term([Key-Value|_], KVE1),
	  nb_linkarg(2, KVE1, Entries),
	  nb_linkarg(Index, Table, KVE1)
        ).

%! htm_del(+HashTable, +Key, +Value) is semidet.
%
%  Delete an occurence of `Value` from `Key` in `HashTable`. `Key` and `Value`
%  are identified there using =@=/2. Fails if there is no such occurrence.
%
%  @arg HashTable A HTM hash table.
%
%  @arg Key A term.
%
%  @arg Value A term.
%
htm_del(htm(Table, Size), Key, Value) :-
	term_hash(Key, N),
	Index is 1 + (N mod Size),
	arg(Index, Table, Entries),
	del_variant_nb_linkarg(Entries, Key-Value, Index, Table).

del_variant_nb_linkarg(L, E, PlaceArg, Place) :-
	( L == [] ->
	  fail
	; arg(1, L, Car),
	  arg(2, L, Cdr),
	  ( Car =@= E ->
	    nb_linkarg(PlaceArg, Place, Cdr)
	  ; del_variant_nb_linkarg(Cdr, E, 2, L)
	  )
	).

del_unifiable_nb_linkarg(L, E, PlaceArg, Place) :-
	( L == [] ->
	  fail
	; arg(1, L, Car),
	  arg(2, L, Cdr),
	  ( Car = E ->
	    nb_linkarg(PlaceArg, Place, Cdr)
	  ; del_unifiable_nb_linkarg(Cdr, E, 2, L)
	  )
	).

%! htm_key(+HashTable, -Key) is nondet.
%
%  Enumerate as `Key` the keys with an associated value occurrence in
%  `HashTable`.
%
%  @arg HashTable A HTM hash table.
%
%  @arg Key A term.
%
htm_key(htm(Table, Size), Key) :-
	htm_key_1(Table, Size, Key).

htm_key_1(T, N, K) :-
	N > 0,
	arg(N, T, E),
	setof(K1, V^member(K1-V, E), Ks),
	member(K, Ks).
htm_key_1(T, N, K) :-
	N > 0,	
	N1 is N - 1,
	htm_key_1(T, N1, K).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% BACKTRACKABLE BASIC HASHTABLES
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! mk_htb(++Capacity, --HashTable) is det.
%
%  Create a HTB hash table.
%
%  @arg Capacity A natural number specifying the capacity of
%  the created hash table.
%
%  @arg HashTable The newly created HTB hash table.
%
mk_htb(Capacity, HashTable) :-
	functor(Table, k, Capacity),
	init_table(Capacity, Table),
	HashTable = htb(Table, Capacity).

%!  htb_capacity(+HashTable, -Capacity) is semidet.
%
% Unify `Capacity` with the capacity of `HashTable`.
%
% @arg HashTable A HTB hash table.
%
% @arg Capacity A natural number > 0.
% 
htb_capacity(htb(_, Size), Size).

%! htb_get(+HashTable, +Key, -Value) is semidet.
%
%  Binds `Value` to the value associated with `Key` in `HashTable`.  Fails if
%  no value or a value that does not unify with `Value` is associated with
%  `Key`.
%
%  @arg HashTable A HTB hash table.
%
%  @arg Key A Term.
%
%  @arg Value A Term.
%
htb_get(htb(Table, Size), Key, Value) :-
	term_hash(Key, N),
	Index is 1 + (N mod Size),
	arg(Index, Table, Entries),
	memberchk(Key-Value1, Entries),
	Value1 = Value.

%! htb_put(+HashTable, +Key, +Value) is det.
%
%  Associates `Value` with `Key` in `HashTable`. 
%
%  @arg HashTable A HTB hash table.
%
%  @arg Key A Term.
%
%  @arg Value A Term.
%
htb_put(htb(Table, Size), Key, Value) :-
	term_hash(Key, N),
	Index is 1 + (N mod Size),
	arg(Index, Table, Entries),
	( member(KV, Entries), KV = Key-_ ->
	  setarg(2, KV, Value)  
	; Entries == [] ->
	  setarg(Index, Table, [Key-Value])
	; setarg(Index, Table, [Key-Value|Entries])
        ).

%! htb_del(+HashTable, +Key) is semidet.
%
%  Deletes the association with `Key` in `HashTable`. 
%  Fails if there is no such association.
%
%  @arg HashTable A HTB hash table.
%
%  @arg Key A term.
%
htb_del(htb(Table, Size), Key) :-
	term_hash(Key, N),
	Index is 1 + (N mod Size),
	arg(Index, Table, Entries),
	( select_once(Key-_, Entries, Entries1) ->
	  setarg(Index, Table, Entries1)
	; fail
	).

select_once(X, [X|Y], Y) :-
	!.
select_once(X, [Y|Z], [Y|U]) :-
	select_once(X, Z, U).

%! htb_key(+HashTable, -Key) is nondet.
%
%  Enumerate as `Key` the keys with associated value in `HashTable`.
%
%  @arg HashTable A HTB hash table.
%
%  @arg Key A term.
%
htb_key(htb(Table, Size), Key) :-
	htb_key_1(Table, Size, Key).

htb_key_1(T, N, K) :-
	N > 0,
	arg(N, T, E),
	member(K-_, E).
htb_key_1(T, N, K) :-
	N > 0,	
	N1 is N - 1,
	htb_key_1(T, N1, K).
