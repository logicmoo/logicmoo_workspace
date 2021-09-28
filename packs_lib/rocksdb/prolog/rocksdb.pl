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

:- module(rocksdb,
	  [ rocks_open/3,		% +Directory, -RocksDB, +Options
	    rocks_close/1,		% +RocksDB

	    rocks_put/3,		% +RocksDB, +Key, +Value
	    rocks_merge/3,		% +RocksDB, +Key, +Value
	    rocks_delete/2,		% +RocksDB, +Key
	    rocks_batch/2,		% +RocksDB, +Actions

	    rocks_get/3,		% +RocksDB, +Key, -Value
	    rocks_enum/3,		% +RocksDB, ?Key, ?Value

            rocks_property/2            % +RocksDB, ?Property
	  ]).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_foreign_library(foreign(rocksdb4pl)).

:- meta_predicate
	rocks_open(+, -, :).

/** <module> RocksDB interface

RocksDB is an embeddable persistent key-value   store  for fast storage.
The store can be used only from one process  at the same time. It may be
used from multiple Prolog  threads  though.   This  library  provides  a
SWI-Prolog binding for RocksDB.  RocksDB   just  associates byte arrays.
This interface defines several mappings   between  Prolog datastructures
and byte arrays that may be configured   to  store both keys and values.
See rocks_open/3 for details.

@see http://rocksdb.org/
*/

%%	rocks_open(+Directory, -RocksDB, +Options) is det.
%
%	Open a RocksDB database in Directory   and  unify RocksDB with a
%	handle to the opened database.  Defined options are:
%
%	  - alias(+Name)
%	  Give the database a name instead of using an anonymous
%	  handle.  A named database is not subject to GC and must
%	  be closed explicitly.
%	  - open(+How)
%	  If How is `once` and an alias is given, a second open simply
%	  returns a handle to the already open database.
%	  - key(+Type)
%	  - value(+Type)
%	  Define the type for the key and value. This must be
%	  consistent over multiple invocations.  Defined types are:
%	    - atom
%	      Accepts an atom or string.  Unifies the result with an
%	      atom.  Data is stored as a UTF-8 string in RocksDB.
%	    - string
%	      Accepts an atom or string.  Unifies the result with a
%	      string.  Data is stored as a UTF-8 string in RocksDB.
%	    - binary
%	      Accepts an atom or string with codes in the range 0..255.
%	      Unifies the result with a string. Data is stored as a
%	      sequence of bytes in RocksDB.
%	    - int32
%	      Maps to a Prolog integer in the range
%	      -2,147,483,648...2,147,483,647.  Stored as a 4 bytes in
%	      native byte order.
%	    - int64
%	      Maps to a Prolog integer in the range
%	      -9223372036854775808..9223372036854775807 Stored as a 8
%	      bytes in native byte order.
%	    - float
%	      Value is mapped to a 32-bit floating point number.
%	    - double
%	      Value is mapped to a 64-bit floating point number (double).
%	    - term
%	      Stores any Prolog term. Stored using PL_record_external().
%	      The PL_record_external() function serializes the internal
%	      structure of a term, including _cycles_, _sharing_ and
%	      _attributes_.  This means that if the key is a term, it
%	      only matches if the the same cycles and sharing is
%	      used. For example, `X = f(a), Key = k(X,X)` is a different
%	      key from `Key = k(f(a),f(a))` and `X = [a|X]` is a
%	      different key from `X = [a,a|X]`. Applications for which
%	      such keys should match must first normalize the key.
%	      Normalization can be based on term_factorized/3 from
%	      library(terms).
%	  - merge(:Goal)
%	  Define RocksDB value merging.  See rocks_merge/3.
%	  - mode(+Mode)
%	  One of `read_write` (default) or `read_only`.  The latter
%	  uses OpenForReadOnly() to open the database.

rocks_open(Dir, DB, Options0) :-
	meta_options(is_meta, Options0, Options),
	rocks_open_(Dir, DB, Options).

is_meta(merge).


%%	rocks_close(+RocksDB) is det.
%
%	Destroy the RocksDB handle.  Note   that  anonymous  handles are
%	subject to (atom) garbage collection.

%%	rocks_put(+RocksDB, +Key, +Value) is det.
%
%	Add Key-Value to the RocksDB  database.   If  Key  already has a
%	value, the existing value is silently replaced by Value.

%%	rocks_merge(+RocksDB, +Key, +Value) is det.
%
%	Merge Value with the already existing   value  for Key. Requires
%	the option merge(:Merger) to be used  when opening the database.
%	Using  rocks_merge/3  rather  than    rocks_get/2,   update  and
%	rocks_put/3  makes  the  operation  _atomic_  and  reduces  disk
%	accesses.
%
%	`Merger` is called as below, where two clauses are required:
%	one with `How` set to `partial` and one with `How` set to
%	`full`.  If `full`, `MergeValue` is a list of values that need
%	to be merged, if `partial`, `MergeValue` is a single value.
%
%	    call(:Merger, +How, +Key, +Value0, +MergeValue, -Value)
%
%	If Key is not in RocksDB, `Value0`  is unified with a value that
%	depends on the value type. If the value   type is an atom, it is
%	unified with the empty atom; if it is `string` or `binary` it is
%	unified with an empty string; if it  is `int32` or `int64` it is
%	unified with the integer 0; and finally if the type is `term` it
%	is unified with the empty list.
%
%	For example, if the value is a set  of Prolog values we open the
%	database with value(term) to allow for Prolog lists as value and
%	we define merge_set/5 as below.
%
%	==
%	merge(partial, _Key, Left, Right, Result) :-
%	    ord_union(Left, Right, Result).
%	merge(full, _Key, Initial, Additions, Result) :-
%	    append([Initial|Additions], List),
%	    sort(List, Result).
%	==
%
%	If the merge callback fails  or   raises  an exception the merge
%	operation fails and the error  is   logged  through  the RocksDB
%	logging facilities. Note that the merge   callback can be called
%	in a different thread or even in   a temporary created thread if
%	RocksDB decides to merge remaining values in the background.
%
%	@error	permission_error(merge, rocksdb RocksDB) if the database
%		was not opened with the merge(Merger) option.
%
%	@see https://github.com/facebook/rocksdb/wiki/Merge-Operator for
%	understanding the concept of value merging in RocksDB.

%%	rocks_delete(+RocksDB, +Key) is semidet.
%
%	Delete Key from RocksDB. Fails if Key is not in the database.

%%	rocks_get(+RocksDB, +Key, -Value) is semidet.
%
%	True when Value is the current value associated with Key in
%	RocksDB.

%%	rocks_enum(+RocksDB, -Key, -Value) is nondet.
%
%	True when Value is the  current   value  associated  with Key in
%	RocksDB. This enumerates all keys in the database.

%%	rocks_batch(+RocksDB, +Actions:list) is det.
%
%	Perform  a  batch  of  operations  on  RocksDB  as  an  _atomic_
%	operation. Actions is a list of:
%
%	  - delete(+Key)
%	  As rocks_delete/2.
%	  - put(+Key, +Value)
%	  As rocks_put/3.
%
%	The  following  example  is   translated    from   the   RocksDB
%	documentation:
%
%	==
%	  rocks_get(RocksDB, key1, Value),
%	  rocks_batch(RocksDB,
%		      [ delete(key1),
%		        put(key2, Value)
%		      ])
%	==


%!  rocks_property(+RocksDB, ?Property) is nondet

rocks_property(RocksDB, Property) :-
    var(Property), !,
    rocks_property(P),
    rocks_property(RocksDB, P, Value),
    Property =.. [P,Value].
rocks_property(RocksDB, Property) :-
    Property =.. [P,Value], !,
    rocks_property(RocksDB, P, Value).
rocks_property(_RocksDB, Property) :-
    type_error(property, Property).

rocks_property(estimate_num_keys).
