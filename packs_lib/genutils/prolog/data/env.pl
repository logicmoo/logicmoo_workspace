/* Part of plumdrum
	Copyright 2012-2015 Samer Abdallah (Queen Mary University of London; UCL)
	 
	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(env,
		[	init_env//0
		,	key//1
		,	key_val//2
		,	ins_key//1
		,	ins_key//2
		,	get_key//2
		,	get_key//3
		,	set_key//2
		,	upd_key//3
		,	with_key//2
		,	del_key//1
		,	sel_key//2
		,	with_env/1
		,	ins_keys//1
		,	sel_keys//1
		]).

:- meta_predicate with_key(+,//,+,-), with_env(:).

/** <module> Environments for stateful computations

This module provides DCG compatible rules for managing a state
variable which consistists of an environment, which contains
a set of key-value mappings. The predicates do a number of checks
to ensure safe and consistant use.
*/

:- use_module(library(dcg_core)).
:- use_module(library(rbtrees)).

user:goal_expansion(no_fail(K,S,G), (G -> true; check(K,S), throw(failed(G)))).
user:portray(t(_,_)) :- write('<rbtree/2>').

%% init_env// is det.
%  Set state to empty environment.
init_env --> set_with(rb_empty).

%% key(?Key, ?Val)// is nondet.
%  Enumerate all keys in environment.
key(K,S,S) :- rb_in(K,_,S).

%% key_val(?Key, ?Val)// is nondet.
%  Enumerate all keys and associated values.
key_val(K,V,S,S) :- rb_in(K,V,S).

%% get_key(+Key, +Default, ?Val)// is det.
%  Unify Val with value associated with Key or Default if not present.
get_key(K,V,D,S,S) :- (rb_lookup(K,V,S) -> true; V=D).

%% get_key(+Key, ?Val)// is det.
%  Unify Val with value associated with Key.
get_key(K,V,S,S)       :- no_fail(K, S, rb_lookup(K,V,S)).

%% set_key(+Key, ?Val)// is det.
%  Set value associated with Key to Val.
set_key(K,V,S1,S2)     :- no_fail(K, S1, rb_update(S1,K,V,S2)).

%% upd_key(+Key, ?Val1, ?Val2)// is det.
%  Unify Val1 with value associated with Key and set new value to Val2.
upd_key(K,V1,V2,S1,S2) :- no_fail(K, S1, rb_update(S1,K,V1,V2,S2)).

%% del_key(+Key)// is det.
%  Remove Key from environment.
del_key(K,S1,S2)       :- no_fail(K, S1, rb_delete(S1,K,S2)).

%% sel_key(+Key, ?Val)// is det.
%  Remove Key from environment and unify Val with its value.
sel_key(K,V,S1,S2)     :- no_fail(K, S1, rb_delete(S1,K,V,S2)).

%% ins_key(+Key, ?Val)// is det.
%% ins_key(+Key)// is det.
%
%  Add Key to environment with given value or unbound if no value given.
ins_key(K,S1,S2) :- ins_key(K,_,S1,S2).
ins_key(K,V,S1,S2) :- 
	(	var(K) -> throw(instantiation_error('environment key'))
	;	rb_in(K,_,S1) -> throw(error(key_exists(K)))
	;	no_fail(K, S1, rb_insert_new(S1,K,V,S2))
	).

%% with_key(+Key, :Phrase)// is nondet.
%  Use Phrase to compute new value of key from old.
with_key(K,P,S1,S2)    :- check(K,S1), rb_apply(S1,K,call_dcg(P),S2).


%% with_env(:Phrase) is nondet.
%  Run phrase with initial state equal to an empty environment.
with_env(G) :- init_env(_,E), call_dcg(G,E,_).


check(K,S) :- 
	(	var(K) -> throw(instantiation_error('environment key'))
	;	rb_in(K,_,S) -> true
	;	throw(error(key_not_found(K)))
	).

prolog:message(error(key_not_found(K))) -->
	[ 'Key (~w) not found in current environment.'-[K], nl].

prolog:message(error(key_exists(K))) -->
	[ 'Key (~w) already present in current environment.'-[K], nl].
% alternative is to use assoc, but assoc does not allow keys to
% be removed.
%key_set(K,V,S1,S2) :- put_assoc(K,S1,V,S2).
%with_key(K,P,S1,S2) :- get_assoc(K,S1,V1), call_dcg(P,V1,V2), put_assoc(K,S1,V2,S2).


ins_keys([]) --> !.
ins_keys([(K,V)|KX]) --> ins_key(K,V), ins_keys(KX).

sel_keys([]) --> !.
sel_keys([(K,V)|KX]) --> sel_key(K,V), sel_keys(KX).

