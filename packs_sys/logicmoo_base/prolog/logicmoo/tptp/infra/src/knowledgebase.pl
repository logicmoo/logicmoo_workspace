/*
* Copyright (C) 2002, 2007 Christoph Wernhard
* 
* This program is free software; you can redistribute it and/or modify it
* under the terms of the GNU General Public License as published by the Free
* Software Foundation; either version 2 of the License, or (at your option)
* any later version.
* 
* This program is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
* more details.
* 
* You should have received a copy of the GNU General Public License along with
* this program; if not, see <http://www.gnu.org/licenses/>.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Knowledgebase
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(knowledgebase, [init_knowledgebase/0,
                          fact/4,
			  informative_fact/4,

                          make_kb/4,
			  make_meta_kb/3,
			  kb_id/2,
			  kb_user/2,
			  kb_groups/2,
			  kb_is_meta/1,

			  make_permissions/7,
			  
			  create_knowledgebase/2,
			  create_knowledgebase_like/4,
			  delete_knowledgebase/1,
			  extend_knowledgebase/3,
			  reload_knowledgebase/2,
			  
			  add_documents/3,
			  add_namespace/3,
			  add_facts/4,
			  match_namespace/5,

			  add_to_library/2,

			  has_read_permission/1,
			  has_write_permission/1,

			  file_out/1,
			  file_in/1,

			  catalog_fact/3]).

:- use_module('swilib/err').

:- use_module(webget).
:- use_module(rdf_convert).
:- use_module(indexing).

%%%% 
%%%% Note: KB is used in two senses:
%%%% 
%%%% 1. As supplied by users, a term, indicating the KB implementation
%%%%    and information about the accessing user. In most cases KB is used
%%%%    as parameter name for that.
%%%% 
%%%% 2. As used internally, an atom (or URI). This is extracted by kb_id/2
%%%%    from the term form. The parameter name is usually K.
%%%% 
%%%% A special case of (1.) is is meta_kb, which has no corresponding
%%%% internal KB.
%%%%

%%%% 
%%%% blank(Id), where Id starts with 'k' are reserved.
%%%% 

%%%% 
%%%% *** TODO - Security: The catalog (sys_documents) and
%%%% sys_known_documents is world readable, i.e. all users can see with
%%%% which document other users work. Should be changed.
%%%% 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% The Catalog Knowledgebase
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% General facts
%%%% 
base_fact(sys_catalog, S, P, O) :-
	base_fact(S, P, O).

%%%% 
%%%% Class sys_Knowledgebase 
%%%%
base_fact(sys_catalog, sys_Knowledgebase, rdf_type, rdfs_Class).
base_fact(sys_catalog, sys_cacheValid, rdfs_domain, sys_Knowledgebase).
base_fact(sys_catalog, sys_owner, rdfs_domain, sys_Knowledgebase).
base_fact(sys_catalog, sys_group, rdfs_domain, sys_Knowledgebase).
base_fact(sys_catalog, sys_owner, rdfs_range, sys_User).
base_fact(sys_catalog, sys_group, rdfs_range, sys_Group).
base_fact(sys_catalog, sys_permissions, rdfs_domain, sys_Knowledgebase).
base_fact(sys_catalog, sys_document, rdfs_domain, sys_Knowledgebase).
base_fact(sys_catalog, sys_creationTime, rdfs_domain, sys_Knowledgebase).
base_fact(sys_catalog, sys_modificationTime, rdfs_domain, sys_Knowledgebase).
base_fact(sys_catalog, sys_genBlankNext, rdfs_domain, sys_Knowledgebase).
base_fact(sys_catalog, sys_transient, rdfs_domain, rdfs_Property).
base_fact(sys_catalog, sys_cacheValid, sys_transient, sys_true).

%%%% 
%%%% The sys_catalog Knowledgebase
%%%% 
base_fact(sys_catalog, sys_catalog, rdf_type, sys_Knowledgebase).

base_fact(sys_catalog, sys_catalog, rdfs_comment,
	  literal('The catalog meta-information knowledgebase of the system.')).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Dummy Knowledgebase
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

base_fact(sys_catalog, sys_meta, rdf_type, sys_Knowledgebase).
base_fact(sys_catalog, sys_meta, rdfs_comment, literal('For internal system use.')).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% The Library Knowledgebase
%%%% 
%%%% Automatically maintained meta information about documents (for use
%%%% as history, bookmarks, suggestions, link-lists).
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

base_fact(sys_catalog, sys_library, rdf_type, sys_Knowledgebase).

base_fact(sys_catalog, sys_library, rdfs_comment,
          literal('Automatically maintained meta-information about documents.')).
base_fact(sys_library, S, P, O) :-
	base_fact(S, P, O).

base_fact(sys_library, sys_RdfDocument, rdfs_subClassOf, sys_Document).
base_fact(sys_library, sys_HtmlDocument, rdfs_subClassOf, sys_Document).
base_fact(sys_library, sys_accessed, rdf_domain, sys_Document).

add_to_library(Uri, DocClass) :-
	current_time(Time),
	invalidate_cache(sys_library),
	retractall_store_1(sys_library, Uri, sys_accessed, _),
	store_assert(sys_library, Uri, sys_accessed, literal(Time)),
	store_assert(sys_library, Uri, rdf_type, DocClass).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Datatypes
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_kb(Id, User, Groups, kb(Id, User, Groups)).
make_meta_kb(User, Groups, meta_kb(User, Groups)).

kb_id(kb(Id, _, _), Id) :-
	!.
kb_id(meta_kb(_, _), sys_meta).
%	msg('Attempt to access ID of a meta KB.').

kb_user(kb(_, User, _), User).
kb_user(meta_kb(User, _), User).

kb_groups(kb(_, _, Groups), Groups).
kb_groups(meta_kb(_, Groups), Groups).

kb_is_meta(meta_kb(_, _)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Reading Access
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic( extends/2 ).

fact(KB, S, P, O) :-
	check_read_permission(KB),
	kb_id(KB, K),
	fact_1(K, S, P, O).

%%%% - This is here as simple approximation to an
%%%%   "anti closure" procedure, which removes
%%%%   facts that could be inferred via closure
%%%%
informative_fact(KB, S, P, O) :-
	check_read_permission(KB),
	kb_id(KB, K),
	store_1(K, S, P, O).

store_1(K, S, P, O) :-
	store(K, S, P, O).
store_1(K, S, P, O) :-
	extends(K, K1),
	store_1(K1, S, P, O).

catalog_fact(S, P, O) :-
	fact_1(sys_catalog, S, P, O).

once_catalog_fact(S, P, O) :-
	fact_1(sys_catalog, S, P, O),
	!.

fact_1(K, S, P, O) :-
	store(K, S, P, O).
fact_1(K, S, P, O) :-
	ensure_cache(K),
	cache(K, S, P, O).
fact_1(K, S, P, O) :-
	extends(K, K1),
	fact_1(K1, S, P, O).

%%%% 
%%%% Version of fact called during cache computation.
%%%% 
proto_fact(K, S, P, O) :-
	cache(K, S, P, O).
proto_fact(K, S, P, O) :-
	store(K, S, P, O).
proto_fact(K, S, P, O) :-
	extends(K, K1),
	proto_fact(K1, S, P, O).

ensure_cache(K) :-
	cache_is_valid(K),
	!.
ensure_cache(K) :-
	msg('Kb: Start computing cache for ~q.' , [K]),
	compute_cache(K),
	msg('Kb: Finished computing cache for ~q.' , [K]),
	cache_set_valid(K).

%%
%% To be "complete", we should invalidate the sys_catalog KB,
%% each time we modify sys_cacheValid. We do not do this, ASSUMING
%% that this is not necessary.
%%
cache_set_valid(K) :-
	assert_store(sys_catalog, K, sys_cacheValid, sys_true).
cache_set_invalid(K) :-
	retractall_store_1(sys_catalog, K, sys_cacheValid, _).
cache_is_valid(K) :-
	store(sys_catalog, K, sys_cacheValid, sys_true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Writing Access
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


store_assert(K, S, P, O) :-
	proto_fact(K, S, P, O),
	!.
store_assert(K, S, P, O) :-
	assert_store(K, S, P, O).

invalidate_cache(K) :-
	cache_set_invalid(K),
	( extends(K1, K),
	  invalidate_cache(K1),
	  fail
	; true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Compute Cache
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compute_cache(K) :-
	extends(K, _),
	!,
	err('Cannot recompute cache for extending knowledgebase ~q.', [K]).
compute_cache(K) :-
	retractall_cache_1(K, _, _, _),
	cache_mt_facts(K),
	cache_mt_explicit(K),
	get_time(T1),
	cache_mt_closure(K),
	cache_directs(K),
	get_time(T2),
	TD is T2 - T1,
	msg('Kb: Computed cache ~q in time: ~q.', [K, TD]),
	cache_namespaces(K).

%%%% 
%%%% Assert if not already there.
%%%% 
cache_assert(K, S, P, O) :-
	proto_fact(K, S, P, O),
	!.
cache_assert(K, S, P, O) :-
	assert_cache(K, S, P, O). 

%%%% 
%%%% MT Facts
%%%% 
cache_mt_facts(K) :-
	( mt_fact(S, P, O),
	  cache_assert(K, S, P, O),
	  fail
        ; true
	).

%%%% 
%%%% MT Extra
%%%%
%%%% Stuff that can not be handled by the rule mechanism.
%%%% To be called before the rule closure is computed.
%%%%
cache_mt_explicit(K) :-
	( store(K, _, P, _),
	  rdf_container_num_property(P),
	  cache_assert(K, P, rdfs_subPropertyOf, rdfs_member),
	  fail
	; true
	).

rdf_container_num_property(P) :-
	sub_atom(P, 0, 5, _, 'rdf__').

cache_directs(K) :-
	( tp_direct(K, rdfs_subClassOf, X, Y),
	  assert_cache(K, X, sys_directSubClassOf, Y),
	  fail
	; true
	),
	( tp_direct(K, rdfs_subPropertyOf, X, Y),
	  assert_cache(K, X, sys_directSubPropertyOf, Y),
	  fail
	; true
	).


tp_direct(K, TP, X, X1) :-
	proto_fact(K, X, TP, X1),
	X \= X1,
	\+ ( proto_fact(K, X, TP, X2),
             X2 \= X,
	     X2 \= X1,
	     proto_fact(K, X2, TP, X1),
	     \+ proto_fact(K, X2, TP, X),
	     \+ proto_fact(K, X1, TP, X2) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Model Theory - RDFS base facts
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mt_fact(rdfs_Resource, rdf_type, rdfs_Class).
mt_fact(rdfs_Literal, rdf_type, rdfs_Class).
mt_fact(rdfs_Class, rdf_type, rdfs_Class).
mt_fact(rdf_Property, rdf_type, rdfs_Class).

mt_fact(rdf_type, rdf_type, rdf_Property).
mt_fact(rdf_type, rdfs_domain, rdfs_Resource).
mt_fact(rdf_type, rdfs_range, rdfs_Class).

mt_fact(rdfs_domain, rdf_type, rdf_Property).
mt_fact(rdfs_domain, rdfs_domain, rdf_Property).
mt_fact(rdfs_domain, rdfs_range, rdfs_Class).

mt_fact(rdfs_range, rdf_type, rdf_Property).
mt_fact(rdfs_range, rdfs_domain, rdf_Property).
mt_fact(rdfs_range, rdfs_range, rdfs_Class).

mt_fact(rdfs_subPropertyOf, rdf_type, rdf_Property).
mt_fact(rdfs_subPropertyOf, rdfs_domain, rdf_Property).
mt_fact(rdfs_subPropertyOf, rdfs_range, rdf_Property).

mt_fact(rdfs_subClassOf, rdf_type, rdf_Property).
mt_fact(rdfs_subClassOf, rdfs_domain, rdfs_Class).
mt_fact(rdfs_subClassOf, rdfs_range, rdfs_Class).

%%
%% This is not actually in the MT (April 2002), but I think it should be.
%% It corresponds to the RDFS spec.
%%
mt_fact(rdf_Bag, rdfs_subClassOf, rdfs_Container).
mt_fact(rdf_Alt, rdfs_subClassOf, rdfs_Container).
mt_fact(rdf_Set, rdfs_subClassOf, rdfs_Container).
mt_fact(rdfs_member, rdfs_domain, rdfs_Container).

%% 
%% Some fringe stuff not included in the MT (April 2002).
%% 
mt_fact(rdfs_comment, rdfs_range, rdfs_Literal).
mt_fact(rdfs_label, rdfs_range, rdfs_Literal).

% Added Oct 2007: RDF Semantics 10. Feb 2004
% Rudimentary support for rdfs_Datatype

mt_fact(rdfs_Datatype, rdfs_subClassOf, rdfs_Class).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Closure Rules
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op(1200, xfx, :--).

%%%%
%%%% Model Theory - RDFS Closure Rules
%%%%
mt_rules([

( fact(P, rdf_type, rdf_Property) :-- fact(_, P, _) ),
( fact(S, rdf_type, C) :-- fact(P, rdfs_domain, C), fact(S, P, _) ),
( fact(O, rdf_type, C) :-- fact(P, rdfs_range, C), fact(_, P, O) ),
( fact(S, rdf_type, rdfs_Resource) :--	fact(S, _, _) ),
( fact(O, rdf_type, rdfs_Resource) :-- fact(_, _, O) ),
%% Literals O are excluded by patch in processing engine,
( fact(P1, rdfs_subPropertyOf, P2) :--
          fact(P1, rdfs_subPropertyOf, P3),
          fact(P3, rdfs_subPropertyOf, P2) ),
( fact(S, P, O) :-- fact(P1, rdfs_subPropertyOf, P), fact(S, P1, O) ),
( fact(C, rdfs_subClassOf, rdfs_Resource) :-- fact(C, rdf_type, rdfs_Class) ),
( fact(C1, rdfs_subClassOf, C2) :--
          fact(C1, rdfs_subClassOf, C3),
          fact(C3, rdfs_subClassOf, C2) ),
( fact(S, rdf_type, C) :--
          fact(S, rdf_type, C1),
          fact(C1, rdfs_subClassOf, C) ),
	  
% Added Oct 2007: RDF Semantics 10. Feb 2004 rdfs13
% Rudimentary support for rdfs_Datatype	  
( fact(S, rdfs_subClassOf, rdfs_Literal) :--
          fact(S, rdf_type, rdfs_Datatype))
]).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Seminaive Rule Engine
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%
%%%% Only rules with a body of maximal two literals are supported.
%%%%

:- dynamic(new/3).
:- dynamic(delta/3).

% :- index( delta(1,1,1) ).

%%%% 
%%%% cache_mt_closure(K)
%%%% 
%%%% Add the closure of model theory rules to the cache/4 predicate.
%%%%
cache_mt_closure(K) :-
	retractall( new(_, _, _) ),
	mt_rules(Rules),
	eval(K, Rules),
	procnews(K),
	seminaive_loop(K, Rules).

%%%% 
%%%% Closure for rules, starting with new delta facts
%%%% 
extend_kb(K, Facts, Rules) :-
	%% assume store and cache of K are fresh or cleared
	retractall( new(_, _, _) ),
	retractall( delta(_, _, _) ),
	( member( rdf(S, P, O), Facts ),
	  ( proto_fact(K, S, P, O) ->
	    true
	  ; assert_store(K, S, P, O),
	    assert( delta(S, P, O) ),
	    fail
	  )
	; true
	),
	( delta(_, _, _) ->
	  seminaive_loop(K, Rules)
	; true
	).

seminaive_loop(K, Rules) :-
	eval_incr(K, Rules),
	procnews(K),
	delta(_, _, _),
	!,
	seminaive_loop(K, Rules).
seminaive_loop(_, _).

einfo(X) :- write(user_error, X), flush_output(user_error).

procnews(K) :-
	( predicate_property(new(_,_,_), number_of_clauses(NNew)) ->
	  msg('Kb: ~w new', [NNew])  
        ; true
	),
	retractall( delta(_,_,_) ),
	( retract( new(S, P, O) ),
	  ( proto_fact(K, S, P, O) ->
	    true
          ; assert_cache(K, S, P, O),
	    assert( delta(S, P, O) )
	  ),
	  fail
        ; true
	),
	( predicate_property(delta(_,_,_), number_of_clauses(NDelta)) ->
	  msg('Kb: ~w delta', [NDelta])  
        ; true
	).

eval(K, Rules) :-
	( member(R, Rules),
	  einfo('.'),
	  eval_rule(R, K),
	  fail
        ; true
	).

eval_incr(K, Rules) :-
	( member(R, Rules),
	  einfo('.'),
	  eval_rule_incr(R, K),
	  fail
        ; true
	).

eval_rule((fact(S,P,O) :-- fact(S1,P1,O1)), K) :-
	!,
	proto_fact(K, S1, P1, O1),
	S \= literal(_), %% *** Necessary Patch
	assert( new(S, P, O) ).
eval_rule((fact(S,P,O) :-- fact(S1,P1,O1), fact(S2,P2,O2)), K) :-
	proto_fact(K, S1, P1, O1),
	proto_fact(K, S2, P2, O2),
	S \= literal(_), %% *** "Just for safety"
	assert( new(S, P, O) ).

eval_rule_incr((fact(S,P,O) :-- fact(S1,P1,O1)), _) :-
	!,
	delta(S1, P1, O1),
	S \= literal(_), %% *** Necessary Patch
	assert( new(S, P, O) ).
eval_rule_incr((fact(S,P,O) :-- fact(S1,P1,O1), fact(S2,P2,O2)), K) :-
	delta(S1, P1, O1),
	proto_fact(K, S2, P2, O2),
	S \= literal(_),%% *** "Just for safety"
	assert( new(S, P, O) ).
eval_rule_incr((fact(S,P,O) :-- fact(S1,P1,O1), fact(S2,P2,O2)), K) :-
	proto_fact(K, S1, P1, O1),
	delta(S2, P2, O2),
	S \= literal(_),%% *** "Just for safety"
	assert( new(S, P, O) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Permission Checks
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_permissions(UR, UW, GR, GW, WR, WW, Permissions) :-
	(UR = y -> P1 = 400 ; P1 = 0),
	(UW = y -> P2 = 200 ; P2 = 0),
	(GR = y -> P3 = 40 ; P3 = 0),
	(GW = y -> P4 = 20 ; P4 = 0),
	(WR = y -> P5 = 4 ; P5 = 0),
	(WW = y -> P6 = 2 ; P6 = 0),
	Permissions1 is P1+P2+P3+P4+P5+P6,
	term_to_atom(Permissions1, Permissions).

system_maintained_kb(KB) :- 
	kb_id(KB, sys_catalog),
	!.
system_maintained_kb(KB) :- 
	kb_id(KB, sys_library),
	!.
system_maintained_kb(KB) :- 
	kb_id(KB, sys_meta),
	!.

has_read_permission(KB) :-
	system_maintained_kb(KB),
	!.
has_read_permission(KB) :-
	kb_user(KB, User),
	kb_groups(KB, Groups),
	kb_id(KB, K),
	once_catalog_fact(K, sys_owner, Owner),
	once_catalog_fact(K, sys_group, Group),
	once_catalog_fact(K, sys_permissions, literal(Permissions)),
	term_to_atom(Permissions1, Permissions),
	( User = Owner ->
	  Permissions1 /\ 400 > 0  
	; memberchk(Group, Groups) ->  
	  Permissions1 /\ 40 > 0  
        ; Permissions1 /\ 4 > 0  
	),
	!.

check_read_permission(KB) :-
	has_read_permission(KB),
	!.
check_read_permission(KB) :-
	kb_user(KB, User),
	kb_id(KB, K),
	err('No read permission for user ~q on knowledgebase ~q.', [User, K]).

has_write_permission(KB) :-
	system_maintained_kb(KB),
	!,
	fail.
has_write_permission(KB) :-
	kb_user(KB, User),
	kb_groups(KB, Groups),
	kb_id(KB, K),
	once_catalog_fact(K, sys_owner, Owner),
	once_catalog_fact(K, sys_group, Group),
	once_catalog_fact(K, sys_permissions, literal(Permissions)),
	term_to_atom(Permissions1, Permissions),
	( User = Owner ->
	  Permissions1 /\ 200 > 0  
	; memberchk(Group, Groups) ->  
	  Permissions1 /\ 20 > 0  
        ; Permissions1 /\ 2 > 0  
	),
	!.

check_write_permission(KB) :-
	has_write_permission(KB),
	!.
check_write_permission(KB) :-
	kb_user(KB, User),
	kb_id(KB, K),
	err('No write permission for user ~q on knowledgebase ~q.', [User, K]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Namespace Stuff
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic( namespace_cache/3 ).

%%%%
%%%% match_namespace(+KB, +Resource, -Namespace, -NamespaceAbbrev, -Name).
%%%% 
%%%% Fails if no namespace is found. Succeeds at most once.
%%%% 
match_namespace(KB, Resource, N, A, Name) :-
	check_read_permission(KB),
	kb_id(KB, K),
	ensure_cache(K),
	namespace_cache(K, N, A),
	sub_atom(Resource, 0, S, L, N),
	L > 0,
	sub_atom(Resource, S, L, _, Name).

%%%% 
%%%% We need to access namespaces sorted, so they are cached specially.
%%%%
cache_namespaces(K) :-
	retractall( namespace_cache(K, _, _) ),
	findall(N-A, proto_fact(K, N, sys_abbreviation, literal(A)), NAs),
	sort(NAs, NAs1),
	( member(N1-A1, NAs1),
	  %% asserta - if a prefix is the prefix of another one,
	  %% the longer should come earlier.
	  asserta( namespace_cache(K, N1, A1) ),
	  fail
	; true
	),
	( pseudo_namespace(Prefix, _, Abbrev),
	  asserta( namespace_cache(K, Prefix, Abbrev) ),
	  fail
	; true
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Base Stuff
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

base_fact(sys_abbreviation, rdfs_domain, sys_Namespace).
base_fact(Namespace, sys_abbreviation, literal(Abbrev)) :-
	pseudo_namespace(_, Namespace, Abbrev).

base_fact(sys_Namespace, rdfs_comment, literal('URI considered as representation of a namespace (prefix of other URIs).')).
base_fact(sys_abbreviation, rdfs_comment, literal('Symbol used as short representation of a namespace.')).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Create Knowledgebase
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_knowledgebase(KB, _) :-
	kb_id(KB, K),
	once_catalog_fact(K, _, _),
	!,
	err('Knowledgebase ~q already exists.', [K]).
create_knowledgebase(KB, Args) :-
	kb_id(KB, K),             %% the id is always canonicalized
	kb_user(KB, User),
	kb_groups(KB, Groups),
	getarg(owner=Owner, Args),
	getarg(group=Group, Args),
	canonicalize_item(Owner, Owner1),
	canonicalize_item(Group, Group1),
	getarg(permissions=Permissions, Args),
	( User \= Owner1 ->
	  err('No permission to create kb with foreign user id.')
	; \+ memberchk(Group1, Groups) ->
	  err('No permission to create kb in foreign group.')
	; true
	),
	create_store(K),
	create_cache(K),
	invalidate_cache(K),
	invalidate_cache(sys_catalog),
	store_assert(sys_catalog, K, rdf_type, sys_Knowledgebase), %% existence
	store_assert(sys_catalog, K, sys_owner, Owner1),
	store_assert(sys_catalog, K, sys_group, Group1),
	store_assert(sys_catalog, K, sys_permissions, literal(Permissions)),
	( memberchk(comment=Comment, Args) ->
	  store_assert(sys_catalog, K, rdfs_comment, literal(Comment))
	; true
	),
	store_assert(sys_catalog, K, sys_genBlankNext, literal('0')),
	current_time(Time),
	store_assert(sys_catalog, K, sys_creationTime, literal(Time)),
	store_assert(sys_catalog, K, sys_modificationTime, literal(Time)),
	initialize_kb(K, Args).

reinitialize_kb(K) :-
	initialize_kb(K, []).

initialize_kb(K, Args) :-
	( memberchk(extends=KBase, Args) ->
	  assert( extends(K, KBase) )
	; ( base_fact(S, P, O),
	    store_assert(K, S, P, O),
	    fail
	  ; true
	  )
	).
	
getarg(KV, KVs) :-
	memberchk(KV, KVs),
	!.
getarg(K=_, _) :-
	err('Missing argument: ~q.', [K]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Extend Knowledgebase
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% extend_knowledgebase(+KB, +Facts, KB1)
%%%% 
%%%% Create a new kb, which includes facts from the given one and
%%%% additional facts supplied. The new kb is returned as KB1.
%%%% It is closed according to the RDF Model Theory.
%%%%
%%%% This mechanism is intended for evaluating certain kinds of queries, where
%%%% the result KB1 is only temporarily used. KB1 can be deleted by
%%%% delete_knowledgebase/1 after use.
%%%%
%%%% - If KB is invalidated, KB1 can no longer be used.
%%%% - match_namespace may fail for KB1
%%%% - add_documents, add_namespace are not permitted for KB1
%%%% - other stuff, such as sys_direct... properties are not available for KB1
%%%% - blanks in Facts are NOT made local (i.e. localize_blanks is not called).
%%%%   Facts may use only blanks with ids not starting with 'k' to  avoid
%%%%   clashing with system generated local blanks.
%%%%
extend_knowledgebase(KB, Facts, KB1) :-
	kb_id(KB, K),
	gensym('sys_extending', K1),
	create_knowledgebase_like(KB, K1, [extends=K], KB1),
	ensure_cache(K),
	cache_mt_explicit(K),
	mt_rules(Rules),
	extend_kb(K1, Facts, Rules),
	cache_set_valid(K1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Create Knowledgebase Like
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Create a knowledgebase with group/owner and permission attributes
%%%% defaulted from a given knowledgebase.
%%%% Also namespaces are copied from the first knowledgebase.
%%%%
create_knowledgebase_like(KB, ID, Options, KB1) :-
	kb_id(KB, K),
	kb_user(KB, User),
	kb_groups(KB, Groups),
	make_kb(ID, User, Groups, KB1),
	once_catalog_fact(K, sys_owner, Owner),
	once_catalog_fact(K, sys_group, Group),
	once_catalog_fact(K, sys_permissions, literal(Permissions)),
	append(Options, [ group=Group,
			  owner=Owner,
			  permissions=Permissions],
	       Options1),
	create_knowledgebase(KB1, Options1),
	( fact(KB, N, sys_abbreviation, literal(A)),
	  store_namespace(ID, N, A),
	  fail
	; true
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Store and Cache Representation
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(store/4).
:- dynamic(assert_store/4).
:- dynamic(retractall_store/4).
:- dynamic(cache/4).
:- dynamic(assert_cache/4).
:- dynamic(retractall_cache/4).

:- dynamic(auxpredicate/2).

print_auxinfo :-
	listing(cache/4),
	listing(store/4),
	( auxpredicate(K, F/A),
	  functor(Template, F, A),
	  format('============= ~q ~q ============~n', [K, F/A]),
	  ( predicate_property(Template, P),
	    P = number_of_clauses(_),
	    writeq(P),
	    nl,
	    fail
	  ; true
	  ),
	  fail
	; true
	).
     

retractall_store_1(A, B, C, D) :-
	retractall_store(A, B, C, D).
retractall_store_1(_, _, _, _).

retractall_cache_1(A, B, C, D) :-
	retractall_cache(A, B, C, D).
retractall_cache_1(_, _, _, _).

cache_patterns(K,
        [ cache(K, _, _, _),
          cache(K, _, rdf_type, _),
          cache(K, _, rdf_type, rdfs_Resource),
          cache(K, _, rdf_type, rdfs_Class),
          cache(K, _, rdf_type, rdf_Property),
	  cache(K, _, rdfs_subClassOf, rdfs_Resource),
	  cache(K, _, rdfs_subClassOf, _),
	  cache(K, _, rdfs_subPropertyOf, _),
	  cache(K, _, sys_directSubClassOf, _)-[first(1)],
	  cache(K, _, sys_directSubPropertyOf, _)-[first(1)]
	  ] ).

store_patterns(K,
        [ store(K, _, _, _),
          store(K, _, rdf_type, _),
          store(K, _, rdf_type, rdfs_Resource),
          store(K, _, rdf_type, rdfs_Class),
          store(K, _, rdf_type, rdf_Property),
	  store(K, _, rdfs_subClassOf, rdfs_Resource),
	  store(K, _, rdfs_subClassOf, _),
	  store(K, _, rdfs_subPropertyOf, _),
	  %% The following two appear here to block vain searches in store.
	  store(K, _, sys_directSubClassOf, _)-[first(1)],
	  store(K, _, sys_directSubPropertyOf, _)-[first(1)]
	]).  

create_cache(K) :-
	cache_patterns(K, Patterns),
	install_auxpredicates(K, Patterns).

create_store(K) :-
	store_patterns(K, Patterns),
	install_auxpredicates(K, Patterns).

install_auxpredicates(K, Patterns) :-
	specialized_clauses(Patterns, AuxPreds, Clauses),
	( member(AP, AuxPreds),
	  dynamic(AP),
	  assert( auxpredicate(K, AP) ),
	  fail
	; true
	),
	( member(C, Clauses),
	  assert(C),
	  fail
	; true
	).

init_knowledgebase :-
	remove_knowledgebase(_),

	create_store(sys_catalog),
	create_cache(sys_catalog),

	create_store(sys_library),
	create_cache(sys_library),

	( base_fact(A, B, C, D),
          assert_store(A, B, C, D),
          fail
	; true
	).

%%%% 
%%%% K may be a variable here.
%%%% 
remove_knowledgebase(K) :-
	retractall(namespace_cache(K, _, _) ),
	retractall(store(K, _, _, _)),
	retractall(cache(K, _, _, _)),
	retractall(assert_store(K, _, _, _)),
	retractall(assert_cache(K, _, _, _)),
	retractall(retractall_store(K, _, _, _)),
	retractall(retractall_cache(K, _, _, _)),
	( auxpredicate(K, P/N),
	  functor(Template, P, N),
	  retractall( Template ),
	  fail
	; true
	),
	( extends(K1, K),
	  remove_knowledgebase(K1),
	  fail
	; true
	),
	retractall( extends(K, _) ),
	retractall_store_1(sys_catalog, K, _, _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Delete Knowledgebase
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delete_knowledgebase(KB) :-
	check_write_permission(KB),
	kb_id(KB, K),
	remove_knowledgebase(K).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Reload Knowledgebase
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reload_knowledgebase(KB, PrettyInfo) :-
	check_write_permission(KB),
	kb_id(KB, K),
	findall( Doc, catalog_fact(K, sys_document, Doc), Docs ),
	sort(Docs, Docs1),
	retrieve_documents(Docs1, TripleSets, NamespaceSets),
	map_pretty_load_info(Docs1, TripleSets, PrettyInfo),
	invalidate_cache(K),
	invalidate_cache(sys_catalog),
	retractall_store_1(K, _, _, _),
	retractall_store_1(sys_catalog, K, sys_document, _),
	reinitialize_kb(K),
	assert_triples(KB, Docs1, TripleSets, NamespaceSets).

map_pretty_load_info(Uris, Lists, PrettyInfo) :-
	mpl1(Uris, Lists, PI1),
	concat_atom(PI1, ', ', PrettyInfo).

mpl1([U|Us], [L|Ls], [PI|PIs]) :-
	length(L, N1),
	concat_atom([U, ': ', N1, ' Triples'], PI),
	mpl1(Us, Ls, PIs).
mpl1([], [], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Add Facts (*** experimental)
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_facts(KB, Uris, TripleSets, NamespaceSets) :-
	check_write_permission(KB),
	assert_triples(KB, Uris, TripleSets, NamespaceSets).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Add Documents
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_documents(_, [], 'No documents') :-
	!.
add_documents(KB, Uris, PrettyInfo) :-
	check_write_permission(KB),
	retrieve_documents(Uris, TripleSets, NamespaceSets),
	map_pretty_load_info(Uris, TripleSets, PrettyInfo),
	assert_triples(KB, Uris, TripleSets, NamespaceSets).

assert_triples(KB, Uris, TripleSets, NamespaceSets) :-
	kb_id(KB, K),
	invalidate_cache(K),
	invalidate_cache(sys_catalog),
	( member(Triples, TripleSets),
	  localize_blanks(K, Triples, Triples1),
          member( rdf(S, P, O), Triples1 ),
	  store_assert(K, S, P, O),
	  fail
        ; true
 	),
	( member(Uri, Uris),
	  store_assert(sys_catalog, K, sys_document, Uri),
	  add_to_library(Uri, sys_RdfDocument),
	  fail
	; true
	),
	( member(NAs, NamespaceSets),
	  member(N-A, NAs),
	  store_namespace(K, N, A),
	  fail
        ; true	
	),
	current_time(Time),
	retractall_store_1(sys_catalog, K, sys_modificationTime, _),
	store_assert(sys_catalog, K, sys_modificationTime, literal(Time)).

localize_blanks(K, Triples, Triples1) :-
	store(sys_catalog, K, sys_genBlankNext, literal(I)),
	term_to_atom(I1, I),
	fb_1(Triples, [], I1, I2, Triples1),
	term_to_atom(I2, I3),
	retractall_store_1(sys_catalog, K, sys_genBlankNext, _ ),
	assert_store(sys_catalog, K, sys_genBlankNext, literal(I3)).

fb_1([T|Ts], M, I, I1, [T1|Ts1]) :-
	fb_2(T, M, M1, I, I2, T1),
	fb_1(Ts, M1, I2, I1, Ts1).
fb_1([], _, I, I, []).

fb_2(rdf(S, P, O), M, M1, I, I1, rdf(S1, P1, O1)) :-
	fb_3(S, M, M2, I, I2, S1),
	fb_3(P, M2, M3, I2, I3, P1),
	fb_3(O, M3, M1, I3, I1, O1).
	
fb_3(blank(Id), M, M1, I, I1, blank(Id1)) :-
	!,
	( memberchk(Id-Id1, M) ->
	  M1 = M,
	  I1 = I
	; concat_atom(['k', I], Id1),
	  M1 = [Id-Id1|M],
	  I1 is I + 1
	).
fb_3(X, M, M, I, I, X).

retrieve_documents([X|Xs], [X1|Xs1], [N|Ns]) :-
	uri_load_type(X, LoadType),
	get_document(X, LoadType, [namespaces(N)], X1),
	retrieve_documents(Xs, Xs1, Ns).
retrieve_documents([], [], []).

uri_load_type(Uri, plrdf) :-
	sub_atom(Uri, _, _, 0, '.plrdf'),
	!.
uri_load_type(Uri, plrdf) :-
	sub_atom(Uri, _, _, 0, '.PLRDF'),
	!.
uri_load_type(_, rdf).

add_namespace(_, N, _) :-
	pseudo_namespace(_, N, A),
	!,
	err('Namespace ~q is built in as ~q.', [N, A]).
add_namespace(KB, N, _) :-
	fact(KB, N, sys_abbreviation, literal(A1)),
	!,
	err('Namespace ~q is already abbreviated as ~q.', [N, A1]).
add_namespace(KB, _, A) :-
	fact(KB, N1, sys_abbreviation, literal(A)),
	!,
	err('Abbreviation ~q is already used for namespace ~q.', [A, N1]).
add_namespace(KB, N, A) :-
	check_write_permission(KB),
	kb_id(KB, K),
	assert_store(K, N, sys_abbreviation, literal(A)),
	invalidate_cache(K).


%% 
%% We assume that sys_abbreviation is just in store, and not extended
%% by the closure/cache stuff.
%% 
store_namespace(_, N, _) :-
	pseudo_namespace(_, N, _),
	!.
store_namespace(K, N, _) :-
	store(K, N, sys_abbreviation, _),
	!.
store_namespace(K, N, A) :-
	find_name(A, A1),
	\+ store(K, _, sys_abbreviation, literal(A1)),
	!,
	assert_store(K, N, sys_abbreviation, literal(A1)).

find_name(Name, Name).
find_name(Name, Name1) :-
	gen_new_name_n(Name, 1, Name1).

gen_new_name_n(Name, N, Name1) :-
	concat_atom([Name, N], Name1).
gen_new_name_n(Name, N, Name1) :-
	N1 is N + 1,
	gen_new_name_n(Name, N1, Name1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Sortable Time Representation
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


current_time(Time) :-
	get_time(T1),
	convert_time(T1, Y, M, D, H, N, S, _),
	FMT='~`0t~d~4|/~`0t~d~7|/~`0t~d~10| ~`0t~d~13|:~`0t~d~16|:~`0t~d~19|',
	sformat(T2, FMT, [Y, M, D, H, N, S]),
	string_to_atom(T2, Time).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% File Out And In
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% A primitive way to file out and in the complete state of the system.
%%%%
%%%% - Extending KBs are not properly dumped.
%%%%

:- use_module('swilib/fromonto').

file_out(File) :-
	onto_file( dump_write , File ).

dump_write :-
	format('% Knowledgebases Dump.~n'),
	setof(K, catalog_fact(K, rdf_type, sys_Knowledgebase), Ks),
	format('~k.~n', [kbs(Ks)]),
	( store(K, B, C, D),
	  \+ store(K, C, sys_transient, sys_true),
	  format('~k.~n', [s(K, B, C, D)]),
	  fail
	; true
	),
	format('% EOF~n').

file_in(File) :-
        from_file( dump_read, File ).

dump_read :-
	remove_knowledgebase(_),	
	( read(kbs(KBs)) ->
	  ( member(K, KBs),
	    create_store(K),
	    create_cache(K),
	    fail
	  ; true
	  )
	; err('Bad dump format.')
	),
	( repeat,
	  read(T),
	  ( T == end_of_file ->
	    !
	  ; T = s(A, B, C, D),
	    store_assert(A, B, C, D),
	    fail
	  )
	; true
	).
