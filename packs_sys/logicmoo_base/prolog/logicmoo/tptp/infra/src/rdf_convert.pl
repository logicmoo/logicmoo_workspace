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

:- module(rdf_convert, [ canonicalize_triples/2,
			 canonicalize_item/2,
                         pseudo_namespace/3,
			 public_item/2]).


:- use_module(library(ordsets)).

%%%% 
%%%% Notes:
%%%% 
%%%% - canonicalized items are
%%%%   - used as input of the knowledgebase
%%%%   - used in queries against the knowledgebase
%%%% 
%%%% - library(rdf)'s xml_to_rdf does NOT accept canonicalized XML elements,
%%%%   i.e. canonicalization must be performed afterwards on the triples.
%%%% 
%%%% - canonicalize_xml/2 can be used to canonicalize XML elements which
%%%%   are used by other functions.
%%%% 


%%%% 
%%%% canonicalize_triples(+Triples, -Triples1)
%%%%
%%%% This procedure canonicalized the output of the RDF parser for
%%%% input to the knowledgebase.
%%%%

canonicalize_triples(Triples, Triples1) :-
	concat_namespaces(Triples, Triples1, _Namespaces).

%%%%
%%%% concat_namespaces(+Triples, -Triples1, -Namespaces)
%%%% 
%%%% Replace the (Namespace:Reference) items in the triples by
%%%% their concatenation. The namespaces found are
%%%% also returned as ordered set.
%%%%
%%%% Namespaces listed in pseudo_namespace/3 are handled specially.
%%%%
concat_namespaces(Triples, Triples1, Namespaces) :-
	cns(Triples, Triples1, [], Namespaces).

cns([rdf(S,P,O)|Ts], [rdf(S1,P1,O1)|Ts1], Ns, Ns1) :-
	concat_namespace(S, S1, Ns, Ns2),
	concat_namespace(P, P1, Ns2, Ns3),
	concat_namespace(O, O1, Ns3, Ns4),
	cns(Ts, Ts1, Ns4, Ns1).
cns([], [], Ns, Ns).

concat_namespace(A:B, Uri, Ns, Ns1) :-
	!,
	( pseudo_namespace(Prefix, A, _) ->
	  A1 = A,  
	  atom_concat(Prefix, B, Uri)
	; pseudo_namespace(Prefix, A1, A) ->
	  atom_concat(Prefix, B, Uri)
	; A1 = A,
	  atom_concat(A, B, Uri)
	),
	ord_add_element(Ns, A1, Ns1).
concat_namespace(A, A1, Ns, Ns1) :-
	atom(A),
	pseudo_namespace(Prefix, Namespace, _),
	sub_atom(A, 0, B, L, Namespace),
	!,
	sub_atom(A, B, L, _, A2),
	atom_concat(Prefix, A2, A1),
	ord_add_element(Ns, Namespace, Ns1).
concat_namespace(A, A1, Ns, Ns) :-
	atom(A),
	sub_atom(A, 0, B, _, 'http://www.infraengine.com/blank#'),
	!,
	sub_atom(A, B, _, 0, Encoded),
	term_to_atom(A1, Encoded).
concat_namespace(A, A, Ns, Ns).	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Specially handled prefixes
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pseudo_namespace('rdf_', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#', rdf).
pseudo_namespace('rdfs_', 'http://www.w3.org/2000/01/rdf-schema#', rdfs).
pseudo_namespace('sys_', 'http://www.infraengine.com/system#', sys).
pseudo_namespace('user_', 'http://www.infraengine.com/user#', user).
pseudo_namespace('inf_', 'http://www.infraengine.com/planner/', inf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Standardize Item
%%%% 
%%%% 
%%%% Mimics the canonicalization of concat_namespaces for single items, that
%%%% are asserted in special ways to a knowledgebase.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

canonicalize_item(Item, Uri) :-
	concat_namespace(Item, Uri, [], _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% public_item(Item, Uri)
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

public_item(Item, Item1) :-
	atom(Item),
	pseudo_namespace(Prefix, Namespace, _),
	sub_atom(Item, 0, B, L, Prefix),
	!,
	sub_atom(Item, B, L, _, Name),
	atom_concat(Namespace, Name, Item1).
public_item(Item, Item).


