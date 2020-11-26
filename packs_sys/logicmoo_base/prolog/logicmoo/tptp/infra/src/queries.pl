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
%%%% Queries
%%%% 
%%%% Queries useful in different contexts.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(queries,  [tp_direct_sub_of/4,
                     tp_equal/4,
		     tp_graph/3,
		     tp_lessq/4,
		     min_class/4,
		     min_property/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Transitive Properties
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% The transitive property (e.g. rdfs_subClassOf, rdfs_subPropertyOf) TP
%%%% is assumed to actually transitively closed in all of these predicates.
%%%% 

%%%%
%%%% tp_direct_sub_of(+K, +TP, ?X, ?X1)
%%%% 
%%%% The direct relation of a transitive property TP (e.g. direct subclass).
%%%% 
%%%% Call examples: direct_sub_of(kb1, rdfs_subClassOf, C, C1) :-
%%%%                direct_sub_of(kb1, rdfs_subPropertyOf, P, P1) :-
%%%%
tp_direct_sub_of(KB, rdfs_subClassOf, X, X1) :-
	!,
	fact(KB, X, sys_directSubClassOf, X1).
tp_direct_sub_of(KB, rdfs_subPropertyOf, X, X1) :-
	!,
	fact(KB, X, sys_directSubPropertyOf, X1).
tp_direct_sub_of(KB, TP, X, X1) :-
	fact(KB, X, TP, X1),
	X \= X1,
	\+ ( fact(KB, X, TP, X2),
             X2 \= X,
	     X2 \= X1,
	     fact(KB, X2, TP, X1),
	     \+ fact(KB, X2, TP, X),
	     \+ fact(KB, X1, TP, X2) ).

%%%% 
%%%% tp_equal(+K, +TP, ?X, ?X1)
%%%% 
%%%% The equality relation of a transitive property TP. Excludes syntactic
%%%% equality. Symmetrically closed.
%%%% 
tp_equal(KB, TP, X, X1) :-
	X \= X1,
	fact(KB, X, TP, X1),
	fact(KB, X1, TP, X).

%%%%
%%%% tp_graph(KB, TP, Graph)
%%%% 
%%%% Returns a P-Graph of the direct relation corresponding to the direct
%%%% relationship. Equal nodes are identified and represented by terms
%%%% equal(Nodes), where nodes is the ordered list of identified nodes.
%%%% 
tp_graph(KB, TP, Graph) :-
	tp_equal_table(KB, TP, Table),
	( setof( X1-X2, tp_graph_edge(KB, TP, Table, X1, X2), Graph ) ->
	  true
        ; Graph = []
	).

tp_equal_table(KB, TP, Table) :-
	setof(X-equal(X1s),
	      setof(X1, tp_equal(KB, TP, X, X1), X1s),
	      Table),
	!.
tp_equal_table(_, _, []).

tp_graph_edge(KB, TP, Table, X1, X2) :-
	tp_direct_sub_of(KB, TP, X3, X4),
	( memberchk(X3-X1, Table) -> true ; X1 = X3 ),
	( memberchk(X4-X2, Table) -> true ; X2 = X4 ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Minimal Relationships
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% min_class(+K, ?S, ?P, ?C)
%%%% min_property(+K, ?S, ?P, ?O)
%%%% 
%%%% The given relationship, minimized with respect to rdfs_subClassOf or
%%%% and rdfs_subPropertyOf. Examples:
%%%% 
%%%% min_class(kb, s, rdf_type, C) - the direct types of s
%%%% min_class(kb, p, sys_domain, C) - the direct domains of p 
%%%% min_class(kb, p, sys_range, C) - the direct ranges of p
%%%% min_property(kb, s, P, O) - gives only "minimal" properties of s, e.g.
%%%%                             only the "father" is returned, and not the
%%%%  		                 same object as "parent" too.
%%%% 

min_class(KB, S, P, C) :-
	fact(KB, S, P, C),
	\+ ( fact(KB, S, P, C1),
	     C1 \= C,
	     fact(KB, C1, rdfs_subClassOf, C),
	     \+ fact(KB, C, rdfs_subClassOf, C1) ).

min_property(KB, S, P, O) :-
	fact(KB, S, P, O),
	\+ ( fact(KB, S, P1, O),
	     P1 \= P,
	     fact(KB, P1, rdfs_subPropertyOf, P),
	     \+ fact(KB, P, rdfs_subPropertyOf, P1) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% tp_lessq(+KB, +TP, +X1, -X2)
%%%% tp_lessq(+KB, +TP, -X1, +X2)
%%%% tp_lessq(+KB, +TP, +X1, +X2)
%%%% 
tp_lessq(KB, TP, X1, X2) :-
	( X1 = X2
	; X1 \== X2,
	  fact(KB, X1, TP, X2),
	  X1 \== X2
	).
