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
%%%% Graphs Util
%%%% 
%%%% Further graph operations.
%%%% See also 'swilib/graphs'.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(graphs_util, [ pgraph_minimize/2,
			 pgraph_subsumes_chk/4,
			 nodeset_subsumes_chk/2 ]).

:- use_module('swilib/graphs').
:- use_module(library(ordsets)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Graph subsumption
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%% 
%%%% Checks whether there is a mapping of nodes from Graph1 to Graph2
%%%% such that the Labels are preserved under some unifier (i.e. they can be
%%%% more special in Graph2) and Graph1 is a subgraph of Graph2.
%%%% 
%%%% Call this with the transitively closed graph for transitive properties
%%%% (such as action_after).
%%%%
%%%% Edges are PGraphs
%%%% Nodes are lists of Label-Id
%%%%
pgraph_subsumes_chk(Nodes1, Edges1, Nodes2, Edges2) :-
	length(Nodes1, L1),
	length(Nodes2, L2),
	( L1 > L2 ->
	  !,
	  fail
	; length(Edges1, L3),
	  length(Edges2, L4),
	  L3 > L4 ->
	  !,
	  fail
	),
	fail.
pgraph_subsumes_chk(Nodes1, Edges1, Nodes2, Edges2) :-
	copy_term(Nodes1, Nodes3),
	copy_term(Nodes2, Nodes4),
	numbervars(Nodes4, 0, _),
	node_assignment(Nodes3, Nodes4, Map),
	map_apply_node_assignment(Edges1, Map, Edges3),
	sort(Edges3, Edges4),
	sort(Edges2, Edges5),
	ord_subset(Edges4, Edges5),
	!.

%%%%
%%%% Checks whether there is a mapping of nodes from Graph1 to Graph2
%%%% such that the Labels are preserved under some unifier (i.e. they can be
%%%% more special in Graph2).
%%%%
nodeset_subsumes_chk(Nodes1, Nodes2) :-
	length(Nodes1, L1),
	length(Nodes2, L2),
	( L1 > L2 ->
	  !,
	  fail
	; fail
	).
nodeset_subsumes_chk(Nodes1, Nodes2) :-
	copy_term(Nodes1, Nodes3),
	copy_term(Nodes2, Nodes4),
	numbervars(Nodes4, 0, _),
	node_assignment_1(Nodes3, Nodes4),
	!.

node_assignment_1([N-_|NIs], NIs1) :-
	%% Seems hard to make use of the ordered nodes in the select,
	%% since N can be nonground.
	oc_select_1(N, NIs1, NIs2),
	node_assignment_1(NIs, NIs2).
node_assignment_1([], _).

oc_select_1(X, [X1-_|Xs], Xs) :-
	X == X1,
	!.
oc_select_1(X, [X1-_|Xs], Xs) :-
	unify_with_occurs_check(X, X1).
oc_select_1(X, [Y|Xs], [Y|Ys]) :-
	oc_select_1(X, Xs, Ys).



node_assignment([N-I|NIs], NIs1, [I-I1|Is]) :-
	%% Seems hard to make use of the ordered nodes in the select,
	%% since N can be nonground.
	oc_select(N-I1, NIs1, NIs2),
	node_assignment(NIs, NIs2, Is).
node_assignment([], _, []).

map_apply_node_assignment([X|Xs], Y1, [X1|Xs1]) :-
	apply_node_assignment(X, Y1, X1),
	map_apply_node_assignment(Xs, Y1, Xs1).
map_apply_node_assignment([], _, []).

apply_node_assignment(I1-I2, Map, I3-I4) :-
	memberchk(I1-I3, Map),
	memberchk(I2-I4, Map).

%%%% 
%%%% oc_select(+Elem, +List, -Rest)
%%%% 
oc_select(X, [X1|Xs], Xs) :-
	unify_with_occurs_check(X, X1).
oc_select(X, [Y|Xs], [Y|Ys]) :-
	oc_select(X, Xs, Ys).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Minimize edges representing a transitive property.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pgraph_minimize(PG, PG1) :-
	p_to_s_graph(PG, SG),
	warshall(SG, Cs),
	stcgraph_minimize(SG, Cs, PG1).

stcgraph_minimize(SG, TC, PG) :-	
	rmi_1(SG, TC, PG).

rmi_1([N-Ss|G], Cs, PG) :-
	rmi_2(Ss, Ss, N, Cs, PG1, PG),
	rmi_1(G, Cs, PG1).
rmi_1([], _, []).

rmi_2([S|Ss], Ss1, N, Cs, PG, PG1) :-
	member(S1, Ss1),
	memberchk(S1-Cs1, Cs),
	memberchk(S, Cs1),
	!,
	rmi_2(Ss, Ss1, N, Cs, PG, PG1).
rmi_2([S|Ss], Ss1, N, Cs, PG, [N-S|PG1]) :-
	rmi_2(Ss, Ss1, N, Cs, PG, PG1).
rmi_2([], _, _, _, PG, PG).

