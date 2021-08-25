/* Copyright(C) 1992, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   File   : UGRAPHS.plL							      %
%   Maintainer : Mats Carlsson						      %
%            New versions of transpose/2, reduce/2, top_sort/2 by Dan Sahlin  %
%   Updated: 3 September 1999						      %
%   Purpose: Unweighted graph-processing utilities			      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*  Adapted from shared code written by Richard A O'Keefe */

/*  An unweighted directed graph (ugraph) is represented as a list of
    (vertex-neighbors) pairs, where the pairs are in standard order
    (as produced by keysort with unique keys) and the neighbors of
    each vertex are also in standard order (as produced by sort), and
    every neighbor appears as a vertex even if it has no neighbors
    itself.

    An undirected graph is represented as a directed graph where for
    each edge (U,V) there is a symmetric edge (V,U).

    An edge (U,V) is represented as the term U-V.

    A vertex can be any term.  Two vertices are distinct iff they are
    not identical (==).

    A path is represented as a list of vertices.  
    No vertex can appear twice in a path.
*/

tupleToGraphComp3(P,A,B).

/*
:- module(ugraphs, [
	vertices_edges_to_ugraph/3,
	vertices/2,
	edges/2,
	add_vertices/3,
	del_vertices/3,
	add_edges/3,
	del_edges/3,
	transpose/2,
	neighbors/3,
	neighbours/3,
	complement/2,
	compose/3,
	transitive_closure/2,
	symmetric_closure/2,
	top_sort/2,
	max_path/5,
	min_path/5,
	min_paths/3,
	path/3,
	reduce/2,
	reachable/3,
	random_ugraph/3,
	min_tree/3,
	clique/3,
	independent_set/3,
	coloring/3,
	colouring/3
   ]).

:- use_module(library(ordsets), [
	my_ord_add_element/3,
	my_ord_del_element/3,
	my_ord_disjoint/2,
	my_ord_intersection/3,
	my_ord_subset/2,
	my_ord_subtract/3,
	my_ord_union/3,
	my_ord_union/4
   ]).

:- use_module(library(lists), [
	append/3,
	member/2,
	reverse/2
   ]).

:- use_module(library(assoc), [
	list_to_assoc/2,
	ord_list_to_assoc/2,
	get_assoc/3,
	get_assoc/5
   ]).

:- use_module(library(random), [
	random/1
   ]).
	    */
%   vertices_edges_to_ugraph(+Vertices, +Edges, -Graph)
%   is true if Vertices is a list of vertices, Edges is a list of edges,
%   and Graph is a graph built from Vertices and Edges.  Vertices and
%   Edges may be in any order.  The vertices mentioned in Edges do not
%   have to occur explicitly in Vertices.  Vertices may be used to
%   specify vertices that are not connected to any edges.

vertices_edges_to_ugraph(Vertices0, Edges, Graph) :-
	sort(Vertices0, Vertices1),
	sort(Edges, EdgeSet),
	edges_vertices(EdgeSet, Bag),
	sort(Bag, Vertices2),
	my_ord_union(Vertices1, Vertices2, VertexSet),
	group_edges(VertexSet, EdgeSet, Graph).

edges_vertices([], []).
edges_vertices([From-To|Edges], [From,To|Vertices]) :-
	edges_vertices(Edges, Vertices).

group_edges([], _, []).
group_edges([Vertex|Vertices], Edges, [Vertex-Neibs|G]) :-
	group_edges(Edges, Vertex, Neibs, RestEdges),
	group_edges(Vertices, RestEdges, G).

group_edges([V0-X|Edges], V, [X|Neibs], RestEdges) :- V0==V, !,
	group_edges(Edges, V, Neibs, RestEdges).
group_edges(Edges, _, [], Edges).



%   vertices(+Graph, -Vertices)
%   unifies Vertices with the vertices in Graph.

vertices([], []).
vertices([Vertex-_|Graph], [Vertex|Vertices]) :- vertices(Graph, Vertices).



%   edges(+Graph, -Edges)
%   unifies Edges with the edges in Graph.

edges([], []).
edges([Vertex-Neibs|G], Edges) :-
	edges(Neibs, Vertex, Edges, MoreEdges),
	edges(G, MoreEdges).

edges([], _, Edges, Edges).
edges([Neib|Neibs], Vertex, [Vertex-Neib|Edges], MoreEdges) :-
	edges(Neibs, Vertex, Edges, MoreEdges).



%   add_vertices(+Graph1, +Vertices, -Graph2)
%   is true if Graph2 is Graph1 with Vertices added to it.

add_vertices(Graph0, Vs0, Graph) :-
	sort(Vs0, Vs),
	vertex_units(Vs, Graph1),
	graph_union(Graph0, Graph1, Graph).



%   del_vertices(+Graph1, +Vertices, -Graph2)
%   is true if Graph2 is Graph1 with Vertices and all edges to and from
%   Vertices removed from it.

del_vertices(Graph0, Vs0, Graph) :-
	sort(Vs0, Vs),
	graph_del_vertices(Graph0, Vs, Vs, Graph).



%   add_edges(+Graph1, +Edges, -Graph2) 
%   is true if Graph2 is Graph1 with Edges and their "to" and "from"
%   vertices added to it.

add_edges(Graph0, Edges0, Graph) :-
	sort(Edges0, EdgeSet),
	edges_vertices(EdgeSet, Vs0),
	sort(Vs0, Vs),
	group_edges(Vs, EdgeSet, Graph1),
	graph_union(Graph0, Graph1, Graph).



%   del_edges(+Graph1, +Edges, -Graph2)
%   is true if Graph2 is Graph1 with Edges removed from it.

del_edges(Graph0, Edges0, Graph) :-
	sort(Edges0, EdgeSet),
	edges_vertices(EdgeSet, Vs0),
	sort(Vs0, Vs),
	group_edges(Vs, EdgeSet, Graph1),
	graph_difference(Graph0, Graph1, Graph).



vertex_units([], []).
vertex_units([V|Vs], [V-[]|Us]) :- vertex_units(Vs, Us).


graph_union(G0, [], G) :- !, G = G0.
graph_union([], G, G).
graph_union([V1-N1|G1], [V2-N2|G2], G) :-
	compare(C, V1, V2),
	graph_union(C, V1, N1, G1, V2, N2, G2, G).

graph_union(<, V1, N1, G1, V2, N2, G2, [V1-N1|G]) :-
	graph_union(G1, [V2-N2|G2], G).
graph_union(=, V, N1, G1, _, N2, G2, [V-N|G]) :-
	my_ord_union(N1, N2, N),
	graph_union(G1, G2, G).
graph_union(>, V1, N1, G1, V2, N2, G2, [V2-N2|G]) :-
	graph_union([V1-N1|G1], G2, G).



graph_difference(G0, [], G) :- !, G = G0.
graph_difference([], _, []).
graph_difference([V1-N1|G1], [V2-N2|G2], G) :-
	compare(C, V1, V2),
	graph_difference(C, V1, N1, G1, V2, N2, G2, G).

graph_difference(<, V1, N1, G1, V2, N2, G2, [V1-N1|G]) :-
	graph_difference(G1, [V2-N2|G2], G).
graph_difference(=, V, N1, G1, _, N2, G2, [V-N|G]) :-
	my_ord_subtract(N1, N2, N),
	graph_difference(G1, G2, G).
graph_difference(>, V1, N1, G1, _, _, G2, G) :-
	graph_difference([V1-N1|G1], G2, G).


graph_del_vertices(G1, [], Set, G) :- !,
	graph_del_vertices(G1, Set, G).
graph_del_vertices([], _, _, []).
graph_del_vertices([V1-N1|G1], [V2|Vs], Set, G) :-
	compare(C, V1, V2),
	graph_del_vertices(C, V1, N1, G1, V2, Vs, Set, G).

graph_del_vertices(<, V1, N1, G1, V2, Vs, Set, [V1-N|G]) :-
	my_ord_subtract(N1, Set, N),
	graph_del_vertices(G1, [V2|Vs], Set, G).
graph_del_vertices(=, _, _, G1, _, Vs, Set, G) :-
	graph_del_vertices(G1, Vs, Set, G).
graph_del_vertices(>, V1, N1, G1, _, Vs, Set, G) :-
	graph_del_vertices([V1-N1|G1], Vs, Set, G).

graph_del_vertices([], _, []).
graph_del_vertices([V1-N1|G1], Set, [V1-N|G]) :-
	my_ord_subtract(N1, Set, N),
	graph_del_vertices(G1, Set, G).



%   transpose(+Graph, -Transpose)
%   is true if Transpose is the graph computed by replacing each edge
%   (u,v) in Graph by its symmetric edge (v,u).  It can only be used
%   one way around.  The cost is O(N log N).

transpose(Graph, Transpose) :-
	transpose_edges(Graph, TEdges, []),
	sort(TEdges, TEdges2),
	vertices(Graph, Vertices),
	group_edges(Vertices, TEdges2, Transpose).

transpose_edges([]) --> [].
transpose_edges([Vertex-Neibs|G]) -->
        transpose_edges(Neibs, Vertex),
	transpose_edges(G).

transpose_edges([], _) --> [].
transpose_edges([Neib|Neibs], Vertex) --> [Neib-Vertex],
	transpose_edges(Neibs, Vertex).


%   neighbours(+Vertex, +Graph, -Neighbors)
%   neighbors(+Vertex, +Graph, -Neighbors)
%   is true if Vertex is a vertex in Graph and Neighbors are its neighbors.

neighbours(Vertex, Graph, Neighbors) :-
	neighbors(Vertex, Graph, Neighbors).

neighbors(V, [V0-Neighbors|_], Neighbors) :- V0==V, !.
neighbors(V, [_|Graph], Neighbors) :- neighbors(V, Graph, Neighbors).



%   complement(+Graph, -Complement)
%   Complement is the complement graph of Graph, i.e. the graph that has
%   the same vertices as Graph but only the edges that are not in Graph.

complement(Graph, Complement) :-
	vertices(Graph, Vertices),
	complement(Graph, Vertices, Complement).

complement([], _, []).
complement([V-Neibs|Graph], Vertices, [V-Others|Complement]) :-
	my_ord_add_element(Neibs, V, Neibs1),
	my_ord_subtract(Vertices, Neibs1, Others),
	complement(Graph, Vertices, Complement).



%   compose(+G1, +G2, -Composition)
%   computes Composition as the composition of two graphs, which need
%   not have the same set of vertices.

compose(G1, G2, Composition) :-
	vertices(G1, V1),
	vertices(G2, V2),
	my_ord_union(V1, V2, V),
	compose(V, G1, G2, Composition).

compose([], _, _, []).
compose([V0|Vertices], [V-Neibs|G1], G2, [V-Comp|Composition]) :- V==V0, !,
	compose1(Neibs, G2, [], Comp),
	compose(Vertices, G1, G2, Composition).
compose([V|Vertices], G1, G2, [V-[]|Composition]) :-
	compose(Vertices, G1, G2, Composition).

compose1([V1|Vs1], [V2-N2|G2], SoFar, Comp) :- !,
	compare(Rel, V1, V2),
	compose1(Rel, V1, Vs1, V2, N2, G2, SoFar, Comp).
compose1(_, _, Comp, Comp).

compose1(<, _, Vs1, V2, N2, G2, SoFar, Comp) :-
	compose1(Vs1, [V2-N2|G2], SoFar, Comp).
compose1(>, V1, Vs1, _, _, G2, SoFar, Comp) :-
	compose1([V1|Vs1], G2, SoFar, Comp).
compose1(=, V1, Vs1, V1, N2, G2, SoFar, Comp) :-
	my_ord_union(N2, SoFar, Next),
	compose1(Vs1, G2, Next, Comp).



%   transitive_closure(+Graph, -Closure) 
%   computes Closure as the transitive closure of Graph in O(N^3) time.

/*

transitive_closure(Graph, Closure) :-
	warshall(Graph, Graph, Closure).

warshall([], Closure, Closure).
warshall([V-_|G], E, Closure) :-
	neighbors(V, E, Y),
	warshall(E, V, Y, NewE),
	warshall(G, NewE, Closure).

warshall([], _, _, []).
warshall([X-Neibs|G], V, Y, [X-NewNeibs|NewG]) :-
	my_ord_subset([V], Neibs), !,
	my_ord_del_element(Y, X, Y1),
	my_ord_union(Neibs, Y1, NewNeibs),
	warshall(G, V, Y, NewG).
warshall([X-Neibs|G], V, Y, [X-Neibs|NewG]) :-
	warshall(G, V, Y, NewG).
*/


%   symmetric_closure(+Graph, -Closure) 
%   computes Closure as the symmetric closure of Graph, i.e.  for each
%   edge (u,v) in Graph, add its symmetric edge (v,u).  Approx O(N log N)
%   time.  This is useful for making a directed graph undirected.

symmetric_closure(Graph, Closure) :-
	transpose(Graph, Transpose),
	symmetric_closure(Graph, Transpose, Closure).

symmetric_closure([], [], []).
symmetric_closure([V-Neibs1|Graph], [V-Neibs2|Transpose], [V-Neibs|Closure]) :-
	my_ord_union(Neibs1, Neibs2, Neibs),
	symmetric_closure(Graph, Transpose, Closure).



%   top_sort(+Graph, -Sorted)
%   finds a topological ordering of a Graph and returns the ordering
%   as a list of Sorted vertices.  Fails iff no ordering exists, i.e.
%   iff the graph contains cycles.  Approx O(N log N) time.

top_sort(Graph, Sorted) :-
	fanin_counts(Graph, Counts),
	get_top_elements(Counts, Top, 0, I),
	ord_list_to_assoc(Counts, Map),
	top_sort(Top, I, Map, Sorted).

top_sort([], 0, _, []).
top_sort([V-VN|Top0], I, Map0, [V|Sorted]) :-
	dec_counts(VN, I, J, Map0, Map, Top0, Top),
	top_sort(Top, J, Map, Sorted).

dec_counts([], I, I, Map, Map, Top, Top).
dec_counts([N|Ns], I, K, Map0, Map, Top0, Top) :-
	get_assoc(N, Map0, NN-C0, Map1, NN-C),
	C is C0-1,
	(C=:=0 -> J is I-1, Top1 = [N-NN|Top0]; J = I, Top1 = Top0),
	dec_counts(Ns, J, K, Map1, Map, Top1, Top).

get_top_elements([], [], I, I).
get_top_elements([V-(VN-C)|Counts], Top0, I, K) :-
	(C=:=0 -> J = I, Top0 = [V-VN|Top1]; J is I+1, Top0 = Top1),
	get_top_elements(Counts, Top1, J, K).

fanin_counts(Graph, Counts) :-
	transpose_edges(Graph, Edges0, []),
	keysort(Edges0, Edges),
	fanin_counts(Graph, Edges, Counts).

fanin_counts([], [], []).
fanin_counts([V-VN|Graph], Edges, [V-(VN-C)|Counts]) :-
	fanin_counts(Edges, V, 0, C, Edges1),
	fanin_counts(Graph, Edges1, Counts).

fanin_counts([V-_|Edges0], V0, C0, C, Edges) :-
	V==V0, !,
	C1 is C0+1,
	fanin_counts(Edges0, V0, C1, C, Edges).
fanin_counts(Edges, _, C, C, Edges).


%   max_path(+V1, +V2, +Graph, -Path, -Cost)
%   is true if Path is a list of vertices constituting a longest path
%   of cost Cost from V1 to V2 in Graph, there being no cyclic paths from
%   V1 to V2.  Takes O(N^2) time.

max_path(Initial, Final, Graph, Path, Cost) :-
	transpose(Graph, TGraph),
	max_path_init(Initial, Final, Graph, TGraph, TGraph2, Order),
	max_path_init(TGraph2, Val0),
	max_path(Order, TGraph2, Val0, Val),
	max_path_select(Val, Path, Cost).

max_path_init(Initial, Final, Graph, TGraph, TGraph2, Order) :-
	reachable(Initial, Graph, InitialReachable),
	reachable(Final, TGraph, FinalReachable),
	my_ord_intersection(InitialReachable, FinalReachable, Reachable),
	subgraph(TGraph, Reachable, TGraph2),
	top_sort(TGraph2, Order).

max_path_init([], []).
max_path_init([V-_|G], [V-([]-0)|Val]) :- max_path_init(G, Val).

max_path_select([V-(Longest-Max)|Val], Path, Cost) :-
	max_path_select(Val, V, Longest, Path, Max, Cost).

max_path_select([], V, Path, [V|Path], Cost, Cost).
max_path_select([V1-(Path1-Cost1)|Val], V2, Path2, Path, Cost2, Cost) :-
	(   Cost1>Cost2 -> 
	    max_path_select(Val, V1, Path1, Path, Cost1, Cost)
	;   max_path_select(Val, V2, Path2, Path, Cost2, Cost)
	).

max_path([], _, Val, Val).
max_path([V|Order], Graph, Val0, Val) :-
	neighbors(V, Graph, Neibs),
	neighbors(V, Val0, Item),
	max_path_update(Neibs, V-Item, Val0, Val1),
	max_path(Order, Graph, Val1, Val).

max_path_update([], _, Val, Val).
max_path_update([N|Neibs], Item, [Item0|Val0], Val) :-
	Item0 = V0-(_-Cost0),
	N==V0, !,
	Item = V-(Path-Cost),
	Cost1 is Cost+1,
	(   Cost1>Cost0 -> Val = [V0-([V|Path]-Cost1)|Val1]
	;   Val = [Item0|Val1]
	),
	max_path_update(Neibs, Item, Val0, Val1).
max_path_update(Neibs, Item, [X|Val0], [X|Val]) :-
	max_path_update(Neibs, Item, Val0, Val).

subgraph([], _, []).
subgraph([V-Neibs|Graph], Vs, [V-Neibs1|Subgraph]) :-
	my_ord_subset([V], Vs), !,
	my_ord_intersection(Neibs, Vs, Neibs1),
	subgraph(Graph, Vs, Subgraph).
subgraph([_|Graph], Vs, Subgraph) :-
	subgraph(Graph, Vs, Subgraph).



%   min_path(+V1, +V2, +Graph, -Path, -Length)
%   is true if Path is a list of vertices constituting a shortest path
%   of length Length from V1 to V2 in Graph.  Takes O(N^2) time.

min_path(Initial, Final, Graph, Path, Length) :-
	min_path([[Initial]|Q], Q, [Initial], Final, Graph, Rev),
	reverse(Rev, Path),
	length(Path, N),
	Length is N-1.

min_path(Head0, Tail0, Closed0, Final, Graph, Rev) :-
	Head0 \== Tail0,
	Head0 = [Sofar|Head],
	Sofar = [V|_],
	(   V==Final -> Rev = Sofar
	;   neighbors(V, Graph, Neibs),
	    my_ord_union(Closed0, Neibs, Closed, Neibs1),
	    enqueue(Neibs1, Sofar, Tail0, Tail),
	    min_path(Head, Tail, Closed, Final, Graph, Rev)
	).

enqueue([], _) --> [].
enqueue([V|Vs], Sofar) --> [[V|Sofar]], enqueue(Vs, Sofar).



%   min_paths(+Vertex, +Graph, -Tree)
%   is true if Tree is a tree of all the shortest paths from Vertex to
%   every other vertex in Graph.  This is the single-source shortest
%   paths problem.  The algorithm is straightforward.

min_paths(Vertex, Graph, Tree) :-
	min_paths([Vertex], Graph, [Vertex], List),
	keysort(List, Tree).

min_paths([], _, _, []).
min_paths([Q|R], Graph, Reach0, [Q-New|List]) :-
	neighbors(Q, Graph, Neibs),
	my_ord_union(Reach0, Neibs, Reach, New),
	append(R, New, S),
	min_paths(S, Graph, Reach, List).



%   path(+Vertex, +Graph, -Path)
%   is given a Graph and a Vertex of that Graph, and returns a maximal
%   Path rooted at Vertex, enumerating more Paths on backtracking.

path(Initial, Graph, Path) :-
	path([Initial], [], Graph, Path).

path(Q, Not, Graph, Path) :-
	Q = [Qhead|_],
	neighbors(Qhead, Graph, Neibs),
	my_ord_subtract(Neibs, Not, Neibs1),
	(   Neibs1 = [] -> reverse(Q, Path)
	;   my_ord_add_element(Not, Qhead, Not1),
	    member(N, Neibs1),
	    path([N|Q], Not1, Graph, Path)
	).



%   reduce(+Graph, -Reduced)
%   is true if Reduced is the reduced graph for Graph. The vertices of
%   the reduced graph are the strongly connected components of Graph.
%   There is an edge in Reduced from u to v iff there is an edge in
%   Graph from one of the vertices in u to one of the vertices in v. A
%   strongly connected component is a maximal set of vertices where
%   each vertex has a path to every other vertex.
%   Algorithm from "Algorithms" by Sedgewick, page 482, Tarjan's algorithm.
%   Approximately linear in the maximum of arcs and nodes (O(N log N)).

reduce(Graph, Reduced) :-
	strong_components(Graph, SCCS, Map),
	reduced_vertices_edges(Graph, Vertices, Map, Edges, []),
	sort(Vertices, Vertices1),
	sort(Edges, Edges1),
	group_edges(Vertices1, Edges1, Reduced),
	sort(SCCS, Vertices1).

strong_components(Graph, SCCS, A) :-
	nodeinfo(Graph, Nodeinfo, Vertices), 
	ord_list_to_assoc(Nodeinfo, A0), 
	visit(Vertices, 0, _, A0, A, 0, _, [], _, SCCS, []).

visit([], Min, Min, A, A, I, I, Stk, Stk) --> [].
visit([V|Vs], Min0, Min, A0, A, I, M, Stk0, Stk) -->
	{get_assoc(V, A0, node(Ns,J,Eq), A1, node(Ns,K,Eq))},
	(   {J>0} ->
	    {J=K, J=Min1, A1=A3, I=L, Stk0=Stk2}
	;   {K is I+1},
	    visit(Ns, K, Min1, A1, A2, K, L, [V|Stk0], Stk1),
	    (   {K>Min1} -> {A2=A3, Stk1=Stk2}
	    ;   pop(V, Eq, A2, A3, Stk1, Stk2, [])
	    )
	),
	{Min2 is min(Min0,Min1)},
	visit(Vs, Min2, Min, A3, A, L, M, Stk2, Stk).

pop(V, Eq, A0, A, [V1|Stk0], Stk, SCC0) -->
	{get_assoc(V1, A0, node(Ns,_,Eq), A1, node(Ns,16'100000,Eq))},
	(   {V==V1} -> [SCC], {A1=A, Stk0=Stk, sort([V1|SCC0], SCC)}
	;   pop(V, Eq, A1, A, Stk0, Stk, [V1|SCC0])
	).

nodeinfo([], [], []).
nodeinfo([V-Ns|G], [V-node(Ns,0,_)|Nodeinfo], [V|Vs]) :-
	nodeinfo(G, Nodeinfo, Vs).

reduced_vertices_edges([], [], _) --> [].
reduced_vertices_edges([V-Neibs|Graph], [V1|Vs], Map) -->
	{get_assoc(V, Map, N), N=node(_,_,V1)},
	reduced_edges(Neibs, V1, Map),
	reduced_vertices_edges(Graph, Vs, Map).

reduced_edges([], _, _) --> [].
reduced_edges([V|Vs], V1, Map) -->
	{get_assoc(V, Map, N), N=node(_,_,V2)},
	({V1==V2} -> []; [V1-V2]),
	reduced_edges(Vs, V1, Map).


%   reachable(+Vertex, +Graph, -Reachable)
%   is given a Graph and a Vertex of that Graph, and returns the set
%   of vertices that are Reachable from that Vertex.  Takes O(N^2)
%   time.

reachable(Initial, Graph, Reachable) :-
	reachable([Initial], Graph, [Initial], Reachable).

reachable([], _, Reachable, Reachable).
reachable([Q|R], Graph, Reach0, Reachable) :-
	neighbors(Q, Graph, Neighbors),
	my_ord_union(Reach0, Neighbors, Reach1, New),
	append(R, New, S),
	reachable(S, Graph, Reach1, Reachable).



%   random_ugraph(+P, +N, -Graph)
%   where P is a probability, unifies Graph with a random graph of N
%   vertices where each possible edge is included with probability P.

random_ugraph(P, N, Graph) :-
	(   float(P), 
		P >= 0.0, P =< 1.0 -> true
	;   /*prolog:*/illarg(domain(float,between(0.0,1.0)),
	                  random_ugraph(P,N,Graph), 1)
	),
	(   integer(N), N >= 0 -> true
	;   /*prolog:*/illarg(domain(integer,>=(0)),
	                  random_ugraph(P,N,Graph), 2)
	),
	random_ugraph(0, N, P, Graph).

random_ugraph(N, N, _, Graph) :- !, Graph = [].
random_ugraph(I, N, P, [J-List|Graph]) :-
	J is I+1,
	random_neighbors(N, J, P, List, []),
	random_ugraph(J, N, P, Graph).

random_neighbors(0, _, _, S0, S) :- !, S = S0.
random_neighbors(N, J, P, S0, S) :-
	(   N==J -> S1 = S
	;   random(X), X > P -> S1 = S
	;   S1 = [N|S]
	),
	M is N-1,
	random_neighbors(M, J, P, S0, S1).



%   min_tree(+Graph, -Tree, -Cost)
%   is true if Tree is a spanning tree of an *undirected* Graph with
%   cost Cost, if it exists.  Using a version of Prim's algorithm.

min_tree([V-Neibs|Graph], Tree, Cost) :-
	length(Graph, Cost),
	prim(Cost, Neibs, Graph, [V], Edges),
	vertices_edges_to_ugraph([], Edges, Tree).	

prim(0, [], _, _, []).
prim(I, [V|Vs], Graph, Reach, [V-W,W-V|Edges]) :-
	neighbors(V, Graph, Neibs),
	my_ord_subtract(Neibs, Reach, Neibs1),
	my_ord_subtract(Neibs, Neibs1, [W|_]),
	my_ord_add_element(Reach, V, Reach1),
	my_ord_union(Vs, Neibs1, Vs1),
	J is I-1,
	prim(J, Vs1, Graph, Reach1, Edges).



%   clique(+Graph, +K, -Clique)
%   is true if Clique is a maximal clique (complete subgraph) of N
%   vertices of an *undirected* Graph, where N>=K.  Adapted from
%   Algorithm 457, "Finding All Cliques of an Undirected Graph [H]",
%   Version 1, by Coen Bron and Joep Kerbosch, CACM vol. 6 no. 9 pp.
%   575-577, Sept. 1973.

clique(Graph, K, Clique) :-
	(   integer(K), K >= 0 -> true
	;   /*prolog:*/illarg(domain(integer,>=(0)),
	                  clique(Graph,K,Clique), 2)
	),
	J is K-1,
	prune(Graph, [], J, Graph1),
	clique1(Graph1, J, Clique).

clique1([], J, []) :- J < 0.
clique1([C-Neibs|Graph], J, [C|Clique]) :-
	neighbor_graph(Graph, Neibs, C, Vs, Graph1),
	J1 is J-1,
	prune(Graph1, Vs, J1, Graph2),
	clique1(Graph2, J1, Clique).
clique1([C-Neibs|Graph], J, Clique) :-
	prune(Graph, [C], J, Graph2),
	clique1(Graph2, J, Clique),
	\+ my_ord_subset(Clique, Neibs).

neighbor_graph([], _, _, [], []).
neighbor_graph([V0-Neibs0|Graph0], [V|Vs], W, Del, [V-Neibs|Graph]) :-
	V0==V, !,
	my_ord_del_element(Neibs0, W, Neibs),
	neighbor_graph(Graph0, Vs, W, Del, Graph).
neighbor_graph([V-_|Graph0], Vs, W, [V|Del], Graph) :-
	neighbor_graph(Graph0, Vs, W, Del, Graph).

prune(Graph0, [], K, Graph) :- K =< 0, !, Graph = Graph0.
prune(Graph0, Vs0, K, Graph) :-
	prune(Graph0, Vs0, K, Graph1, Vs1),
	(   Vs1==[] -> Graph = Graph1
	;   prune(Graph1, Vs1, K, Graph)
	).

prune([], _, _, [], []).
prune([V-Ns0|Graph0], Vs1, K, [V-Ns|Graph], Vs2) :-
	my_ord_disjoint([V], Vs1),
	my_ord_subtract(Ns0, Vs1, Ns),
	length(Ns, I),
	I >= K, !,
	prune(Graph0, Vs1, K, Graph, Vs2).
prune([V-_|Graph0], Vs1, K, Graph, Vs2) :-
	(   my_ord_disjoint([V], Vs1) -> Vs2 = [V|Vs3]
	;   Vs2 = Vs3
	),
	prune(Graph0, Vs1, K, Graph, Vs3).



%   independent_set(+Graph, +K, -Set)
%   is true if Set is a maximal independent (unconnected) set of N
%   vertices of an *undirected* Graph, where N>=K.

independent_set(Graph, K, Set) :-
	complement(Graph, Complement),
	clique(Complement, K, Set).



%   colouring(+Graph, +K, -Coloring)
%   coloring(+Graph, +K, -Coloring)
%   is true if Coloring is a mapping from vertices to colors 1..N of
%   an *undirected* Graph such that all edges have distinct end colors,
%   where N=<K.  Adapted from "New Methods to Color the Vertices of a
%   Graph", by D. Brelaz, CACM vol. 4 no. 22 pp. 251-256, April 1979.
%   Augmented with ideas from Matula's smallest-last ordering.

colouring(Graph, K, Coloring) :-
	coloring(Graph, K, Coloring).

coloring(Graph, K, Coloring) :-
	(   integer(K), K >= 0 -> true
	;   /*prolog:*/illarg(domain(integer,>=(0)),
	                  coloring(Graph,K,Coloring), 2)
	),
	color_map(Graph, Coloring),
	color_map(Graph, Graph1, Coloring, Coloring),
	coloring(Graph1, K, 0, [], Stack),
	color_stack(Stack).

coloring([], _, _, Stk0, Stk) :- !, Stk0 = Stk.
coloring(Graph, K, InUse, Stk0, Stk) :-
	select_vertex(Graph, K, Compare, -, 0+0, V-Ns),
	graph_del_vertices(Graph, [V], [V], Graph1),
	(   Compare = < ->
	    coloring(Graph1, K, InUse, [V-Ns|Stk0], Stk)
	;   M is min(K,InUse+1),
	    vertex_color(Ns, 1, M, V),
	    add_color(Graph1, Ns, V, Graph2),
	    InUse1 is max(V,InUse),
	    coloring(Graph2, K, InUse1, Stk0, Stk)
	).

%   select_vertex(+Graph, +K, -Comp, +Pair0, +Rank, -Pair)
%   return any vertex with degree<K right away (Comp = <) else return
%   vertex with max. saturation degree (Comp = >=), break ties using
%   max. degree

select_vertex([], _, >=, Pair, _, Pair).
select_vertex([V-Neibs|Graph], K, Comp, Pair0, Rank0, Pair) :-
	evaluate_vertex(Neibs, 0, Rank),
	(   Rank < K -> Comp = <, Pair = V-Neibs
	;   Rank @> Rank0 ->
	    select_vertex(Graph, K, Comp, V-Neibs, Rank, Pair)
	;   select_vertex(Graph, K, Comp, Pair0, Rank0, Pair)
	).

evaluate_vertex([V|Neibs], Deg, Rank) :- var(V), !,
	Deg1 is Deg+1,
	evaluate_vertex(Neibs, Deg1, Rank).
evaluate_vertex(Neibs, Deg, Sat+Deg) :-
	/*prolog:*/length(Neibs, 0, Sat).

add_color([], _, _, []).
add_color([V-Neibs|Graph], [W|Vs], C, [V-Neibs1|Graph1]) :- V==W, !,
	my_ord_add_element(Neibs, C, Neibs1),
	add_color(Graph, Vs, C, Graph1).
add_color([Pair|Graph], Vs, C, [Pair|Graph1]) :-
	add_color(Graph, Vs, C, Graph1).

vertex_color([V|Vs], I, M, Color) :- V@<I, !,
	vertex_color(Vs, I, M, Color).
vertex_color([I|Vs], I, M, Color) :- !,
	I<M, J is I+1, vertex_color(Vs, J, M, Color).
vertex_color(_, I, _, I).
vertex_color(Vs, I, M, Color) :-
	I<M, J is I+1, vertex_color(Vs, J, M, Color).

color_stack([]).
color_stack([V-Neibs|Stk]) :- sort(Neibs, Set), color_stack(Set, 1, V, Stk).

color_stack([I|Is], I, V, Stk) :- !, J is I+1, color_stack(Is, J, V, Stk).
color_stack(_, V, V, Stk) :- color_stack(Stk).

color_map([], []).
color_map([V-_|Graph], [V-_|Coloring]) :- color_map(Graph, Coloring).

color_map([], [], [], _).
color_map([V-Ns|Graph], [C-Ns1|Graph1], [V-C|Cols], Coloring) :-
	map_colors(Ns, Coloring, Ns1),
	color_map(Graph, Graph1, Cols, Coloring).

map_colors([], _, []).
map_colors(Ns, Coloring, Ns1) :-
	Ns = [X|_],
	Coloring = [V-_|_],
	compare(C, X, V),
	map_colors(C, Ns, Coloring, Ns1).

map_colors(=, [_|Xs], [_-Y|Coloring], [Y|Ns1]) :-
	map_colors(Xs, Coloring, Ns1).
map_colors(>, Ns, [_|Coloring], Ns1) :-
	map_colors(Ns, Coloring, Ns1).
	

/* Copyright(C) 1988, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   File   : ORDSETS.plL							      %
%   Maintainer : Lena Flood						      %
%   Updated: 9 September 1988						      %
%   Purpose: Ordered set manipulation utilities				      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		  /*
		  
:- module(ordsets, [
	is_ordset/1,
	list_to_my_ord_set/2,
	my_ord_add_element/3,
	my_ord_del_element/3,
	my_ord_disjoint/2,
	my_ord_intersect/2,
	my_ord_intersection/3,
	my_ord_intersection/4,
	my_ord_intersection/2,
	my_ord_member/2,
	my_ord_seteq/2,
	my_ord_setproduct/3,
	my_ord_subset/2,
	my_ord_subtract/3,
	my_ord_symdiff/3,
	my_ord_union/3,
	my_ord_union/4,
	my_ord_union/2
		   ]).
				    */
%   Adapted from shared code written by Richard A O'Keefe. 

%   In this package, sets are represented by ordered lists with no
%   duplicates.	 Thus {c,r,a,f,t} would be [a,c,f,r,t].	 The ordering
%   is defined by the @< family of term comparison predicates, which
%   is the ordering used by sort/2 and setof/3.

%   The benefit of the ordered representation is that the elementary
%   set operations can be done in time proportional to the Sum of the
%   argument sizes rather than their Product.  



%   is_ordset(+Set)
%   is true when Set is an ordered set.

is_ordset(X) :- var(X), !, fail.
is_ordset([]).
is_ordset([Head|Tail]) :-
	is_ordset(Tail, Head).

is_ordset(X, _) :- var(X), !, fail.
is_ordset([], _).
is_ordset([Head|Tail], Left) :-
	Left @< Head,
	is_ordset(Tail, Head).


%   list_to_my_ord_set(+List, ?Set)
%   is true when Set is the ordered representation of the set represented
%   by the unordered representation List.  

list_to_my_ord_set(List, Set) :-
	sort(List, Set).


%   my_ord_add_element(+Set1, +Element -Set2)
%   is true when Set2 is Set1 with Element inserted in it, preserving
%   the order.

my_ord_add_element([], Element, [Element]).
my_ord_add_element([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	my_ord_add_element(Order, Head, Tail, Element, Set).

my_ord_add_element(<, Head, Tail, Element, [Head|Set]) :-
	my_ord_add_element(Tail, Element, Set).
my_ord_add_element(=, Head, Tail, _, [Head|Tail]).
my_ord_add_element(>, Head, Tail, Element, [Element,Head|Tail]).


%   my_ord_del_element(+Set1, +Element, ?Set2)
%   is true when Set2 is Set1 but with Element removed.

my_ord_del_element([], _, []).
my_ord_del_element([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	my_ord_del_element(Order, Head, Tail, Element, Set).

my_ord_del_element(<, Head, Tail, Element, [Head|Set]) :-
	my_ord_del_element(Tail, Element, Set).
my_ord_del_element(=, _, Tail, _, Tail).
my_ord_del_element(>, Head, Tail, _, [Head|Tail]).



%   my_ord_disjoint(+Set1, +Set2)
%   is true when the two ordered sets have no element in common.  

my_ord_disjoint(Set1, Set2) :-
	\+ my_ord_intersect(Set1, Set2).



%   my_ord_intersect(+Set1, +Set2)
%   is true when the two ordered sets have at least one element in common.

my_ord_intersect([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	my_ord_intersect(Order, Head1, Tail1, Head2, Tail2).

my_ord_intersect(<, _, [Head1|Tail1], Head2, Tail2) :-
	compare(Order, Head1, Head2),
	my_ord_intersect(Order, Head1, Tail1, Head2, Tail2).
my_ord_intersect(=, _, _, _, _).
my_ord_intersect(>, Head1, Tail1, _, [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	my_ord_intersect(Order, Head1, Tail1, Head2, Tail2).



%   my_ord_intersection(+Set1, +Set2, ?Intersection)
%   is true when Intersection is the intersecton of Set1
%   and Set2, provided that Set1 and Set2 are ordered sets.

my_ord_intersection([], _, []).
my_ord_intersection([Head1|Tail1], Set2, Intersection) :-
	my_ord_intersection3(Set2, Head1, Tail1, Intersection).

my_ord_intersection3(<, _, Set1, Head2, Tail2, Intersection) :-
	my_ord_intersection3(Set1, Head2, Tail2, Intersection).
my_ord_intersection3(=, Head, Tail1, _, Tail2, [Head|Intersection]) :-
	my_ord_intersection(Tail1, Tail2, Intersection).
my_ord_intersection3(>, Head1, Tail1, _, Set2, Intersection) :-
	my_ord_intersection3(Set2, Head1, Tail1, Intersection).

% could be a disjunction, but is used in three places
my_ord_intersection3([], _, _, []).
my_ord_intersection3([Head2|Tail2], Head1, Tail1, Intersection) :-
	compare(Order, Head1, Head2),
	my_ord_intersection3(Order, Head1, Tail1, Head2, Tail2, Intersection).



%   my_ord_intersection(+Set1, +Set2, ?Intersection, ?Difference)
%   is true when Intersection is the intersection of Set1 and Set2, 
%   and Differens is Set2 \ Set1 (like in my_ord_union/4),
%    provided that Set1 and Set2 are ordered sets.

my_ord_intersection([], Set2, [], Set2).
my_ord_intersection([Head1|Tail1], Set2, Intersection, Difference) :-
	my_ord_intersection4(Set2, Head1, Tail1, Intersection, Difference).

my_ord_intersection4(<, _, Set1, Head2, Tail2, Intersection, Difference) :-
	(   Set1 = [], Intersection = [], Difference = [Head2|Tail2]
	;   Set1 = [Head1|Tail1],
	    compare(Order, Head1, Head2),
	    my_ord_intersection4(Order, Head1, Tail1, Head2, Tail2, Intersection, Difference)
	).
my_ord_intersection4(=, Head, Tail1, _, Tail2, [Head|Intersection], Difference) :-
	my_ord_intersection(Tail1, Tail2, Intersection, Difference).
my_ord_intersection4(>, Head1, Tail1, Head2, Set2, Intersection, [Head2|Difference]) :-
	my_ord_intersection4(Set2, Head1, Tail1, Intersection, Difference).

my_ord_intersection4([], _, _, [], []).
my_ord_intersection4([Head2|Tail2], Head1, Tail1, Intersection, Difference) :-
	compare(Order, Head1, Head2),
	my_ord_intersection4(Order, Head1, Tail1, Head2, Tail2, Intersection, Difference).



%   my_ord_intersection(+Sets, ?Intersection)
%   is true when Intersection is the ordered set representation of the
%   intersection of all the sets in Sets.

my_ord_intersection(Sets, Intersection) :- 
	length(Sets, NumberOfSets),
	NumberOfSets > 0,
	my_ord_intersection2(NumberOfSets, Sets, Intersection, []).

my_ord_intersection2(1, [Set|Sets], Set0, Sets0) :- !,
	Set = Set0,
	Sets = Sets0.
my_ord_intersection2(2, [Set,Set2|Sets], Intersection, Sets0) :- !,
	Sets = Sets0,
	my_ord_intersection(Set, Set2, Intersection).
my_ord_intersection2(N, Sets0, Intersection, Sets) :-
%	N > 2,
	A is N>>1,
	Z is N-A,
	my_ord_intersection2(A, Sets0, X, Sets1),
	my_ord_intersection2(Z, Sets1, Y, Sets),
	my_ord_intersection(X, Y, Intersection).



%   my_ord_member(+Elt, +Set)
%   is true when Elt is a member of Set.  Suggested by Mark Johnson.

my_ord_member(X, [E|Es]) :-
        compare(C, X, E),
        my_ord_member(C, X, Es).

my_ord_member(=, _X, _Es).
my_ord_member(>, X, [E|Es]) :-
        compare(C, X, E),
        my_ord_member(C, X, Es).



%   my_ord_seteq(+Set1, +Set2)
%   is true when the two arguments represent the same set.  Since they
%   are assumed to be ordered representations, they must be identical.


my_ord_seteq(Set1, Set2) :-
	Set1 == Set2.


%   my_ord_setproduct(+Set1, +Set2, ?SetProduct)
%   is true when SetProduct is the cartesian product of Set1 and Set2. The
%   product is represented as pairs Elem1-Elem2, where Elem1 is an element
%   from Set1 and Elem2 is an element from Set2.

my_ord_setproduct([], _, []).
my_ord_setproduct([Head|Tail], Set, SetProduct)  :-
	my_ord_setproduct(Set, Head, SetProduct, Rest),
	my_ord_setproduct(Tail, Set, Rest).

my_ord_setproduct([], _, Set, Set).
my_ord_setproduct([Head|Tail], X, [X-Head|TailX], Tl) :-
	my_ord_setproduct(Tail, X, TailX, Tl).



%   my_ord_subset(+Set1, +Set2)
%   is true when every element of the ordered set Set1 appears in the
%   ordered set Set2.

my_ord_subset([], _).
my_ord_subset([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	my_ord_subset(Order, Head1, Tail1, Tail2).

my_ord_subset(=, _, Tail1, Tail2) :-
	my_ord_subset(Tail1, Tail2).
my_ord_subset(>, Head1, Tail1, [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	my_ord_subset(Order, Head1, Tail1, Tail2).



%   my_ord_subtract(+Set1, +Set2, ?Difference)
%   is true when Difference contains all and only the elements of Set1
%   which are not also in Set2, i.e. Set1 \ Set2.

my_ord_subtract(Set1, Set2, Union) :-
	/*prolog:*/subtract(Set1, Set2, Union).

/***
subtract([], _, []).
subtract([Head1|Tail1], Set2, Difference) :-
	subtract3(Set2, Head1, Tail1, Difference).

subtract3(<, Head, Set1, Head2, Tail2, [Head|Difference]) :-
	(   Set1 = [], Difference = []
	;   Set1 = [Head1|Tail1],
	    compare(Order, Head1, Head2),
	    subtract3(Order, Head1, Tail1, Head2, Tail2, Difference)
	).
subtract3(=, _, Tail1, _, Tail2, Difference) :-
	subtract(Tail1, Tail2, Difference).
subtract3(>, Head1, Tail1, _, Set2, Difference) :-
	subtract3(Set2, Head1, Tail1, Difference).

subtract3([], Head1, Tail1, [Head1|Tail1]).
subtract3([Head2|Tail2], Head1, Tail1, Difference) :-
	compare(Order, Head1, Head2),
	subtract3(Order, Head1, Tail1, Head2, Tail2, Difference).
***/


%   my_ord_symdiff(+Set1, +Set2, ?Difference)
%   is true when Difference is the symmetric difference of Set1 and Set2.

my_ord_symdiff([], Set2, Set2).
my_ord_symdiff([Head1|Tail1], Set2, Symdiff) :-
	my_ord_symdiff(Set2, Head1, Tail1, Symdiff).

my_ord_symdiff(<, Head1, Set1, Head2, Tail2, [Head1|Symdiff]) :-
	my_ord_symdiff(Set1, Head2, Tail2, Symdiff).
my_ord_symdiff(=, _, Tail1, _, Tail2, Symdiff) :-
	my_ord_symdiff(Tail1, Tail2, Symdiff).
my_ord_symdiff(>, Head1, Tail1, Head2, Set2, [Head2|Symdiff]) :-
	my_ord_symdiff(Set2, Head1, Tail1, Symdiff).

% could be a disjunction, but is used in three places
my_ord_symdiff([], Head1, Tail1, [Head1|Tail1]).
my_ord_symdiff([Head2|Tail2], Head1, Tail1, Symdiff) :-
	compare(Order, Head1, Head2),
	my_ord_symdiff(Order, Head1, Tail1, Head2, Tail2, Symdiff).



%   my_ord_union(+Set1, +Set2, ?Union)
%   is true when Union is the union of Set1 and Set2.  Note that when
%   something occurs in both sets, we want to retain only one copy.

my_ord_union(Set1, Set2, Union) :-
	/*prolog:*/merge(Set1, Set2, Union).

/***
merge([], Set, Set).
merge([O|Os], Ns, Set) :- merge(Ns, O, Os, Set).

merge([], O, Os, [O|Os]).
merge([N|Ns], O, Os, Set) :-
	compare(C, O, N), 
	merge(C, O, Os, N, Ns, Set).

merge(<, O1, Os, N, Ns, [O1|Set]) :- merge(Os, N, Ns, Set).
merge(=, _, Os, N, Ns, [N|Set]) :- merge(Os, Ns, Set).
merge(>, O, Os, N1, Ns, [N1|Set]) :- merge(Ns, O, Os, Set).
***/



%   my_ord_union(+Set1, +Set2, ?Union, ?New)
%   is true when Union is the union of Set1 and Set2, and New is
%   Set2 \ Set1.  This is useful if you
%   are accumulating members of a set and you want to process new
%   elements as they are added to the set.

my_ord_union([], Set2, Set2, Set2).
my_ord_union([Head1|Tail1], Set2, Union, Difference) :-
	my_ord_union4(Set2, Head1, Tail1, Union, Difference).

my_ord_union4(<, Head, Set1, Head2, Tail2, [Head|Union], Difference) :-
	(   Set1 = [], Union = [Head2|Tail2], Difference = [Head2|Tail2]
	;   Set1 = [Head1|Tail1],
	    compare(Order, Head1, Head2),
	    my_ord_union4(Order, Head1, Tail1, Head2, Tail2, Union, Difference)
	).
my_ord_union4(=, Head, Tail1, _, Tail2, [Head|Union], Difference) :-
	my_ord_union(Tail1, Tail2, Union, Difference).
my_ord_union4(>, Head1, Tail1, Head2, Set2, [Head2|Union], [Head2|Difference]) :-
	my_ord_union4(Set2, Head1, Tail1, Union, Difference).

my_ord_union4([], Head1, Tail1, [Head1|Tail1], []).
my_ord_union4([Head2|Tail2], Head1, Tail1, Union, Difference) :-
	compare(Order, Head1, Head2),
	my_ord_union4(Order, Head1, Tail1, Head2, Tail2, Union, Difference).



%   my_ord_union(+Sets, ?Union) 
%   is true when Union is the union of all the sets in Sets. 

my_ord_union([], Union) :- !, Union = [].
my_ord_union(Sets, Union) :-
	length(Sets, NumberOfSets),
	my_ord_union_all(NumberOfSets, Sets, Union, []).

my_ord_union_all(1, [Set|Sets], Set, Sets) :- !.
my_ord_union_all(2, [Set,Set2|Sets], Union, Sets) :- !,
	my_ord_union(Set, Set2, Union).
my_ord_union_all(N, Sets0, Union, Sets) :-
	A is N>>1,
	Z is N-A,
	my_ord_union_all(A, Sets0, X, Sets1),
	my_ord_union_all(Z, Sets1, Y, Sets),
	my_ord_union(X, Y, Union).



%   Copyright(C) 1994, Swedish Institute of Computer Science

%   File       : ASSOC.plL
%   Maintainer : Mats Carlsson
%   Updated    : 15 December 1994
%   Purpose    : AVL tree implementation of "association lists".
	     /*
:- module(assoc, [
	assoc_to_list/2,		% Assoc -> List
	empty_assoc/1,			% -> Assoc
	del_assoc/4,			% Key x Assoc x Val -> Assoc
	del_max_assoc/4,		% Assoc -> Key x Val x Assoc
	del_min_assoc/4,		% Assoc -> Key x Val x Assoc
	gen_assoc/3,			% Key x Assoc x Val
	get_assoc/3,			% Key x Assoc -> Val
	get_assoc/5,			% Key x Assoc x Val -> Assoc x Val
	get_next_assoc/4,		% Key x Assoc -> Key x Val
	get_prev_assoc/4,		% Key x Assoc -> Key x Val
	is_assoc/1,			% Assoc ->
	list_to_assoc/2,		% List -> Assoc
	map_assoc/2,			% Goal x Assoc ->
	map_assoc/3,			% Goal x Assoc -> Assoc
	max_assoc/3,			% Assoc -> Key x Val
	min_assoc/3,			% Assoc -> Key x Val
	ord_list_to_assoc/2,		% List -> Assoc
	put_assoc/4			% Key x Assoc x Val -> Assoc
   ]).

:- meta_predicate
	map_assoc(:, ?),
	map_assoc(:, ?, ?).

:- use_module(library(lists), [
	append/3
			      ]).

		    */
%   Adapted from shared assoc.pll, which used binary trees,
%   written by Richard A O'Keefe.

%   In this package, finite functions are represented by AVL trees, i.e.
%   they are subject to the Adelson-Velskii-Landis balance criterion:
%   
%     A tree is balanced iff for every node the heights of its
%     two subtrees differ by at most 1.
%   
%   The empty tree is represented as t.
%   A tree with key K, value V, and left and right subtrees L and R is
%   represented as t(K,V,|R|-|L|,L,R).
%   |T| denotes the height of T.
%   
%   The advantage of this representation is that lookup, insertion and
%   deletion all become - in the worst case - O(log n) operations.
%   
%   The algorithms are due to Wirth, "Algorithms + Data Structures =
%   Programs", 4.4.6 - 4.4.8.



%   empty_assoc(?Assoc)
%   is true when Assoc is an empty AVL tree.

%empty_assoc(t).					% also in Compiler/comp_sup.pll



%   assoc_to_list(+Assoc, ?List)
%   assumes that Assoc is a proper AVL tree, and is true when
%   List is a list of Key-Value pairs in ascending order with no
%   duplicate keys specifying the same finite function as Assoc.
%   Use this to convert an Assoc to a list.

/*
assoc_to_list(Assoc, List) :-
	/*prolog:*/assoc_to_list(Assoc, List, []).	% in Compiler/comp_sup.pll
*/

assoc_to_list(t) --> [].
assoc_to_list(t(K,V,_,L,R)) -->
	assoc_to_list(L),
	[K-V],
	assoc_to_list(R).



%   is_assoc(+Assoc)
%   is true when Assoc is a (proper) AVL tree.  It checks both that the keys 
%   are in ascending order and that Assoc is properly balanced.

is_assoc(Assoc) :-
	is_assoc(Assoc, nokey, _, _).

is_assoc(-, _, _, _) :- !, fail.
is_assoc(t, Min, Min, 0).
is_assoc(t(Key,_,B,L,R), Min0, Max, Height) :-
	Min = key(Key),
	is_assoc(L, Min0, Mid, HeightL),
	Mid @< Min,
	is_assoc(R, Min, Max, HeightR),
	B is HeightR-HeightL,
	(   HeightL < HeightR -> Height is HeightR+1
	;   Height is HeightL+1
	).



%   min_assoc(+Assoc, ?Key, ?Val)
%   is true when Key is the smallest key in Assoc and Val is its value.

min_assoc(t(K,V,_,L,_), Key, Val) :-
	min_assoc(L, Key, Val, K, V).

min_assoc(t, K, V, K, V).
min_assoc(t(K,V,_,L,_), Key, Val, _, _) :-
	min_assoc(L, Key, Val, K, V).



%   max_assoc(+Assoc, ?Key, ?Val)
%   is true when Key is the greatest key in Assoc and Val is its value.

max_assoc(t(K,V,_,_,R), Key, Val) :-
	max_assoc(R, Key, Val, K, V).

max_assoc(t, K, V, K, V).
max_assoc(t(K,V,_,_,R), Key, Val, _, _) :-
	max_assoc(R, Key, Val, K, V).



%   gen_assoc(?Key, +Assoc, ?Value)
%   assumes that Assoc is a proper AVL tree, and is true when
%   Key is associated with Value in Assoc.  Can be used to enumerate
%   all Values by ascending Keys.

/*
gen_assoc(Key, Assoc, Value) :-
	/*prolog:*/gen_assoc(Key, Assoc, Value).	% in Compiler/comp_sup.pll
*/


gen_assoc(Key, t(K,V,_,L,R), Val) :-
	(   gen_assoc(Key, L, Val)
	;   Key = K, Val = V
	;   gen_assoc(Key, R, Val)
	).


%   get_assoc(+Key, +Assoc, ?Value)
%   assumes that Assoc is a proper AVL tree.  It is true when
%   Key is identical to (==) one of the keys in Assoc, and Value
%   unifies with the associated value.

/*
get_assoc(Key, Assoc, Value) :-
	/*prolog:*/get_assoc(Key, Assoc, Value).	% in Compiler/comp_sup.pll
*/
/*
get_assoc(Key, t(K,V,_,L,R), Val) :-
	compare(Rel, Key, K),
	get_assoc(Rel, Key, Val, V, L, R).

get_assoc(<, Key, Val, _, Tree, _) :- get_assoc(Key, Tree, Val).
get_assoc(=, _, Val, Val, _, _).
get_assoc(>, Key, Val, _, _, Tree) :- get_assoc(Key, Tree, Val).

  */

%   get_assoc(+Key, +OldAssoc, ?OldValue, ?NewAssoc, ?NewValue)
%   is true when OldAssoc and NewAssoc are AVL trees of the same
%   shape having the same elements except that the value for Key in
%   OldAssoc is OldValue and the value for Key in NewAssoc is NewValue.

/*
get_assoc(Key, OldAssoc, OldValue, NewAssoc, NewValue) :-
	/*prolog:*/get_assoc(Key, OldAssoc, OldValue, NewAssoc, NewValue).	% in Compiler/comp_sup.pll

*/
/*
get_assoc(Key, t(K0,V0,B,L0,R0), Val0, t(K,V,B,L,R), Val) :-
	compare(Rel, Key, K0),
	get_assoc(Rel, Key, K0, V0, L0, R0, Val0, K, V, L, R, Val).

get_assoc(<, Key, K, V, Tree0, R, Val0, K, V, Tree, R, Val) :-
	get_assoc(Key, Tree0, Val0, Tree, Val).
get_assoc(=, _, K, Val0, L, R, Val0, K, Val, L, R, Val).
get_assoc(>, Key, K, V, L, Tree0, Val0, K, V, L, Tree, Val) :-
	get_assoc(Key, Tree0, Val0, Tree, Val).
*/




%   get_next_assoc(+Key, +Assoc, ?Knext, ?Vnext)
%   is true when Knext and Vnext is the next key and associated value 
%   after Key in Assoc.

get_next_assoc(Key0, t(K,V,_,L,R), Key, Val) :-
	(   K @=< Key0 ->
	    get_next_assoc(Key0, R, Key, Val)
	;   get_next_assoc(Key0, L, K1, V1) ->
	    Key = K1, Val = V1
	;   Key = K,  Val = V
	).



%   get_prev_assoc(+Key, +Assoc, ?Kprev, ?Vprev)
%   is true when Kprev and Vprev is the previous key and associated value 
%   to Key in Assoc.

get_prev_assoc(Key0, t(K,V,_,L,R), Key, Val) :-
	(   K @>= Key0 ->
	    get_prev_assoc(Key0, L, Key, Val)
	;   get_prev_assoc(Key0, R, K1, V1) ->
	    Key = K1, Val = V1
	;   Key = K,  Val = V
	).


%   ord_list_to_assoc(+List, ?Assoc)
%   is true when List is a proper list of Key-Val pairs (keysorted)
%   and Assoc is an association tree specifying the same finite function
%   from Keys to Values.

ord_list_to_assoc(List, Assoc) :-
	length(List, N),
	ord_list_to_assoc(N, List, [], Assoc).


ord_list_to_assoc(0, List, List, t) :- !.
ord_list_to_assoc(1, [Key-Val|List], List, t(Key,Val,0,t,t)) :- !.
ord_list_to_assoc(N, List0, List, t(Key,Val,Bal,L,R)) :-
	Bal is msb(N)-msb(N-1),
	A is (N-1) >> 1,
	Z is (N-1) - A,
	ord_list_to_assoc(A, List0, [Key-Val|List1], L),
	ord_list_to_assoc(Z, List1, List, R).


:-arithmetic_function(msb/1).

%msb(X,Y):-Y is integer(X/3)+1. 

msb(X,Y):-Y is integer(log(X)). 
%msb(X,Y):-Y is integer(log(2,X)). 

%   list_to_assoc(+List, ?Assoc)
%   is true when List is a proper list of Key-Val pairs (in any order)
%   and Assoc is an association tree specifying the same finite function
%   from Keys to Values.

list_to_assoc(Pairs, Assoc) :-
	list_to_assoc(Pairs, t, Assoc).


list_to_assoc([], Assoc, Assoc).
list_to_assoc([K-V|Pairs], Assoc0, Assoc) :-
	put_assoc(K, Assoc0, V, Assoc1),
	list_to_assoc(Pairs, Assoc1, Assoc).



%   put_assoc(+Key, +OldAssoc, +Val, -NewAssoc)
%   is true when OldAssoc and NewAssoc define the same finite function
%   except that NewAssoc associates Val with Key.  OldAssoc need not have
%   associated any value at all with Key.  
/* 

see sigma_ale.pl 
put_assoc(Key, Assoc0, Val, Assoc1) :-
	put_assoc(Assoc0, Key, Val, Assoc1, _).	% in Compiler/comp_sup.pll

put_assoc(t,            Key, Val, t(Key,Val,0,t,t), 1).
put_assoc(t(K,V,B,L,R), Key, Val, Result, Delta) :-
	compare(O, Key, K),
	put_assoc(O, Key, Val, Result, Delta, K, V, B, L, R).
		     */

put_assoc(<, Key, Val, Assoc, Delta, K, V, B, L, R) :-
	put_assoc(L, Key, Val, Lassoc, D1),
	Delta is \(B) /\ D1,			% grew?
	B1 is B-D1,
	assoc(B1, K, V, Lassoc, R, Assoc).
put_assoc(=, Key, Val, t(Key,Val,B,L,R), 0, _, _, B, L, R).
put_assoc(>, Key, Val, Assoc, Delta, K, V, B, L, R) :-
	put_assoc(R, Key, Val, Rassoc, D1),
	Delta is \(B) /\ D1,			% grew?
	B1 is B+D1,
	assoc(B1, K, V, L, Rassoc, Assoc).



assoc(-2, K, V, L, R, Assoc) :-
	L = t(K1,V1,B1,L1,R1),
	assoc_left(B1, K1, V1, L1, R1, K, V, R, Assoc).
assoc(-1, K, V, L, R, t(K,V,-1,L,R)).
assoc( 0, K, V, L, R, t(K,V, 0,L,R)).
assoc( 1, K, V, L, R, t(K,V, 1,L,R)).
assoc( 2, K, V, L, R, Assoc) :-
	R = t(K1,V1,B1,L1,R1),
	assoc_right(B1, K1, V1, L1, R1, K, V, L, Assoc).

assoc_left(-1, K1, V1, L1, R1, K, V, R,		% single LL rotation
	    t(K1,V1, 0,L1,t(K,V, 0,R1,R))).
assoc_left( 0, K1, V1, L1, R1, K, V, R,		% single LL rotation
	    t(K1,V1, 1,L1,t(K,V,-1,R1,R))).
assoc_left( 1, K1, V1, L1, R1, K, V, R,		% double LR rotation
	    t(K2,V2, 0,t(K1,V1,BK1,L1,L2),t(K,V,BK,R2,R))) :-
        R1 = t(K2,V2,B2,L2,R2),
	assoc(B2, BK1, BK).

assoc_right( 1, K1, V1, L1, R1, K, V, L,	% single RR rotation
	     t(K1,V1, 0,t(K,V, 0,L,L1),R1)).
assoc_right( 0, K1, V1, L1, R1, K, V, L,	% single RR rotation
	     t(K1,V1,-1,t(K,V, 1,L,L1),R1)).
assoc_right(-1, K1, V1, L1, R1, K, V, L,	% double RL rotation
	     t(K2,V2, 0,t(K,V,BK,L,L2),t(K1,V1,BK1,R2,R1))) :-
        L1 = t(K2,V2,B2,L2,R2),
	assoc(B2, BK, BK1).

assoc(-1,  0, 1).
assoc( 0,  0, 0).
assoc( 1, -1, 0).


%   del_assoc(+Key, +OldAssoc, ?Val, -NewAssoc)
%   is true when OldAssoc and NewAssoc define the same finite function
%   except that OldAssoc associates Key with Val and NewAssoc doesn't
%   associate Key with any value.

del_assoc(Key, Assoc0, Val, Assoc) :-
	del_assoc(Assoc0, Key, Val, Assoc, _).

del_assoc(t(K,V,B,L,R), Key, Val, Assoc, Delta) :-
	compare(C, Key, K),
	del_assoc(C, Key, Val, Assoc, Delta, K, V, B, L, R).

del_assoc(<, Key, Val, Assoc, Delta, K, V, B, L, R) :-
	del_assoc(L, Key, Val, L1, D1),
        B1 is B+D1,
	/*prolog:*/assoc(B1, K, V, L1, R, Assoc),
	assoc_shrinkage(Assoc, D1, Delta).
del_assoc(=, _, Val, Assoc, Delta, _, Val, B, L, R) :-
	(   L == t -> Assoc = R, Delta = 1
	;   R == t -> Assoc = L, Delta = 1
	;   del_max_assoc(L, K, V, L1, D1),
	    B1 is B+D1,
	    /*prolog:*/assoc(B1, K, V, L1, R, Assoc),
	    assoc_shrinkage(Assoc, D1, Delta)
	).
del_assoc(>, Key, Val, Assoc, Delta, K, V, B, L, R) :-
	del_assoc(R, Key, Val, R1, D1),
	B1 is B-D1,
	/*prolog:*/assoc(B1, K, V, L, R1, Assoc),
	assoc_shrinkage(Assoc, D1, Delta).


%   del_min_assoc(+OldAssoc, ?Key, ?Val, -NewAssoc)
%   is true when OldAssoc and NewAssoc define the same finite function
%   except that OldAssoc associates Key with Val and NewAssoc doesn't
%   associate Key with any value and Key precedes all other keys in OldAssoc.

del_min_assoc(Assoc0, Key, Val, Assoc) :-
	del_min_assoc(Assoc0, Key, Val, Assoc, _).

del_min_assoc(t(K,V,B,L,R), Key, Val, Assoc, Delta) :-
	(   L == t ->
	    Assoc = R, Key = K, Val = V, Delta = 1
	;   del_min_assoc(L, Key, Val, L1, D1),
	    B1 is B+D1,
	    %/*prolog:*/
	    assoc(B1, K, V, L1, R, Assoc),
	    assoc_shrinkage(Assoc, D1, Delta)
	).



%   del_max_assoc(+OldAssoc, ?Key, ?Val, -NewAssoc)
%   is true when OldAssoc and NewAssoc define the same finite function
%   except that OldAssoc associates Key with Val and NewAssoc doesn't
%   associate Key with any value and 
%   Key is preceded by all other keys in OldAssoc.

del_max_assoc(Assoc0, Key, Val, Assoc) :-
	del_max_assoc(Assoc0, Key, Val, Assoc, _).

del_max_assoc(t(K,V,B,L,R), Key, Val, Assoc, Delta) :-
	(   R == t ->
	    Assoc = L, Key = K, Val = V, Delta = 1
	;   del_max_assoc(R, Key, Val, R1, D1),
	    B1 is B-D1,
	    /*prolog:*/assoc(B1, K, V, L, R1, Assoc),
	    assoc_shrinkage(Assoc, D1, Delta)
	).


assoc_shrinkage(t(_,_,B,_,_), D1, Delta) :-
	Delta is \(B) /\ D1.		% this shrank iff L/R shrank and
					% this became balanced

%   map_assoc(:Pred, ?Assoc)
%   is true when Assoc is an association tree, and for each Key, 
%   if Key is associated with Value in Assoc, Pred(Value) is true.

map_assoc(MPred, Assoc) :-
	/*prolog:*/get_module(MPred, Pred, M),
	map_assoc_1(Assoc, M, Pred).

map_assoc_1(t, _, _).
map_assoc_1(t(_,Val,_,L,R), M, Pred) :-
	map_assoc_1(L, M, Pred),
	add_arguments(Pred, [Val], Goal),
	M:Goal,
	map_assoc_1(R, M, Pred).



%   map_assoc(:Pred, ?OldAssoc, ?NewAssoc)
%   is true when OldAssoc and NewAssoc are association trees of the
%   same shape, and for each Key, if Key is associated with Old in
%   OldAssoc and with New in NewAssoc, Pred(Old,New) is true.

map_assoc(MPred, OldAssoc, NewAssoc) :-
	/*prolog:*/get_module(MPred, Pred, M),
	map_assoc_1(OldAssoc, NewAssoc, M, Pred).

map_assoc_1(t, t, _, _).
map_assoc_1(t(Key,Old,B,L0,R0), t(Key,New,B,L1,R1), M, Pred) :-
	map_assoc_1(L0, L1, M, Pred),
	add_arguments(Pred, [Old,New], Goal),
	M:Goal,
	map_assoc_1(R0, R1, M, Pred).

add_arguments(Goal, Args, Goal1) :-
	Goal =.. GoalList,
	append(GoalList, Args, GoalList1),
	Goal1 =.. GoalList1.




