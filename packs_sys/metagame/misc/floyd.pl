%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% floyd.pl
%%% Barney Pell
%%% University of Cambridge
%%% 1992

/* From graphs.pl: 
    The P-representation of a graph is a list of (from-to) vertex
    pairs, where the pairs can be in any old order.  This form is
    convenient for input/output.

    The S-representation of a graph is a list of (vertex-neighbours)
    pairs, where the pairs are in standard order (as produced by
    keysort) and the neighbours of each vertex are also in standard
    order (as produced by sort).  This form is convenient for many
    calculations.

    New addition:  

    The D-representation of a weighted digraph is a list of
    (vertex-neighbours) pairs, where the pairs are in standard order (as
    produced by keysort) and the neighbours of each vertex are also in
    standard order (as produced by sort).  Unlike the S-representation,
    though, each neighbour is a (vertex-weight) pair, which thus weights
    the edge from non-weighted S-representation of a graph.

    s_to_d_graph(SForm, Dform) converts an S-rep to a D-rep
    by giving each edge a weight of 1. 
    d_to_s_graph(SForm, Dform) converts a D-rep to S-rep by 
    dropping the weights. 

    s_floyd(Graph,Closure) computes the reachability matrix for a 
    graph in S-form, where the closure is in D-form.
    (NB: this is not the reflexive transitive closure).

    floyd(Graph,Closure) computes the all-pairs shortest-path distance
    matrix for a weighted digraph in D-form, with the closure also in D-form.
    (NB: this is not the reflexive transitive closure).

*/

s_to_d_graph([], []) :- !.
s_to_d_graph([Vertex-Neibs1|G], [Vertex-Neibs2|D]) :-
	init_dists(Neibs1,Neibs2),
	s_to_d_graph(G,D).

init_dists([],[]).
init_dists([H|T],[H-1|Ts]) :- 
	init_dists(T,Ts).
	
d_to_s_graph([], []) :- !.
d_to_s_graph([Vertex-Neibs1|G], [Vertex-Neibs2|D]) :-
	strip_dists(Neibs1,Neibs2),
	d_to_s_graph(G,D).

strip_dists([],[]).
strip_dists([H-_|T],[H|Ts]) :- 
	strip_dists(T,Ts).


s_floyd(S_Graph,Closure) :-
	s_to_d_graph(S_Graph,Graph),
	floyd(Graph,Closure).

% Replaces all diagonal entries with 0. 
zero_self_d_graph([], []) :- !.
zero_self_d_graph([Vertex-Neibs1|G], [Vertex-Neibs2|D]) :-
	ord_min_union([Vertex-0],Neibs1,Neibs2),
	zero_self_d_graph(G,D).


% FLOYD(+Graph, -Closure)
% =======================
% is true when Closure is the all-pairs-shortest-path solution
% to the weighted digraph Graph. 
%
% Graph is a weighted-digraph:
% [V_1-Edges_1,...,V_n-Edges_n]
% Each Edges is a list of weighted vertices:
% EdgesI = [V_j-Weight_ij
% 
% 
% Result is a new weighted-digraph, where W_ij is the 
% weight of the shortest path from V_i to V_j in the original
% graph.  
%
% This is O(N**3).
% This was based on Richard O'Keefe's implementation of warshall/2,
% and the description of Floyd's algorithm given in AHO.

floyd(Graph, Closure) :-
	zero_self_d_graph(Graph,Init),
	floyd(Init, Init, Closure).

floyd([], Closure, Closure) :- !.
floyd([V-_|G], E, Closure) :-
	memberchk(V-Y, E),	%  Y := E(v)
	floyd(E, V, Y, NewE),
	floyd(G, NewE, Closure).

floyd([X-Neibs|G], V, Y, [X-NewNeibs|NewG]) :-
	memberchk(V-VDist, Neibs),
	!,
	increment_dists(Y,VDist,YInc),
	ord_min_union(Neibs, YInc, NewNeibs),
	floyd(G, V, Y, NewG).
floyd([X-Neibs|G], V, Y, [X-Neibs|NewG]) :- !,
	floyd(G, V, Y, NewG).
floyd([], _, _, []).

increment_dists([],_,[]).
increment_dists([H-D|T],N,[H-D1|T1]) :- 
	D1 is D+N,
	increment_dists(T,N,T1).


%   ord_min_union(+Set1, +Set2, ?Union)
%
%   just like ord_union, except our elements are Elt-Weight
%   pairs instead of just Elts.  Then Union is like the ord_union
%   except when both sets of have two same Elt's  with different
%   weights, the minimum weight is kept.  

ord_min_union([], Set2, Set2).
ord_min_union([Head1|Tail1], Set2, Union) :-
	ord_min_union_1(Set2, Head1, Tail1, Union).

ord_min_union_1([], Head1, Tail1, [Head1|Tail1]).
ord_min_union_1([Head2|Tail2], Head1, Tail1, Union) :-
	weighted_compare(Order, Head1, Head2),
	ord_min_union_1(Order, Head1, Tail1, Head2, Tail2, Union).

ord_min_union_1(<, Head1, Tail1, Head2, Tail2, [Head1|Union]) :-
	ord_min_union_1(Tail1, Head2, Tail2, Union).
ord_min_union_1(>, Head1, Tail1, Head2, Tail2, [Head2|Union]) :-
	ord_min_union_1(Tail2, Head1, Tail1, Union).
ord_min_union_1(=, Head1-N1, Tail1, Head1-N2,  Tail2, [Head1-N|Union]) :-
	min(N1,N2,N),
	ord_min_union(Tail1, Tail2, Union).

weighted_compare(Order, Head1-_N1, Head2-_N2) :- 
	compare(Order,Head1,Head2).



/* Testing   */

time_floyd(P,N) :- 
	random_graph(P,N,G), 
	s_to_d_graph(G,Z), 
	runtime(floyd(Z,_ZLast)).

