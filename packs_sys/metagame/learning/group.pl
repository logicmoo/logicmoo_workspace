%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================


do_graph(P_Graph, S_Graph) :-
	sort(P_Graph, EdgeSet),
	do_vertices(EdgeSet, VertexBag),
	sort(VertexBag, VertexSet),
	do_group(VertexSet, EdgeSet, S_Graph).


do_vertices([], []) :- !.
do_vertices([A^Z|Edges], [A|Vertices]) :-
	do_vertices(Edges, Vertices).


do_group([], _, []) :- !.
do_group([Vertex|Vertices], EdgeSet, [Vertex-Neibs|G]) :-
	do_group(EdgeSet, Vertex, Neibs, RestEdges),
	do_group(Vertices, RestEdges, G).


do_group([V^X|Edges], V, [X|Neibs], RestEdges) :- !,
	do_group(Edges, V, Neibs, RestEdges).
do_group(Edges, _, [], Edges).

