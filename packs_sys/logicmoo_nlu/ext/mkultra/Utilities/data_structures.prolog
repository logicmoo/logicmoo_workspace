%%%
%%% Misc useful data structures and data structure algorithms
%%%

:- public list_to_array/2, array_member/2, topological_sort/3.

list_to_array(List, Array) :-
   Array =.. [array | List].

array_member(Member, Array) :-
   arg(_, Array, Member).

%% topological_sort(*RootList, *EdgeRelation, -List)
%  Generates a topologically sorts a DAG defined by EdgeRelation.
topological_sort(RootList, EdgeRelation, List) :-
   topological_sort_recur(RootList, EdgeRelation, [ ], List).

topological_sort_recur([ ], _, List, List).
topological_sort_recur([Element | Elements], EdgeRelation, InList, OutList) :-
   memberchk(Element, InList) ->
      topological_sort_recur(Elements, EdgeRelation, InList, OutList)
      ;
      ( all(Neighbor, call(EdgeRelation, Element, Neighbor), Neighbors),
	topological_sort_recur(Neighbors, EdgeRelation, InList, WithNeighbors),
	topological_sort_recur(Elements, EdgeRelation, [Element | WithNeighbors], OutList) ).

