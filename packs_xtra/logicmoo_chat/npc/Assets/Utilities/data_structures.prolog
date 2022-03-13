%%%
%%% Misc useful data structures and data structure algorithms
%%%

:- public list_to_array/2, array_member/2, topological_sort/3.



%=autodoc
%% list_to_array( ?List, ?Array) is semidet.
%
% List Converted To Array.
%
list_to_array(List, Array) :-
   Array =.. [array | List].



%=autodoc
%% array_member( ?Member, ?Array) is semidet.
%
% Array Member.
%
array_member(Member, Array) :-
   must_be(compound,Array),
   arg(_, Array, Member).

%% topological_sort(*RootList, *EdgeRelation, -List)
%  Generates a topologically sorts a DAG defined by EdgeRelation.
topological_sort(RootList, EdgeRelation, List) :-
   topological_sort_recur(RootList, EdgeRelation, [ ], List).

%=autodoc
%% topological_sort( ?RootList, ?EdgeRelation, ?List) is semidet.
%
% Topological Sort.
%




%=autodoc
%% topological_sort_recur( ?ARG1, ?ARG2, ?ARG3, ?List) is semidet.
%
% Topological Sort Recur.
%

topological_sort_recur(A,B,C,D):- topological_sort_recur(100,A,B,C,D).

topological_sort_recur(R,_,_,_,_):- (R>0 -> true ; !), fail.

topological_sort_recur(_,[ ], _, List, List).
topological_sort_recur(R2,[Element | Elements], EdgeRelation, InList, OutList) :-
   R is R2 -1,
  (memberchk(Element, InList) ->
      topological_sort_recur(R,Elements, EdgeRelation, InList, OutList)
      ;
      ( all(Neighbor, call(EdgeRelation, Element, Neighbor), Neighbors),
	topological_sort_recur(R,Neighbors, EdgeRelation, InList, WithNeighbors),
	topological_sort_recur(R,Elements, EdgeRelation, [Element | WithNeighbors], OutList) )).



%=autodoc
%% el_get( ?X, ?Y, ?Z) is semidet.
%
% El Get.
%
el_get(X,Y,Z):-
 X/Y/Z.
