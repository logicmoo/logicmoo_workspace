list_array(List, Array) :-
   Array =.. [array | List].

array_member(Member, Array) :-
   arg(_, Array, Member).

%% topological_sort(*StartElement, *Relation, -List)
%  Generates a topologically sorted list of all elements of Relation.
%  Relation must define a DAG.
topological_sort(Start, Relation, List) :-
   toplogical_sort_recur([Start], Relation, [ ], List).

topological_sort_recur([], _, List, List).
topological_sort_recur([Element | Elements], Relation, InList, OutList) :-
   memberchk(Element, InList),  % Element already added
   !,
   topological_sort_recur(Elements, Relation, InList, OutList).
