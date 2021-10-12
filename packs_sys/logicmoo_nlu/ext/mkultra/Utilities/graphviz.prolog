draw_dag(NameFunction, RootPredicate, NeighborRelation) :-
   open("/tmp/foo.dat", "write", Stream),
   writeln(Stream, 'digraph foo {'),
   forall(call(RootPredicate, Node),
	  walk_dag(Stream, Node, NameFunction, NeighborRelation)),
   writeln('}', Stream),
   close(Stream).

walk_dag(Stream, Node, NameFunction, NeighborRelation) :-
   call(NameFunction, Node, Name),
   
   