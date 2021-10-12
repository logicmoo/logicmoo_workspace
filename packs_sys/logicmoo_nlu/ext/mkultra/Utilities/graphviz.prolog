:- public draw_dag/1, draw_dag/2.

%test :-
%   draw_dag(immediate_kind_of).

draw_dag(NeighborRelation) :-
   draw_dag(identity, NeighborRelation).

draw_dag(NameFunction, NeighborRelation) :-
   open("c:\\temp\\foo.dot", write, Stream),
   writeln(Stream, 'digraph foo {'),
   gz_command(Stream, 'rankdir=BT'),
   forall(call(NeighborRelation, From, To),
	  write_transition(Stream, NameFunction, From, To)),
   writeln(Stream, '}'),
   close(Stream).
   %shell("open", "-a Graphviz /tmp/foo.dot").

:- public identity/2.
identity(X,X).

write_transition(Stream, NameFunction, From, To) :-
   begin(call(NameFunction, From, FName),
	 call(NameFunction, To, TName),
	 gz_command(Stream, (FName -> TName))).

gz_command(Stream, Command) :-
	 write(Stream, Command),
	 write(Stream, ';').
   