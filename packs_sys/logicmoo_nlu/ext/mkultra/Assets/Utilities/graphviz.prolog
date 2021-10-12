:- public draw_dag/1, draw_dag/2.

%test :-
%   draw_dag(immediate_kind_of).

draw_dag(NeighborRelation) :-
   draw_dag(identity, NeighborRelation).

draw_dag(NameFunction, NeighborRelation) :-
   open("c:\\Users\\ian\\Desktop\\MKULTRA.dot", write, Stream),
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

write_node_with_attributes(Stream, Node, Attributes) :-
   string_representation(Node, N),
   gz_command(Stream, N, Attributes).

write_transition_with_attributes(Stream, From, To, Attributes) :-
   string_representation(From, F),
   string_representation(To, T),
   gz_command(Stream, (F -> T), Attributes).

gz_command(Stream, Command) :-
	 write(Stream, Command),
	 write(Stream, ';').
gz_command(Stream, Command, []) :-
   !,
   gz_command(Stream, Command).
gz_command(Stream, Command, Attributes) :-
   write(Stream, Command),
   write(Stream, Attributes),
   write(Stream, ';').

draw_diggraph(NodeRelation, EdgeRelation, Groupings) :-
   open("c:\\Users\\ian\\Desktop\\MKULTRA.dot", write, Stream),
   writeln(Stream, 'digraph foo {'),
   gz_command(Stream, 'rankdir=LR'),
   forall(call(NodeRelation, Node, Attributes),
	  write_node_with_attributes(Stream, Node, Attributes)),
   forall(call(Groupings, Nodes, Attributes),
	  begin(writeln(Stream, ' subgraph {'),
		forall(member(Attr, Attributes),
		       gz_command(Stream, Attr)),
		forall(member(Node, Nodes),
		       gz_command(Stream, Node)),
		writeln(Stream, '}'))),
   forall(call(EdgeRelation, From, To, Label),
	  write_transition_with_attributes(Stream, From, To, Label)),
   writeln(Stream, '}'),
   close(Stream).
   %shell("open", "-a Graphviz /tmp/foo.dot").
