
bad_semantic_triples(Triples) :-
	member([be, arg_3, level], Triples),
	member([be, arg_2, Arg2], Triples),
	Arg2 \== null.
