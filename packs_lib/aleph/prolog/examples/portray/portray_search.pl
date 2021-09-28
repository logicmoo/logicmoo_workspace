% Predicate to portray search using the davinci graph drawing tool
aleph_portray(search):-
        open('search.davinci',write,Stream),
        set_output(Stream),
        write_davinci_graph,
        close(Stream),
        set_output(user_output),
        concat([davinci,' ','search.davinci'],Cmd),
        execute(Cmd).


write_davinci_graph:-
	write('['), nl,
	write_davinci_graph(0),
	nl, write(']'), nl.

write_davinci_graph(0):-
	!,
	get_children(0,First,Last),
	write_davinci_nodes(First,Last).
write_davinci_graph(Node):-
	get_node_attributes(Node,[Name,Type,Colour,Clause,Pos,Neg]),
	tab(4),
	write('l("'), write(Name), write('",'), nl,
	tab(8),
	write('n("'), write(Type), write('",'), nl,
	tab(12),
	write('[a("COLOR","'), write(Colour), write('"),'), nl,
	(Type = best ->
		tab(12),
		write('a("BORDER","double"),'), nl;
		true),
	tab(12),
	write('a("OBJECT","'),
	write_davinci_clause(Clause),
	write_davinci_pn(Pos,Neg),
	write('")],'), nl,
	get_children(Node,First,Last),
	write_davinci_links(Node,First,Last),
	nl, tab(8), write(')'), nl,
	tab(4),
	write(')'),
	(Last >= First -> write(','), nl; true),
	write_davinci_nodes(First,Last).



write_davinci_nodes(F,L):- F > L, !.
write_davinci_nodes(Node,Last):-
	write_davinci_graph(Node),
	Node1 is Node + 1,
	(Node1 =< Last -> write(','), nl; true),
	write_davinci_nodes(Node1,Last).
	

write_davinci_links(_,First,Last):-
	First > Last, !,
	tab(12),
	write('[]').
write_davinci_links(Parent,First,Last):-
	tab(12),
	write('['),
	write_davinci_link(Parent,First,Last),
	tab(12),
	write(']').

write_davinci_link(_,F,L):- F > L, !.
write_davinci_link(P,F,L):-
	write('l("'), write(P), write('->'), write(F), write('",'), nl,
	tab(16),
	write('e("",[a("EDGEPATTERN","solid"),a("_DIR","normal")],'),
	write('r("'), write(F), write('")))'),
	F1 is F + 1,
	(F1 =< L -> write(','), nl, tab(12); true),
	write_davinci_link(P,F1,L).

	
get_children(Parent,First,Last):-
	recorded(search,expansion(_,Parent,First,Last),_), !.
get_children(_,1,0).

get_node_attributes(Node,[Node,Type,Colour,Clause,PC,NC]):-
	recorded(search,node(Node,Clause),_), 
	recorded(search,label(Node,[PC,NC|_]),_), 
	numbervars(Clause,0,_),
	(recorded(search,good(Node),_) ->
		(recorded(search,best(Node),_) -> Type = best; Type = good),
		Colour = green;
		(recorded(search,bad(Node),_) ->
			Type = bad,
			Colour = red;
			Type = normal,
			Colour = white)), !.
get_node_attributes(Node,[Node,pruned,red,Clause,na,na]):-
	recorded(search,node(Node,Clause),_), 
	numbervars(Clause,0,_), !.
get_node_attributes(Node,[Node,bad,red,pruned,na,na]).

write_davinci_clause((H:-B)):-
	!,
	write_davinci_literal(H),
	write_davinci_conditional,
	write_davinci_lits(B),
	write('.').
write_davinci_clause(H):-
	write_davinci_literal(H),
	write('.').

write_davinci_lits((A,B)):-
	!,
	write('\t'),
	write_davinci_literal(A),
	(setting(portray_literals,true) -> write(' and'); write(',')),
	write('\n'),
	write_davinci_lits(B).
write_davinci_lits(A):-
	write('\t'),
	write_davinci_literal(A).

write_davinci_literal(L):-
	setting(portray_literals,true), 
	portray(L), !.
write_davinci_literal(L):-
	write(L).

write_davinci_conditional:-
	setting(portray_literals,true),
	write(' if:\n'), !.
write_davinci_conditional:-
	write(' :-\n').

write_davinci_pn(na,na):-
	!,
	write('\n\npruned').
write_davinci_pn(Pos,Neg):-
	write('\n\npos: '), write(Pos),
	write('\nneg: '), write(Neg).



