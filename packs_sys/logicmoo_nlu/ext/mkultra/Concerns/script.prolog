on_enter_stat

next_events(Nonterminal, History-[NextEvent | Tail], NextSet) :-
	all(Next,
	    parse(Nonterminal, History, Tail),
	    NextSet).