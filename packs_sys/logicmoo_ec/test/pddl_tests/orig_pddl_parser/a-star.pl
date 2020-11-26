% Dostupne veci:
%step(+State, -NewState)
%is_goal(State)
%h(State, Value) 
%repeating(+State, +AnotherState)

:-use_module(library(ordsets)).
:-use_module(library(heaps)).

%search(InitState, GoalState, -Solution)
search(I, _, Solution):-
		a_star(I, Solution, _).
		
		
%bfs(+InitState, -Actions, -Cost).
a_star(S, A, C):-
		state_record(S, nil, nil, 0, SR),
		list_to_heap([0-SR], PQ),
		a_star(PQ, [], A, C).


%a_star(+Queue, +Visited, -Solution, -Cost)
a_star(PQ, _, 'NO SOLUTION', _):-
		empty_heap(PQ),!.
a_star(PQ, V, Solution, C):-
		get_from_heap(PQ, C, SR, _),
		state_record(S, _, _, _, SR),
		is_goal(S),
%		write('FOUND SOLUTION'),nl,
%		state_record(S, _, _, D, SR), write(C-D), write('   '),write(S),nl,
%		writel(V),nl,halt,
		solution(SR, V, Solution).

a_star(PQ, V, Solution, C):-
		get_from_heap(PQ, _K, SR, RPQ),
		ord_add_element(V, SR, NV),
		(bagof(K-NS, next_node(SR, PQ, NV, K, NS), NextNodes) ; NextNodes=[]),
%		state_record(S, _, _, D, SR), write(_K-D), write('   '),write(S),length(NextNodes, L), write(L),nl,
%		write(NextNodes),nl,

		add_list_to_heap(RPQ, NextNodes, NPQ),
		
		stat_node,
		a_star(NPQ, NV, Solution, C).

%next_node(+StateRecord, +Queue, +Visited, -EstimateDeep, -NewStateRecord)
next_node(SR, Q, V, E, NewSR):-
		state_record(S, _, _, D, SR),
		step(S, A, NewS),
		state_record(NewS, _, _, _, Temp),
		\+ my_ord_member(NewS, V),
		heap_to_list(Q, PQL),
		\+ member(Temp, PQL),
		h(S, H),
		E is H+D,
		ND is D+1,
		state_record(NewS, S, A, ND, NewSR).

%add_list_to_heap(+OldHeap, List, NewHeap)
add_list_to_heap(OH, [], OH).
add_list_to_heap(OH, [K-D|T], NH):-
		add_to_heap(OH, K, D, H),
		add_list_to_heap(H, T, NH).

my_ord_member(S, [SR|_]):-
		state_record(S2, _, _, _,SR),
		repeating(S, S2),
		!.
my_ord_member(S, [_|T]):-
		my_ord_member(S, T).
