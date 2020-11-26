%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This file must implements predicate search
%% search(+InitState, +GoalState, -Solution)
%%   InitState has an arbitrary definition.
%%   GoalState
%%   Solution is a list of actions obtained from ActionDef of predicate step.
%%  
%% Following predicates may be used:
%%   step(+State, -ActionDef, -NewState)
%%     During backtracking generates all possible descendants.
%%
%%   is_goal(+State) - is true when State is a goal state.
%%
%%   h(+State, +Value) - is estimated distance to the goal state.
%%
%%   repeating(+State, +AnotherState) - is true when two states are equal.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-use_module(library(ordsets)).
:-use_module(library(queues)).

%search(InitState, GoalState, -Solution)
search(I, _, Solution):-
		state_record(I, nil, nil, 0, SR),
		list_queue([SR], Q),
		bfs(Q, [], Solution).
		
		
		
%bfs(+Queue, +Visited, -Solution)
bfs(Q, _, 'NO SOLUTION'):-
		empty_queue(Q),
%		write('NO SOLUTION'),nl,
		!.
bfs(Q, V, Solution):-
		queue_cons(SR, _, Q),
		state_record(S, _, _, _, SR),
		is_goal(S),
%		write('FOUND SOlution!'),
		solution(SR, V, Solution).
bfs(Q, V, Solution):-
		queue_cons(SR, RQ, Q),
%		state_record(S, _, _, D, SR),	write(D), write('  '),queue_length(Q, QL), write(QL), write(S),nl,
		(bagof(NS, next_node(SR, Q, V, NS), NextNodes) ; NextNodes=[]),
%		length(NextNodes, L), write(L), nl, 
%		write('NN'), write(NextNodes), nl, 
		queue_append(RQ, NextNodes, NQ),
		ord_add_element(V, SR, NV),
		stat_node,
		bfs(NQ, NV, Solution).
		
next_node(SR, Q, V, NewSR):-
		state_record(S, _, _, D, SR),
		step(S, AD, NewS),
		state_record(NewS, _, _, _, Temp),
		\+ my_ord_member(NewS, V),
		\+ queue_member(Temp, Q),
		NewD is D+1,
		state_record(NewS, S, AD, NewD, NewSR).

%%
% subroutines
%Similar to ord_member except that use predicate repeat for comparing.		
my_ord_member(S, [SR|_]):-
		state_record(S2, _, _, _,SR),
		repeating(S, S2),
		!.
my_ord_member(S, [_|T]):-
		my_ord_member(S, T).



				
