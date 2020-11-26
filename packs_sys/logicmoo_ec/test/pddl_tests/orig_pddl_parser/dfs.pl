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

%search(+InitState, GoalState, -Solution)
search(I, _, Solution):-
		bb_put(upperBound, 10000000000000),
		setof(SD-As, dfs(I, [], 0, As, SD), Results),
		min_member(_-S, Results),
		make_solution(S, Solution).
		
%dfs(+State, +Visited, +Deep, -Solution, SolutionDeep)
dfs(State, _, D, [], D):-
		is_goal(State), 
		bb_put(upperBound, D),
		!.
dfs(S, [_|V], _, a, _):-
		member(R, V),
		repeating(S, R), !, fail.
dfs(S, V, D, [ActionDef|Ls], SD):-
		test_bound(D),
		setof(F-Temp-AD, best_step(S, D, AD, Temp, F), NextNodes),
		member(_E-NewS-ActionDef, NextNodes),
		NewD is D+1,
%		length(NextNodes, L), write(L), debug(ActionDef, D, NewS),
		stat_node,
		dfs(NewS, [NewS|V], NewD, Ls, SD).

%best_step(State, Deep, ActionDef, NewState, EstimatedValue):-
best_step(S, D, ActionDef, NewS, E):-
		step(S, ActionDef, NewS),
		h(S, V),
		E is D+V.

%test_bound(Deep):-
test_bound(D):-
		bb_get(upperBound, B),
		D<B.

%debug(Action, D, S):-
%		space(D), write('-'), write(Action), write('    '), write(S),nl.
