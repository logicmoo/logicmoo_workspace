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

%search(InitState, GoalState, -Solution)
search(I, _, Solution):-
		iterative(I, 0, Solution).
				
iterative(I, Deep, O):-
		dfs(I, [I], 0, Deep, O, _), !.
iterative(I, Deep, O):-
		NewDeep is Deep+1,
		iterative(I, NewDeep, O).


%dfs(+State, +Visited, +Deep, +MaxDeep, -Solution, SolutionDeep)
dfs(State, _, D, _, [], D):-
		is_goal(State), 
%		write('Found solution!!!!'), nl,
		!.
dfs(S, [_|V], _, _, _, _):-
		member(R, V),
		repeating(S, R), !,
%		write('repeating!!!!!!'),nl,
		fail.
		
dfs(S, V, D, MD, [ActionDef|Ls], SD):-
		D < MD,
		step(S, ActionDef, NewS),
%		debug(ActionDef, D, NewS),
		NewD is D+1,
		stat_node,
		dfs(NewS, [NewS|V], NewD, MD, Ls, SD).