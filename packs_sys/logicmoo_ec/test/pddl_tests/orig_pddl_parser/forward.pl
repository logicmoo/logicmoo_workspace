%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This file must implements following predicates:
%% step(+State, -ActionDef, -NewState)
%%   Return descendant of State and ActionDefinition that was used.
%% is_goal(State) - is true when State is a goal state.  
%%	repeating(Goal1, Goal2):-  Goal1 is the same as Goal2.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-use_module(library(ordsets)).

make_init_state(I):-
		get_init(I),
		get_goal(G),
		bb_put(fictiveGoal, G).


make_solution(S, S).
		
step(State, ActionDef, NewState):-
		get_action(A, ActionDef),
		get_precondition(A, P),    mysubset(P, State),	% choose suitable action
		get_negativ_effect(A, NE), ord_subtract(State, NE, State2),	
		get_positiv_effect(A, PE), ord_union(State2, PE, NewState).

is_goal(S):-
		get_goal(G),
		ord_subset(G, S).

repeating(S1, S2):-
		S1 =  S2.
		
