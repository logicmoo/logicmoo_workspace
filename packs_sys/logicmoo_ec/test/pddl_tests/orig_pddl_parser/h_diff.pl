%h(State, EstimatedValue)
% Estimated distance to achive Goal.
h(S, E):-
		bb_get(fictiveGoal, G),
		ord_subtract(G, S, I),
		length(I, E).
		
init_heuristics(_).