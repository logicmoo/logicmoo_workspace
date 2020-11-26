%h(State, EstimatedValue)
% Estimated distance to achive Goal.
:-use_module(library(sets)).

h(S, E):-
		bb_get(fictiveGoal, G),
		relax(S, G, E).
%    write(G-S-E),nl.

relax(_, [], 0):-!.
relax(S, G, E):-
    subtract(G, S, Delta),
		setof(P, relax_step(S, P), RS),
		ord_union([S|RS], NS),
    relax(NS, Delta, NE),
		length(Delta, LD),
		E is LD+NE.

relax_step(State, PE):-
		get_action(A),	get_precondition(A, P),
		mysubset(P, State),
		get_positiv_effect(A, PE).
