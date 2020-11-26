%h(State, EstimatedValue)
% Estimated distance to achive Goal.
:-use_module(library(sets)).

h([], 0).
h([H|T], E):-
		bb_get(predicatesPrices, Ps),
		member(H-Price, Ps),
		h(T, Max),
    (Max < Price -> E=Price; E=Max).


%init_heuristics(+InitState).
init_heuristics(InitState):-
    relax(InitState, InitState, 0, Ps),
    bb_put(predicatesPrices, Ps).

relax(_, [], _D, []):-!.
relax(S, Delta, D, Ps):-
    mark_by(Delta, D, Marked),    
		setof(P, relax_step(S, P), PE),
		ord_union([S|PE], NS),
		ord_subtract(NS, S, NewDelta),
    ND is D+1,
    relax(NS, NewDelta, ND, NewPs),
    ord_union(NewPs, Marked, Ps).

relax_step(State, PE):-
		get_action(A),	get_precondition(A, P),
		mysubset(P, State),
		get_positiv_effect(A, PE).


 mark_by([], _, []).
 mark_by([H|T], D, [H-D|NT]):-
    mark_by(T, D, NT).
