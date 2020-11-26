%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This file must implements following predicates:
%% step(+State, -ActionDef, -NewState)
%%   Return descendant of State and ActionDefinition that was used.
%% is_goal(State) - is true when State is a goal state.  
%%	 repeating(Goal1, Goal2):-  Goal1 is the same as Goal2.
%% make_init_state(-Goal)
%%   return initial state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-use_module(library(ordsets)).
:-use_module(library(sets)).

make_init_state(G):-
		make_mutex(M0), bb_put(mutex, M0),
		get_goal(G),
		bb_put(fictiveGoal, G),
    get_init(I),
    init_heuristics(I).

make_solution(S, NS):-
		reverse(S, NS).
		

step(G, ActionDef, NewG):-
		get_action(A, ActionDef),
		get_positiv_effect(A, PE),
		get_negativ_effect(A, NE),
		get_precondition(A, P),
		get_parameters(A, Param),
		instance(Param),
		ord_intersection(G, PE, [_|_]),
		ord_intersection(G, NE, []),
		ord_subtract(G, PE, G0), ord_union(G0, P, NewG),
		check_mutex(NewG).

is_goal(G):-
		get_init(I),
		ord_subset(G, I).

repeating(Goal1, Goal2):-
		ord_subset(Goal2, Goal1).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Subroutines		
%instance(PredicateList)
%Make a ground instance of list of predicates		
instance([]).
instance([P|Ps]):-
		ground(P),!,
		instance(Ps).
instance([P|Ps]):-
		var(P),
		!,
		bb_get(objects, O),
		member(P, O),
		instance(Ps).
instance([P|Ps]):-
		\+ ground(P),
		P=.. [F,[V]],
		Type=..[F,List],
		bb_get(objects, O),
		member(Type, O),
		member(A, List),
		V=A,
		instance(Ps).

%debug(+Action, +Deep, +State)
%Writes debug info
debug(Action, D, S):-
		space(D), write('-'), write(Action), write('    '), write(S),nl.