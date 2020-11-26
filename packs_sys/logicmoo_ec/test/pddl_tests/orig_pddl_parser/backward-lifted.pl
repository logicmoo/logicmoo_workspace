%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This file must implements following predicates:
%% step(+State, -ActionDef, -NewState)
%%   Return descendant of State and ActionDefinition that was used.
%% is_goal(State) - is true when State is a goal state.  
%%	repeating(Goal1, Goal2):-  Goal1 is the same as Goal2.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-use_module(library(ordsets)).
:-use_module(library(sets)).
:-[domain].

make_init_state(G):-
		make_mutex(M0), bb_put(mutex, M0),
		get_goal(G),
		bb_put(fictiveGoal, G), !.

make_solution(S, NS):-
		reverse(S, NS).
		

step(G, ActionDef, NewG):-
		get_action(A, ActionDef),
		get_positiv_effect(A, PE),
		get_negativ_effect(A, NE),
		get_precondition(A, P),
		get_parameters(A, Param),
		
		define_domain(Param),
%		write('Params'),w(Param),nl,
%		write('Akcia'-ActionDef-G-PE-NE),nl,
		intersection_nonempty(G, PE),
%		write('Goal'), w(PE),nl,nl,
%		write('1'),nl,
%		w(G-NE),nl,
		intersection_empty(G, NE),
%		write('2'),nl,
		subtract(G, PE, G0),
%		write('3'),nl,
		union(G0, P, NewG),
%		write('NG'), w(NewG),nl,
%		write('OK akcia'-ActionDef),nl,
		check_mutex(NewG),
%   write('Mutex OK'-ActionDef),nl,
    true.
    %write('NG'), w(NewG),nl.

is_goal(G):-
		get_init(I),
		subset(G, I).

repeating(Goal1, Goal2):-
		subset_domain(Goal2, Goal1).
		

		
%%%%%%%%%%%%%%%%

subset_domain([], _).
subset_domain([H|T], M):-
    select(SM, M, RM),
    H=SM,
    subset_domain(T, RM).

intersection_nonempty([H|T], B):-
		member(M, B), 
    H=M;
		intersection_nonempty(T, B).

intersection_empty([], _):-!.
intersection_empty(_, []):-!.
intersection_empty([H|T], B):-
		intersection_empty_list(H, B),
		intersection_empty(T, B).
intersection_empty_list(_, []).
intersection_empty_list(A, [H|T]):-
		not_equal(A, H),
		intersection_empty_list(A, T).


can_unify(A, B):-
	\+ \+ A=B.

can_unify(_, [], []).
can_unify(A, [H|T], R):-
		(can_unify(A, H) ->
			R=[H|Rs] ;
			R=Rs
		),
		can_unify(A, T, Rs).

rec_intersection([], _, []).
rec_intersection(_, [], []).
rec_intersection([H|T], Ss, [H|Is]):-	%const-const
		ground(H),!,
		select(S, Ss, Rs),
		H=S,
		rec_intersection(T, Rs, Is).
rec_intersection([H|T], Ss, I):-			%else
		select(S, Ss, Rs),
		can_unify(H, S),
		!,
		(
			H=S, NewS=Rs, I=[H|Is],rec_intersection(T, NewS, Is)
			;
			not_equal(H, S),
			select(S2, Rs, _),
			H=S2,
			I=[H|Is],
			rec_intersection(T, [S|Rs], Is)
		).
rec_intersection([_|T], Ss, I):-
		rec_intersection(T, Ss, I).


rec_subtract([], _, []).
rec_subtract(S, [], S).
rec_subtract([H|T], Ss, I):-
		select(S, Ss, Rs),
		(can_unify(H, S),! ->
			H=S,					NewS=Rs, I=Is;
			not_equal(H, S),	NewS=S,	I=[H|Is]
		),
		rec_subtract(T, NewS, Is).
rec_subtract([_|T], Ss, I):-
		rec_subtract(T, Ss, I).


define_domain([]).
define_domain([P|Ps]):-
		ground(P),!,
		define_domain(Ps).
define_domain([P|Ps]):-
		\+ ground(P),
		P=.. [F,[V]],
		Type=..[F,List],
		bb_get(objects, O),
		member(Type, O),
		domain(V, List),
		define_domain(Ps).










instance([]).
instance([P|Ps]):-
		ground(P),!,
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

debug(Action, D, S):-
		space(D), write('-'), write(Action), write('    '), write(S),nl.

