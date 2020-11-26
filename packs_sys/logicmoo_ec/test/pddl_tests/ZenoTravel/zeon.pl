fluent(aircraft(_)).
fluent(person(_)).
fluent(city(_)).
fluent(flevel(_)).

fluent(at(_,_)).
fluent(in(_,_)).
fluent(fuel_level(_,_)).
fluent(next(_,_)).

action(board(_,_,_)).

poss(board(P,A,C),and(person(P),
		      and(aircraft(A),
			  and(city(C),
			      and(at(P,C),at(A,C)))))).

causes_true(in(P,A),board(P,A,C),true).
causes_false(at(P,C),board(P,A,C),true).

action(debark(_,_,_)).
poss(debark(P,A,C),and(person(P),
		      and(aircraft(A),
			  and(city(C),and(in(P,A),at(A,C)))))).

causes_true(at(P,C),debark(P,A,C),true).
causes_false(in(P,A),debark(P,A,C),true).


action(fly(_,_,_,_,_)).
poss(fly(A,C1,C2,L1,L2),and(aircraft(A),
			    and(city(C2),
				and(at(A,C1),
				   and(fuel_level(A,L1),
				       next(L2,L1)))))).

causes_true(at(A,C2),fly(A,C1,C2,L1,L2),true).
causes_true(fuel_level(A,L2),fly(A,C1,C2,L1,L2),true).
causes_false(fuel_level(A,L1),fly(A,C1,C2,L1,L2),true).
causes_false(at(A,C1),fly(A,C1,C2,L1,L2),true).

% action(zoom(_,_,_,_,_,_)).
% poss(zoom(A,C1,C2,L1,L2,L3),and(aircraft(A),
% 				and(city(C2),
% 				    and(at(A,C1)
% 				       and(fuel_level(A,L1),
% 					   and(next(L2,L1),next(L3,L2))))))).


% causes_true(at(A,C2),zoom(A,C1,C2,L1,L2,L3),true).
% causes_true(fuel_level(A,L3),zoom(A,C1,C2,L1,L2,L3),true).
% causes_false(at(A,C1),zoom(A,C1,C2,L1,L2,L3),true).
% causes_false(fuel_level(A,L1),zoom(A,C1,C2,L1,L2,L3),true).


action(refuel(_,_,_,_)).
poss(refuel(A,C,L,L1),and(aircraft(A),and(city(C),and(fuel_level(A,L),next(L,L1))))).
causes_true(fuel_level(A,L1),refuel(A,C,L,L1),true).
causes_false(fuel_level(A,L),refuel(A,C,L,L1),true).

