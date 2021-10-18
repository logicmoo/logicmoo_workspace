% create a list omitting all instances of Something
without(_, [], []).
without(Something, [Something|T], NT) :-
	without(Something, T, NT).
without(Something, [H|T], [H|NT]) :-
	Something \= H,
	without(Something, T, NT).


unique([],[]).
unique([H|T],[H|UNT]) :-
	without(H, T, NT),
	unique(NT, UNT).

