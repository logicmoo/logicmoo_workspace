
:- export same_length/2.

same_length([],[]).
same_length([_|T],[_,T1]):-
	same_length(T,T1).
