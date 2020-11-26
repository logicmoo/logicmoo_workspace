% Demonstrate the effect of \c:

:- module( quotes_demo,
         [ a/1
         ] ).

a(X) :-
	X = 'alpha
		bravo
		charlie'.

a(X) :- Y = "x", Y == Y,
	X = "delta
	echo \c
	foxtrot".
