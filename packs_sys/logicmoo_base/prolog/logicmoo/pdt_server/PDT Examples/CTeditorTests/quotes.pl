% Demonstrate the effect of \c:

:- module( quotes_demo,
         [ a/1
         ] ).


a(atom__(X)) :-
	X = 'alpha
		bravo
		charlie'.

a(atom_c(X)) :-
    X = 'alpha
		bravo  \c
		charlie'.

a(string__(X)) :- 
	X = "alpha
		bravo 
		charlie".
		
a(string_c(X)) :- 
	X = "alpha
		bravo \c
		charlie".

