:- module( metacalls_demo,
         [ 
		 ] ).

p(A) :-
	call(A, B).

p(A) :-
	setof(_,(a, a, b),_).
