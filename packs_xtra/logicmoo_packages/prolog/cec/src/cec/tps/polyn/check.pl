/*
 *	file:		check.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates to check, whether a given
 *	polynomial is a legal interpretation of an operator.
 *
 *	history:
 *	891010	js	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

% pol_legal_interpretation(P):
% checks, whether a given polynomial P is a legal interpretation
% of an operator.
% P is said to be a legal interpretation, if it is a constant
% (nullary function) greater than 2 or if it is a polynomial
% of arity N > 0, such that every variable has a degree greater
% than zero and all coefficients are natural numbers.
% new : the interpretation polynomial of an unary Operator is not only 
% a single variable (e.g. I[s(x)] is not only 'x').

pol_legal_interpretation([(q(N,1), [])]) :- % interpretation of constants
	!,
	N >= 2.
pol_legal_interpretation([(q(N,1), [X])]) :- % interpretation of unary op
% if the interpretation is not a single monomial (e.g x+1),
% take the next clause
	!,
	(X = 1 ->
		N > 1  		% 2*x or N*x is legal
	;
		X > 1, 
		N > 0 		% x^2 or N*(x^X) is legal
	).  
pol_legal_interpretation([(C, [E1|ERest])|Rest]) :-
	pol_checkpolynomial([(C, [E1|ERest])|Rest], SumExpLists),
	pol_nonzero(SumExpLists).


pol_checkpolynomial([(q(N,1), ExpList)], ExpList) :-
	!,
	N > 0.
pol_checkpolynomial([(q(N,1), ExpList)|Rest], ResList) :-
	!,
	N > 0,
	pol_checkpolynomial(Rest, Res1),
	pol_addlists(ExpList, Res1, ResList).


pol_addlists([], [], []).
pol_addlists([X1|L1], [X2|L2], [X|L]) :-
	X is X1 + X2,
	pol_addlists(L1, L2, L).


pol_nonzero([]).
pol_nonzero([0|_L]) :-
	!,
	fail.
pol_nonzero([_X|L]) :-
	pol_nonzero(L).




