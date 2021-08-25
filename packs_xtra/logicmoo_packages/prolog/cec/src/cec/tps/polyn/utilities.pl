/*
 *	file:		utilities.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains some useful predicates for polynomial
 *	termination orderings.
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

% pol_zerolist checks whether its argument is a list of zeros

pol_zerolist([]).
pol_zerolist([0|R]) :-
	pol_zerolist(R).



% pol_expand_to_list(X, N, L) succeeds if L is a list containing
% the element X N times.

pol_expand_to_list(_X, 0, []) :- !.

pol_expand_to_list(X, N, [X|L]) :-
	N1 is N - 1,
	pol_expand_to_list(X, N1, L).



%  pol_subst(Elem,Subst,OldList,NewList)  substitutes the first occurence
%  of Elem in OldList resulting NewList

pol_subst(_,[],[]).
pol_subst(Elem,Subst,[Elem|Rest],[Subst|Rest]) :- !.
pol_subst(Elem,Subst,[Non_Elem|Rest],[Non_Elem|NewRest]) :-
	pol_subst(Elem,Subst,Rest,NewRest).


%  pol_insert(Elem,OldList,NewList) inserts Elem in OldList resulting NewList.
%  If OldList already contains Elem, NewList is equal to OldList


pol_insert(Elem,[],[Elem]).
pol_insert(Elem,[Elem|Rest],[Elem|Rest]) :- !.
pol_insert(Elem,[NonElem|Rest],[NonElem|NewRest]) :-
	pol_insert(Elem,Rest,NewRest).


%  pol_remove(Elem,OldList,NewList) removes the first occurence of Elem
%  in OldList resulting NewList. If Elem is not in OldList, NewList
%  is identical to it.

pol_remove(_,[],[]).
pol_remove(Elem,[Elem|Rest],Rest) :- !.
pol_remove(Elem,[NonElem|Rest],[NonElem|NewRest]) :-
	pol_remove(Elem,Rest,NewRest).


%  pol_remove_dupl(List,NewList) iff NewList is List without duplicates

pol_remove_dupl([],[]).
pol_remove_dupl([A|Rest],NewRest) :-
	member(A,Rest),
	!,
	pol_remove_dupl(Rest,NewRest).
pol_remove_dupl([A|Rest],[A|NewRest]) :-
	pol_remove_dupl(Rest,NewRest).



%  pol_retractall(X) deletes all facts X (not clauses with head X)
%  but does not delete (as quintus-prolog abolish/2 does)
%  dynamic-declarations

pol_retractall(X) :- retract(X),fail.
pol_retractall(_X).
