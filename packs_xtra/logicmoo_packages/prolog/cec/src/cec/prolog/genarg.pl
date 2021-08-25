/*
 *	file:		genarg.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains the predicate find_subterm/3, which is used
 *	in antecedent3.pl .
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


/* find_subterm(Occ,Term,SubTerm)
 * finds the occurences of Subterm in Term. Subterm is not instantiated
 * with subterms of Term, so even prolog variables can be searched for.
 * (used in antecedent3.pl)
 */

find_subterm([],T1,T2) :-
	T1 == T2.
find_subterm([Index|Indices],Term,SubTerm) :-
	gen_arg(Index,Term,Arg),
	find_subterm(Indices,Arg,SubTerm).

gen_arg(I1,T,Arg) :-
	nonvar(T),
	functor(T,_,N),
	gen_args(I1,T,Arg,N).

gen_args(_,_,_,0) :- !, fail.
gen_args(N,T,Arg,N) :-
	arg(N,T,Arg).
gen_args(I1,T,Arg,N) :-
	M is N-1,
	M > 0,
	gen_args(I1,T,Arg,M).
