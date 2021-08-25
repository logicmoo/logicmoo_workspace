/*
 *	file:		polGreater.pl
 *	version:	1.0
 *	date:		December 8, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains the user interface for comparison of
 *	polynomials.
 *
 *	history:
 *	891010	js	Added this comment
 *	891208  js	Fixed a bug in polGreater, occuring if the lists
 *                      of variables in the polynomials did not agree.
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

% polGreater(+Interpretation1, +Interpretation2) is the user interface
% for pol_tuplegreater/2

polGreater('$$interpretation_with_vars'(I1,V1),
	   '$$interpretation_with_vars'(I2,V2)) :-
	union(V1,V2,V),
	pol_tupleinject(V1,I1,V,InjI1),
	pol_tupleinject(V2,I2,V,InjI2),
%	print('$$interpretation_with_vars'(InjI1,V)),
%	print('$$interpretation_with_vars'(InjI2,V)),
	pol_tuplegreater(InjI1, InjI2).


/* pol_tupleinject(+V1,+T1,+V,-T)
 * injects a tuple T1 of polynoms over variables V1 into a tuple T over V
 */

pol_tupleinject(V1,T1,V,T) :-
	pol_makeinject(V1,V,0,Inj),
	map(lambda([P1,P],
		map(lambda([(K,L1),(K,L)],
			InjC^(copy_term(Inj,InjC),InjC=(L1,L))),P1,P)),T1,T).


/* pol_makeinject(+S1,+S,+F,-Inj)
 * S1 must be a subset of S. It then creates a injection Inj taking the
 * injection from S1 to S as a template. F is used to fill up free
 * places in the list. Inj is a pair of lists of corresponding variables
 * or fill elements.
 * Example: S1=[a,c,d], S=[a,b,c,d,e], F=0 -> Inj=([X,Y,Z],[X,0,Y,Z,0])
 */

pol_makeinject(S1,S,F,(L1,L)) :-
	length(S1,N),
	length(L1,N),
	pol_makeinject(S1,S,F,L1,L).

pol_makeinject(_,[],_,_,[]).
pol_makeinject(S1,[E|S],F,L1,[V|L2]) :-
	(position(S1,E,P) ->
	    position(L1,V,P)
	;
	    V=F
	),
	pol_makeinject(S1,S,F,L1,L2).
