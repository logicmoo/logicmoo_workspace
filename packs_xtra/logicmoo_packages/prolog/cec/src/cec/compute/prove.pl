/*
 *	file:		prove.pl
 *	version:	1.5
 *	date:		November 6, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains some predicates proving the equality of terms.
 *
 *	history:
 *	891106	uh	Added this comment
 *	891106	uh	Moved definitions of
 *			contextuallyEqual/2 		contextuallyEqual/3
 *			reduceByRules/3			reduceByRules/4
 *			from compute/reduce.pl into this file
 *	891128	uh	Moved definition of 
 *			applyRule/3
 *			into file compute/norm1.pl
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

prove :-		% js 910419
	bagof(Cs,doProve(Cs),Tasks),
	mapP(prove,Tasks).


prove((T1=T2)) :-
	norm(T1,TN1),
	norm(T2,TN2),
	nl,
	sPrint("Normal forms are: % and %",[TN1,TN2]),
	!,
	ac_matchvf(TN1,TN2).
prove((C => T1=T2)) :-
	norm((C=>T1),TN1),
	norm((C=>T2),TN2),
	nl,
	sPrint("Normal forms are: % and %",[TN1,TN2]),
	!,
	ac_matchvf(TN1,TN2).
prove(E) :-
	indProve(E).

indProve(_E):-
	ignoreOutput(showCStatus(C)),
	C\==complete,
	!,
	sPrint("
Inductive theorem proving requires a complete system to start with.
",[]),
	!,
	fail.
indProve(E):-
	asList(E,EL),
	map(internalEqRep,EL,EI),
	map(addDomainConditions,EI,ED),
	genDomainAxiomsAndComplete,
	indProve:==true,
	redPredsForRedNopEqs(Addresses),
	assert(needsToBeErased(Addresses)),
	ss('$beforeIndProve'),
	addEquations(ED),
	sPrint("

starting inductive completion ...
",[]),
	cNoFreezeNoUndo,
	indProve:==false,
	mapA(erase,Addresses),
	ignoreOutput(showCStatus(C)),
	!,
	C==complete,
	sPrint("

End of Proof.",[]).


equalNormalForms(I,J) :-
	reduceU([],I,N),
	reduceU([],J,N),
	!.


/* --------------------------------------------------------------
	contextuallyEqual/2 is called from reduce-procedures for
	rewrite rules upon testing validity of conditions.
	Assumes that context equations N=M are present as facts
	of form '$context'(N,M)
----------------------------------------------------------------- */
contextuallyEqual(L,R):-
	setof1((N=M),'$context'(N,M),Context),
	contextuallyEqual(Context,L,R).


/* ----------------------------------------------------------------
	contextuallyEqual/3 has the meaning as contextuallyEqual/2.
	Here, the context equations are given by the first
	parameter.
   --------------------------------------------------------------- */

contextuallyEqual([],L,R):-	% optimization of frequent case
	!,
	ac_matchvf(L,R),
	!.
contextuallyEqual(CNO,DLN,DRN):-
	reduceByRules(CNO,DLN,L),
	reduceByRules(CNO,DRN,R),
	ac_matchvf(L,R).



reduceByRules(Rs,T,TN):-	% Rs may be nonterminating
	reduceByRules(10,Rs,T,TN),
	!.

reduceByRules(0,_,T,T):-
	!.
reduceByRules(N,Rs,T,TN):-
	member((L=R),Rs),
	ac_matchvf(T,L),
	M is N-1,
	reduceByRules(M,Rs,R,TN).
reduceByRules(N,Rs,T,TN):-
	T=..[O|Ts],
	mapF(lambda([T1,TN1],reduceByRules(N,Rs,T1,TN1)),Ts,TsN),
	TN=..[O|TsN].


