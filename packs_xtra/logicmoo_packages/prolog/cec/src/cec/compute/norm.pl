/*
 *	file:		norm1.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		Ullrich Hustadt
 *
 *	description:
 *	This file contains the static part of the definition of the norm-
 *	function.
 *
 *	history:
 *	891010	uh	Added this comment
 *	891128	uh	Moved definition of
 *			applyRule/3
 *			from compute/prove.pl into this file
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

norm(T1) :-
  norm(T1,'$term'(T2)),
  sPrint("The normalform of % is % .",[T1,'$osTerm'(T2)]).

/*----------------------------------------------------------------------*/ 
/* norm(+Term,-NF)							*/
/* computes the normalform of Term. norm is universal for all different */
/* kinds of term representation used in CEC				*/

norm(T,_) :-
	varsOf(T,L),
	L \== [],
	!,
	error("Prolog variables not allowed in terms:
		%.",[T],typeCheck),
	cont1(error,none).
norm(let Defs in Term,'$term'(NF)) :-
	!,
  	internalLetExpRep(let Defs in Term,PTerm),
	apply:=allRules,
	reduceL(PTerm,NF,reduceR),
%  	trace:=off,
   	!.
norm((C=>T),'$term'(N)) :-
	!,
	internalEqRep((C=>T='$void'),(CT,[TT=_])),
	condReduceClausesFor(CT,Clauses),
	apply:=allRules,
	reduceU(Clauses,TT,N),
%	trace:=off,
	!.
norm(T1,'$term'(NF)) :-
   	!,
   	internalTermRep1(T1,IT1,reduceR),
   	apply:=allRules,
   	reduceU([],IT1,NF),
%  	trace:=off,
   	!.


applyRule(T,RuleIndex,term(T1)) :-
	internalTermRep(T,TT),
	prologVariant(RuleIndex,'$rule',((C,[L=R]),_)),
	applyRuleAndUnify(ac_uniAC,(L,R),TT,T1),
	mapP(lambda([I=J],equalNormalForms(I,J)),C).
