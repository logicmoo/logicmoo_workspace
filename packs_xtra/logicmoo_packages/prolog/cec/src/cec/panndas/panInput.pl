/*
 *	file:		panInput.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates for reading the translated 
 *	PAnndA-specifications into CEC.
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

pan_transferIn(File) :-
	pushState(undo),
	restoreWorld('$initial'),
	(error:=none),
	!,
	pan_transferReadEqs(File,Eqs),
	write('   ... analyzing axioms'),
	nl,
	eraseNonEqTerms(Eqs,Eqs1),
	map(syntaxAndTypeCheck,Eqs1,Eqs2),
	(cont(error,none);undo,!,fail),
	recordAxioms(Eqs2).


pan_transferReadEqs(From,Es) :-
	pan_transferReadFrom(From,Es).


pan_transferReadFrom(From,Terms):-
	fileExists(From),
	!,
	write('

   ... starting specification transfer '),
	nl,
	nl,
	see(From),
	(	inTerms(_, Terms,success)
	;	
		seen,
		!,
		fail
	),
	seen,
	write('

   ... specification successfully loaded '),
        nl,
	nl.
