/*
 *	file:		panndasTrace.pl
 *	version:	1.5
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains 
 *
 *	history:
 *	891106	uh	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

% *** muss auf explain/notify umgestellt werden

setTrace(M,on) :-
	(	trace(M)
	;
		assert(trace(M))
	).
setTrace(M,off) :-
	(	retract(trace(M))
	;
		true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

traceModes :-
	traceMode(M,I),
	nl,
	write(M),tab,
	write(I),
	fail.
traceModes :-
	nl.


traceMode(red,'shows all attempts to reduce rewrite rules and equations').
traceMode(cp,'prints more information about calculation of critical pairs').
traceMode(cpall,'prints very detailed information about critical pair calculation').
