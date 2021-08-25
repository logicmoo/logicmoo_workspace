/*
 *	file:		arith.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains some additional arithmetic functions.
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

plus(X,Y,Z):-
	Z is X+Y.

max(X,Y,Y):-
	X<Y,
	!.
max(X,_Y,X).

min(X,Y,Y):-
	X>Y,
	!.
min(X,_Y,X).
