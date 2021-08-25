/*
 *	file:		panMiscell.pl
 *	version:	1.0
 *	date:		November 3, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains a simple predicate for inserting an element
 *	into a list, avoiding double occurrences of this element.
 *
 *	history:
 *	891103	uwe	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

% pan_insert(OldList, Element, NewList)

pan_insert([], Element, [Element]) :-
  !.
pan_insert([Element|Rest], Element, [Element|Rest]) :-
  !.
pan_insert([X|Rest], Element, [X|NewRest]) :-
  pan_insert(Rest, Element, NewRest).

