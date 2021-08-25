/*
 *	file:		undo.pl
 *	version:	1.5
 *	date:		November 8, 1989
 *	creation:	October 31, 1989
 *	author:		-
 *
 *	description:
 *	This file contains the  predicates for the user commands undo
 *	and undoUndo. Also the internally used predicate undoUponFail
 *	is defined in this file.
 *
 *	history:
 *	891108	uh	Added this comment
 *	891108	uh	Moved definition of 
 *			forget/0
 *			into this file
 *	891108	uh	Moved definition of
 *			isUndoState/1
 *			into this file
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */


undo :-
	pushState(undoUndo),
	(	popState(undo)
	;
		popState(undoUndo)
	),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

undoUndo :-
	pushState1(undo),
	(	popState(undoUndo)
	;
		popState(undo)
	),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isUndoState(WorldName) :-
	(appendAtoms('undo(',Tail,WorldName)
	 ;
	 appendAtoms('undoUndo(',Tail,WorldName)
	),
	appendAtoms(Number,')',Tail),
	number(Number),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

undoUponFail(F,Error) :-
	pushState(undo),
	F,
	!,
	(Error \== none -> 
		popState(undo),
		error("failed",[],F),
		fail
	;	true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

forget :-
	deleteStack(undo),
	deleteStack(undoUndo).
