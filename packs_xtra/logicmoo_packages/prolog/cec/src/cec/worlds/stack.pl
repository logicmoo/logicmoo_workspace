/*
 *	file:		stack.pl
 *	version:	1.5
 *	date:		November 8, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates to handle a stack of states.
 *	In the current CEC-version only used for the undo stack.
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

% 'pushState(+S) pushes current status on a stack named S'

pushState(S) :-
%	(S = undo ->
%		deleteStack(undoUndo) 
%	; 
%		true
%	),
	pushState1(S).


pushState1(S) :-
	(	cont1(stackPtr(S),_)
	;
		assign1(stackPtr(S),0)
	),
	cont1(stackPtr(S),Top),
	NewTop is Top+1,
	mkAtom('%(%)',[S,NewTop],TopElem),
	ss(TopElem),
	assign1(stackPtr(S),NewTop), 
	!.


% 'popState(+S) pops state from stack S'

popState(S) :-
	(cont1(stackPtr(S),Top) ->
		true
	;	write(' *** no such stack known '),
		fail
	),
	(Top > 0 ->
		true
	;	write(' *** stack empty'),
		fail
	),
	mkAtom('%(%)',[S,Top],TopElem),
	NewTop is Top-1,
	assign1(stackPtr(S),NewTop),
	rs(TopElem),	
%	ps(TopElem),	
	!.


beyondTop(S) :-
	(cont1(stackPtr(S),Top) ->
		true
	;	write(' *** no such stack known '),
		fail
	),
	Top1 is Top+1,
	mkAtom('%(%)',[S,Top1],BeyondTopElem),
	
	(world(BeyondTopElem) ->
		true
	;	write(' *** stack empty'),
		fail
	),
	assign1(stackPtr(S),Top1),
	rs(BeyondTopElem),	
	!.


deleteStack(S) :-
	(cont1(stackPtr(S),T) ->
		T=0
	;	true
	),
	!.
deleteStack(S) :-
	repeat,
	cont1(stackPtr(S),Top),
	mkAtom('%(%)',[S,Top],TopElem),
	NewTop is Top-1,
	assign1(stackPtr(S),NewTop),
	ds(TopElem),
	NewTop=<0,
	!.



rs(S) :-
	restoreWorld(S).


ps(S) :-
	rs(S),
	ds(S).

ss(S) :-
	(	deleteWorld(S)
	;
		true
	),
	saveWorld(S).


ds(S) :-
	S \== '$initial',
	deleteWorld(S),
	!.
ds(_).


