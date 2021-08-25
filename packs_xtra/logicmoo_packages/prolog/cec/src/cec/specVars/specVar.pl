/*
 *	file:		loadStore.pl
 *	version:	1.5
 *	date:		November 6, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates for the handling of specification variables
 *
 *	history:
 *	891102	uh	Added this comment
 *	891107	uh 	Moved definitions of
 *			load/1		load/2
 *			store/0		store/1		store/2
 *			into this file
 *	891108	uh	Moved definitions of 
 *			delete/1	delete/2
 *			into this file
 *	891108	uh	Moved definition of
 *			ss/1
 *			into file specVars/stack.pl
 *	891108	uh	Moved definition of
 *			specifications/0	specificationLoaded/0
 *			lookState/1
 *			into this file
 *			
 *				
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

load(ModuleName,OrderName) :-
	atomic(ModuleName),
	atomic(OrderName),
	getVarNameForSpec(_,ModuleName,OrderName,Mod),
	!,
	load(Mod).

load(S) :-
	(world(S) ->
		pushState(undo),
		rs(S)
	;
		nl,
		write(' *** There exists no specification of that name!'),
		!,
		fail
	).


store :-
	getVarNameForSpec(_,_,_,S),
	ss(S).

store(ModuleName,OrderName) :-
	atomic(ModuleName),
	atomic(OrderName),
	getVarNameForSpec(_,ModuleName,OrderName,S),
	!,
	ss(S).

store(S) :-
	ss(S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


delete(S) :-
	S \== '$initial',
	deleteWorld(S).

delete(ModulName,OrderName) :-
	atomic(ModulName),
	atomic(OrderName),
	getVarNameForSpec(_,ModulName,OrderName,S),
	!,
	delete(S).




/*-----------------------------------------------------------------*/
/* specifications 						   */
/* lists all specifications that are loaded into a state variable  */
/*-----------------------------------------------------------------*/

specifications :-
    \+ specificationLoaded,
    print('No specification loaded until now.'), !.
specifications :-
    format('Variable~25|ModuleName~50|OrderName~1n',[]),
    format('--------~25|----------~50|---------~1n',[]),
    lookState(user),
    fail.
specifications :-
    world(M),
    lookState(M),
    fail.
specifications :- !.

specificationLoaded :- 
    current_module(M), M:cont(moduleName,_).

lookState(WorldName) :-
    !,
    (isUndoState(WorldName) ; WorldName == 'user' ->
	true
    ;
	(WorldName:cont(moduleName,MN),
	 WorldName:cont(orderName,ON) ->
	    format('~a~25|~a~50|~a~1n',[WorldName,MN,ON])
	;
	    WorldName:cont(ctr('$equation'),NoE),
	    WorldName:cont(ctr('$rule'),NoR),
	    WorldName:cont(ctr('$nonoperational equation'),NoNop),
	    No is NoE + NoR + NoNop,
	    No > 0 -> 
		format('~a~25|~a~50|~a~1n',[WorldName,'',''])
	    ;
		false
	)	
    ),
    !.


