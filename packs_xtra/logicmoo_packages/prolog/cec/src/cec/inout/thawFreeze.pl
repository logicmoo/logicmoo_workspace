/*
 *	file:		thawFreeze.pl
 *	version:	1.5
 *	date:		November 6, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates for thawing and freezing a saved 
 *	state of a specification
 *
 *	history:
 *	891102	uh	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

thaw(From) :-
	thawTo(From,user).

thaw(ModName,OrdName) :-
	fileNameExt(ModName,OrdName,File),
	thawTo(File,user).

thaw(ModName,OrdName,To) :-
	fileNameExt(ModName,OrdName,File),
	thawTo(File,To).

thawTo(From,To) :-
	(To = '$current' ->
		pushState(undo)
	;
		true
	),
	thaw('',From,To,Error),
	!,
	(Error = none -> 
		true
	;
		(Error = noAtom ->
			error("% not atomic.",[From],thaw(From))
		;
			error("no frozen state of name %",[From],thaw(From))
		)
	).


thaw(Depth,From,To,Error) :-
	atomic(From),
	cec_version(Prolog,_),
	fileNameExt(From,Prolog,F),
	exists(F),
    	try(deleteWorld(To)),
%	resetState,
	baseName(F,BaseName),
	sPrint("%[thawing % into %...]",[Depth,BaseName,To]),
	nl,
	(readState(F,To) ->
		Error = none
	;
		Error = syntaxError
	),
	(Error = none ->
		getVarNameForSpec(thaw(From),_,_,M),
		loadModule(Depth,To,M)
	;
		true
	).
thaw(_,From,_,noAtom) :-
	not atomic(From).
thaw(_,_,_,noFile).


readState(F,To) :-
	assign1(error,none),   
	see(F),
	repeat,
	(read(T) ->
		storeClause(To,T),
		(	T = end_of_file
		;
			\+ cont1(error,none)
		)
	;	error("% is probably no frozen CEC-state.",[F],readState(F,To))
	),
	seen,
	!,
	cont1(error,none),
	(To = user ->
		integrityConstraint
	;
		retractAll(world(To)),
		assertz(world(To))
	),
	!.



freeze :-
	cont(moduleName,M),
	M \== '$noName',
	!,
	cont(orderName,Ord),
	(Ord == noorder ->
		tps_current_ordering(Order),
		transformed_Order(ExternalOrd,Order),		
		fileNameExt(M,ExternalOrd,File)
	;
		fileNameExt(M,Ord,File)
	),
	freeze(M,Ord,File),
	nl,
	storeLog,
	nl.
freeze :-
	error("no name associated with current specification",[],freeze),
	fail.


freeze(File) :-
	(nonvar(File) ->
		cont(moduleName,ModName),
		cont(orderName,OrdName),
		freeze(ModName,OrdName,File)
	;
		sPrint("Freeze impossible",[]),
		nl,
		cont(moduleName,ModName),
		cont(orderName,OrdName),
		store(ModName,OrdName),
		sPrint("Specification variable %.% updated",[ModName,OrdName]),
		nl,
		!,
		fail
	).
	
freeze(ModName,OrdName) :-
	fileNameExt(ModName,OrdName,File),
	freeze(ModName,OrdName,File).

freeze(ModName,OrdName,File) :-
		cec_version(Prolog,_),
		fileNameExt(File,Prolog,F),
		tell(F),
		listWorld,
		told,
		sPrint("State frozen to %",[F]),
		nl,
		!,
		store(ModName,OrdName),
		sPrint("Specification variable %.% updated",[ModName,OrdName]),
		nl,
		!,
		(OrdName == noorder ->
			tps_current_ordering(Order),
			transformed_Order(ExternalOrd,Order),
			store(ModName,ExternalOrd),
			sPrint("Specification variable %.% updated",[ModName,ExternalOrd]),
		nl
		;
			true
		).		



