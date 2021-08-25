/*
 *	file:		modExp.pl
 *	version:	1.5
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates to evaluate module expressions.
 *
 *	history:
 *	891010	js	Added this comment
 *	900207	uh	Changed definition of moduleExists:
 *			It is no longer required that the termination ordering
 *			must coincide with the ordername of a module stored
 *			in a specification variable.
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

getStandard(Depth,PathFileName,To,ET) :-
	getVarNameForSpec(thaw(PathFileName),ModuleName,OrderName,_),
	(moduleExists(ModuleName,OrderName,Where) ->
		loadModule(Depth,Where,To),
		ET = none
	;	thaw(Depth,PathFileName,To,ET)
	).



evalModExp(Depth,noInterface,noInterface,Ord) :-
	transformed_Order(Ord,poly(N)),
	!,
	fileNameExt(standard,poly,PolyStandard),
	concAtomNames(PolyStandard,N,PolyNStandard),
	getStandard(Depth,PolyNStandard,user,E0),
	(E0 = noFile ->
		getStandard(Depth,standard,user,E1),
		(E1 = noFile ->
			libPath(P),
			concAtomNames(P,'/standard/',Path),
			baseName(PolyNStandard,PolyNStandardBase),
			concAtomNames(Path,PolyNStandardBase,POLYNStandard),
			getStandard(Depth,POLYNStandard,user,E2),
			(E2 = noFile ->
				concAtomNames(Path,standard,Standard),
				getStandard(Depth,Standard,user,E3),
				(E3 = noFile ->
					error("Can't find module standard.",[],evalModExp),
					fail
				;	true
				)
			;	true
			)
		;	true
		)
	;	true
	).
evalModExp(Depth,noInterface,noInterface,_Ord) :-
	!,
	getStandard(Depth,standard,user,E1),
	(E1 = noFile ->
		libPath(P),
		concAtomNames(P,'/standard/',Path),
		concAtomNames(Path,standard,Standard),
		getStandard(Depth,Standard,user,E2),
		(E2 = noFile ->
			error("Can't find module standard.",[],evalModExp),
			fail
		;	true
		)
	;	true
	).
evalModExp(Depth,noInterface,OrderExp,Ord) :-
	!,
	sPrint(" *** % : No interface-expression in the specification.
	Interface-expression in the ordering: using %.",[evalModExp,OrderExp]),
	nl,
	evalModExp(Depth,noInterface,noInterface,Ord),
	!.
evalModExp(Depth,ModExp,OrderExp,_) :-
	!,
	resetState,
	evalModExp(Depth,ModExp,OrderExp,_,user),
	!.




evalModExp(Depth,ModExp,noInterface,_Ord,IsStoredIn):-
	atomic(ModExp),
	!,
	evalModExp(Depth,ModExp,(noorder for ModExp),_,IsStoredIn),
	!.
evalModExp(Depth,ModExp,(OrderName for ModExp),_Ord,IsStoredIn):-
	atomic(ModExp),
	\+ (OrderName == noorder),
	\+cont1(fromScratch,true),
	(moduleExists(ModExp,OrderName,Where) ->
		loadModule(Depth,Where,IsStoredIn)
	;	fileNameExt(ModExp,OrderName,File),
		thaw(Depth,File,user,ET),
		getVarNameForSpec(thaw(File),_,_,NewMod),
		ET = none,
		(atomic(IsStoredIn) -> 
		    loadModule(Depth,user,IsStoredIn)
		    ;
		    IsStoredIn = NewMod
		)
	),
	!.
evalModExp(Depth,ModExp,(OrderName for ModExp),_Ord,IsStoredIn):-
	atomic(ModExp),
	!,
	(moduleExists(ModExp,OrderName,Where) ->
		loadModule(Depth,Where,IsStoredIn)
	;	fileNameExt(ModExp,'eqn',FileOfSpec),
		(OrderName == noorder ->
			FileOfOrder = noorder
		;	fileNameExt(ModExp,OrderName,FOfOrder),
			fileNameExt(FOfOrder,'ord',FileOfOrder)
		),
		in(Depth,FileOfSpec,FileOfOrder,OrderName,none),
		getVarNameForSpec(_,_,_,NewMod),
%		storeNewModule(ModExp,OrderName,user,NewMod),
		(atomic(IsStoredIn) -> 
		    loadModule(Depth,user,IsStoredIn)
		    ;
		    IsStoredIn = NewMod
		)
	),
	!.
evalModExp(_Depth,ModExp,OrderExp,_Ord,_ToBeStoredIn):-
	atomic(ModExp),
	error("use expression in the specification file : %,
	use expression in the ordering file : %.",[ModExp,OrderExp],evalModExp),
	fail,
	!.
evalModExp(Depth,rename(ModExp,SA,OA),OrderExp,_Ord,IsStoredIn):-
	!,
	concAtomNames(Depth,' ',D1),
	evalModExp(D1,ModExp,OrderExp,_,user),
	!,
	append(SA,OA,A),
	(atomic(IsStoredIn) -> 
		true
	;
		newMod(IsStoredIn)
	),
	sPrint("%[renaming %, to become % ...]",[Depth,'$assoc'(A),IsStoredIn]),
	nl,
	renameSpec(A,IsStoredIn,none),
	!.
evalModExp(Depth,(E1+E2),noInterface,_Ord,IsStoredIn):-
	!,
	evalModExp(Depth,(E1+E2),(noInterface + noInterface),_,IsStoredIn),
	!.
evalModExp(Depth,(E1+E2),(OE1+OE2),_Ord,IsStoredIn):-
	concAtomNames(Depth,' ',D1),
	evalModExp(D1,E1,OE1,_,IsInS1),
	!,
	evalModExp(D1,E2,OE2,_,IsInS2),
	!,
	(atomic(IsStoredIn) -> 
		true
	;
		newMod(IsStoredIn)
	),
	sPrint("%[combining % and % into %...]",[Depth,IsInS1,IsInS2,IsStoredIn]),
	nl,
	combineSpecs(IsInS1,IsInS2,IsStoredIn),
	!.


%testOrder(noorder,_Ord) :- !.
%testOrder(Order,Ord) :-
%	prefix_order(Order1,Order),
%	(tps_combineable(Order1,Ord,_) ->
%		true
%	;
%		nl,
%		error("Inconsistencies in the use expression of the ordering.",[],evalModExp),
%		sPrint("	order(%) using (... % for ...).",[Ord,Order]),
%		nl,
%		nl
%	).


%prefix_order(kns,Order) :-
%	appendAtoms(kns,_Suffix,Order).
%prefix_order(neqkns,Order) :-
%	appendAtoms(neqkns,_Suffix,Order).
%prefix_order(poly(1),Order) :-
%	appendAtoms(poly,_Suffix,Order).


pushMod(TopElem):-
	pushState(m),
	cont1(stackPtr(m),Top),
	mkAtom('m(%)',[Top],TopElem),
	!.

newMod(TopElem):-
	(cont1(stackPtr(m),M);M=0),
	M1 is M+1,
	assign1(stackPtr(m),M1),
	mkAtom('m(%)',[M1],TopElem),
	!.

storeNewModule(ModExp,OrderName,From,To) :-
	getVarNameForSpec(_,ModExp,OrderName,To),
	(From = user ->
	    ss(To)
	    ;
	    rs(From),   
	    ss(To)
	).


moduleExists(ModExp,OrderName,Where):-
	getVarNameForSpec(_,ModExp,OrderName,Where),
	Where:cont(moduleName,ModExp),
	Where:cont(orderName,_OrderName), % changed uh 07.02.89
	!.

splitSpecName(SpecName,Module,Order) :-
	name(SpecName,Chars),
	append(ModChars,[46|OrdChars],Chars), 
	name(Module,ModChars),
	name(Order,OrdChars),
	!.
	
getVarNameForSpec(_,Module,Order,Mod) :-
	atomic(Module),
	atomic(Order), 
	!,
	mkAtom('%.%',[Module,Order],Mod).
getVarNameForSpec(_,Module,Order,Mod) :-
	cont(moduleName,Module),
	(cont(orderName,Order) ->
		true
	;
		orderName = noorder
	),
	mkAtom('%.%',[Module,Order],Mod).
getVarNameForSpec(thaw(PathFileName),Module,Order,Mod) :-
	baseName(PathFileName,FileName),
	appendAtoms(SpecName,'.q2.0',FileName),
	splitSpecName(SpecName,Module,Order),
	moduleName:=Module,
	orderName:=Order,
	mkAtom('%.%',[Module,Order],Mod).
getVarNameForSpec(thaw(PathFileName),Module,Order,Mod) :-
	baseName(PathFileName,SpecName),
	splitSpecName(SpecName,Module,Order),
	moduleName:=Module,
	orderName:=Order,
	mkAtom('%.%',[Module,Order],Mod).
getVarNameForSpec(thaw(PathFileName),Module,Order,Mod) :-
	baseName(PathFileName,FileName),
	appendAtoms(Module,'.q2.0',FileName),
	Order = noorder,
	moduleName:=Module,
	orderName:=Order,
	mkAtom('%.%',[Module,Order],Mod).
getVarNameForSpec(thaw(PathFileName),Module,Order,Mod) :-
	baseName(PathFileName,Module),
	Order = noorder,
	moduleName:=Module,
	orderName:=Order,
	mkAtom('%.%',[Module,Order],Mod).
