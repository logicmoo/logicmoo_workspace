/*
 *	file:		in.pl
 *	version:	1.5
 *	date:		November 6, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates for input and output of specifications
 *
 *	history:
 *	891102	uh	Added this comment
 *	891102	uh	Moved definition of 
 *			addEquations/3		readTerms/3
 *			store/0			store/1		store/2
 *			ss/1
 *			into this file
 *	891106	uh	Moved definition of
 *			cd/0			cd/1			pathAsAtoms/2	
 *			exec/1			pwd/0			currentPath/1
 *			into this file
 *	891108	uh	Moved definition of
 *			enterUnique/2
 *			from worlds.pl into this file
 *	900309	js	Added cuts in iTerm/1 to avoid backtracking
 *			into error message
 *	900320	uh	Changed call to orderSortedOrdering
 *				
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */


in :-
	inNoExt('Scratch.pl').


% in(Module) :  read a specification from the file <Module>.eqn,
% ----------	no pragmas for the ordering

in(Module) :-
	in(Module,noorder).


% in(Module,Order) :
% ----------------
%	read a specification from the file <Module>.eqn and
%	read the pragmas for the ordering from the file <Module>.<Order>.ord
		
fromScratch(M,O):-
	fromScratch:==true,
	in(M,O),
	fromScratch:==false.
fromScratch(_,_):-
	fromScratch:==false,
	fail.


in(Module,Order) :-
	initTime,
	time((	deleteGarbage,
		fileNameExt(Module,eqn,File),
		(Order == noorder ->
			OrderFile = noorder
		;
			fileNameExt(Module,Order,OrdFile),
			fileNameExt(OrdFile,ord,OrderFile)
		),
		'prefixWithout@'(Order,OrderWithout),
		undoUponFail(in('',File,OrderFile,OrderWithout,Error),Error),
		deleteGarbage
	      )
	    ).


'prefixWithout@'(Order,OrderWithout) :-
	name(Order,NOrder),
	name('.@',KA),
	append(NOrderWithout,KA,NOrder),
	!,
	name(OrderWithout,NOrderWithout).
'prefixWithout@'(Order,Order).


% in(Depth,FileOfSpec,FileOfOrder,OrderName,Error) :
% ------------------------------------------------ 
%	read the specification from the file <FileOfSpec> and
%	read the pragmas for the ordering from the file <FileOfOrder>,
%	<FileOfOrder> = <Module>.<OrderName>[.@].ord,
%	the name of the module and the name of the ordering (-> OrderName)
%	will be fetched for subsequent freeze-commands

in(Depth,FileOfSpec,FOfOrder,OrderName,Error) :-
	(error :== none),
	resetState,
	((FOfOrder == noorder ; 
	  exists(FOfOrder) ;
	  appendAtoms(_,'@.ord',FOfOrder)) ->
		FileOfOrder = FOfOrder
	;
		appendAtoms(Base,'.ord',FOfOrder),
		fileNameExt(Base,'@.ord',FileOfOrder)
	),
	(	baseName(FileOfSpec,ModuleFile),
		baseName(FileOfOrder,OrderFile),
		sPrint("%[evaluating base of % with %...]",
		       [Depth, ModuleFile, OrderFile]),
		nl,
		readTerms(FileOfSpec,1,[ModuleInterfaceExp]),
		interpretModInterfaceExp(ModuleInterfaceExp,
					 Module,MInterfaceExp),
		(FileOfOrder == noorder ->
			Order = noorder,
			OInterfaceExp = noInterface
		;
			readTerms(FileOfOrder,1,[OrderInterfaceExp]),
			interpretOrdInterfaceExp(Module,OrderInterfaceExp,
						 Order,OInterfaceExp)
		),
		mkAtom('% ',[Depth],D1),
		evalModExp(D1, MInterfaceExp,OInterfaceExp,Order),
		moduleName := Module,
		orderName  := OrderName,
		orderInterfaceExp := OInterfaceExp,
		!,
		accTime(enrich(Depth,FileOfSpec,FileOfOrder,Error))
	;
		Error = error
	),
	(Error = none ->
		getVarNameForSpec(_,Module,OrderName,_),
		storeNewModule(Module,OrderName,user,_)
	;
		true
	),
	!.


interpretModInterfaceExp(module(Module), Module, noInterface) :- !.
interpretModInterfaceExp(module(Module) using MInterfaceExp,
			 Module, MInterfaceExpN) :-
	normalizeInterfaceExp(MInterfaceExp,MInterfaceExpN),
	!.
interpretModInterfaceExp(_, '$noName', noInterface).


normalizeInterfaceExp(A+B,AN+BN):-
	!,
	normalizeInterfaceExp(A,AN),
	normalizeInterfaceExp(B,BN).
normalizeInterfaceExp(rename(A,S,O),rename(AN,S,O)):-
	!,
	normalizeInterfaceExp(A,AN).
normalizeInterfaceExp((O for A),(O for AN)):-
	A=..[AN,_S,_O],
	!.
normalizeInterfaceExp((O for A),(O for A)):-
	atomic(A),
	!.
normalizeInterfaceExp(A,rename(AN,S,O)):-
	A=..[AN,S,O],
	list(S),
	list(O),
	!.
normalizeInterfaceExp(A,rename(AN,S,[])):-
	\+ atom(A),
	A=..[AN|Assoc],
	map(lambda([Pair,(X,Y)],
		(Pair = (X <- Y) ->
			true
		;	error("Illegal element in interface expression: %",[Pair],normalizeInterfaceExp))),Assoc,S),
	!.
normalizeInterfaceExp(A,A).


interpretOrdInterfaceExp(Module, order(Ord for Module), Ord, noInterface) :- !.
interpretOrdInterfaceExp(Module, order(_Ord for M),_,_) :-
	!,
	error("That's not the right ordering for the module %.
	The ordering is for the module %", [Module,M],in),
	fail.
interpretOrdInterfaceExp(_Module, order(Ord), Ord, noInterface) :- !.
interpretOrdInterfaceExp(Module, order(Ord for Module) using noorder,
			 Ord, noInterface) :- !.
interpretOrdInterfaceExp(Module, order(Ord for Module) using OInterfaceExp,
			 Ord, OInterfaceExpN) :-
	normalizeInterfaceExp(OInterfaceExp,OInterfaceExpN),
	!.
interpretOrdInterfaceExp(Module, order(_Ord for M) using _OInterfaceExp,_,_) :-
	!,
	error("This ordering should be for module %.
	It says it is for module %", [Module,M],in),
	fail.
interpretOrdInterfaceExp(_Module, order(Ord) using OInterfaceExp,
			 Ord, OInterfaceExpN) :-
	normalizeInterfaceExp(OInterfaceExp,OInterfaceExpN),
	!.
interpretOrdInterfaceExp(_Module,OrdInterfaceExp,_,_) :-
	!,
	error("Illegal interface expression in ordering %", [OrdInterfaceExp],in),
	fail.


% inNoExt(File) :
% -------------
% 	read a specification from the file <File>, no pragmas for the ordering

inNoExt(File) :-
	deleteGarbage,
	undoUponFail(in('',File,noorder,noorder,Error),Error),
	deleteGarbage.


% deleteGarbage
%
% prints a message and deletes the stack used for evaluating
% module expressions.

deleteGarbage :-
	sPrint("[collecting garbage...]",[]),
	nl,
	deleteStack(m).


% deleteGarbage.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enrich :-
	enrichNoExt('Scratch.pl').


enrich(Module) :-
	enrich(Module,noorder).


enrich(Module,noorder) :-
	!,
	fileNameExt(Module,eqn,File),
	undoUponFail(enrich('',File,noorder,Error),Error).
enrich(Module,Order) :-
	fileNameExt(Module,eqn,File),
	fileNameExt(Module,Order,FOfOrder),
	fileNameExt(FOfOrder,ord,OrderFile),
	undoUponFail(enrich('',File,OrderFile,Error),Error).


enrich(Depth,FileOfSpec,FileOfOrder,Error) :-
	(error :== none),
	abolish(parameterDecl,1),
	abolish(generatedByDecl,2),
	abolish(doProve,1),
	setof1(O,ac_ist_AC(O),OldACOps),
	setof1(O,[O1,T]^(O1 ofType (O:T)),OldOps),
	!,
	baseName(FileOfSpec,ModuleName),
	(FileOfOrder == noorder ->
		sPrint("%[reading body of %...]",[Depth,ModuleName]),
		nl
	;
		baseName(FileOfOrder,OrderName),
		sPrint("%[reading body of % and %...]",
		       [Depth,ModuleName,OrderName]),
		nl
	),	
	readTerms(FileOfSpec,_All,Eqs),
	readTerms(FileOfOrder,_All,Pragmas),
	sPrint("%[analyzing axioms...]",[Depth]),
	nl,
	eraseNonEqTerms(Eqs,Pragmas,Eqs1),
	map(syntaxAndTypeCheck,Eqs1,Eqs2),
	!,
	(cont1(error,none) ->
		globalConstraintsSatisfied,
		addEquations(Eqs2,OldACOps,OldOps),
		cont1(error,Error)
	;
		Error = error
	),
	!.

globalConstraintsSatisfied:-
	checkConstructors,
	(cont1(domainConstraints,on) ->
		convertDomainConstraints
	;
		true
	).
	

enrichNoExt(File) :-
	undoUponFail(enrich('',File,noorder,Error),Error).


readTerms(noorder,_,[]) :- !.
readTerms(F,N,Es) :-
	readFrom(F,N,Es).


addEquations(Eqs):-
	setof1(O,ac_ist_AC(O),OldACOps),
	setof1(O,[O1,T]^(O1 ofType (O:T)),OldOps),
	addEquations(Eqs,OldACOps,OldOps).



addEquations(Eqs,OldACOps,OldOps) :-
	tps_current_ordering(Order),
	recordAxioms(Eqs),
	(ac_ist_AC(O),
	 not member(O,OldACOps),
	 member(O,OldOps) ->

		resetOrientation,
	    	write('An old operator has become an AC-operator.'),
		nl,
	    	write('All rules have been turned back into equations.')
	;
		((	\+ '$rule'(_,_)
		 ;	not ac_ist_AC(_)
		 ;	Order=poly(_N)
		 ;	Order=manual
		 ) ->

			true
		;
			resetOrientation,
			write('An AC-operator has been added.'),
			nl,
			write('The previous ordering has not supported AC.'),
			nl,
			write('All rules have been turned back into equations.')
		)
	),
	!.




recordAxioms(Eqs) :-
	mapP(lambda([Eq],[A,R,C,E]^
			   ((Eq=(C->E);Eq=(C,[E]);C=[],E=Eq),
			     new('$equation',(C,[E]),A,[(origin,user)],R))),
	     Eqs),
	treatPermutativeAxioms,
	genInheritanceAxioms,
	(cont1(domainConstraints,on) ->
		genDomainPreds,
		genDomainAxioms
	;
		true
	).

:- dynamic allOverloadedInstances/2.
allOverloadedInstances(P,O) :-
	repeat((
		(O1 ofType (O:T)),
		map(subsortRT,S,T),
		(O1 ofType (O2:S)),
		apply(P,[O2])
	       )),
	(O1 ofType (O:_)),
	apply(P,[O1]),
	!.

treatPermutativeAxioms :-
	commutativeOp(O),
	findAssocLawIfPresent(O),
	(ac_ist_AC(O) ->
		(	tps_current_ordering(Ord),
		 	member(Ord,[manual,poly(_N)])
		->	true
		;	tps_order(poly(1)),
			nl,
			write('The ordering has been set to "poly(1)".'),
			nl
		),
		allOverloadedInstances(lambda([OX],tps_declac(OX)),O),
		allOverloadedInstances(lambda([OX],genRewriteExpertClauseFor(OX)),O)
	;
		allOverloadedInstances(lambda([OX],tps_declc(OX)),O)
	),
	fail.
treatPermutativeAxioms.	


commutativeOp(O) :-
	i('$equation',([],[L=R]),_,Ref),
	functor(L,O,2),
	permutative((L=R),commLaw),
	newNonreductiveEquation(nextIndex,([],[L=R]),[(origin,user),(kind,commLaw)],_),
	allOverloadedInstances(lambda([OX],assertz(ac_ist_C(OX))),O),
	dispose('$equation',Ref).
commutativeOp(O) :-
	ac_ist_C(O),	% old C-operator
	allOverloadedInstances(lambda([OX],enterUnique(ac_ist_C(OX))),O).

permutative([L=R],Law) :-
	permutative((L=R),Law).
permutative((L=R),commLaw) :-
	L=..[O,@V1,@V2|Rest],
	R=..[O,@V2,@V1|Rest],
	V1\==V2,
	setof1(@V,(member(@V,Rest),V\==V1,V\==V2),RV),
	length(Rest,N),
	length(RV,N),
	!.
permutative((L=R),assocLaw) :-
	(
		L=..[O,T12,@V3],
	    	R=..[O,@V1,T23]
	;
	 	R=..[O,T12,@V3],
	    	L=..[O,@V1,T23]
	),
	T12=..[O,@V1,@V2],
	T23=..[O,@V2,@V3],
	!.


% used by treatPermutativeAxioms
findAssocLawIfPresent(O) :-
	'$equation'(Ref1,([],[L1=R1])),
	functor(L1,O,2),
	permutative((L1=R1),assocLaw),
	dispose('$equation',Ref1),
	newNonreductiveEquation(nextIndex,([],[L1=R1]),[(origin,user),(kind,assocLaw)],_),
	allOverloadedInstances(lambda([OX],assertz(ac_ist_A(OX))),O),
	!.
findAssocLawIfPresent(O) :-
	'$rule'(Ref1,([],[L1=R1])),
	functor(L1,O,2),
	permutative((L1=R1),assocLaw),
	dispose('$rule',Ref1),
	newNonreductiveEquation(nextIndex,([],[L1=R1]),[(origin,user),(kind,assocLaw)],_),
	allOverloadedInstances(lambda([OX],assertz(ac_ist_A(OX))),O),
	!.
findAssocLawIfPresent(O):-
	ac_ist_A(O),
	allOverloadedInstances(lambda([OX],enterUnique(ac_ist_A(OX))),O),
	!.
findAssocLawIfPresent(_).

enterUnique(H):-
	enterUnique(H,true).

enterUnique(H,B) :-
	B\==true,
	fromProlog([],(H,B)),  % to avoid occur check problems
	clause(H,B),
	!.
enterUnique(Head,true) :-
	Head,
	!.
enterUnique(Head,Body) :-
	assertz((Head:-Body)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% used by treatPermutativeAxioms
genRewriteExpertClauseFor(O) :-		
	(_ ofType (O:_)),
	Template=..[O,_,_],
	retract((reduce(Template,_) :-_)),
	retract((reduceI(Template,_,_) :-_)),
	rewriteExpertClause(O,Clauses),
	mapA(asserta,Clauses),
	!.
genRewriteExpertClauseFor(_).		


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eraseNonEqTerms(Terms,Pragmas,Eqs) :-
	split(lambda([E],
		     [A,B,C]^
		     (	E=(A=>B=C); 
			E=(B=C);
		        E=(or A);
		 	E=(A=>false);
		 	E=(A=>or B)
		     )
		    ),
	      Terms,
	      Eqs,
	      NonEqs),
	!,
	mapA(checkForVar,NonEqs),
	mapA(checkForVar,Pragmas),
	mapA(recordSubsorts,NonEqs),
	elimTransSubsorts(NonEqs,Es),
	mapA(interpretTerm,Es),
	mapA(interpretTerm,Pragmas),
	resetOrientation(ordering_changed).


checkForVar(X) :-
	var(X),
	!,
	prologVarInInput(X),
	X=error.
checkForVar(_).


convertDomainConstraints :-
	checkDomainConstraints,
	setof1(F,combinedConstraint('$domainConstraint','$base',F),Fs),
	abolish(parameterSort,1),
	abolish(generatingMsOps,2),
	abolish(unconstrainedSort,1),
	mapP(assert,Fs).


generatedSort(S) :-
	generatingMsOps(S,_).


/* domainConstraint(-Constraint)
 * solutions are the domain constraints generated from the give input
 * (parameterDecl/1 and generatedByDecl/2).
 */

domainConstraint(parameterSort(S)) :-
	parameterDecl(S1),
	subsortRT(S,S1).
domainConstraint(generatingMsOps(S,Ops)) :-
	generatedByDecl(S1,_),
	subsortRT(S,S1),
	setof1(Op,msGeneratingOp(S,Op),Ops).
domainConstraint(unconstrainedSort(S)) :-
	properSort(S),
	\+domainConstraint(parameterSort(S)),
	\+parameterSort(S),
	\+domainConstraint(generatingMsOps(S,_)),
	\+generatingMsOps(S,_).


/* world(W,F)
 * succeeds for fact F in world W. Used to plug domainConstraint/1
 * into the combine mechanism. The treatment of base is a hack,
 * it is used to distinguish material imported from the base specification
 * and material from the module currently read.
 * Note that parameterSort/1, generatingMsOps/2 and unconstrainedSort/1
 * must have been imported, and that exactly one of these facts exists
 * for each proper sort.
 */

world(W,F) :-
	world(W),
	W:F.
world(W,properSort(S)) :-
	world(W),
	(	W:parameterSort(S)
		;
		W:generatingMsOps(S,_)
		;
		W:unconstrainedSort(S)
	).
world('$base',properSort(S)) :-
	parameterSort(S)
	;
	generatingMsOps(S,_)
	;
	unconstrainedSort(S).
world('$base',parameterSort(S)) :- parameterSort(S).
world('$base',generatingMsOps(S,Ops)) :- generatingMsOps(S,Ops).
world('$base',unconstrainedSort(S)) :- unconstrainedSort(S).
world('$domainConstraint',F) :-
	domainConstraint(F).


combinedConstraint(W1,W2,(parameterSort(S))) :-
	world(W1,parameterSort(S)),
	\+world(W2,properSort(S)).
combinedConstraint(W1,W2,(parameterSort(S))) :-
	world(W2,parameterSort(S)),
	\+world(W1,properSort(S)).
combinedConstraint(W1,W2,(generatingMsOps(S,Ops))) :-
	world(W1,generatingMsOps(S,Ops)),
	\+world(W2,properSort(S)).
combinedConstraint(W1,W2,(generatingMsOps(S,Ops))) :-
	world(W2,generatingMsOps(S,Ops)),
	\+world(W1,properSort(S)).
combinedConstraint(W1,W2,(unconstrainedSort(S))) :-
	world(W1,unconstrainedSort(S)),
	\+world(W2,properSort(S)).
combinedConstraint(W1,W2,(unconstrainedSort(S))) :-
	world(W2,unconstrainedSort(S)),
	\+world(W1,properSort(S)).
combinedConstraint(W1,W2,(parameterSort(S))) :-
	world(W1,parameterSort(S)),
	world(W2,parameterSort(S)).
combinedConstraint(W1,W2,(generatingMsOps(S,Ops))) :-
	world(W1,generatingMsOps(S,Ops)),
	world(W2,parameterSort(S)).
combinedConstraint(W1,W2,(generatingMsOps(S,Ops))) :-
	world(W1,parameterSort(S)),
	world(W2,generatingMsOps(S,Ops)).
combinedConstraint(W1,W2,(generatingMsOps(S,Ops))) :-
	world(W1,generatingMsOps(S,Ops1)),
	world(W2,generatingMsOps(S,Ops2)),
	(Ops1=Ops2 ->
		Ops=Ops1
	;
		error("Conflicting generatedBy constraints for sort %.",[S],in)
	).
combinedConstraint(W1,W2,(unconstrainedSort(S))) :-
	world(W1,unconstrainedSort(S)),
	world(W2,unconstrainedSort(S)).
combinedConstraint(W1,W2,(generatingMsOps(S,_))) :-
	(	world(W1,unconstrainedSort(S)),
		(world(W2,parameterSort(S));world(W2,generatingMsOps(S,_))),
		W=W1
	;
		W2:unconstrainedSort(S),
		(world(W1,parameterSort(S));world(W1,generatingMsOps(S,_))),
		W=W2
	),
	error("Missing domain constraint for sort % in %.",[S,W],in).


msGeneratingOp(S,Operator) :-
	generatedByDecl(S2,Ds),
	member(Op,Ds),
	Op ofType (Operator:[S1|_]),
	subsortRT(S1,S),
	subsortRT(S,S2).
msGeneratingOp(S,Operator) :-
	generatedByDecl(S,Ds),
	member(S1,Ds),
	subsortT(S1,S),
	msGeneratingOp(S1,Operator).


checkDomainConstraints :-
	parameterDecl(S),
	\+properSort(S),
	error("undefined sort % in parameter declaration",[S],in).
checkDomainConstraints :-
	generatedByDecl(S,_),
	\+properSort(S),
	error("undefined sort % in generatedBy declaration",[S],in).
checkDomainConstraints :-
	generatedByDecl(S,Ds),
	member(D,Ds),
	\+D ofType (_:_),
	\+properSort(D),
	error("undefined sort or operator % in generatedBy declaration for %",[D,S],in).
checkDomainConstraints :-
	generatedByDecl(S,Ds),
	member(Op,Ds),
	Op ofType _,
	\+(Op ofType (_:[S1|_]),subsortRT(S1,S)),
	error("operator % in generatedBy declaration not of sort %",[Op,S],in).
checkDomainConstraints :-
	generatedByDecl(S2,Ds),
	member(S1,Ds),
	once(properSort(S1)),
	\+subsortRT(S1,S2),
	error("sort % in generatedBy declaration not subsort of %",[S1,S2],in).
checkDomainConstraints :-
	(parameterDecl(S);generatedByDecl(S,_)),
	unconstrainedSort(S1),
	subsortRT(S1,S),
	error("no parameter or generatedBy declaration possible for sort %.
	Subsort % is unconstrained in base of specification.",[S,S1],in).
checkDomainConstraints.


recordSubsorts((S1<S2)) :-
	subsortT(S1,S2),
	!.
recordSubsorts((S1<S2)) :-
	os:=true,
	assert(subsort(S1,S2)),
	(subsortT(S,S) ->
		error("Subsort relation not irreflexive.
	Offending subsort declaration %.",[S1<S2],in)
	;	true
	).
recordSubsorts(_).


elimTransSubsorts([(S1<S2)|SpecElems],E1) :-
	subsort(S1,S3),
	subsortT(S3,S2),
	try(retract(subsort(S1,S2))),
	elimTransSubsorts(SpecElems,E1).
elimTransSubsorts([E|SpecElems],[E|E1]) :-
	elimTransSubsorts(SpecElems,E1).
elimTransSubsorts([],[]).


% In den Klauseln fuer "interpretTerm/1" wurde jeweils vor "T=Type" 
% die Abfrage "(Op ofType T)" eingefuegt.
% (falls "not (Op ofType T)" failt, wird T sonst nicht instantiiert, damit
% ist "T=Type" stets erfuellbar !)
%      Ulrich Wertz, Uwe Waldmann   26.6.87

interpretTerm((:-T)) :- T.
interpretTerm((S:-T)) :-
	assertz((S:-T)).
interpretTerm(parse(A,B)) :-
	assertz(parse(A,B)).
interpretTerm(predicate(P)) :-
	assertz(predicate(P)).
interpretTerm(pretty(T)) :-
	assertz(pretty(T)).
interpretTerm(T) :-
	functor(T,Name,_),
	member(Name,[forwardChainingDepth,resolutionDepth,
		     allowedEliminationTime,actionDefault,action,act]),
	assertz(T).
interpretTerm(T) :-
	iTermWOVars(T).


iTermWOVars(T) :-
	functor(T,F,N),
	knsPragma(F,N),
	!,
	iTerm(T).
iTermWOVars(T) :-
	varsOf(T,[]),
	!,
	iTerm(T).
iTermWOVars(T) :-
	prologVarInInput(T).

knsPragma(greater,1).
knsPragma(equal,1).
knsPragma(status,1).


iTerm(error) :-
	!.
iTerm(module(_)) :-
	!.
iTerm(module(_Name) using _Exp) :-
	!.
iTerm((order(Ord for _Name) using _Exp)) :-
	!,
	order(Ord).
iTerm((order(Ord) using _Exp)) :-
	!,
	order(Ord).
iTerm(order(Ord for _Name)) :-
	!,
	order(Ord).
iTerm(order(Ord)) :-
	!,
	order(Ord).
iTerm(parameter(Ss)) :-	% js 900807
	!,
	(cont1(domainConstraints,on) ->
		tupleAsList(Ss,L),
		mapP(declareParameter,L)
	;
		true
	).
iTerm(generatedBy(S,Ds)) :-	% js 900807
	!,
	(cont1(domainConstraints,on) ->
		tupleAsList(Ds,L),
		declareGenerated(S,L)
	;
		true
	).
iTerm(prove(Cs)) :-		% js 910419
	!,
	assertz(doProve(Cs)).
iTerm((S1<S2)) :-
	!,
	appendTypes('$inj',[S2,S1],I12),
	newOp(('$inj' ofType (I12:[S2,S1]))),
	newInjection(I12).
iTerm((op(Op,_P,_Not) : _T)) :-
	doNotUseAsOperator(Op),
	!,
	error("Predefined operator '%'; not allowed in signature.",[Op],in).
iTerm((op(Op,P,Not) : T)) :-
	!,
	convertSig(T,Type),
	assert(notation(Op,P,Not)),
	newOp('ofType'(Op,(_:Type))).
iTerm((op Op : _T)) :-
	doNotUseAsOperator(Op),
	!,
	error("Predefined operator '%'; not allowed in signature.",[Op],in).
iTerm((op Op : T)) :-
	!,
	convertSig(T,Type),
	newOp('ofType'(Op,(_:Type))).
iTerm((cons(Op,_P,_Not) : _T)) :-
	doNotUseAsOperator(Op),
	!,
	error("Predefined operator '%'; not allowed in signature.",[Op],in).
iTerm((cons(Op,P,Not) : T)) :-
	!,
	convertSig(T,Type),
	newConstructor(Op ofType (_:Type)),
	assert(notation(Op,P,Not)).
iTerm((cons Op : _T)) :-
	doNotUseAsOperator(Op),
	!,
	error("Predefined operator '%'; not allowed in signature.",[Op],in).
iTerm((cons Op : T)) :-
	!,
	convertSig(T,Type),
	newConstructor('ofType'(Op,(_:Type))).
iTerm((var VL)) :-
	list(VL),
	!,
	declareVars(VL).
iTerm((var VL)) :-
	listToTuple(VList,VL),
	!,
	declareVars(VList).
iTerm(T) :-
	functor(T,Name,Arity),
	member(Name,[order,setInterpretation,greater,equal,status,constructor]),
	!,
	(knsPragma(Name,Arity) ->
		expandOpsInClauses(T,TE)
	;
		TE = T
	),
	TE.
iTerm(T) :-
	error("illegal element in specification: %.",[T],in).


declareParameter(S) :-
	((parameterDecl(S);generatedByDecl(S,_)) ->
		error("duplicate domain constraint for sort %.",[S],in)
	;
		assert(parameterDecl(S))
	).
declareGenerated(S,Ds) :-
	((parameterDecl(S);generatedByDecl(S,_)) ->
		error("duplicate domain constraint for sort %.",[S],in)
	;
		assert(generatedByDecl(S,Ds))
	).

% added uh 29.11.89
expandOpsInClauses(constructor(F),constructor(O)) :-
	expandOp(F,O),
	!.
expandOpsInClauses(status(L1),status(L2)) :-
	map(expandOpsInPair,L1,L2),
%	append(L1,L2,L3),
	!.
expandOpsInClauses(C1,C2) :-
	C1=..[P,L1],
	map(expandOpsInList,L1,L2),
	C2=..[P,L2].

expandOpsInList(L1,L2) :-
	map(expandOp,L1,L2).

expandOpsInPair(O1:S1,O2:S1) :-
	expandOp(O1,O2).

		 

declareVars(VList):-
	varDecls(VList,TVL),
	(cont(vars,V) ->
		append(TVL,V,VV)
	;	VV=TVL
	),
	vars:=VV.



newConstructor(Op ofType (OT:Type)):-
	uniqueOpName(Op,Type,OT),
	(	hhConstructor(OT)
	;	assert(hhConstructor(OT)),
		makeOp(Op,Type,OSOp),
		assert(hhConstructor(OSOp))),
	newOp('ofType'(Op,(_:Type))).



newOp(OpDef) :-
	OpDef,
	!.
newOp((OO ofType (O:T))) :-
	(sort(OO) ->
		error("% denotes a sort. Sort symbols cannot be used for operators.",[OO],newOp)
	;	true),
	(var(O) ->
		uniqueOpName(OO,T,O)
	;	true
	),
	assert((OO ofType (O:T))),
	length(T,N1),
	N is N1-1,
	osOpName(OO,N,OsOp),			% new version
	(hhConstructor(OsOp) ->
		assert(hhConstructor(O))
	;	true),
	genReduceProcedureForOp(O/N,Clauses),
	mapA(asserta,Clauses),
	precedencesBetweenAuxOps(OsOp,O),	% uh 29.11.89 
%	precedencesBetweenAuxOps(OO,O),		% old version
	greaterThanImplicitOps([injection,domainPred,hhConstructor],O),
	!.


precedencesBetweenAuxOps(_OsOp,_MsOp) :-
	\+ tps_current_ordering(kns_or_neqkns),
	!.
precedencesBetweenAuxOps('$inj',MsOp) :-
	tps_greaterC([['$r-ms',MsOp]]),
	!.
precedencesBetweenAuxOps(OsOp,_MsOp) :-
	hhConstructor(OsOp),
	!.
precedencesBetweenAuxOps(OsOp,MsOp) :-
	tps_greaterC([['$r'/_,OsOp]]),
	tps_greaterC([['$r-ms',MsOp]]).


greaterThanImplicitOps(_,_) :-
	\+ tps_current_ordering(kns_or_neqkns),
	!.
greaterThanImplicitOps(OClasses,O) :-
	repeat((
		append(_,[P|LOClasses],OClasses),
		apply(P,[I]),
		\+ (member(P1,[P|LOClasses]),apply(P1,[O])),
		\+hhConstructor(I),
		tps_greaterC([[O,I]])
		)
	).


predefinePrecedences:-
	\+ tps_current_ordering(kns_or_neqkns),
	!.
predefinePrecedences:-
	_ ofType (O:_),
	greaterThanImplicitOps([injection,domainPred,hhConstructor],O),
	fail.
predefinePrecedences:-
	O ofType (_:[_|T]),
	O\=='$ground',
	length(T,N),
	\+hhConstructor(O/N),
	tps_greaterC([[O/N,'$ground'/1]]),
	fail.
predefinePrecedences.


newInjection(I) :-
	domain(I,[T]),
	mkAtom('X1-%',[T],X),
	mkAtom('X2-%',[T],Y),
	IX=..[I,@X],
	IY=..[I,@Y],
	newNonreductiveEquation(nextIndex,([IX=IY],[@X= @Y]),
		[(origin,orderSortedTrans),(kind,injectivityLaw),
		 (absolutelyNonreductive,[l,r])],_),
	smallerThanOtherOps(I),
	(tps_current_ordering(kns_or_neqkns) ->
		tps_status(I,ms)
	;	true
	),
	!.


smallerThanOtherOps(_) :-
	\+ tps_current_ordering(kns_or_neqkns),
	!.
smallerThanOtherOps(I) :-
	setof1([O,I],[O1,T]^((O1 ofType (O:T)),
			(\+ injection(O);smallerInjection(I,O)),
			\+ hhConstructor(O)),Precs),
	tps_greaterC(Precs).


convertSig([T|Ts],[T|Ts]) :-
	!.
convertSig((Domain -> Codomain),[Codomain|DomainConverted]) :-
	!,
	convertDomain(Domain,DomainConverted),
	!.
convertSig(D,[D]).


convertDomain((Domain*A),DA) :-
	!,
	convertDomain(Domain,Converted),
	append(Converted,[A],DA),
	!.
convertDomain(D,[D]).

varDecls([],[]).
varDecls([V:_T|Vs],TVs) :-
	member((V:_),Vs),
	!,
	error("Multiple declarations for variable %.",[V],varDecls),
	varDecls(Vs,TVs).
varDecls([V:T|Vs],[V:T|TVs]) :-
	admissibleVarName(V),
	(sort(T) -> true
	;	error("Unknown sort % in declaration of variable %.",[T,V],varDecls)
	),
	varDecls(Vs,TVs).


sort('$anyType').
sort('$pred').
sort(S):-
	properSort(S).


properSort(T) :-
	(_ ofType (_:Ts)),
	member(T,Ts),
	T\=='$pred'.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

genInheritanceAxioms :-
	setof1((O1,T1,O2,T2,T1AndT2),
		opsWithInhProps(O1,T1,O2,T2,T1AndT2),
		InhList),
	mapA(genOverloadInhAxiom,InhList),
	setof1((S1,S2,P1,P2),twoPaths(S1,S2,P1,P2),MultiInhs),
	mapA(genInjInhAxiom,MultiInhs),
	!.

% used by genInheritanceAxioms
opsWithInhProps(O1,[RT1|T1],O2,[RT2|T2],T1AndT2) :-
	(O ofType (O1:[RT1|T1])),
	(O ofType (O2:[RT2|T2])),
	O\=='$inj',
	% unterscheiden sich zwei 
	% Operatoren nur in der Zielsorte, so kann die groessere
	% doch in einem lowest parse auftreten, wenn die kleinere erst
	% in einer Erweiterung definiert wird.
	subsortRT(RT1,RT2),
	\+[RT1|T1]=[RT2|T2],
	(RT1=RT2 ->
		\+ map(subsortRT,T2,T1)
	;	true
	),
	\+ opBetween(O,[RT1|T1],[RT2|T2]),
	pairing(T1,T2,T12),
	map(glb,T12,T1AndT2).

% used by genInheritanceAxioms
genOverloadInhAxiom((O1,[RT1|T1],O2,[RT2|T2],T1AndT2)) :-
	inheritanceAxiom(overloadedOp,(O1,[RT1|T1],O2,[RT2|T2],T1AndT2)),
	!.
genOverloadInhAxiom((O1,[RT1|T1],O2,[RT2|T2],T1AndT2)) :-
	inheritanceAxiom(overloadedOp,(O2,[RT2|T2],O1,[RT1|T1],T1AndT2)),
	!.
genOverloadInhAxiom((O1,[RT1|T1],O2,[RT2|T2],T1AndT2)) :-
	pairing(T1AndT2,_Vars,From), 
	pairing(T1,Terms1,To1), 
	pairing(T2,Terms2,To2), 
	map(injection,From,To1),
	map(injection,From,To2),
	R1=..[O1|Terms1], 
	injection((RT1,R1),(RT2,RP)), 
	LP=..[O2|Terms2],
	fromProlog((LP,RP),[],(L,R),_), 
	!, 
	orientInhRule(O2,L,O1,R), % 1
		new('$rule',([],[L=R]),_,[(origin,orderSortedTrans)],_), % 1 
		% alternatively to 1:  axiom may be added as equation 
		% Its orientation from right to left may generate many 
		% equations from the injectivity axiom for the injection 
		% new(equation,([],[L=R]),_,[(origin,orderSortedTrans)],_),
	assertz(inheritanceAxiom(overloadedOp,(O1,[RT1|T1],O2,[RT2|T2],T1AndT2))), 
	!.

% used by genOverloadInhAxiom
orientInhRule(O1,L,O2,R) :-
	(tps_current_ordering(kns_or_neqkns) ->
		tps_greaterC([[O1,O2]])
%		tps_status(O2,ms)
	;	true
	),
	orderSortedOrdering(("
Trying to orient the inheritance equation
	%
from left to right:",[L=R]),
		singleEq,L,R,choices([],[]),_,Mode),
	!,
	(Mode=oriented(_) ->
		legalRule(L,R,Mode,Mode1)
	;	Mode1=Mode
	),
	(Mode1=oriented(_) ->
		true
	;	error("Unable to order inheritance axiom %",[L=R],orientInhRule)
	).

% used by genInheritanceAxioms
genInjInhAxiom((S1,_,P1,P2)) :-
	inheritanceAxiom(inj,([S1|P1],[S1|P2])),
	!.
genInjInhAxiom((S1,_,P1,P2)) :-
	inheritanceAxiom(inj,([S1|P2],[S1|P1])),
	!.
genInjInhAxiom((S1,_,P1,P2)) :-
	mkAtom('X-%',[S1],X),
	injectionT(S1,@X,P1,I1),
	injectionT(S1,@X,P2,I2),
	new('$equation',([],[I1=I2]),_,[(origin,orderSortedTrans)],_),
	assertz(inheritanceAxiom(inj,([S1|P1],[S1|P2]))),
	!.


%>> js 900615

genDomainPred(Sort):-
	'$ground' ofType (_:['$pred',Sort]),
	!.
genDomainPred(Sort):-
	newOp('$ground' ofType (_:['$pred',Sort])),
	newConstructor('$true'   ofType (_:['$pred'])).


/* dead code
genTrivialDomainAxiom(Sort):-
	domainAxiomVar(('',Sort),V),
	domainEquation(Eq,Sort,V),
	E=([],[Eq]),
	!,
	genAxiom(E,domainAxiom).
*/


genDomainAxiom(Operator,_Sort,_ArgTypes):-
	domainAxiomHasBeenGenerated(Operator),
	!.
genDomainAxiom(Operator,Sort,ArgTypes):-
	length(ArgTypes,N),
	numberedList(ArgTypes,NArgTypes),
	map(domainAxiomVar,NArgTypes,Vars),
	map(domainCondition,Vars,Conditions),
	Term=..[Operator|Vars],
	domainEquation(Conclusion,Sort,Term),
	(hhConstructor(Operator) ->
	    (N=1 ->
		(ConclusionLeft='$true-$pred')=Conclusion,
		[ConclusionRight='$true-$pred']=Conditions,
		genAxiom(([],[ConclusionLeft=ConclusionRight]),domainAxiom)
	    ;
		genAxiom((Conditions,[Conclusion]),domainAxiom),
		mapA(lambda([Condition],
			    genNonRedAxiom(([Conclusion],[Condition]),
					   domainAxiom)),
		     Conditions)
	    )
	;
	    genAxiom((Conditions,[Conclusion]),domainAxiom)
	),
	assert(domainAxiomHasBeenGenerated(Operator)).


genAxiom(Eq,Origin) :-
	(relationTypes(Ts),member(T,Ts),object(_,T,Eq) ->
	    true
	;
	    new('$equation',Eq,_,[(origin,Origin)],_)
	).

genNonRedAxiom(Eq,Origin) :-
	(relationTypes(Ts),member(T,Ts),object(_,T,Eq) ->
	    true
	;
	    newNonreductiveEquation(nextIndex,Eq,
		[(origin,Origin),(absolutelyNonreductive,[l,r])],_)
	).


genDomainPreds:-
	properSort(S),
	genDomainPred(S),
	fail.
genDomainPreds :-
	predefinePrecedences.


genDomainAxioms :-
	_Op ofType (Operator:[Sort|ArgTypes]),
	properSort(Sort),
	genDomainAxiom(Operator,Sort,ArgTypes),
	fail.
genDomainAxioms.


genDomainAxiomsAndComplete:-
	cont(ctr('$equation'),N),
	genDomainPreds,
	genDomainAxioms,
	recordAxioms([]),	% for finalization of state
	cont(ctr('$equation'),M),
	(M=N ->
		true
	;	
		sPrint("
completing system with domain equations ...
",[]),
		pushState(undo),
		cNoFreezeNoUndo),
	!.
%<<		
		
domainPredicate(Sort,Pred) :-
	'$ground' ofType (Pred:['$pred',Sort]).


domainEquation(E,Sort,T) :-
	domainPredicate(Sort,Op),
	G=..[Op,T],
	(G='$true-$pred')=E.


domainCondition(@X,Eq):-
	!,
	termType(@X,T),
	domainEquation(Eq,T,@X),
	!.
domainCondition(X,C):-
	domainCondition(@X,C).

domainAxiomVar((I,T),@X):-
	mkAtom('X%-%',[I,T],X),
	!.

addDomainConditions((C,E),(CC,E)):-
	vars((C,E),VRNames),
	map(lambda([N,V],V= @N),VRNames,VR),
	reverse(VR,V),
	map(domainCondition,V,C1),
	append(C1,C,CC).

addNonParameterDomainConditions((C,E),(CC,E)):-
	vars((C,E),VRNames),
	map(lambda([N,V],V= @N),VRNames,VR),
	split(lambda([V],(termType(V,S),parameterSort(S))),VR,_,NonParVR),
	reverse(NonParVR,V),
	map(domainCondition,V,C1),
	append(C1,C,CC).


% used by genInheritanceAxioms
twoPaths(S1,S2,P1,P2) :-
	path(subsort,S1,S2,[],P1),
	path(subsort,S1,S2,[],P2),
	length(P1,N1),
	length(P2,N2),
	(N1>1;N2>1),
	intersection_s(P1,P2,[S2]).



loadModule(_Depth,From,From).
loadModule(Depth,From,user):-
	sPrint("%[loading %...]",[Depth,From]),
	nl,
	rs(From),
	!.
loadModule(Depth,user,To):-
	sPrint("%[storing to %...]",[Depth,To]),
	nl,
	ss(To),
	!.
loadModule(Depth,F,T):-
	loadModule(Depth,F,user),
	loadModule(Depth,user,T),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dead code ?


genInjections :-
	subsort(S1,S2),
	appendTypes('$inj',[S2,S1],I12),
	newOp(('$inj' ofType (I12:[S2,S1]))),
	newInjection(I12).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

moduleName :-
    cont(moduleName,Module),
    !,
    write(Module).
moduleName :-
    error("no name associated with current specification",[],moduleName),
    fail.


orderName :-
    cont(orderName,Order),
    !,
    write(Order).
orderName :-
    error("no order name associated with current specification",[],orderName),
    fail.




% Zu Experimentierzwecken
% indProve/1 jetzt in prove.pl

/*
indProve(E,Schema):-
	sPrint("
Trying to prove
	%
using the induction scheme
	%
-----------

",['$list'(E,'
','',''),'$list'(Schema,'
	','','
	')]),
	asList(Schema,SchemaL),
	map(internalEqRep,SchemaL,SchemaI),
	ss('$beforeIndProve'),
	cont(ctr('$rule'),N),
	addEquations(SchemaI),
	indProve:=true,
	cNoFreezeNoUndo,
	cont(ctr('$rule'),N),
	ps('$beforeIndProve'),
	lemma:=true(SchemaI),
	lemma(E),
	lemma:=false,
	sPrint("

-----------

	%
is inductively valid,

	%
will be assumed as lemma.",
		['$list'(Schema,'
	','',''),'$list'(E,'
','','')]). 

		



lemma(E):-
	asList(E,EL),
	map(internalEqRep,EL,EI),
	cont(ctr('$equation'),M),
	addEquations(EI),
%	ignoreOutput(enrich('/tmp/lemma')),
	cont(ctr('$equation'),N),
	M1 is M+1,
	for(M1,N,lambda([I],orderEq(I))),
	!.





reProve(Ls):-
	map(proofTask,Ls,Schemes),
	pushState(undo),
	repeat(removeLemma),
	mapP(lambda([(E,S)],indProve(E,S)),Schemes).
	

proofTask(r(I),('$osTerm'(E),OS)):-
	'$rule'(I,E),
	lemma(I,'$rule',indScheme(S)),
	map(lambda([(C,D),Y],Y='$equation'(C,D)),S,OS).
proofTask(n(I),('$osTerm'(E),OS)):-
	'$nonoperational equation'(I,E),
	lemma(I,'$nonoperational equation',indScheme(S)),
	map(lambda([(C,D),Y],Y='$equation'(C,D)),S,OS).


removeLemma:-
	lemma(I,T,indScheme(_S)),
	dispose(T,I).

*/
