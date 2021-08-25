
/*
 *	file:		eqCompletion.pl
 *	version:	1.5
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates to execute the single steps of
 *	the completion process, including user interaction during
 *	completion.
 *
 *	history:
 *	891010	js	Added this comment
 *	900118  uh	Changed definition of
 *			sp_member/2:
 *			The fact '$maySuperpose'(...) is now retracted
 *			after both calls of reducible failed, because the
 *			fact is needed in the case one of the calls succeeds.
 *	900528	hg	Changed definition of 
 *			tobeKeptFormula
 *			
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

:- dynamic explain/1.

explain(X):-
	X\==forwardChaining.

/*----------------------------------------------------------------------*
 *			 explain(Kind,Info)				*
 *									*
 * if explain(Kind) then print the Information onto the screen		*
 *----------------------------------------------------------------------*/

explain(Rule,Instance) :-
	(explain(Rule) ->
		nl,
		notify(Rule,Instance)
	;
		true
	),
	!.
explain(_,_).


/*----------------------------------------------------------------------*
 *			 notify(Kind,Info)				*
 *----------------------------------------------------------------------*/

notify(orderEq,[_E,R]) :-
	sPrint("new rule % .
",['$oref'('$rule',R)]).


notify(narrow,[Type,I]) :-
	!,
	sPrint("Trying to eliminate
	%
	by bounded narrowing ... ",['$oref'(Type,I)]).


notify(redEq,[Type,Old,New]) :-
	!,
	sPrint("The equation
	%
	has been reduced to
	% .
",['$oref'(Type,Old),'$oref'('$equation',New)]).


notify(contextualReduction,[Type,I,E]) :-
	!,
	sPrint("The %
	%
	is reduced to
	%.
",[Type,'$oref'(Type,I),E]).


notify(forwardChaining,[Old,CO,Using,CU,NewC]) :-
	!,
	sPrint("The condition in
	%
	may be forward chained, using the nonoperational equation
	%,
	to %.
",[Old,'$oref'('$nonoperational equation',Using),'$conjunction'(NewC),'$term'(CO),'$term'(CU)]).


notify(formulaEliminated,[T,E]) :-
	sPrint("% deleted:
	%
",[T,'$oref'(T,E)]).

notify(oldProof,[T,E]) :-
	sPrint("% deleted (simpler proof previously constructed):
	%
",[T,'$oref'(T,E)]).


notify(depDel,[E]) :-
	sPrint("The dependent equation
	%
	has been deleted.
",[E]).


notify(elimNOpEq,[E]) :-
	sPrint("The equation
	%
	has been deleted.
",['$oref'('$nonoperational equation',E)]).


notify(redRuleRight,[R,'$rule'(R1)]) :-
	sPrint("The rule
	%
	has been reduced to the new rule
	% .
", ['$oref'('$rule',R),'$oref'('$rule',R1)]).

notify(redAuxRule,[R]) :-
	sPrint("The auxiliary rule for
	%
	has been deleted.
", ['$oref'('$rule',R)]).


notify(redRuleLeft,[R,eq(E)]) :-
	var(E),
	sPrint("The rule
	%
	has been reduced to the identity.
", ['$oref'('$rule',R)]).
notify(redRuleLeft,[R,eq(E)]) :-
	sPrint("The rule
	%
	has been reduced on its left side to the equation
	% .
", ['$oref'('$rule',R),'$oref'('$equation',E)]).


notify(trivialSuperFound,[C,E]) :-
	sPrint("convergent instance %",['$equation'(C,E)]).
notify(superFound,[E]) :-
	sPrint(
"new clause %",[E]).
notify(superFrom,[I,_TI,TyJ,J,LJ,TJ]) :-
	sPrint(
"of	%
by superposing
	% 
on %.
",['$oref'(TyJ,J),'$oref'('$rule',I),'$superpositionOcc'(LJ,TJ)]).




notify(toConstraint,[(C,D)]) :-
	sPrint("The equation % 
will be considered as nonoperational.
",[(C,D)]).

notify(toConstraint,[EqRef]) :-
	sPrint("The equation % 
will be considered as nonoperational.
",['$oref'('$equation',EqRef)]).



notify(subsumed,[E,C,P]) :-
	sPrint("Eliminating equation
	% 
with complexity
	%:

%.
",[E,C,'$proofStep'('',P)]).





/***********************************************************************/


repeatRed('$rule') :-
	mayBeReducible(I,'$rule',_),
	redRule(I),
	fail.
repeatRed('$equation') :-
	mayBeReducible(I,'$equation',_),
	redEq(I),
	fail.
repeatRed(nopEq) :-
	mayBeReducible(I,'$nonoperational equation',_),
	redNOpEq(I),
	fail.
repeatRed(_).


/*----------------------------------------------------------------------*
 * 				redEq(EqI)				*
 *									*
 * Reduces equation with index "EqI". The predicate fails if the 	*
 * equation does not exist of if the equation cannot be reduced or if	*
 * the equation is eliminated during reduction.				*
 *----------------------------------------------------------------------*/

redEq(EqI) :-
	reduceIfPossible('$equation',EqI,EN,Result),
	(Result=reduced ->
		origin(EqI,'$equation',K),
		complexityBound(EqI,'$equation',ComplN),
		new('$equation',EN,_,[(origin,K),(complexityBound,ComplN),
			(reducedTag,reduced)],RefRed),
		explain(redEq,['$equation',EqI,RefRed]),
		dispose('$equation',EqI)
	;	(Result=irreducible ->
			retract(mayBeReducible(EqI,'$equation',_)),
			fail
		;	true
		)
	).

/*----------------------------------------------------------------------*
 *			    redNOpEq(EqI)				*
 *									*
 * Reduces nonoperational equation with index "EqI". The predicate	*
 * fails if the equation does not exist of if the equation cannot be 	*
 * reduced or if the equation is eliminated during reduction.		*
 *----------------------------------------------------------------------*/


redNOpEq(EqI) :-
	mayBeReducible(EqI,'$nonoperational equation',_),
	reduceIfPossible('$nonoperational equation',EqI,EN,Result),
	(Result=reduced ->
		origin(EqI,'$nonoperational equation',O),
		new('$equation',EN,_,[(reducedTag,reduced),(origin,O)],RefRed),
		explain(redEq,['$nonoperational equation',EqI,RefRed]),
		dispose('$nonoperational equation',EqI)
	;	(Result=eliminated ->
			true
		;	retract(mayBeReducible(EqI,'$nonoperational equation',_)),
			fail
		)
	).





/*----------------------------------------------------------------------*
 *   		 reduceIfPossible(Type,Index,(C,D),Result)		*
 *									*
 * produces reduced version of the formula of type "Type" with index 	*
 * "Index", if it does not fail. 					*
 * Result = reduced => C => D is the reduced form of the formula	*
 * Result = irreducible => C => D is the given formula			*
 * Type = equation or 'nonoperational equation' or rule			*
 * Result = irreducible or reduced					*
 *----------------------------------------------------------------------*/

reduceIfPossible(Type,I,EqReduced,Res) :-	
	nontrivial(Type,I,EqReduced,Res),
	!.
reduceIfPossible(Type,I,E,irreducible):-
	object(I,Type,E).




/*----------------------------------------------------------------------*
 *	 nontrivial(Type,Index,						*
 *			(RedCondition,RedConclusion),Result)		*
 *----------------------------------------------------------------------*/
nontrivial(Type,I,Eq,Res) :-
	mayBeReducible(I,Type,_Kind),
	object(I,Type,Eq),
	Eq=([_|_],_),
	simplerProofExistsFor(Eq,Compl),
	complexityBound(I,Type,ComplI),
	(	Compl=ComplI
	;	fromProlog([],Compl),
		simplerProof(Type,I,ComplI,Eq,Compl)),
	!,
	retract(mayBeReducible(I,Type,_)),
	explain(oldProof,[Type,I]),
	dispose(Type,I),
	Res=eliminated.
nontrivial(Type,I,E1,Res) :-
	mayBeReducible(I,Type,Kind),
	object(I,Type,Eq),
	prologVariant(I,Type,(CP,ComplP,_)),
	(	(functor(Kind,contextually,_) ->
			contextualReduction(Type,I,Eq,EqR)
		;	EqR=Eq
		),
		tobeKeptFormula(Type,I,EqR,E1)
	->	resultOfReduction(Type,I,Eq,E1,Res)
	;
		retract(mayBeReducible(I,Type,_)),
		(CP=([_|_],_) -> 
			assert(simplerProofExistsFor(CP,ComplP))
		;	true),
		explain(formulaEliminated,[Type,I]),
		dispose(Type,I),
		Res=eliminated
	).




resultOfReduction(Type,I,(C,D),(CR,DR),Res) :-
	retractMayBeReducible(Type,I),
	(D=DR, C=CR ->
		Res=irreducible
	;	Res=reduced
	),
	!.
							


retractMayBeReducible('$equation',_) :-
	!.
retractMayBeReducible(Type,I) :-
	try(retract(mayBeReducible(I,Type,_))),
	!.


% changed hg 28.05.90
tobeKeptFormula(Type,I,(C,D),E) :-
	storeStartTime,
	stop:=false,
	complexityBound(I,Type,ComplI),
	(cont(trace(elim(I)),on) ->
		sPrint("
Trying to eliminate % with complexity complexity
	%...
",['$oref'(Type,I),'$term'(ComplI)])
	;	true),
	ComplI=(_,[CN]),
	(\+msComplexity(CN) ->
	 	msComplexity:=false
	;       (	Type='$equation',
	 		origin(I,'$equation',user) ->
			msComplexity:=false
	;		msComplexity:=true)),
	(cont(trace(elim(I)),ptrace) -> trace;true),
	tobeKeptFormula(0,Type,I,(C,D),(C2,D2)),
	(C\==C2 ->
		simplifiedCondition(C,(C2,D2),E)
	;	E=(C2,D2)).



storeStartTime:-
	totalRuntime(T),
	beginElimination:=T.

tooLate:-
	completion_interrupted.
tooLate:-
	cont(beginElimination,B),
	totalRuntime(E),
	Elapsed is E-B,
	(	allowedEliminationTime(Allowed)
	;	Allowed = 300),			% time in cpu seconds
	!,
	Elapsed>Allowed.


tobeKeptFormula(_,_,_,([],D),([],D)) :-
	!.
tobeKeptFormula(_,'$rule',_,R,R) :-
	!.
tobeKeptFormula(Depth,_,_,R,R) :-
	(forwardChainingDepth(D) ->
		true
	;	D=1
	),
	(	Depth >= D
	;	tooLate
	),
	!.
tobeKeptFormula(Depth,Type,I,(C,D),E) :-
	forwardChain(Type,I,(C,D),(C1,D1)),
	\+ subsumedBounded(Type,I,(C1,D1)),
	(C\==C1,\+cont(stop,true) ->
		Depth1 is Depth+1,
		tobeKeptFormula(Depth1,Type,I,(C1,D1),E)
	;	E=(C1,D1)).




simplifiedCondition(C,(C1,D1),(C2,D1)):-
%	C : alte Bedingung
%	C1: Bedingung nach forward chaining
%	C2: endg"ultige Bedingung; wird hier ausgerechnet
	unlimitedSimplification:==true,
	contextualReduction(_,_,(C1,['true-bool'='false-bool']),(CN,_DN)),
	unlimitedSimplification:==false,
	forwardChainedCondition:=C1,	% js 900628 % hg
	(subset(CN,C1),C\==CN ->
		literalsOf(C,CTs),
		literalsOf(CN,CNTs),
		multiSetOrdering(("Simplifying condition:",[]),CTs,CNTs,choices([],[]),_Ext,M),
		(M=oriented(_) ->
			C2=CN
		;	C2=C)
	;	C2=C).
simplifiedCondition(_C,_E1,_E2):-
	unlimitedSimplification:==false,
	!,
	fail.


forwardChain(Type,I,(C,D),EN):-
	nextOnForwardChain(Type,I,(C,D),Eqs),
	append(C,Eqs,C1),
	(Eqs=[] ->
		EN=(C,D)
	;	contextualReduction(Type,I,(C1,D),(C2,D2)),
		((cont1(domainConstraints,on);cont1(indProve,true))
		->	union(Eqs,C2,C3)
		;	C3=C2),
		EN=(C3,D2)
	),
	!.


allowedLevel(L) :-
	L<3.

addToEncounteredFormulas((C,D)):-
	assertz(encounteredFormula((C,D))).

nextOnForwardChain(Type,I,(C,D),Eqs):-
	setof1(Eq,osfc(Type,I,(C,D),Eq),Eqs),
	cont(trivial,false),
	!.




osfc(Type,I,(C,D),Eq):-
	trivial:=false,
	oneStepForwardChaining(Type,I,J,Complexity,(C,D),Eq,ComplJ),
	append(C,[Eq],CN2),
	explain(forwardChaining,['$equation'(C,D),Complexity,J,ComplJ,CN2]),
	(contextualReduction(Type,I,(CN2,D),_) ->
		true
	;	trivial:=true,
		!,
		fail).



/*----------------------------------------------------------------------*
 *   contextualReduction(Type,I,(C0,[DL0=DR0]),Compl0,(CN,DN),ComplN)	*
 *									*
 * contextually reduces C0=>DL0=DR0. It fails, if the resulting formula *
 * is trivial. Otherwise, CN=>DN is the contexually reduced result	*
 * Adjusts complexity bounds Compl0 to ComplN if substutions are	*
 * eliminated/								*
 *----------------------------------------------------------------------*/



contextualReduction(T,I,E1,E2):-
	number(I),
	!,
	complexityBound(I,T,Compl1),
	redIndexes(I,T,Red),
	contextualReduction1(T,E1,(Compl1,Red),E2,Substs),
	applySubsts(Substs,Compl1,Compl2),
	seta(T,I,complexityBound,Compl2).
contextualReduction(_T,I,E1,E2):-
	var(I),
	!,
	contextualReduction1('$equation',E1,(min,('infty','infty')),E2,_Substs).
contextualReduction(([],[A=B]),(Compl,(RIA,RIB)),([],[A1=B1]),[]):-
	reduceC([],(Compl,RIA),A,AN),
	reduceC([],(Compl,RIB),B,BN),
	!,
	\+ ac_matchvf(AN,BN),
	detachInjections((AN=BN),(A1=B1)).
contextualReduction(E,RedIndexes,EN,Substs):-
	contextualReduction1('$equation',E,RedIndexes,EN,Substs).


contextualReduction1(Type,(C0,[DL0=DR0]),(Compl,(RIL,RIR)),(CN,DN),Substs) :-
 	DL0\==DR0,
	elimSubstitutions(Type,(C0,[DL0=DR0]),(C,[DL=DR]),Substs0),
	reducedCondition(Compl,C,CN0),
	detachConstrs(CN0,CN2),

		% satisfiability test
	!,
	satisfiable(CN2),

		% contextual reduction of conclusion with reduced conditions

	condReduceClausesAndNonOConds(CN2,CCls1,CNO),
	reduceC(CCls1,(Compl,RIR),DR,DRN),
	reduceC(CCls1,(Compl,RIL),DL,DLN),
	!,
	\+ contextuallyEqual(CNO,DLN,DRN),
	!,
	sortIndAC((CN2,[DLN=DRN]),(CN1,[DLN2=DRN2])),
	detachInjections((DLN2=DRN2),(DLN1=DRN1)),
	%	% recursive call, if condition has been reduced

%	(C=CN1,DLN=DLN2,DRN=DRN2 ->
		CN=CN1,
		DN=[DLN1=DRN1],
		Substs=Substs0
%	;
%		relaxedRedIndex(DL,RIL,DLN1,RIL1),
%		relaxedRedIndex(DR,RIR,DRN1,RIR1),
%		contextualReduction1(Type,(CN1,[DLN1=DRN1]),(Compl,(RIL1,RIR1)),(CN,DN),Substs1),
%		append(Substs0,Substs1,Substs))
	,
	!.


detachInjections((IT1=IT2),(T1=T2)):-
	IT1=..[I,T11],
	IT2=..[I,T21],
	injectiveOperator(I),
	!,
	detachInjections((T11=T21),(T1=T2)).
detachInjections(E,E).

injectiveOperator(O):-
	injection(O).
injectiveOperator(O):-
	kind(J,'$nonoperational equation',injectivityLaw),
	'$nonoperational equation'(J,([L=_],_)),
	functor(L,O,1),
	!.

detachConstructors((CT1=CT2),Es):-
	CT1=..[I|Ts1],
	CT2=..[I|Ts2],
	length(Ts1,N),
	length(Ts2,N),
	isConstructor(I/N),
	pairing(Ts1,Ts2,Es1),
	mapF(lambda([(T1,T2),(T1=T2)],true),Es1,Es),
	!.
detachConstructors(E,[E]).

detachConstrs(Es1,Es2):-
	map(detachConstructors,Es1,EsL),
	rReduce(append,[],EsL,Es2),
	!.


/*----------------------------------------------------------------------*
 *        	    reducedCondition(Compl,[L=R|Es],Es1)		*
 *		   reducedCondition(Compl,EL,[L=R|ER],Es1)		*
 *----------------------------------------------------------------------*/

reducedCondition(Compl,Es,Es1):-
	map(detachInjections,Es,EsI),
	reducedCondition1(Compl,EsI,Es1).

reducedCondition1(_,[],[]).
reducedCondition1(Compl,[L=R],Es) :-
	reduceC([],(Compl,infty),L,LN),
	reduceC([],(Compl,infty),R,RN),
	elimId((LN=RN),Es).
reducedCondition1(Compl,[L=R|Es],Es1) :-
	reducedCondition(Compl,[],[L=R|Es],Es1),
	!.

reducedCondition(Compl,EL,[L=R|ER],Es1) :-
	append(EL,ER,Es),
	condReduceClausesAndNonOConds(Es,CCls,NOEs),
	reduceC(CCls,(Compl,infty),L,LN),
	reduceC(CCls,(Compl,infty),R,RN),
	!,
	detachInjections((LN=RN),(L1=R1)),
	elimIdAndDuplications((L1=R1),NOEs,E1N),
	append(EL,E1N,EL1),
	reducedCondition(Compl,EL1,ER,Es1),
	!.
reducedCondition(_,C,[],C).


elimIdAndDuplications((L=R),Es,[]):-
	contextuallyEqual(Es,L,R),
	!.
elimIdAndDuplications(E,_,[E]).


/*----------------------------------------------------------------------*
 *        	     elimSubstitutions(T,(C0,D0),(C,D)			*
 *		      assignment((T= @(V)),(@(V)=T))			*
 *----------------------------------------------------------------------*/

assignment((@(V)=T),(@(V)=T)) :-
	notOccursIn(@V,T).
assignment((T= @(V)),(@(V)=T)) :-
	notOccursIn(@V,T).

elimSubstitutions('$rule',E,E,[]) :-
	!.
elimSubstitutions(T,(C0,D0),(C,D),[[(V,Term)]|Substs]) :-
	member(E,C0),
	expandableAssignment(E,(@V=Term)),
	deleteOne(C0,E,C1),
	applySubst([(V,Term)],(C1,D0),(C2,D2)),
	!,
	elimSubstitutions(T,(C2,D2),(C,D),Substs),
	!.
elimSubstitutions(_,E,E,[]).




expandableAssignment(E,(@V=Term)):-
	assignment(E,(@V=Term)),
	(	cont(unlimitedSimplification,true)
	;	constrTerm(Term)),
	!.




/*----------------------------------------------------------------------*
 *        	     	     satisfiable(C)				*
 *			previouslyUnsatisfiable(CP)			*
 *----------------------------------------------------------------------*/

satisfiable(C) :-
	mapA(satisfiableEq,C).

satisfiableEq((L=R)):-
	detachConstrTerm(L,CL,_),
	detachConstrTerm(R,CR,_),
	!,
	CL=CR.



/* satisfiable(C) :-
 *	previouslyUnsatisfiable(D),
 *	splitCondition(D,C,_,[]),
 *	!,
 *	fail.
*/

previouslyUnsatisfiable(CP) :-
	prologVariant('$nonoperational equation',((CP,[L=R]),_,_)),
	atomic(L),
	atomic(R),
	hhConstructor(L),
	hhConstructor(R),
	L\==R.


/*----------------------------------------------------------------------*
 *	 oneStepForwardChaining('$nonoperational equation',J,I,		*
 *				ComplexityBound,(C,D),C1,ComplI)	*
 *	   chainByNOpEq(I,ComplexityBound,(C,_),C1,ComplIP)		*
 *----------------------------------------------------------------------*/

oneStepForwardChaining(T,J,I,ComplexityBound,(C,D),C1,ComplI) :-
	C\==[],
	!,
	(tryfw(I);true),
	'$nonoperational equation'(I,([_|_],DI)),
	realistic(T,I,J,DI,D),
	chainByNOpEq(T,J,I,ComplexityBound,(C,D),C1,ComplI).

realistic(T,I,J,DI,D):-
	kind(I,'$nonoperational equation',K),
	functor(K,other,_),
	size(I,'$nonoperational equation',S),
	smallConstraint(S),
	(cont(msComplexity,true) ->
		complexityBound(I,'$nonoperational equation',(_,[Co])),
		msComplexity(Co)
	;	true),		
	(T\=='$nonoperational equation'->
		I\==J
	;	true),
	(vars(DI,[]) ->
		D\==DI
	;	true
	),
	!.

chainByNOpEq(T,J,I,Compl,(C,_D),DP,ComplIP) :-
%	isReductive(I,'$nonoperational equation',no),
	(origin(J,T,super(_,_,'$nonoperational equation',TryFirst,_,_)) ->
		true
	;	TryFirst=0),
	(	prologVariant(TryFirst,'$nonoperational equation',E),
		I=TryFirst
	;	prologVariant(I,'$nonoperational equation',E),
		I\==TryFirst),
	E=((CP,[DP]),ComplIP,_Env),
	\+redPredGenerated(I),
	impInstanceOfAll(C,CP),
	appropriateForFChaining(C,DP),
	fromProlog([],ComplIP),	% to instantiate remaining variables in complexity
	complexityBound(J,T,Compl),
	(cont(trace(elim(J)),on) ->
		nl,
		sPrint("Attempting forward chaining using
	% 
with instantiated complexity
	%... ",['$oref'('$nonoperational equation',I),'$term'(ComplIP)])
	;	true),
	simplerProof(T,J,Compl,(CP,[DP]),ComplIP),
	(cont(trace(elim(J)),on) ->
		write('succeeded.')
	;	true).




unflatten(C,C1):-
	setof(E,chainByRefl(C,E),C2),
	append(C,C2,C1),
	(cont(trace(elim(X)),on),var(X) ->
		sPrint("
Conditions % added.",[C2])
	;	true),
	!.
	
chainByRefl(C,(Context=Context1)):-
	member(E,C),
	assignment(E,(V=Term)),
	member((L=R),C),
	E\==(L=R),
	(Try=L;Try=R),
	varContext(Try,V,Context,Hole),
	copy_term((Context,Hole),(Context1,Hole1)),
	Hole=V,
	Hole1=Term,
	\+member((Context=Context1),C),
	Compl1=((['$r'(Context1)],['$r'(Context1)]),['$r'(Context1)]),
	verifyComplexity(([V=Term],[Context=Context1]),Compl1).


	

varContext(C,V,S1,Hole):-
	subtermAndContext(C,(_Context,_H,S)),
	S\==V,
	\+notOccursIn(V,S),
	V= @N,
	applySubst([(N,Hole)],S,S1).


appropriateForFChaining(C,DP):-
	varfree(DP),
	DP=(L=R),
	reduceC([],(min,infty),L,LN),
	reduceC([],(min,infty),R,RN),
	\+ ac_matchvf(LN,RN),
	\+ impInstanceOfAll(C,DP),
	!.

	
/*----------------------------------------------------------------------*
 *	 		boundedProof(T,Terms)				*
 *----------------------------------------------------------------------*/



simplerProof(_T,_I,(('$infty','$infty'),NCI),_,(_RC,NC)):-
	!,
	boundedProof(NCI,NC).
simplerProof(_T,_I,((RCL,RCR),_NC),_,(_RC,NC)):-
	boundedProof(RCR,NC),
	boundedProof(RCL,NC).
simplerProof(T,I,(RCI,NCI),EJ,(RC,NC)):-
	nonvar(T),
	boundedProof(NCI,NC),
	object(T,I,EI),
	trace,
	leReductive(EI,RCI,EJ,RC,(RCIL,RCIR),(RCJL,RCJR)),
	!,
	boundedProof(RCIL,RCJL),
	boundedProof(RCIR,RCJR).



leReductive(E,RC,E1,RC1,RC,RC1):-
	leReductive1(E,E1).
leReductive((C,[L=R]),(RCL,RCR),E1,RC1,(RCR,RCL),RC1):-
	leReductive1((C,[R=L]),E1).
leReductive((C,[L=R]),(RCL,RCR),(C1,[L1=R1]),(RC1L,RC1R),(RCR,RCL),(RC1R,RC1L)):-
	leReductive1((C,[R=L]),(C1,[R1=L1])).
leReductive(E,RC,(C1,[L1=R1]),(RC1L,RC1R),RC,(RC1R,RC1L)):-
	leReductive1(E,(C1,[R1=L1])).



leReductive1((C,[L=R]),(C1,[L1=R1])):-
	setof1(greater(L,T),
		S^(member((T=S),C);member((S=T),C)),InEqualities),
	setof1(greater(L,T),
		S^(member((T=S),C1);member((S=T),C1)),InEqualities1),
	IE=[greater(L,R)|InEqualities],
	IE1=[greater(L1,R1)|InEqualities1],
	inTransitive(InEqualities),
	!,
	mapP(lambda([InEq],impliesGT(IE,InEq)),IE1),
	!.

inTransitive(InEqualities):-
	transClosure(lambda([X,Y],member(greater(X,Y),InEqualities)),A,A),
	!,
	fail.
inTransitive(_InEqualities).

impliesGT(IE,greater(L,R)):-
	member(greater(L,T),IE),
	impliesGTRT(IE,greater(T,R)).
impliesGT(_IE,greater(L,R)):-
	greaterInRedOrder(L,R).


impliesGTRT(_IE,greater(L,L)).
impliesGTRT(IE,greater(L,R)):-
	member(greater(L,T),IE),
	impliesGTRT(IE,greater(T,R)).
impliesGTRT(_IE,greater(L,R)):-
	greaterInRedOrder(L,R).


	



boundedProof('$infty',_):-
	!.
boundedProof(_,'$infty'):-
	!,
	fail.
boundedProof(min,_):-
	!,
	fail.
boundedProof(C11,C22):-
	asList(C11,C1),
	asList(C22,C2),	% alte freeze states !
	member(T1,C1),
	mapP(lambda([T2],boundedProof1(T1,T2)),C2),
	!.

boundedProof1(T,Terms) :-
	(cont(os,true) ->
		Ext=no
	;	true
	),
	extendedMultiSetOrdering(("
Checking complexity of simplification proof:",[]),T,Terms,Ext,M),
	M = oriented(_),
	!.


smallConstraint(eq(S,_,_)) :-
	(cont1(smallClause,N);default(smallClause,N)),
	!,
	S<N.




impInstanceOfAll(_,[]) :- !.
impInstanceOfAll(L1,[X=Y|L2]) :-
	ac_uniAC(X,Y),
	impInstanceOfAll(L1,L2).	
impInstanceOfAll(L1,[P|L2]) :-
	containsInstanceOf(L1,P),
	impInstanceOfAll(L1,L2).	
impInstanceOfAll(L1,(X=Y)) :-
	impInstanceOfAll(L1,[X=Y]).




containsInstanceOf(C,[L=R]) :-
	containsInstanceOf(C,(L=R)).
containsInstanceOf(C,(L=R)) :-
	member((LL=RR),C),
	ac_match(LL,L),
	ac_match(RR,R).
containsInstanceOf(C,(L=R)) :-
	member((RR=LL),C),
	ac_match(LL,L),
	ac_match(RR,R).





/*----------------------------------------------------------------------*
 *			orderEq(EqI)					*
 *									*
 *	 reduce and try to orient equation with index EqI 		*
 *----------------------------------------------------------------------*/

orderEq(EqI) :-
	nextEquation(EqI),
	reduceIfPossible('$equation',EqI,EN,Result),
	Result\==eliminated,
	complexityBound(EqI,'$equation',ComplN),
	redIndexes(EqI,'$equation',RI),
	equationDisplayed:==false,
	orientEq(ask,EqI,EN,EN0,SuperPTerms,Mode),
	origin(EqI,'$equation',K),
		Attrs=[(origin,K),(redIndexes,RI)],
 		try(retract(mayBeReducible(EqI,'$equation',_))), /* its here that
		the reducible info can be deleted. An abortion in orientEq
		would then leave the info there.
		*/
	(Mode=hasBecomeReductive ->
		new('$rule',EN0,_,[(superpositionTerms,SuperPTerms),(origin,K)],RuleRef),
		explain(orderEq,[EqI,RuleRef]),
		dispose('$equation',EqI)
	;
		(Mode=nonoperational ->
			newNonreductiveEquation(EqI,EN,Attrs,NewEqRef),
			explain(toConstraint,[EN]),
		   	dispose('$equation',EqI)
		;
		   	(Result=irreducible ->
				seta('$equation',EqI,priority,postponed),
				fail
		    	;
				new('$equation',EN,_,[(priority,postponed),(reducedTag,reduced),(complexityBound,ComplN)|Attrs],NewEqRef),
		           	explain(redEq,['$equation',EqI,NewEqRef]),
			   	dispose('$equation',EqI),
			   	fail
		   	)
		 )
	).



nextEquation(I,N):-
	setof1((SortTerm,J),sortCriterium('$equation',J,SortTerm),Sorted),
	length(Sorted,N),
	nextEqs:==Sorted,
	member((_,I),Sorted),
	sPrint("
[There are % equations in the system.]
",[N]),
	!.

nextEquation(I):-
	cont1(nextEqs,Sorted),
	member((_,I),Sorted),
	priority(I,'$equation',P),
	P\==postponed.

mayOrder(I):-
	cont1(nextEqs,Sorted),
	member((eq(W,_,_),I),Sorted),
	(cont1(smallClause,Limit) -> true; default(smallClause,Limit)),
	(W<Limit ->
		orderEq(I)).

/*----------------------------------------------------------------------*
 *			   nopEq(EqI)					*
 *									*
 *	 declare equation with index EqI as nonoperational 		*
 *----------------------------------------------------------------------*/

nopEq(EqI) :-
	'$equation'(EqI,_),
	reduceIfPossible('$equation',EqI,(CN,[LN=RN]),Result),
	Result\==eliminated,
%	CN=[_|_], /* equation EqI must be a conditional equation */
	origin(EqI,'$equation',K),
	try(retract(mayBeReducible(EqI,'$equation',_))), /* its here that
		the reducible info can be deleted. An abortion in orientEq
		would then leave the info there.
		*/
	assertz(actionNew(orient(n),'$equation'(CN,[LN=RN]))),
	newNonreductiveEquation(EqI,(CN,[(LN=RN)]),[(origin,K)],_),
	explain(toConstraint,[(CN,[LN=RN])]),
	dispose('$equation',EqI).




			
/*----------------------------------------------------------------------*
 *		 subsumedBounded(Type,I,(C,[L=R]))			*
 *----------------------------------------------------------------------*/

subsumedBounded(Type,I,(C,D)) :-
	installContext(Type,I,C,ComplI,CNO),
	resolution(0,CNO,(C,D),Proof),
	explain(subsumed,['$equation'(C,D),'$term'(ComplI),Proof]),
	eraseContext,
	!.
subsumedBounded(_Type,_I,_E) :-
	eraseContext,
	fail.



installContext(Type,I,C,ComplI,CNO):-
	condReduceClausesAndNonOConds(C,CCls,CNO),
	complexityBound(I,Type,ComplI),
	assert(tryingToEliminate(Type,I)),
	map(asserta,CCls,Refs),
	assert(contextClauses(Refs)),
	!.

eraseContext:-
	retract(tryingToEliminate(_,_)),
	retract(contextClauses(Refs)),
	mapA(erase,Refs).


resolution(_Lev,_CNO,(C,D),subsumed(C,D,TypeJ,J,ComplJP)) :-
	emptyResolvent((C,D),TypeJ,J,(CJ,DJ),ComplJP),
	verifyComplexity((CJ,DJ),ComplJP),
	!.
resolution(Lev,CNO,(C,D),resolution(CJ,DJ,ComplJP,ProofsDJ)) :-
	allowedResolutionLevel(Lev),
	selectEqFromConclusion((C,D),(CJ,DJ),ComplJP,Resolvent),
	(ComplJP=='$min' ->
		proveInContext(Lev,verified,CNO,C,Resolvent,(CJ,DJ),ComplJP,ProofsDJ)
	;	L1 is Lev+1,
		proveInContext(L1,notVerified,CNO,C,Resolvent,(CJ,DJ),ComplJP,ProofsDJ)),
	!.




allowedResolutionLevel(L):-
	(	resolutionDepth(D)
	;	D=2),
	!,
	L<D,
	\+ tooLate.



proveInContext(L,verified,CNO,C,Goals,EJ,ComplJP,Proofs):-
		resolveGoalsInContext(L,verified,CNO,C,Goals,EJ,ComplJP,Proofs).
proveInContext(L,notVerified,CNO,C,Goals,EJ,ComplJP,Proofs):-
	(varfree(ComplJP) ->
		verifyComplexity(EJ,ComplJP),
		resolveGoalsInContext(L,verified,CNO,C,Goals,EJ,ComplJP,Proofs)
	;	resolveGoalsInContext(L,notVerified,CNO,C,Goals,EJ,ComplJP,Proofs)
	).


reduceGoalInContext(CNO,GP,GRP):-
	fromProlog(GP,[],(L=R),Env),
	reduceC([],(min,infty),L,LN),
	reduceC([],(min,infty),R,RN),
	(contextuallyEqual(CNO,LN,RN) ->
		EqN=(LN=LN)
	;	EqN=(LN=RN)
	),
	applySubst(Env,EqN,GRP),
	!.


resolveGoalsInContext(_,_,_,_,_,_,_,_):-
	cont(stop,true),
	!,
	fail.
resolveGoalsInContext(_L,verified,_CNO,_C,[],_E1,_Compl1,[]).
resolveGoalsInContext(_L,notVerified,_CNO,_C,[],E1,Compl1,[]):-
		verifyComplexity(E1,Compl1).
resolveGoalsInContext(Lev,V,CNO,C,Gs,E1,Compl1,[hypothesis(G)|Proofs]):-
	append(GL,[G|GR],Gs),
	containsInstanceOf(C,G),
	append(GL,GR,Gs1),
	proveInContext(Lev,V,CNO,C,Gs1,E1,Compl1,Proofs).
resolveGoalsInContext(Lev,V,CNO,C,Gs,E1,Compl1,[reflexivity((L=R))|Proofs]):-
	append(GL,[L=R|GR],Gs),
	unifyTerms(L,R),
	!,	% hg 31.7.90 !?
	append(GL,GR,Gs1),
	proveInContext(Lev,V,CNO,C,Gs1,E1,Compl1,Proofs).
resolveGoalsInContext(Lev,V,CNO,C,[G|Gs],E1,Compl1,[reduction(G,GR,P)|Proofs]):-
	reduceGoalInContext(CNO,G,GR),
	G\==GR,
	spy(G,GR),
	proveInContext(Lev,V,CNO,C,[GR|Gs],E1,Compl1,[P|Proofs]).
resolveGoalsInContext(Lev,V,CNO,C,Gs,E1,Compl1,[P|Proofs]):-
	append(GL,[G|GR],Gs),
	varfree(G),
	!,	% hg 31.7.90 !?
	resolution(Lev,CNO,(C,[G]),P),
	append(GL,GR,Gs1),
	proveInContext(Lev,V,CNO,C,Gs1,E1,Compl1,Proofs).
%resolveGoalsInContext(Lev,V,CNO,C,[G|Gs],E1,Compl1,
%		[subsumed(C,[G],TypeJ,J,ComplJP)|Proofs]) :-
%	\+ varfree(G),
%	fromProlog([],G),
%	emptyResolvent((C,[G]),TypeJ,J,(CJ,DJ),ComplJP),
%	verifyComplexity((CJ,DJ),ComplJP),
%	!,
%	proveInContext(Lev,V,CNO,C,Gs,E1,Compl1,Proofs).


spy(_,_).







verifyComplexity(EJ,ComplJP):-
	tryingToEliminate(Type,I),
	(cont(trace(elim(I)),on) ->
		nl,
		sPrint("Attempting simplification with 
	% 
and complexity
	%... ",['$equation'(EJ),'$term'(ComplJP)])
	;	true),
	(Type='$equation',
         origin(I,Type,user) ->
		true;
		complexityBound(I,Type,ComplI),
		varfree(ComplJP),
%	 	fromProlog([],ComplJP),
		simplerProof(I,Type,ComplI,EJ,ComplJP),
		(cont(trace(elim(I)),on) ->
			write('succeeded.')
		;	true)),
	!.

sameComplexity(ComplJP):-
	tryingToEliminate(Type,I),
	complexityBound(I,Type,ComplI),
	sameC(ComplJP,ComplI).



sameC(((C1,C2),C3),((E1,E2),E3)):-
	eqMultiSet(C1,E1),
	eqMultiSet(C2,E2),
	eqMultiSet(C3,E3).

eqMultiSet(M,M).
eqMultiSet([M1],[M2]):-
	M1=..['$r'|CJ],
	M2=..['$r'|CI],
	ac_diff(CI,CJ,[]),
	ac_diff(CJ,CI,[]),
	!.





emptyResolvent((C,[E]),TypeJ,J,(C1P,[E1P]),ComplJP):-
	C\==[],
	eqType(E,T),
	selectNEqOrRule(TypeJ,J,((C1P,[E1P]),ComplJP,_Env)),
	consequentType(J,TypeJ,TJ),
	equalTypes(T,TJ),	% allows for restricted resolution with 
				% collapsing rules
	kind(J,TypeJ,K),
	unifyEquations(E,E1P,K),
	splitCondition(C1P,C,_Implied,[]).



selectEqFromConclusion(([_|_],[L=R]),(D,[L=R]),'$min',D):-
	L=..[O|Ls],
	R=..[O|Rs],
	\+ac_ist_AC(O),
	pairing(Ls,Rs,'=',D).
selectEqFromConclusion(([_|_],[E]),(C1P,[E1P]),ComplJP,C1P):-
	eqType(E,T),
	selectNEqOrRule(TypeJ,J,((C1P,[E1P]),ComplJP,_Env)),
	nonCollapsing(J,TypeJ,E1P),
	consequentType(J,TypeJ,T),
	kind(J,TypeJ,K),
	unifyEquations(E,E1P,K).

nonCollapsing(J,T,_E):-
	\+consequentType(J,T,'$anyType'),
	!.
nonCollapsing(J,T,_):-
	kind(J,T,injectivityLaw).

nonCollapsing((L=_R)):-
	nonvar(L),
	!.
nonCollapsing((_L=R)):-
	nonvar(R).


selectNEqOrRule('$nonoperational equation',J,E):-
	tryingToEliminate(Type,I),
	(origin(I,Type,super(_,_,'$nonoperational equation',TryFirst,_,_)) ->
		true
	;	TryFirst=0),
	(	prologVariant(TryFirst,'$nonoperational equation',E),
		J=TryFirst
	;	prologVariant(J,'$nonoperational equation',E),
		J\==TryFirst),
	\+redPredGenerated(J),
	(cont(msComplexity,true) ->
		complexityBound(J,'$nonoperational equation',(_,[C])),
		msComplexity(C)
	;	true),		
	(J=I -> 
		Type\=='$nonoperational equation'
	;
		true
	).
selectNEqOrRule('$rule',J,E):-
	tryingToEliminate(Type,I),
	prologVariant(J,'$rule',E),
	(J=I -> 
		Type\=='$rule'
	;
		true
	),
	\+reductive(J).

/*
	(reductive(J) ->
		origin(J,'$rule',domainAxiom)
	;	true).
*/




/*----------------------------------------------------------------------*
 *		     splitCondition([P|Ps],C,[P|PsI],NI)		*
 *----------------------------------------------------------------------*/

splitCondition([],_,[],[]).
splitCondition([P|Ps],C,[P|PsI],NI) :-
	containsInstanceOf(C,P),
	splitCondition(Ps,C,PsI,NI).
splitCondition([X=Y|Ps],C,[X=Y|PsI],NI) :-
	ac_uniAC(X,Y),
	splitCondition(Ps,C,PsI,NI).
splitCondition([P|Ps],C,PsI,[P|NI]) :-
	splitCondition(Ps,C,PsI,NI).


/*----------------------------------------------------------------------*
 *		   	  complexity(C,CTs1)				*
 *----------------------------------------------------------------------*/

complexity(C,CTs1) :-
	mapF(lambda([X=_,X],true),C,CT1),
	mapF(lambda([_=X,X],true),C,CT2),
	append(CT1,CT2,CTs1),
%	split(lambda([X],constrTerm(X)),CTs1,_,CTs),	
	!.



/*----------------------------------------------------------------------*
 *		   		  redRule(I)				*
 *									*
 * reduces rule with index I if possible				*
 *----------------------------------------------------------------------*/

redRule :-
	mayBeReducible(I,'$rule',_),
	redRule(I).

redRule(I) :-
 	reduceIfPossible('$rule',I,(CN,[LN=RN]),Result),
 	(Result=reduced ->
 		'$rule'(I,(_,[L=_])),
 		(L=LN -> 
 			new('$rule',(CN,[L=RN]),NewI,[(origin,'$rule'(I))],_),
 			explain(redRuleRight,[I,'$rule'(NewI)])
 		;
 			new('$equation',(CN,[(LN=RN)]),_,[(reducedTag,reduced),(origin,redRule(I))],EqRef),
 			explain(redRuleLeft,[I,eq(EqRef)])
 		),
 		dispose('$rule',I)
 	;	(Result=eliminated ->
 			true
 		;	try(retract(mayBeReducible(I,'$rule',_))),
 			reducibleAuxRule(I),
 			explain(redAuxRule,[I]),
 			fail
 		)
	).
 
reducibleAuxRule(I):-
	auxRule(I,'$rule',(_,(_,_))),
	prologVariant(I,'$rule',RP),
	assign1(self(prologVariant),RP),	% assumed by reduceClause/2
	'$rule'(I,R),
	reduceClause(('$rule',I,R),(reduceClause,Clauses)),
	!,
	cont1(self(auxRule),(_,none)),		% self(auxRule) is set by reduceClause/2
	seta('$rule',I,auxRule,(_,none)),
	seta('$rule',I,reduceClause,Clauses).


/*----------------------------------------------------------------------*
 *		   		  cp(I,I1)				*
 *									*
 * computes critical pairs between rules I and I1			*
 *----------------------------------------------------------------------*/

cp(I,I1) :-
	superpose(I,left,'$rule',I1,conclusion,left).


sp(I,I1) :-
	superpose(I,left,'$nonoperational equation',I1,condition(1),left).


superpose(RuleIndex,NopEqIndex,Literal,LiteralSide) :-
      superpose(RuleIndex,left,'$nonoperational equation',
                NopEqIndex,Literal,LiteralSide).


superpose(I,TI,Ty1,I1,L1,TI1):-
	nonvar(L1),
	normalizePos(L1,LN),
	selectSuperposition(super(I,TI,Ty1,I1,LN,TI1)),
	superposition(super(I,TI,Ty1,I1,LN,TI1)).
superpose(I,TI,Ty1,I1,L1,TI1):-
	var(L1),
	selectSuperposition(super(I,TI,Ty1,I1,LN,TI1)),
	normalizePos(L1,LN),
	superposition(super(I,TI,Ty1,I1,LN,TI1)).


'superpose!'(RuleIndex,NopEqIndex,Literal,LiteralSide) :-
      superpose1(RuleIndex,left,'$nonoperational equation',
                 NopEqIndex,Literal,LiteralSide).
'superpose!'(I,TI,Ty1,I1,L1,TI1) :-
      superpose1(I,TI,Ty1,I1,L1,TI1).


superpose1(I,TI,Ty1,I1,L1,TI1):-
	nonvar(L1),
	normalizePos(L1,LN),
	superposition(super(I,TI,Ty1,I1,LN,TI1)).
superpose1(I,TI,Ty1,I1,L1,TI1):-
	var(L1),
	superposition(super(I,TI,Ty1,I1,LN,TI1)),
	normalizePos(L1,LN).



normalizePos(conclusion,0).
normalizePos(condition(I),I).


superposition(super(I,TI,TyJ,J,LJ,TJ)):-
%	assign1(superFound,false),
	assign1(nontrivialSuperFound,false),
	(	accessEqs(I,TyJ,J,EqI,EqJ,ComplJ,EnvIJ),
		extSuper(I,TI,EqI,TyJ,J,LJ,TJ,EqJ,(C,D),Super,RedIndexes),
%		assign1(superFound,true),
		inc(nmbovs),
		fromProlog(EnvIJ,(C,D,Super)),
		superPCompl(I,J,LJ,Super,(C,D),ComplJ,Compl),
		(contextualReduction((C,D),(Compl,RedIndexes),(E,F),Substs) ->
			applySubsts(Substs,Compl,ComplR),
			new('$equation',(E,F),_,
				[(origin,super(I,TI,TyJ,J,LJ,TJ)),
			 	 (complexityBound,ComplR),
				 (redIndexes,RedIndexes)],
				CPRef),
			explain(superFound,['$oref'('$equation',CPRef)]),
			assign1(nontrivialSuperFound,true)
%	  	;
%			explain(trivialSuperFound,[C,D])
	  	),
	  	fail
	;
	     	(cont1(nontrivialSuperFound,true) -> explain(superFrom,[I,TI,TyJ,J,LJ,TJ]))
%	     	cont1(nontrivialSuperFound,true)
	).

	

extSuper(reflexivity,_,(reflexivity,_),
		TJ,J,LJ,_,(_,(CJ,DJ)),
		(C,DJ),_Super,Red):-
	select([_|CJ],LJ,(A=B)),
	!,
	ac_uniAC(A,B),
	redIndexes(J,TJ,Red),
	deleteElem([_|CJ],LJ,[_|C]).
extSuper(I,left,(KindI,(CI,[AI=BI])),
		'$rule',J,0,left,(aux,(CJ,[AJ=BJ])),
		(C,[AS=BS]),AJ,Red):-
	(KindI=aux ->
		I=<J
	;	true
	),
	superPRedIndexes('$rule',J,0,Red),
	!,
	headOverlap(ac_uniAC,(AI,BI),(AJ,BJ),(AS,BS)),	
	simplifiedCondition(CI,I,CJ,'$rule',J,C).
extSuper(I,left,(KindI,(CI,[AI=BI])),
		TyJ,J,LJ,TJ,(_,(CJ,[EJ])),
		(C,[E]),AJ,Red):-
	select([EJ|CJ],LJ,(A=B)),
	(TJ = left ->
		AJ=A,
		BJ=B
	;	AJ=B,
		BJ=A
	),
	superPRedIndexes(TyJ,J,LJ,Red),
	unificationType(I,'$rule',U),
	!,
	(cont1(nonEqLogic,true) ->
		headOverlap(unify,(AI,BI),(AJ,BJ),(AS,BS))
	;noSuperPAtTop(KindI,I,TyJ,LJ,J) ->
		overlapBelowHead(U,(AI,BI),(AJ,BJ),(AS,BS))
	;	overlap(U,(AI,BI),(AJ,BJ),(AS,BS))
	),	
	change([EJ|CJ],LJ,(AS=BS),[E|CJ1]),
	simplifiedCondition(CI,I,CJ1,TyJ,J,C).





superPRedIndexes('$rule',_J,0,(infty,infty)):-
	!.
superPRedIndexes(TJ,J,_LJ,Red):-
	redIndexes(J,TJ,Red).


superPCompl(reflexivity,_,_,_,_,Co,Co):-
	!.
superPCompl(I,J,0,STerm,_SEq,_,((['$r'(STerm)],['$r'(STerm)]),['$r'(STerm)])):-
	reductive(I),
	reductive(J),
	!.
superPCompl(_I,_J,0,_STerm,SEq,_,((Co,Co),Co)):-
	nredComplexity(SEq,Co),
	!.
superPCompl(I,_J,_LJ,_STerm,_SEq,((RCL,RCR),NC),((RCL1,RCR1),NC)):-
	% _LJ > 0
	reductive(I),
	(RCR='$infty' ->
		RCR1=NC
	;	RCR1=RCR
	),
	(RCL='$infty' ->
		RCL1=NC
	;	RCL1=RCL
	),
	!.
superPCompl(_I,_J,_LJ,_STerm,SEq,(RC,_NC),(RC,NC)):-
	nredComplexity(SEq,NC),
	!.




noSuperPAtTop(normal,I,'$rule',_,I):-
	unificationType(I,'$rule',ac_uniAC),
	!,
	fail.
noSuperPAtTop(normal,I,'$rule',_,I):-
	reductive(I),
	!.
noSuperPAtTop(normal,I,'$rule',0,J):-
	J>I.





reductive(I):-
	superpositionTerms(I,'$rule',ST),
	length(ST,1).


simplifiedCondition([],_,C,_,_,C):-!.
simplifiedCondition(C,_,[],_,_,C):-!.
simplifiedCondition(CI,I,CJ,'$rule',J,C):-
	boundTermAssoc(CI,I,AssocI),
	transform((replace,[AssocI]),CJ,CJ1),
	boundTermAssoc(CJ,J,AssocJ1),
	setof1((T1,T2),(member((T1,T2),AssocJ1), \+member((T1,_),AssocI)),AssocJ),
	transform((replace,[AssocJ]),CI,CI1),
	union(AssocI,AssocJ,AIJ),
	union(AIJ,CI1,ACI),
	union(ACI,CJ1,C),
	!.
simplifiedCondition(CI,I,CJ,'$nonoperational equation',_J,C):-
	boundTermAssoc(CI,I,AssocI),
	transform((replace,[AssocI]),CJ,CJ1),
	union(AssocI,CJ1,AIJ),
	union(AIJ,CI,C),
	!.

boundTermAssoc(CI,I,Assoc):-
	superpositionTerms(I,'$rule',STI),
	setof1(E,rightReducedEq(CI,STI,E),Assoc).



replace(T,_,_):-
	var(T),
	!,
	fail.
replace(T1,T2,R):-
	member((T=T2),R),
	T1==T.





rightReducedEq(C,_ST,(L=R)):-
	member((L=R),C),
	constrTerm(R).
rightReducedEq(C,ST,E):-
	member((condition(I),right),ST),
	select([notHere|C],I,E).

negateDir(left,right).
negateDir(right,left).

accessEqs(reflexivity,TyJ,J,(reflexivity,_),(normal,EqJ),ComplJ,EnvIJ):-
	prologVariant(J,TyJ,(EqJ,ComplJ,EnvIJ)).
accessEqs(reflexivity,'$rule',J,(reflexivity,_),(aux,(C,[E])),ComplJ,EnvIJ):-
	auxRule(J,'$rule',(C,[E])),
	prologVariant(J,'$rule',(_Eq,Compl,EnvIJ)),
	toProlog(Compl,EnvIJ,ComplJ,_),
	!.
accessEqs(I,TyJ,J,(NA1,EqI),(NA2,EqJ),ComplJ,EnvIJ):-
	prologVariant(I,'$rule',(EI,_ComplI,SI)),
	prologVariant(J,TyJ,(EJ,ComplJ,SJ)),
	renameVars(SI,[],SIRenamed),	% to remove the X<i> in SI
	renameVars(SJ,SIRenamed,SJRenamed), % removes also the X<i> in SJ
	append(SJRenamed,SIRenamed,EnvIJ),
	!,
	(	NA1=normal,
		EqI=EI
	;	auxRule(I,'$rule',EqI),
		NA1=aux),
	(	NA2=normal,
		EqJ=EJ
	;	auxRule(J,TyJ,EqJ),
		NA2=aux).




reducible(reflexivity,_Ty):-	% this is rather a type clash
	!,
	fail.
reducible(I,Ty):-
	\+object(I,Ty,_),
	!.
reducible(I,'$rule'):-
	mayBeReducible(I,'$rule',_),
	redRule(I).
reducible(I,'$nonoperational equation'):-
	mayBeReducible(I,'$nonoperational equation',_),
	redNOpEq(I).








/*----------------------------------------------------------------------*
 *		   	      cps(moreCPs)				*
 *----------------------------------------------------------------------*/






cps(moreCPs) :-
	selectSuperposition(Super),
	superposition(Super).
cps(moreCPs):-
	'$maySuperpose'(_,_,_,_,_,_,_),
	!.
cps(noMoreCPs).


selectSuperposition(Super):-
	newSuperpositions:==false,
	(cont1(sps,M),M>500 ->
		repeat(reducible(_,'$rule')),
		repeat(reducible(_,'$nonoperational equation'))
	;
		true
	),
	setof(super(Ch,I,TI,Ty1,I1,L1,TI1),
		'$maySuperpose'(Ch,I,TI,Ty1,I1,L1,TI1),SP),
	length(SP,N),
	nl,sPrint("[% superpositions yet to be considered.]",[N]),nl,
	sps:==N,
	sp_member(Super,SP).



sp_member(super(I,TI,Ty1,I1,LN,TI1),SP):-
	length(SP,N),
	Check is N//10,
	checkedSPsThisTime:=0,
	append(_,[super(Ch,I,TI,Ty1,I1,L1,TI1)|_SP1],SP),
	inc(checkedSPsThisTime),
	(	(	cont1(newSuperpositions,false)
		;	% length(SP1,N1),
			% N1>1000,
			cont(checkedSPsThisTime,Checked),
			Checked < Check)
	 -> 	true
	;	!,
		fail),
%	retract('$maySuperpose'(Ch,I,TI,Ty1,I1,L1,TI1)),  changed uh 18.01.90
% 'maySuperpose'(...) is needed in the following two reducible calls
% if they have success, so it will be only disposed after both calls
% failed
	\+reducible(I,'$rule'),
	\+reducible(I1,Ty1),
	retract('$maySuperpose'(Ch,I,TI,Ty1,I1,L1,TI1)), 
	normalizePos(L1,LN).







/*----------------------------------------------------------------------*
 *		   	   	 kbLoop					*
 *----------------------------------------------------------------------*/

kbLoop :-
   	enable_q_option,
 	repeat,
 	cps(Effect),
 	checkForAbort,
 	(Effect=noMoreCPs ->
 		(eqsExist ->
 			nextEquation(_,NmbEqs),
 			orderEq(_I),
 			Order is NmbEqs//10+2,
 			solutions((mayOrder(_),checkForAbort),Order),
 			fail
 		;
 			true
 		)
 	;
 		reduceAndOrder,
 		checkForAbort,
 		fail
 	),
 	\+ redNOpEq(_),
 	notifyPostponedEqs,
 	nl,
 	cont(nmbovs,Ovs),
 	sPrint("% superpositions have been computed.",[Ovs]),
 	disable_q_option,
 	!.


checkForAbort:-
	(completion_interrupted ->
		abortFromCompletion
	;	true).





/*----------------------------------------------------------------------*
 *		   	    reduceAndOrder				*
 *----------------------------------------------------------------------*/

reduceAndOrder :-
	nextEquation(_,N),
	O is N//10,
	max(O,2,O1),
	solutions((mayOrder(_),checkForAbort),O1),  
	!.


/*----------------------------------------------------------------------*
 *		   	        eqsExist				*
 *----------------------------------------------------------------------*/

% 14.11.90 uh
% removed new version of eqsExist
% inserted old version
% eqsExist :-
% 	priority(I,'$equation',postponed),
% 	redEq(I).
% eqsExist :-
% 	'$equation'(_,_).

eqsExist :-
        priority(I,'$equation',P),
        (P\==postponed ->
                true
        ;       redEq(I)).
	


notifyPostponedEqs:-
	setof1(I,priority(I,'$equation',postponed),P),
	length(P,N),
	(N>0	 ->
		sPrint("
You may reconsider the % postponed equation(s) by restarting completion.",[N]),
		nl
	;	true).


/*----------------------------------------------------------------------*
 *		   	        peval					*
 *			 pevalCompletionLoop				*
 *			     pevalEq(I)					*
 *----------------------------------------------------------------------*/

peval :-
	abolish(critPair,1),
	nmbovs:=0,
	time((
	repeat(pevalEq(_)),
	repeat(orderEq(_)),
	pevalCompletionLoop
	)).

pevalCompletionLoop :-
	repeat,
	cps(Effect),
	(Effect=noMoreCPs ->
		(eqsExist ->
			repeat(redEq(_)),
			repeat(pevalEq(_)),
			orderEq(_),
			(	mayOrder(_)
			;
				true
			),
			repeat(redRule(_)),
			repeat(redNOpEq(_)),
			fail
		;
			nl,
			cont(nmbovs,Ovs),
			sPrint("% superpositions have been computed.",[Ovs])
		)
	;
		repeat(redEq(_)),
		repeat(pevalEq(_)),
		reduceAndOrder,
		fail
	).



pevalEq(I) :-
	compositions(I).

%%02.11.89%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

redEqs :-
	repeat(redEq(_)).

%%02.11.89%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

redNOpEqs :-
	repeat(redNOpEq(_)).

%%02.11.89%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

redRules :-
	repeat(redRule(_)).

