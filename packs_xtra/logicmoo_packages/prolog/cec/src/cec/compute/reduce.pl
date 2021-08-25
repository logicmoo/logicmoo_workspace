/*
 *	file:		reduce.pl
 *	version:	1.5
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains the functions for building rewrite-rules
 *	and the static functions for connecting the predicate norm 
 *	and all other predicates dealing with rewriting with the 
 *	generated rules.
 *
 *	history:
 *	891010	js	Added this comment
 *	891110	uh	Added the description
 *	891106	uh	Moved definitions of
 *			contextuallyEqual/2 		contextuallyEqual/3
 *			reduceByRules/3			reduceByRules/4
 *			from into file compute/prove.pl
 *	891208	uh	Changed definition of
 *			opCall/4
 *			opCall/5
 *			Order-sorted operator symbols are now disambiguated
 *			using their arity length.
 *	900320	uh	Changed call to orderSortedOrdering
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

/*----------------------------------------------------------------------*/

arity(O,N) :-
	(_ ofType (O:Type)),
	length(Type,M),
	N is M-1,
	!.
	
/*----------------------------------------------------------------------*/

reduceCall((Term,Nf),
	   (Term =.. [O|Args],
	    ReduceCall =..[O,Nf|Args],
	    ReduceCall)).

/*----------------------------------------------------------------------*/
/* 	Generates two reduction clauses which perform the rewriting
	for the rule C=>L->R.
	The second clause is called in cases in which it must be
	avoided that the rule is used to reduce its own left side.
	In such cases, the test I\==Id fails, if the clause is
	called with Id being its own rule number I.

	In case L has an AC-Operator at the top, a third clause must
	be generated. This is only for rules that do not represent
	algebraic laws which are dealt by the "rewrite expert".
	The rewrite expert will only call this third clause.
	It is generated with the predicate buReducerForGeneralACOpRule,
	cf. below.

	In the AC-case it is assumed that calls to these rewrite clauses 
	producing predicates are made both
	for a rule and its implicit auxiliary rule D=>L op X -> R op X.
*/

buReducerForRule(Rule,Cls) :-
	buReducerFor('$rule',Rule,Cls),
	!.

buReducerFor(Type,(I,D,(A=R)),[(Head:-Body)|RClause]) :-
	varConstraints(A,L,VC),
	append(VC,D,C),
	L=..[OpL|LArgs],
	renamePVars(LArgs,LArgsRenamed),
	LRenamed=..[OpL|LArgsRenamed],
	matchCodeForTerm((LRenamed,Patterns),MatchCode),
		% after execution of MatchCode at rewrite-time,
		% the variables in LRenamed will be bound to the
		% corresponding subterms. These need not be reduced,
		% if OpL is AC and if the variable is a direct son of OpL.

	ac_del_assoc(OpL,LArgsRenamed,AllArgsRenamed),
	reduceCallsForACOpVars(OpL,AllArgsRenamed,R,LArgs,RCode),
		% after execution of RCode, which is a list of calls
		% reduceR(LArgRenamed,LArg), for any possibly nonreduced
		% variable LArgRenamed directly below OpL in LRenamed.
		% LArg is then the corresponding member of LArgs. 
		% Alltogether: after execution of MatchCode and RCode,
		% the variables in L are bound to irreducible subterms.
	length(LArgs,N),
	TyEq=..[Type,I,(C,[L=R])],
	condsTestCode(TyEq,C,TestCode),
	mkAtom('$%%',[N,OpL],RewriteOp),
	Head=..[RewriteOp,NormalForm|Patterns],
	detachConstrTerm(R,NormalForm,Assoc),
	map(buReduceCalls,Assoc,ReduceCallsList),
	rReduce(append,[],[MatchCode,RCode,TestCode|ReduceCallsList],Code),
	(Code=[] -> 
		Body='!',
		Body1=true
	;
		listToTuple(Code,Body1),
		Body=(Body1,!)
	),
	restrictedRClause(Type,I,Head,Body1,RClause),
	!.

reduceCallsForACOpVars(OpL,AllArgsRenamed,R,LArgs,RCode):-
	ac_ist_AC(OpL),
	!,
	ac_del_assoc(OpL,LArgs,AllArgs),
	genReduceForVars(OpL,R,AllArgsRenamed,AllArgs,RCode).
reduceCallsForACOpVars(OpL,AllArgs,_R,LArgs,[]):-
	ac_del_assoc(OpL,LArgs,AllArgs). % to identify original with renamed
		% variables in L.



/*----------------------------------------------------------------------*/

buReducerForGeneralACOpRule(Rule,Cls) :-
	buReducerForGeneralACOp('$rule',Rule,Cls),
	!.

buReducerForGeneralACOp(Type,(I,D,(A=R)),[(Head:-Body)|RClause]):-
	varConstraints(A,L,VC),
	append(VC,D,C),
	L=..[OpL|LArgs],
	renamePVars(LArgs,LArgsRenamed),
	ac_del_assoc(OpL,LArgsRenamed,AllArgsRenamed),
	reduceCallsForACOpVars(OpL,AllArgsRenamed,R,LArgs,RCode),
	MatchCode=[ac_matchForOp(OpL,CallArgs,AllArgsRenamed)],
	TyEq=..[Type,I,(C,[L=R])],
	condsTestCode(TyEq,C,TestCode),
	mkAtom('$2G%',[OpL],RewriteOp),
	Head=..[RewriteOp,NormalForm,CallArgs],
	detachConstrTerm(R,NormalForm,Assoc),
	map(buReduceCalls,Assoc,ReduceCallsList),
	rReduce(append,[],[MatchCode,RCode,TestCode|ReduceCallsList],Code),
	(Code=[] -> 
		Body='!',
		Body1=true
	;
		listToTuple(Code,Body1),
		Body=(Body1,!)
	),
	restrictedRClause(Type,I,Head,Body1,RClause),
	!.


restrictedRClause('$rule',I,Head,Body,
	[(Head1:-
		Body,
		(RedIndex=infty ->
			true
		;	myRuleIndex(I,MyI),  % I will be renamed when rule index is changed
			redIndexes(MyI,'$rule',(RI,_)),
			smallerRedIndex(MyI,RI,RedIndex)),
		!
	 )]) :-
	reductive(I),  % only the red indexes of reductive rules may be
		% changed without having to worry about recomputation of
		% superpositions
	Head=..[O|A],
	Head1=..[O,(_Compl,RedIndex)|A],
	!.	
restrictedRClause('$rule',I,Head,Body,
	[(Head1:-
		(RedIndex=infty ->
			true
		;	myRuleIndex(I,MyI),  % I will be renamed when rule index is changed
			redIndexes(MyI,'$rule',(RI,_)),
			smallerRIndex(RI,RedIndex)),
		Body,
		!
	 )]) :-
	Head=..[O,Res|A],
	Head1=..[O,(_Compl,RedIndex),Res|A],
	!.	
restrictedRClause('$nonoperational equation',I,Head,Body,
	[(Head1:-
		try(tryingToEliminate(Type,J)),
		(redPredGenerated(I) ->
			true % previously proved inductive theorem
		;	simplerProof(J,Type,Compl,EP,ComplP)),
		Body,
		!
	 )]) :-
	Head=..[O,Res|ArgsP],
	Head1=..[O,(Compl,_RedIndex),Res|ArgsP],
	cont1(self(prologVariant),(EP,ComplP,_Subst)),
	EP=(_,[LP=_]),
	LP=..[_|ArgsP],	% to fully instantiate EP and ComplP with
			% the corresponding variables in head
	!.	
restrictedRClause(cond,_I,Head,_Body,
	[(Head1:-
		Head,
		!
	 )]) :-
	Head=..[O,Res|ArgsP],
	Head1=..[O,_,Res|ArgsP],
	!.	
restrictedRClause(_,_,_,_,[]):-
	!.






/*----------------------------------------------------------------------*/

genReduceForVars(_,_,[],[],[]).
genReduceForVars(O,R,[AR|AllArgsRenamed],[A|AllArgs],[reduceR(AR,A)|RCode]) :-	% rs 29.03.88 reduce -> reduceR
	var(AR),
	occursNotBelow(R,O,A),
	!,
	genReduceForVars(O,R,AllArgsRenamed,AllArgs,RCode),
	!.
genReduceForVars(O,R,[A|AllArgsRenamed],[A|AllArgs],RCode) :-
	genReduceForVars(O,R,AllArgsRenamed,AllArgs,RCode).



occursNotBelow(R,_O,V):-
    var(R),
    !,
    R==V.
occursNotBelow(R,O,V):-
    R=..[O|Args],
    member(A,Args),
    A\==V,
    occursNotBelow(A,O,V),
    !.
occursNotBelow(R,O,V):-
    R=..[_|Args],
    member(Arg,Args),
    occursNotBelow(Arg,O,V),
    !.



/*----------------------------------------------------------------------*/

condsTestCode(Eq,[],[applying(Eq)]):-
	!.
condsTestCode(Eq,C,Code) :-
	functor(Eq,Ty,2),
	length(C,N),
	listOfNumbers(N,Positions),
	pairing(Positions,C,Conds),
	(Ty='$rule' ->
		cont1(self(superpositionTerms),ST)
	;	ST=[]),
	ruleType(Eq,Type),
	mapF(lambda([Cond,CondCode],condTestCode(Type,Cond,ST,CondCode)),
		Conds,CodeList),
	rReduce(append,[applying(Eq)],CodeList,Code),
	!.

ruleType('$rule'(_,(Conds,[L = _R])),Type) :-
	varsOf(L,LVars),
	varsOf(Conds,CVars),
	(subsetPV(CVars,LVars) ->
	    Type = reductive
	;
	    Type = quasiReductive
	).
ruleType(_,reductive).


condTestCode(_,(I,(L=R)),ST,Code) :-
	\+ member((condition(I),right),ST),
	\+ (constrTerm(R),varfree(R)),
	!,
	detachConstrTerm((L=R),(CL=CR),Assoc),
	map(buReduceCalls,Assoc,CallsLR),
	rReduce(append,[],CallsLR,Code1),
	append(Code1,[contextuallyEqual(CL,CR)],CodeNO),
	map(optimize,CodeNO,Code),
	!.
condTestCode(Type,(_I,(L=R)),_ST,Code) :-
	detachConstrTerm((L='true-bool'),(CL=_CR),Assoc),
	map(buReduceCalls,Assoc,CallsLR),
	rReduce(append,[],CallsLR,Code1),
	(Type = reductive ->
		append(Code1,[ac_matchvf(CL,R)],CodeNO)
	;
		append(Code1,[ac_match(CL,R)],CodeNO)
	),
	map(optimize,CodeNO,Code).






optimize((C -> true;Wanted=Got),C):-
	nonvar(Wanted),
	functor(Wanted,W,_),
	functor(Got,G,_),
	W\==G,
	!.
optimize(C,C).





/*----------------------------------------------------------------------*/

/* jetzt aus file ~huba/matching/gen  auf snert
 * matchCodeForTerm((L,Patterns),[]) :-
 *	L=..[_|Patterns],
 *	not(containsCOp(L)),
 *	(ac_ist_AC(_) ->
 *		linear(L)
 *	;
 *		true
 *	).
 * matchCodeForTerm((L,[X,Y]),
 *		[ac_del_assoc(O,[X,Y],TArgs),ac_matchForOp(O,TArgs,LArgs)]) :-
 *	L=..[O|LArgs1],
 *	ac_ist_AC(O),
 *	!,
 *	ac_del_assoc(O,LArgs1,LArgs).
 * matchCodeForTerm((L,[X,Y]),Code) :-
 *	L=..[O,L1,L2],
 *	ac_ist_C(O),
 *	cMatchCode(L1,L2,X,Y,Code).
 * matchCodeForTerm((L,TArgs),Code) :-
 *	L=..[_|LArgs],
 *	length(LArgs,N),
 *	genVarList(N,TArgs),
 *	pairing(TArgs,LArgs,TLArgs),
 *	mapF(lambda([(X,Y),ac_match(X,Y)],true),TLArgs,Code).
 *
*/

/*----------------------------------------------------------------------*/

cMatchCode(L1,L2,X1,X2,[ac_match(X1,X2)]) :-
	var(L1),
	L1==L2.
cMatchCode(L1,L2,X1,X2,[(L1=X1,L2=X2;L1=X2,L2=X1)]) :-
	not(containsCOp(L1)),
	not(containsCOp(L2)),
	linear((L1,L2)).
cMatchCode(L1,L2,X1,X2,
		[(ac_match(X1,L1),ac_match(X2,L2);
			ac_match(X2,L1),ac_match(X1,L2))]).

/*----------------------------------------------------------------------*/

containsCOp(_) :-
	\+ac_ist_C(_),
	!,
	fail.
containsCOp(T) :-
	nonvar(T),
	T=..[O|Ts],
	(	ac_ist_C(O)
	;
		member(T1,Ts),
		containsCOp(T1)
	).

/*----------------------------------------------------------------------*/

linear(T) :-
	linear(T,[],_),
	!.

linear(V,Vars,[V|Vars]) :-
	var(V),
	!,
	not(vmember(V,Vars)).
linear(T,Vars,NewVars) :-
	T=..[_|Ts],
	linearTermList(Ts,Vars,NewVars).

linearTermList([],V,V).
linearTermList([X|Xs],V,NV) :-
	linear(X,V,V1),
	!,
	linearTermList(Xs,V1,NV),
	!.

/*----------------------------------------------------------------------*/

vmember(X,[Y|_]) :-
	X==Y.
vmember(X,[_|Ys]) :-
	vmember(X,Ys).


/*----------------------------------------------------------------------*/

buReduceCalls((X,X),[]) :-
	var(X),
	!.
buReduceCalls((X,T),Calls) :-
	T=..[O|Args],
	length(Args,N),
	detachConstrTermList([],Args,CTsOfArgs,Assoc),
	map(buReduceCalls,Assoc,RCallsList),
	rReduce(append,[],RCallsList,RCalls),
	mkAtom('$%%',[N,O],RewriteOp),
	OCall=..[RewriteOp,X|CTsOfArgs],
	XTerm=..[O|CTsOfArgs],
	append(RCalls,[(OCall->true;X=XTerm)],Calls),
	!.

/*----------------------------------------------------------------------*/

buReduceCallsForCond((X,T),[]) :-
	var(T),
	not ac_occurs(X,T),
	!,
	T=X.
buReduceCallsForCond((X,T),Calls) :-
	var(T),
	!,
	buReduceCallsForCond((T,X),Calls).
buReduceCallsForCond((X,T),Calls) :-
	T=..[O|Args],
	length(Args,N),
	detachConstrTermList([],Args,CTsOfArgs,Assoc),
	map(buReduceCallsForCond,Assoc,RCallsList),
	rReduce(append,[],RCallsList,RCalls),
	mkAtom('$%%',[N,O],RewriteOp),
	OCall=..[RewriteOp,X|CTsOfArgs],
	XTerm=..[O|CTsOfArgs],
	(nonvar(X)->
		append(RCalls,[OCall],Calls);
		append(RCalls,[(OCall->true;X=XTerm)],Calls)),
	!.

/*----------------------------------------------------------------------*/

genReduceCall((X,X),[]) :-
	var(X),
	!.
genReduceCall((X,T),[reduce(T,X)]) :-
	!.
	
/*----------------------------------------------------------------------*/		
detachConstrTerm(T,ConstrContext,ArgumentAssociation) :-
	detachConstrTerm([],T,ConstrContext,ArgumentAssociation).
detachConstrTerm(Assoc,T,ConstrContext,Assoc1) :-
	nonvar(T),
	T=..[O|Args],
	constructorOrEq(O),
	!,
	detachConstrTermList(Assoc,Args,ConstrContexts,Assoc1),
	ConstrContext=..[O|ConstrContexts],
	!.
detachConstrTerm(Assoc,T,X,Assoc) :-
	associatedWith(Assoc,X,T),
	!.
detachConstrTerm(Assoc,T,X,[(X,T)|Assoc]).


constructorOrEq('=').
constructorOrEq(O):-
	hhConstructor(O).

/*----------------------------------------------------------------------*/

associatedWith([(X,T)|_],Y,S) :-
	X==Y,
	T==S,
	!.
associatedWith([_|Assoc],Y,T) :-
	associatedWith(Assoc,Y,T).

/*----------------------------------------------------------------------*/

detachConstrTermList(A,[],[],A).
detachConstrTermList(A,[T|Ts],[CT|CTs],A1) :-
	detachConstrTerm(A,T,CT,A2),
	detachConstrTermList(A2,Ts,CTs,A1),
	!.

/*----------------------------------------------------------------------*/
/* genReduceProcedure(+Rule,-Refs)					*/
/* generates clauses for the reriting rule Rule, asserts them and gives */
/* back references Refs on these clauses.				*/

genReduceProcedure(Rule,Refs) :-
	buReducerForRule(Rule,Clauses),
	map(assert,Clauses,Refs),
	!.


genReduceProcedureNopEq((I,(C,[L=R])),Clauses) :-
	buReducerFor('$nonoperational equation',(I,C,(L=R)),Clauses),
	!.




redPredsForRedNopEqs(Refs):-
	setof1((I,E),inductiveLemma(I,E),REqs),
	setof1((I,O),indACProp(I,O),ACEqs),
	diff(REqs,ACEqs,REqs1),
	append(REqs,ACEqs,Eqs),
	unlimitedRestriction:=true,
	map(genReduceProcedureNopEq,REqs1,CCls),
	unlimitedRestriction:=false,
	map(lambda([(I,_E),F],F=redPredGenerated(I)),Eqs,Facts),
	map(lambda([(I,O),F],F=indAC(O)),ACEqs,ACFacts),
	rReduce(append,[],[ACFacts,Facts|CCls],Clauses),
	map(assert,Clauses,Refs).


indACProp(J,O):-
	cont1(indProve,true),
	'$nonoperational equation'(I,(C,D)),
	D=[L=_],
	functor(L,O,2),
	permutative(D,commLaw),
	splitDomConds(C,_,OC),
	OC=[],
	'$nonoperational equation'(K,(C1,D1)),
	D1=[L1=_],
	functor(L1,O,2),
	permutative(D1,assocLaw),
	splitDomConds(C1,_,OC1),
	OC1=[],
	(J=I;J=K).

	


genReduceProcedureForOp(O/N,[R1,R2]) :-
	genVarList(N,Vars),
	genVarList(N,VarsReduced),
	OTerm=..[O|Vars],
	pairing(Vars,VarsReduced,VarPairs),
	recursiveDescentMayFail(RedIndex,O/N,0,VarPairs,VarsReduced,NF,CodeI),	
	recursiveDescentMayFail(O/N,0,VarPairs,VarsReduced,NF,Code),	
	R1=(	reduceI(OTerm,RedIndex,NF) :- 
			CodeI,
			!
	   ),
	R2=(	reduce(OTerm,NF) :- 
			Code,
			!
	   ),
	!.

opCall(O/_N,_,_,fail):-
	hhConstructor(O),
	!.
opCall(O/N,NF,ReducedSubterms,OCall):-
	mkAtom('$%%',[N,O],OProc),
	OCall=..[OProc,NF|ReducedSubterms],
	!.


opCall(_,O/_N,_,_,fail):-
	hhConstructor(O),
	!.
opCall(RedIndex,O/N,NF,ReducedSubterms,OCall):-
	mkAtom('$%%',[N,O],OProc),
	OCall=..[OProc,RedIndex,NF|ReducedSubterms],
	!.



recursiveDescentNoFail(O/N,N,[],VarsR,NF,Code):-
	opCall(O/N,NF,VarsR,OCall),
	NFTerm=..[O|VarsR],
	Code=(OCall;NF=NFTerm),
	!.
recursiveDescentNoFail(O/N,FromArg,[(X,XR)|VarPairs],VarsR,NF,Code):-
	A is FromArg+1,
	recursiveDescentNoFail(O/N,A,VarPairs,VarsR,NF,C),
	Code=	(reduce(X,XR) -> C  ; XR=X,C),
	!.

recursiveDescentMayFail(O/N,N,[],VarsR,NF,Code):-
	opCall(O/N,NF,VarsR,Code),
	!.
recursiveDescentMayFail(O/N,0,VarPairs,VarsR,NF,Code):-
	cont1(nonEqLogic,true),
	!,
	mapP(lambda([(X,Y)],X=Y),VarPairs),
	opCall(O/N,NF,VarsR,Code),
	!.
recursiveDescentMayFail(O/N,FromArg,[(X,XR)|VarPairs],VarsR,NF,Code):-
	A is FromArg+1,
	recursiveDescentNoFail(O/N,A,VarPairs,VarsR,NF,C),
	recursiveDescentMayFail(O/N,A,VarPairs,VarsR,NF,CF),
	Code=	(reduce(X,XR) -> C  ; XR=X,CF),
	!.


recursiveDescentMayFail(RedIndex,O/N,N,[],VarsR,NF,Code):-
	opCall(RedIndex,O/N,NF,VarsR,Code),
	!.
recursiveDescentMayFail(RedIndex,O/N,0,VarPairs,VarsR,NF,Code):-
	cont1(nonEqLogic,true),
	!,
	mapP(lambda([(X,Y)],X=Y),VarPairs),
	opCall(RedIndex,O/N,NF,VarsR,Code),
	!.
recursiveDescentMayFail(RedIndex,O/N,FromArg,[(X,XR)|VarPairs],VarsR,NF,Code):-
	A is FromArg+1,
	recursiveDescentNoFail(O/N,A,VarPairs,VarsR,NF,C),
	recursiveDescentMayFail(RedIndex,O/N,A,VarPairs,VarsR,NF,CF),
	Code=	(reduce(X,XR) -> C  ; XR=X,CF),
	!.

condReduceClausesFor(C,Clauses) :-
	condReduceClausesAndNonOConds(C,Clauses,_CNO),
	!.


condReduceClausesAndNonOConds(C,CC,CNO):-
	length(C,N),
	listOfNumbers(N,Positions),
	pairing(Positions,C,Conds),
	mapF(lambda([(Pos,CondEq),ClsAndNO],
		buReducerForCondition((cond(Pos),[],CondEq),ClsAndNO)),
	     Conds,ClausesAndCNOList),
	pairing(ClausesList,CNOList,ClausesAndCNOList),
	rReduce(append,[],ClausesList,Clauses),
	rReduce(append,[],CNOList,CNO),
	mapF(lambda([(L=R),Context],contextClause(L,R,Context)),CNO,Contexts),
	append(Clauses,Contexts,CC),
	!.
			


orderedCondition(L0,R0,L,R):-
	orderSortedOrdering(("
Orienting condition equation
	%:",['$equation'([],[L0=R0])]),condEq,L0,R0,L,R,choices([],[]),_Ext,Mode),
	Mode=oriented(_),
	!.
orderedCondition(L,R,L,R):-
	constrTerm(R),
	\+R= @_,
	\+constrTerm(L),
	!.
orderedCondition(R,L,L,R):-
	constrTerm(R),
	\+R= @_,
	\+constrTerm(L),
	!.


buReducerForCondition((P,[],(L=R)),Result) :-
	R@<L,
	!,
	buReducerForCondition1((P,[],(L=R)),Result).
buReducerForCondition((P,[],(R=L)),Result) :-
	buReducerForCondition1((P,[],(L=R)),Result).




buReducerForCondition1((_,[],(L=R)),Result) :-
	conditionReduceClause(L,R,Result),
	!.
buReducerForCondition1((cond(Pos),[],(L0=R0)),(Cls,[])) :-
	orderedCondition(L0,R0,L,R),
	!,
	buReducerFor(cond,(Pos,[],(L=R)),C1),
	functor(L,O,_),
	(ac_ist_AC(O) ->
		LX=..[O,L,X],
		RX=..[O,R,X],
		buReducerFor(cond,(Pos,[],(LX=RX)),C2),
		buReducerForGeneralACOp(cond,(Pos,[],(L=R)),C3),
		buReducerForGeneralACOp(cond,(Pos,[],(LX=RX)),C4),
		rReduce(append,[],[C1,C2,C3,C4],Cls)
	;
		Cls=C1
	),
	assert(conditionReduceClause(L0,R0,(Cls,[]))),
	!.
buReducerForCondition1((_,_,(L=R)),([],[L=R])):-
	prologMatch(L,R),
	assert(conditionReduceClause(L,R,([],[L=R]))),
	!.
buReducerForCondition1((_,_,(R=L)),([],[L=R])):-
	assert(conditionReduceClause(R,L,([],[L=R]))).
		

contextClause(L,R,'$context'(L,R)):-
	prologMatch(L,R).
contextClause(L,R,'$context'(R,L)).
				


% constant part of the reducing mechanism

reduceC([],RedIndex,T,NF) :-
	reduceI(T,RedIndex,NF),
	!.
reduceC([],_RedIndex,T,T):-
	!.
reduceC(CondReduceClauses,RedIndex,T,NF) :-
	map(asserta,CondReduceClauses,Refs),
	reduceR(T,RedIndex,NF),
	mapA(erase,Refs),
	!.

reduceU(CondReduceClauses,T,NF) :-
	map(asserta,CondReduceClauses,Refs),
	reduceR(T,NF),
	mapA(erase,Refs),
	!.


reduceR(T,T1):-
	reduce(T,T1),
	!.
reduceR(T,T).

reduceRL([],[]).
reduceRL([X|Xs],[XN|XNs]):-
	(	reduceRLMayFail(Xs,XNs)
	;	Xs=XNs),
	(reduce(X,XN) ->
		true
	;	X=XN).


reduceRLMayFail([X|Xs],[XN|XNs]):-
	(reduceRLMayFail(Xs,XNs) ->
		(reduce(X,XN) ->
			true
		;	X=XN)
	;	(reduce(X,XN) ->
			Xs=XNs)).





reduceR(T,RI,T1):-
	reduceI(T,RI,T1),
	!.
reduceR(T,_,T).

/*----------------------------------------------------------------------*/
/* reduceL(+LetTerm,-Normalform)                                        */
/* the special varsion of reduceR for let-Terms                         */

reduceL(let Defs in Exp,NF,RedRel) :-
   !,
   reduceD(Defs,RedRel),
   reduceL(Exp,NF,RedRel).
reduceL(Term,NF,RedRel) :-
   CALL=..[RedRel,Term,NF],
   CALL.


/*----------------------------------------------------------------------*/
/* reduceD(+Definitions)                                                */
/* binds the variables on the lefthandsides of Definitions to terms by  */
/* reducing the righthandsides of Definitions and matching with the     */
/* lefthandsides                                                        */

reduceD([],_RedRel) :- !.
reduceD([(CT = T)|Defs],RedRel) :-
   reduceL(T,NF,RedRel),
   contextuallyEqual(NF,CT),
   reduceD(Defs,RedRel).


rewriteExpertClause(O,
	[(reduce(Template,Nf) :-
		ac_del_assoc(O,[A,B],AllArgs),
		rewriteExpert((_,infty),O,AllArgs,AllArgs1),
		reduceRL(AllArgs1,AllArgsNf1),
		(	AllArgsNf1=[Nf]
		;	(AllArgs1=AllArgsNf1 ->
				AllArgsNf=AllArgsNf1
			;	ac_del_assoc(O,AllArgsNf1,T),
				rewriteExpert((_,infty),O,T,AllArgsNf)
			),
			(RestrictedReduceCall->
				true
			;	AllArgs\==AllArgsNf,
				ac_rreduce(O,AllArgsNf,Nf)
			)
		),
		!		
	),
	(reduceI(Template,RedIndex,Nf) :-
		ac_del_assoc(O,[A,B],AllArgs),
		rewriteExpert(RedIndex,O,AllArgs,AllArgs1),
		reduceRL(AllArgs1,AllArgsNf1), 	% rs 29.03.88 reduce -> reduceR
		(	AllArgsNf1=[Nf]
		;	(AllArgs1=AllArgsNf1 ->
				AllArgsNf=AllArgsNf1
			;	ac_del_assoc(O,AllArgsNf1,T),
				rewriteExpert((_,infty),O,T,AllArgsNf)
			),
			(AllArgs\==AllArgsNf ->
				(RestrictedReduceCall ->
					true
				;	ac_rreduce(O,AllArgsNf,Nf)
				)
			;	RestrictedReduceCallI
			)
		),
		!
	)]

) :-
	Template=..[O,A,B],
	mkAtom('$2G%',[O],RewriteOp),
	RestrictedReduceCall=..[RewriteOp,Nf,AllArgsNf],
	RestrictedReduceCallI=..[RewriteOp,RedIndex,Nf,AllArgsNf],
	!.




thisRuleReductive:-
	cont1(self(superpositionTerms),ST),
	length(ST,1).


smallerRedIndex(_,_,infty):-
	!.
smallerRedIndex(_Rule,I,I1):-
	I<I1.
smallerRedIndex(_,I,I):-
	!,
	fail.
smallerRedIndex(R,I,J):-
	D is I-J,
	D < 100,
	I1 is J-1,
	max(I1,0,II),
	II<J,
	seta('$rule',R,redIndexes,(II,infty)),
	!.


applying('$rule'(N,R)):-
	cont(trace,Kind),
	(Kind == on ; Kind == ms ; Kind == os),
	cont(rulesToTrace,L), 
	L \== all,
	!,
	(member(N,L) -> 
		print('$applying'(Kind,'$rule'(N,R)))
	;
		true
	).
applying(R) :-
	cont(trace,Kind),
	(Kind == on ; Kind == ms ; Kind == os),
	print('$applying'(Kind,R)).
applying(_).

myRuleIndex(I,I). 
