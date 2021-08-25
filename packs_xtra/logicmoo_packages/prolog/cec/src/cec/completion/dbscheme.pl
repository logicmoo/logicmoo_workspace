/*
 *	file:		dbscheme.pl
 *	version:	1.5
 *	date:		December 15, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains descriptions of the attributes of objects, where
 *	objects are equations, rules and nonoperational equations,  and
 *	definitions of dependencies between objects. Attribute evaluation
 *	functions are given.
 *
 *	history:
 *	891128	uh	Added this comment
 *      891215  js      removed call to distinctRightSides from
 *			maySuperposeWith. It would only be appropriate
 *			to remove superpositions at the root if the
 *			right sides were equal.
 *	900320	uh 	Changed calls to orderSortedOrdering
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

/*----------------------------------------------------------------------*
 * 			concrete db structure				*
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
 *		     declaration of relation schemes			*
 *----------------------------------------------------------------------*/

relationTypes(['$equation','$rule','$nonoperational equation']).


/*----------------------------------------------------------------------*
 *		       declaration of attributes			*
 *----------------------------------------------------------------------*/
/* origin
 *   describes the origin of an object, one of
 *   user
 *   super(I,TI,TyJ,J,LJ,TJ)
 *     I:   Index of Rule to superpose, or reflexivity
 *     TI:  Side of Rule to superpose
 *     TyJ: Type of object to be superposed onto
 *     J:   Index of object
 *     LJ:  Literal to superpose on (0=conclusion,N=condition N)
 *     TJ:  Side of Literal to superpose
 *   orderSortedTrans
 *   narrow(I,J)
 *   '$rule'(I)
 *   redRule(I)
 */

attributes('$equation',
	[opsLeft,opsRight,condOps,opClasses,complexityBound,
	 prologVariant,redIndexes,consequentType,
	 origin,reducedTag,priority,
	 usedForSimplification,absolutelyNonreductive]).
attributes('$rule',
	[superpositionTerms,opsLeft,rootLeft,opsRight,opClasses,
	 complexityBound,prologVariant,redIndexes,reduceClause,
	 unificationType,
	 auxRule,origin,consequentType, narrowRule, auxNarrowRule]).
attributes('$nonoperational equation',
	[size,rootLeft,consequentType,
	 opsInFirstCondition,opsLeft,origin,complexityBound,prologVariant,
	 opClasses,kind,usedForSimplification,
	 superpositionTerms,redIndexes,
	 isReductive,reduceClause,	% hg 1.8.90
	 conclusionOriented,absolutelyNonreductive]).

functDependency('$rule',(prologVariant,reduceClause)).
functDependency('$nonoperational equation',(prologVariant,reduceClause)). % hg 1.8.90
functDependency('$nonoperational equation',(isReductive,reduceClause)). % hg 1.8.90
functDependency('$rule',(prologVariant,unificationType)).
functDependency('$nonoperational equation',(prologVariant,kind)).
functDependency('$rule',(superpositionTerms,reduceClause)).
functDependency('$rule',(redIndexes,reduceClause)).
functDependency(_,(complexityBound,prologVariant)).

clauseAttribute('$rule',reduceClause).
clauseAttribute('$nonoperational equation',reduceClause).


/*----------------------------------------------------------------------*
 *		       sort criteria for objects			*
 *----------------------------------------------------------------------*/




/* old version
 sortCriterium('$equation',I,eq(N,C,S)) :-
	opsLeft(I,'$equation',(_,OL)),
	opsRight(I,'$equation',(_,OR)),
	(condOps(I,'$equation',(_,C)) -> true; C=0),	%C=0 for old states
	max(OL,OR,N1),
	max(N1,C,N),
	S is OL+OR+C.
	
*/

sortCriterium('$equation',I,N) :-
	condOps(I,'$equation',N).


sortCriterium('$equation'(C,[E]),eq(SS,SL,S1)) :-
	E=(L=_R),
	depthAndSizeOf(L,(DL,OL)),
	clauseSize((C,[E]),SS),
	length([E|C],S1),
	SL is DL*OL.


termWeight(T,W):-
	depthAndSizeOf(T,(D,S)),
	W is S*D*D,
	!.


clauseSize((C,[E]),SS):-
	woConstructorTerms([E|C],Ts),
	mapF(termWeight,Ts,S),
	rReduce(max,0,S,SMax),
%	rReduce(min,10000,S,SMin),
	length(C,NC),
	SS is SMax + 10*NC.
%	SS is 2*SMax - SMin + 20*NC.


/*----------------------------------------------------------------------*
 *		       subsumption predicate for objects		*
 *----------------------------------------------------------------------*/

subsumed('$equation',J,(C,D),As,I) :-
	length(C,N),
	N > 0,
	prologVariant(I,'$equation',((CP,[DP]),ComplIP,_Env)),
	\+ origin(I,'$equation',user),
	I \== J,
	length(CP,N),
	containsInstanceOf(D,DP),
	impInstanceOfAll(C,CP),
	member((complexityBound,Compl),As),
	fromProlog([],ComplIP),
	simplerOrEqProof('$equation',J,Compl,(CP,[DP]),ComplIP),
	seta('$equation',I,usedForSimplification,yes),
	!.



subsumedNOpEq(I,J) :-	% J subsumes I
	'$nonoperational equation'(I,(C,D)),
	complexityBound(I,'$nonoperational equation',CB),
	prologVariant(J,'$nonoperational equation',((C,D),CB,_)),
	I \== J.


subsumedRule(I,J) :-	% J subsumes I
	'$rule'(I,(C,D)),
	prologVariant(J,'$rule',((C,D),_,_)),
	I \== J.
%	redIndexes(I,'$rule',(RIL,_)),
%	redIndexes(J,'$rule',(RJL,_)),
%	RJL=<RIL.



subsumed1('$rule',W1,I,W2,J):-
	subsumedRule(W1,I,W2,J).
subsumed1('$nonoperational equation',W1,I,W2,J):-
	subsumedNOpEq(W1,I,W2,J).

subsumedNOpEq(W1,I,W2,J) :-	% J in W2 subsumes I in W1 W1\==W2
	W1:'$nonoperational equation'(I,(C,D)),
	W1:complexityBound(I,'$nonoperational equation',CB),
	W2:prologVariant(J,'$nonoperational equation',((C,D),CB,_)),
	once((W1\==W2;I\==J)).



subsumedRule(W1,I,W2,J) :-	% J subsumes I
	W1:'$rule'(I,(C,D)),
	W2:prologVariant(J,'$rule',((C,D),_,_)),
	once((W1\==W2;I\==J)).






/*----------------------------------------------------------------------*
 *	     actions to be performed after creation of object		*
 *----------------------------------------------------------------------*/

afterCreation('$equation',I) :-
	relaxEqRedIndexes('$equation',I),
	memoize(clauseHasTerm(I,'$equation',Class),
		(opClasses(I,'$equation',OpClasses),member(Class,OpClasses))),
	memoize(mayBeReducible(I,'$equation',Kind),eqIsNotReduced(I,Kind)),
	!.
afterCreation('$rule',I) :-
	adjustRuleRedIndexes(I),
	opsLeft(I,'$rule',(Ops,_)),
	origin(I,'$rule',Origin),
	repeat(makeContextuallyReducible(I,Ops)),
	memoize(clauseHasTerm(I,'$rule',Class),
		(opClasses(I,'$rule',OpClasses),member(Class,OpClasses))),
	overlapsWith(I,Origin),
	!.
afterCreation('$nonoperational equation',I) :-
	relaxEqRedIndexes('$nonoperational equation',I),
	(kind(I,'$nonoperational equation',injectivityLaw) ->
		true
	;
		makeEqsReducible(I),
		makeNOpEqsReducible(I)
	),
	memoize(clauseHasTerm(I,'$nonoperational equation',Class),
		(	opClasses(I,'$nonoperational equation',OpClasses),
			member(Class,OpClasses)
		)
	),
	memoize('$maySuperpose'(Char,I1,A1,'$nonoperational equation',I,CL,A),
		maySuperposeWith(Char,user,I1,A1,
				      user,'$nonoperational equation',I,CL,A)).

maySuperposeWith(Char,W1,I1,A1,W,'$nonoperational equation',I,Lit,A) :-
	W1:superpositionTerms(I1,'$rule',ST1),
	member((conclusion,A1),ST1),
	rootOpW(W1,I1,A1,O),
	W:superpositionTerms(I,'$nonoperational equation',ST),
	member((Lit,A),ST),
	opsInTermW(I,W,'$nonoperational equation',Lit,A,Ops),
	member(O,Ops),
	distinctRightSides(W1,I1,A1,W,'$nonoperational equation',I,Lit,A),
	newSuperpositions:==true,
	characteristicsW(I1,W1,'$rule',conclusion,A1,C1), 
	characteristicsW(I,W,'$nonoperational equation',Lit,A,C),
	(Lit=condition(1) ->
		Ty='$nonoperational equation'
	;	Ty='$rule'
	),
	overlapCharacteristics(C1,'$rule',C,Ty,Char).
maySuperposeWith(Char,_W1,reflexivity,_,W,'$nonoperational equation',I,condition(1),_A) :-
	W:superpositionTerms(I,'$nonoperational equation',ST),
	member((condition(1),root),ST),
	newSuperpositions:==true,
	characteristicsW(I,W,'$nonoperational equation',condition(1),left,C1),
	characteristicsW(I,W,'$nonoperational equation',condition(1),right,C),
	overlapCharacteristics(C1,reflexivity,C,'$nonoperational equation',Char).
maySuperposeWith(Char,W1,I1,A1,W,'$rule',I,Lit,A) :-
	W1:superpositionTerms(I1,'$rule',ST1),
	member((conclusion,A1),ST1),
	rootOpW(W1,I1,A1,O1),
	W:superpositionTerms(I,'$rule',ST),
	member((Lit,A),ST),
	opsInTermW(I,W,'$rule',Lit,A,Ops),
	member(O1,Ops),
	distinctRightSides(W1,I1,A1,W,'$rule',I,Lit,A),	    
	newSuperpositions:==true,
	characteristicsW(I1,W1,'$rule',conclusion,A1,C1),
	characteristicsW(I,W,'$rule',Lit,A,C),
	overlapCharacteristics(C1,'$rule',C,'$rule',Char).
maySuperposeWith(Char,_W1,reflexivity,_,W,'$rule',I,condition(J),_A) :-
	W:superpositionTerms(I,'$rule',ST),
	member((condition(J),root),ST),
	newSuperpositions:==true,
	characteristicsW(I,W,'$rule',condition(J),left,C1),
	characteristicsW(I,W,'$rule',condition(J),right,C),
	overlapCharacteristics(C1,'$rule',C,reflexivity,Char).


%
inWorld(Attribute,W,I,Type,Value) :-
	!,
	F =.. [Attribute,I,Type,Value],
	W:F.


kindW(W,I,T,Kind) :-
	W:kind(I,T,Kind).


rootOpW(W,I,left,O) :-
	W:rootLeft(I,'$rule',O).
rootOpW(W,I,right,O) :-
	W:'$rule'(I,(_,[_=R])), 
	functor(R,O,_).

:-dynamic characteristicsW/6.	
characteristicsW(I,W,T,Lit,A,(SS,S1)) :-
%	termOfClauseW(W,I,T,Lit,A,Term),
%	otherSide(A,A1),
%	termOfClauseW(W,I,T,Lit,A1,Term1),
%	sizeOf(Term,S),
%	sizeOf(Term1,S1),
	Obj =.. [T,I,(C,[E])],
	W:Obj,
	clauseSize((C,[E]),SS),
	length([E|C],S1),
	asserta((characteristicsW(I,W,T,Lit,A,(SS,S1)):-!)),
	!.

otherSide(left,right).
otherSide(right,left).

opsAndSizeOfTermW(W,I,T,Lit,A,(Ops,Size)) :-
	termOfClauseW(W,I,T,Lit,A,Term),
	termInfo(Term,(Ops,Size)).


distinctRightSides(_W1,_I1,_A1,_W,_Ty,_I,condition(_),_A):-
	!.
distinctRightSides(W1,I1,_A1,_W,_Ty,_I,_Lit,_A):-
	auxRuleW(W1,I1),
	!.
distinctRightSides(_W1,_I1,_A1,W,'$rule',I,_Lit,_A):-
	auxRuleW(W,I),
	!.
distinctRightSides(W1,I1,A1,W,Ty,I,Lit,A):-
	otherSide(A1,B1),
	otherSide(A,B),
	termOfClauseW(W1,I1,'$rule',conclusion,B1,T1),
	termOfClauseW(W,I,Ty,Lit,B,T),
	!,
	\+ (	T1=T,
		topTermSort(T),
		varsIn(T,0)),
	!.


topTermSort(T):-
	get_type(T,Ty,[]),
	\+ (	(_ ofType (_:[_|Ts])),
		member(Ty,Ts)),
	!.




auxRuleW(W,I):-
	W:auxRule(I,'$rule',(_,[_=_])).



:-dynamic opsInTermW/6.
opsInTermW(I,W,T,Lit,A,Ops) :-
	opsAndSizeOfTermW(W,I,T,Lit,A,(Ops,_)),
	asserta((opsInTermW(I,W,T,Lit,A,Ops):-!)),
	!.

termOfClauseW(W,I,T,conclusion,A,Te) :-
	!,
	Obj =.. [T,I,(C,[E])],
	W:Obj,
	termIn([E|C],0,A,Te).
termOfClauseW(W,I,T,condition(J),A,Te) :-
	!,
	Obj =.. [T,I,(C,[E])],
	W:Obj,
	termIn([E|C],J,A,Te).

termIn(Lits,I,left,L) :-
	select(Lits,I,(L=_)).
termIn(Lits,I,right,R) :-
	select(Lits,I,(_=R)).



makeContextuallyReducible(I,Ops) :-
	'$rule'(I1,_),
	I \== I1,
	makeContextuallyReducible(Ops,I1,'$rule').
makeContextuallyReducible(_I,Ops) :-
	kind(I1,'$nonoperational equation',K),
	functor(K,other,_),
	makeContextuallyReducible(Ops,I1,'$nonoperational equation').
makeContextuallyReducible(_I,Ops) :-
	makeContextuallyReducible(Ops,_I1,'$equation').


makeContextuallyReducible(Ops,I1,Type1) :-
	clauseHasTerm(I1,Type1,Ops1),
	\+ mayBeReducible(I1,Type1,contextually),
	subset(Ops,Ops1),
	repeat(retract(mayBeReducible(I1,Type1,_))),	
		% axiom may have been reducible of kind "general"
		% or of kind "contextually(K)", for various K,
		% it now becomes potentially reducible by contextual rewriting
		% in general
	assertz(mayBeReducible(I1,Type1,contextually)).

mayUnify(@_L,_R) :- !.
mayUnify(_L,@_R) :- !.
mayUnify(L,R) :-
	functor(L,F,N),
	functor(R,F,N).


eqIsNotReduced(I,_) :-
	reducedTag(I,'$equation',reduced),
	!,
	fail.
eqIsNotReduced(I,K) :-
	(eqIsContextuallyReduced(I) ->
		'$equation'(I,([_|_],_)),
		nonsimpleNOpEq,
		K = general
	;	K = contextually
	).

eqIsContextuallyReduced(_) :-
	\+ '$rule'(_,_),
	\+ ac_ist_C(_).
eqIsContextuallyReduced(I) :-
	'$equation'(I,([],_)),
	origin(I,'$equation',O),
	member(O,[super(_,_,_,_,_,_),redRule(_)]).

nonsimpleNOpEq:-
	'$nonoperational equation'(I,([_|_],_)),
	kind(I,'$nonoperational equation',K),
	K \== injectivityLaw,
	!.



makeEqsReducible(_) :-
	'$equation'(I,_),
	try(retract(mayBeReducible(I,'$equation',Kind))),
	(nonvar(Kind) ->
		assert(mayBeReducible(I,'$equation',Kind))
	;	assert(mayBeReducible(I,'$equation',general))
	),
	fail.	
makeEqsReducible(_).


makeNOpEqsReducible(I):-
	prologVariant(I,'$nonoperational equation',(EIP,_,_)),
	\+redPredGenerated(J),
	I\==J,
	'$nonoperational equation'(J,EJ),
	callOnce(ac_match(EJ,EIP)),
	try(retract(mayBeReducible(J,'$nonoperational equation',Kind))),
	(nonvar(Kind) ->
		assert(mayBeReducible(J,'$nonoperational equation',Kind))
	;	assert(mayBeReducible(J,'$nonoperational equation',general))
	),
	fail.	
makeNOpEqsReducible(_).
	


/*----------------------------------------------------------------------*
 *	     actions to be performed after deletion of object		*
 * afterDeletion(W,Type,I):- fail.					*
 *		... none, except for removing dependants		*
 *----------------------------------------------------------------------*/


/*----------------------------------------------------------------------*
 *	     	dependencies between objects and clauses		*
 *----------------------------------------------------------------------*/

dependent(_,T,I,mayBeReducible(I,T,_)).
dependent(_,'$rule',I,mayBeReducible(_,_,contextually(I))).
dependent(_,T,I,clauseHasTerm(I,T,_)).
dependent(_,'$rule',I,'$maySuperpose'(_,I,_,_,_,_,_)).
dependent(_,'$rule',I,'$maySuperpose'(_,_,_,'$rule',I,_,_)).
dependent(_,'$rule',I,specialLaw('$rule'(I,_Law))).
dependent(W,'$rule',I,object(J,'$equation')) :-
	W:origin(J,'$equation',super(_J,_TJ,'$rule',I,0,_TI)),
	W:usedForSimplification(J,'$equation',no).
dependent(W,'$rule',I,object(J,'$equation')) :-
	W:origin(J,'$equation',super(I,_TI,_TyJ,_J,_LJ,_TJ)),
	W:usedForSimplification(J,'$equation',no).
dependent(_,'$nonoperational equation',I,'$maySuperpose'(_,_,_,'$nonoperational equation',I,_,_)).

/* 
 *  the following dependencies are only valid as long as the non-
 *  operational equation e has not been used for the simplification
 *  of other equations. Upon elimination of one of the sources
 *  (a rule or a nonoperational equation), 
 *  a simpler proof of this rule or equation (and hence of e)
 *  has been found. The complexity of the new proof of  e 
 *  might nevertheless exceed the complexity of 
 *  e. Simplifications that use e might therefore not be
 *  possible without e.
 *
 * dependent('$rule',I,object(J,'$nonoperational equation')) :-
 *	origin(J,'$nonoperational equation',narrow(I,_)).
 * dependent('$nonoperational equation',I,object(J,'$nonoperational equation')) :-
 *	origin(J,'$nonoperational equation',narrow(_,I)).
 *
*/

/*----------------------------------------------------------------------*
 *	     	     attribute evaluation functions			*
 *----------------------------------------------------------------------*/

priority(('$equation',_,_),(priority,normal)).


kind(('$nonoperational equation',_,E),(kind,other(symmetric))):-
	cont1(self(prologVariant),(EP,_,_)),
	symmetricEq(E,EP),
	!.
kind(('$nonoperational equation',_,_),(kind,other(nonsymmetric))).

symmetricEq(_,_):-
	fail.

opsInFirstCondition(('$rule',_,([_L=R|_],_)),(opsInFirstCondition,Ops)) :-
	termInfo(R,(Ops,_)),
	!.
opsInFirstCondition((_,_,([],_)),(opsInFirstCondition,'$nop')).
opsInFirstCondition((_,_,([C|_],_)),(opsInFirstCondition,Ops)) :-
	termInfo(C,(Ops,_)),
	!.


consequentType((_,_,(_,[E])),(consequentType,T)) :-
	eqType(E,T).



condOps((_,_,(C,D)),(condOps,K)) :-	% wollte den Namen nicht "andern
%	old	termInfo(C,(Ops,Size)),
	sortCriterium('$equation'(C,D),K),
	!.

opsLeft((_,_,(_,[(L=_)])),(opsLeft,(Ops,Size))) :-
	termInfo(L,(Ops,Size)),
	!.


opsRight((_,_,(_,[(_=R)])),(opsRight,(Ops,Size))) :-
	termInfo(R,(Ops,Size)),
	!.


opClasses((_,_,(C,[L=R])),(opClasses,Classes)) :-
	setof1(Class,
		[T,T1,S]^(
			(member((T=T1),[L=R|C]);member((T1=T),[L=R|C])),
			\+ constrTerm(T),
			termInfo(T,(Class,S)),
			Class \== []
		),Classes),
	!.



unificationType(('$rule',_,_),(unificationType,U)):-
	cont1(self(prologVariant),((_,[LP=_]),_,_)),
	(containsCOp(LP) ->
		U=ac_uniAC
	;	(linear(LP) ->
			U='='
		;	U=unify)),
	!.




complexityBound(('$rule',_,(C,[L=R])),(complexityBound,((['$r'(L)],['$r'(R)]),Co))) :-
	nredComplexity((C,[L=R]),Co),
	!.
complexityBound(('$equation',_,(C,[L=R])),
		(complexityBound,((['$r'(L)],['$r'(R)]),[Co]))) :-
	!,
	woConstructorTerms([L=R|C],WO),
	Co =.. ['$r'|WO].
complexityBound((_,_,(C,[L=R])),(complexityBound,((['$r'(L)],['$r'(R)]),Co))) :-
	nredComplexity((C,[L=R]),Co).


nredComplexity((C,[L=R]),[Co]) :-
	woConstructorTerms([L=R|C],WO),
	Co =.. ['$r'|WO],
	!.

% im Notfall:
%nredComplexity([Co1],(C,[L=R]),[Co]) :-
%	woConstructorTerms([L=R|C],InherentCo),
%	Co=..['$r'|InherentCo].




nredComplexity([Co1],(C,[L=R]),[Co]) :-
	woConstructorTerms([L=R|C],InherentCo),
	(Co1=..['$r'|AssignedCo] ->
		intersect(AssignedCo,InherentCo,Intersection),
		diff(InherentCo,AssignedCo,IMinusA),
		diff(AssignedCo,Intersection,CanBeLowered),
		bagof1(T,S^(	member(T,IMinusA),
				callOnce((member(S,CanBeLowered),
					  greaterInRedOrderNoExt(S,T)))),
			T2),
		append(Intersection,T2,Ts)
	;	Ts=InherentCo),
	Co =.. ['$r'|Ts],
	!.



/* woConstructorTerms(C,Ts)
 * computes a list Ts of all [nonconstructor] terms in the condition C.
 * the same (?) as complexity/2
 */


woConstructorTerms(T=S,[T,S]).
woConstructorTerms([E|Es],EEswo) :-
	woConstructorTerms(E,Ewo),
	woConstructorTerms(Es,Eswo),
	append(Ewo,Eswo,EEswo).
woConstructorTerms([],[]).

literalsOf([],[]).
literalsOf([T=S|Es],[T,S|Ls]) :-
	woConstructorTerms(Es,Ls).




declarePrecedences(OpMs,_,S) :-
	tps_current_ordering(kns_or_neqkns),
	(S = lr;S = rl),
	opPrefix(OpMs,OpOs,_),
	tps_greater([[OpMs,'$r-ms']]),
	tps_status(OpMs,S),
	tps_greater([[OpOs/_,'$r'/_]]),
	tps_status(OpOs/_,S),
	!.
declarePrecedences(_,_,_).		




reducedTag(('$equation',_,_),(reducedTag,nonreduced)) :-
	!.


rootLeft((_,_,(_,[(L=_)])),(rootLeft,O)) :-
	functor(L,O,_),
	!.


prologVariant((_,_,R),(prologVariant,(RP,ComplP,Subst))) :-
	cont1(self(complexityBound),Compl),
	toProlog((R,Compl),[],(RP,ComplP),Subst),
	!.


reduceClause(('$rule',I,(C,[L=R])),(reduceClause,Clauses)) :-
	cont1(self(prologVariant),((CP,[LP=RP]),_,Env)),
	functor(L,O,_),
	buReducerForRule((I,CP,(LP=RP)),Clauses11),
	(ac_ist_AC(O) ->
		ruleKind(C,L,R,Kind),
		(Kind = general ->
			buReducerForGeneralACOpRule((I,CP,(LP=RP)),Clauses12),
			append(Clauses11,Clauses12,Clauses1)
		;
			assert(specialLaw('$rule'(I,Kind))),
			Clauses1 = Clauses11
		),
		map(assert,Clauses1,Ref1) 
		/* In the AC-case, the reduce clauses have to be immediately
		asserted, to allow proper generation of the corresponding
		auxiliary rule 
		*/
	;
		Clauses1 = Clauses11
	),	
	(auxRule(O,LP,Env,'$context',R,C,CP,LPAux,RPAux) ->
		assign1(self(auxRule),(CP,[LPAux=RPAux])),
		buReducerForRule((I,CP,(LPAux=RPAux)),Clauses21),
		(Kind = general ->
			buReducerForGeneralACOpRule((I,CP,(LPAux=RPAux)),Clauses22),
			append(Clauses21,Clauses22,Clauses2)
		;
			Clauses2 = Clauses21
		),
		append(Clauses1,Clauses2,Clauses)
	;
		Clauses = Clauses1,
		assign1(self(auxRule),(_,none))
	),
	(ac_ist_AC(O) ->
		mapA(erase,Ref1)
	;
		true
	),
	!.
reduceClause(('$nonoperational equation',_,(D,[L=_R])),(reduceClause,[])) :-
	(
		\+cont1(indProve,true),
		\+cont1(domainConstraints,on)
	;	
		cont1(self(isReductive),no)
	;	cont1(self(complexityBound),((Co,Co),Co)),
		\+msComplexity(Co)
	;	D=[]
	;	sizeOf(L,S),
		S>12),
	!.
reduceClause(('$nonoperational equation',I,(C,[L=R])),(reduceClause,Clauses)) :-
	cont1(self(prologVariant),((CP,[LP=RP]),_,Env)),
	functor(L,O,_),
	buReducerFor('$nonoperational equation',(I,CP,(LP=RP)),Clauses11),
	(ac_ist_AC(O) ->
		buReducerForGeneralACOp('$nonoperational equation',(I,CP,(LP=RP)),Clauses12),
		append(Clauses11,Clauses12,Clauses1)
	;	Clauses1 = Clauses11),
	(auxRule(O,LP,Env,'$context',R,C,CP,LPAux,RPAux) ->
		buReducerForRule((I,CP,(LPAux=RPAux)),Clauses21),
		buReducerForGeneralACOp('$nonoperational equation',(I,CP,(LPAux=RPAux)),Clauses22),
		append(Clauses21,Clauses22,Clauses2),
		append(Clauses1,Clauses2,Clauses)
	;	Clauses = Clauses1),
	!.



isReductive(('$nonoperational equation',_,_),(isReductive,no)).


size(('$nonoperational equation',_,(C,D)),(size,S)) :-
	sortCriterium('$equation'(C,D),S),
	!.


origin((_,_,_),(origin,unknown)).


superpositionTerms(('$rule',_,_),(superpositionTerms,[(conclusion,left)])) :-
	!.
superpositionTerms((_,_,_),(superpositionTerms,[])).


redIndexes((_,_,_),(redIndexes,(infty,infty))).



absolutelyNonreductive((_,_,([],_)),(absolutelyNonreductive,[])) :-
	!.
absolutelyNonreductive((_,_,(C,[L=R])),(absolutelyNonreductive,N)) :-
	(	(	member((A=B),C),
			(	greaterOrEqual(A,L)
			;	greaterOrEqual(B,L)
			)
		;	greaterOrEqual(R,L)
		)
	->	N1 = [l]
	;	N1 = []
	),
	(	(	member((A1=B1),C),
			(	greaterOrEqual(A1,R)
			;	greaterOrEqual(B1,R)
			)
		;	greaterOrEqual(L,R)
		)
	->	N = [r|N1]
	;	N = N1
	),
	!.




greaterOrEqual(X,X) :-
	!.
greaterOrEqual(A,B) :-
	greaterInRedOrderNoExt(A,B).



/*----------------------------------------------------------------------*
 *	     	    	   auxiliary functions				*
 *----------------------------------------------------------------------*/

auxRule(('$rule',_,(_,_)),(auxRule,N)) :-
	cont1(self(auxRule),N),
	% The effect of renaming the variables in the rule is
	% absolutely required.
	!.





auxRule(O,_,_,_,_,_,_,_,_) :-
	not ac_ist_AC(O),
	!,
	fail.
auxRule(O,LP,Env,Var,R,C,CP,_,_) :-
	LP =.. [O|Args],
	ac_del_assoc(O,Args,OArgs),
	singleVar([],OArgs,SVarP),
	not ac_occurs(CP,SVarP),   
% auxRule needed if singleVar is in condition C
	findPVar(SVar,Env,SVarP),
% gets the non-prolog name of SVarP in Env
	T =.. [O,@ SVar, @ Var],
        applySubst([(SVar,T)],R,RT),
	RAux =.. [O,R,@ Var],
        condReduceClausesFor(C,Clauses),
	reduceC(Clauses,(min,infty),RAux,RAuxN),
	reduceC(Clauses,(min,infty),RT,RTN),
	ac_matchvf(RAuxN,RTN),
	!,
	fail.
auxRule(O,LP,Env,Var,R,_,_,LPAux,RPAux) :-
	RAuxN =.. [O,R,@ Var],
	LPAux =.. [O,LP,Context],
	toProlog(RAuxN,[(Var,Context)|Env],RPAux,_),
	!.







findPVar(V,[(V,PV)|_],PV1) :-
	PV1 == PV,
	!.
findPVar(V,[_|S],PV) :-
	findPVar(V,S,PV).



overlapsWith(I,'$rule'(I1)) :-
	memoize('$maySuperpose'(Ch,I,TI,Ty,J,L,TJ),'$maySuperpose'(Ch,I1,TI,Ty,J,L,TJ)),
	memoize('$maySuperpose'(Ch,J,TJ,'$rule',I,L,TI),
		('$maySuperpose'(Ch,J,TJ,'$rule',I1,L,TI),I1\==J)),
	!.
overlapsWith(I,_OtherOrigin) :-
	memoize('$maySuperpose'(Ch,I,TI,Ty,J,L,TJ),
		maySuperposeWith(Ch,user,I,TI,user,Ty,J,L,TJ)),
	memoize('$maySuperpose'(Ch,J,TJ,'$rule',I,L,TI),
		(maySuperposeWith(Ch,user,J,TJ,user,'$rule',I,L,TI),I\==J)).

	
overlapCharacteristics((SL,SR),_,(SL1,SR1),'$rule',(Q1,Q)) :-
	max(SL,SL1,Q1),
	max(SR,SR1,Q),
	!.
overlapCharacteristics((SL,_SR),_,(SL1,_SR1),_,(Q1,Q)) :-
	Q1 is SL1+2*SL//3,	% max(SL,SL1,M),
			% Q1 is M+5,
	Q is SL,	% Q is Q1 +2,
	!.






newNonreductiveEquation(I,([],D),As,Ref) :-
	nopEqComplexity(As,I,([],D),Complexity,_),
	(permutative(D,_) ->
		ST=[]
	;	ST=[(conclusion,left),(conclusion,right)]
	),
	new('$nonoperational equation',([],D),_,
		[(complexityBound,Complexity),(conclusionOriented,no),
		 (superpositionTerms,ST)|As],Ref).
newNonreductiveEquation(I,([FC|C],[E]),As,Ref) :-
	tryToOrientLiteral(E,E1,ModeC),
	orderConditions(I,([FC|C],[E1]),ModeC,Conds),		% rs 4.01.89
	nopEqComplexity(As,I,([FC|C],[E]),((RCL,RCR),NC),As1),  
	(ModeC = yes ->
		(E1 = E ->
			Complexity = ((RCL,'$infty'),NC)
		;
			Complexity = ((RCR,'$infty'),NC)
		)
	;
		Complexity = ((RCL,RCR),NC)
	),
	Conds = [FirstCond|C1],
	tryToOrientLiteral(FirstCond,FCond,ST1),
	(	member((kind,Kind),As1)
	;	Kind = other
	),
	superpositionTermsNop(ST1,FCond,Kind,ST),
	CondsO = [FCond|C1],
	reductiveNopEq((CondsO,[E1]),ModeC,ModeR),
	new('$nonoperational equation',(CondsO,[E1]),_,
		[(complexityBound,Complexity),
		(conclusionOriented,ModeC),
		(superpositionTerms,ST),
		(isReductive,ModeR)|As1],Ref),
	!.




superpositionTermsNop(_,_,injectivityLaw,[(condition(1),left)]) :-
	!.
superpositionTermsNop(yes,_,_,[(condition(1),left)]).
superpositionTermsNop(no,(L=R),_,[(condition(1),root),
				(condition(1),left),(condition(1),right)]) :-
	mayUnify(L,R),
	!.
superpositionTermsNop(no,_,_,[(condition(1),left),(condition(1),right)]).


injectivityLaw(([U=V],[@X= @Y])):-
	(	U=..[I,@X],
		V=..[I,@Y]
	;	U=..[I,@Y],
		V=..[I,@X]).


nopEqComplexity(As,_I,E,((Co,Co),Co),As1) :-
	injectivityLaw(E),
	nredComplexity(E,Co),
	(append(AsL,[(kind,_)|AsR],As) ->
		append(AsL,[(kind,injectivityLaw)|AsR],As1)
	;	As1=[(kind,injectivityLaw)|As]),
	!.
nopEqComplexity(As,_I,(C,[E]),(([Complexity],[Complexity]),[Complexity]),As) :-
	member((origin,user),As),
	!,
	length([E|C],N),
	M is N-1,
	(fetchAction(status(Status),_,'$equation'(C,[E]),_,no,_) ->
		true
	;
		askStatus(user,(C,[E]),M,Status),
		assertz(actionNew(status(Status),'$equation'(C,[E])))
	),
	(Status = ms ->
		woConstructorTerms([E|C],WO),
		Complexity =.. ['$r'|WO],
		St = ms
	;
		applyPermutation([E|C],Status,TermOrder),
		woConstructorTerms(TermOrder,WO),
		St = lr,
		cont(ctr('$nonoperational equation'),J),
		I is J+1,
		mkAtom('$neq%-ms',[I],O),
		declarePrecedences(O,N,St),
		Complexity =.. [O|WO]
	).
nopEqComplexity(As,_I,(C,[L=R]),((Co,Co),Co),As) :-
	(	member((origin,orderSortedTrans),As)
	;	member((origin,domainAxiom),As)),
	nredComplexity((C,[L=R]),Co),
	!.
nopEqComplexity(As,I,E,((Co,Co),Co),As) :-
	complexityBound(I,'$equation',((C,C),C)),
	lowerComplexity(C,E,Co),
	!.
nopEqComplexity(As,I,E,(('$infty','$infty'),Co),As) :-
	complexityBound(I,'$equation',(('$infty','$infty'),C)),
	lowerComplexity(C,E,Co),
	!.
nopEqComplexity(As,I,E,((Co,'$infty'),Co),As) :-
	complexityBound(I,'$equation',((C,'$infty'),C)),
	lowerComplexity(C,E,Co),
	!.
nopEqComplexity(As,I,E,(('$infty',Co),Co),As) :-
	complexityBound(I,'$equation',(('$infty',C),C)),
	lowerComplexity(C,E,Co),
	!.
nopEqComplexity(As,I,E,((LRC,RRC),Co),As) :-
	complexityBound(I,'$equation',((LRC,RRC),NC)),
	lowerComplexity(NC,E,Co).
%	complexityBound(I,'$equation',Co).



msComplexity(C):-
	functor(C,R,_),
	redComplOp(R).	

redComplOp(F):-
	name('$r',[D,R]),
	name(F,[D,R|_]),
	!.

nRedComplOp(F):-
	name('$neq',[D,N,E,Q]),
	name(F,[D,N,E,Q|_]),
	!.
		
auxOpForComplexities(F):-
	nRedComplOp(F).
auxOpForComplexities(F):-
	redComplOp(F).


auxOpForComplexities(OpMs,O,M) :-
	opPrefix(OpMs,O,ms),
	atom_chars(O,[D,N,E,Q|Nmb]),
	name('$neq',[D,N,E,Q]),
	name(M,Nmb),
	number(M),
	!.




%lowerComplexity([_OldC],E,[NewC]):-	Old version
%	nredComplexity(E,[NewC]).





lowerComplexity([OldC],E,[New]):-
	tps_current_ordering(poly(_N)),
	OldC=..[NR|_],
	nRedComplOp(NR),
	!,
	nredComplexity([OldC],E,[NewC]),
	NewC=..[_R|Ts],
	deleteBoundedTerms(2,Ts,Ts,Ts1),
	asSet(Ts1,Ts2),
	C=..[NR|Ts2],
	(	orderSortedOrdering(("Lowering complexity of equation:",[]),
					condEq,OldC,C,choices([],[]),no,M),
		M=oriented(_)
	->	New=C
	;	New=OldC).
lowerComplexity(Old,E,New):-
	nredComplexity(Old,E,New).



deleteBoundedTerms(0,_,Ts,Ts):-
	!.
deleteBoundedTerms(N,Ts,[L|Xs],Ts1):-
	\+constrTerm(L),
	member(S,Ts),
	S\==L,
	greaterInRedOrderNoExt(S,L),
	!,
	M is N-1,
	deleteBoundedTerms(M,Ts,Xs,Ts1).
deleteBoundedTerms(N,Ts,[L|Xs],[L|Ts1]):-
	deleteBoundedTerms(N,Ts,Xs,Ts1).
deleteBoundedTerms(_N,_Ts,[],[]).




chosenSuperCondition(Is,I,(C,[L=R]),ConclusionOriented):-
	numberedList(C,CN),
	splitDomConds(CN,DomCN,OCN),
	prologVariant(I,'$equation',(EP,_,_)),
	(symmetricEq((C,[L=R]),EP)->
		COS=yes
	;	COS=ConclusionOriented),
	chosenSC(Is,DomCN,OCN,L,R,COS).


splitDomConds(CN,DomCN,OCN):-
	split(lambda([P],
		[F,X,S,A,B,I]^(	(P=(A=B);P=(I,A=B)),
				B='$true-$pred',
				functor(A,F,1),
				arg(1,A,@X),
				opPrefix(F,'$ground',S))),
		CN,DomCN,OCN),
	!.




chosenSC(Is,DomCN,OCN,L,R,_ConclusionOriented):-
	constrTerm(L),
	constrTerm(R),
	!,
	pairing(IsO,_,OCN),
	pairing(IsD,_,DomCN),
	(IsO=[] ->
		Is=IsD
	;	Is=IsO).
chosenSC(Is,DomCN,OCN,L,R,ConclusionOrientedOrSymmetric):-
	(ConclusionOrientedOrSymmetric=yes ->
		inductionVars(L,V)
	;	inductionVars((L,R),V)),
	setof1(I,
		[A,B,X]^(	member(X,V),
			member((I,A=B),DomCN),
			arg(1,A,X)),
		Is).

				
inductionVar(@_V,_):-
	!,fail.
inductionVar(T,@V):-
	T=..[_|Ts],
	(	(	origin(J,'$nonoperational equation',domainAxiom),
			prologVariant(J,'$nonoperational equation',(([A=_|_],[L=_]),_,_)),
			arg(1,L,T),
			A=..[_,@V]
		->	true
		;	member(@V,Ts))
	;	member(T1,Ts),
		inductionVar(T1,@V)).





inductionVars(T,Vs):-
	setof1(V,inductionVar(T,V),Vs),
	!.


tryToOrientLiteral((L=R),(L1=R1),M) :-
	orderSortedOrdering(("
Trying to orient literal % of nonoperational equation
(if possible, this may speed-up completion):",['$term'(L)='$term'(R)]),
		condEq,L,R,L1,R1,choices([],[]),_,Mode),
	!,
	(Mode = oriented(_) ->
		M  = yes
	;
		M  = no,
		L1 = L,
		R1 = R
	),
	!.


askStatus(_,_,0,ms) :-
	!.
askStatus(user,(C,_),_,ms):-
	cont1(indProve,true),
	splitDomConds(C,_,OCN),
	OCN=[].
askStatus(user,(C,D),N,Status) :-
	!,
%	(tps_current_ordering(kns_or_neqkns) ->
		(cont1(indProve,true) ->
			sPrint("Consider the equation
	%.",['$equation'(C,D)])
		;	true),
		repeat,
		nl,
		sPrint("In which order should the literals of the equation be
inspected when comparing proofs that use this equation?",[]),
		nl,
		sPrint("Please enter ms (for multiset ordering) or a permutation of [0 .. %]
(0 stands for the consequent, i>0 for the ith condition). > ",[N]),
		read(Status),
		(	isStatus(Status,N)
		;
			write('???'),
			fail
%		)
%	;
%		Status = ms
	),
	!.
askStatus(_,_,_,ms).


orderConditions(_,([],_),_,[]) :-
	!.
orderConditions(J,(C,[Eq]),Mode,CO) :-
	(retract(cont(forwardChainedCondition,CF)) ->		% -- HG
	    rReduce(lambda([E,R,LR],
			[Vs,L]^
				(vars(([E|R],[Eq]),Vs),
				expandedDomainEquation(Vs,E,L),
			   	append(L,R,LR))),
		    [],CF,CE),
		append(C,CE,C1)
	;	C1=C),
	selectCondition(J,(C1,[Eq]),Mode,I),
	assertz(actionNew(selectcondition(I),'$equation'(C1,[Eq]))),
	I1 is I-1,
	select(C1,I1,E),
	(member(E,C) ->
		deleteOne(C,E,C2),
		CO = [E|C2]
	;
		member(E1,C1),
		(expandedDomainEquation([],_,[E,E1]);
		 expandedDomainEquation([],_,[E1,E])),
		append([E,E1],C,CO)
	),
	!.


expandedDomainEquation(Vs,C,[E1,E2]) :-
	nonvar(C),
	domainEquation(C,bool,T),
	newVar('b-bool',Vs,V),
	domainEquation(E1,bool,@V),
	E2=(T=(@V)),
	nonVar(T),
	!.
expandedDomainEquation(_,C,[]):-
	nonvar(C),
	!.
expandedDomainEquation(_,_,[E1,E2]) :-
	domainEquation(_,bool,T),
	domainEquation(E1,bool,@V),
	E2=(T=(@V)),
	nonVar(T),
	!.






selectCondition(J,(C,[Eq]),Mode,I) :-
	cont1(indProve,true),
	!,
	split(lambda([C1],[S,T]^(domainEquation(C1,S,T),parameterSort(S))),C,_,NPC),
	(NPC=[] ->
		nl,
		error("Failure of inductive proof.
The new identity
	%
has been derived from equation %.

",['$equation'(C,[Eq]),J],indProve),
		abortFromCompletion
	;
		selectCondition1(J,(NPC,[Eq]),Mode,NPI),
		NPI1 is NPI-1,		% igitt
		select(NPC,NPI1,SC),
		position(C,SC,I1),
		I is I1+1
	).
selectCondition(J,E,Mode,I) :-
	selectCondition1(J,E,Mode,I).

selectCondition1(_J,([_E],_),_Mode,1):-
	!.
selectCondition1(_J,(C,[Eq]),_Mode,I):-
	fetchAction(selectcondition(I),_,'$equation'(C,[Eq]),_,no,_),
	!.
%selectCondition1(J,E,Mode,I):-
%	chosenSuperCondition([I],J,E,Mode),
%	!.
selectCondition1(_J,(C,[Eq]),_Mode,I):-
	length(C,N),
	nl,
	(cont1(equationDisplayed,false) ->
		sPrint("Consider the equation
	%.",['$equation'(C,[Eq])])
	;	true),
	repeat,
	nl,
	sPrint("Which of the condition equations in
	% 
should be selected for superposition?
Please enter index from 1 to % (A. to abort). > ",['$conjunction'(C),N]),
	read(I),
	(	number(I),
		1 =< I,
		I =< N
	;	var(I),
		abortFromCompletion
	;	write('???'),
		fail
	),
	!.




isStatus(ms,_).
isStatus(S,N) :-
	N1 is N+1,
	listOfNumbers(N1,0,L),
	permutation(L,S).


between :-
	listing('$maySuperpose'/7),
	listing(mayBeReducible/3).


usedForSimplification((_,_,_),(usedForSimplification,no)).


relaxedRedIndex(T,I,T,I) :-
	!.
relaxedRedIndex(_,_,_,infty).


relaxEqRedIndexes(T,I) :-
	object(I,T,([_|_],[L=R])),
	redIndexes(I,T,(RIL,RIR)),
	complexityBound(I,T,((RCL,RCR),NC)),
	(RCL = ['$r'(L)] ->
		RIL1 = RIL
	;	RIL1 = infty
	),
	(RCR = ['$r'(R)] ->
		RIR1 = RIR
	;	RIR1 = infty
	),
	seta(T,I,redIndexes,(RIL1,RIR1)),
	prologVariant(I,T,(EP,((RCLP,RCRP),NCP),EnvP)),
	absolutelyNonreductive(I,T,N),
	(N = [] ->
		true
	;	(member(l,N) ->
			RCLL  = '$infty',
			RCLLP = '$infty'
		;	RCLL  = RCL,
			RCLLP = RCLP
		),
		(member(r,N) ->
			RCRR  = '$infty',
			RCRRP = '$infty'
		;	RCRR  = RCR,
			RCRRP = RCRP
		),
		seta(T,I,complexityBound,((RCLL,RCRR),NC)),
		seta(T,I,prologVariant,(EP,((RCLLP,RCRRP),NCP),EnvP))
	),
	!.
relaxEqRedIndexes(_,_).


adjustRuleRedIndexes(I) :-
	redIndexes(I,'$rule',(RIL,_)),
	adjustedRedIndex(I,RIL,J),
	seta('$rule',I,redIndexes,(J,infty)),
	!.

adjustedRedIndex(I,infty,J) :-
	J1 is 100000-I,
	max(J1,0,J), % auch wenn's nie soweit kommen wird.
	!.
adjustedRedIndex(_I,RIL,RIL).
	

eqType((L=_R),T) :-
	nonvar(L),
	!,
	termType(L,T).
eqType((_L=R),T) :-
	nonvar(R),
	termType(R,T).


termType(@V,S) :-
	opPrefix(V,_VN,S),
	!.
termType(Te,Ty) :-
	functor(Te,F,_),
	codomain(F,Ty).


varName(V,VN) :-
	opPrefix(V,VN,_S).

codomain(F,T) :-
	ofType(_,(F:[T|_])).


domain(F,T) :-
	ofType(_,(F:[_|T])).


equalTypes(_,'$anyType') :-
	!.
equalTypes('$anyType',_) :-
	!.
equalTypes('$someType',_) :-
	!,
	fail.
equalTypes(_,'$someType') :-
	!,
	fail.
equalTypes(T,T).

/*----------------------------------------------------------------------*/
/* narrowRule(+RuleDescription,-NarrowRuleDescription)                 */
/* given the description of a rule the elements of a narrowing rule     */
/* will be computed.                                                    */
/* The elements are:                                                    */
/* 1. The condition list is transformed into one term                   */
/* 2. The left and right hand side of the rule are passed unchanged     */
/* 3. The nonvariable occurrences of the condition and the rule will    */
/*    be computed                                                       */

narrowRule((_,_,(C,[L = R])),(narrowRule,[(PTC,[PL = PR]),MTC,MR])) :-
	condition_to_term(C,TC),
	toProlog((TC,[L = R]),[],(PTC,[PL = PR]),_),
	marked_term(PTC,MTC,vars(ff),nonvars(tt),nonvars), 
	marked_term(PR,MR,vars(ff),nonvars(tt),nonvars).


auxNarrowRule(('$rule',N,(C,[L = R])),(auxNarrowRule,Information)) :-
   (auxRule(('$rule',N,(C,[L = R])),(auxRule,PBed,[PL = PR])) ->
       (fromProlog(tup(PBed,PL,PR),[],tup(Bed1,L1,R1)),
        condition_to_term(Bed1,TC),
	toProlog((TC,[L1 = R1]),[],(PTC,[PL1 = PR1]),_),
        marked_term(PTC,MTC,vars(ff),nonvars(tt),nonvars), 
        marked_term(PR1,MR,vars(ff),nonvars(tt),nonvars),
        Information = [(PTC,[PL1 = PR1]),MTC,MR])
       ;
       (Information = none)
   ).

