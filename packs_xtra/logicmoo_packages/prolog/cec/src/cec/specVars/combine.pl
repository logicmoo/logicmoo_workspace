/*
 *	file:		combine.pl
 *	version:	1.5
 *	date:		November 8, 1989
 *	creation:	November 6, 1989
 *	author:		-
 *
 *	description:
 *	This file contains the predicates for combining
 *	of specifications
 *
 *	history:
 *	891106	uh	Added this comment
 *	900212	uh	Changed definitions of
 *			combine     maySuperposeS     adaptedIndexes
 *			See comments for reason
 *
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */


combineSpecs(user,user,S3) :-
	(S3=user ->
		true
	;	store(S3)).
combineSpecs(S1,S2,S3) :-
	world(S1),
	world(S2),
	S1=S2,
	(S3\==user ->
		pushState(undo)
	;	true),
	rs(S1),
	(S3\==user ->
		ss(S3),
		popState(undo)
	;	true),
	!.
combineSpecs(S1,S2,S3) :-
	combineWorlds(S1,S2,S3).

combineWorlds(W1,W2,W3) :-	% only called with W1\==W2
	checkCombine(W1,W2,W3),
	try(deleteWorld(W3)),
	try(beforeCombine(W1,W2,W3)),
	error:==none,
	combine(W1,W2,W3,Clause),
	stCl(W3,Clause),
	\+cont1(error,none),
	!,
	fail.
combineWorlds(W1,W2,W3) :-
	world(W1),
	world(W2),
	try(afterCombine(W1,W2,W3)),
	(W3=user ->
		integrityConstraint	
	;	assert(world(W3))
	).


/*----------------------------------------------------------------------*/

checkCombine(W1,W2,W3) :-
	error:==none,
	(var(W3)->
		error:==varW3,
		error("variable result parameter",[],combineSpecs(W1,W2,W3))

	;	true
	),
	(world(W1) ->
		true
	;
		error:==no(W1),
		error("there exists no specification of name %",[W1],combineSpecs(W1,W2,W3))
	),
	(world(W2) ->
		true
	;
		error:==no(W2),
		error("there exists no specification of name %",[W2],combineSpecs(W1,W2,W3))
	),
	cont1(error,none),
	!.


beforeCombine(W1,W2,_W3):-
	mkIndex(W1),
	mkIndex(W2),
	abolish(copyToBeDeleted,3),
	abolish(copyToBeDeleted,2),
	abolish(subsumptionClass,1),
	abolish(tps_renamings,2),
	assert(tps_renamings(W2,[])).

afterCombine(_W1,_W2,W3):-
	disambiguateRedIndexes(W3),	% before removal of duplicates !
	removeDuplicates(W3),
	normalizeReducibleInfo(W3),
%	abolish(duplicate,3),
	abolish(tps_renamings,2).

disambiguateRedIndexes(W):-
	minRedIndex(W,M),
	!,
	rules(W,Rs),
	mapP(lambda([I],
		[I1,II]^
		(	I1 is M-I+1,
			max(I1,0,II),
			seta(W,'$rule',I,redIndexes,(II,infty))
		)), Rs).
disambiguateRedIndexes(_).

rules(W,Rs):-
	setof1(I,O^(W:'$rule'(I,O)),Rs).

minRedIndex(W,M):-
	setof(I,[J,K]^(W:redIndexes(J,'$rule',(I,K))),Indexes),
	min(Indexes,M),
	!.


removeDuplicates(W):-
	copyToBeDeleted(T,I),
	dispose(W,T,I),
	fail.
removeDuplicates(W):-
	subsumed1(T,W,I,W,J),
	\+copyToBeDeleted(T,I),	% not yet deleted
	\+copyToBeDeleted(T,J), % not to be deleted
	dispose(W,T,I),
	fail.
removeDuplicates(_).


normalizeReducibleInfo(W3):-
	W3:mayBeReducible(I,T,contextually(_)),
	retractAll(W3:mayBeReducible(I,T,contextually(_))),
	(W3:mayBeReducible(I,T,contextually) ->
		true
	;	try(retract(W3:mayBeReducible(I,T,_))),
		assert(W3:mayBeReducible(I,T,contextually))),
	fail.
normalizeReducibleInfo(_).

% integrity constraint
integrity(integrityConstraint).

/*----------------------------------------------------------------------*
 *			integrityConstraint				*
 *----------------------------------------------------------------------*/

integrityConstraint :-
	assertClauseAttributes,
	(setof(notation(O,P,N),notation(O,P,N),NL) ->
	     mapA(declareNotation,NL)
	;
	     true),
	(cont(completeState,true) ->
		true
	;	(subsortT(S,S) ->
			nl,
			sPrint("*** Subsort relation not irreflexive for sort %.",[S]),
			error:=true
		;
			genInheritanceAxioms
		),
		checkConstructors),
	completeState:=true,
	!.

checkConstructors:-
	tps_current_ordering(kns_or_neqkns),
	repeat(retract_generated_kns_gt),	% 23.7.90 HG bug fix
	hhConstructor(H),
	(kns_status(H,S) ->
		error(
"Status declaration % for constructors such as % not allowed.",
			[S,H],checkConstructors)
	;	true),
	((kns_eq(H,_);kns_eq(_,H)) ->
		error(
"Precedence declaration for constructors such as % not allowed.",
			[H],checkConstructors)
	;	true),
	(kns_gt(H,_) ->
		error(
"Precedence declaration for constructors such as % not allowed.",
			[H],checkConstructors)
	;	true),
	('$rule'(I,R),rootLeft(I,H) ->
		error(
"Constructor % occurs as root of left side of rule %.",
			[H,'$rule'(I,R)],checkConstructors)
	;	true),
	kns_gt(O,H),
	(hhConstructor(O) ->
		true
	;	retract(kns_gt(O,H))),
	fail.
checkConstructors.


copiedAttribute(redIndexes,'$rule').



retract_generated_kns_gt:-
	kns_gt(H,I),
	hhConstructor(H),
	(	injection(I)
	;	domainPredicate(_,I)),
	retract(kns_gt(H,I)).



/*----------------------------------------------------------------------*
 *			 combine(W1,W2,W3,Fact3)			*
 *		    combine(W1,W2,W3,cont(W3,ctr(T),N3))		*
 *----------------------------------------------------------------------*/

combine(W1,W2,_W3,Fact1) :-
	relationTypes(Ts),
	member(T,Ts),
	functor(Fact1,T,2),
	W1:Fact1,
	arg(1,Fact1,I),
	(minimalInSubsumptionClass(T,W1,I,W2,_) ->
		true
	;	assert(copyToBeDeleted(T,I))).
combine(W1,W2,_W3,Fact3) :-
	relationTypes(Ts),
	member(T,Ts),
	modCtr(W1,T,N1),
	functor(Fact2,T,2),
	arg(1,Fact2,I),
	arg(2,Fact2,O),
	W2:Fact2,
	I3 is I+N1,
	(minimalInSubsumptionClass(T,W1,_,W2,I) ->
		true
	;	assert(copyToBeDeleted(T,I3))),		
	Fact3=..[T,I3,O].
combine(W1,W2,_W3,cont(ctr(T),N3)) :-
	relationTypes(Ts),
	member(T,Ts),
	modCtr(W1,T,N1),
	modCtr(W2,T,N2),
	N3 is N2+N1.
combine(W1,W2,_W3,cont(os,true)) :-
	(	W1:cont(os,true)
	;	W2:cont(os,true)
	).
combine(W1,W2,_W3,cont(vars,Env3)):-
	W1:cont(vars,Env1),
	W2:cont(vars,Env2),
	append(Env1,Env2,Env3).
combine(W1,_,_W3,Fact1) :-	% 5
	attribute(A,T),
%	copiedAttribute(A,T),
	functor(Fact1,A,3),
	arg(2,Fact1,T),
	W1:Fact1.
combine(W1,W2,_W3,Fact3) :-
	attribute(A,T),
%	copiedAttribute(A,T),
	A\==reduceClause,
	\+ (A=complexityBound,T='$nonoperational equation'),
	\+ (A=prologVariant,T='$nonoperational equation'),
	modCtr(W1,T,N1),
	functor(Fact2,A,3),
	arg(2,Fact2,T),
	arg(1,Fact2,I),
	arg(3,Fact2,V),
	W2:Fact2,
	I3 is I+N1,
	functor(Fact3,A,3),
	arg(2,Fact3,T),
	arg(1,Fact3,I3),
	arg(3,Fact3,V).
/* Note: deleteGarbage ausschalten!
combine(W1,_,_W3,(Fact1:- W1:Fact1)) :-	% 5
	attribute(A,T),
	\+copiedAttribute(A,T),
	functor(Fact1,A,3),
	arg(2,Fact1,T).
combine(W1,W2,_W3,(Fact3 :-
			var(I3),
			W2:Fact2,
			I3 is I+N1)):-
	attribute(A,T),
	\+copiedAttribute(A,T),
	\+ (A=reduceClause,T='$rule'),
	\+ (A=complexityBound,T='$nonoperational equation'),
	\+ (A=prologVariant,T='$nonoperational equation'),
	modCtr(W1,T,N1),
	functor(Fact2,A,3),
	arg(2,Fact2,T),
	arg(1,Fact2,I),
	arg(3,Fact2,V),
	functor(Fact3,A,3),
	arg(2,Fact3,T),
	arg(1,Fact3,I3),
	arg(3,Fact3,V).
combine(W1,W2,_W3,(Fact3 :-
			nonvar(I3),
			I is I3-N1,
			W2:Fact2)):-
	attribute(A,T),
	\+copiedAttribute(A,T),
	\+ (A=reduceClause,T='$rule'),
	\+ (A=complexityBound,T='$nonoperational equation'),
	\+ (A=prologVariant,T='$nonoperational equation'),
	modCtr(W1,T,N1),
	functor(Fact2,A,3),
	arg(2,Fact2,T),
	arg(1,Fact2,I),
	arg(3,Fact2,V),
	functor(Fact3,A,3),
	arg(2,Fact3,T),
	arg(1,Fact3,I3),
	arg(3,Fact3,V).
*/
combine(W1,W2,_W3,reduceClause(I3,T,(Cs3,_))) :-
	relationTypes(Ts),
	member(T,Ts),
	modCtr(W1,T,N1),
	W2:reduceClause(I,T,(Cs,_)),
	I3 is I+N1,
	transform((incrRuleIndex,[N1]),Cs,Cs3).
combine(W1,W2,_W3,complexityBound(I3,'$nonoperational equation',C3)) :-
	modCtr(W1,'$nonoperational equation',N1),
	W2:complexityBound(I2,'$nonoperational equation',C2),
	mkAtom('$neq%',[I2],NeqOp),
	( equalKnsStatus(W1,W2,NeqOp) ->
		C2=C3
	;	transform((incrNeqIndex,[N1,W2]),C2,C3)),
	I3 is I2+N1.
combine(W1,W2,_W3,prologVariant(I3,'$nonoperational equation',C3)) :-
	modCtr(W1,'$nonoperational equation',N1),
	W2:prologVariant(I2,'$nonoperational equation',C2),
	mkAtom('$neq%',[I2],NeqOp),
	( equalKnsStatus(W1,W2,NeqOp) ->
		C2=C3
	;	transform((incrNeqIndex,[N1,W2]),C2,C3)),
	I3 is I2+N1.
combine(W1,W2,_W3,'$maySuperpose'(Ch,I1,TI,Ty,J1,L,TJ)) :-     % 10
	(	Ty='$rule'
	;	Ty='$nonoperational equation'),
	(	N1=0,
		N2=0,
		W1:'$maySuperpose'(Ch,I,TI,Ty,J,L,TJ),
		maySuperposeS(Ch,W1,I,TI,W1,Ty,J,L,TJ)
	;	W2:'$maySuperpose'(Ch,_I1,TI,Ty,_J1,L,TJ),
		maySuperposeS(Ch,W2,I,TI,W2,Ty,J,L,TJ)
	;	N1=0,
		maySuperposeS(Ch,W1,I,TI,W2,Ty,J,L,TJ)
	;	N2=0,
		maySuperposeS(Ch,W2,I,TI,W1,Ty,J,L,TJ)),
	adaptedIndexes(W1,N1,N2,I,Ty,J,I1,J1).
combine(W1,_,_W3,clauseHasTerm(I,T,J)) :-
	W1:clauseHasTerm(I,T,J).
combine(W1,W2,_W3,clauseHasTerm(I,T,J)) :-
	W2:clauseHasTerm(I2,T,J),
	modCtr(W1,T,N1),
	I is I2+N1.
combine(_,_,W3,mayBeReducible(I,'$equation',contextually)) :-
	W3:'$equation'(I,_). % order relevant!
combine(W1,_,_W3,mayBeReducible(I,T,R)) :-
	W1:mayBeReducible(I,T,R).
combine(W1,W2,_W3,mayBeReducible(I,T,R)) :-
	W2:mayBeReducible(I2,T,R2),
	modCtr(W1,T,N1),
	I is I2+N1,
	(R2 =contextually(K2) ->
		modCtr(W1,'$rule',M1),
		J2 is M1+K2,
		R=contextually(J2)
	;	R=R2).
combine(W1,W2,_W3,mayBeReducible(I2,T,contextually(I))) :-
	reducibleByRule(W2,T,I1,W1,I),
	modCtr(W1,T,N),
	I2 is I1+N.
combine(W1,W2,_W3,mayBeReducible(I1,T,contextually(I2))) :-	% 15
	reducibleByRule(W1,T,I1,W2,I),
	modCtr(W1,'$rule',N),
	I2 is I+N.
combine(W1,W2,_W3,ofType(O,(O1:T))) :-	% 20
	(	W1:ofType(O,(O1:T)),
		\+ W2:ofType(O,(O1:T))
	;
		W2:ofType(O,(O1:T)),
		(W1:ofType(O,(O1:T)),
		 cInconsistent(W1,W2,O1) ->
			nl,
			write('*** operator '),
			print((O ofType (O1:T))),
			write(' is declared with different commutativity properties in the two specifications'),
			(error:=true)
		;
			true
		)
	).
combine(W1,_,_W3,notation(O,P,N)) :-
	W1:notation(O,P,N).
combine(_,W2,_W3,notation(O,P,N)) :-
	W2:notation(O,P,N).
combine(W1,W2,_W3,ac_ist_C(HH)) :-
	W1:ac_ist_C(HH);
	W2:ac_ist_C(HH).
combine(W1,W2,_W3,ac_ist_A(HH)) :-
	W1:ac_ist_A(HH);
	W2:ac_ist_A(HH).
combine(W1,W2,_W3,hhConstructor(HH)) :-
	W1:hhConstructor(HH);
	W2:hhConstructor(HH).
combine(W1,W2,_W3,domainAxiomHasBeenGenerated(Op)) :-
	W1:domainAxiomHasBeenGenerated(Op);
	W2:domainAxiomHasBeenGenerated(Op).
combine(W1,W2,_W3,tps_state(P)) :-
	W1:tps_state(P1),
	W2:tps_state(P22),
	tps_renamings(W2,Assoc),
	applyAssoc(P22,P2,Assoc),	% to rename the auxiliary neq-operators
					% in W2, cf. tpsStateRename 
	(not tps_state(_) ->	% hack fuer tps_combine!
		assert(tps_state(P1)),
		assign1(tps_state_created,true)
	;	assign1(tps_state_created,false)
	),
	(tps_combine(P1,P2,P) ->
		true
	;
		P=P1,
		error("The orderings of % and % cannot be combined",[W1,W2],combine)
	),
	(cont1(tps_state_created,true) ->
		try(retract(tps_state(_)))	% not backtrackable ! 
	;	true
	).
combine(W1,_W2,_W3,kns_gt(A,B)):-
	W1:kns_gt(A,B).
combine(W1,W2,_W3,kns_gt(A1,B1)):-
	W2:kns_gt(A,B),
	tps_renamings(W2,Assoc),
	applyAssoc((A,B),(A1,B1),Assoc),
	\+W1:kns_gt(A1,B1),
	(kns_isConsistent(W1,gt(A1,B1),[]) ->
		true
	;	error("The orderings of % and % cannot be combined.
Offending precedence in %: %",[W1,W2,W2,gt(A1,B1)],combine)
	).
combine(W1,_W2,_W3,kns_eq(A,B)):-
	W1:kns_eq(A,B).
combine(W1,W2,_W3,kns_eq(A1,B1)):-
	W2:kns_eq(A,B),
	tps_renamings(W2,Assoc),
	applyAssoc((A,B),(A1,B1),Assoc),
	\+W1:kns_eq(A1,B1),
	(kns_isConsistent(W1,eq(A1,B1),[]) ->
		true
	;	error("The orderings of % and % cannot be combined.
Offending precedence in %: %",[W1,W2,W2,eq(A1,B1)],combine)
	).
combine(W1,_W2,_W3,kns_status(A,B)):-
	W1:kns_status(A,B).
combine(W1,W2,_W3,kns_status(A1,B)):-
	W2:kns_status(A,B),
	tps_renamings(W2,Assoc),
	applyAssoc(A,A1,Assoc),
	\+W1:kns_status(A1,B),
	(kns_isConsistent(W1,status(A1,B),[]) ->
		true
	;	error("The orderings of % and % cannot be combined.
Offending status in %: %",[W1,W2,W2,status(A1,B)],combine)
	).
combine(W1,W2,_W3,(reduce(A,B):-T)) :-
	(	clause(W1:reduce(A,B),T)
	;	clause(W2:reduce(A,B),T)
	).
combine(W1,W2,_W3,(reduceI(A,I,B):-T)) :-
	(	clause(W1:reduceI(A,I,B),T)
	;	clause(W2:reduceI(A,I,B),T)
	).
combine(W1,_W2,_W3,specialLaw('$rule'(I,L))) :-
	W1:specialLaw('$rule'(I,L)).
combine(W1,W2,_W3,specialLaw('$rule'(I,L))) :-
	W2:specialLaw('$rule'(I2,L)),
	modCtr(W1,'$rule',N),
	I is N+I2.
combine(W1,W2,_W3,(pretty(A):-T)) :-
	(	clause(W1:pretty(A),T)
	;	clause(W2:pretty(A),T)).
combine(W1,W2,_W3,(parse(A,B):-T)) :-
	clause(W1:parse(A,B),T);
	clause(W2:parse(A,B),T).
combine(W1,W2,_W3,(predicate(A):-T)) :-
	clause(W1:predicate(A),T);
	clause(W2:predicate(A),T).
combine(W1,W2,_W3,(subsort(S,T))) :-
	W1:subsort(S,T);
	W2:subsort(S,T).
combine(W1,W2,_W3,(inheritanceAxiom(S,T))) :-
	W1:inheritanceAxiom(S,T);
	W2:inheritanceAxiom(S,T).
combine(W1,W2,_W3,(action(A,E))) :-
	clause(W1:action(A,E),_);
	clause(W2:action(A,E),_).
combine(W1,W2,_W3,(actionNew(A,E))) :-
	clause(W1:actionNew(A,E),_);
	clause(W2:actionNew(A,E),_).
combine(W1,W2,_W3,F) :-			% 900810 js
	combinedConstraint(W1,W2,F).


maySuperposeS(Ch,W1,I,TI,W2,Ty,J,L,TJ):-
	subsumptionClass([('$rule',W1,I)|_]),
	subsumptionClass([(Ty,W2,J)|_]),
	allCopiesSuperpose(Ch,W1,I,TI,W2,Ty,J,L,TJ).




mkIndex(W):-
	abolish(W:superposOp,5),
	W:superpositionTerms(I,Ty,ST),
	member((Lit,A),ST),
	opsInTermW(I,W,Ty,Lit,A,Ops),
	member(O,Ops),
	assert(W:superposOp(O,I,Ty,Lit,A)),
	fail.
mkIndex(_W).

maySuperposeWith(W1,I1,A1,W,'$nonoperational equation',I,Lit,A) :-
	W1:superpositionTerms(I1,'$rule',ST1),
	\+W1:lemma(I1,'$rule',indScheme(_)),
	member((conclusion,A1),ST1),
	rootOpW(W1,I1,A1,O1),
	W:superposOp(O1,I,'$nonoperational equation',Lit,A),
	\+W:lemma(I,'$nonoperational equation',indScheme(_)),
	newSuperpositions:==true.
maySuperposeWith(W1,I1,A1,W,'$rule',I,Lit,A) :-
	W1:superpositionTerms(I1,'$rule',ST1),
	\+W1:lemma(I1,'$rule',indScheme(_)),
	member((conclusion,A1),ST1),
	rootOpW(W1,I1,A1,O1),
	W:superposOp(O1,I,'$rule',Lit,A),
	\+W:lemma(I,'$rule',indScheme(_)),
	newSuperpositions:==true.





% adaptedIndexes(World1, BaseNumber1, BaseNumber2, 
%		 Index1, ObjectType, Index2, ActualIndex1, ActualIndex2)
% computes the correct values of the indices Index1 and Index2 for the
% combined world:
% - Index1 is the index of some object of type ObjectType
%   counted relatively to one of the worlds to combine
% - Index2 is the index of some rule 
%   counted relatively to one of the worlds to combine
% If BaseNumber1 or BaseNumber2 is instantiated, it has the value 0, and
% indicates that the object having Index1 or Index2 respectively, belongs
% to the first world World1. So this index mustn't be corrected.
% Otherwise we take the value of the appropriate counter out of the 
% World1 and add to the index.
% The result are the corrected values ActualIndex1 and ActualIndex2
% changed uh 12.02.90: The old version sometimes added the value of the
%                      counter of the second world to the index of an
%                      object also belonging to the second world.

adaptedIndexes(W1,N1,N2,I,Ty,J,I1,J1):-
	(N1==0 ->
		true
	;	modCtr(W1,'$rule',N1)),
	(N2==0 ->
		true
	;	modCtr(W1,Ty,N2)),
	I1 is I+N1,
	J1 is J+N2,
	!.


allCopiesSuperpose(Ch,WI,I,TI,WJ,Ty,J,L,TJ):-
	superposes(Ch,WI,I,TI,WJ,Ty,J,L,TJ),
	allMustSuperpose(Ch,WI,I,TI,WJ,Ty,J,L,TJ).

superposes((5,5),W1,I,TI,W2,Ty,J,L,TJ):-
	W1\==W2,
	maySuperposeWith(W1,I,TI,W2,Ty,J,L,TJ).
superposes(Ch,W,I,TI,W,Ty,J,L,TJ):-
	W:'$maySuperpose'(Ch,I,TI,Ty,J,L,TJ).


allMustSuperpose(Ch,WI,I,TI,WJ,Ty,J,L,TJ):-
	subsumptionClass([('$rule',WI,I)|ClI]),
	member(('$rule',WI1,I1),[('$rule',WI,I)|ClI]),
	subsumptionClass([(Ty,WJ,J)|ClJ]),
	member((Ty,WJ1,J1),[(Ty,WJ,J)|ClJ]),
	\+superposes(Ch,WI1,I1,TI,WJ1,Ty,J1,L,TJ),
	!,
	fail.
allMustSuperpose(_Ch,_WI,_I,_TI,_WJ,_Ty,_J,_L,_TJ).

minimalInSubsumptionClass(T,W1,I,W2,K):-
	var(K),
	!,
	\+copyToBeDeleted(T,W1,I),
	subsumptionClass(T,W1,I,W2,K).
minimalInSubsumptionClass(T,W1,_,W2,I):-
	\+copyToBeDeleted(T,W2,I),
	subsumptionClass(T,W1,_,W2,I).


subsumptionClass(T,W1,I,W2,K):-
	var(K),
	!,
	setof1((T,W,J),
		(	(W=W1;W=W2),
			subsumed1(T,W1,I,W,J),
			subsumed1(T,W,J,W1,I)),Class),
	assert(subsumptionClass([(T,W1,I)|Class])),
	mapP(lambda([(T_,W_,I_)],assert(copyToBeDeleted(T_,W_,I_))),Class),
	!.
subsumptionClass(T,_W1,_K,W2,I):-
	setof1((T,W2,J),
		(	subsumed1(T,W2,I,W2,J),
			subsumed1(T,W2,J,W2,I)),Class),
	assert(subsumptionClass([(T,W2,I)|Class])),
	mapP(lambda([(T_,W_,I_)],assert(copyToBeDeleted(T_,W_,I_))),Class),
	!.

reducibleByRule(W1,T,I1,W2,I):-
	W2:opsLeft(I,'$rule',(Ops,_)),
	\+W2:lemma(I,'$rule',indScheme(_)),
	\+copyToBeDeleted('$rule',W2,I),
	W1:clauseHasTerm(I1,T,Ops1),
	\+W1:lemma(I1,T,indScheme(_)),
	(T='$nonoperational equation' ->
		W1:kind(I1,T,K),
		functor(K,other,_)
	;	true),
	subset(Ops,Ops1),
	(W1:mayBeReducible(I1,T,contextually) ->
		true
	;	subsumptionClass((W2,'$rule',I),Class),
		\+member((W1,'$rule',_),Class)).


modCtr(W,T,N):-
	functor(Obj,T,2),
	arg(1,Obj,I),
	W:Obj,
	\+copyToBeDeleted(T,W,I),
	W:cont(ctr(T),N),
	!.
modCtr(_,_,0).

/*----------------------------------------------------------------------*
 *			incrRuleIndex(T,T1,N1)				*
 *----------------------------------------------------------------------*/

incrRuleIndex(X,_,_):-
	var(X),
	!,
	fail.
incrRuleIndex(myRuleIndex(I,D),myRuleIndex(I1,D),N1) :-
	I1 is I+N1.
incrRuleIndex(applying('$rule'(I,R)),applying('$rule'(I1,R)),N1) :-
	I1 is I+N1.

incrNeqIndex(X,_,_,_):-
	var(X),
	!,
	fail.
incrNeqIndex(C2,C3,N1,W2):-
	C2=..[O|As],
	(	auxOpForComplexities(O,OpOs,M2),
		number(M2)
	->	M3 is M2+N1,
		mkAtom('$neq%-ms',[M3],O3),
		mkAtom('$neq%',[M3],O3Os),
		C3=..[O3|As],
		tpsStateRename(W2,(O,O3)),
		tpsStateRename(W2,(OpOs,O3Os))).

equalKnsStatus(W1,W2,O) :-
	(	W1:kns_status(O,S1),
		W2:kns_status(O,S2) ->
		S1=S2
	;	true).


/*----------------------------------------------------------------------*
 *			tpsStateRename(W,Assoc)				*
 *----------------------------------------------------------------------*/

tpsStateRename(W,Assoc) :-
	(	retract(tps_renamings(W,Assocs))
	;	Assocs=[]
	),
	assertz(tps_renamings(W,[Assoc|Assocs])),
	!.

/*----------------------------------------------------------------------*
 *			cInconsistent(W1,W2,O)				*
 *----------------------------------------------------------------------*/

cInconsistent(W1,W2,O) :-
	ac_ist_C(W1,O),
	\+ ac_ist_C(W2,O).
cInconsistent(W1,W2,O) :-
	ac_ist_C(W2,O),
	\+ ac_ist_C(W1,O).
	

