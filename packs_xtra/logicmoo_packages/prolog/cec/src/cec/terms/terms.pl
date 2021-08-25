/*
 *	file:		terms.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates to handle terms.
 *
 *	history:
 *	891010	js	Added this comment
 *	891030  uh	Moved definitions of
 *			ac_matchForOp/3		ac_matchAnyOf/3
 *			ac_matchForOpvf/2	ac_matchAnyOfvf/3
 *			into compute/achelp.pl
 *	891116	uh	Moved definition of
 *			declareNotation/1
 *			into this file
 *	891116	uh	Moved definition of
 *			map/4			mapTo/5	
 *			mapA/3			mapTo/6
 *			into prolog/apply.pl
 *	891127	uh	Moved definitions of
 *			equalUpToVarRenaming/2	eqUpToVarRenaming/4
 *			into this file
 *	891208	uh	Changed definition of
 *			expandTerm
 *			to allow the use of disambiguated order-sorted
 *			operators O/N
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

:-dynamic transform/3.
:-dynamic transformOrFail/3.
:-dynamic transformL/5.

	
varsOf(X,V,V):-
	var(X),
	member_s(X,V),
	!.
varsOf(X,V,[X|V]):-
	var(X),
	!.
varsOf(X,_,[]) :-
	var(X),
	!,
	fail.
varsOf(T,U,V):-
	T=..[_|Ts],
	varsOfL(Ts,U,V).

varsOfL([],V,V).
varsOfL([T|Ts],V,W):-
	varsOf(T,V,U),
	varsOfL(Ts,U,W).

varsOf(T,Vs) :-
	varsOf(T,[],Vs),
	!.



transform(S,T,T1):-
	transformOrFail(S,T,T1),
	!.
transform(_,T,T).

transform(S,Args,T1,T2):-		% nonstandard (historical)
	transformOrFail((S,Args),T1,T2),
	!.
transform(_,_,T,T).

transformOrFail(Subst,T,T1) :-
	apply(Subst,[T,T1]),
	!.
transformOrFail(Subst,P,T) :-
	\+ var(P),
	functor(P,F,N),
	transformL(Subst,N,0,P,Ts),
	T=..[F|Ts].

transformL(_,N,N,_,_):-
	!,
	fail.
transformL(S,N,I,P,[A|As]) :-
	J is I+1,
	arg(J,P,PJ),
	(transformOrFail(S,PJ,A) -> 
		(transformL(S,N,J,P,As)->
			true
		;	subterms(J,N,P,As,[])
		)
	;	transformL(S,N,J,P,As),
		A=PJ
	).




/*----------------------------------------------------------------------*/

rewriteInfo(C,L,R,rewriteInfo(Balance,ConsRight,Kind)) :-
	varBalance(L,R,Balance),
	(constrTerm(R) ->
		ConsRight=consRight
	;
		ConsRight=noConsRight
	),
	ruleKind(C,L,R,Kind),
	!.

/*----------------------------------------------------------------------*/

varBalance(L,R,B) :-
	varsIn(L,VL),
	varsIn(R,VR),
	B is VL-VR.

/*----------------------------------------------------------------------*
 *			linearity(Term,Type)				*
 *									*
 * linearity(Term,linear) <=> Term is linear				*
 * linearity(Term,nonlinear) <=> Term is not linear			*
 *----------------------------------------------------------------------*/

linearity(T,linear) :-
	isLinear(T,[],_),
	!.
linearity(_,nonlinear).


isLinear(@V,Vars,[V|Vars]) :-
	!,
	not(member(V,Vars)).
isLinear(T,Vars,NewVars) :-
	functor(T,_,N),
	isLinear(T,N,Vars,NewVars).


isLinear(_,0,V,V):-!.
isLinear(T,N,V,NV) :-
	arg(N,T,X),
	isLinear(X,V,V1),
	!,
	M is N-1,
	isLinear(T,M,V1,NV).


varConstraints(T,T1,C):-
	varConstraints(T,[],T1,C,_).

varConstraints(X,Env,Y,[X=Y],Env):-
	var(X),
	member_s(X,Env),
	!.
varConstraints(X,Env,X,[],[X|Env]):-
	var(X),
	!.
varConstraints(T,Env,T1,C,Env1):-
	T=..[O|Ts],
	varConstraintsL(Ts,Env,Ts1,C,Env1),
	T1=..[O|Ts1].

varConstraintsL([],E,[],[],E).
varConstraintsL([T|Ts],E,[T1|Ts1],C,E2):-
	varConstraints(T,E,T1,CT,E1),
	varConstraintsL(Ts,E1,Ts1,CTs,E2),
	append(CT,CTs,C).


/*----------------------------------------------------------------------*
 *			   varsIn(Term,Number)				*
 *									*
 * varsIn(Term,Number) <=> Number is the number of Variables in Term	*
 *----------------------------------------------------------------------*/

varsIn(@_,1) :-
	!.
varsIn(T,Vars) :-
	T=..[F|Ts],
	F\== @,
	varsInList(Ts,Vars),
	!.


varsInList([],0).
varsInList([X|Xs],NV) :-
	varsIn(X,VX),
	varsInList(Xs,VXs),
	NV is VX+VXs,
	!.

/*----------------------------------------------------------------------*/



/*----------------------------------------------------------------------*/

ruleKind([],L,@X,idempotence(O)) :-
	functor(L,O,2),
	arg(1,L,@X),
	arg(2,L,@X),
	ac_ist_AC(O).
ruleKind([],L,One,complement(O,One)) :-
	atom(One),
	functor(L,O,2),
	arg(1,L,@X),
	arg(2,L,@X),
	ac_ist_AC(O).
ruleKind([],L,Zero,zero(O,Zero)) :-
	atom(Zero),
	functor(L,O,2),
	(	arg(1,L,@X),
		arg(2,L,Zero)
	;	arg(1,L,Zero),
		arg(2,L,@X)
	),
	ac_ist_AC(O).
ruleKind([],L,@X,unit(O,One)) :-
	functor(L,O,2),
	(	arg(1,L,@X),
		arg(2,L,One)
	;	arg(1,L,One),
		arg(2,L,@X)
	),
	atom(One),
	ac_ist_AC(O).
ruleKind(_,_,_,general).		



sortIndACF(X,_):-
	var(X),
	!,
	fail.
sortIndACF(@_,_):-
	!,
	fail.
sortIndACF(T,TS):-
	T=..[O,A,B],
	indAC(O),
	!,
	ac_del_assoc(O,[A,B],Ts),	
	mapF(sortIndAC,Ts,Ts1),
	sort(Ts1,TsS),
	!,
	TsS\==Ts,
	ac_rreduce(O,TsS,TS).
sortIndACF(T,TS):-
	T=..[O|Ts],
	mapF(sortIndAC,Ts,TsS),
	!,
	Ts\==TsS,
	TS=..[O|TsS].

sortIndAC(T,S):-
	cont1(indProve,true),
	sortIndACF(T,S),
	!.
sortIndAC(T,T).
	
/*----------------------------------------------------------------------*/

sizeOf(T,2) :-
	T= @(_),
	!.
sizeOf(T,S) :-
	T=..[O|Ts],
	map(sizeOf,Ts,Ss),
	rReduce(plus,0,Ss,N),
	(ac_ist_AC(O) ->
		S is N+2
	;	S is N+1),
	!.

depthAndSizeOf(T,(1,1)) :-
	T= @(_),
	!.
depthAndSizeOf(T,(D,S)) :-
	T=..[O|Ts],
	map(depthAndSizeOf,Ts,DSs),
	rReduce(lambda([(A,B),(C,D),(E,F)],(F is B+D,max(A,C,E))),(0,0),DSs,(DD,SS)),
	(ac_ist_AC(O) ->
		S is SS+2
	;	S is SS+1),
	D is DD+1,
	!.


sizeOfRule((L->R),S) :-
	sizeOf(L,SL),
	sizeOf(R,SR),
	S is SL+SR//2,
	!.


sizeOfEq((_->(L=R)),S) :-
	sizeOf(L,SL),
	sizeOf(R,SR),
	max(SL,SR,S),
	!.
sizeOfEq((L=R),S) :-
	sizeOf(L,SL),
	sizeOf(R,SR),
	max(SL,SR,S),
	!.

/*----------------------------------------------------------------------*
 *			termInfo(Term,(Operators,Size))			*
 *									*
 * termInfo(Term,(Operators,Size)) <=> 					*
 *	Operators is the sorted set of all operators in Term		*
 *	Size is the size of Term					*
 *----------------------------------------------------------------------*/

termInfo(T,(O,S1)) :-
	termI(T,(O1,S1)),
	sort(O1,O),
	!.


termI(@_,([],1)) :- !.
termI(T,(Ops,Size)) :-
	functor(T,F,N),
	(injection(F) ->
		M=0
	;	M=N
	),
	map(termI,T,N,Os),
	combineInfo(Os,(OpsTs,SizeTs)),
	Size is SizeTs+M,
	(member(F,OpsTs) ->
		Ops=OpsTs
	;
		Ops=[F|OpsTs]
	),
	!.


combineInfo([],([],0)).
combineInfo([(OpsT,SizeT)|Infos],(Ops,Size)) :-
	combineInfo(Infos,(OpsTs,SizeTs)),
	union(OpsTs,OpsT,Ops),
	Size is SizeT+SizeTs.

/*----------------------------------------------------------------------*/

shuffleIndex((_,'$rule'(R,info(O,Op,Lin,S,RP,I,M,L,Ov,Ref))),
	     (I,'$rule'(R,info(O,Op,Lin,S,RP,I,M,L,Ov,Ref)))).

/*----------------------------------------------------------------------*
 *			  constrTerm(Term)				*
 *									*
 * constrTerm(Term) <=> Term contains no Operators which are not	*
 *			constructors					*
 *----------------------------------------------------------------------*/

constrTerm(X) :-
	var(X),
	!.
constrTerm(@_) :- !.
constrTerm(T) :-
	functor(T,O,N),
	(hhConstructor(O) ; hhConstructor(O/N)),
	mapA(constrTerm,T,N),
	!.

/*----------------------------------------------------------------------*/

eq(_,T,T).
eq(_,L,R) :-
	eqL((L=R),_,[]).


eqL((L=R),Rs,C) :-
	normalForm1(Rs,C,L,LN),
	normalForm1(Rs,C,R,RN),
	impliesvf(C,(LN=RN)),
	!.

/*----------------------------------------------------------------------*/

implies(_,(L=R)) :-
	not(pragma(trans)),
	ac_uniAC(L,R).
implies(C,(L=R)) :-
	not(pragma(trans)),
	member((LL=RR),C),
	(	ac_uniAC(L,LL),
	  	ac_uniAC(R,RR)
	;
		ac_uniAC(L,RR),
	  	ac_uniAC(R,LL)
	).
implies(C,Eq) :-
	pragma(trans),
	symTransClosure(Eq,C,ac_uniAC).

/*----------------------------------------------------------------------*/

impliesvf(_,(L=R)) :-
	not(pragma(trans)),
	ac_matchvf(L,R).
impliesvf(C,(L=R)) :-
	not(pragma(trans)),
	member((LL=RR),C),
	(	ac_matchvf(L,LL),
		ac_matchvf(R,RR)
	;
		ac_matchvf(L,RR),
		ac_matchvf(R,LL)
	).
impliesvf(C,Eq) :-			% case of varfree equation Eq
	pragma(trans),
	symTransClosure(Eq,C,ac_matchvf).

/*----------------------------------------------------------------------*/

impliesInstanceOfAll(_,[]) :- !.
impliesInstanceOfAll(L1,L2) :-
	member(P,L2),
	implies(L1,P),
	delete(L2,P,L3),
	impliesInstanceOfAll(L1,L3).

/*----------------------------------------------------------------------*/


% contextual rewriting with oriented conditions



rewriteExpert((_,infty),O,S,T) :-
	(specialLaw('$rule'(_IZ,zero(O,Zero))) ->
		deleteZeros(Zero,S,S1)
	;
		S1=S
	),
	(specialLaw('$rule'(_IC,complement(O,One))) ->
		deleteCompl(One,S1,S2)
	;
		S1=S2
	),
	(specialLaw('$rule'(_IO,unit(O,One))) ->
		deleteOnes(One,S2,S3)
	;
		S2=S3
	),
	(specialLaw('$rule'(_II,idempotence(O))) ->
		deleteCopies(S3,T)
	;
		T=S3
	),
	!.
rewriteExpert((_,RI),O,S,T) :-
	(specialLaw('$rule'(IZ,zero(O,Zero))),
	redIndexes(IZ,'$rule',(IZI,_)),
	smallerRIndex(IZI,RI) ->
		deleteZeros(Zero,S,S1)
	;
		S1=S
	),
	(specialLaw('$rule'(IC,complement(O,One))),
	redIndexes(IC,'$rule',(ICI,_)),
	smallerRIndex(ICI,RI) ->
		deleteCompl(One,S1,S2)
	;
		S1=S2
	),
	(specialLaw('$rule'(IO,unit(O,One))),
	redIndexes(IO,'$rule',(IOI,_)),
	smallerRIndex(IOI,RI) ->
		deleteOnes(One,S2,S3)
	;
		S2=S3
	),
	(specialLaw('$rule'(II,idempotence(O))),
	redIndexes(II,'$rule',(III,_)),
	smallerRIndex(III,RI) ->
		deleteCopies(S3,T)
	;
		T=S3
	),
	!.


smallerRIndex(_,infty):-
	!.
smallerRIndex(I,J):-
	I<J.

/*----------------------------------------------------------------------*/

deleteCompl(One,[S1|Ss1],[One|Ts]) :-
	member(S2,Ss1),
	ac_matchvf(S1,S2),
	delete(Ss1,S2,Ss2),
	!,
	deleteCompl(One,Ss2,Ts).
deleteCompl(One,[S1|Ss1],[S1|Ts1]) :-
	deleteCompl(One,Ss1,Ts1).
deleteCompl(_,[],[]).

/*----------------------------------------------------------------------*/

deleteZeros(Zero,Ss,[Zero]) :-
	member(Zero,Ss),
	!.
deleteZeros(_,Ts,Ts).

/*----------------------------------------------------------------------*/

deleteOnes(One,Ss,Ts) :-
	deleteOnes1(One,Ss,Ts1),
	(Ts1=[] ->
		Ts=[One]
	;
		Ts1=Ts
	).


deleteOnes1(One,[One|Ss1],Ts) :-
	!,
	deleteOnes1(One,Ss1,Ts).
deleteOnes1(One,[S1|Ss1],[S1|Ts1]) :-
	deleteOnes1(One,Ss1,Ts1).
deleteOnes1(_,[],[]).

/*----------------------------------------------------------------------*/

deleteCopies([S1|Ss1],Ts) :-
	member(S2,Ss1),
	ac_matchvf(S1,S2),
	!,
	deleteCopies(Ss1,Ts).
deleteCopies([S1|Ss1],[S1|Ts1]) :-
	deleteCopies(Ss1,Ts1).
deleteCopies([],[]).

/*----------------------------------------------------------------------*/

matches(T,Sub) :-
	toProlog(Sub,[],SP,_),
	prologMatch(T,SP).



hasSubterm(T,S):-
	prologMatch(T,S),
	!.








/*----------------------------------------------------------------------*
 *			prologMatch(T,Sub)				*
 *									*
 * prologMatch(T,Sub) <=> Sub unifies with a subterm of a 		*
 *			  variable-free T				*
 *----------------------------------------------------------------------*/

prologMatch(T,T).
prologMatch(T,Sub) :-
	functor(T,_,N),
	subterm(_,N,T,S),
	prologMatch(S,Sub),
	!.


prologMatchA(T,T).
prologMatchA(T,Sub) :-
	functor(T,_,N),
	subterm(_,N,T,S),
	prologMatchA(S,Sub).


unifyTerms(T,T1):-		% bug fix -  hg 11.7.90
	var(T),
	!,
	ac_uniAC(T,T1).
unifyTerms((L=R),(L1=R1)):-
	!,
	(	ac_uniAC(L,L1),
		ac_uniAC(R,R1)
	;	ac_uniAC(L,R1),
		ac_uniAC(R,L1)
	).
unifyTerms(T,T1):-
	ac_uniAC(T,T1).


unifyEquations((L=R),(L1=R1),K):-
	(	K=other(symmetric)
	;	K=injectivityLaw
	),
	succeeded:==false,
	!,
	(	ac_uniAC(L,L1),
		ac_uniAC(R,R1),
		succeeded:==true
	;	cont1(succeeded,false),
		ac_uniAC(L,R1),
		ac_uniAC(R,L1)
	).
unifyEquations(E,E1,_):-
	unifyTerms(E,E1).



/*----------------------------------------------------------------------*/

varfree(X):-
	var(X),
	!,
	fail.
varfree(X):-
	functor(X,_,N),
	mapA(varfree,X,N),
	!.

/*----------------------------------------------------------------------*/

applySubst(S,T1,T2):-
	transform((association,[S]),T1,T),
	!,
	T2=T.

applySubsts([],T,T).
applySubsts([S|Ss],T1,T2):-
	applySubst(S,T1,T3),
	applySubsts(Ss,T3,T2).



/* subterms(J,K,T,STs,AtEnd) computes the list [TJ+1,...,TN|AtEnd]
   of subterms of T  between indexes J+1 and K, followed by list AtEnd,
   1=<J=<K=<arity(T).
*/
subterms(N,N,_,AtEnd,AtEnd):-!.	
subterms(J,N,P,[PK|Ps],AtEnd):-
	K is J+1,
	arg(K,P,PK),
	subterms(K,N,P,Ps,AtEnd),
	!.







/*----------------------------------------------------------------------*/
association(V,_,_):-
	var(V),
	!,
	fail.
association(@V2,A,[(V1,A)|_]) :-
	 V1==V2,
	 !.
association(@V,A,[_|S]) :-
	association(@V,A,S).


/*----------------------------------------------------------------------*/

notOccursIn(@V,T) :-
	@V\==T,
	applySubst([(V,T)],T,T1),
	!,
	T==T1.

/*----------------------------------------------------------------------*/

vars(T,[]) :-
	var(T),
	!.
vars(@V,[V]) :-
	!.
vars(T,[]) :-
	atomic(T),
	!.
vars(T,V) :-
	functor(T,_,N),
	N>0,
	map(vars,T,N,VTs),
	rReduce(union,[],VTs,V),
	!.

/*----------------------------------------------------------------------*/

toProlog(X,S,X,S):-
	var(X),
	!.
toProlog(@V,Subst,PT,NSubst) :-
	createPVar(V,Subst,PT,NSubst),
	!.
toProlog(OT,Subst,PT,NewSubst) :-
	functor(OT,F,N),
	oTTL(OT,N,Subst,[],PTs,NewSubst),
	PT=..[F|PTs],
	!.


oTTL(_,0,S,R,R,S) :- !.
oTTL(OT,J,S,PTsFromJP1,PTs,NewS) :-
	arg(J,OT,OTJ),
	toProlog(OTJ,S,PTJ,S1),
	JM1 is J-1,
	oTTL(OT,JM1,S1,[PTJ|PTsFromJP1],PTs,NewS),
	!.

/*----------------------------------------------------------------------*/

toPrologX(@V,Subst,PT,NSubst) :-
	% replaces only variables of form @X... by Prolog-variables
	atom(V),
	name(V,[X|_]),
	name('X',[X]),
	!,
	createPVar(V,Subst,PT,NSubst),
	!.
toPrologX(@V,Subst,@V,Subst) :-
	!.
toPrologX(OT,Subst,PT,NewSubst) :-
	OT=..[F|OTs],
	oTTLX(OTs,Subst,PTs,NewSubst),
	PT=..[F|PTs],
	!.


oTTLX([],S,[],S) :- !.
oTTLX([OT|OTs],S,[PT|PTs],NewS) :-
	toPrologX(OT,S,PT,S1),
	oTTLX(OTs,S1,PTs,NewS),
	!.

/*----------------------------------------------------------------------*/

/*
createPVar(V,[(V,PV)|S],PV1,[(V,PV)|S]) :-
	PV1==PV,
	!.
This clause is subsumed by the next one
*/

createPVar(V,[(V,PV)|S],PV,[(V,PV)|S]) :- !.
createPVar(V,[E|S],PV,[E|S1]) :-
	createPVar(V,S,PV,S1),
	!.
createPVar(V,[],PV,[(V,PV)]) :- !.

/*----------------------------------------------------------------------*/

fromProlog(PT,Subst,PT) :-	  % possibly dead code
	(varCtr:=0),
	fromProlog1(PT,'$someType',Subst),
	!.

fromProlog(Subst,PT) :-	% instantiates variables in PT according to Subst
	(varCtr:=0),
	fromProlog1(PT,'$someType',Subst),
	!.




fromProlog1(PT,Type,Subst) :-
	var(PT),
	!,
	lookupPVar(_VName,Subst,Type,PT),
	!.
fromProlog1((L=R),_T,Subst) :-
	(	nonvar(L),
		functor(L,F,_),
		F\== (@)
	;	nonvar(R),
		functor(R,F,_),
		F\== (@)),
	!,
	(ofType(_,(F:[Type|_])) ->
		fromProlog1(L,Type,Subst),
		fromProlog1(R,Type,Subst)
	;	fromProlog1(L,'$someType',Subst),
		fromProlog1(L,'$someType',Subst)),
	!.
fromProlog1(PT,_T,Subst) :-
	functor(PT,F,N),
	(	ofType(_,(F:Types))
	;	N1 is N+1,
		genList(N1,'$someType',Types)
	),
	pTTL(PT,N,Types,Subst),
	!.


pTTL(_,0,_,_) :- !.
pTTL(PT,J,Ts,S) :-
	arg(J,PT,PTJ),
	select(Ts,J,T),
	fromProlog1(PTJ,T,S),
	JM1 is J-1,
	pTTL(PT,JM1,Ts,S),
	!.


lookupPVar(Name,[(Name,X)|_],_,V) :-
	X==V,
	V= @(Name),     % this replaces at the same time all other
			% Prolog-Variables that are unified with V by @(Name).
			% -> use only, when the Subst-Par can be destroyed
	!.
lookupPVar(Name,[_|S],T,V) :-
	lookupPVar(Name,S,T,V).
lookupPVar(XN,[],Type,V) :-
	inc(varCtr),
	cont(varCtr,N),
	mkAtom('X%-%',[N,Type],XN),
	V = @XN.


normalizeSubst([],[]).
normalizeSubst([(Name,PrologVar)|S],S1):-
	nonUserVarName(Name),
	var(PrologVar),
	!,
	normalizeSubst(S,S1).
normalizeSubst([A|S],[A|S1]):-
	normalizeSubst(S,S1).



nonUserVarName(Name):-
	XC is "X",
	name(Name,[XC|_]),
	!.


/*-----------------------
	fromProlog(PT,Subst,OT,NSubst)
	creates OT = Subst(PT), without instantiating the variables
	in PT. If PT contains more variables than bound by Subst, new variables
	names @Xi are invented and their binding to the corresponding
	Prolog-variable in PT is added to NSubst
                        -----------------------------------------------*/

fromProlog(PT,Subst,OT,NSubst) :-	
	(varCtr:=0),
	fromProlog1(PT,Subst,'$someType',OT,NSubst),
	!.


fromProlog1(PT,Subst,T,@VName,NSubst) :-
	var(PT),
	!,
	lookupPVar(PT,Subst,VName,T,NSubst),
	!.
fromProlog1(OT,Subst,_,PT,NewSubst) :-
	functor(OT,F,N),
	(	ofType(_,(F:Types))
	;	N1 is N+1,
		genList(N1,'$someType',Types)
	),
	pTTL(OT,N,Types,Subst,[],PTs,NewSubst),
	PT=..[F|PTs],
	!.


pTTL(_,0,_,S,R,R,S) :- !.
pTTL(OT,J,Ts,S,PTsFromJP1,PTs,NewS) :-
	arg(J,OT,OTJ),
	select(Ts,J,T),
	fromProlog1(OTJ,S,T,PTJ,S1),
	JM1 is J-1,
	pTTL(OT,JM1,Ts,S1,[PTJ|PTsFromJP1],PTs,NewS),
	!.


lookupPVar(V,S,Name,_,S) :-
	member((Name,X),S),
	X==V,
	!.
lookupPVar(V,S,XN,Type,[(XN,V)|S]) :-
	inc(varCtr),
	cont(varCtr,N),
	mkAtom('X%-%',[N,Type],XN).


/*----------------------------------------------------------------------*/

renameVars(S,S1,SR) :-
	renameVars(S,S,S1,SR),
	!.

renameVars([],_,_,[]).
renameVars([(N,_V)|Vs],S1,S2,VsR) :-
	nonUserVarName(N),	% eliminates variables of form X<i>
	!,
	renameVars(Vs,S1,S2,VsR).
renameVars([(N,V)|Vs],S1,S2,[(NR,V)|VsR]) :-
	awayFrom(N,S1,S2,NR),
	renameVars(Vs,S1,[(NR,V)|S2],VsR),
	!.

/*----------------------------------------------------------------------*/

awayFrom(N,_,S2,N) :-
	\+member((N,_),S2),
	!.
awayFrom(N,S1,S2,M) :-
	opPrefix(N,NP,NS),
	ren(NP,MP),
	mkAtom('%-%',[MP,NS],M),
	\+member((M,_),S1),
	\+member((M,_),S2),
	!.

/*----------------------------------------------------------------------*/


nonVar(@_) :-
	!,
	fail.
nonVar(_).



/* renVar(+Var,-New)
 * creates new variable New by renaming Var.
 */

renVar(V,V).
renVar(V,New) :-
	opPrefix(V,VP,VS),
	ren(VP,NewP),
	mkAtom('%-%',[NewP,VS],New).


/* newVar(+Var,+Other,-New)
 * creates new variable New by renaming Var away from Other.
 */

newVar(V,Other,New) :-
	renVar(V,New),
	opPrefix(New,NewP,_),
	\+X^(member(X,Other),opPrefix(X,NewP,_)),
	!.

/*----------------------------------------------------------------------*/

ren(N,M) :-
	atom_chars(N,SN),
	assign0(nameIndex,0),
	chgSuffix(SN,SM),
	atom_chars(M,SM).

/*----------------------------------------------------------------------*/

chgSuffix(SN,SM) :-
	append(SM,[X],SN),
	digitCode(X).
chgSuffix(SN,SM) :-
	(append(Prefix,[X],SN),digitCode(X) ->true; Prefix=SN),
	incr(nameIndex),
	cont(nameIndex,I),
	number_chars(I,SI),
	append(Prefix,SI,SM).

/*----------------------------------------------------------------------*/

digitCode(X) :-
	number_chars(N,[X]),
	number(N),
	0=<N,
	N=<9,
	!.

/*----------------------------------------------------------------------*/

renamePVars(T,TR) :-  % renames Prolog-variables in T
	copy_term(T,TR),
	!.



/*----------------------------------------------------------------------*/

recordCP(E) :-
	assert(cpComputed(E)).

/*----------------------------------------------------------------------*/

recordOverlap(I) :-
	cont(overlaps,Ov),
	isInsert(Ov,I,Ov1),
	(overlaps:=Ov1),
	!.

/*----------------------------------------------------------------------*/

singleVar(LC,[V|OArgs],V) :-
	var(V),
	not ac_occurs((LC,OArgs),V).
singleVar(LC,[V|OArgs],X) :-
	singleVar([V|LC],OArgs,X).


/*----------------------------------------------------------------------*/


newCP((C->(A=B))) :-
	inc(nmbovs),
	(	critPair((C->(A=B)))
	;
		critPair((C->(B=A)))
	),
	!,
	fail.
newCP((C->(A=B))) :-
	toProlog((C->(A=B)),[],(CP->(AP=BP)),_),
	assert(critPair((CP->(AP=BP)))),
	!.

/*---------------------------------------------------------------------
	overlap(U,(L,R),(L1,R1),(LC,RC))
	superposes L->R on nonvariable occurences u in L1,
	producing  LC=L1[u<-R] sigma  and  RC=R1 sigma
	U is the unification procedure to be used
  ---------------------------------------------------------------------*/

overlap(U,(L,R),(L1,RC),(LC,RC)) :-	
	nonvar(L1),
	applyRuleAndUnify(U,(L,R),L1,LC).


overlapBelowHead(U,(L,R),(L1,RC),(LC,RC)) :-
	nonvar(L1),
	applyRuleAndUnifyBelowHead(U,(L,R),L1,LC).



/*----------------------------------------------------------------------*/

headOverlap('=',(L,R),(L1,R1),(R,R1)) :-
	nonvar(L1),
	L=L1.
headOverlap(unify,(L,R),(L1,R1),(R,R1)) :-
	nonvar(L1),
	unify(L,L1).
headOverlap(ac_uniAC,(L,R),(L1,R1),(R,R1)) :-
	nonvar(L1),
	ac_uniAC(L,L1).


/*----------------------------------------------------------------------*/

applyRuleAndUnify('=',(L,R),L,R).
applyRuleAndUnify(unify,(L,R),T,R) :-
	unify(T,L).
applyRuleAndUnify(ac_uniAC,(L,R),T,R) :-
	ac_uniAC(T,L).
applyRuleAndUnify(U,(L,R),T,TR) :-
	applyRuleAndUnifyBelowHead(U,(L,R),T,TR).


applyRuleAndUnifyBelowHead(U,(L,R),T,TR) :-
	functor(T,O,N),
	\+ac_ist_AC(O),
	subterm(I,N,T,S),
	nonvar(S),
	applyRuleAndUnify(U,(L,R),S,ST),
	IM1 is I-1,
	subterms(I,N,T,T_IP1ToN,[]),
	subterms(0,IM1,T,LT,[ST|T_IP1ToN]),
	TR=..[O|LT].
applyRuleAndUnifyBelowHead(U,(L,R),T,TR) :-
	T=..[O|Args],
	ac_ist_AC(O),
	ac_del_assoc(O,Args,Sons),
	append(LL,[S|LR],Sons),
	nonvar(S),
	applyRuleAndUnify(U,(L,R),S,ST),
	append(LL,[ST|LR],LT),
	ac_rreduce(O,LT,TR).




subtermAndContext(T,(X,X,T)).
subtermAndContext(T,(C,Hole,Subterm)):-
	nonvar(T),
	T=..[O|Ts],
	append(L,[T1|R],Ts),
	subtermAndContext(T1,(Context,Hole,Subterm)),
	append(L,[Context|R],Ts1),
	C=..[O|Ts1].

	
	



subterm(_,0,_,_):-
	!,
	fail.
subterm(N,N,T,S):-
	arg(N,T,S).
subterm(I,N,T,S):-
	K is N-1,
	subterm(I,K,T,S).

/*----------------------------------------------------------------------*/

expandTerm(V,V) :- var(V), !.
expandTerm((H:-B),(H:-B)).
expandTerm((:-T),(:-T)).
expandTerm(T,T):-
	functor(T,F,N),
	interFaceOrPragmaOp(F/N),
	!.
expandTerm(T,T1) :-
	parse(T,T1).
expandTerm(cons(Op,P,Not),cons(Op,P,Not)) :-
	declareNotation(notation(Op,P,Not)).
expandTerm(op(Op,P,Not),op(Op,P,Not)) :-
	declareNotation(notation(Op,P,Not)).
expandTerm(I,T) :-
	nonvar(I),
	I=..[O|Ts],
	!,
	map(expandTerm,Ts,ETs),
	T=..[O|ETs].
expandTerm(X,X).

declareNotation(notation(Op,Priority,Notation)) :-
	current_op(Priority,Notation,Op),
	!.
declareNotation(notation(Op,_Priority,_Notation)) :-
	doNotChangeNotation(Ops),
	member(Op,Ops),
	!,
	error("Do not change the notation of the operator '%'.",[Op],
	      declareNotation).

declareNotation(notation(Op,Priority,Notation)) :-
	O =.. [(op),Priority,Notation,Op],
	O.


interFaceOrPragmaOp((module)/1).
interFaceOrPragmaOp(using/2).
interFaceOrPragmaOp((order)/1).
interFaceOrPragmaOp(setInterpretation/1).
interFaceOrPragmaOp(equal/1).
interFaceOrPragmaOp(status/1).
interFaceOrPragmaOp(greater/1).
interFaceOrPragmaOp(constructor/1).
interFaceOrPragmaOp(action/2).
interFaceOrPragmaOp(forwardChainingDepth/1).
interFaceOrPragmaOp(resolutionDepth/1).
interFaceOrPragmaOp(allowedEliminationTime/1).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Treatment of order-sorted signatures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






/*----------------------------------------------------------------------*/


minimalOp(O,T,_O1,_T1):-
	\+ operatorAbove(O,T),
	error("no declaration for '% : %'.",[O,'$op_signature'(T)],typeCheck),
	fail.
minimalOp(O,T,O1,T1):-
	(O ofType (O1:T1)), 
	map(subsortRT,T,T1),
	not opBetween(O,T,T1),
	!.

operatorAbove(O,T):-
	(O ofType (_:T1)),
	map(subsortRT,T,T1).

opBetween(O,[RLB|LB],[RT|T]) :-
	(O ofType (_:[RT1|T1])),
	[RT|T]\==[RT1|T1],
	[RLB|LB]\==[RT1|T1],
	map(subsortRT,LB,T1),
	map(subsortRT,[RT1|T1],[RT|T]),
	!.

/*----------------------------------------------------------------------*/

derivedName(O,O) :-
	!.
derivedName(O,O1) :-
	name(O1,O1N),
	name(O,ON),
	name('-',[M]),
	not member(M,ON),
	append(ON,[M|_],O1N).

/*----------------------------------------------------------------------*/

injection((T1,T),(T1,T)).
injection((T1,T1Term),(T2,Inject)) :-
	subsort(T1,T2),
	injectionName(T1,T2,I12),
	Inject=..[I12,T1Term].
injection((T1,T1Term),(T3,Inject13)) :-
	subsort(T1,T2),
	injectionName(T1,T2,I12),
	Inject12=..[I12,T1Term],
	injection((T2,Inject12),(T3,Inject13)).


injectionName(T1,T2,I12) :-
	appendTypes('$inj',[T2,T1],I12),
	!.

/*----------------------------------------------------------------------*/

subsortT(S1,S2) :-
	subsort(S1,S2).
subsortT(S1,S3) :-
	subsort(S1,S2),
	subsortT(S2,S3).

subsortRT(S,S).
subsortRT(S1,S2) :-
	subsortT(S1,S2).

subsortRTS(S1,S2) :-
	subsortRT(S1,S2).
subsortRTS(S1,S2) :-
	subsortT(S2,S1).


glb((S1,S2),S):-
	subsortRT(S,S1),
	subsortRT(S,S2),
	not (	subsort(S,SS),
		subsortRT(SS,S1),
		subsortRT(SS,S2)).
	
glb(S1,S2,S):-
	glb((S1,S2),S).

lub(S1,S2,S):-
	subsortRT(S1,S),
	subsortRT(S2,S),
	not (	subsort(SS,S),
		subsortRT(S1,SS),
		subsortRT(S2,SS)).

lub((S1,S2),S):-
	lub(S1,S2,S).


maxInComponent(S,SM):-
	subsortRT(S,SM),
	not subsort(SM,_),
	!.

/*----------------------------------------------------------------------*/


injection(I) :-
	('$inj' ofType (I:_)).

domainPred(P):-
	('$ground' ofType (P:_)).


smallerInjection(I,J):-
	('$inj' ofType (I:[ToI,From])),
	('$inj' ofType (J:[ToJ,From])),
	subsortT(ToI,ToJ),
	!.



/*----------------------------------------------------------------------*/

uniqueOpName('$inj',T,I) :-
	!,
	appendTypes('$inj',T,I).
uniqueOpName('$ground',T,I) :-
	!,
	appendTypes('$ground',T,I).
uniqueOpName(O,T,O1) :-
	admissibleOpName(O,T),
	appendTypes(O,T,O1).

appendTypes(O,T,O1) :-
	map(name,T,TN),
	name('-',[M]),
	mapF(lambda([N,MN],MN=[M|N]),TN,TMN),
	flatten(TMN,Suffix),
	name(O,ON),
	append(ON,Suffix,O1N),
	name(O1,O1N),
	!.


admissibleOpName(O,_T):-
	atom(O),
	atom_chars(O,[_|N]),
	name('-',[M]),
	member(M,N),
	error("Operator % must not contain '-' in the tail of its name.",[O],admissibleOpName).
admissibleOpName(O,T):-
	length(T,M),
	M=1,
	atom(O),
	name(O,N),
	name(O1,N),
	number(O),
	error("Number % in atom form % not allowed as name of constant.",[O1,O],admissibleOpName).
admissibleOpName(O,T):-
	number(O),
	length(T,M),
	M>1,
	error("Number % not allowed as name of operator with arity > 0.",[O],admissibleOpName).
admissibleOpName(O,T):-
	cec_language(cp),
	length(T,M),
	M>1,
	name(O,N),
	name(O1,N),
	number(O1),
	error("For C-Prolog version, number atom % not allowed as operator name.",[O],admissibleOpName).
admissibleOpName(_O,_T).



admissibleVarName(O):-
	\+ atom(O),
	error("Variables must be atoms: %.",[O],admissibleVarName).
admissibleVarName(O):-
	\+ variable(O),
	error("Variables must be different from function symbols: %.",[O],admissibleVarName).
admissibleVarName(O):-
	atom_chars(O,N),
	name('-',[M]),
	member(M,N),
	error("Variable name must not contain '-': %.",[O],admissibleVarName).
admissibleVarName(O):-
	atom(O),
	name(O,N),
	name(O1,N),
	number(O),
	error("Number % in atom form % not allowed as name of variable.",[O1,O],admissibleVarName).
admissibleVarName(_O).



typedVars(T,TT):-
	transform(typedVarName,T,TT),
	!.

typedVarName('$var'(X,T),@XT):-
	nonvar(T),
	!,
	appendTypes(X,[T],XT).
typedVarName('$var'(X,_T),@XT):-
	mkAtom('%-%',[X,'$anyType'],XT).


/*----------------------------------------------------------------------*/
/* osFunctor(+OS_Term,-OS_Functor,-Functor)				*/
/* Functor is the functor of OS_Term. If there are other operators	*/
/* named Functor, but having not the same arity, then OS_Functor is	*/
/* Functor/N, where N is the arity of Functor. Therewise OS_Functor	*/
/* is identical to Functor.						*/

osFunctor(T,O,F) :-
	functor(T,F,N),
	osOpName(F,N,O).


/*-----------------------------------------------------------------------*/
/* differentArities(+Functor)						 */
/* checks if there are declarations for Functor having different arities */

differentArities(F) :-
	(F ofType (_:Arity1)),
	(F ofType (_:Arity2)),
	length(Arity1,N1),
	length(Arity2,N2),
	N1\==N2,
	!.


/*----------------------------------------------------------------------*/
/* osDecompose(+OS_Term,-OS_Functor,-Functor,-Args)			*/
/* OS_Functor and Functor are determined using osFunctor/3. Args are	*/
/* simply the immediate subterms of OS_Term.				*/

osDecompose(T,O,F,Args) :-
	osFunctor(T,O,F),
	T=..[F|Args],
	!.

osOpName(@,1,@) :-
	!.
%osOpName(F,_,F) :-
%	appendAtoms('$',_,F),
%	!.
osOpName(F,N,Op) :-
	opPrefix(F,F1,_S1),
	(F == F1 ->
		(number(N) ->
			Op = F/N
		;
%			(var(N) ->
%				Op = F/N
%			;
				length(N,Arity),
				Op = F/Arity
%			)
		)
	;
		Op = F
	).



	
expandOp(F/N,F/N) :-
	atomic(F), 
	(number(N) ; var(N)).
expandOp(@,@) :- 
	!.
%expandOp(F,F) :-
%	appendAtoms('$',_,F),
%	!.
expandOp(F,Op) :-
	opPrefix(F,F1,_S1),
	(F == F1 ->
		(differentArities(F) ->
			Op = F/_
		;
			(F ofType (_:Type)),
			makeOp(F,Type,Op)
		)
	;
		Op = F
	),
	!.
expandOp(T1,T1/_) :-	
	!.


makeOp(F,[_Codomain|ArgTypes],F/N) :-
	atomic(F),
	length(ArgTypes,N),
	!.

/*----------------------------------------------------------------------*/

overloaded(O) :-
	O\=='$inj',
	(O ofType (O1:_)),
	(O ofType (O2:_)),
	O1\==O2.

/*----------------------------------------------------------------------*/

opPrefix(I,BI,Suffix) :-
	name(I,IN),
	name('-',[M]),
	% The operator name must be decomposed into the name part, i.e.
	% the sequence of characters reaching to the first '-' symbol
	% or the '-' symbol itself (now taken as operator name), and
	% the suffix part.
	append(BIN,[M|SuffixN],IN),
	(	BIN=[M]
	;
		\+member(M,BIN)
	),
	BIN\==[],
	!,
	name(BI,BIN),
	name(Suffix,SuffixN),
	!.
opPrefix(I,I,'').


/*----------------------------------------------------------------------*
 *     	   		 manyToOrderSorted(T,T1)			*
 *----------------------------------------------------------------------*/

manyToOrderSorted(T,T) :-
	var(T),
	!.
manyToOrderSorted(@VT,@VT) :-
%	varName(VT,V),	% HG
	!.
% manyToOrderSorted(DBR,DBR) :-
% 	db_reference(DBR),
%	!.
manyToOrderSorted(T,T1) :-
	T=..[O,T11],
	('$inj' ofType (O:_)),
	!,
	manyToOrderSorted(T11,T1),
	!.
manyToOrderSorted(T,T1) :-
	T=..[O|Ts],
	(	(N ofType (O:_))
	;	auxOpForComplexities(O,N,_)
	),
	!,
	map(manyToOrderSorted,Ts,Ts1),
	T1=..[N|Ts1],			    
	!.
manyToOrderSorted(T,T1) :-
	T=..[O|Ts],
	map(manyToOrderSorted,Ts,Ts1),
	T1=..[O|Ts1],			   
	!.

manyToOrderSortedPR(T,T) :-
	var(T),
	!.
manyToOrderSortedPR(@VT,VOS) :-
%	varName(VT,V1),
	termType(@VT,S1),
	maxInComponent(S1,S2),
%	injection((S1,@V1),(S2,V2)),
	injection((S1,@VT),(S2,V2)),
	VOS =.. [S2,V2],
	!.
% manyToOrderSortedPR(DBR,DBR) :-
% 	db_reference(DBR),
%	!.
manyToOrderSortedPR(T,T1) :-
	T=..[O,T11],
	('$inj' ofType (O:_)),
	!,
	manyToOrderSortedPR(T11,T1),
	!.
manyToOrderSortedPR(T,T1) :-
	T=..[O|Ts],
	(	(N ofType (O:_))
	;	auxOpForComplexities(O,N,_)
	),
	!,
	map(manyToOrderSortedPR,Ts,Ts1),
	T1=..[N|Ts1],		
	!.
manyToOrderSortedPR(T,T1) :-
	T=..[O|Ts],
	map(manyToOrderSortedPR,Ts,Ts1),
	T1=..[O|Ts1],			  
	!.

/* -----------------------------------------------------------------------
   toOrderSorted
   translates ``half-way'' to order-sorted terms, i.e. keeps injections in
   overloaded form
   -----------------------------------------------------------------------  */

toOrderSortedHalf1(T,T) :-
	var(T),
	!.
%toOrderSortedHalf1(@V,N) :-				% HG 28.5.90
%	(cont(os,true) ; cont1(showVarSorts,on)),
%	termType(@V,''),
%	!,
%	varName(V,N),
%	!.
%toOrderSortedHalf1(@V,(N:T)) :-
%	(cont(os,true) ; cont1(showVarSorts,on)),
%	termType(@V,T),
%	varName(V,N),
%	!.
toOrderSortedHalf1(@V,N) :-
	varName(V,N),
	!.
toOrderSortedHalf1('$ground'(T),T):-
	!.
toOrderSortedHalf1(T,T1) :-
	T=..[O|Ts],
	(O1 ofType (O:[TN|TA])),
	!,
	map(toOrderSortedHalf1,Ts,Ts1),
	(O1='$inj',cont1(showMS,on)    ->
		T1=..[TN|Ts1];
	 O1='$inj' ->
		[T1]=Ts1;
	 O1='$ground' ->
		TA=[S],
		mkAtom('is-%',[S],N),
		T1=..[N|Ts1];
	T1=..[O1|Ts1]
	),
	!.
toOrderSortedHalf1(T,T1) :-
	T=..[O|Ts],
	auxOpForComplexities(O,N,_),
	map(toOrderSortedHalf1,Ts,Ts1),
	T1=..[N|Ts1],
	!.
toOrderSortedHalf1(T,T1) :-
	T=..[O|Ts],
	map(toOrderSortedHalf1,Ts,Ts1),
	T1=..[O|Ts1],
	!.

toOrderSortedHalf2(T,T) :-
	var(T),
	!.
toOrderSortedHalf2(V:_T,V) :-
	!.
toOrderSortedHalf2(T,T1) :-
	cont(os,true),
	T=..[TN,T2],
	('$inj' ofType (_O:[TN,_])),
	toOrderSortedHalf2(T2,T1),
	!.
toOrderSortedHalf2(T,T1) :-
	T=..[O|Ts],
	map(toOrderSortedHalf2,Ts,Ts1),
	T1=..[O|Ts1],
	!.

% equalUpToVarRenaming(Terms1,Terms2) == true <=>
%	Terms1 and Terms2 are equal up to consistent variable renaming
%	no Prolog-variable allowed in Terms1 and Terms2

equalUpToVarRenaming(Terms1,Terms2) :-
	Terms1 == Terms2,
	!.
equalUpToVarRenaming(Terms1,Terms2) :-
	eqUpToVarRenaming(Terms1,Terms2,[],_Assoc).


% eqUpToVarRenaming(Terms1,Terms2,Assoc,NewAssoc) == true <=>
%	Terms1 and Terms2 are equal up to consistent variable renaming
%	where NewAssoc denotes this renaming,
%	NewAssoc is a list of pairs of variable names:
%		[(x1,y1),(x2,y2), ... ] (xi will be renamed to yi)
%	and NewAssoc includes Assoc and the new renaming which are needed
%	for the renaming of Terms1 into Terms2

eqUpToVarRenaming([],[],Assoc,Assoc) :- !.
eqUpToVarRenaming([T1|R1],[T2|R2],Assoc,NewAssoc) :- 
	!,
	eqUpToVarRenaming(T1,T2,Assoc,NewAssoc1),
	eqUpToVarRenaming(R1,R2,NewAssoc1,NewAssoc),
	!.
eqUpToVarRenaming(@V1,@V2,Assoc,Assoc) :-
	member((V1,NewV),Assoc),
	!,
	NewV == V2.
eqUpToVarRenaming(@V1,@V2,Assoc,[(V1,V2)|Assoc]) :-
	!,
	\+ member((_V,V2),Assoc),
	!.
eqUpToVarRenaming(T1,T2,Assoc,NewAssoc) :-
	T1 =.. [Op|T1s],
	T2 =.. [Op|T2s],
	!,
	eqUpToVarRenaming(T1s,T2s,Assoc,NewAssoc),
	!.



isConstructor(Op/Arity) :-
	var(Arity),
	!,
	fail.
isConstructor(Op/Arity) :-
	!,
	(hhConstructor(Op) ; hhConstructor(Op/Arity)).
isConstructor(Op) :-
	opPrefix(Op,_,Types),
	(Types = '' ->
		(differentArities(Op) ->
			false
		;
			(Op ofType (_:[_Codomain|Types])),
			length(Types,N),
			hhConstructor(Op/N)
		)
	;
		hhConstructor(Op)
	).

isConstructor(Op,_) :-
	hhConstructor(Op).
isConstructor(Op,N) :-
	number(N),
	hhConstructor(Op/N).
isConstructor(Op,args(Args)) :-
	length(Args,N),
	hhConstructor(Op/N).
isConstructor(Op,type([Codomain|Args])) :-
	length(Args,N),
	hhConstructor(Op/N).
	
