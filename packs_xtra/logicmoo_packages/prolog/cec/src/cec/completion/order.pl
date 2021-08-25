/*
 *	file:		order.pl
 *	version:	1.5
 *	date:		December 7, 1989
 *	creation:	October 10, 1989
 *	author:		-
 *
 *	description:
 *	This file contains predicates to order equations during
 *	completion.
 *
 *	history:
 *	891010	js	Added this comment
 *	891102	rs	Changed definition of
 *			normalizeConstraint/2
 *			reductivityConstraints/3
 *			two improve the provability of quasi-reductivity
 *      891207  js      Fixed a bug in permutEq, causing rejection
 *                      of annotation r for conclusion
 *	900215	uh	Added definition of 
 *			legalOrientation/2
 *			Changed definition of
 *			legalRule/4		orientEq/6	
 *			orderSortedOrdering/6 	orderSortedOrdering/8
 *	900320	uh	Changed definition of
 *			legalOrientation/5
 *			orderSortedOrdering/7 	orderSortedOrdering/9
 *	900518	uh	Changed definition of
 *			orient/5		reductive/3
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

/*----------------------------------------------------------------------*
 *		 orientEq(Kind,(Conditions,(L=R)),E0,ST,M)		*
 *									*
 * tries to orient "Conditions => L=R";					*
 * result : E0=(C0,[L0=R0])						*
 * M = nonoperational or postponed or ...				*
 * ST = set of terms on which the rule has to be superposed on          *
 *----------------------------------------------------------------------*/
orientEq(ask,EqI,(C,[L=R]),E0,ST,M) :-
	fetchAction(orient(A),tryRed,'$equation'(C,[L=R]),EqI,_,[o,p,n]),
	!,
	(A = n ->
		M  = nonoperational
	;
		(A = p ->
			M  = postponed
		;
			orientEq(noask,EqI,(C,[L=R]),E0,ST,M)
		)
	),
	!.
% changed uh 15.02.90
% if we deal with an unconditional equation, we can't check for quasi-reductivity
orientEq(ask,EqI,(C,[L=R]),E0,ST,M) :-
	C == [],
	fetchAction(orient,ask,'$equation'(C,[L=R]),EqI,_,_),
	!,
	sPrint("
Consider the equation
	%.
The following actions may be taken:",['$equation'(C,[L=R])]),
	prompt1(['p. for postpone',
		'n. for considering equation as nonoperational'],[p,n],A),
	assertz(actionNew(orient(A),'$equation'(C,[L=R]))),
	(A = n ->
		M  = nonoperational
	;
		(A = p ->
			M  = postponed
		;
			orientEq(noask,EqI,(C,[L=R]),E0,ST,M)
		)
	),
	!.
orientEq(ask,EqI,(C,[L=R]),E0,ST,M) :-
	fetchAction(orient,ask,'$equation'(C,[L=R]),EqI,_,_),
	!,
	sPrint("
Consider the equation
	%.
The following actions may be taken:",['$equation'(C,[L=R])]),
	prompt1(['o. for attempting to orient into a (quasi-)reductive rule',
		'p. for postpone',
		'n. for considering equation as nonoperational'],[o,p,n],A),
	assertz(actionNew(orient(A),'$equation'(C,[L=R]))),
	(A = n ->
		M  = nonoperational
	;
		(A = p ->
			M  = postponed
		;
			orientEq(noask,EqI,(C,[L=R]),E0,ST,M)
		)
	),
	!.
orientEq(_,_,(C,[L=R]),(C0,[L0=R0]),[(conclusion,left)|CSuperPTerms],M) :-
	(equationClass(C,(L=R),possiblyReductive) ->
		reductive((C,[L=R]),(_C0,[L0=R0]),Mode),
		(Mode = hasBecomeReductive ->
		CSuperPTerms = [],
			C0 = C,
			M  = Mode
		;
			(C \== [], % changed uh 15.02.90 if we deal with an unconditional equation, we can't check for quasi-reductivity
			 Mode = notoriented(c) ->
				orientQuasiRedEq((C,[L=R]),(C0,[L0=R0]),CSuperPTerms,M)
			;	C0 = C,
				L0 = L,
				R0 = R,
				(Mode = notoriented(n) ->
					M = nonoperational
				;	M = postponed
				)
			)
		)
	;	C \== [], % changed uh 15.02.90 if we deal with an unconditional equation, we can't check for quasi-reductivity
		orientQuasiRedEq((C,[L=R]),(C0,[L0=R0]),CSuperPTerms,M)
	),
	!.


orientQuasiRedEq(E,E0,CSuperPTerms,M) :-
	quasiReductive(E,Answer),
	(Answer = sr(E0,CSuperPTerms) ->
		M = hasBecomeReductive
	;	(Answer = n ->
			M = nonoperational
		;	M = postponed
		),
		E0 = E
	),
	!.
			
quasiReductive(E,A) :-
	E = (C,[L=R]),
	fetchAction(orient(A1),tryQuasi,'$equation'(C,[L=R]),_,no,[c,n,p]),
	!,
	(A1 = c ->
		inquireSTs(E,E0,ST),
		A = sr(E0,ST)
	;
		A = A1
	).
quasiReductive(E,A) :-
	E = (C,[L=R]),
	repeat,
	sPrint("The equation
	%
is not reductive.
Do you want a check for quasi-reductivity?",['$equation'(C,[L=R])]),
	prompt1(['c. for check',
		'n. for considering the equation as nonoperational',
		'p. for postpone'],[c,n,p],A1),
	(A1 = c ->
		inquireSTs(E,E0,ST),
		A = sr(E0,ST)
	;
		A = A1,
		try(retract(actionNew(orient(c),'$equation'([],[L=R]))))
	),
	assertz(actionNew(orient(A1),'$equation'(C,[L=R]))).
		


inquireSTs((C,[L=R]),(C1,D1),ST) :-
	fetchAction(annotation(Order),_,'$equation'(C,[L=R]),_,no,_),
	!,
	permuteEq((C,[L=R]),Order,(C1,D1),CT,ST),
	quasiReductive((CT,D1)),
	!.

inquireSTs((C,[L=R]),(C1,D1),ST) :-
	nl,
	sPrint("Enter annotations of literals in
	%
	: ",['$equation'(C,[L=R])]),
	promptFor(Order,lambda([O],listOfLiteralAnnotations(C,O)),
		  sPrint("Syntax: [Orientation, AnnotationCond1, ... , AnnotationCondN],
	Orientation (of consequent) = 
		l(eft-to-right), r(ight-to-left).
	Annotation (of condition)   = 
		l(eft-to-right), r(ight-to-left), u(noriented), s(yntactic).",[])
		 ),
	(permuteEq((C,[L=R]),Order,(C1,D1),CT,ST) ->
		(quasiReductive((CT,D1)) ->
			assertz(actionNew(annotation(Order),'$equation'(C,[L=R]))),
			true
		;
			sPrint("Unable to verify quasi-reductivity. Please, try again.",[]),
			nl,
			fail
		)
	;
		sPrint("With the given annotation, the equation is not deterministic.
Please try again.",[]),
		nl,
		fail
	),
	!.

quasiReductive((C,[L=R])) :-
	vars(L,VL),
	split(lambda([(X=Y,_)],
			[XL,YL]^(	vars(X,XL),
					vars(Y,YL),
					subset(XL,VL),
					subset(YL,VL))),
		C,
		BC,FC),
	setof1((L>X),[A,Y]^(	member((X=Y,A),BC)
			   ;	member((Y=X,u),BC)),
		Reduct1),
	reductivityConstraints((FC,[L=R]),Reduct2),
	append(Reduct1,Reduct2,Reduct),
	!,
	mapA(checkRConstraint,Reduct).



reductivityConstraints((C,D),CSN) :-
	reverse(C,CR),
	reductivityConstraints((CR,D),0,CS),
	transform(normalizeConstraint,CS,CSN),
	!.

%normalizeConstraint(h(_I,T,[]),T) :-
%	!.
%normalizeConstraint(h(I,T,V),HI) :-
%	(	cont(ctr('$rule'),R)
%	;
%		R = 0
%	),
%	mkAtom('$h%_%',[R,I],H),
%	mapF(lambda([X,@X],true),V,VV),<---- removed: see reductivityConstraints
%	HI =.. [H,T|VV],
%	!.
% changed rs 2.11.89 into
normalizeConstraint(h(I,T,V,ST),HI) :-
	(	cont(ctr('$rule'),R)
	;
		R = 0
	),
	mkAtom('$h%_%',[R,I],H),
	append(V,ST,SubTerms), 
	HI =.. [H,T|SubTerms],   % <--- changed [H,T|V]
	!.	


checkRConstraint(L>R) :-
	greaterInRedOrderExt("
Checking reductivity constraint:",L,R).
checkRConstraint(L>=L) :-
	!.
checkRConstraint(L>=R) :-
	checkRConstraint(L>R).
		

crc(([L1=R1|C],[L=R]),TrueConstraints,Constraints,All):-
	crc((C,[L=R]),[L>=L1,L1>=R1|TrueConstraints],
              	      [(TrueConstraints,L>L1)|Constraints],All).
crc(([],[L=R]),TrueConstraints,Constraints,[(TrueConstraints,L>R)|Constraints]).




reductivityConstraints(([],[L=R]),_I,[(L>R)]).
%reductivityConstraints(([(X=Y,A)|C],[L=R]),I,[(h(I,Y,V)>=R)|Constr]) :-
%						      ^  <---------
%	A \== u,
%	vars(R,VR),
%	vars(Y,VY),
%	diff(VR,VY,V),
%	J is I+1,
%	reductivityConstraints((C,[L=h(I,X,V)]),J,Constr).
%					   ^  <----------
% changed rs 2.11.89 into
reductivityConstraints(([(X=Y,A)|C],[L=R]),I,[(h(I,Y,VV,Subterms)>=R)|Constr]) :-
	A \== u,
	vars(L,VL),
	(R = h(_,LS,_Vars,Subt) ->
		RR = LS
	;	RR = R,
		Subt = []
	),
	subtermsWithVars(RR,Rsubt,VL),
	append(Rsubt,Subt,Subterms),
	vars(Subterms,VSub),
	vars(R,VR),
	vars(Y,VY),
	diff(VR,VY,V1),
	diff(V1,VSub,Vars),
	mapF(lambda([X1,@X1],true),Vars,VV), % added
	J is I+1,
	reductivityConstraints((C,[L=h(I,X,VV,Subterms)]),J,Constr).
reductivityConstraints(([(X=Y,u)|C],D),I,Constr) :-
	mkAtom('%%',['V',I],V),
	reductivityConstraints(([(X= @V,l),(Y= @V,l)|C],D),Constr).


subtermsWithVars(T,[T],V) :-
	vars(T,Vt),
	subset(Vt,V),
	!.
subtermsWithVars(@_,[],_V) :- 
	!.
subtermsWithVars(T,L,V) :-
	functor(T,_,N),
	mapTo(subtermsWithVars,T,[V],N,[],VL),
	rReduce(union,[],VL,L).
subtermsWithVars(_T,[],_V).


% permuteEq((+C,[+L=+R]),+LTypes,(-C1,[-L1=-R1]),CT,ST)
% looks for a permutation of the condition C that makes C => L=R
% quasireductive. LTypes gives the orientations of the individual
% equations. C1 => L1=R1 is the permuted equation.
% CT:
% ST:

permuteEq((C,[L=R]),LTypesOrig,(C1,[L1=R1]),CT,ST) :-
	length(C,N),
	listOfNumbers(N,OrderC),
	permutation(OrderC,Perm),
	applyPermutation([notHere|C],[0|Perm],[notHere|CP]),
	applyPermutation(LTypesOrig,[0|Perm],LTypes),
	directedConds(LTypes,LTypesD),
	LTypesD = [Type|CTypesD],
	numberedList(CTypesD,COrderD),
	OrderD = [(0,Type)|COrderD],
	pairing([L=R|CP],LTypesD,CPT),
	mapF(lambda([(LL=RR,CType),LC=RC],
			((CType=r;CType=rres) -> LC=RR,RC=LL;LC=LL,RC=RR)),
		CPT,[L1=R1|C1]),
	superPTerms(OrderD,ST),
	pairing(C1,CTypesD,CT),
	deterministic(L1,R1,CT),
	!.

superPTerms(Annotations,ST) :-
	setof1((condition(I),D1),superPTerm(Annotations,I,D1),ST).

superPTerm(Annotations,I,D1) :-
	member((I,D),Annotations),
	I \== 0,
	(D = l -> D1 = right
	;
	(D = r -> D1 = right	% condition will be reversed
	;
	((D = lres;D = rres) -> (D1 = left;D1 = right))
	)).



directedConds([],[]).
directedConds([s|C1],[lres|C2]) :-
	directedConds(C1,C2).
directedConds([s|C1],[rres|C2]) :-
	!,
	directedConds(C1,C2).
directedConds([D|C1],[D|C2]) :-
	directedConds(C1,C2).


% nondeterministicCond(+L,+C)
% L: left side of conclusion
% C: annotated condition
% succeeds if Cond is nondeterministic (with regard to the Left side
% of the conclusion).

nondeterministicCond(L,C) :-
	append(C1,[(X=_Y,_D)|_C2],C),
	vars((L,C1),V1),
	vars(X,VX),
	\+subset(VX,V1).
nondeterministicCond(L,C) :-
	append(C1,[(_X=Y,u)|_C2],C),
	vars((L,C1),V2),
	vars(Y,VY),
	\+subset(VY,V2).


% deterministic(+L,+R,+C)
% L,R: left,right side of conclusion
% C: annotated condition
% succeeds if the equation C => L=R is deterministic

deterministic(L,R,C) :-
	\+nondeterministicCond(L,C),
	vars((L,C),V),
	vars(R,VR),
	subset(VR,V).



listOfLiteralAnnotations(C,A) :-
	length(C,N),
	length(A,M),
	M is N+1,
	listOfLiteralAnnotationsFrom(0,A).

listOfLiteralAnnotationsFrom(_,[]).
listOfLiteralAnnotationsFrom(0,[T|Xs]) :-
	!,
	(T = l;T = r),
	listOfLiteralAnnotationsFrom(1,Xs).
listOfLiteralAnnotationsFrom(I,[T|Xs]) :-
	(T = l;T = r;T = s;T = u),
	J is I+1,
	listOfLiteralAnnotationsFrom(J,Xs).


% equationClass(+C,(+L=+R),possiblyReductive)
% succeeds if C => L=R could be reductive (considering variables)

equationClass([],_,possiblyReductive):-
	!.
equationClass(C,(L=R),possiblyReductive) :-
	vars(C,VC),
	vars(L,VL),
	vars(R,VR),
	!,
	(	subset(VC,VL),
		subset(VR,VL)
	;	subset(VC,VR),
		subset(VL,VR)
	).




% changed 900518 uh
% call of orient has been changed 
reductive((C,[L=R]),(C,[L0=R0]),M) :-
	(C == [] -> 
		Choices = choices(['p. for postpone',
			 'n. for considering equation as nonoperational'],
			[p,n]),
		EqType  = singleEq
	;
		Choices = choices(['c. for checking quasi-reductivity of equation',
			 'p. for postpone',
			 'n. for considering equation as nonoperational'],
			[c,p,n]),
		EqType  = condEq
	),
	orient(("
Trying to orient equation
	%
into a reductive rule:",['$equation'(C,[L=R])]),EqType,(L=R),(L1->R1),Choices,Mode),
	(Mode = oriented(_),
	 fair((C->(L1->R1)),ModeF) ->
		(ModeF = oriented(_) ->
			M = hasBecomeReductive,
			L0 = L1,
			R0 = R1
		;
			M = ModeF
		)
	;
%		(	Mode = notoriented(A)
%		;
%			A = Mode
%		),
%		assertz(actionNew(orient(A),'$equation'(C,[L=R]))),
		L0 = L,
		R0 = R,
		M  = Mode
	),
	!.





/*----------------------------------------------------------------------*
 *		   	  fair((C->(L->R)),Mode)			*
 *----------------------------------------------------------------------*/


fair((C->(L->R)),Mode) :-
	complexity(C,CTerms),
	multiSetOrdering(("
Checking reductivity constraints for
	%:",['$rule'(C,[L=R])]),[L],CTerms,
		choices(['c. for checking quasi-reductivity of equation',
			 'p. for postpone',
			 'n. for considering equation as nonoperational'],[c,p,n]),
		_,Mode).

reductiveCondition((C,[L=R]),Mode) :-
	complexity(C,CTerms),
	multiSetOrdering(("
Checking reductivity of
	%:",['$nonoperational equation'(C,[L=R])]),[L],CTerms,choices([],[]),_,Mode).


inductiveLemma(I,EP):-
	isReductive(I,'$nonoperational equation',yes),
	prologVariant(I,'$nonoperational equation',(EP,_)),
	'$nonoperational equation'(I,E).


reductiveNopEq(E,yes,yes):-
	reductiveCondition(E,M),
	M=oriented(_),
	!.
reductiveNopEq(_,_,no).



generateBounds(C,L,R,STerms,BL,BR) :-
	setof1((TL,TR),orderCondition(C,STerms,TL,TR),OConds),
	pairing(BTerms,FTerms,OConds),
	vars(L,VL),
	vars(R,VR),
	intersection_s(VL,VR,V),
	mapF(lambda([X,@X],true),V,AtV),
	genAuxOp(O),
	append(BTerms,AtV,BTs),
	append(FTerms,AtV,FTs),
	BL =.. [O|BTs],
	BR =.. [O|FTs],
	!.


orderCondition(C,STerms,T,T1) :-
	position([notHere|C],(T=T1),I),
	\+ member((condition(I),left),STerms).
orderCondition(C,STerms,T,T1) :-
	position([notHere|C],(T1=T),I),
	\+ member((condition(I),right),STerms).
	       



genAuxOp(O) :-
	inc(auxopCtr),
	cont(auxopCtr,N),
	mkAtom('$A%',[N],O),
	!.





/*----------------------------------------------------------------------*
 *		orient(Comment,EqType,(L=R),(LO->RO),Choice,Mode)	*
 * Comment	: for user information					*
 * EqType 	: condEq   for conditional equation			*
 *	    	  singleEq for unconditional equation			*
 * (L=R)  	: conclusion of the equation				*
 * (LO->RO) 	: oriented conclusion, if orient is successful		*
 * Choice	:							*
 * Mode		:							*
 *----------------------------------------------------------------------*/

% changed 900518 uh
% additional parameter EqType
orient(_,_,((A==B)=R),((A==B)->R1),_,oriented(_)) :-
	functor(R,solution,_),
	instantiateDanglingVars((A,B),R,R1),
	!.
orient(_,_,(R=(A==B)),((A==B)->R1),_,oriented(_)) :-
	functor(R,solution,_),
	instantiateDanglingVars((A,B),R,R1),
	!.
orient(Comment,EqType,(L=R),(LO->RO),Choice,Mode) :-
	orderSortedOrdering(Comment,EqType,L,R,LO,RO,Choice,_,Mode1),
	!,
	(Mode1 = oriented(_) ->
		legalRule(LO,RO,Mode1,Mode)
	;
		Mode = Mode1
	).


/*----------------------------------------------------------------------*/
/* legalOrientation(EqType,Left,Right)					*/
/* checks if a some rule with conclusion Left -> Right would be correct	*/
/* This is not the case if						*/
/* - Left is a variable							*/
/* - The top symbol of Left is an constructor				*/
/* - The variables of Right are not contained in the variables of Left,	*/
/*   in the case that the rule is has no condition (otherwise it could  */
/*   be a quasi-reductive rule)						*/

legalOrientation(singleEq,@_,_) :-
	!, fail.
legalOrientation(_,LO,_) :-
	functor(LO,FL,_),
	hhConstructor(FL),
	!, 
	fail.
legalOrientation(singleEq,LO,RO) :-
	!,
	vars(LO,LV), 
	vars(RO,RV),
	subset(RV,LV).
legalOrientation(_,_,_) :- 
	!.

% changed uh 15.02.90
% rules are not legal, if the right term contains variables not
% occurring in the left term
legalRule(LO,RO,Mode1,Mode) :-
		(LO = @_ -> 
			write('Rules with variable left side are not supported.'),
			nl,
			Mode = notoriented(p)
		;
			(functor(LO,FL,_),
			 hhConstructor(FL) -> 
				sPrint("Rules with constructor on top of left side are not supported.",[]),
				nl,
				Mode = notoriented(p)
			;
				(vars(LO,LV), 
				 vars(RO,RV),
				 subset(RV,LV) ->
					Mode = Mode1
				;
					sPrint("Free variables on right side are not supported.",[]),
					nl,
					Mode = notoriented(p)
				)
			)
		).

/*----------------------------------------------------------------------*
 *			matchCheck(L,R,L,R)				*
 *----------------------------------------------------------------------*/

matchCheck(L,R,L,R) :-
	not(L = @(_)),
	matches(L,R).
matchCheck(L,R,R,L) :-
	not(R = @(_)),
	matches(R,L).


/*----------------------------------------------------------------------*
 *			instantiateDanglingVars(T1,T2,T2I)		*
 *			instantiate(T,Env,TI,Env1)			*
 *			instantiateL([X|Xs],E,[XI|XIs],E1)		*
 *----------------------------------------------------------------------*/

instantiateDanglingVars(T1,T2,T2I) :-
	toProlog(T1,[],_,Env),
	instantiate(T2,Env,T2I,_),
	!.


instantiate(@X,Env,@X,Env) :-
	member((X,_),Env),
	!.
instantiate(@X,Env,CX,[(@X,CX)|Env]) :-
	concAtomNames('_',X,CX),
	!.
instantiate(T,Env,TI,Env1) :-
	T =.. [O|Ts],
	instantiateL(Ts,Env,TIs,Env1),
	TI =.. [O|TIs],
	!.


instantiateL([],E,[],E).
instantiateL([X|Xs],E,[XI|XIs],E1) :-
	instantiate(X,E,XI,EX),
	instantiateL(Xs,EX,XIs,E1),
	!.


/*----------------------------------------------------------------------*
 *		    splitEquation(L,R,[(L->T),(R->T)])			*
 *----------------------------------------------------------------------*/

splitEquation(L,R,[(L->T),(R->T)]) :-
	nl,
	write('to split the equation, enter a new function symbol'),
	readAndCheck(F),
	inferType(F,L,R,VarList),
	T =.. [F|VarList],
	!.


/*----------------------------------------------------------------------*
 *		 	   readAndCheck(F)				*
 *----------------------------------------------------------------------*/

readAndCheck(F) :-
	repeat,
	read(F),
	(	atom(F)
	;
		nl,
	     	write('function symbols must be atoms; try again.'),
	     	fail
	),
	(	not (F : _)
	;
		nl,
	     	write('function symbol already used; try again.'),
	     	fail
	),
	!.


/*----------------------------------------------------------------------*
 *      multiSetOrdering((Comment,CArgs),L,R,choices(Requests,Answers),	*
 *								_,Mode)	*
 *----------------------------------------------------------------------*/


multiSetOrdering(Comment,S1,S2,_,Ext,oriented(_)) :-
	multiSetDiff(S1,S2,T1),
	multiSetDiff(S2,S1,T2),
	T1 \== [],
	mapP(lambda([X2],
		    [X1,M,H]^
		    (  member(X1,T1),
		       orderSortedOrdering(Comment,condEq,X1,X2,choices([],[]),Ext,M),
		       M = oriented(H)
		    )),
	     T2),
	!.
multiSetOrdering(_,_,_,choices([],[]),_,M):-
	!,
        M=notoriented(_).
multiSetOrdering((_Comment,_CArgs),L,R,choices(_Requests,_Answers),_,
		 oriented(manually)) :-
	fetchAction(orient(a),multiSetOrd,'$equation'([],[L=R]),_,no,_),
	!.
multiSetOrdering((_Comment,_CArgs),L,R,choices(_Requests,Answers),_,
		 notoriented(A)) :-
	fetchAction(orient(A),multiSetOrd,'$equation'([],[L=R]),_,no,Answers),
	!.
multiSetOrdering((Comment,CArgs),L,R,choices(Requests,Answers),_,Mode) :-
	sPrint(Comment,CArgs),
	nl,
	write('The current ordering fails to prove'),
	nl,
	print('$term'(L)),
	write('  >  '),
	print('$term'(R)),
	write('.
At this point you may take any of the the following actions:'),
	prompt1(['a. for assume to be proved'|Requests],[a|Answers],A),
	(A = a ->
		Mode = oriented(manually)
	;
		Mode = notoriented(A)
	),
	assertz(actionNew(orient(A),'$equation'([],[L=R]))),
	!.


/*----------------------------------------------------------------------*
 *      extendedMultiSetOrdering(Comment,X1,X2,Ext,oriented(_))		*
 *----------------------------------------------------------------------*/

extendedMultiSetOrdering(Comment,X1,X2,Ext,oriented(_)) :-
	tps_current_ordering(kns_or_neqkns),
	X1 =.. ['$r'|S1],
	X2 =.. ['$r'|S2],
	multiSetOrdering(Comment,S1,S2,choices([],[]),Ext,M),
	!,
	M = oriented(_).
extendedMultiSetOrdering(Comment,X1,X2,Ext,oriented(_)) :-
	tps_current_ordering(poly(_N)),
	X1 =.. ['$r'|S1],
	X2 =.. ['$r'|S2],
	multiSetOrdering(Comment,S1,S2,choices([],[]),Ext,M),
	!,	% muss sein, da der letzte Fall nicht zus"atzlich
		% angewendet werden kann; sonst nicht wohl-fundiert
	M = oriented(_).
extendedMultiSetOrdering(_Comment,X1,X2,_Ext,oriented(_)) :-
	tps_current_ordering(poly(_N)),
	\+ msComplexity(X1),
	msComplexity(X2),
	!.
extendedMultiSetOrdering(Comment,X1,X2,Ext,oriented(_)) :-
	(	tps_current_ordering(kns_or_neqkns)
	;	\+msComplexity(X1),
		\+msComplexity(X2)),
	orderSortedOrdering(Comment,condEq,X1,X2,choices([],[]),Ext,M),
	!,
	M = oriented(_),
	!.
extendedMultiSetOrdering(_,_,_,_,notoriented(_)).
/* old
extendedMultiSetOrdering(Comment,X1,X2,Ext,oriented(_)) :-
	tps_current_ordering(poly(_N)),
	X1 =.. [F1|S1],
	X2 =.. [F2|S2],
	nRedComplOp(F1),
	nRedComplOp(F2),
	!,
	alphabeticOrdering(Comment,S1,S2,choices([],[]),Ext,M),
	M = oriented(_).
*/





alphabeticOrdering(_Comment,[_|_],[],_,_Ext,oriented(_)).
alphabeticOrdering(Comment,[X1|S1],[X2|S2],_,Ext,M):-
	equalInterpretations(X1,X2),
	alphabeticOrdering(Comment,S1,S2,_,Ext,M).
alphabeticOrdering(Comment,[X1|_],[X2|_],_,Ext,oriented(_)) :-
	orderSortedOrdering(Comment,condEq,X1,X2,choices([],[]),Ext,M),
	!,
	M = oriented(_),
	!.
alphabeticOrdering(_Comment,_,_,_,_Ext,notoriented(_)).








/*----------------------------------------------------------------------*
 *     			 multiSetDiff(S1,[X|S2],S)			*
 *----------------------------------------------------------------------*/

multiSetDiff(S,[],S) :- !.
multiSetDiff(S1,[X|S2],S) :-
	multiSetDelete(S1,X,S11),
	!,
	multiSetDiff(S11,S2,S).
multiSetDiff(S1,[_|S2],S) :-
	multiSetDiff(S1,S2,S).


/*----------------------------------------------------------------------*
 *     			 multiSetDelete([X|Xs],Y,Xs)			*
 *----------------------------------------------------------------------*/

multiSetDelete([X|Xs],Y,Xs) :-
	equalInterpretations(X,Y),
	!.
multiSetDelete([Y|Xs],X,[Y|Ys]) :-
	multiSetDelete(Xs,X,Ys).
multiSetDelete([],_,[]).

equalInterpretations(X,Y):-
	ac_matchvf(X,Y),
	!.
equalInterpretations(X,Y):-
	tps_current_ordering(poly(_N)),
	manyToOrderSorted(X,XOS),
	manyToOrderSorted(Y,YOS),
	XOS\==YOS,
	!,	% also, \+ac_matchvf(X,Y) at this point
	polInterpretation(XOS,Vars,P),
	polInterpretation(YOS,Vars,P).
equalInterpretations(X,Y):-
	tps_current_ordering(poly(_N)),
	% XOS=YOS and \+ac_matchvf(X,Y) at this point
	polInterpretation(X,Vars,P),
	polInterpretation(Y,Vars,P).

/*----------------------------------------------------------------------*
 *     	    orderSortedOrdering(Comment,L,R,LO,RO,Choice,Ext,M)		*
 *     	       orderSortedOrdering(Comment,L,R,Choice,Ext,M)		*
 *----------------------------------------------------------------------*/


% changed uh 15.02.90
% special treatment for equation which can only in one or no direction	
orderSortedOrdering(Comment,_,L,R,LO,RO,choices([],[]),Ext,M) :-
	manyToOrderSorted(L,L1),
	manyToOrderSorted(R,R1),
	(L \== R,
	 L1 = R1 ->
		LL = L,
		RR = R
	;
		LL = L1,
		RR = R1
	),
	!,
	tps_ordering(Comment,LL,RR,LLO,_,choices([],[]),Ext,M),
	!,
	(LL = LLO ->
		LO = L,
		RO = R
	;
		LO = R,
		RO = L
	),
	!.
orderSortedOrdering(Comment,EqType,L,R,LO,RO,choices(Requests,Answers),Ext,M) :-
	manyToOrderSorted(L,L1),
	manyToOrderSorted(R,R1),
	(L \== R,
	 L1 = R1 ->
		LL = L,
		RR = R
	;
		LL = L1,
		RR = R1
	),
	!,
	(legalOrientation(EqType,LL,RR) ->
		(legalOrientation(EqType,RR,LL) ->
			append(['l. for left-to-right',
				'r. for right-to-left'],Requests,NRequests),
			append([l,r],Answers,NAnswers),
			tps_ordering(Comment,LL,RR,LLO,_,choices(NRequests,NAnswers),Ext,M)
			;
			LLO = LL,
			(Ext = yes ->
				true
			;
				Ext1 = no
			),
			tps_ordering(Comment,LL,RR,choices(Requests,Answers),Ext1,M)
		)
		;
		(legalOrientation(EqType,RR,LL) ->
			LLO = RR,
			(Ext = yes ->
				true
			;
				Ext1 = no
			),
			tps_ordering(Comment,RR,LL,choices(Requests,Answers),Ext1,M)
			;
			unorientableEq(LL,RR,M)
		)
	),
	!,
	(LL = LLO ->
		LO = L,
		RO = R
	;
		LO = R,
		RO = L
	),
	!.

orderSortedOrdering(Comment,_,L,R,choices([],[]),Ext,M) :-
	manyToOrderSorted(L,L1),
	manyToOrderSorted(R,R1),
	(L \== R,
	 L1 = R1 ->
		LL = L,
		RR = R
	;
		LL = L1,
		RR = R1
	),
	!,
	(Ext = yes ->
		true
	;
		Ext1 = no
	),
	tps_ordering(Comment,LL,RR,choices([],[]),Ext1,M),
	!.
orderSortedOrdering(Comment,EqType,L,R,Choice,Ext,M) :-
	manyToOrderSorted(L,L1),
	manyToOrderSorted(R,R1),
	(L \== R,
	 L1 = R1 ->
		LL = L,
		RR = R
	;
		LL = L1,
		RR = R1
	),
	!,
	(Ext = yes ->
		true
	;
		Ext1 = no
	),
	legalOrientation(EqType,LL,RR),
	tps_ordering(Comment,LL,RR,Choice,Ext1,M),
	!.

unorientableEq(LL,RR,M) :-
	nl,
	write('CEC is not able to complete a system containing the equation'),
	nl,
	sPrint("	%.",['$singleEq'(LL,RR)]),
	write('
You may postpone this equation and complete the rest of the system:
'),
	prompt1(['p. for postpone'],[p],C),
	M = notoriented(C),
	assertz(actionNew(orient(C),'$equation'([],[LL=RR]))),
	!.

greaterInRedOrder(X1,X2) :-
	(cont(os,true) ->
		Ext = no
	;
		true
	),
	orderSortedOrdering(("
Checking complexity of simplification proof:",[]),condEq,X1,X2,choices([],[]),Ext,M),
	M = oriented(_),
	!.

greaterInRedOrderExt(Comment,X1,X2) :-
	orderSortedOrdering((Comment,[]),condEq,X1,X2,choices([],[]),_yes,M),
	M = oriented(_),
	!.
greaterInRedOrderExt(_Comment,L,R) :-
	fetchAction(orient(a),greaterInRedOrd,'$equation'([],[L=R]),_,no,_),
	!.
greaterInRedOrderExt(Comment,L,R) :-
	sPrint("%
The current ordering fails to prove
	% > %.
At this point you may take any of the following actions:",['$string'(Comment),'$term'(L),'$term'(R)]),
	prompt1(['a. for assume to be proved'],[a],A),
	!,
	A = a,
	assertz(actionNew(orient(a),'$equation'([],[L=R]))).


greaterInRedOrderNoExt(X1,X2) :-
	orderSortedOrdering(("",[]),condEq,X1,X2,choices([],[]),no,M),
	M = oriented(_),
	!.
