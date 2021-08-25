/*
 *	file:		mapexpansion.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates to expand higher order predicates
 *	during compilation. This is done for efficiency.
 *
 *	history:
 *	891010	js	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

/*----------------------------------------------------------------------*
 *	   defines procedures to expand higher-order predicates		*
 *		during compilation
 *		removes calls to accTime, where wanted			*
 *----------------------------------------------------------------------*/

% declaration of higher-order predicates:
% higherOrderPred(P/N,M) <=>
%	 P/N is a predicate which takes as first argument a predicate of arity M

higherOrderPred(mapP/2,1).
higherOrderPred(mapF/3,2).
higherOrderPred(mapA/2,1).
higherOrderPred(map/3,2).
higherOrderPred(rReduce/4,3).
higherOrderPred(transform/3,2).
higherOrderPred(transformOrFail/3,2).
higherOrderPred(transformL/5,2).
higherOrderPred(allOverloadedInstances/2,1).
higherOrderPred(split/4,1).
higherOrderPred(transClosureTest/3,2).
higherOrderPred(transClosure/3,2).
higherOrderPred(reflTransClosure/3,2).
higherOrderPred(path/5,2).


/*----------------------------------------------------------------------*
 *	   			compileL(F)				*
 *----------------------------------------------------------------------*/

compileL(F) :-
	term_expansion,
	abolish(specializedClauses,2),
	compileA(F),
	abolish(term_expansion,2),
	baseName(F,File),
	objPath(ObjPath),
	mkAtom('%/%.map',[ObjPath,File],FM),
	compileSpecializations(FM,compile).


/*----------------------------------------------------------------------*
 *	   	 	        reconsultL(F)				*
 *----------------------------------------------------------------------*/

reconsultL(F) :-
	term_expansion,
	abolish(specializedClauses,2),
	reconsult(F),
	abolish(term_expansion,2),
	baseName(F,File),
	objPath(ObjPath),
	mkAtom('%/%.map',[ObjPath,File],FM),
	compileSpecializations(FM,consult).


/*----------------------------------------------------------------------*
 *	   	 	        compileSpecializations(FM)		*
 *----------------------------------------------------------------------*/

compileSpecializations(_,_) :-
	\+ specializedClauses(_,_),
	!.
compileSpecializations(FM,_) :-
	tell(FM),
	retract(specializedClauses(_,Cls)),
	mapP(lambda([C],(portray_clause(C),nl)),Cls),
	fail.
compileSpecializations(FM,compile) :-
	told,
	no_style_check(single_var),
	compileL(FM),
	style_check(single_var).
compileSpecializations(FM,consult) :-
	told,
	no_style_check(single_var),
	reconsultL(FM),
	style_check(single_var).


/*----------------------------------------------------------------------*
 *	   	 	        term_expansion				*
 *				term_exp(_,_,_)				*
 *----------------------------------------------------------------------*/

term_expansion :-
	assertz((term_expansion(A,B):-clause_exp(A,B))).

clause_exp(X,X):-
	var(X),
	!.
clause_exp((H:-G),(H:-G1)) :-
	!,
	term_exp(G,G1),
	(cont1(trace(expansion),true),
	 G\==G1 ->
		sPrint("
% expanded to
%
",[(H:-G),(H:-G1)])
	;
		true
	).
clause_exp(H,H).



term_exp(X,X) :-
	var(X),
	!.
term_exp((H,G),(H1,G1)) :-
	term_exp(G,G1),
	term_exp(H,H1).
term_exp((H;G),(H1;G1)) :-
	term_exp(G,G1),
	term_exp(H,H1).
term_exp((P->H),(P1->H1)) :-
	term_exp(P,P1),
	term_exp(H,H1).
term_exp(\+P,\+P1) :-
	term_exp(P,P1).
term_exp(accTime(_,T),T1):-
	benchmark(no),
	!,
	term_expLiteral(T,T1).
term_exp(accTime(T),T1):-
	benchmark(no),
	!,
	term_expLiteral(T,T1).
term_exp(T,accTime(T1)) :-
	functor(T,F,N),
	(	benchmark(F/N)
	;
		benchmark(F)
	),
	!,
	term_expLiteral(T,T1).
term_exp(T,T1):-
	term_expLiteral(T,T1).
%term_exp(accTime(N,P),accTime(N,P1)) :-
%	benchmark(N),
%	!,
%	term_exp(P,P1).
%term_exp(accTime(_,P),P1) :-
%	term_exp(P,P1).


term_expLiteral(noExpansion(X),X).
term_expLiteral(X,X) :-
	mapExpansion(no).
term_expLiteral(T,CallToExpanded) :-
	higherOrderCall(T,Pred/N,[lambda(BoundVarsOfFirstArg,LV^Body)|OtherArgs]),
	!,
	term_exp(Body,BodyOfFirstArg),
	freeVars(lambda(BoundVarsOfFirstArg,LV^BodyOfFirstArg),Free),
	genExpandedProc(Pred/N,lambda(BoundVarsOfFirstArg,LV^BodyOfFirstArg),
			       SpecializedPred),
	append(OtherArgs,Free,ArgsOfExpandedCall),
	CallToExpanded =.. [SpecializedPred|ArgsOfExpandedCall].
term_expLiteral(X,X).


higherOrderCall(T,P/N,[FunArg|OtherArgs]) :-
	nonvar(T),
	functor(T,P,N),
	T =.. [P,Q|OtherArgs],
	nonvar(Q),
	higherOrderPred(P/N,ArityOfPredParam),
	normalizedFunArg(Q/ArityOfPredParam,FunArg),
	!.


normalizedFunArg(Q/N,lambda(Vars,[]^Body)) :-
	atom(Q),
	functor(Body,Q,N),
	Body =.. [_|Vars].
normalizedFunArg(lambda(Vars,LV^Body)/_N,lambda(Vars,LocalVars^Body)) :-
	varsOf(LV,LocalVars),
	!.
normalizedFunArg(lambda(Vars,Body)/_N,lambda(Vars,[]^Body)).
normalizedFunArg((Proc,FreeVars)/N,lambda(Vars,[]^Body)) :-
	atom(Proc),
	genVarList(N,Vars),
	append(Vars,FreeVars,Args),
	Body =.. [Proc|Args].
normalizedFunArg((Proc,_FreeVars)/_N,_) :-
	var(Proc),
	!,
	fail.
normalizedFunArg((lambda(Vars,[]^Body),FreeVars)/_N,lambda(Vars,[]^BodyF)) :-
	Body =.. [P|Vars],
	append(Vars,FreeVars,V),
	BodyF =.. [P|V].


genExpandedProc(P/N,FunArg,NewPred) :-
	nameForNewSpecialization(P/N,FunArg,NewPred,Status),
	(Status = new ->
		freeVars(FunArg,Free),
		length(Free,NF),
		NN is N+NF-1,
		clauses(P/N,Cls),
		mapF(lambda([C,SC],
			specializedClause(P/N,NewPred/NN,C,closure(FunArg,Free),SC)),
				Cls,SpecializedCls),
		assert(specializedClauses(NewPred,SpecializedCls))
	;
		true
	).


specializedClause(P/N,NewP/NN,(Head:-Body),closure(L,Free),(HeadS:-BodyS)) :-
	Head =.. [_,FunArg|Args],
	append(Args,Free,ArgsP),
	HeadS =.. [NewP|ArgsP],
	specializedBody(Body,P/N,NewP/NN,(FunArg=closure(L,Free)),BodyS),
	!.


specializedBody(Body,P,NewP,Closure,BodyS) :-
	transform(lambda([T,ST],specialized(T,P,NewP,Closure,ST)),Body,BodyS).


specialized(X,_,_,(FunArg=closure(L,_Free)),Y) :-
	var(X),
	!,
	(X == FunArg ->
		Y = L
	;
		Y = X
	).
specialized(apply(Proc,Params),_,_,(FunArg=closure(lambda(Vs,Local^B),Free)),BSubst) :-
	Proc == FunArg,
	!,
	renamePVars(closure(lambda(Vs,Local^B),Free),closure(lambda(Params,_^BSubst),Free)).
specialized(PCall,P/N,NewP/_NN,(FunArg=closure(_L,Free)),NewPCall) :-
	functor(PCall,P,N),
	PCall =.. [P,FArg|Args],
	FArg == FunArg,
	!,
	append(Args,Free,NewPArgs),
	NewPCall =.. [NewP|NewPArgs].


clauses(P/N,Cls) :-
	functor(Head,P,N),
	bagof((Head:-B),clause(Head,B),Cls),
	!.
clauses(P/N,_):-
	sPrint("*** No clauses for %. Term expansion failed",[P/N]),
	abort.


/*----------------------------------------------------------------------*
 *			freeVars(Local,V,P,FreeVars)			*
 *----------------------------------------------------------------------*/

freeVars(lambda(V,Local^P),FreeVars) :-
	varsOf(V,VV),
	append(VV,Local,LV),
	varsOf(P,VP),
	setof1(Var,(member(Var,VP),\+member_s(Var,LV)),FreeVars).


/*----------------------------------------------------------------------*
 *	   nameForNewSpecialization     				*
 *----------------------------------------------------------------------*/


nameForNewSpecialization(P/N,FunArg,NewPred,Status) :-
	fromProlog(FunArg,[],F,_),
	(specialGenerated(P/N,F,NewPred) ->
		Status = old
	;
		Status = new,
		(	retract(pc(PC))
		;
			PC = 1
		),
		mkAtom('$%$%$%',[P,N,PC],NewPred),
		PC1 is PC+1,
		assert(pc(PC1)),
		assert(specialGenerated(P/N,F,NewPred))
	),
	!.

	
writeRule(R) :-
	assertz(auxMapClause(R)).

	
:- abolish(specialGenerated,3).
