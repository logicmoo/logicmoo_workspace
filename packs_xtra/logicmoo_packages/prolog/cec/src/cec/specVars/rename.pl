/*
 *	file:		rename.pl
 *	version:	1.5
 *	date:		November 8, 1989
 *	creation:	November 6, 1989
 *	author:		-
 *
 *	description:
 *	This file contains the predicates for applying a sort and 
 *	operator renaming to a specification.
 *
 *	history:
 *	891106	uh	Added this comment
 *	891108	uh 	Moved definition of
 *			renameInClauses/4 
 *			into this file
 *	891108 	uh 	Moved defintion of
 *			isAssoc/1
 *			into file morphisms.pl
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */


renameSpec(Assoc) :-
	undoUponFail(renameSpec(Assoc,[],user,E),E),
	!,
	E = none.

renameSpec(S,O) :-
	undoUponFail(renameSpec(S,O,user,E),E),
	!,
	E = none.


renameSpec(Assoc,To,E) :-
	split(lambda([(X,_)],sort(X)),Assoc,S,O),
	renameSpec(S,O,To,E).

renameSpec(S,O,To,none) :-
	error:==none,
	isAssoc(sorts,S),
	isAssoc(operators,O),
	completeOpAssoc(O,S,OC),
	injective(OC),
	cont1(error,none),
	split(lambda([(X,Y)],X=Y),OC,_,OI),
	auxOpRenaming(OI,OIA),
	renameInClauses(S,OIA,O,To).
renameSpec(Assoc,[],To,none) :-
	error:==none,
	map(lambda([Pair,(X,Y)],
		(Pair = (X <- Y) ->
			true
		;	error("Illegal element in interface expression: %",[Pair],normalizeInterfaceExp))),Assoc,AssocPairs),
	split(lambda([(X,_)],sort(X)),AssocPairs,S,O),
	completeOpAssoc(O,S,OC),
	injective(OC),
	cont1(error,none),
	split(lambda([(X,Y)],X=Y),OC,_,OI),
	auxOpRenaming(OI,OIA),
	renameInClauses(S,OIA,O,To).
renameSpec(S,O,_,E):-
	error("Illegal association lists
     %
and  %
or operator renaming not injective!",[S,O],renameSpec),
	E=error.

/*----------------------------------------------------------------------*
 *		 	  renameInClauses(S,O)				*
 *----------------------------------------------------------------------*/

renameInClauses(S,O,OOS,'user') :-
	rename((S,O,OOS),Fact),
	assertz('$afterRename':Fact),
	fail.
renameInClauses(_,_,_,user) :-
	assertz(world('$afterRename')),
	rs('$afterRename'),
	ds('$afterRename'),
	!.
renameInClauses(S,O,OOS,To) :-
	ds(To),
	rename((S,O,OOS),Fact),
	assertz(To:Fact),
	fail.
renameInClauses(_,_,_,To) :-
	assertz(world(To)).

/*----------------------------------------------------------------------*
 *			 rename((_,Assoc,_),W2,Fact3)			*
 *		       rename(_,W3,cont(W3,ctr(T),N1))			*
 *----------------------------------------------------------------------*/

%rename((_,Assoc),W2,Fact3) :-	% geaendert rs 4.01.89
rename((SAssoc,OAssoc,_),Fact3) :-
	relationTypes(Ts),
	member(T,Ts),
	Fact1=..[T,I,O],
	Fact1,
	Fact3=..[T,I,OR],
	applyAssoc(O,OR,(SAssoc,OAssoc)).



rename(_,cont(ctr(T),N1)) :-
	relationTypes(Ts),
	member(T,Ts),
	functor(Obj,T,2),
	(Obj ->
		cont(ctr(T),N1)
	;	N1=0).
%       an Stelle von if-then-else einfach cont(ctr(T),N1) in Version 13.2.89

rename(_,cont(os,true)) :-
	cont(os,true).

rename((SA,_,_),cont(vars,VR)) :-
	cont(vars,V),
	applyAssoc(V,VR,SA).

rename(([],_,_),cont(completeState,C)):-
	cont(completeState,C).

rename((SA,OA,_),Fact3) :-	
	attribute(A,T),
	A\==consequentType,
	Fact1=..[A,I,T,O],
	Fact1,
	Fact3=..[A,I,T,OR],
	applyAssoc(O,OR,(SA,OA)).

rename((SA,_,_),consequentType(I,T,TypeR)):-
	consequentType(I,T,Type),
	applyAssoc(Type,TypeR,SA).



/*----------------------------------------------------------------------*
 *		   rename(_,W3,'$maySuperpose'(W3,I,J))			*
 *----------------------------------------------------------------------*/

rename(_,'$maySuperpose'(Ch,I,TI,Ty,J,L,TJ)) :-
	'$maySuperpose'(Ch,I,TI,Ty,J,L,TJ).


/*----------------------------------------------------------------------*
 *		 rename((_,Assoc,_),W3,clauseHasTerm(W3,I,T,JR))	*
 *----------------------------------------------------------------------*/

rename((SA,OA,_),clauseHasTerm(I,T,JR)) :-
	clauseHasTerm(I,T,J),
	applyAssoc(J,JR,(SA,OA)).


/*----------------------------------------------------------------------*
 *		   rename(_,W3,mayBeReducible(W3,I,T,K))		*
 *----------------------------------------------------------------------*/

rename(_,mayBeReducible(I,T,K)) :-
	mayBeReducible(I,T,K).	




/*----------------------------------------------------------------------*
 *		  rename((SA,OA,_),W3,ofType(W3,OR,(O1R:TR)))		*
 *----------------------------------------------------------------------*/

rename((SA,OA,_),ofType(ORA,(O1R:TR))) :-	
	ofType(_,(O1:T)),
	applyAssoc(O1,O1R,OA),
	applyAssoc(T,TR,SA),
	opPrefix(O1R,OR,_),
	numberAsAtom(OR,T,ORA).



/*----------------------------------------------------------------------*
 *	     	    rename((SA,_,_),W3,subsort(W3,S,T))			*
 *----------------------------------------------------------------------*/

rename((SA,_,_),subsort(S,T)) :-
	subsort(S1,T1),
	applyAssocA((S1,T1),(S,T),SA).


/*----------------------------------------------------------------------*
 *		 rename((SA,OA),W3,inheritanceAxiom(W3,T,D))		*
 *----------------------------------------------------------------------*/

rename((SA,OA,_),inheritanceAxiom(T,D)) :-
	inheritanceAxiom(T,D1),
	applyAssoc(D1,D2,SA),
	applyAssoc(D2,D,(SA,OA)).


/*----------------------------------------------------------------------*
 *		    rename(_,W3,notation(W3,O,P,N))			*
 *----------------------------------------------------------------------*/

rename(_,notation(O,P,N)) :-
	notation(O,P,N).


/*----------------------------------------------------------------------*
 *		    rename(_,W3,ac_ist_C(W3,HH))			*
 *		    rename(_,W3,ac_ist_A(W3,HH))			*
 *----------------------------------------------------------------------*/


rename((_,OA,OAOS),ac_ist_C(HHR)) :-		% rs 29.03.88
	append(OA,OAOS,A),
	ac_ist_C(HH),
	applyAssoc(HH,HHR,A).


rename((_,OA,OAOS),ac_ist_A(HHR)) :-		% rs 29.03.88
	append(OA,OAOS,A),
	ac_ist_A(HH),	
	applyAssoc(HH,HHR,A).


/*----------------------------------------------------------------------*
 *		    rename(_,W3,hhConstructor(W3,HH))			*
 *----------------------------------------------------------------------*/

rename((_,OA,OAOS),hhConstructor(HHR)) :-	% rs 29.03.88
	append(OA,OAOS,A),
	hhConstructor(HH),
	applyAssoc(HH,HHR,A).


/*----------------------------------------------------------------------*
 *		    rename(_,W3,domainAxiomHasBeenGenerated(W3,Op))	*
 *----------------------------------------------------------------------*/

rename((_,OA,OAOS),domainAxiomHasBeenGenerated(OpR)) :-
	append(OA,OAOS,A),
	domainAxiomHasBeenGenerated(Op),
	applyAssoc(Op,OpR,A).


/*----------------------------------------------------------------------*
 *		      rename(_,W3,tps_state(W3,P))			*
 *----------------------------------------------------------------------*/

rename((_,OA,OAOS),tps_state(tps(Type,PR,KnsState))) :-
	append(OA,OAOS,A),
	tps_state(tps(Type,P,KnsState)),
	pol_rename2(P,A,PR).




/*----------------------------------------------------------------------*
 *		   rename((_,OA,_),W3,(reduce(W3,AR,BR):-TR))		*
 *----------------------------------------------------------------------*/

rename((_,OA,_),(reduce(AR,BR):-TR)) :-
	clause(reduce(A,B),T),
	applyAssoc((A,B,T),(AR,BR,TR),OA).


rename((_,OA,_),(reduceI(AR,I,BR):-TR)) :-
	clause(reduceI(A,I,B),T),
	applyAssoc((A,B,T),(AR,BR,TR),OA).



/*----------------------------------------------------------------------*
 *		      rename((_,OA,_),W3,specialLaw(W3,LR))		*
 *----------------------------------------------------------------------*/

rename((SA,OA,_),specialLaw(LR)) :-
	specialLaw(L),
	applyAssoc(L,LR,(SA,OA)).


/*----------------------------------------------------------------------*
 *		      rename((_,OA,_),W3,kns_gt(A,B))			*
 *		      rename((_,OA,_),W3,kns_eq(A,B))			*
 *		      rename((_,OA,_),W3,kns_status(A,B))		*
 *----------------------------------------------------------------------*/

rename((_,OA,OAOS),kns_gt(A,B)):-
	append(OA,OAOS,Assoc),
	kns_gt(A1,B1),
	applyAssoc((A1,B1),(A,B),Assoc).

rename((_,OA,OAOS),kns_eq(A,B)):-
	append(OA,OAOS,Assoc),
	kns_eq(A1,B1),
	applyAssoc((A1,B1),(A,B),Assoc).

rename((_,OA,OAOS),kns_status(A,B)):-
	append(OA,OAOS,Assoc),
	kns_status(A1,B),
	applyAssoc(A1,A,Assoc).



/*----------------------------------------------------------------------*
 *		rename((_,OA,_),W3,(pretty(W3,AR):-TR))			*
 *		rename((_,OA,_),W3,(parse(W3,AR,BR):-TR))		*
 *		rename((_,OA,_),W3,(predicate(W3,AR):-TR))		*
 *----------------------------------------------------------------------*/

rename((_,OA,_),(pretty(AR):-TR)) :-
	clause(pretty(A),T),
	applyAssoc((A,T),(AR,TR),OA).


rename((_,OA,_),(parse(AR,BR):-TR)) :-
	clause(parse(A,B),T),
	applyAssoc((A,B,T),(AR,BR,TR),OA).


rename((_,OA,_),(predicate(AR):-TR)) :-
	clause(predicate(A),T),
	applyAssoc((A,T),(AR,TR),OA).


/*----------------------------------------------------------------------*
 *		rename((SA,OA,_),(action(A,EqR)))			*
 *		rename((SA,OA,_),(actionNew(A,EqR)))			*
 *----------------------------------------------------------------------*/

rename((SA,OA,_),action(A,EqR)) :-
	action(A,Eq),
	applyAssoc(Eq,EqR,(SA,OA)).
rename((SA,OA,_),actionNew(A,EqR)) :-
	actionNew(A,Eq),
	applyAssoc(Eq,EqR,(SA,OA)).


/*----------------------------------------------------------------------*
 *		rename((SA,OA,_),(parameterSort(S)))			*
 *----------------------------------------------------------------------*/

rename((SA,_,_),parameterSort(SR)) :-
	parameterSort(S),
	applyAssoc(S,SR,SA).

/*----------------------------------------------------------------------*
 *		rename((SA,OA,_),(generatingMsOps(S,Ops))		*
 *----------------------------------------------------------------------*/

rename((SA,OA,OAOS),generatingMsOps(SR,OpsR)) :-
	append(OA,OAOS,Assoc),
	generatingMsOps(S,Ops),
	applyAssoc(S,SR,SA),
	applyAssoc(Ops,OpsR,Assoc).

/*----------------------------------------------------------------------*/

numberAsAtom(O,T,O1):-
	length(T,M),
	M>1,
	number(O),
	number_chars(O,N),
	atom_chars(O1,N),
	!.
numberAsAtom(O,_,O).

