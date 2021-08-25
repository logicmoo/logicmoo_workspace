/*
 *	file:		dbKit.pl
 *	version:	1.5
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains database manipulation functions.
 *
 *	history:
 *	891010	js	Added this comment
 *	891030  uh	Moved definitions of
 * 			showDBos/0   	    	showDBms/0  	
 *			showDB/0		printTupelsOS/1		
 *			printTupelsMS/1		printTuples/1
 *			hasOverloadedOp/1	relevantOrderSorted/1
 *			nonredundantOS/2
 *			into display/show.pl
 *	891039  uh	Moved definition of
 *			try/1
 *			into prolog/utilities.pl
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

/*----------------------------------------------------------------------*
 *		  new(Type,Object,I,WithAttributes,Ref)			*
 *----------------------------------------------------------------------*/

new(T,O,I,A,R):-
	accTime(new(T),new1(T,O,I,A,R)),
	!.

new1(Type,Object,I,WithAttributes,Ref) :-
	var(I),
	!,
	newIndex(Type,I1),
	new(Type,Object,I1,WithAttributes,Ref,Kind),
	(	Kind=old(I)
	;
		I=I1
	),
	!.
new1(Type,Object,I,WithAttributes,Ref) :-
	new(Type,Object,I,WithAttributes,Ref,_),
	!.


genNew(Type,(new2(Type,Object,I,WithAttributes,AllAttributes):-Body)):-
	attributes(Type,ANames),
	pairing(ANames,_VNames,AllAttributes),
	mapF(lambda([(A,V),C],genACall(Type,A,V,@type,@i,@object,@withAttributes,C)),AllAttributes,ACalls),
	toProlog(ACalls,[(type,Type),(i,I),(object,Object),(withAttributes,WithAttributes)],Calls,_),
	listToTuple(Calls,Body).


genACall(T,A,V,Type,I,Object,As,(member((A,V),As)->P;C,P)):-
	C=..[A,(Type,I,Object),(A,V)],
	(functDependency(T,(A,_)) ->
		P=assign1(self(A),V)
	;	P=true
	),
	!.




genNew2:-
	relationTypes(Ts),
	map(genNew,Ts,News),
	objPath(ObjPath),
	mkAtom('%/%',[ObjPath,news],N),
	tell(N),
	mapA(portray_clause,News),
	told,
	rg_env:rg_compile(user:N).

/*----------------------------------------------------------------------*
 *		 new(Type,Object,I,WithAttributes,Ref,Kind)		*
 *----------------------------------------------------------------------*/


new(Type,Object,I,WithAttributes,Ref,Kind) :-
	(new2(Type,Object,I,WithAttributes,AllAttributes) ->
		recordFact(Type,(I,Object,AllAttributes),RefNew)
	;
		sPrint(" *** new: evaluation of attributes for object type % failed!",[Type]),
		trace
	),
	(	subsumed(Type,I,Object,AllAttributes,IOld),
		Kind=old(IOld),
		dispose(Type,RefNew),
		i(Type,_,IOld,Ref)
	;
		Ref=RefNew,
		Kind=new,
		try(accTime(afterCreation(Type),afterCreation(Type,I)))
	),
	!.


/*----------------------------------------------------------------------*
 *		 	recordFacta(Type,(I,O,A),Ref)			*
 *		 	recordFactz(Type,(I,O,A),Ref)			*
 *		 	recordFact(Type,(I,O,A),Ref)			*
 *----------------------------------------------------------------------*/

recordFact(Type,(I,O,A),Ref) :-
	Clause=..[Type,I,O],
	mapP(lambda([(N,V)],
		[AFact,Refs]^
		(
			(	clauseAttribute(Type,N) ->
				AFact=..[N,I,Type,(V,Refs)],
				(	N=reduceClause,O=([],_) ->
					map(asserta,V,Refs)
			% this causes unconditional rules to be tried first
				;	map(assertz,V,Refs)
				)
			;
				AFact=..[N,I,Type,V]
			),
			asserta(AFact)
		)),A),
	asserta(Clause,Ref),
	!.



/*----------------------------------------------------------------------*
 *		 	assertUnique((H:-B),Ref)			*
 *----------------------------------------------------------------------*/

assertUnique((H:-B),Ref) :-
	fromProlog((H1,B1),[],(H,B)),  % to avoid occur check problems
	clause(H1,B1,Ref),
	!.
assertUnique(C,Ref) :-
	assert(C,Ref).

	
/*----------------------------------------------------------------------*
 *		   recordObject(Type,Object,AllAttributes,I)		*
 *----------------------------------------------------------------------*/

% wird nicht mehr gebraucht:
recordObject(Type,Object,AllAttributes,I) :-
	cont(allInstancesOf(Type),Instances),
	(lessPred(Type,Object,AllAttributes,LessPred) ->
		insert(Instances,LessPred,I,NewInstances),
		allInstancesOf(Type):=NewInstances
	;
		sPrint(" *** recordObject: evaluation of sort criterium for object % failed",[I]),
		trace
	),
	!.


/*----------------------------------------------------------------------*
 *		   		newIndex(Type,I)			*
 *----------------------------------------------------------------------*/

newIndex(Type,I) :-
	inc(ctr(Type)),
	cont(ctr(Type),I),
	!.


/*----------------------------------------------------------------------*
 *		   insert([Ref|Ys],LessPred,Ref1,[Ref|Ys1])		*
 *----------------------------------------------------------------------*/

insert([Ref|Ys],LessPred,Ref1,[Ref|Ys1]) :-
	apply(LessPred,[Ref]),
	!,
	insert(Ys,LessPred,Ref1,Ys1),
	!.
insert([X|Xs],_,X,[X|Xs]) :- !.
insert([Ref|Ys],_,Ref1,[Ref1,Ref|Ys]) :- !.
insert([],_,X,[X]) :- !.


/*----------------------------------------------------------------------*
 *		   	i(Type,Object,Index,Ref)			*
 *		   i(Type,Object,WithAttributes,Index,Ref)		*
 *----------------------------------------------------------------------*/

i(Type,Object,Index,Ref) :-
	Fact=..[Type,Index,Object],
	clause(Fact,true,Ref).


i(Type,Object,WithAttributes,Index,Ref) :-
	Fact=..[Type,Index,Object],
	clause(Fact,true,Ref),
	hasAttributes(Type,WithAttributes,Index).


/*----------------------------------------------------------------------*
 *		   	  setOfObjects(T,O)				*
 *----------------------------------------------------------------------*/

setOfObjects(T,O) :-
	Goal=..[T,_,Ob],
	setof1((R,Ob),clause(Goal,true,R),O),
	!.



object(I,Type,O):-
	Obj=..[Type,I,O],
	Obj.



/*----------------------------------------------------------------------*
 *	     hasAttributes(T,[(A,V)|RequestedAttributes],Index)		*
 *----------------------------------------------------------------------*/

hasAttributes(_,[],_) :-
	!.
hasAttributes(T,[(A,V)|RequestedAttributes],Index) :-
	accessAttribute(T,(A,V),Index),
	hasAttributes(T,RequestedAttributes,Index),
	!.


/*----------------------------------------------------------------------*
 *	     		accessAttribute(T,(A,V),Index)			*
 *----------------------------------------------------------------------*/

accessAttribute(T,(A,V),Index) :-
	(clauseAttribute(T,A) ->
		AFact=..[A,Index,T,(V,_)]
	;
		AFact=..[A,Index,T,V]
	),
	AFact,
	!.


/*----------------------------------------------------------------------*
 *	     			a(Ref,A,V)				*
 *				a(T,I,A,V)				*
 *----------------------------------------------------------------------*/

a(Ref,A,V) :-
	clause(Fact,_,Ref),
	functor(Fact,Type,_),
	arg(1,Fact,I),
	a(Type,I,A,V),
	!.


a(T,I,A,V) :-
	accessAttribute(T,(A,V),I),
	!.
a(T,I,A,_) :-
	nl,
	sPrint(" *** Error :  Object with index % of type % does not have an attribute %",[I,T,A]),
	nl,
	trace.

	
/*----------------------------------------------------------------------*
 *	     		     seta(Type,R,N,V)				*
 *----------------------------------------------------------------------*/

seta(Type,R,N,V) :-
	seta(user,Type,R,N,V).

seta(W,Type,R,N,V):-
	(number(R) ->
		I=R
	;
		W:clause(Fact,_,R),
		Fact=..[Type,I|_]
	),
	(clauseAttribute(Type,N),W=user ->
		NewFact=..[N,I,Type,(V,Refs)],
		OldFact=..[N,I,Type,(_,OldRefs)],
		('$rule'(I,([_|_],_)) ->
			map(assertz,V,Refs)
		;	map(asserta,V,Refs)
		)
	;
		OldFact=..[N,I,Type,_],
		NewFact=..[N,I,Type,V],
		OldRefs=[]
	),
	(retract(W:OldFact) ->
		mapA(erase,OldRefs)
	;
		nl,
		sPrint(" *** Error :  Object with index % of type % does not have an attribute %",[I,Type,N]),
		nl,
		trace
	),
	assert(W:NewFact),
	!.


/*----------------------------------------------------------------------*
 *	     		     dispose(Type,Id)				*
 *	     		   dispose(Type,Ref,I)				*
 *	     		  disposeDependants(T,I)			*
 *	     		  deleteDep(object(I,T))			*
 *----------------------------------------------------------------------*/





dispose(Type,Id) :-
	dispose(user,Type,Id).

dispose(W,Type,Id) :-
	(number(Id) ->
		I=Id
	;
		Ref=Id
	),
	dispose1(W,Type,Ref,I),
	eraseAttributes(W,Type,I),
	disposeDependants(W,Type,I),
	try(afterDeletion(W,Type,I)),
	!.


dispose1(W,Type,Ref,I) :-
	Fact=..[Type,I,_],
	W:clause(Fact,true,Ref),
	erase(Ref),
	!.


disposeDependants(W,T,I) :-
	dependent(W,T,I,O),
	deleteDep(W,O),
	fail.
disposeDependants(_W,_T,_I).


deleteDep(W,object(I,T)) :-
	!,
	explain(depDel,['$oref'(T,I)]),
	dispose(W,T,I).
deleteDep(W,Fact) :-
	retract(W:Fact).


/*----------------------------------------------------------------------*
 *	     		        dbInit					*
 *----------------------------------------------------------------------*/

dbInit :-
	eraseClauseAttributes,
	relationTypes(Ts),
	member(T,Ts),
	abolish(T,2),
	fail.
dbInit:-
	attribute(A,_),
	abolish(A,3),
	fail.
dbInit.
	
	


/*----------------------------------------------------------------------*
 *	     		  eraseRecordedTerms(T)				*
 *----------------------------------------------------------------------*/
/* dead Code ?
eraseRecordedTerms(T) :-
	Clause=..[T,I,_],
	clause(Clause,_,Ref),
	eraseAttributes(T,I),
	erase(Ref),
	fail.
eraseRecordedTerms(_).
*/

/*----------------------------------------------------------------------*
 *	     		  eraseAttributes(T,Index)			*
 *----------------------------------------------------------------------*/

eraseAttributes(W,T,Index) :-
	attribute(A,T),
	Attr=..[A,Index,T,V],
	retract(W:Attr),
	(clauseAttribute(T,A),W=user ->
		V=(_,ClauseRefs),
		mapA(erase,ClauseRefs)
	),
	fail.
eraseAttributes(_,_,_).


/*----------------------------------------------------------------------*
 *	     		  eraseClauseAttributes				*
 *----------------------------------------------------------------------*/

eraseClauseAttributes :-
	clauseAttribute(T,A),
	Fact=..[A,_,T,(_,ClauseRefs)],
	Fact,
	mapA(erase,ClauseRefs),
	fail.
eraseClauseAttributes:-
	retract(needsToBeErased(ClauseRefs)),
	mapA(erase,ClauseRefs).
eraseClauseAttributes.


	
/*----------------------------------------------------------------------*
 *	     		  assertClauseAttributes			*
 *----------------------------------------------------------------------*/

assertClauseAttributes :-
	clauseAttributeName(A),
	setof1((A,I,T,Cls),
		OldRefs^Att^(	Att=..[A,I,T,(Cls,OldRefs)],
				Att
		),Clauses),
	abolish(A/3),
	mapA(assertAttrClauses,Clauses),
	fail.
assertClauseAttributes.
	

assertAttrClauses((A,I,T,Cls)):-
	map(assertz,Cls,NewRefs),
	Fact=..[A,I,T,(Cls,NewRefs)],
	assertz(Fact),
	!.


clauseAttributeName(B):-
	setof1(A,T^clauseAttribute(T,A),As),
	member(B,As).



/*----------------------------------------------------------------------*
 *	     		      attribute(A,T)				*
 *----------------------------------------------------------------------*/
	
attribute(A,T) :-
	attributes(T,As),
	member(A,As).

