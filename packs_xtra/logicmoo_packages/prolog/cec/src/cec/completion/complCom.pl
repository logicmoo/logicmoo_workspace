/*
 *	file:		complCom.pl
 *	version:	1.5
 *	date:		November 6, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates for
 *	- setting up completion parameters
 *	- completion of the current specification
 *	- completion of a specification using a action file
 *	- completion of all specifications in the current directory
 *	- completion of all specifications listed in a file named 
 *	  "moduleHierarchy" which describes a hierarchy of modules
 *
 *	history:
 *	891106	uh 	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */


make :- 
	completeHierarchy.

completeAll:-
	currentPath(PA),
	listOfFiles(PA,'*','.@.ord',A),
	time(mapP(complete,A)).

completeHierarchy :-
	currentPath(PA),
	concAtomNames(PA,'/moduleHierarchy',HF),
	see(HF),
	read(H),
	seen,
	time(mapP(complete,H)).

complete(ActionFile):-
	name('.',[Dot]),
	name(ActionFile,NameString),
	append(ModuleString,[Dot|OrdString],NameString),
	!,
	name(Module,ModuleString),
	name(O,OrdString),
	complete(Module,O).


complete(Module,Order):-
	mkAtom('%.@',[Order],O),
	sPrint("


****************** % with % ******************

",[Module,O]),
	in(Module,O),
	'c!',
	!.


completeN(Module,O):-
	sPrint("


****************** % with % ******************

",[Module,O]),
	in(Module,O),
	'c!',
	nl,
	listing(timeUsedFor),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do :-			% js 910419
	in,
	c,
	prove.

do(Mod) :-		% js 910419
	in(Mod),
	c,
	prove.

do(Mod,Ord) :-		% js 910419
	in(Mod,Ord),
	c,
	prove.

c :-
	pushState(undo),
	cNoFreezeNoUndo,
	!.

cf:-
	c,
	try(freeze).

% 14.11.90
% inserted 
%	repeat(toNormalEq(_))
% which will turns all postponed equations into equations 
% considered by orderEq.
cNoFreezeNoUndo:-
	setComplParameter,
	abolish(critPair,1),
	abolish(forwardChainedCondition,1),
	initTime,
	nmbovs:=0,
	time((
		repeatRed('$rule'),
		repeat(toNormalEq(_)),
		repeat(orderEq(_)),
		kbLoop
	)).

toNormalEq(EqI) :-
	retract(priority(EqI,'$equation',postponed)),
	assertz(priority(EqI,'$equation',normal)).


cResume :-
        (cont(nmbovs,_) ->
	    time(kbLoop)
	;
	    error("Completion not started",[],cResume)
	),
	freeze.


'c!' :-
	setComplParameter,
	abolish(critPair,1),
	initTime,
	nmbovs:=0,
	time((
		repeatRed('$rule'),
		repeat(toNormalEq(_)),
		repeat(orderEq(_)),
		kbLoop
	)),
	freeze.

csave :-
	pushState(undo),
	abolish(critPair,1),
	initTime,
	nmbovs:=0,
	time((
		repeat(orderEq(_)),
		ss('$oriented'),
		kbLoop
	)),
	freeze.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initComplParams :-
	abolish(forwardChainingDepth/1),
	abolish(resolutionDepth/1),
	abolish(allowedEliminationTime/1),
	cParameters := no.

setComplParameter :-
	\+ cont(cParameters,set),
	!,
	abolish(actionDefault/4),
	/* declare all equations as nonoperational that are absolutely */
        /* nonreductive and not supplied by the user		       */
	assertz((actionDefault(orient(n),tryRed,'$equation'([_],_),I) :-
		 absolutelyNonreductive(I,'$equation',[r,l]),
		 origin(I,'$equation',K),
		 K \== user
		)),
	assertz((actionDefault(orient(n),tryRed,'$equation'([AA=Any],[BB=Any]),I) :-
		\+greaterInRedOrderNoExt(BB,AA),
		 origin(I,'$equation',K),
		 K \== user
		)),
	/* ask for all equations that are not supplied by the user */
	assertz((actionDefault(orient,ask,'$equation'([_E|_C],[L=_R]),I) :-
			notOrientedByDefault(I,L))),
	assertz((actionDefault(orient(n),tryRed,'$equation'(_,[Eq]),I) :-
			domainEquation(Eq,S,T),
			put_away_injections(T,T1),
			nonVar(T1),
			termType(T1,S1),
			parameterSort(S1))),
	assertz((actionDefault(orient(A),tryRed,'$equation'(_,[Eq]),I) :-
			domainEquation(Eq,S,T),
			put_away_injections(T,T1),
			nonVar(T1),
			T1=..[Op|Vs],
			generatingMsOps(S,Ops),
			(member(Op,Ops),mapP(lambda([V],X^(V= @X)),Vs) ->
				A=o
			;
				A=n
			))),
	assertz((actionDefault(orient(A),tryRed,'$equation'(Cs,[Eq]),I) :-
			domainEquation(Eq,S,T),
			put_away_injections(T,@V),
			(Cs=[]->
				A=o
			;
				A=n
			))),
	assertz((actionDefault(orient(n),tryRed,'$equation'([_|_],[_=_]),I) :-
			       cont1(indProve,true))),
	assertz((actionDefault(orient(n),tryRed,'$equation'([_|_],[_=_]),I) :-
			       cont1(prove,true))),
	assertz((actionDefault(orient(_),tryRed,'$equation'([],[L=R]),I) :-
			       cont1(indProve,true),
			       !,
			       error("Failure of inductive proof.
The new identity
	%
has been derived from equation %.

",['$equation'([],[L=R]),I],indProve),
			       abortFromCompletion)),
	assertz((actionDefault(orient(n),lrOrRl,'$equation'(_,[_=_]),I) :-
			       cont(lemma,true(_)))).
setComplParameter.




notOrientedByDefault(I,L):-
       origin(I,'$equation',K),
       (    K = super(_,_,_,_,_,_)
       ;    K = redRule(_)
       ;    K = unknown
       ;    K = domainAxiom,
	    arg(1,L,L1),
	    functor(L1,O,_NO),
	    \+hhConstructor(O)).



setCompletionParameter :-
	abolish(actionDefault/4),
	prompt1(['Orienting of equations:','
Try to orient automatically',
'    a:  all equations',
'    u:  all equations given by the user (default)',
'    uu: all unconditional equations given by the user',
'    gu: all generated unconditional equations (default)',
'    uc: all conditional equations given by the user',
'    gc: all generated conditional equations',
'    n:  no equations'],
		['a list of [a,u,uu,gu,uc,gc,n]'],
		(listOf([a,u,uu,gu,uc,gc,n],A);member(A,[a,u,uu,gu,uc,gc,n])),A),
	((member(a,A) ; A = a) ->
		true
	;((member(n,A) ; A = n) ->
		assertz((actionDefault(orient,ask,
				       '$equation'(_C,[_L=_R]),_I)))
	;	((member(u,A) ; A = u)->
			true
		;
			((member(uu,A) ; A = uu) ->
				true
			;
				assertz((actionDefault(orient,ask,
						       '$equation'([],[_L=_R]),I) :-
					 origin(I,'$equation',K),
					 K = user))
			),
			((member(uc,A) ; A = uc) ->
				true
			;
				assertz((actionDefault(orient,ask,
						       '$equation'([_E|_C],[_L=_R]),I) :-
					 origin(I,'$equation',K),
					 K = user))
			)
		),
		((member(gu,A) ; A = gu) ->
			true
		;
			assertz((actionDefault(orient,ask,
					       '$equation'([],[_L=_R]),I) :-
				 origin(I,'$equation',K),
				 (  K = super(_,_,_,_,_,_)
				 ;  K = redRule(_)
				 ;  K = unknown)))
		),
		((member(gc,A) ; A = gc) ->
			true
		;
			assertz((actionDefault(orient,ask,
					       '$equation'([_E|_C],[_L=_R]),I) :-
				 origin(I,'$equation',K),
		    		 (  K = super(_,_,_,_,_,_)
				 ;  K = redRule(_)
				 ;  K = unknown)))
		)
	)),
	prompt1(['Checking Quasi-Reductivity of equations:',
'    c: check all nonreductive conditional equations for quasi-reductivity',
'    n: do not check quasi-reductivity automatically (default)'],
	       [c,n],A1),
	(A1 = c ->
		assertz((actionDefault(orient(c),tryQuasi,
				       '$equation'([_E|_C],[_L=_R]),_I)))
	;
		true
	),
	prompt1(['Postponing of equations:',
'    p: postpone all nonreductive equations',
'    n: do not postpone equations automatically (default)'],
		[p,n],A2),
	(A2 = p ->
		assertz((actionDefault(orient(p),lrOrRl,
				       '$equation'(_C,[_L=_R]),_I)))
	;
		true
	),
	prompt1(['Declaring equations as nonoperational:',
'    d: declare all absolutely nonreductive equations with one condition
       as nonoperational (default)',
'    n: do not declare any equation as nonoperational automatically'],
		[d,n],A3),
	(A3 = d ->
		assertz((actionDefault(orient(n),tryRed,
				       '$equation'([_],_),I) :-
			 absolutelyNonreductive(I,'$equation',[r,l]),
			 origin(I,'$equation',K),
			 K \== user
		))
	;	true
	),
	prompt1(['Depth of forward chaining:',
'     d : default value',
'    <N>: <natural number>'],
		[d,'<N>'],(A4 == d ; number(A4)),A4),
	try(retract(forwardChainingDepth(_))),
	(A4 == d ->
		true
	;	assertz(forwardChainingDepth(A4))
	),
	prompt1(['Depth of resolution:',
'     d : default value',
'    <N>: <natural number>'],
		[d,'<N>'],(A5 == d ; number(A5)),A5),
	try(retract(resolutionDepth(_))),
	(A5 == d ->
		true
	;	assertz(resolutionDepth(A5))
	),
	prompt1(['Allowed elimination time:',
'     d : default value',
'    <N>: <natural number>'],
		[d,'<N>'],(A6 == d ; number(A6)),A6),
	try(retract(allowedEliminationTime(_))),
	(A6 == d ->
		true
	;	assertz(allowedEliminationTime(A6))
	),
	cParameters := set.
