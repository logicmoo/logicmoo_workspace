/*
 *	file:		action.pl
 *	version:	1.0
 *	date:		October 17, 1989
 *	creation:	October 17, 1989
 *	author:		js
 *
 *	description:
 *	This file contains predicates related to actions. Actions
 *	are decisions which must be made during completion.
 *	They can be read in from a log file created during a previous
 *	completion, some can be generated automatically and the remaining
 *	actions are queried from the user.
 *
 *	history:
 *	891710	js	Created file
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */


/* fetchAction(Action,Kind,+Eq,EqI,-Reverse,AllowedActions)
 * Action:	    term with variables describing desired action,
 *		    i.e. orient(A)
 * Kind:	    some additional information for actionDefault/4
 * Eq:		    the conditional equation we are working on
 * EqI:		    its equation index
 * Reverse:	    yes:action is intended for reversed equation
 * AllowedActions:  list of choices for action
 * Fetches an action or defaultAction for the equation.
 */

fetchAction(Action,_Kind,'$equation'(C,[L=R]),_EqNr,Reverse,AllowedActions) :-
	(var(C) -> C1 = C ; true),
	(	nonvar(C),nonvar(L),nonvar(R),
	 	act(Action,'$equation'(C,[L=R]),Body),
		Body,
		allowedAction(Action,AllowedActions)
	->	Reverse=no
	;	action(Action,'$equation'(C1,[L1=R1])),
		action(Action,'$equation'(C1,[L1=R1])),
		allowedAction(Action,AllowedActions),
		(var(C1) -> C2 = [] ; C2 = C1),
		(	equalUpToVarRenaming(C=>L=R,C2=>L1=R1),
			Reverse = no
		;
			equalUpToVarRenaming(C=>L=R,C2=>R1=L1),
			Reverse = yes
		),
		retract(action(Action,'$equation'(C1,[L1=R1]))),
		assertz(actionNew(Action,'$equation'(C1,[L1=R1])))),
	!.

fetchAction(Action,Kind,Eq,EqI,Reverse,AllowedActions) :-
	autoEnabled(Action),
	autoAction(Action,Kind,Eq,EqI,Reverse,AllowedActions),
	allowedAction(Action,AllowedActions),
	assertz(actionNew(Action,Eq)),
	!.

fetchAction(Action,Kind,'$equation'(C,[L=R]),EqNr,Reverse,AllowedActions) :-
	(	actionDefault(Action,Kind,'$equation'(C,[L=R]),EqNr),
		Reverse = no
	;
		actionDefault(Action,Kind,'$equation'(C,[R=L]),EqNr),
		Reverse = yes
	),
	allowedAction(Action,AllowedActions),
	!.


fetchNewAction(Action,Kind,'$equation'(C,[L=R]),EqNr,Reverse,AllowedActions) :-
	fetchAction(Action,Kind,'$equation'(C,[L=R]),EqNr,Reverse,AllowedActions),
	!.
fetchNewAction(Action,_Kind,'$equation'(C,[L=R]),_EqNr,Reverse,AllowedActions) :-
	(var(C) -> C1 = C ; true),
	actionNew(Action,'$equation'(C1,[L1=R1])),
	allowedAction(Action,AllowedActions),
	Action \== orient(p), % changed uh 12.02.90 ignore new postpone actions
	(var(C1) -> C1 = [] ; true),
	(	equalUpToVarRenaming(C=>L=R,C1=>L1=R1),
		Reverse = no
	;
		equalUpToVarRenaming(C=>L=R,C1=>R1=L1),
		Reverse = yes
	),
	!.


/* allowedAction(Action,+AllowedActions)
 * succeeds if Action is allowed
 */

allowedAction(Action,AllowedActions) :-
	Action =.. [_Op,Param],
	!,
	allowedAct(Param,AllowedActions).
allowedAction(Action,AllowedActions) :-
	Action =.. [_Op|ParamList],
	allowedAct(ParamList,AllowedActions).


allowedAct(Params,AllowedActions) :-
	(	var(AllowedActions)
	;
		var(Params)
	),
	!.
allowedAct(Params,AllowedActions '|' _AllowedActionsList) :-
	allowedAct(Params,AllowedActions),
	!.
allowedAct(Params,_AllowedActions '|' AllowedActionsList) :-
	allowedAct(Params,AllowedActionsList),
	!.
allowedAct([],[]) :- !.
allowedAct([Param | ParamList], [AllowedAction | AllowedActions]) :-
	allowedAct(Param,AllowedAction),
	!,
	allowedAct(ParamList,AllowedActions).
allowedAct(Param,AllowedActionList) :-
	member(Param,AllowedActionList),
	!.
allowedAct(Param,Param) :-
	!.

/*----------------------------------------------------------------------*/
/* automatic actions */


/* user interface, enabling and disabling */

/* enableAuto
 * enables all automatic actions.
 */

enableAuto :-
    assert(automatic(all)).


/* enableAuto(+ActionType)
 * enables automatic actions for one type of actions
 * ActionType: Atom, one of status, selectcondition, orient
 *             or annotation.
 */

enableAuto(AT) :-
    assert(automatic(AT)).


/* disableAuto
 * disables all automatic actions.
 */

disableAuto :-
    retractall(automatic(_)).


/* disableAuto(+ActionType)
 * disables automatic actions for one type of actions
 * ActionType: Atom, one of status, selectcondition, orient
 *             or annotation.
 */

disableAuto(AT) :-
    retractall(automatic(AT)).


/* autoEnabled(+Action)
 * Action: term with variables
 */

autoEnabled(_Action) :-
    automatic(all),
    !.
autoEnabled(Action) :-
    functor(Action,F,_N),
    automatic(F).



/* autoAction(Action,Kind,Eq,EqI,Reverse,AllowedActions)
 * can generate some automatic actions for completion. This
 * is done on a best guess basis.
 * The main difference between autoAction and actionDefault
 * is that actionDefault is fast and simple, and autoAction
 * is not. This is also the reason for recording actions
 * generated by autoAction.
 */

autoAction(status(ms),_,'$equation'(C,[_Eq]),_,_,AllowedActions) :-
	C=[],
	allowedAction(status(ms),AllowedActions),
	!.
autoAction(status(LR),_,'$equation'(C,[Eq]),_,_,_AllowedActions) :-
	similarLiteralsIn([Eq|C]),
	length([Eq|C],Len),
	listOfNumbers(Len,0,LR),
	!.
autoAction(status(ms),_,'$equation'(_C,[_Eq]),_,_,_AllowedActions) :-
	!.

autoAction(selectcondition(N),_,'$equation'(C,[_Eq]),_,_,_AllowedActions) :-
	literalForSuperposition(C,N),
	!.

autoAction(orient(n),_,'$equation'([_|_],[_Eq]),_,_,AllowedActions) :-
	allowedAction(orient(n),AllowedActions),
	!.

/* similarLiteralsIn(+ListOfEquations)
 * succeeds if there are two literals in L, which differ only in
 * variable names, orientation or negation, i.e. true on one side
 * of the first and false on one side of the second equation.
 */

similarLiteralsIn(L) :-
	twoMembers(Lit1,Lit2,L),
	similarLiteral(Lit1,Lit2).

similarLiteral(L1=R1,L2=R2) :-
	similar(L1,L2),
	similar(R1,R2),
	!.
similarLiteral(L1=R1,L2=R2) :-
	similar(L1,R2),
	similar(R1,L2).

similar('true-bool','false-bool').
similar('false-bool','true-bool').
similar(T1,T2) :-
	equalUpToVarRenaming(T1,T2).


/* literalForSuperposition(+Cond,-N)
 * selects a literal for superposition of nonoperational equations
 */

literalForSuperposition(C,N) :-
	map(fitnessForSuperposition,C,CF),
	maxOfList(CF,_,N).


/* fitnessForSuperposition(+Lit,-Fitness)
 * calculates a value for Lit by some rule of thumb. The higher the
 * value is, the better Lit is suited for superposition.
 */

fitnessForSuperposition(@_V,-1) :-
	!.
fitnessForSuperposition(T,F) :-
	functor(T,Func,N),
	T =.. [_|Args],
	functorFitness(Func,N,FF),
	map(fitnessForSuperposition,Args,AFL),
	rReduce(plus,0,AFL,AF),
	F is FF+AF.

functorFitness(=,2,0) :-
	!.
functorFitness(Op,_N,Fit) :-
	(
	    (ac_ist_A(Op) ; ac_ist_C(Op)) -> Fit = -128
	;
	    injection(Op) -> Fit = 1
	;
	    hhConstructor(Op) -> Fit = 16
	;
	    Fit = 4
	).


/*----------------------------------------------------------------------*/

/* maybeReductive(+Eq)
 * necessary condition, checks if Eq could become (quasi-)reductive.
 * Assume that the equation is oriented left to right.
 * Then there must be at least one side U of a condition such that
 * L is possibly greater than U, so U will become the first term of the
 * condition for a quasireductive equation.
 * All other terms in the condition and the other side of the conclusion
 * may not be bigger than L. This is a weaker condition, since not all
 * variables of L need to appear in these terms. (->absolutelyNonreductive).
 */

%maybeReductive(EqI,[]=>L=R) :-
%	!.
%maybeReductive(EqI,C=>L=R) :-
%	\+ absolutelyNonreductive(EqI,EqT,Eq),
%	true. %*x* noch nicht fertig


/* maybeGreater(+S,+T)
 * checks if S could possibly be greater than T in the current
 * termination ordering
 */

maybeGreater(S,T) :-
	vars(S,VS),
	vars(T,VT),
	subset(VS,VT),	
	\+ greaterInRedOrderNoExt(T,S),
	\+ partialTerm(T,S).


/* partialTerm(+S,?T)
 * checks if T can be obtained from S by leaving out some operators.
 * If this is true, then s>t in any simplification ordering follows.
 */

partialTerm(S,T) :-
	functor(S,F,N),
	functor(T,F,N),
	S =.. [F|SArgs],
	T =.. [F|TArgs],
	map(partialTerm,SArgs,TArgs).
partialTerm(S,T) :-
	S =.. [_F|Args],
	member(A,Args),
	partialTerm(A,T).

/*----------------------------------------------------------------------*/

listActions :-
	nl,
	action(A,E),
	sPrint("action(%,%).",[A,E]),
	nl,
	fail.
listActions :-
	nl,
	actionNew(A,E),
	sPrint("actionNew(%,%).",[A,E]),
	nl,
	fail.
listActions.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

storeLog:-
	storeActionsAndOrder.

storeLog(To):-
	storeActionsAndOrder(To).

storeLog(To,OrderName) :-
	fileNameExt(To,OrderName,File),
	storeActionsAndOrder(File).


storeActionsAndOrder :-
	cont(moduleName,M),
	M \== '$noName',
	!,
	cont(orderName,OrderName),
	(OrderName == noorder ->
		tps_current_ordering(Order),
		transformed_Order(ExternalOrd,Order),
		fileNameExt(M,ExternalOrd,File)
	;
		fileNameExt(M,OrderName,File)
	),
	storeActionsAndOrder(File).

storeActionsAndOrder :-
	error("no name associated with current specification",[],storeActionsAndOrder),
	fail.


storeActionsAndOrder(To) :-
	nonvar(To) ->
		fileNameExt(To,'@',F),
		fileNameExt(F,ord,File),
		tell(File),
		try(storeOrdering),
		nl,
		storeAction(hhConstructor(O),ofType(_,O:_),constructor(O)),
		storeAction(forwardChainingDepth(D)),
		storeAction(resolutionDepth(D)),
		storeAction(allowedEliminationTime(A)),
		storeAction(clause(actionDefault(A1,A2,E,I)),true,
			    actionDefault(A1,A2,E,I)),
		storeAction(clause(actionNew(A,E),_),
			    printCommentForAction(A,E),
			    action(A,E)),
		told,
		write('Actions and ordering written to '),
		write(File).


	


storeAction(Clause) :-
	storeAction(Clause,true,Clause).

storeAction(Clause,Condition) :-
	storeAction(Clause,Condition,Clause).

storeAction(Clause1,Condition,Clause2) :-
	(	Clause1,
		Condition,
		writeq(Clause2),
		write('.'),
		nl,
		fail
	;
		nl,
		true
	).


printCommentForAction(orient(l),E) :-
	nl,
	sPrint("/* orient from left to right :
/*	% */",[E]),
	nl,
	!.
printCommentForAction(orient(r),E) :-
	nl,
	sPrint("/* orient from right to left :
/*	% */",[E]),
	nl,
	!.
printCommentForAction(orient(n),E) :-
	nl,
	sPrint("/* declare as nonoperational :
/*	% */",[E]),
	nl,
	!.
printCommentForAction(status(Status),E) :-
	nl,
	sPrint("/* ordering of the literals is % for
/*	% */",[Status,E]),
	nl,
	!.
printCommentForAction(selectcondition(I),E) :-
	nl,
	sPrint("/* condition number % is the selected condition for
/*	% */",[I,E]),
	nl,
	!.
printCommentForAction(orient(c),E) :-
	nl,
	sPrint("/* %
/* should be checked for quasi-reductivity */",[E]),
	nl,
	!.
printCommentForAction(annotation(Order),E) :-
	nl,
	sPrint("/* annotation of literals is % in
/*	% */",[Order,E]),
	nl,
	!.
printCommentForAction(orient(p),E) :-
	nl,
	sPrint("/* %
   should be postponed, */",[E]),
	nl,
	!.
printCommentForAction(orient(o),E) :-
	nl,
	sPrint("/* try to orient into a (quasi-)reductive rule :
/*	% */",[E]),
	nl,
	!.
printCommentForAction(orient(a),E) :-
	nl,
	sPrint("/* The left side is assumed to be greater than the right side :
/*	% */",[E]),
	nl,
	!.
printCommentForAction(_,E) :-
	nl,
	sPrint("/* Action for
/*	% */",[E]),
	nl,
	!.


	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loadLog:-
	loadActionsAndOrder.

loadLog(To):-
	loadActionsAndOrder(To).

loadLog(To,OrderName) :-
	fileNameExt(To,OrderName,File),
	loadActionsAndOrder(File).

loadActionsAndOrder :-
	cont(moduleName,M),
	M \== '$noName',
	!,
	cont(orderName,OrderName),
	(OrderName == noorder ->
		tps_current_ordering(Order),
		transformed_Order(ExternalOrd,Order),
		fileNameExt(M,ExternalOrd,File)
	;
		fileNameExt(M,OrderName,File)
	),
	fileNameExt(M,OrderName,File),
	loadActionsAndOrder(File).
loadActionsAndOrder :-
	error("no name associated with current specification",[],loadActionsAndOrder),
	fail.


loadActionsAndOrder(From) :-
	nonvar(From) ->
		fileNameExt(From,'@',F),
		fileNameExt(F,ord,File),
		baseName(File,BaseName),
		sPrint("[loading % ...]",[BaseName]),
		nl,
                tmpFileName(TF),
		tell(TF),
		undoUponFail((abolish(action/2),
			      abolish(actionDefault/4),
			      abolish(forwardChainingDepth/1),
			      abolish(resolutionDepth/1),
			      abolish(allowedEliminationTime/1),
			      enrich('',File,noorder,Error)),
			     Error),
		told,
                concAtomNames('rm ',TF,Cmd),
		unix(shell(Cmd)).


% ************** defaults ***********


default(smallTerm,300).
default(smallClause,600).