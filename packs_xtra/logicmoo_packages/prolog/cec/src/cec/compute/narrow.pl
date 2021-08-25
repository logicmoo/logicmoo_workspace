/*
 *	file:		narrow3.pl
 *	version:	1.5
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains the predicates defining narrowing.
 *
 *	history:
 *	891010	js	Added this comment
 *	891031  uh	Moved definitions of 
 *			solve/1		solve/2
 *			from completion/pe.pl into this file
 *	891206	uh	Changed definition of 
 *			decompose/2
 *			If the two terms of an term have different 
 *			constructors as principal functors, narrowing
 *			will fail 
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */


timer(Call) :-
   totalRuntime(A),
   Call,
   totalRuntime(E),
   U is E-A,
   nl,write('Time used: '), write(U), write(' sec.'),nl.

	
performance :-
	retractAll(cont1(jobPerf(_),_)),
	waitingNodes:==0,
	activeNodes:==0,
	cont(calls,CN),
	countNodes(CN),
	setof(J,cont1(jobPerf(J),_),L),
	length(L,TotalJobs),
	cont1(waitingNodes,Waiting),
	cont1(activeNodes,Active),
   	sPrint("The total number of          jobs is %.",['$dec'(TotalJobs,5,' ')]), nl, !,
   	sPrint("The total number of waiting nodes is %.",['$dec'(Waiting,5,' ')]), nl, !,
   	sPrint("The total number of working nodes is %.", ['$dec'(Active,5,' ')]),nl,nl,  !,
	sPrint("Job statistic:",[]), nl,
	jobStatistic,
	!.

jobStatistic :-
	cont1(jobPerf(SubProc),(W,A,L)),
	sPrint("Job: %   Waiting nodes: %   Working nodes: %   Maximal Layer: %",
	       ['$dec'(SubProc,3,' '),'$dec'(W,5,' '),'$dec'(A,5,' '),'$dec'(L,2,' ')]),
	nl,
	fail.
jobStatistic :- !.


jobPerf(SubProc,PlusWaiting,PlusActive,LayerN) :-
	(cont1(jobPerf(SubProc),(Wait,Active,LayerO)) ->
		(LayerN > LayerO ->
			W is Wait+PlusWaiting,
			A is Active+PlusActive,
			jobPerf(SubProc):==(W,A,LayerN)
		;
			W is Wait+PlusWaiting,
			A is Active+PlusActive,
			jobPerf(SubProc):==(W,A,LayerO)
		)
	;
		(jobPerf(SubProc):==(PlusWaiting,PlusActive,LayerN)
		)
	),
	!.

countNodes(CN) :-
	narrterm(awaiting(job(CN,SubProc1)),_,job(CN,_SubProc2),stufe(L),_,_,unif(_)),
	cont1(waitingNodes,N),
	M is N+1,
	waitingNodes:==M,
	jobPerf(SubProc1,1,0,L),
	fail.
countNodes(CN) :-
	narrterm(job(CN,SubProc),stufe(L),_,_,unif(_)),
	cont1(activeNodes,N),
	M is N+1,
	activeNodes:==M,
	jobPerf(SubProc,0,1,L),
	fail.
countNodes(_) :- 
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% for solving goals (-> narrowing)

solve(A) :-
	narrow(A,C),
	C = '$solution'(B),
	sPrint("Unifier: %",['$osSolution'(B)]),
	nl,
	sPrint("Another solution ? ",[]),
	read(Answer),
	Answer = 'n'.
solve(A,B) :-
	narrow(A,B).



/*----------------------------------------------------------------------*/
/* narrow(+ConditionalEquation,-Solution)                               */
/* computes an answer substitution Solution for ConditionalEquation     */
/* Backtracking gives the set of all such answer substitutions          */
/*----------------------------------------------------------------------*/
/* Hints: All clauses from last computation are retracted, then we      */
/* convert the Inputterms into our internal representation and reduce   */
/* them to normalform. Then the starting clause for the compution is    */
/* asserted with some additional information necessary for efficient    */
/* computation                                                          */

narrow((C => T1 = T2),'$solution'(U)) :-
   !,
   timer((
	(cont(apply,RuleKind) ->
		true
		;
		(RuleKind = off)
	),
	(cont(calls,CO) -> 
		(CN is CO+1)
		;
		(CN = 1)
	),
	clear_board(nonRecursive,CN),
	calls:=CN,
	asserta(maxJob(job(CN,1))),
	internalEqRep((C => T1 = T2),(IC1,[IT1 = IT2])),
	/* Preparing the condition */
	condition_to_term(IC1,IC2),
	toProlog(IC2,[],PIC2,TransSubst1),
	normalize(PIC2,PNFC),
	varsOf(PNFC,PCVars),
	marked_term(PNFC,COccs,vars(ff),nonvars(tt),nonvars),
	assertz(narrterm(job(CN,1),stufe(0),
			 occs(COccs),PNFC,unif(PCVars))),
	/* Preparing the conclusion */
	reduceR(IT1,NFIT1),
	reduceR(IT2,NFIT2),
	vars((NFIT1 = NFIT2),IVars),
	toProlog((NFIT1 = NFIT2),TransSubst1,(PT1 = PT2),TransSubst2),
	varsOf((PT1 = PT2),PVars),
	marked_term((PT1 = PT2),Occs,vars(ff),nonvars(tt),nonvars),
	assertz(narrterm(awaiting(job(CN,1)),bind(PCVars),job(CN,0),stufe(0),
			 occs(Occs),(PT1 = PT2),unif(PVars))),
	((narrowSolution(CN,unbounded,PSolTerms),
	  findSolution(PVars,IVars,PSolTerms,TransSubst2,U))
	; 
	((\+ solution(job(_,1),_)), U = [], !),
	apply:=RuleKind
	)
   )).
narrow(Goal,'$solution'(U)) :-
   timer((
	(cont(apply,RuleKind) ->
		true
		;
		(RuleKind = off)
	),
	(cont(calls,CO) -> 
		(CN is CO+1)
		;
		(CN = 1)
	),
	clear_board(nonRecursive,CN),
	calls:=CN,
	asserta(maxJob(job(CN,0))),
	internalGoalRep(Goal,IGoal),
	vars(IGoal,IVars),
	toProlog(IGoal,[],PGoal,TransSubst),
        toPrologVars(IVars,TransSubst,PVars),
	marked_term(PGoal,Occs,vars(ff),nonvars(tt),nonvars),
	assertz(narrterm(job(CN,0),stufe(0),
			 occs(Occs),PGoal,unif(PVars))),
	narrowSolution(CN,unbounded,PSolTerms),
	findSolution(PVars,IVars,PSolTerms,TransSubst,U),
	apply:=RuleKind
   )).



intNarrow(UBoundKind,PGoal1) :-
	(cont(apply,RuleKind) ->
		true
		;
		(RuleKind = off)
	),
	normalize(PGoal1,PGoal),
	(PGoal = 'true-bool' ->
		true
		;
		(varsOf(PGoal,PVars),
		 marked_term(PGoal,Occs,vars(ff),nonvars(tt),nonvars),
		 (cont(calls,CO) -> 
			(CN is CO+1)
			;
			(CN = 1)
		 ),
		 clear_board(nonRecursive,CN),
		 calls:=CN,
		 asserta(maxJob(job(CN,0))),
		 assertz(narrterm(job(CN,0),stufe(0),
				 occs(Occs),PGoal,unif(PVars))),
		 (UBoundKind = unbounded ->
        	     (UpperBound = unbounded)
		     ;
		     (UpperBound = 4)
		 ),
		 narrowSolution(CN,UpperBound,PGoalSolved),
		 adjust(PVars,PGoalSolved)
		)
	 ),
	 apply:=RuleKind .



findSolution(PVars,IVars,PSolterms,TransSubst,Sol) :-
        adjust(PVars,PSolterms),
	fromProlog(PSolterms,TransSubst,ISolterms,_),
	pairing(IVars,ISolterms,U),
	delete_identities(U,Sol).


adjust([],[]) :- !.
adjust([X1|XL1],[X2|XL2]) :-
	X1 = X2,
	adjust(XL1,XL2).


findSubst((@V,T),[(V,T)]) :- !.
findSubst((T,T),[]) :- atomic(T), !.
findSubst((T1,T2),S) :-
	T1=..[F|Args1], T2=..[F|Args2],
	pairing(Args1,Args2,ArgsPairs),
	map(findSolution,ArgsPairs,Sols),
	rReduce(append,[],Sols,S).


/*----------------------------------------------------------------------*/
/* internalGoalRep(+Goal1,-Goal2)					*/
/* computes the internal representation Goal2 for Goal1 if Goal1 is a   */
/* conjunction of equation.						*/

internalGoalRep(G1,G4) :-
   error:==none,
   (varsOf(G1,[]) ->
       expandEqs(G1,G2),
       ((cont(vars,Env) -> true; Env=[]),
            (typeCheckGoal(G2,Env,G3,_) ->
            	condition_to_term(G3,G4)
	    ;
		error("can't associate a type with
			%.",[G2],typeCheck)
	    )
       )
       ;
       error("Prolog variables not allowed in goals:
		%.",[G1],typeCheck)
   ), !,
   cont1(error,none),
   !.
internalGoalRep(G1,_) :-
   error("Goal not in syntactical correct form:
	%.",[G1],syntaxCheck),
   cont1(error,none).


/*----------------------------------------------------------------------*/
/* expandEqs(+Eq1,-Eq2)							*/
/* expands the terms in a conjunction of equations			*/

expandEqs((T1 = T2),(ET1 = ET2)) :-
   !, expandTerm(T1,ET1),
   expandTerm(T2,ET2).
expandEqs((T1 = T2) and Eqs,(ET1 = ET2) and EEqs) :-
   !, expandTerm(T1,ET1),
   expandTerm(T2,ET2),
   expandEqs(Eqs,EEqs).


/*----------------------------------------------------------------------*/
/* typeCheckGoal(+Goal1,+Env1,-Goal2,-Env2)				*/
/* typechecks a goal that is a conjunction of equations in environment	*/
/* Env1 and normalizes the goal getting Goal2.				*/

typeCheckGoal((T1 = T2),Env1,[(NF1 = NF2)],Env2) :- 
	typeCheckEq((T1 = T2),Env1,(IT1 = IT2),Env2,_),
	typedVars((IT1 = IT2),(ITR1 = ITR2)),
	reduceR(ITR1,NF1),
	reduceR(ITR2,NF2),
	!.
typeCheckGoal((T1 = T2) and Eqs,Env1,[(NF1 = NF2)|Eqs1],Env3) :-
	typeCheckEq((T1 = T2),Env1,(IT1 = IT2),Env2,_),
	typedVars((IT1 = IT2),(ITR1 = ITR2)),
	reduceR(ITR1,NF1),
	reduceR(ITR2,NF2),
	typeCheckGoal(Eqs,Env2,Eqs1,Env3),
	!.

/*----------------------------------------------------------------------*/
/* clear_board								*/
/* removes all informations left behind from the last computation	*/

clear_board(nonRecursive,_) :-
	!,
	retractall(_,narrterm,5),
	retractall(_,narrterm,6),
	retractall(_,solution,2),
	retractall(maxJob(job(_,_))), !.
clear_board(recursive,CallNo) :-
	retractall(CallNo,narrterm,5),
	retractall(CallNo,narrterm,6),
	retractall(CallNo,solution,2), 
	retractall(maxJob(job(CallNo,_))), !.

retractall(CallNo,Name,N) :-
	M is N-1,
	genVarList(M,Vars),
	CLAUSE=..[Name,job(CallNo,_)|Vars],
	retractAll(CLAUSE), !.
retractall(_MIN,_Name,_N) :- !.

retractAll(Pattern) :-
	retract(Pattern), fail.
retractAll(_) :- !.

/*----------------------------------------------------------------------*/
/* narrowSolution(+UpperBound,-Solution)		                */
/* engine for repeated production of solution, checking if such a 	*/
/* solution is new, starting waiting jobs and delivering solutions for  */
/* the goal of the user to narrow.					*/

narrowSolution(CN,UpperBound,Sol) :-
	narrowed(CN,UpperBound,DealtPID, T,Sol),
	T == 'true-bool',
	new_solution(DealtPID, Sol),
	start_waiting(DealtPID, Sol),
	DealtPID = job(_,0).


/*----------------------------------------------------------------------*/
/* narrowed(-PID,-Term,-Substitution)					*/
/* causes some narrowing step on some term yielding Term and gives back */
/* the identification PID of the concerned job, the resulting term Term */
/* of the narrowing step and the composition of all narrowing substi-   */
/* tutions during narrowing Substitutition.				*/

narrowed(CN,UpperBound,job(CN,JID),T2,U2) :-
     /* performance , */
	repeatForAllNodes(CN,UpperBound,job(CN,JID),T2,U2),
	((U2 == noMoreSolutions, !, fail)
	 ;
	 true
	).

repeatForAllNodes(CN,UpperBound,job(CN,JID),T2,U2) :-
	(no_clauses:==0),
	repeat,
	(narrterm(job(_,_),stufe(_),occs(_),_,unif(_)) ->
		((no_clauses:==0),
	 	 narrowLayer(CN,UpperBound,job(CN,JID),T2,U2))
		;
		((inc(no_clauses),cont(no_clauses,1)) ->
			fail
			;
			(U2 = noMoreSolutions)
		)
	).


narrowLayer(CN,UpperBound,job(CN,JID),T2,U2) :-
	retract(narrterm(job(CN,JID),stufe(N),occs(Occs),T1,unif(UnifBefore))),
	(T1 = 'true-bool' ->
		T2 = T1,
		U2 = UnifBefore
		;
	        M is N+1, 
                narrowingInGoal(UpperBound,job(CN,JID),M,Occs,T1,UnifBefore, T2,U2)
	).
	
    

narrowingInGoal(UpperBound,PID,M,Occs,T1,UnifBefore, T2,U2) :-
      	pathInFirstEq([],T1,Occs,Path,SubTerm1,SubOccs),
        narrow_step(PID,UpperBound,M,
	            T1,Occs,UnifBefore,Path,SubTerm1,SubOccs,
 		    T2,U2,CompStatus),
        ((CompStatus = eqSolved;
          CompStatus = goalSolved) ->
         !
         ;
         true).



/*----------------------------------------------------------------------*/
/* start_waiting(+PID,+Sol)						*/
/* starts all jobs waiting for a solution for job PID.   		*/


start_waiting(PID,Sol) :-
   narrterm(awaiting(PID),bind(Sol),AltePID,stufe(N),
            occs(Occs1),Narrterm1,unif(UnifStart)),
   normalize(Narrterm1,Narrterm2),
   weakly_based(Narrterm1,Occs1,Narrterm2,Occs2),
   assertz(narrterm(AltePID,stufe(N),
           occs(Occs2),Narrterm2,unif(UnifStart))), fail .
start_waiting(_PID,_Sol) :- !.


/*----------------------------------------------------------------------*/
/* narrow_step(+PID,+N,+OrigVars,+Vars,+Narrterm1,+Occs1,+Unif1,	*/
/*	       +Path,+Subterm1,+SubtermOccs1,-Narrterm2,-Unif2)         */
/* tries to make a narrowing step in Narrterm1 at occurrence Path.	*/
/* The result is Narrterm2 and the composition of the narrowing		*/
/* substitution with Unif1 gives Unif2.					*/


narrow_step(_PID,_UpperBound,_N,_Narrterm1,_Occs1,_Unif,_Path,X,_SubOccs,
            X,_U,_) :- 
   var(X), !, fail.
narrow_step(_PID,_UpperBound,_N,'true-bool',_Occs1,Unif, 
            [],'true-bool',_SubOccs, 'true-bool',Unif,goalSolved) :- !.
narrow_step(PID,_UpperBound,N,Narrterm1,Occs1,Unif, 
            Path,(T1 = T2),SubOccs, Narrterm2,Unif,CompStatus) :-
   !,
   functor(SubOccs,tt,_Arity), 
   unifier(T1,T2),
   normalized_subst(Unif),
   delete_solved_subgoal(Narrterm1,Occs1,Path,Narrterm2,Occs2),
   (Narrterm2 == 'true-bool' ->
      CompStatus = goalSoved
      ;
      (CompStatus = eqSolved,
       assertz(narrterm(PID,stufe(N),occs(Occs2),
                        Narrterm2,unif(Unif)))
   )  ).
narrow_step(PID,UpperBound,N,Narrterm1,Occs1,Unif,
            Path,T1,SubOccs, ResultTerm,ResultSub,simpleStep) :-
   upperBoundNotReached(N,UpperBound),
   functor(SubOccs,tt,_SArity),
   functor(T1,OP,_Arity), 
   getNarrowRule(OP,Cond1,L1,R1,_MTC,MR1),
   % Bestimme die Variablenmenge von L1 und T1
   varsOf(L1,VL1),
   varsOf(T1,VL2),
   append(VL1,VL2,VL),
   % Unifiziere L1 und T1
   unifier(L1,T1),
   % Ueberpruefe, ob VL eine normalisierte Substitution ist
   normalized_subst(VL),
   applyNarrowRule(PID,N,Narrterm1,Occs1,Unif,
                   Path,(Cond1,R1,MR1), ResultTerm,ResultSub).

applyNarrowRule(PID,N,Narrterm1,Occs1,Unif,
                Path,(Cond1,R1,MR1), ResultTerm,ResultSub) :-
   % Fuehre die Subtermersetzung durch
   change_path_arg(Path,Narrterm1,Narrterm2,R1),
   left_to_right_based((Path,MR1,Occs1),Occs2),
   sufficiently_large(Occs2,Narrterm2),
   normalize(Narrterm2,Narrterm3),
   weakly_based(Narrterm2,Occs2,Narrterm3,Occs3),
   (Cond1 == 'true-bool' ->
      (assertz(narrterm(PID,stufe(N),
                        occs(Occs3),Narrterm3,unif(Unif))),
       ResultTerm = Narrterm3,
       ResultSub  = Unif)
      ;
      (job_inc(PID,NewPID), 
       normalize(Cond1,NFCond),
       varsOf(NFCond,UnifStart),
       marked_term(NFCond,MTCond,vars(ff),nonvars(tt),nonvars),
       assertz(narrterm(awaiting(NewPID),bind(UnifStart),PID,stufe(N),
                        occs(Occs3),Narrterm3,unif(Unif))),
       assertz(narrterm(NewPID,stufe(0),
                        occs(MTCond),NFCond,unif(UnifStart))),
       ResultTerm = suspended,
       ResultSub  = [])
   ), !.


/*----------------------------------------------------------------------*/
/* getNarrowRule(+OP,						*/
/*		 -Condition,-Lhs,-Rhs,-MTCond,-MRhs)		*/
/* looks up a narrowing-rule Condition => Lhs = Rhs for operator OP	*/

getNarrowRule(OP,TC,L1,R1,MTC,MR1) :-
   rootLeft(Number,'$rule',OP),
   narrowRule(Number,'$rule',[(TC,[L1 = R1]),MTC,MR1]).
getNarrowRule(OP,TC,L1,R1,MTC,MR1) :-
   rootLeft(Number,'$rule',OP),
   auxNarrowRule(Number,'$rule',[(TC,[L1 = R1]),MTC,MR1]).



/*----------------------------------------------------------------------*/
/* job_inc(+OldPID,-NewPID)						*/
/* increments the job counter, giving back the result			*/
/* 08.12.88 no changes */

job_inc(job(Call,_),job(Call,Akt)) :-
	retract(maxJob(job(Call,PID))), 
	Akt is PID+1, 
	asserta(maxJob(job(Call,Akt))), !.

job_dec(PID) :-
   retract(job(Akt)), PID is Akt-1, asserta(job(PID)), !.


/*----------------------------------------------------------------------*/
/* normalize(+Goal,-NFGoal)						*/
/* computes the normalform NFGoal of a goal Goal			*/

normalize(PGoal,NPGoal) :-
	fromProlog(PGoal,[],IGoal,Subst),
	apply:=redRules,
	normalize(internal,IGoal,NGoal),
	apply:=off,
	toProlog(NGoal,Subst,NPGoal,_),
	!.

normalize(internal,'true-bool','true-bool') :- !.
normalize(internal,'false-bool','false-bool') :- !.
normalize(internal,(T1 = T2),Eqs) :-
	!, 
	reduceR(T1,NFT1),
	reduceR(T2,NFT2),
	decompose((NFT1 = NFT2),Eqs).
normalize(internal,'$2and-bool-bool-bool'(ST1,ST2),Term2) :-
	!,
	normalize(internal,ST1,NFST1),
	normalize(internal,ST2,NFST2),
        form_conjunction(NFST1,NFST2,Term2).
   
/*----------------------------------------------------------------------*/
/* decompose(+Eq,-Eqs)							*/
/* decomposes the equation Eq into several equations Eqs according to   */
/* the following rules:							*/
/* 1. If Eq = (f(t1,...,tn) = f(s1,...,sn)) and f is a constructor,     */
/* the decomposition process is applied to (t1=s1) and ... and (tn=sn)  */
/* getting Eqs.								*/
/* 2. If Eq = (f(t1,...,tn) = g(s1,...,sm)) and f,g are constructors,	*/
/* the equation is unsolvable. So decompose fails.			*/
/* 3. Otherwise Eqs is to Eq						*/

decompose((@X = @X),'true-bool')  :- !.
decompose((@X = T2),(@X = T2)) :- !.
decompose((T1 = @X),(T1 = @X)) :- !.
decompose((T1 = T2),Eqs) :-
	functor(T1,F,N),
	hhConstructor(F), !, 
	(functor(T2,F,N) ->
	   (T1=..[F|Args1],
	    T2=..[F|Args2], !, 
	    buildEqs(Args1,Args2,Eqs))
	   ;
	   ((functor(T2,G,_M),
	    hhConstructor(G)) -> % changed uh 06.12.89
	       (!, fail)
	       ;
	       (Eqs = (T1 = T2), !)
	   )
	).
decompose((T1 = T2),(T1 = T2)) :- !.


/*----------------------------------------------------------------------*/
/* buildEqs(+TL1,+TL2,-Eqs)						*/
/* takes two lists of terms TL1 = (t1,...,tn) and TL2 = (s1,...,sn),    */
/* applies decompose to every equation (ti = si) getting EqsI (if the   */
/* predicate decompose does not fail) and forms the conjunction of all  */
/* EqsI (additionally using the identity rule for 'true-bool').		*/

buildEqs([],[],'true-bool') :- !.
buildEqs([T1],[T2],Eqs) :-
	!, decompose((T1 = T2),Eqs).
buildEqs([T1|TL1],[T2|TL2],Eqs) :-
	decompose((T1 = T2),Eqs1),
	buildEqs(TL1,TL2,Eqs2),
        form_conjunction(Eqs1,Eqs2,Eqs).


/*----------------------------------------------------------------------*/
/* form_conjunction(Eqs1,Eqs2,Eqs)					*/
/* takes two conjunctions of equations Eqs1 and Eqs2 and forms the 	*/
/* conjunction of them, taking into account that 'true-bool' is an	*/
/* identity for the conjunction operator '$2and-bool-bool-bool'.	*/

form_conjunction(Eqs1,Eqs2,Eqs) :-
	(Eqs1 == 'true-bool' ->
  	    (Eqs = Eqs2)
	    ;
	    (Eqs2 == 'true-bool' ->
		(Eqs = Eqs1)
		;
		(Eqs = '$2and-bool-bool-bool'(Eqs1,Eqs2))
	)   ).


/*----------------------------------------------------------------------*/
/* delete_solved_subgoal(+Goal1,+Occs1,+Sol,+Path,-Goal2,-Occs2)        */
/* removes a solved subgoal from Goal1 getting Goal2. Additionally the  */
/* set of occurrences of Goal1 is corrected.				*/
/* 08.12.88 no changes */

delete_solved_subgoal(_Narrterm1,_Occs1,[],'true-bool',ff) :- !.
delete_solved_subgoal(Narrterm1,Occs1,Path,Narrterm2,Occs2) :-
	append(PathStart,[1],Path), !,
	append(PathStart,[2],Path2),
	path_arg(Path2,Narrterm1,SecondSubgoal),
	path_arg(Path2,Occs1,SecondSubgoalOccs),
	change_path_arg(PathStart,Narrterm1,Narrterm2,SecondSubgoal),
	change_path_arg(PathStart,Occs1,Occs2,SecondSubgoalOccs).
delete_solved_subgoal(Narrterm1,Occs1,Path,Narrterm2,Occs2) :-
	append(PathStart,[2],Path), !,
	append(PathStart,[1],Path1),
	path_arg(Path1,Narrterm1,FirstSubgoal),
	path_arg(Path1,Occs1,FirstSubgoalOccs),
	change_path_arg(PathStart,Narrterm1,Narrterm2,FirstSubgoal),
	change_path_arg(PathStart,Occs1,Occs2,FirstSubgoalOccs).


/*----------------------------------------------------------------------*/
/* normalized_subst(+Termlist)						*/
/* checks if Subst is a normalized substitution                         */

normalized_subst(PSubst) :-
	apply:=redRules,
	fromProlog(PSubst,[],Subst,_),
	normalized_subst(internal,Subst),
	apply:=off.

normalized_subst(internal,[]) :- !.
normalized_subst(internal,[T1|R]) :-
	reduceR(T1,T2),
	T1 == T2,
	normalized_subst(internal,R).


/*----------------------------------------------------------------------*/
/* delete-identities(+Subst1,-Subst2)                                   */
/* deletes all identity replacements from Subst1 yielding Subst2        */

delete_identities([],[]) :- !.
delete_identities([(X1,@X1)|Tail1],Tail2) :-
	!, delete_identities(Tail1,Tail2).
delete_identities([(X1,T1)|Tail1],[(X1,T1)|Tail2]) :-
	delete_identities(Tail1,Tail2).


/*----------------------------------------------------------------------*/
/* pathInFirstEq(+OldPath,+Term,+Occs,-Path,-Subterm,-SubtermOccs	*/
/* due to the fact that Term is a conjunction of equations, only the 	*/
/* first equation will be expected for narrowing occurrences. So this	*/
/* procedure enumerates all possible narrowing occurrences in the first */
/* equation of Term. 							*/
/* 11.01.89 maybe not correct regarding completeness of narrowing	*/

pathInFirstEq(OldPath,'$2and-bool-bool-bool'(Eqs1,_),TO,Path,ST,STO) :-
	!,
	append(OldPath,[1],PFH),
	pathInFirstEq(PFH,Eqs1,TO,Path,ST,STO).
% pathInFirstEq(OldPath,Eq,TO,Path,ST,STO) :-
%	path(OldPath,Eq,TO,Path,ST,STO).
pathInFirstEq(OldPath,(L = R),TO,OldPath,(L = R),TO).
pathInFirstEq(OldPath,(L = R),TO,Path,ST,STO) :-
        TO =.. [tt|OccArgs],
	path_args(OldPath,1,[L,R],OccArgs,Path,ST,STO).


/*----------------------------------------------------------------------*/
/* path(+OldPath,+Term,+Occs,-Path,-Subterm,-SubtermOccs)               */
/* computes an occurrence Path of Term having OldPath as prefix which   */
/* belongs to Occs. Gives also the Subterm of Term at Path and the set  */
/* of occurrences from Occs which belong to Subterm.                    */

path(_OldPath,X,Occs,_Path,X,Occs) :- 
	var(X), !, fail.
path(OldPath,T1,Occs1,Path,T2,Occs2) :-
	Occs1 =.. [tt|OccArgs],
	T1 =.. [_F|Args], 
	path_args(OldPath,1,Args,OccArgs,Path,T2,Occs2).
path(OldPath,T1,Occs,OldPath,T1,Occs) :-
	functor(Occs,tt,_N),
	functor(T1,F,_),
	\+ hhConstructor(F), !.

path_args(_OldPath,_N,[],[],_Path,_T,_Occs) :- fail.
path_args(OldPath,N,[T1|_TL],[O1|_OL],Path,T2,O2) :-
	append(OldPath,[N],PFH), path(PFH,T1,O1,Path,T2,O2).
path_args(OldPath,N,[_T1|TL],[_O1|OL],Path,T2,O2) :-
	M is N+1, path_args(OldPath,M,TL,OL,Path,T2,O2).


/*----------------------------------------------------------------------*/
/* new_solution(+PID,+Subst)                                            */
/* succeeds if Subst is a new solution for PID, that means that Subst   */
/* is not subsumed by any previous solution found for PID.              */
/* In this case Subst is added to the list of the solution for PID      */
/* Otherwise new_solution fails                                         */

new_solution(PID,NewSol) :-
	\+ solution(PID,_),
	assertz(solution(PID,NewSol)), !.
new_solution(PID,NewSol) :-
	fromProlog(NewSol,[],TestSol,_),
	\+ solution(PID,TestSol),
	assertz(solution(PID,NewSol)), !.


/*----------------------------------------------------------------------*/
/* unifier(+Term1,+Term2)							*/
/* tries to unifier Term1 and Term2					*/
/* 08.12.88 */

unifier(T1,T2) :-
	ac_uniAC(T1,T2).


/*----------------------------------------------------------------------*/
/* fromProlog_subst(+Subst1,+Subst2,-Subst3)				*/
/* translates Subst1 involving Prolog variables back to a substitution  */
/* having only internal variables, using the translation substitution	*/
/* Subst2, getting Subst3.						*/

fromProlog_subst([],_Subst,[]).
fromProlog_subst([(X1 , T1)|TailSub1],Subst,[(X1 , T2)|TailSub2]) :-
	fromProlog(T1,Subst,T2,NSubst),	
	fromProlog_subst(TailSub1,NSubst,TailSub2), !.


/*----------------------------------------------------------------------*/
/* toProlog_subst(+Subst1,+Subst2,-Subst3)				*/

toProlog_subst([],_Subst,[]).
toProlog_subst([(X1 , T1)|TailSub1],Subst,[(X2 , T2)|TailSub2]) :-
	toProlog(@X1,Subst,X2,Subst1),
	toProlog(T1,Subst1,T2,Subst2),
	toProlog_subst(TailSub1,Subst2,TailSub2), !. 


/*----------------------------------------------------------------------*/
/* subst(+Term1,+Subst,-Term2)						*/
/* applies substitution Subst to Term1 getting Term2			*/

subst(Term, [], Term) :- !.
subst(@X, [(Y , Term1)|_TailSubst], Term1) :-
	X == Y, !.
subst(@X, [(_Y , _Term1)|TailSubst], Term2) :-
	!, subst(@X, TailSubst, Term2).
subst(Term1, _Subst, Term1) :-
	atomic(Term1), !.
subst(Term1, Subst, Term2) :-
	Term1 =.. [F|Termlist1], !,
	subst_termlist(Termlist1, Subst, Termlist2),
	Term2 =.. [F|Termlist2].

subst_termlist([], _Subst, []).
subst_termlist([Term1|Tail1], Subst, [Term2|Tail2]) :-
	!, subst(Term1, Subst, Term2),
	subst_termlist(Tail1, Subst, Tail2).


/*----------------------------------------------------------------------*/
/* add_replacement(+Rep,+Subst1,-Subst2)				*/
/* composes substitution Subst1 with a single replacement Rep getting   */
/* Subst2.								*/

add_replacement((X , T), [], [(X , T)]) :- !.
add_replacement((X , T), [(X1 , T1)|Tail1], Tail2) :-
	X == X1, T == T1, !, 
	add_replacement((X , T),Tail1,Tail2).
add_replacement((X , T), [(X1 , T1)|Tail1], [(X1 , T2)|Tail2]) :-
!, subst(T1, [(X , T)], T2),
   add_replacement((X , T), Tail1, Tail2).


/*----------------------------------------------------------------------*/
/* comp(+Subst1,+Subst2,-Subst3)                                        */
/* computes the composition Subst1 and Subst2 yielding Subst3           */

comp([],Subst,Subst) :- !.
comp([Rep1|RepL],Subst1,Subst3) :-
   add_replacement(Rep1,Subst1,Subst2),
   comp(RepL,Subst2,Subst3).


/*----------------------------------------------------------------------*/
/* comp_restrict(+Subst1,+Subst2,-Subst3)                               */
/* computes the composition of Subst1 and Subst2 and restricting the    */
/* resulting substitution to the domain of Subst2                       */

comp_restrict(_NewSubst,[],[]) :- !.
comp_restrict(NewSubst,[(X,T1)|RepL1],[(X,T2)|RepL2]) :-
   subst(T1,NewSubst,T2),
   comp_restrict(NewSubst,RepL1,RepL2).


/*----------------------------------------------------------------------*/
/* condition_to_term(+EqList,-Goal)					*/
/* transformes a list of equations into a goal using the operator 	*/
/* '$2and-bool-bool-bool' as connective.				*/

condition_to_term([],'true-bool') :- !.
condition_to_term([L = R],(L = R)) :- !.
condition_to_term([(L = R)|GR],'$2and-bool-bool-bool'((L = R),T)) :-
   condition_to_term(GR,T).

/*----------------------------------------------------------------------*/

upperBoundNotReached(_,unbounded) :- !.
upperBoundNotReached(N,M) :- N =< M.


/*----------------------------------------------------------------------*/

toPrologVars([],_,[]) :- !.
toPrologVars([V1|VL1],S,[V2|VL2]) :-
	createPVar(V1,S,V2,_),
        toPrologVars(VL1,S,VL2), !.


