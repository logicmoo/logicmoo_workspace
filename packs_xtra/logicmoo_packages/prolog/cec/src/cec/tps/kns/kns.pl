/*
 *	file:		kns.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains the implementation of the ordering KNS
 *
 *	history:
 *	891010	js	Added this comment
 *	891127	uh	Changed definition of
 *			kns_GT/3
 *			kns_termEQ/3
 *			to allow the use of disambiguated order-sorted
 *			operators O/N
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

%                       kns
%                  ============
%
%  This is an implementation of the path ordering proposed by
%
%       D. Kapur, P. Narendran, G. Sivakumar,
%       "A Path Ordering for Proving Termination of Term Rewriting Systems",
%       Proceedings of the International Joint Conference on Theory
%       and Practice of Software Development (TAPSOFT), Berlin, March 1985,
%       Volume 1: (CAAP 85)
%
%  In the following this ordering is referred to by "kns".
%
%  All predicates in the following program package have the prefix "kns_",
%  except standard predicates imported from elsewhere.
%
%  The interface to the program package:
%
%  1.a) kns_ordering(Comment,L,R,NL,NR,ExtYesNo).
%
%       If "L == R" can be ordered by the simplification ordering, possibly
%       after an extension of the ordering, this predicate is "true" and
%       "NL" is the new left side, "NR" the new right side. "ExtYesNo" contains
%       "yes" if an extension of the operator precedence was necessary to
%       order the equation, otherwise it contains "no". The predicate fails,
%       if "L == R" is unorderable by the actual ordering.
%       In front of every interaction with the user the string in "Comment"
%       is displayed.
%
%
%  1.b) kns_ordering(Comment,L,R,ExtYesNo).
%
%       If "L == R" can be ordered to "L -> R" by the simplification ordering,
%       possibly after an extension of the ordering, this predicate is "true" .
%       "ExtYesNo" says
%       "yes" if an extension of the operator precedence was necessary
%       otherwise it is "no". The predicate fails,
%       if "L == R" is unorderable in the desired direction by the
%       actual ordering.
%
%  2.   kns_clear.
%
%       deletes all precedence and status informations.
%       Also the "flexible" attribute is set to the default value "on".
%
%  3.   kns_declConstr([OpList]).
%
%       the set of already existing contructors is extended by the list of
%       operators in "OpList". This extension will only be executed if it
%       is consistent with the actual operator precedence. Otherwise this
%       predicate fails.
%
%  4.   kns_declPrecGT(OP1,OP2).
%
%       inserts in the operator table, that "OP1" is greater than "OP2"
%       but only in the case of a consistent precedence extension.
%       In the other case the predicate fails.
%
%  5.   kns_declPrecEQ(OP1,OP2).
%
%       inserts in the operator table, that "OP1" has an to "OP2" equal
%       precedence but only in the case of a consistent precedence extension.
%       In the other case the predicate fails.
%
%  6.   kns_declStatus_lr(OP). ,
%       kns_declStatus_rl(OP). ,
%       kns_declStatus_ms(OP).
%
%       insert in the operator table, that "OP" has Status
%       "lr" (left to right),
%       "rl" (right to left) or
%       "ms" (multiset)
%       but only in the case of a consistent precedence extension.
%       In the other case the predicates fails.
%
%
%  7.   kns_flexible_on. kns_flexible_off.
%
%       These predicates can be used to set or unset the "flexible" attribute.
%       In the case of "off" kns can order the equations only in the given
%       direction. If an equation is orderable in the reverse direction,
%       the user is asked, if he wants to reverse the equation, if not
%       "kns_ordering(...)."  fails.
%       If flexible is "on", all orderable equations are oriented without
%       informing the user, that an equation is reversed.
%       By default "flexible" is "on".
%
%  8.   kns_displayOpPrec.
%
%       displays all precedence and status information.
%
%
%  STATE OF THE PRECEDENCE
%
%       The actual state of the precedence is represented by the values of
%       the following two predicates:
%
%       1. kns_opPrecStat(...).
%       2. kns_flexible(...).
%
%****************** THE PROGRAM************************************************

%       The actual ordering allows the equation
%       to order in the given direction
%       (The operator precedence does not change):

kns_ordering(_,L,R,no) :-
	kns_GT(L,R,extToBy([],[],[])),
	!.
kns_ordering((Comment,CArgs),L,R,yes) :-
%	kns_GT(L,R,extToBy(Stat_Prec,_,_)),
%	nl,sPrint(Comment,CArgs),
	!,
	kns_ord((Comment,CArgs),[],Ext,L,R,_,_,asGiven),
	kns_makeopPrecStat(Ext),
	!.


kns_ordering(_,L,R,L,R,no) :-
	kns_GT(L,R,extToBy([],[],[])),
	!.

%       The actual ordering allows the equation
%       to order in the reverse direction :

kns_ordering(_,L,R,R,L,no) :-
	kns_GT(R,L,extToBy([],[],[])),
	(	kns_flexible(on)
	;	sPrint("

Consider the equation
	%.

This equation can be ordered in the reverse direction, and
the ""flexible"" attribute is ""off"".

Should it be reversed anyway?(y./n.)
",['$singleEq'(L,R)]),
		repeat,
		read(C),
		((C=y ; C=n) ->
			true
		;
			write('??? '),
			fail
		),
		(C=y ->
			true
		;
			!,
			fail
		)
	),
	!.						
%       By the actual ordering "L = R" is not orderable,
%       but the equation is orderable by an appropriate extension
%       of the operator precedence either in the one or in the other direction :

kns_ordering((Comment,CArgs),L,R,NL,NR,yes) :-
	(kns_GT(L,R,extToBy([],_,_)) ->
		(kns_GT(R,L,extToBy([],_,_)) ->
			nl,
			sPrint(Comment,CArgs),
			kns_askUserForDirection(L,R,AskedDir),
			!,    % neu
			kns_ord((Comment,CArgs),[],Ext,L,R,NL,NR,AskedDir)
		;
			kns_ord((Comment,CArgs),[],Ext,L,R,NL,NR,asGiven)
	  	)
	;
		% kns_GT(R,L,extToBy(Stat_Prec,_,_)),
	    	kns_ord((Comment,CArgs),[],Ext,L,R,NL,NR,reverse)
	),
	kns_makeopPrecStat(Ext),
	!.


kns_ord((Comment,CArgs),Stat_Prec,Ext,L,R,NL,NR,AskedDir) :-
	kns_alOrder(L,R,Stat_Prec,Options,AskedDir),
	(	Options \== [],
		nl,
		sPrint(Comment,CArgs),
	  	kns_showOptions(L,R,Options),
	  	write('your choice?(#.): '),
	  	read(Nr),
	  	!,
	  	kns_orient_extendPrec(Nr,Options,_,_,_,_,Ext2,NewAskedDir),
		append(Stat_Prec,Ext2,NewPrec),
	  	kns_ord((Comment,CArgs),NewPrec,Ext3,L,R,NL,NR,NewAskedDir),
		append(NewPrec,Ext3,Ext)
	;
		Options = [],
	  	kns_orient(L,R,NL,NR,AskedDir),
		Ext = []
	),
	!.


kns_alOrder(L,R,Stat_Prec,Options,Direction) :-
	(	(	var(Direction)
		;
			(	Direction = notYetKnown
			;	Direction = asGiven
			)
		),
		kns_tryAlternatives(L,R,Stat_Prec,OptionsL),
	    	(	Direction = notYetKnown,
			kns_tryAlternatives(R,L,Stat_Prec,OptionsR),
			kns_numerate(OptionsL,OptionsR,Options)
		;
			Direction = asGiven,
			kns_numerate(OptionsL,[],Options)
		)
	;
		Direction = reverse,
		kns_tryAlternatives(R,L,Stat_Prec,OptionsR),
		kns_numerate([],OptionsR,Options)
	),
	!.


%       "kns_tryAlternatives"
%
%       1. checks if there exist a precedence extension such that "L > R",
%          More precisely it looks for a "minimal" extension.
%          Because of the incrementality property of simplification orderings
%          all consistent precedence extensions do not affect the set of already
%          orderable terms. Therefore we are not interested in precedence
%          extensions, which can be regarded as an extension  of a precedence
%          which states already "L > R". Such an extension is not minimal.
%
%          Let for example the minimal extension be "a > b , c = d",
%
%       2. then it trys all alternatives of the first decision,
%          and asks the user, by which of the succesful alternatives
%          the precedence should be definitely extended.
%
%          In our example the alternatives are "a = b, not(a > b), not(a = b)"
%
%          In the case of only one succesful alternative
%          "kns_tryAlternatives" looks immediately for alternatives
%          on the next decision level and asks the user then to choose between
%          the compound alternatives.
%
%
kns_tryAlternatives(L,R,Stat_Prec,AltsWithSucc) :-
	kns_tryAlternatives2(L,R,Stat_Prec,AltsWithSucc1),
	kns_unpair(AltsWithSucc1,AltsWithSucc),
	!.


kns_tryAlternatives2(L,R,Stat_Prec,[]) :-
	kns_GT(L,R,extToBy(Stat_Prec,Stat_Prec,[])),
	!.
kns_tryAlternatives2(L,R,Stat_Prec,AltsWithSucc) :-
	kns_GT(L,R,extToBy(Stat_Prec,_NewPrec,Ext)),
	kns_tryAlts(L,R,Stat_Prec,Ext,AltsWithSucc1),
	kns_replaceNotgtOreq(L,R,Stat_Prec,AltsWithSucc1,AltsWithSucc2),
	kns_replaceSingleAlternatives(L,R,Stat_Prec,AltsWithSucc2,AltsWithSucc3),
	kns_delayStatusDecision(AltsWithSucc3,AltsWithSucc),
	!.
kns_tryAlternatives2(L,R,Stat_Prec,Ext,AltsWithSucc) :-
	kns_tryAlts(L,R,Stat_Prec,Ext,AltsWithSucc1),
	kns_replaceNotgtOreq(L,R,Stat_Prec,AltsWithSucc1,AltsWithSucc2),
	kns_replaceSingleAlternatives(L,R,Stat_Prec,AltsWithSucc2,AltsWithSucc3),
	kns_delayStatusDecision(AltsWithSucc3,AltsWithSucc),
	!.

kns_replaceNotgtOreq(L,R,Stat_Prec,AltsWithSucc1,AltsWithSucc):-
	'chooseElem'(AltsWithSucc1,L1,[(notgtOreq(A,B),Ext)],R1),
	kns_tryAlternatives2(L,R,[notgtOreq(A,B)|Stat_Prec],Ext,AltsWithSucc2),
	(AltsWithSucc2 = [] ->
		AltsWithSucc = []
	;
		append(L1,AltsWithSucc2,AW1),
		append(AW1,R1,AltsWithSucc)
	),
	!.
kns_replaceNotgtOreq(_L,_R,_Stat_Prec,AltsWithSucc,AltsWithSucc).

% only one alternative --> look forward
kns_replaceSingleAlternatives(L,R,Stat_Prec,AltsWithSucc1,AltsWithSucc):-
	AltsWithSucc1 = [[(Prec,_Rest)]], 
	kns_tryAlternatives2(L,R,[Prec|Stat_Prec],AltsWithSucc2),
	kns_addSuccs(AltsWithSucc1,AltsWithSucc2,AltsWithSucc),
	!.
kns_replaceSingleAlternatives(_L,_R,_Stat_Prec,AltsWithSucc,AltsWithSucc).

% Any status decision for A will allow the orientation of the equation. 
% Therefore a concrete decision can be delayed.

kns_delayStatusDecision(AltsWithSucc1,AltsWithSucc):-
	((member([(status(A,ms),MSR)],AltsWithSucc1), 
	  member([(status(A,lr),LRR)],AltsWithSucc1), 
	  member([(status(A,rl),RLR)],AltsWithSucc1),
	  subset(MSR,LRR),
	  subset(LRR,RLR),
	  subset(RLR,MSR)) -> 
		(MSR = []->
			AltsWithSucc = []
		;
			MSR = [Fst|Rst],
			AltsWithSucc = [[(Fst,Rst)]]
		)

	;
		AltsWithSucc = AltsWithSucc1
	),
	!.


kns_unpair(Ps,Ps1) :-
	map(kns_unpair1,Ps,Ps1).

kns_unpair1([],[]).
kns_unpair1([(P,_)|Ps],[P|Ps1]):-
	!,
	kns_unpair1(Ps,Ps1).
kns_unpair1([P|Ps],[P|Ps1]):-
	kns_unpair1(Ps,Ps1).


kns_unpairL([(P,_R)],[P]) :- !.
kns_unpairL([(P,_R)|PairList],[P|List]) :-
	kns_unpairL(PairList,List).


kns_tryAlts(L,R,Stat_Prec,[status(A,ms)|Ext],[[(status(A,ms),Ext)]|AltsWithSucc]) :-
	kns_try([status(A,lr),status(A,rl)],L,R,Stat_Prec,AltsWithSucc).
kns_tryAlts(L,R,Stat_Prec,[status(A,lr)|Ext],[[(status(A,lr),Ext)]|AltsWithSucc]) :-
	kns_try([status(A,ms),status(A,rl)],L,R,Stat_Prec,AltsWithSucc).
kns_tryAlts(L,R,Stat_Prec,[status(A,rl)|Ext],[[(status(A,rl),Ext)]|AltsWithSucc]) :-
	kns_try([status(A,lr),status(A,ms)],L,R,Stat_Prec,AltsWithSucc).
kns_tryAlts(L,R,Stat_Prec,[gt(A,B)|Ext],[[(gt(A,B),Ext)]|AltsWithSucc]) :-
	kns_try([eq(A,B),notgtOreq(A,B)],L,R,Stat_Prec,AltsWithSucc).
kns_tryAlts(L,R,Stat_Prec,[eq(A,B)|Ext],[[(eq(A,B),Ext)]|AltsWithSucc]) :-
	kns_try([gt(A,B),notgtOreq(A,B)],L,R,Stat_Prec,AltsWithSucc).
kns_tryAlts(L,R,Stat_Prec,[notgtOreq(A,B)|Ext],[[(notgtOreq(A,B),Ext)]|AltsWithSucc]) :-
	kns_try([eq(A,B),gt(A,B)],L,R,Stat_Prec,AltsWithSucc).


kns_try(S,L,R,Stat_Prec,AltsWithSucc):-
	setof(Succ,kns_Alt(S,L,R,Stat_Prec,Succ),AltsWithSucc),
	!.
kns_try(_,_L,_R,_Stat_Prec,[]).

kns_Alt(S,L,R,Stat_Prec,[(S1,FurtherExt)]):-
	member(S1,S),
	kns_isConsistent(S1,Stat_Prec),
	kns_onetimeGT(L,R,extToBy([S1|Stat_Prec],_,FurtherExt)).

kns_onetimeGT(L,R,extToBy(Stat_Prec,_,[])) :-
	kns_GT(L,R,extToBy(Stat_Prec,_,[])),
	!.
kns_onetimeGT(L,R,extToBy(Stat_Prec,_,Ext)) :-
	kns_GT(L,R,extToBy(Stat_Prec,_,Ext)),
	!.


%       TERM COMPARISON:
%
%       L is a non-variable term, R is a variable, then L > R
%       if and only if L contains this variable.

kns_GT(L,@(V),extToBy(Stat_Prec,Stat_Prec,[])) :-
	L \== @(V),
	kns_hasVar(L,V),
	!.

%       s=f(s1,...,sm), t=g(t1,...,tn), M1={s1,...,sm}, M2={t1,...,tn}
%
%       s > t if and only if:
%
%       case f > g, for all i 1<=i<=n  s > ti:

kns_GT(L,R,extToBy(Stat_Prec,NewPrec,Ext)) :-
	L\==R,
	osDecompose(L,FL,_,ArgsL),   % changed 27.11.89 uh
	osDecompose(R,FR,_,ArgsR),   % changed 27.11.89 uh
	FL \== @,
	FR \== @,
	kns_GTvariants(L,R,FL,FR,ArgsL,ArgsR,Stat_Prec,NewPrec,Ext).


kns_GTvariants(L,_R,FL,FR,_ArgsL,ArgsR,Stat_Prec,NewPrec,Ext) :-
	(Ext == [] ->
		Ext1 = [],
		Ext2 = []
	;
		true
	),
	kns_opPrec(gt(FL,FR),extToBy(Stat_Prec,NewPrec1,Ext1)),
	kns_GT_forAll(L,ArgsR,extToBy(NewPrec1,NewPrec,Ext2)),
	append(Ext1,Ext2,Ext).
kns_GTvariants(L,_R,FL,FR,ArgsL,ArgsR,Stat_Prec,NewPrec,Ext) :-
	(Ext == [] ->
		Ext1 = [],
		Ext2 = []
	;
		true
	),
	kns_opPrec(eq(FL,FR),extToBy(Stat_Prec,NewPrec1,Ext1)),
	(ArgsL = [_|_], ArgsR = [] ->
		NewPrec = Stat_Prec,
		Ext = []
	;
		(ArgsL = [L1], ArgsR = [R1] ->
			kns_GT(L1,R1,extToBy(NewPrec1,NewPrec,Ext))
		;
			kns_TC3stati(L,FL,FR,ArgsL,ArgsR,extToBy(NewPrec1,NewPrec,Ext2)),
			append(Ext1,Ext2,Ext)
		)
	).


kns_GT_forAll(_L,[],extToBy(Stat_Prec,Stat_Prec,[])) :- !.
kns_GT_forAll(L,[R|Rs],extToBy(Stat_Prec,NewPrec,Ext)) :-
	(Ext == [] ->
		Ext1 = [],
		Ext2 = []
	;
		true
	),
	kns_GT(L,R,extToBy(Stat_Prec,NewPrec1,Ext1)),
	kns_GT_forAll(L,Rs,extToBy(NewPrec1,NewPrec,Ext2)),
	append(Ext1,Ext2,Ext).


kns_TC3stati(L,FL,FR,ArgsL,ArgsR,Stat_Prec) :-
	kns_TCmsstatus(L,FL,FR,ArgsL,ArgsR,Stat_Prec).
kns_TC3stati(L,FL,FR,ArgsL,ArgsR,Stat_Prec) :-
	kns_TClrstatus(L,FL,FR,ArgsL,ArgsR,Stat_Prec).
kns_TC3stati(L,FL,FR,ArgsL,ArgsR,Stat_Prec) :-
	kns_TCrlstatus(L,FL,FR,ArgsL,ArgsR,Stat_Prec).


%          case ms - status (no status)
%               then MP(M1-M2) >>LP MP(M2-M1)
%

kns_TCmsstatus(_L,FL,FR,ArgsL,ArgsR,extToBy(Stat_Prec,NewPrec,Ext)) :-
	(Ext == [] ->
		Ext1 = [],
		Ext2 = []
	;
		true
	),
	(ArgsL = [_] ->
		kns_opPrec(status(FR,ms),extToBy(Stat_Prec,NewPrec1,Ext1))
	;
		kns_opPrec(status(FL,ms),extToBy(Stat_Prec,NewPrec1,Ext1))
	),
	kns_MP(ArgsL,UnionOfMPL),
	kns_MP(ArgsR,UnionOfMPR),
	kns_multiSetGT(kns_pathGT,UnionOfMPL,UnionOfMPR,extToBy(NewPrec1,NewPrec,Ext2)),
	append(Ext1,Ext2,Ext).


%          case lr - status
%               there exists j such that
%                s1 ~ t2, ..., sj-1 ~ tj-1, sj > tj,    and
%                         s > ti for j+1<=i<=n

kns_TClrstatus(L,FL,FR,ArgsL,ArgsR,extToBy(Stat_Prec,NewPrec,Ext)) :-
	(Ext == [] ->
		Ext1 = [],
		Ext2 = []
	;
		true
	),
	(ArgsL = [_] ->
		kns_opPrec(status(FR,lr),extToBy(Stat_Prec,NewPrec1,Ext1))
	;
		kns_opPrec(status(FL,lr),extToBy(Stat_Prec,NewPrec1,Ext1))
	),
	kns_status_condition(L,ArgsL,ArgsR,extToBy(NewPrec1,NewPrec,Ext2)),
	append(Ext1,Ext2,Ext).


%          case rl - status
%               there exists j such that
%                the other way around


kns_TCrlstatus(L,FL,FR,ArgsL,ArgsR,extToBy(Stat_Prec,NewPrec,Ext)) :-
	(Ext == [] ->
		Ext1 = [],
		Ext2 = []
	;
		true
	),
	(ArgsL = [_] ->
		kns_opPrec(status(FR,rl),extToBy(Stat_Prec,NewPrec1,Ext1))
	;
		kns_opPrec(status(FL,rl),extToBy(Stat_Prec,NewPrec1,Ext1))
	),
	kns_reverse(ArgsL,RevArgsL),
	kns_reverse(ArgsR,RevArgsR),
	kns_status_condition(L,RevArgsL,RevArgsR,extToBy(NewPrec1,NewPrec,Ext2)),
	append(Ext1,Ext2,Ext).


%       case f not(> or ~) g,
%               then MP({s}) >>LP MP({t})


kns_GTvariants(L,R,FL,FR,_ArgsL,_ArgsR,Stat_Prec,NewPrec,Ext) :-
	(Ext == [] ->
		Ext1 = [],
		Ext2 = []
	;
		true
	),
	kns_opPrec(notgtOreq(FL,FR),extToBy(Stat_Prec,NewPrec1,Ext1)),
	kns_MP([L],MPL),
	kns_MP([R],MPR),
	kns_multiSetGT(kns_pathGT,MPL,MPR,extToBy(NewPrec1,NewPrec,Ext2)),
	append(Ext1,Ext2,Ext).


%       (lr) or (rl) status condition:
%               there exists j such that
%                s1 ~ t2, ..., sj-1 ~ tj-1, sj > tj,    and
%                          s > ti for j+1<=i<=n

kns_status_condition(L,[ArgL|ArgsL],[ArgR|ArgsR],extToBy(Stat_Prec,NewPrec,Ext)) :-
	(Ext == [] ->
		Ext1 = [],
		Ext2 = []
	;
		true
	),
	(	kns_termEQ(ArgL,ArgR,extToBy(Stat_Prec,NewPrec1,Ext1)),
	  	kns_status_condition(L,ArgsL,ArgsR,extToBy(NewPrec1,NewPrec,Ext2)),
		append(Ext1,Ext2,Ext)
	;
		kns_GT(ArgL,ArgR,extToBy(Stat_Prec,NewPrec1,Ext1)),
		kns_jplus1GT(L,ArgsR,extToBy(NewPrec1,NewPrec,Ext2)),
		append(Ext1,Ext2,Ext)
	).


kns_jplus1GT(_L,[],extToBy(Stat_Prec,Stat_Prec,[])) :- !.
kns_jplus1GT(L,[ArgR|ArgsR],extToBy(Stat_Prec,NewPrec,Ext)) :-
	(Ext == [] ->
		Ext1 = [],
		Ext2 = []
	;
		true
	),
	kns_GT(L,ArgR,extToBy(Stat_Prec,NewPrec1,Ext1)),
	kns_jplus1GT(L,ArgsR,extToBy(NewPrec1,NewPrec,Ext2)),
	append(Ext1,Ext2,Ext).


%       PATH COMPARISON:
%
%       Let P1 = <f1,t1>,<f2,t2>...<fm,tm> and
%           P2 = <g1,s1>,<g2,s2>...<gn,sn>.
%
%       P1 > P2 <==>
%       for all <gj,sj> in P2 there exists <fi,ti> in P1
%       such that the path-condition holds


%       For an empty P2 the path condition holds trivially, therefore:

kns_pathGT(P1,[],extToBy(Stat_Prec,Stat_Prec,[])) :-
	P1 \== [],
	!.

%       Since pathes ending in different variables are incomparable, this
%       property is checked first. If the paths end with the same variables
%       the variables are dropped from the end of both pathes and the
%       remaining sequences of two-tuples are compared:

kns_pathGT(P1,P2,SP_Opt) :-
	kns_isComparable(P1,P2,DropP1,DropP2),
	kns_deleteCommonSuffix(DropP1,DropP2,NP1,NP2),
	kns_pthGT(NP1,NP2,SP_Opt).


%       This is a modification to the definition stated in the above paper.
%       The new definition allows for a more efficient path comparison
%       and is based on the proposition 3-12 in
%
%       Michael Rusinowitch,
%       "Path Of Subterms Ordering And Recursive Decomposition Ordering
%        Revisited",  in Rewriting Techniques and Application, Dijon,
%       France, May 1985
%
%

kns_pthGT(P1,[],extToBy(Stat_Prec,Stat_Prec,[])) :-
	P1 \== [],
	!.
kns_pthGT(P1,P2,SP_Opt) :-
	P1 \== P2,
	P2 = [A2|R],
	kns_pathGTcond([],A2,R,P1,SP_Opt).


kns_pathGTcond(L2,F2,[R2|R2s],P1,extToBy(Stat_Prec,NewPrec,Ext)) :-
	(Ext == [] ->
		Ext1 = [],
		Ext2 = []
	;
		true
	),
	'chooseElem'(P1,L1,F1,R1),
	kns_pathCond(L1,F1,R1,L2,F2,[R2|R2s],extToBy(Stat_Prec,NewPrec1,Ext1)),
	append(L2,[F2],NL),
	kns_pathGTcond(NL,R2,R2s,P1,extToBy(NewPrec1,NewPrec,Ext2)),
	append(Ext1,Ext2,Ext).
kns_pathGTcond(L2,F2,[],P1,Stat_Prec) :-
	'chooseElem'(P1,L1,F1,R1),
	kns_pathCond(L1,F1,R1,L2,F2,[],Stat_Prec).


%       path condition:
%
%       for all <gj,sj> in P2 there exists <fi,ti> in P2 such that:


%       a) fi > gi


kns_pathCond(_L1,(F1,_T1),_R1,_L2,(F2,_T2),_R2,Stat_Prec) :-
	kns_opPrec(gt(F1,F2),Stat_Prec).
kns_pathCond(L1,(F1,T1),R1,L2,(F2,T2),R2,extToBy(Stat_Prec,NewPrec,Ext)) :-
	(Ext == [] ->
		Ext1 = [],
		Ext2 = []
	;
		true
	),
	kns_opPrec(eq(F1,F2),extToBy(Stat_Prec,NewPrec1,Ext1)),
	kns_PC3stati(L1,(F1,T1),R1,L2,(F2,T2),R2,extToBy(NewPrec1,NewPrec,Ext2)),
	append(Ext1,Ext2,Ext).


kns_PC3stati(L1,(F1,T1),R1,L2,(F2,T2),R2,Stat_Prec) :-
	kns_PCmsstatus(L1,(F1,T1),R1,L2,(F2,T2),R2,Stat_Prec).
kns_PC3stati(L1,(F1,T1),R1,L2,(F2,T2),R2,Stat_Prec) :-
	kns_PClrstatus(L1,(F1,T1),R1,L2,(F2,T2),R2,Stat_Prec).
kns_PC3stati(L1,(F1,T1),R1,L2,(F2,T2),R2,Stat_Prec) :-
	kns_PCrlstatus(L1,(F1,T1),R1,L2,(F2,T2),R2,Stat_Prec).


%       fi ~ gi and if they have no status (that means multiset status):

kns_PCmsstatus(L1,(F1,T1),R1,L2,(F2,T2),R2,extToBy(Stat_Prec,NewPrec,Ext)) :-
	(Ext == [] ->
		Ext1 = [],
		Ext2 = []
	;
		true
	),
	kns_opPrec(status(F1,ms),extToBy(Stat_Prec,NewPrec1,Ext1)),
	kns_cond1Or2Or3(L1,(F1,T1),R1,L2,(F2,T2),R2,extToBy(NewPrec1,NewPrec,Ext2)),
	append(Ext1,Ext2,Ext).


%       fi ~ gi and if they have lr status:

kns_PClrstatus(L1,(F1,T1),_R1,L2,(F2,T2),_R2,extToBy(Stat_Prec,NewPrec,Ext)) :-
	(Ext == [] ->
		Ext1 = [],
		Ext2 = []
	;
		true
	),
	kns_opPrec(status(F1,lr),extToBy(Stat_Prec,NewPrec1,Ext1)),
	kns_cond1OrCond2(L1,(F1,T1),L2,(F2,T2),extToBy(NewPrec1,NewPrec,Ext2)),
	append(Ext1,Ext2,Ext).


%       fi ~ gi and if they have rl status:

kns_PCrlstatus(_L1,(F1,T1),R1,_L2,(F2,T2),R2,extToBy(Stat_Prec,NewPrec,Ext)) :-
	(Ext == [] ->
		Ext1 = [],
		Ext2 = []
	;
		true
	),
	kns_opPrec(status(F1,rl),extToBy(Stat_Prec,NewPrec1,Ext1)),
	kns_cond1OrCond2(R1,(F1,T1),R2,(F2,T2),extToBy(NewPrec1,NewPrec,Ext2)),
	append(Ext1,Ext2,Ext).


%       1. RC(<fi,ti>,P1) > RC(<gj,sj>,P2)  or

kns_cond1Or2Or3(_L1,(_F1,_T1),R1,_L2,(_F2,_T2),R2,Stat_Prec) :-
	kns_pathGT(R1,R2,Stat_Prec).

%       2. RC(<fi,ti>,P1) ~ RC(<gj,sj>,P2)

kns_cond1Or2Or3(L1,(F1,T1),R1,L2,(F2,T2),R2,extToBy(Stat_Prec,NewPrec,Ext)) :-
	(Ext == [] ->
		Ext1 = [],
		Ext2 = []
	;
		true
	),
	kns_pathEQ(R1,R2,extToBy(Stat_Prec,NewPrec1,Ext1)),
	kns_cond2Or3(L1,(F1,T1),R1,L2,(F2,T2),R2,extToBy(NewPrec1,NewPrec,Ext2)),
	append(Ext1,Ext2,Ext).


%       and  ti > sj   or

kns_cond2Or3(_L1,(_F1,T1),_R1,_L2,(_F2,T2),_R2,Stat_Prec) :-
	kns_GT(T1,T2,Stat_Prec).

%       and  ti ~ sj  and
%       LC(<fi,ti>,P1) > LC(<gj,sj>,P2).

kns_cond2Or3(L1,(_F1,T1),_R1,L2,(_F2,T2),_R2,extToBy(Stat_Prec,NewPrec,Ext)) :-
	(Ext == [] ->
		Ext1 = [],
		Ext2 = []
	;
		true
	),
	kns_termEQ(T1,T2,extToBy(Stat_Prec,NewPrec1,Ext1)),
	kns_pathGT(L1,L2,extToBy(NewPrec1,NewPrec,Ext2)),
	append(Ext1,Ext2,Ext).


%       1. ti > sj  or

kns_cond1OrCond2(_L1,(_F1,T1),_L2,(_F2,T2),SP_Opt) :-
	kns_GT(T1,T2,SP_Opt).

%       2. ti ~ sj and
%
%          LC(<fi,ti>,P1) > LC(<gj,sj>,P2) respectively
%          RC(<fi,ti>,P1) > RC(<gj,sj>,P2).

kns_cond1OrCond2(L1,(_F1,T1),L2,(_F2,T2),extToBy(Stat_Prec,NewPrec,Ext)) :-
	(Ext == [] ->
		Ext1 = [],
		Ext2 = []
	;
		true
	),
	kns_termEQ(T1,T2,extToBy(Stat_Prec,NewPrec1,Ext1)),
	kns_pathGT(L1,L2,extToBy(NewPrec1,NewPrec,Ext2)),
	append(Ext1,Ext2,Ext).


%       s = f(s1,...,sm) and t = g(t1,...,tn) are equivalent <==>
%
%       f ~ g   and
%       m = n   and
%       there is a permutation p of the set {1,...,n} such that
%               si ~ tp(i) for all 1=< i =< n.

kns_termEQ(T,T,extToBy(Stat_Prec,Stat_Prec,[])) :- !.
kns_termEQ(T1,T2,extToBy(Stat_Prec,NewPrec,Ext)) :-
	functor(T1,OF1,N),
	functor(T2,OF2,N),
	OF1 \== @,
	OF2 \== @,
	T1=..[OF1|Args1],
	T2=..[OF2|Args2],
%	osDecompose(T1,F1,OF1,Args1),   % changed 27.11.89 uh
%	osDecompose(T2,F2,OF2,Args2),	% changed 27.11.89 uh
	(Ext == [] ->
		Ext1 = [],
		Ext2 = []
	;
		true
	),
	kns_opPrec(eq(OF1,OF2),extToBy(Stat_Prec,NewPrec1,Ext1)),
	kns_PermWithEQterms(Args1,Args2,extToBy(NewPrec1,NewPrec,Ext2)),
	append(Ext1,Ext2,Ext).



kns_PermWithEQterms([],[],extToBy(Stat_Prec,Stat_Prec,[])) :- !.
kns_PermWithEQterms([T1|Args1],Args2,extToBy(Stat_Prec,NewPrec,Ext)) :-
	(Ext == [] ->
		Ext1 = [],
		Ext2 = []
	;
		true
	),
	'chooseElem'(Args2,L,T2,R),
	kns_termEQ(T1,T2,extToBy(Stat_Prec,NewPrec1,Ext1)),
	append(L,R,LR),
	kns_PermWithEQterms(Args1,LR,extToBy(NewPrec1,NewPrec,Ext2)),
	append(Ext1,Ext2,Ext).


%       Let P1 = <f1,t1>,<f2,t2>...<fm,tm> and
%           P2 = <g1,s1>,<g2,s2>...<gn,sn>.
%
%       P1 and P2 are equivalent <==>
%
%       m = n   and
%
%       fi ~ gi and ti ~ si  for all 1 =< i =< n.

kns_pathEQ(P,P,extToBy(Stat_Prec,Stat_Prec,[])).
kns_pathEQ(R1,R2,Stat_Prec) :-
	R1 \== R2,
	kns_equalLength(R1,R2),
	kns_pathEQcond(R1,R2,Stat_Prec).


kns_pathEQcond([(Fi,Ti)|R1],[(Gi,Si)|R2],extToBy(Stat_Prec,NewPrec,Ext)) :-
	(Ext == [] ->
		Ext1 = [],
		Ext2 = [],
		Ext4 = []
	;
		true
	),
	kns_opPrec(eq(Fi,Gi),extToBy(Stat_Prec,NewPrec1,Ext1)),
	kns_termEQ(Ti,Si,extToBy(NewPrec1,NewPrec2,Ext2)),
	append(Ext1,Ext2,Ext3),
	kns_pathEQcond(R1,R2,extToBy(NewPrec2,NewPrec,Ext4)),
	append(Ext3,Ext4,Ext).
