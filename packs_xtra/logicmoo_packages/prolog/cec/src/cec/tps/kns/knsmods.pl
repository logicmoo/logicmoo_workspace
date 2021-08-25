/*
 *	file:		knsmods.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains ..
 *
 *	history:
 *	891010	js	Added this comment
 *	891127	uh	Changed definition of
 *			kns_isAFullPath/2
 *			to allow the use of disambiguated order-sorted
 *			operators O/N
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_orient/5					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_orient(L,R,L,R,asGiven) :- !.
kns_orient(L,R,R,L,reverse) :- !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_orient_extendPrec/8				%
%			kns_oePrec/8					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_orient_extendPrec(Nr,Options,L,R,NL,NR,Ext,AskedDir) :-
	kns_oePrec(Nr,Options,L,R,NL,NR,Ext,AskedDir),
	!.
kns_orient_extendPrec(Nr,Options,L,R,NL,NR,Ext,AskedDir) :-
	Nr \== n,
	nl,
	write('***error***, your choice?(#.): '),
	read(Nr2),
	kns_orient_extendPrec(Nr2,Options,L,R,NL,NR,Ext,AskedDir),
	!.


kns_oePrec(Nr,[opt(Nr,'-->',Ext)|_Options],L,R,L,R,Ext,asGiven) :-
	number(Nr),
	!.
kns_oePrec(Nr,[opt(Nr,'<--',Ext)|_Options],L,R,R,L,Ext,reverse) :-
	number(Nr),
	!.
kns_oePrec(Nr,[opt(_N,_Dir,_Ext)|Options],L,R,NL,NR,Ext2,AskedDir) :-
	number(Nr),
	kns_oePrec(Nr,Options,L,R,NL,NR,Ext2,AskedDir),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_numerate/3					%
%			kns_optionCount/5				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_numerate([],[],[]) :- !.
kns_numerate(RightOptionSet,[],Options1) :-
	kns_optionCount(1,'-->',RightOptionSet,Options1,_Nr),
	!.
kns_numerate([],LeftOptionSet,Options1) :-
	kns_optionCount(1,'<--',LeftOptionSet,Options1,_Nr),
	!.
kns_numerate(RightOptionSet,LeftOptionSet,Options) :-
	kns_optionCount(1,'-->',RightOptionSet,Options1,Nr),
	N is Nr + 1,
	kns_optionCount(N,'<--',LeftOptionSet,Options2,_),
	append(Options1,Options2,Options),
	!.


kns_optionCount(N,Dir,[Opt|OptionSet],[opt(N,Dir,Opt)|NewOptions],HighestNr) :-
	Nnew is N + 1,
	kns_optionCount(Nnew,Dir,OptionSet,NewOptions,HighestNr),
	!.
kns_optionCount(N,Dir,[Opt|[]],[opt(N,Dir,Opt)|[]],N) :- !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_addSuccs/3					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_addSuccs([AltsWithSucc1],[],[AltsWithSucc1]) :- !.
kns_addSuccs([[Pair]],[Pairs],[[Pair|Pairs]]) :- !.
kns_addSuccs([[Pair]],[Pairs|AltsWithSucc2],
		[[Pair|Pairs]|AltsWithSucc]) :-
	kns_addSuccs([[Pair]],AltsWithSucc2,AltsWithSucc),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_hasVar/2					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_hasVar(@(V),V) :- !.
kns_hasVar(T,V) :-
	T=..[_F|Args],
	member(Arg,Args),
	kns_hasVar(Arg,V),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_multiSetGT/4				%
%			kns_multiSetCond/4				%
%			kns_withoutMS/3					%
%			kns_removeMS/3					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_multiSetGT(ElemGT,M1,M2,SP_Opt) :-
	M1 \== M2,
	kns_withoutMS(M1,M2,NewM1),
	kns_withoutMS(M2,M1,NewM2),
	kns_multiSetCond(ElemGT,NewM1,NewM2,SP_Opt).

%       NewM1 >> NewM2 <==>
%           for each x el NewM2
%                   there is a  y el NewM1  such that
%               y > x.


kns_multiSetCond(_ElemGT,M1,[],extToBy(Stat_Prec,Stat_Prec,[])) :-
	M1 \== [],
	!.
kns_multiSetCond(ElemGT,M1,[X|M2],extToBy(Stat_Prec,NewPrec,Ext)) :-
	(	Ext == [] ->

		(	Ext1 = [],
			Ext2 = []
		)
	;
		true
	),
	member(Y,M1),
	Call =..[ElemGT,Y,X,extToBy(Stat_Prec,NewPrec1,Ext1)],
	Call,
	kns_multiSetCond(ElemGT,M1,M2,extToBy(NewPrec1,NewPrec,Ext2)),
	append(Ext1,Ext2,Ext).


kns_withoutMS([],_ArgsR,[]) :- !.
kns_withoutMS([Arg|ArgsL],ArgsR,NewL) :-
	kns_removeMS(Arg,ArgsR,NewArgsR),
	kns_withoutMS(ArgsL,NewArgsR,NewL),
	!.
kns_withoutMS([Arg|ArgsL],ArgsR,[Arg|NewL]) :-
	kns_withoutMS(ArgsL,ArgsR,NewL),
	!.


kns_removeMS(Arg,ArgsR,NewArgsR) :-
	'chooseElem'(ArgsR,L,Arg,R),
	append(L,R,NewArgsR),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_MP/2					%
%			kns_allFullPaths/2				%
%			kns_isAFullPath/2				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%       generating multisets of pathes:

kns_MP([],[]) :- !.
kns_MP([@(V)],[[(@,V)]]) :- !.
kns_MP([T],MP) :-
	kns_allFullPaths(T,MP),
	!.
kns_MP([T|Ts],Union) :-
	kns_allFullPaths(T,MP),
	kns_MP(Ts,MPs),
	append(MP,MPs,Union),
	!.


kns_allFullPaths(@(V),[[(@,V)]]) :- !.
kns_allFullPaths(T,MP) :-
	setof(P,kns_isAFullPath(T,P),MP),
	!.
kns_allFullPaths(_T,[]) :- !.


%       A full path of a term t is defined as follows:
%
%       a) If t = x, a variable, then x is the only full path in t.
%
%       b) If t = b, a constant, then <b,b> is the only full path in t.
%
%       c) If t = f(t1,...,tm) then a full path in t
%               is <f,t>.p   where p is a full path in some ti.
%

kns_isAFullPath(@(V),[(@,V)]) :- !.
kns_isAFullPath(T,[(Op,T)]) :-
	atomic(T),
	osOpName(T,0,Op),
	!.
kns_isAFullPath(T,[(F,T)|PTi]) :-
	osDecompose(T,F,_,Tis),    % changed 27.11.89 uh
	member(Ti,Tis),
	kns_isAFullPath(Ti,PTi).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_isComparable/4				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_isComparable(P1,P2,DropP1,DropP2) :-
	'chooseElem'(P1,DropP1,(@,V),[]),
	'chooseElem'(P2,DropP2,(@,V),[]),
	!.
kns_isComparable(P1,P2,P1,P2) :-
	'chooseElem'(P1,_L1,F1,[]),
	'chooseElem'(P2,_L2,F2,[]),
	not((F1 = (@,_) , F2 = (@,_))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_deleteCommonSuffix/4			%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_deleteCommonSuffix(P,P,[],[]) :- !.
kns_deleteCommonSuffix(P,[],P,[]) :- !.
kns_deleteCommonSuffix([],P,[],P) :- !.
kns_deleteCommonSuffix(P1,P2,L1,[]) :-
	append(L1,P2,P1),
	!.
kns_deleteCommonSuffix(P1,P2,[],L2) :-
	append(L2,P1,P2),
	!.
kns_deleteCommonSuffix(P1,P2,NP1,NP2) :-
	append(L1,[F1|Su],P1),
	append(L2,[F2|Su],P2),
	F1 \== F2,
	!,
	append(L1,[F1],NP1),
	append(L2,[F2],NP2),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_opPrec/2					%
%			kns_gtplus/3					%
%			kns_gtplus/4					%
%			kns_eqstar/3					%
%			kns_eqstar/4					%
%			kns_statusstar/3				%
%			kns_statusstar/4				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_opPrec(eq(Op,Op),extToBy(Stat_Prec,Stat_Prec,[])) :-
	Op \== @,
	!.
kns_opPrec(eq(Op1,Op2),extToBy(Stat_Prec,Stat_Prec,[])) :-
	tps_current_ordering(kns),
	Op1 \== @,       % Variables don't have a precedence
	Op2 \== @,       % Variables don't have a precedence
	kns_eqstar(Op1,Op2,Stat_Prec),
	!.
kns_opPrec(eq(Op1,Op2),
           extToBy(Stat_Prec,[eq(Op1,Op2)|Stat_Prec],[eq(Op1,Op2)])) :-
	tps_current_ordering(kns),
	Op1 \== @,       % Variables don't have a precedence
	Op2 \== @,       % Variables don't have a precedence
	kns_isConsistent(eq(Op1,Op2),Stat_Prec),
	!.
kns_opPrec(gt(Op1,Op2),extToBy(Stat_Prec,Stat_Prec,[])) :-
	Op1 \== @,       % Variables don't have a precedence
	Op2 \== @,       % Variables don't have a precedence
	kns_gtplus(Op1,Op2,Stat_Prec),
	!.
kns_opPrec(gt(Op1,Op2),
           extToBy(Stat_Prec,[gt(Op1,Op2)|Stat_Prec],[gt(Op1,Op2)])) :-
	Op1 \== @,       % Variables don't have a precedence
	Op2 \== @,       % Variables don't have a precedence
	kns_isConsistent(gt(Op1,Op2),Stat_Prec),
	!.
kns_opPrec(status(Op,St),extToBy(Stat_Prec,Stat_Prec,[])) :-
	Op \== @,       % Variables don't have a precedence
	kns_statusstar(Op,St,Stat_Prec),	
	!.
kns_opPrec(status(Op,St),
	   extToBy(Stat_Prec,[status(Op,St)|Stat_Prec],[status(Op,St)])) :-
	Op \== @,       % Variables don't have a precedence
	kns_isConsistent(status(Op,St),Stat_Prec),
	!.
kns_opPrec(notgtOreq(Op1,Op2),extToBy(Stat_Prec,Stat_Prec,[])) :-
	Op1 \== @,
	Op2 \== @,
	member(notgtOreq(Op1,Op2),Stat_Prec),
	!.
kns_opPrec(notgtOreq(Op1,Op2),
	   extToBy(Stat_Prec,[notgtOreq(Op1,Op2)|Stat_Prec],Ext)) :-
	var(Ext),
	!,
	Op1 \== @,
	Op2 \== @,
	kns_isConsistent(notgtOreq(Op1,Op2),Stat_Prec),
	Ext = [notgtOreq(Op1,Op2)],
	!.
kns_opPrec(notgtOreq(Op1,Op2),extToBy(Stat_Prec,Stat_Prec,[])) :-
	Op1 \== @,
	Op2 \== @,
	kns_isConsistent(notgtOreq(Op1,Op2),Stat_Prec), 
	!.


kns_gtplus(A,B,_Stat_Prec):-
	A=B,
	!,  
	fail.
kns_gtplus(A,B,Stat_Prec) :-
	\+ kns_eqstar(A,B,Stat_Prec),
	kns_gtOrEqstar(A,B,Stat_Prec).




impliesGtinPrec(Stat_Prec,Stat_Prec1):-
	member(gt(O1,O2),Stat_Prec),
	\+ member(gt(O1,O2),Stat_Prec1),
	!,
	fail.
impliesGtinPrec(Stat_Prec,Stat_Prec1):-
	member(eq(O1,O2),Stat_Prec),
	\+ member(eq(O1,O2),Stat_Prec1),
	!,
	fail.
impliesGtinPrec(_,_).

impliesEqinPrec(Stat_Prec,Stat_Prec1):-
	member(eq(O1,O2),Stat_Prec),
	\+ member(eq(O1,O2),Stat_Prec1),
	!,
	fail.
impliesEqinPrec(_,_).


impliesNotGtOrEqinPrec(Stat_Prec,Stat_Prec1):-
	member(notgtOreq(O1,O2),Stat_Prec),
	\+ member(notgtOreq(O1,O2),Stat_Prec1),
	!,
	fail.
impliesNotGtOrEqinPrec(Stat_Prec,Stat_Prec1):-
	member(eq(O1,O2),Stat_Prec),
	\+ member(eq(O1,O2),Stat_Prec1),
	!,
	fail.
impliesNotGtOrEqinPrec(_,_).


kns_gtplus('$current',A,B,Stat_Prec) :-
	!,
	kns_gtplus(A,B,Stat_Prec).
kns_gtplus(W,A,B,Stat_Prec) :-
	\+ kns_eqstar(W,A,B,Stat_Prec),
	kns_gtOrEqstar(W,A,B,Stat_Prec).




kns_eqstar(A,A,_Stat_Prec) :-
	!.
kns_eqstar(_,_,_Stat_Prec) :-
	tps_current_ordering(neqkns),
	!,
	fail.
kns_eqstar(A,B,Stat_Prec) :-
	kns_eqstar_positive(A,B,Stat_Prec1),
	impliesEqinPrec(Stat_Prec1,Stat_Prec),
	!.
kns_eqstar(A,B,Stat_Prec) :-
	kns_eqstar_negative(A,B,Stat_Prec1),
	impliesEqinPrec(Stat_Prec,Stat_Prec1),
	!,
	fail.
kns_eqstar(A,B,Stat_Prec) :-
	kns_eqstar('$current',A,B,Stat_Prec),
	assert(kns_eqstar_positive(A,B,Stat_Prec)).
kns_eqstar(A,B,Stat_Prec) :-
	assert(kns_eqstar_negative(A,B,Stat_Prec)),
	!,
	fail.



kns_eqstar(_W,A,A,_Stat_Prec) :-
	!.
kns_eqstar(W,A,B,Stat_Prec) :-
	reflTransClosure(lambda([X,Y],kns_eqReq(W,X,Y,Stat_Prec)),A,B).



kns_statusstar(Op,St,Stat_Prec) :-
	kns_statusstar('$current',Op,St,Stat_Prec).


kns_statusstar(W,Op,St,Stat_Prec) :-
	tps_current_ordering(kns),
	!,
	kns_eqstar(W,Op,B,Stat_Prec),
	kns_status(W,B,St,Stat_Prec).
kns_statusstar(W,Op,St,Stat_Prec) :-
	kns_status(W,Op,St,Stat_Prec).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_gt/4					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_gt('$current',C,D,_Stat_Prec) :-
	kns_gt(C,D).
kns_gt('$current',C,D,Stat_Prec) :-
	!,
	member(gt(C,D),Stat_Prec).
kns_gt(W,C,D,_Stat_Prec) :-
	W:kns_gt(C,D).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_eqReq/4					%
%			kns_eq/4					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_eqReq(W,C,D,Stat_Prec) :-
	kns_eq(W,C,D,Stat_Prec).
kns_eqReq(W,C,D,Stat_Prec) :-
	kns_eq(W,D,C,Stat_Prec).


kns_eq('$current',C,D,_Stat_Prec) :-
	kns_eq(C,D).
kns_eq('$current',C,D,Stat_Prec) :-
	!,
	member(eq(C,D),Stat_Prec).
kns_eq(W,C,D,_Stat_Prec) :-
	W:kns_eq(C,D).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_status/4					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_status('$current',B,St,_Stat_Prec) :-
	kns_status(B,St).
kns_status('$current',B,St,Stat_Prec) :-
	!,
	member(status(B,St),Stat_Prec).
kns_status(W,B,St,_Stat_Prec) :-
	W:kns_status(B,St).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_chooseElem/5				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_chooseElem([F|T],[],X1,X2,T) :-
	(	F = X1
	;
		F = X2
	).
kns_chooseElem([F|T],[F|L],X1,X2,R) :-
	kns_chooseElem(T,L,X1,X2,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_isConsistent/2				%
%			kns_isConsistent/3				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_isConsistent(Prec,Stat_Prec) :-
	kns_isConsistent_positive(Prec,Stat_Prec1),
	impliesConsistency(Prec,Stat_Prec1,Stat_Prec),
	!.
kns_isConsistent(Prec,Stat_Prec) :-
	kns_isConsistent_negative(Prec,Stat_Prec1),
	impliesInconsistency(Prec,Stat_Prec1,Stat_Prec),
	!,
	fail.
kns_isConsistent(Prec,Stat_Prec) :-
	kns_isConsistent('$current',Prec,Stat_Prec),
	assert(kns_isConsistent_positive(Prec,Stat_Prec)),
	!.
kns_isConsistent(Prec,Stat_Prec) :-
	assert(kns_isConsistent_negative(Prec,Stat_Prec)),
	!,
	fail.



impliesConsistency(gt(_O,_P),S1,S2):-
	% O>P inconsistent S2  =>  
	%	S2 |- P>O or S2 |- O notgtOrEq P  or S2 |- O=P
	impliesGtinPrec(S2,S1),
	impliesNotGtOrEqinPrec(S2,S1),
	impliesEqinPrec(S2,S1).
	% => S1 |- O notgtOrEq P
impliesConsistency(eq(_O,_P),S1,S2):-
	% eq(O,P) inconsistent S2 
	%	=> S2 |- A>B or S2 |- B>A or S2 |- A notgtOreq B
	impliesGTinPrec(S2,S1),
	impliesNotGtOrEqinPrec(S2,S1).
	%	=> S1 |- A>B or S1 |- B>A  or S1 |- A notgtOreq B
impliesConsistency(notgtOreq(_O,_P),S1,S2):-
	% notgtOreq(O,P) inc. S2  => S2 |- O>P or S2 |- O=P
	impliesGtinPrec(S2,S1),
	impliesEqinPrec(S2,S1).
impliesConsistency(status(O,St),_S1,S2):-
	member(status(O,St),S2).
impliesConsistency(status(O,_),S1,S2):-
	\+member(status(O,_),S2),
	impliesEqinPrec(S2,S1).

impliesInconsistency(gt(_O,_P),S2,S1):-	% note the exchanged order S2 S1 !
	% O>P inconsistent S2  =>  
	%	S2 |- P>O or S2 |- O notgtOrEq P  or S2 |- O=P
	impliesGtinPrec(S2,S1),
	impliesNotGtOrEqinPrec(S2,S1),
	impliesEqinPrec(S2,S1).
	% => S1 |- O notgtOrEq P
impliesInconsistency(eq(_O,_P),S2,S1):-
	% eq(O,P) inconsistent S2 
	%	=> S2 |- A>B or S2 |- B>A or S2 |- A notgtOreq B
	impliesGtinPrec(S2,S1),
	impliesNotGtOrEqinPrec(S2,S1).
	%	=> S1 |- A>B or S1 |- B>A  or S1 |- A notgtOreq B
impliesInconsistency(notgtOreq(_O,_P),S2,S1):-
	% notgtOreq(O,P) inc. S2  => S2 |- O>P or S2 |- O=P
	impliesGtinPrec(S2,S1),
	impliesEqinPrec(S2,S1).
impliesInconsistency(status(O,St),_S2,S1):-
	member(status(O,St1),S1),
	St1\==St.
impliesInconsistency(status(O,_),S2,S1):-
	\+member(status(O,_),S2),
	impliesEqinPrec(S2,S1).






kns_isConsistent(W,notgtOreq(Op1,Op2),Stat_Prec) :-
	tps_current_ordering(kns),
	Op1 \== Op2,
	!,
	\+ kns_gtOrEqstar(W,Op1,Op2,Stat_Prec),	
	!.
kns_isConsistent(W,notgtOreq(Op1,Op2),Stat_Prec) :-
	Op1 \== Op2,
	!,
	\+ kns_gtplus(W,Op1,Op2,Stat_Prec),
	!.

kns_isConsistent(W,gt(Op1,Op2),Stat_Prec) :-
	Op1 \== Op2,
	\+ kns_hhConstructor(W,Op1),
	\+ kns_notgtOreqstar(W,Op1,Op2,Stat_Prec),
	\+ kns_gtOrEqstar(W,Op2,Op1,Stat_Prec),
	!.
kns_isConsistent(_W,eq(A,A),_Stat_Prec) :-!.
kns_isConsistent(W,eq(Op1,Op2),Stat_Prec) :-
	tps_current_ordering(kns),
	\+ kns_notgtOreqstar(W,Op1,Op2,Stat_Prec),	
	\+ kns_gtplus(W,Op1,Op2,Stat_Prec),
	\+ kns_gtplus(W,Op2,Op1,Stat_Prec),
	!.
kns_isConsistent(W,status(Op,St),Stat_Prec) :-
	kns_statusstar(W,Op,Status,Stat_Prec),
	!,
	Status == St,
	!.
kns_isConsistent(_W,status(_Op,_St),_Stat_Prec) :-
	% Es gibt keinen Status fuer Op
%	\+ kns_statusstar(W,Op,_Status,Stat_Prec),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_notgtOreqstar/4				%
%			kns_gtOrEqstar/4				%
%			kns_gtOrEq/4					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_notgtOreqstar(W,Op1,Op2,Stat_Prec) :-
	tps_current_ordering(kns),
	!,
	member(notgtOreq(A,B),Stat_Prec),
	kns_eqstar(W,Op1,A,Stat_Prec),
	kns_eqstar(W,B,Op2,Stat_Prec).
kns_notgtOreqstar(_W,A,B,Stat_Prec) :-
	member(notgtOreq(A,B),Stat_Prec).

kns_gtOrEqstar(Op2,Op1,_Stat_Prec) :-
	isConstructor(Op1),
	\+ isConstructor(Op2),
	!.
kns_gtOrEqstar(Op2,Op1,_Stat_Prec) :-
	isConstructor(Op2),
	\+ isConstructor(Op1),
	!,
	fail.
kns_gtOrEqstar(Op2,Op1,Stat_Prec) :-
	kns_gtOrEqstar_positive(Op2,Op1,Stat_Prec1),
	impliesGtinPrec(Stat_Prec1,Stat_Prec),
	!.
kns_gtOrEqstar(Op2,Op1,Stat_Prec) :-
	kns_gtOrEqstar_negative(Op2,Op1,Stat_Prec1),
	impliesGtinPrec(Stat_Prec,Stat_Prec1),
	!,
	fail.
kns_gtOrEqstar(Op2,Op1,Stat_Prec) :-
	reflTransClosure(lambda([X,Y],kns_gtOrEq(X,Y,Stat_Prec)),Op2,Op1),
	assert(kns_gtOrEqstar_positive(Op2,Op1,Stat_Prec)).
kns_gtOrEqstar(Op2,Op1,Stat_Prec) :-
	assert(kns_gtOrEqstar_negative(Op2,Op1,Stat_Prec)),
	!,
	fail.


kns_gtOrEqstar(W,Op2,Op1,_Stat_Prec) :-
	kns_hhConstructor(W,Op1),
	\+ kns_hhConstructor(W,Op2),
	!.
kns_gtOrEqstar(W,Op2,Op1,Stat_Prec) :-
	reflTransClosure(lambda([X,Y],kns_gtOrEq(W,X,Y,Stat_Prec)),Op2,Op1).


kns_gtOrEq(A,B,_Stat_Prec) :-
	kns_gt(A,B).
kns_gtOrEq(A,B,Stat_Prec) :-
	member(gt(A,B),Stat_Prec).
kns_gtOrEq(A,B,Stat_Prec) :-
	tps_current_ordering(kns),
	kns_eqReq('$current',A,B,Stat_Prec).

kns_gtOrEq(W,A,B,Stat_Prec) :-
	kns_gt(W,A,B,Stat_Prec).
kns_gtOrEq('$current',A,B,Stat_Prec) :-
	!,
	tps_current_ordering(kns), 
	kns_eqReq('$current',A,B,Stat_Prec).
kns_gtOrEq(W,A,B,Stat_Prec) :-
	kns_eqReq(W,A,B,Stat_Prec).
% kns_gtOrEq(_W,A,A,_Stat_Prec).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_hhConstructor/2				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_hhConstructor('$current',Op1) :-
	!,
	isConstructor(Op1).
kns_hhConstructor(W,Op1) :-
	W:isConstructor(Op1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			    INTERFACE:					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_getopPrecStat/1				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_getopPrecStat(Prec) :-
	tps_getCurrentTPS(tps(_,_,kns_state(_,Prec))),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_makeopPrecStat/1				%
%			kns_makePrecedence/1				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_makeopPrecStat([]) :- 
	!.
kns_makeopPrecStat([P|PList]) :-
	kns_makePrecedence(P),
	kns_makeopPrecStat(PList).


kns_makePrecedence(gt(A,B)) :-
	adjustMemos(gt(A,B)),
	assert(kns_gt(A,B)).
kns_makePrecedence(eq(A,B)) :-
	adjustMemos(eq(A,B)),
	assert(kns_eq(A,B)).
kns_makePrecedence(status(A,B)) :-
	adjustMemos(eq(A,B)),
	assert(kns_status(A,B)).
kns_makePrecedence(notgtOreq(_A,_B)).





adjustMemos(Prec):-
	retract(kns_isConsistent_positive(P,S)),
	(append(S1,[Prec|S2],S) ->
		append(S1,S2,S3),
		asserta(kns_isConsistent_positive(P,S3))),
	fail.
adjustMemos(Prec):-
	retract(kns_isConsistent_negative(P,S)),
	(append(S1,[Prec|S2],S) ->
		append(S1,S2,S3),
		asserta(kns_isConsistent_negative(P,S3))),
	fail.
adjustMemos(Prec):-
	(Prec=gt(_,_);Prec=eq(_,_)),
	retract(kns_gtOrEqstar_positive(A,B,S)),
	(append(S1,[Prec|S2],S) ->
		append(S1,S2,S3),
		asserta(kns_gtOrEqstar_positive(A,B,S3))),
	fail.
adjustMemos(Prec):-
	(Prec=gt(_,_);Prec=eq(_,_)),
	retract(kns_gtOrEqstar_negative(A,B,S)),
	(append(S1,[Prec|S2],S) ->
		append(S1,S2,S3),
		asserta(kns_gtOrEqstar_negative(A,B,S3))),
	fail.
adjustMemos(Prec):-
	(Prec=gt(_,_);Prec=eq(_,_)),
	retract(kns_eqstar_positive(A,B,S)),
	(append(S1,[Prec|S2],S) ->
		append(S1,S2,S3),
		asserta(kns_eqstar_positive(A,B,S3))),
	fail.
adjustMemos(Prec):-
	(Prec=gt(_,_);Prec=eq(_,_)),
	retract(kns_gtOrEqstar_negative(A,B,S)),
	(append(S1,[Prec|S2],S) ->
		append(S1,S2,S3),
		asserta(kns_gtOrEqstar_negative(A,B,S3))),
	fail.
adjustMemos(_).
	



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_gtprec/1					%
%			kns_gtOrder/1					%
%			kns_declPrecGT/2				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_gtprec([]) :- !.
kns_gtprec([Order|Lists]) :-
	kns_gtOrder(Order),
	kns_gtprec(Lists),
	!.


kns_gtOrder([]) :- !.
kns_gtOrder([_]) :- !.
kns_gtOrder([A,B|List]) :-
	kns_declPrecGT(A,B),
	kns_gtOrder([B|List]),
	!.


kns_declPrecGT(Op1,Op2) :-
	kns_opPrec(gt(Op1,Op2),extToBy([],NewPrec,_)),
	kns_makeopPrecStat(NewPrec),
	!.
kns_declPrecGT(Op1,Op2) :-
	sPrint(" *** precedence extension ""% > %"" not possible - ignored.",
	       [Op1,Op2]),
	nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_eqprec/1					%
%			kns_eqOrder/1					%
%			kns_declPrecEQ/2				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Achtung: Bei Inkonsistenzen sind Deklarationen teilweise erfolgt !!!!!
%
kns_eqprec([]) :- !.
kns_eqprec([Order|Lists]) :-
	kns_eqOrder(Order),
	kns_eqprec(Lists),
	!.


kns_eqOrder([]) :- !.
kns_eqOrder([_]) :- !.
kns_eqOrder([A,B|List]) :-
	kns_declPrecEQ(A,B),
	kns_eqOrder([B|List]),
	!.


kns_declPrecEQ(Op1,Op2) :-
	kns_opPrec(eq(Op1,Op2),extToBy([],NewPrec,_)),
	kns_makeopPrecStat(NewPrec),
	!.

kns_declPrecEQ(Op1,Op2) :-
	sPrint(" *** precedence extension ""% = %"" not possible - ignored.",
	       [Op1,Op2]),
	nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_statusprec/2				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_statusprec(Op,ISt) :-
	kns_opPrec(status(Op,St),extToBy([],[],[])),
	(St =ISt ->
		true
	;
		sPrint("*** The current status of ""%"" is ""%"" and
    cannot be redefined to ""%"" - ignored.",[Op,St,ISt])
	)
	,
	!.

kns_statusprec(Op,ISt) :-
	kns_makeopPrecStat([status(Op,ISt)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_nonConstructors/1				%
%			kns_isOperatorIn/1				%
%			kns_constructors/1				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_nonConstructors(Ops) :-
	setof(Op,(kns_isOperatorIn(Op),\+ isConstructor(Op)),Ops),
	!.


kns_isOperatorIn(Op) :-
	kns_gt(Op,_).
kns_isOperatorIn(Op) :-
	kns_gt(_,Op).
kns_isOperatorIn(Op) :-
	kns_eq(Op,_).
kns_isOperatorIn(Op) :-
	kns_eq(_,Op).
kns_isOperatorIn(Op) :-
	kns_status(Op,_).


kns_constructors(C) :-
	setof(Op,hhConstructor(Op),C),
	!.


%********************   I / O   ********************************************

kns_headline(L,R):-
	sPrint("

Consider the equation
	%.

",['$singleEq'(L,R)]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_askUserForDirection/3			%
%			kns_userDecision/2				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_askUserForDirection(L,R,AskedDir) :-
	kns_headline(L,R),
	sPrint("
This pair can be ordered in both directions.
For which direction would you like to have precedence suggestions ?
                     1.   ----->
                     2.   <-----
your choice?(#.):",[]),
	read(Nr),
	kns_userDecision(Nr,AskedDir),
	!.


kns_userDecision(1,asGiven) :- !.
kns_userDecision(2,reverse) :- !.
kns_userDecision(_Nr,AskedDir) :-
	nl,
	write('***error***, your choice?(#.): '),
	read(Nr2),
	kns_userDecision(Nr2,AskedDir),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_showOptions/3				%
%			kns_dispOpt/1					%
%			kns_dispPrecSug/1				%
%			kns_dispPrec/1					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_showOptions(L,R,Options) :-
	kns_headline(L,R),
	(length(Options,1) ->
         Li='To order this pair you have to extend the precedence as follows:';
	 Li='To order this pair choose one of the following suggestions:'),
	sPrint("
%

    Direction   Suggestions
    ---------   -----------",[Li]),
	kns_dispOpt(Options),
	!.


kns_dispOpt([opt(N,Dir,Ext)|Options]) :-
	nl,
	(N < 10 ->
		write(' '),
		write(N)
	;
		write(N)
	),
	write('.    '),
	write(Dir),
	write('       '),
	kns_dispPrecSug(Ext),
	kns_dispOpt(Options),
	!.
kns_dispOpt([]) :-
	nl,
	write(' n.'),
	write('     '),
	write('no choice'),
	nl,
	!.


%kns_dispPrecSug([notgt(Op1,Op2),noteq(Op1,Op2)|Exts]) :-
%	write('not('),
%	write(Op1),
%	write(' ''>'' '),
%	write(Op2),
%	write(') and not('),
%	write(Op1),
%	write(' ''='' '),
%	write(Op2),
%	write(')'),
%	kns_dispPrecSug(Exts),
%	!.
kns_dispPrecSug([Ext|Exts]) :-
	kns_dispPrec(Ext),
	length(Exts,LE),
	(LE=0 -> true ; write(' and ')),
	kns_dispPrecSug(Exts),
	!.
kns_dispPrecSug(_) :- !.


kns_dispPrec(gt(A,B)) :-
	write(''''),
	write(A),
	write(''''),
	write(' > '),
	write(''''),
	write(B),
	write(''''),
	!.
kns_dispPrec(eq(A,B)) :-
	write(''''),
	write(A),
	write(''''),
	write(' = '),
	write(''''),
	write(B),
	write(''''),
	!.
kns_dispPrec(status(A,ms)) :-
	write(''''),
	write(A),
	write(''''),
	write(' ms'),
	!.
kns_dispPrec(status(A,rl)) :-
	write(''''),
	write(A),
	write(''''),
	write(' rl'),
	!.
kns_dispPrec(status(A,lr)) :-
	write(''''),
	write(A),
	write(''''),
	write(' lr'),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_displayOpPrec/1				%
%			kns_dispOpElems/2				%
%			kns_dispOpElem/2				%
%			kns_eliminateOps/3				%
%			kns_ordersorted/1				%
%			kns_manysorted/1				%
%			kns_dispConsElems/2				%
%			kns_dispConsRest/2				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Mode is element of {all,os,ms}

kns_displayOpPrec(Mode) :-
	nl,
	write('     operator precedence:'),
	nl,
	nl,
	kns_nonConstructors(NC),
	kns_dispOpElems(Mode,[],_DisplayedClasses,NC,NC),
	kns_constructors(C),
	nl,
	nl,
	write('     There exist the following constructors:'),
	nl,
	nl,
	kns_dispConsElems(Mode,C),
	nl,
	!.
kns_displayOpPrec(_Mode).

kns_dispOpElems(_Mode,DisplayedClasses,DisplayedClasses,[],_NC):- !.
kns_dispOpElems(Mode,DisplayedClasses,NewDisplayedClasses,[Op|Ops],NC) :-
	kns_class(Op,C),
	(kns_isClassIn(C,DisplayedClasses)->
		kns_dispOpElems(Mode,DisplayedClasses,NewDisplayedClasses,Ops,NC)
	;
		kns_dispOpElem(Mode,C,NC),
		kns_dispOpElems(Mode,[C|DisplayedClasses],NewDisplayedClasses,Ops,NC)
	).

kns_isClassIn(C,[C1|_DisplayedClasses]):-
	kns_equalClasses(C,C1),
	!.
kns_isClassIn(C,[_C1|DisplayedClasses]):-
	kns_isClassIn(C,DisplayedClasses).

kns_equalClasses(C1,C2):-
	kns_subset(C1,C2),
	!.

kns_subset(C,C):-!.
kns_subset(C1,C2):-
	kns_properSubset(C1,C2).

kns_properSubset([],[_|_]):- !.
kns_properSubset([Op|C1],C2):-
	member(Op,C2),
	kns_properSubset(C1,C2),
	!.

	
	
kns_dispOpElem(Mode,[Op|C],NCSet) :-
	setof1(Op2,
	      (	member(Op2,NCSet),
	       	kns_gtplus(Op,Op2,extToBy([],[],[]))),
	       Ops),
	kns_eliminateOps(Mode,Ops,ModOps),
%	kns_eliminateConstr(ModOps,NewModOps),
%	kns_printGT([Op|C],NewModOps),
	kns_printGT([Op|C],ModOps),
	!.
kns_dispOpElem(_Mode,_Op,_NCSet).

kns_eliminateConstr([],[]):- !.
kns_eliminateConstr([Op|ModOps],NewModOps):-
	kns_eliminateConstr(ModOps,NewModOps1),
	(isConstructor(Op)->
		NewModOps = NewModOps1
	;
		NewModOps = [Op|NewModOps1]
	).

kns_eliminateOps(all,Ops,Ops).			% rs 5.01.89
kns_eliminateOps(_Mode,[],[]).
kns_eliminateOps(os,[Op|Ops],[Op|NewOps]) :-
	kns_ordersorted(Op),
	!,
	kns_eliminateOps(os,Ops,NewOps).
kns_eliminateOps(os,[_Op|Ops],NewOps) :-
	kns_eliminateOps(os,Ops,NewOps).
kns_eliminateOps(ms,[Op|Ops],[Op|NewOps]) :-
	kns_manysorted(Op),
	!,
	kns_eliminateOps(ms,Ops,NewOps).
kns_eliminateOps(ms,[_Op|Ops],NewOps) :-
	kns_eliminateOps(ms,Ops,NewOps).


kns_ordersorted(Op) :-
	name('$',[D]),
	name(Op,[D|_Rest]),
	!,
	fail.
kns_ordersorted(Op) :-
	name(Op,[_|Charlist]),
	name('-',[M]),
	member(M,Charlist),
	!,
	fail.
kns_ordersorted(_Op).
	

kns_manysorted(Op) :-
	name(Op,[_|Charlist]),
	name('-',[M]),
	member(M,Charlist).
kns_manysorted(Op) :-
	name('$',[D]),
	name(Op,[D|_Rest]),
	!,
	fail.
kns_manysorted(_Op) :-
	fail.


kns_dispConsElems(_Mode,[]).
kns_dispConsElems(all,[C|ConsSet]) :-
	print(C),
	kns_dispConsRest(all,ConsSet).
kns_dispConsElems(os,[C|ConsSet]) :-
	(kns_ordersorted(C) ->
		print(C),
		kns_dispConsRest(os,ConsSet)
	;
		kns_dispConsElems(os,ConsSet)
	).
kns_dispConsElems(ms,[C|ConsSet]) :-
	(kns_manysorted(C) ->
		print(C),
		kns_dispConsRest(ms,ConsSet)
	;
		kns_dispConsElems(ms,ConsSet)
	).


kns_dispConsRest(_Mode,[]).
kns_dispConsRest(all,[C|ConsSet]) :-
	print(', '),
	print(C),
	kns_dispConsRest(all,ConsSet).
kns_dispConsRest(ms,[C|ConsSet]) :-
	(kns_manysorted(C) ->
		print(', '),
		print(C)
	;
		true
	),
	kns_dispConsRest(ms,ConsSet).
kns_dispConsRest(os,[C|ConsSet]) :-
	(kns_ordersorted(C) ->
		print(', '),
		print(C)
	;
		true
	),
	kns_dispConsRest(os,ConsSet).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_printGT/2					%
%			kns_writeOpClass/2				%
%			kns_class/2					%
%			kns_dispOps/1					%
%			kns_printClasses/1				%
%			kns_dispStatus/1				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_printGT([],[]) :- !.
kns_printGT([Op1 | C],[]) :-
	(	kns_statusstar(Op1,ST,extToBy([],[],[]))
	;
		ST=nost
	),
	((C = [_Op2|_C2] ; ST \== nost) ->
		kns_dispStatus(ST),
		kns_writeOpClass([Op1 | C]),
		put(9),
		write(' > {}'),
		nl
	;
		true
	),
	!.
kns_printGT([Op1|C],Ops) :-
	(isConstructor(Op1) ->
		write('*C* ')
	;
		true
	),
	(	kns_statusstar(Op1,ST,extToBy([],[],[]))
	;
		ST=nost
	),
	kns_dispStatus(ST),
	kns_writeOpClass([Op1|C]),
	put(9),
	write(' > '),
	kns_printClasses(Ops),
	nl,
	!.


kns_writeOpClass(C) :-
	write('{'),
	kns_dispOps(C),
	write('}'),
	!.


kns_class(Op1,C1) :-
	setof(Op2,kns_eqstar(Op1,Op2,extToBy([],[],[])),C1),
	!.


kns_dispOps([]) :- !.
kns_dispOps([Op|[]]) :-
	write(''''),
	print(Op), 
	write(''''),
	!.
kns_dispOps([Op|Eq]) :-
	write(''''),
	print(Op),
	write(''''),
	write(','),
	kns_dispOps(Eq),
	!.


kns_printClasses([]) :- !.
kns_printClasses([Op|[]]) :-
	kns_class(Op,C),
	kns_writeOpClass(C),
	!.
kns_printClasses([Op|Ops]) :-
	kns_class(Op,C),
	kns_writeOpClass(C),
	write(', '),
	kns_printClasses(Ops).


kns_dispStatus(nost) :- !.
kns_dispStatus(Status) :-
	write('('),
	write(Status),
	write(') '),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			Initialisation					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% kns_opPrecStat(prec(0,[])).
% kns_flexible(on).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			  Utilities					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_member/2					%
%			kns_reverse/2					%
%			kns_path/7					%
%			kns_clear/0					%
%			kns_clearfacts/0				%
%			kns_flexible/1					%
%			kns_flexible_on/0				%
%			kns_flexible_off/0				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_member(X,L) :-
	member(X,L).


kns_reverse([],[]) :- !.
kns_reverse([H|T],R) :-
	kns_reverse(T,Z),
	append(Z,[H],R). 

kns_clear:-
	tps_getCurrentTPS(tps(Current,Pol,kns_state(Flexible,_))),
	tps_makeCurrentTPS(tps(Current,Pol,kns_state(Flexible,prec(0,[])))),
	kns_clearfacts,
	!.


kns_clearfacts:-
	abolish(kns_gt/2),
	abolish(kns_eq/2),
	abolish(kns_status/2).


kns_flexible(Flexible) :-
	tps_getCurrentTPS(tps(_Current,_Pol,kns_state(Flexible,_Prec))),
	!.


kns_flexible_on :-
	assert(kns_flexible(on)),
	!.


kns_flexible_off :-
	abolish(kns_flexible,1),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			   Dummies					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			kns_declConstr/1				%
%			kns_noteOperator/3				%
%			kns_noteOperators/2				%
%			kns_note/3					%
%			kns_getopPrecStat/1				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kns_declConstr(_).
kns_noteOperator(_,_,_).
kns_noteOperators(_,_).
kns_note(_,_,_).
kns_getopPrecStat([]).


:-assert(assertPerhaps(_)).
