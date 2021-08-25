/*
 *	file:		combine.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *      "kns_combine" combines two precedences if combineable without
 *      a contradiction, otherwise it generates an error message.
 *
 *      P1 is combineable with P2 iff P1 can be consistently be extended
 *      by the definitions of P2.
 *
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






/*
kns_combine(P1,P2,Prec3):-
	kns_saveCurrentPrec(CurrP),
	assert(kns_sp(CurrP)),
	kns_makeCurrentPrec(P1),
	kns_combP(P1,P2),
	kns_saveCurrentPrec(Prec3),
	kns_makeCurrentPrec(CurrP),
	abolish(kns_sp,1), !.

kns_combine(P1,P2,_):-
%       not combineable
	kns_sp(P),
	kns_makeCurrentPrec(P),
	abolish(kns_sp,1),
	!,
	fail.
*/

kns_combine(P1,_P2,P1).

kns_saveCurrentPrec(P):-
	kns_getopPrecStat(P), !.

kns_makeCurrentPrec(P):-
	kns_makeopPrecStat(P), !.

kns_combP(P1,P2):-
	kns_makeCurrentPrec(P1),
	(setof(class(Nr,CL,ST,CS),kns_member(class(Nr,CL,ST,CS),P2),Classes) ; Classes= []),
	(setof(gt(A,B),kns_member(gt(A,B),P2),Gts) ; Gts = []),
	kns_combClasses(Classes),
	kns_combGts(P2,Gts), !.

kns_combClasses([]):- !.
kns_combClasses([Class|Classes]):-
	kns_combClass(Class),
	kns_combClasses(Classes).

kns_combClass(class(_Nr,CL,ST,CR)):-
	(CR = yes -> kns_declConstr(CL); true),
	kns_declEqL(CL),
	CL = [Op|_L],
	(ST == ms -> kns_declStatus_ms(Op);true),
	(ST == lr -> kns_declStatus_lr(Op);true),
	(ST == rl -> kns_declStatus_rl(Op);true),
	!.

kns_declEqL([Op]):-
	kns_opPrecStat(X),
	kns_noteOperator(Op,X,NewStat_Prec),
	assert(kns_opPrecStat(NewStat_Prec)),!.

kns_declEqL([Op|OpL]):-
	kns_decleqWith(Op,OpL), !.
kns_decleqWith(_Op,[]):- !.
kns_decleqWith(Op,[F|OpL]):-
	kns_declPrecEQ(Op,F),
	kns_decleqWith(Op,OpL).



kns_combGts(_P2,[]):- !.
kns_combGts(P2,[gt(ANr,BNr)|Gts]):-
	kns_isOfClassInPrec(_A,class(ANr,AC,_ASt,_AIsC),P2),
	kns_isOfClassInPrec(_B,class(BNr,BC,_BSt,_BIsC),P2),
	AC = [F|_],
	BC = [G|_],
	kns_declPrecGT(F,G),
	kns_combGts(P2,Gts).
