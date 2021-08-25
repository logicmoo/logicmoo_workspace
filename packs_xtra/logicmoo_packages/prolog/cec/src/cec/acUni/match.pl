/*
 *	file:		match.pl
 *	version:	1.5
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains the function to do matching (AC-matching
 *	if necessary).
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

/*------------------------------------------------------------------*
 *								    *
 *	ac_match(T,P)						    *
 *								    *
 *------------------------------------------------------------------*/

ac_match(Term,Term1):-
	\+ ac_ist_C(_),
	!,
	Term=Term1.
ac_match(T,V) :- 
	var(V),
	!,
	T = V.
ac_match(OhneACP,P) :-
	ac_linear(P,LinP,VarSubst),
	ac_removeAC(LinP,OhneACP,ACTs,ACVs),
	map(ac_matchLinear,ACTs,ACVs),
	ac_listACGleich(VarSubst).

/*------------------------------------------------------------------*
 *								    *
 *	ac_matchLinear(ACP,T)				            *
 *								    *
 *------------------------------------------------------------------*/

ac_matchLinear(ACP,T) :-
	ACP =.. [AC|PArgs],
	ac_ist_AC(AC),
	!,
	T =.. [AC|TArgs],
	ac_del_assoc(AC,PArgs,PArgs1),
	ac_del_assoc(AC,TArgs,TArgs1),
	ac_split(PArgs1,VL,TL,GTL),
	ac_removeACGleich(GTL,TArgs1,[],Erg),
	ac_removeACMatchLinear(TL,Erg,[],Erg1),
	ac_mehrElemente(AC,Erg1,VL).

ac_matchLinear(CP,T) :-
	CP =.. [COp,P1,P2],
	ac_ist_C(COp),
	!,
	T =.. [COp,T1,T2],
	ac_matchLinearC(P1,P2,T1,T2).

ac_matchLinear(Var,Var) :-
	var(Var),
	!.

ac_matchLinear(P,OhneACP) :-
	ac_removeAC(P,OhneACP,ACTs,ACVs),
	map(ac_matchLinear,ACTs,ACVs).

/*------------------------------------------------------------------*
 *                                                                  *
 *        ac_matchLinearC(Pattern1,Pattern2,Term1,Term2)	    *
 *                                                                  *
 *------------------------------------------------------------------*/
	
ac_matchLinearC(P1,P2,T1,T2) :-
	var(P1),
	P1 == P2,
	ac_match(T1,T2).

ac_matchLinearC(P1,P2,T1,T2) :-
	(	ac_match(T1,P1),
		ac_match(T2,P2)
	;
		ac_match(T1,P2),
		ac_match(T2,P1)
	).

/*------------------------------------------------------------------*
 *                                                                  *
 *        ac_matchvf(Term,Term1)				    *
 *                                                                  *
 *------------------------------------------------------------------*/

ac_matchvf(Term,Term1):-
	not ac_ist_C(_),
	!,
	Term=Term1.
ac_matchvf(Term,Term1) :-
        Term =.. [F|Args],
        Term1 =.. [F|Args1],
        (	ac_ist_AC(F),
		!,
		ac_del_assoc(F,Args,Args10),
                ac_del_assoc(F,Args1,Args11),
                ac_removeACGleich(Args10,Args11,[],[])
	;
		ac_ist_C(F),
		!,
		ac_removeACGleich(Args,Args1,[],[])
	;
		map(ac_matchvf,Args,Args1)
	).

