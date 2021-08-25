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
 *  900412  huba  rewritten ac_match/2 to handle non-linear Patterns more efficently
 *                      removed ac_matchLinear/2 and ac_matchLinearC/4.
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

/*------------------------------------------------------------------*
 *								    *
 *	ac_match(GroundTerm,Pattern)						    *
 *								    *
 *------------------------------------------------------------------*/

ac_match(GTerm,Pattern):-
	\+ ac_ist_C(_),
	!,
	GTerm=Pattern.
ac_match(GTerm,Pattern) :- 
	var(Pattern),
	!,
	GTerm = Pattern.
ac_match(GTerm,Pattern) :-
	Pattern =.. [Op|PatternArgs],
	ac_ist_AC(Op),
	!,
	GTerm =.. [Op|TermArgs],
	ac_del_assoc(Op,PatternArgs,PArgs),
	ac_del_assoc(Op,TermArgs,TArgs),
	ac_split(PArgs,VarSet,TermSet,GTermSet),
	ac_removeACGleich(GTermSet,TArgs,[],Erg),
	ac_removeACMatch(TermSet,Erg,[],Erg1),
	ac_split(VarSet,VarSetSplit,[],GTermSetSplit),
	ac_del_assoc(Op,GTermSetSplit,GTermSetSplit1),
	ac_removeACGleich(GTermSetSplit1,Erg1,[],Erg2),
	ac_linear(VarSetSplit,VarLinearSet,VarSubst),
	ac_mehrElemente(Op,Erg2,VarLinearSet),
	ac_listACGleich(VarSubst).
ac_match(GTerm,Pattern) :-
	Pattern =.. [Op,P1,P2],
	ac_ist_C(Op),
	!,
	GTerm =.. [Op,T1,T2],
	(	var(P1),
		P1 == P2,
		!,
		ac_match(T1,T2)
	;
		(	ac_match(T1,P1),
			ac_match(T2,P2)
		;
			ac_match(T1,P2),
			ac_match(T2,P1)
		)
	).
ac_match(GTerm,Pattern) :-
	ac_removeAC(Pattern,GTerm,ACPatterns,ACGTs),
	map(ac_match,ACGTs,ACPatterns).

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

