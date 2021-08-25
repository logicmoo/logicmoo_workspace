/*
 *	file:		gen.pl
 *	version:	1.5
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains ..
 *
 *	history:
 *	891010	js	Added this comment
 *	900201  uh	Changed definition of 
 *			gen(ac_match(T,P),Body):
 *			Non-linear left-hand sides of a rule are now
 *			linearized, and the test wether two variables are 
 *			instantiated with the same term	is done using 
 *			ac_matchvf (not with Prolog-unification)
 *  900418  huba  Rewritten gen/2 to create more efficient code for
 *                      nonlinear Patterns.
 *  900601  huba bug gen(ac_match(X,k),Var) and Var not instanziated gefixed.
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

/*----------------------------------------------------------------------*
 *			gen(acMatch(P),Klausel)				*
 *                  This predicate is for testing purposes only.
 *----------------------------------------------------------------------*/

gen(acMatch(P),Klausel) :-
	Head = acMatch(P,T),
	gen(ac_match(T,P),Body),
	(	ac_listToTupel(Body,Tupel),
		!,
		Klausel = (Head :- Tupel)
	;
	 	Klausel = Head
	),
	asserta(Klausel).

/*----------------------------------------------------------------------*
 *    			  gen(ac_match(T,P),Body)			*
 *----------------------------------------------------------------------*/

gen(Pred,Body) :-
	gen_dl(Pred,Body/[]).

gen_dl(ac_match(GTerm,Pattern),Body/Body):-
	\+ ac_ist_C(_),
	!,
	GTerm=Pattern.
gen_dl(ac_match(GTerm,Pattern),Body/Body) :- 
	var(Pattern),
	!,
	GTerm=Pattern.
gen_dl(ac_match(GTerm,Pattern),Body/Body4) :-
	Pattern =.. [Op|PatternArgs],
	ac_ist_AC(Op),
	!,
	gen_dl(GTerm =.. [Op|TermArgs],Body/Body1),
	ac_del_assoc(Op,PatternArgs,PArgs),
	gen_dl(ac_del_assoc(Op,TermArgs,TArgs),Body1/Body2),
	ac_split(PArgs,VarSet,TermSet,GTermSet),
	ac_splitOccurs(VarSet,TermSet,GTermSetSplit,VarSetSplit),
	gen_dl((ac_removeACGleich(GTermSet,TArgs,[],Erg),
	         ac_removeACMatch(TermSet,Erg,[],Erg1),
		    ac_del_assoc(Op,GTermSetSplit,GTermSetSplit1),
	         ac_removeACGleich(GTermSetSplit1,Erg1,[],Erg2)),Body2/Body3),
	ac_linear(VarSetSplit,VarLinearSet,VarSubst),
	gen_dl((ac_mehrElemente(Op,Erg2,VarLinearSet),
	         ac_listACGleich(VarSubst)),Body3/Body4).
gen_dl(ac_match(GTerm,Pattern),Body1/Body2) :-
	Pattern =.. [Op,P1,P2],
	ac_ist_C(Op),
	!,
	GTerm =.. [Op,T1,T2],
	(	var(P1),
		P1 == P2,
		!,
		Body1/Body2 = [ac_matchvf(T1,T2)|List]/List
	;
		(	map(ac_vars,[P1,P2],SetOfSets),
			ac_all_disjunct(SetOfSets),	
			gen_dl((	T1 = T11, T2 = T21,
						ac_match(T11,P1),
						ac_match(T21,P2)
					;
						T1 = T12, T2 = T22,
						ac_match(T12,P2),
						ac_match(T22,P1)
					),Body1/Body2)
		;
			gen_dl((	ac_match_noPartEval(T1,P1),
						ac_match_noPartEval(T2,P2)
					;
						ac_match_noPartEval(T1,P2),
						ac_match_noPartEval(T2,P1)
					),Body1/Body2)
		)
	).
gen_dl(ac_match(GTerm,Pattern),Body/Body1) :-
	ac_removeAC(Pattern,GTerm,ACPatterns,ACGTs),
	map(ac_vars,ACPatterns,Vars),
	(	ac_all_disjunct(Vars),
		gen_dl(map(ac_match,ACGTs,ACPatterns),Body/Body1)
	;
		ACPatterns = [FirstPattern|RestPatterns],
		ACGTs = [FirstGroundTerm|RestGTs],
		gen_dl(ac_match(FirstGroundTerm,FirstPattern),Body/Body1),
		Body1 = [map(ac_match,RestGTs,RestPatterns)|Body2]
	).

/*----------------------------------------------------------------------*
 *    			  gen_dl(ac_match_noPartEval(T,P),Body)			*
 *----------------------------------------------------------------------*/

gen_dl(ac_match_noPartEval(T,P),[ac_match(T,P)|Body]/Body).

/*----------------------------------------------------------------------*
 *    			  gen_dl(ac_removeACGleich(List,List1,RestList,RestList1),Body)			*
 *----------------------------------------------------------------------*/

gen_dl(ac_removeACGleich(List,RestList,[],RestList),Body/Body):-
	\+ var(List),
	List = [],
	!.

/*----------------------------------------------------------------------*
 *    			  gen_dl(ac_removeACMatch(List,List1,RestList,RestList1),Body)			*
 *----------------------------------------------------------------------*/

gen_dl(ac_removeACMatch(List,RestList,[],RestList),Body/Body):-
	\+ var(List),
	List = [].

/*----------------------------------------------------------------------*
 *    			  gen_dl(ac_mehrElemente(Op,TermList,LinearVarList),Body)			*
 *----------------------------------------------------------------------*/

gen_dl(ac_mehrElemente(_,[],LinearVarList),Body/Body):-
	\+ var(LinearVarList),
	LinearVarList = [].
gen_dl(ac_mehrElemente(Op,TermList,LinearVarList),Body/Body1):-
	\+ var(LinearVarList),
	LinearVarList = [X],
	!,
	gen_dl(ac_makeAssoc(Op,[(X,TermList)]),Body/Body1).
	

/*----------------------------------------------------------------------*
 *    			  gen_dl(ac_linear(Term,LinearTerm,VarSubst),Body)			*
 *----------------------------------------------------------------------*/

/* gen_dl(ac_linear(Term,[],[]),Body/Body):-
	\+ var(Term),
	Term = [].
gen_dl(ac_linear(Term,Term,[]),Body/Body):-
	\+ var(Term),
	Term = [X],
	var(X).
*/

/*----------------------------------------------------------------------*
 *    			  gen_dl(ac_listACGleich(VarSubst),Body)			*
 *----------------------------------------------------------------------*/

gen_dl(ac_listACGleich(VarSubst),Body/Body):-
	\+ var(VarSubst),
	VarSubst = [].
gen_dl(ac_listACGleich(VarSubst),Body/Body1):-
	\+ var(VarSubst),
	VarSubst = [[GT,GT1]],
	gen_dl(ac_matchvf(GT,GT1),Body/Body1).

/*----------------------------------------------------------------------*
 *    			  gen_dl(ac_del_assoc(Op,Args,FlattenedArgs),Body)			*
 *----------------------------------------------------------------------*/

gen_dl(ac_del_assoc(Op,Args,[]),Body/Body):-
	\+ var(Args),
	Args = [].

/*----------------------------------------------------------------------*
 *    			  gen_dl(map(ac_match, TermList, PatternList,Body)			*
 *----------------------------------------------------------------------*/

gen_dl(map(ac_match,[],PatternList),Body/Body):-
	\+ var(PatternList),
	PatternList = [].
gen_dl(map(ac_match,TermList,PatternList),Body/Body1):-
	\+ var(PatternList),
	PatternList = [Pattern|PatternList1],
	!,
	TermList = [Term|TermList1],
	gen_dl((ac_match(Term,Pattern),
	          map(ac_match,TermList1,PatternList1)), Body/Body1).

/*----------------------------------------------------------------------*
 *    			  gen_dl((Predicate1,Predicate2),Body)			*
 *----------------------------------------------------------------------*/
gen_dl((P1,P2),Body/Body2):-
	gen_dl(P1,Body/Body1),
	gen_dl(P2,Body1/Body2).

/*----------------------------------------------------------------------*
 *    			  gen_dl((Predicate1;Predicate2),Body)			*
 *----------------------------------------------------------------------*/
gen_dl((P1;P2),Body) :-
	gen_dl(P1,Body1/[]),
	gen_dl(P2,Body2/[]),
	paarListeToOder(Body1,Body2,Body3),
	ac_makeDifferenceList(Body3,Body).

/*----------------------------------------------------------------------*
 *    			  gen_dl(Predicate,Body)			*
 *----------------------------------------------------------------------*/
gen_dl(Pred,[Pred|Body]/Body).

/*----------------------------------------------------------------------*
 *    	         paarListeToOder(BodyL1,BodyL2,[(Body1;Body2)])		*
 *----------------------------------------------------------------------*/

paarListeToOder(BodyL1,[],BodyL1).
paarListeToOder([],BodyL2,BodyL2).
paarListeToOder(BodyL1,BodyL2,[(Body1;Body2)]) :-
	ac_listToTupel(BodyL1,Body1),
	ac_listToTupel(BodyL2,Body2).


/*----------------------------------------------------------------------*
 *    	         matchCodeForTerm((L,TArgs),Codes)			*
 *----------------------------------------------------------------------*/

/* matchCodeForTerm((L,Patterns),[]) :-
 *	L=..[_|Patterns],
 *	not(containsCOp(L)),
 *	% Diese Sonderbehandlung kann mehr als einen Faktor 10 an
 *	% Geschwindigkeit liefern
 *	(ac_ist_AC(_)->
 *		linear(L)
 *	;
 *		true
 *	).
*/

matchCodeForTerm((L,TArgs),Codes) :-
	L =.. [O|LArgs],
	length(LArgs,N),
	genVarList(N,TArgs),
	T =.. [O|TArgs],
	gen(ac_match(T,L),Codes),
	!.

