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
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

/*----------------------------------------------------------------------*
 *			gen(acMatch(P),Klausel)				*
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

gen(ac_match(P,P),[]) :-
	var(P),
	!.
gen(ac_match(T,P),[T =.. [Op,T1,T2],P1 = T1, ac_matchvf(T1,T2)]) :-
	P =.. [Op,P1,P2],
	ac_ist_C(Op),
	var(P1),
	P1 == P2,
	!.
%gen(ac_match(T,P),Body3) :-
gen(ac_match(T,P),Body) :-
	ac_linear(P,LinP,VarSubst),
%	ac_removeAC(LinP,OhneACP,ACTs,ACVs),
	ac_removeAC(LinP,T1,ACTs,ACVs),
	(	ACVs = [],
	 	!,
% old version - changed 01.02.90 uh
%			 T = P,
%			 Body = [])
         	T = LinP,
	 	gen(ac_listACGleich(VarSubst),Body)
	; 
%		(var(OhneACP),!,Body3 = Body,T = OhneACP;
%		 Body3 = [T = OhneACP|Body]),
		T = T1,
		gen(ac_map(ACTs,ac_matchLinearHeadAC,ACVs),Body1),
		gen(ac_listACGleich(VarSubst),Body2),
		append(Body1,Body2,Body)
	),
	!.


/*----------------------------------------------------------------------*
 *    	        gen(ac_matchOhnePartAuswertung(T,P),Body)		*
 *----------------------------------------------------------------------*/

gen(ac_matchOhnePartAuswertung(P1,P2),[P1=P2]) :-	% HG
	var(P1),					% HG
	var(P2),
	!.
gen(ac_matchOhnePartAuswertung(T,P),
		[T =.. [Op,T1,T2],P1 = T1, ac_matchvf(T1,T2)]) :-
	ac_ist_C(Op),
	P =.. [Op,P1,P2],
	var(P1),
	P1 == P2,
	!.
gen(ac_matchOhnePartAuswertung(T,P),Body3) :-
	ac_linear(P,LinP,VarSubst),
	ac_removeAC(LinP,OhneACP,ACTs,ACVs),
	(	var(OhneACP),
		!,
		Body3 = Body,
		T = OhneACP
	;
		Body3 = [T = OhneACP|Body]
	),
	gen(ac_map(ACTs,ac_matchLinearHeadAC,ACVs),Body1),
	gen(ac_listACGleich(VarSubst),Body2),
	append(Body1,Body2,Body),
	!.


/*----------------------------------------------------------------------*
 *    	            gen(ac_map(Args,P,Args1),Body)			*
 *    	          gen(ac_map(Args,F,Args2,Args1),Body)			*
 *----------------------------------------------------------------------*/

gen(ac_map(Args,P,Args1),Body) :-
	P =.. [F|Args2],
	gen(ac_map(Args,F,Args2,Args1),Body),
	!.
gen(ac_map(Arg,_,_2,Arg1),[]) :-
	(	not(var(Arg))
	;
		not(var(Arg1))
	),
	Arg = [],
	Arg1 = [],
	!.
gen(ac_map(Args,F,Args2,Args1),Body) :-
	(	not(var(Args))
	;
		not(var(Args1))
	),
	Args = [Arg3|Args3],
	Args1 = [Arg4|Args4],
	Goal =.. [F,Arg3,Arg4|Args2],
	gen(Goal,Body1),
	gen(ac_map(Args3,F,Args2,Args4),Body2),
	append(Body1,Body2,Body),
	!.


/*----------------------------------------------------------------------*
 *    	     gen(ac_listACGleich([[T1,T2]]),[ac_matchvf(T1,T2)])	*
 *----------------------------------------------------------------------*/

gen(ac_listACGleich([]),[]) :-
	!.
gen(ac_listACGleich([[T1,T2]]),[ac_matchvf(T1,T2)]) :-
	!.


/*----------------------------------------------------------------------*
 *    	     gen(ac_matchLinearHeadAC(ACP,T),Body)			*
 *----------------------------------------------------------------------*/
		
gen(ac_matchLinearHeadAC(ACP,T),Body) :-
	ACP =.. [AC|PArgs],
	ac_ist_AC(AC),
	!,
	ac_del_assoc(AC,PArgs,PArgs1),
	Body1 = [T =.. [AC|TArgs],
		ac_del_assoc(AC,TArgs,TArgs1)],
	ac_split(PArgs1,Vs,Ts,GTs),
	gen(ac_removeACGleich(GTs,TArgs1,[],Erg),Body2),
	gen(ac_removeACMatchLinear(Ts,Erg,[],Erg1),Body3),
	gen(ac_mehrElemente(AC,Erg1,Vs),Body4),
	append(Body1,Body2,Body5),
	append(Body3,Body4,Body6),
	append(Body5,Body6,Body),
	!.
gen(ac_matchLinearHeadAC(CP,T),[T =..  [COp,T1,T2]|Body]) :-
	CP =.. [COp,P1,P2],
	ac_ist_C(COp),
	!,
	gen(ac_matchLinearC(P1,P2,T1,T2),Body),
	!.


/*----------------------------------------------------------------------*
 *    	     gen(ac_matchLinearC(P1,P2,T1,T2),Body)			*
 *----------------------------------------------------------------------*/

gen(ac_matchLinearC(P1,P2,T1,T2),Body) :-
	gen(ac_matchOhnePartAuswertung(T1,P1),Body1),
	gen(ac_matchOhnePartAuswertung(T2,P2),Body2),
	gen(ac_matchOhnePartAuswertung(T1,P2),Body3),
	gen(ac_matchOhnePartAuswertung(T2,P1),Body4),
	append(Body1,Body2,Body5),
	append(Body3,Body4,Body6),
	paarListeToOder(Body5,Body6,Body),
	!.

/*----------------------------------------------------------------------*
 *    	          gen(ac_removeACGleich([],Ts,_,Ts),[])			*
 *----------------------------------------------------------------------*/

gen(ac_removeACGleich([],Ts,_,Ts),[]) :-
	!.

/*----------------------------------------------------------------------*
 *    	        gen(ac_removeACMatchLinear([],Ts,_,Ts),[])		*
 *----------------------------------------------------------------------*/

gen(ac_removeACMatchLinear([],Ts,_,Ts),[]) :-
	!.

/*----------------------------------------------------------------------*
 *    	            gen(ac_mehrElemente(_,[],[]),[])			*
 *----------------------------------------------------------------------*/

gen(ac_mehrElemente(_,[],[]),[]) :-
	!.  


gen(X,[X]) :-
	!.


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

