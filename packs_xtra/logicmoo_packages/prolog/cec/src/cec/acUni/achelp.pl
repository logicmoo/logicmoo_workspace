/*
 *	file:		achelp.pl
 *	version:	1.5
 *	date:		November 6, 1989
 *	creation:	October 10, 1989
 *	author:		-
 *
 *	description:
 *	Contains predicates related to AC-matching and AC-unification.
 *
 *	history:
 *	891010	js	Added this comment
 *	891030  uh	Moved definitions of
 *			ac_matchForOp/3		ac_matchAnyOf/3
 *			ac_matchForOpvf/2	ac_matchAnyOfvf/3
 *			from completion/terms.pl into this file
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

/*------------------------------------------------------------------*
 *								    *
 *	ac_removeAC(Term,Term1,Assoc,Assoc1)			    *
 *								    *
 *------------------------------------------------------------------*/

ac_removeAC(Term,Term1,Ts,Ps) :-
	ac_removeAC_dl(Term,Term1,Ts/[],Ps/[]).


ac_removeAC_dl(Var,Var,Ts/Ts,Ps/Ps) :-
	var(Var),
	!.
ac_removeAC_dl(Term,Var,[Term|Ts]/Ts,[Var|Vs]/Vs) :-
	functor(Term,F,2),
	ac_ist_C(F),
	!.
ac_removeAC_dl(Term,Term1,T/Ts,P/Ps) :-
	Term =.. [F|Args],
	ac_listRemoveAC_dl(Args,Args1,T/Ts,P/Ps),
	Term1 =.. [F|Args1].


ac_listRemoveAC_dl([],[],Ts/Ts,Ps/Ps).
ac_listRemoveAC_dl([Arg|Args],[Arg1|Arg1s],T/Ts,P/Ps) :-
	ac_removeAC_dl(Arg,Arg1,T/Ts1,P/Ps1),
	ac_listRemoveAC_dl(Args,Arg1s,Ts1/Ts,Ps1/Ps).


/*------------------------------------------------------------------*
 *								    *
 *	ac_matchList([(ACTerm,ACPattern1)|R])			    *
 *								    *
 *------------------------------------------------------------------*/

ac_matchList([(ACT,ACP)|R]) :-
	ac_matchLinear(ACP,ACT),
	ac_matchList(R).
ac_matchList([]).


/*------------------------------------------------------------------*
 *								    *
 *	ac_linear(Term,Term1,Varlist)				    *
 *								    *
 *------------------------------------------------------------------*/

ac_linear(Term,Term1,VarList) :-
	ac_linear1(Term,Term1,[],VarList1),
	ac_removeRedundant(VarList1,VarList).
/* ac_linear(Atom,Atom,Varlist,Varlist). */


ac_linear1(Var,Var1,Varliste,Varlist1) :-
	var(Var),
	!,
	ac_appendVar(Var,Var1,Varliste,Varlist1).
ac_linear1(Term,Term1,Varlist,Varlist1) :-
	Term =.. [F|Args],
	ac_mapLinear1(Args,Args1,Varlist,Varlist1),
	Term1 =.. [F|Args1].


/*------------------------------------------------------------------*
 *								    *
 *	ac_removeRedundant(VarListe,VarListe1)			    *
 *								    *
 *------------------------------------------------------------------*/

ac_removeRedundant([[V,V]|R],R1) :-
	ac_removeRedundant(R,R1),
	!.
ac_removeRedundant([[V,V|R]|R1],[[V|R]|R2]) :-
	ac_removeRedundant(R1,R2).
ac_removeRedundant([],[]).


/*------------------------------------------------------------------*
 *								    *
 *	ac_mapLinear1(Args,Args1,Varlist,Varlist1)		    *
 *								    *
 *------------------------------------------------------------------*/

ac_mapLinear1([],[],VL,VL).
ac_mapLinear1([T|Args],[T1|Args1],VL,VL1) :-
	ac_linear1(T,T1,VL,VL2),
	ac_mapLinear1(Args,Args1,VL2,VL1).


/*------------------------------------------------------------------*
 *								    *
 *	ac_appendVar(Var,Var1,Varlist,Varlist1)			    *
 *								    *
 *------------------------------------------------------------------*/

ac_appendVar(Var,Var1,[],[[Var,Var1]]).
ac_appendVar(Var,Var1,[[Var2|Vars]|R],[[Var,Var1|Vars]|R]) :-
	Var == Var2,
	!.
ac_appendVar(Var,Var1,[V|R],[V|R1]) :-
	ac_appendVar(Var,Var1,R,R1).


/*------------------------------------------------------------------*
 *								    *
 *      ac_keineDoppel(VarList)					    * 
 *								    *
 *------------------------------------------------------------------*/

ac_keineDoppel([]).
ac_keineDoppel([[_,_]|R]) :-
	ac_keineDoppel(R).


/*------------------------------------------------------------------*
 *								    *
 *	ac_listToTupel(List,Tupel)				    *
 *								    *
 *------------------------------------------------------------------*/

ac_listToTupel([E],E) :- !.
ac_listToTupel([E|R],(E,R1)) :-
	ac_listToTupel(R,R1).


/*------------------------------------------------------------------*
 *								    *
 *	ac_split(Liste,VarListe,TermListe,GTermListe)	 	    *
 *								    *
 *------------------------------------------------------------------*/

ac_split([],[],[],[]).
ac_split([V|R],[V|R1],TL,GTL) :-
	var(V),
	!,
	ac_split(R,R1,TL,GTL).
ac_split([T|R],VL,[T|TL],GTL) :-
	ac_enthaeltVars(T),
	!,
	ac_split(R,VL,TL,GTL).
ac_split([T|R],VL,TL,[T|GTL]) :-
	ac_split(R,VL,TL,GTL).


/*------------------------------------------------------------------*
 *								    *
 *	ac_enthaeltVars(Term)					    *
 *								    *
 *------------------------------------------------------------------*/

ac_enthaeltVars(V) :-
	var(V),
	!.
ac_enthaeltVars(T) :-
	T =.. [_|Args],
	ac_mapEnthaeltVars(Args).


/*------------------------------------------------------------------*
 *								    *
 *	ac_mapEnthaeltVars(Liste)				    *
 *								    *
 *------------------------------------------------------------------*/

ac_mapEnthaeltVars([T|_]) :-
	ac_enthaeltVars(T),
	!.
ac_mapEnthaeltVars([_|L]) :-
	ac_mapEnthaeltVars(L).


/*------------------------------------------------------------------*
 *                                                                  *
 *        ac_removeACGleich(List,List1,Erg1,Erg)                    *
 *                                                                  *
 *------------------------------------------------------------------*/

ac_removeACGleich([],List1,[],List1).
ac_removeACGleich([T|R],L,Erg2,Erg) :-
        ac_removeAC1(T,L,Erg1),
        !,
        ac_removeACGleich(R,Erg1,Erg2,Erg).
ac_removeACGleich([T|R],L,[T|Erg2],Erg) :-
        ac_removeACGleich(R,L,Erg2,Erg).


/*------------------------------------------------------------------*
 *                                                                  *
 *        ac_removeAC1(Term,Liste,Liste1)                           *
 *                                                                  *
 *------------------------------------------------------------------*/

ac_removeAC1(Term,[Term1|R],R) :-
        ac_matchvf(Term,Term1),
        !.
ac_removeAC1(Term,[Term1|R],[Term1|R1]) :-
        ac_removeAC1(Term,R,R1).


/*------------------------------------------------------------------*
 *                                                                  *
 *        ac_mapACmatchvf(List,List1)                               *
 *                                                                  *
 *------------------------------------------------------------------*/

ac_mapACmatchvf([],[]).
ac_mapACmatchvf([T|R],[T1|R1]) :-
        ac_matchvf(T,T1),
        ac_mapACmatchvf(R,R1).


/*------------------------------------------------------------------*
 *                                                                  *
 *        ac_removeACMatchLinear(List,List1,Erg1,Erg)               *
 *                                                                  *
 *------------------------------------------------------------------*/

ac_removeACMatchLinear([],List1,[],List1).
ac_removeACMatchLinear([T|R],L,Erg2,Erg) :-
        ac_removeACM(T,L,Erg1),
        ac_removeACMatchLinear(R,Erg1,Erg2,Erg).
ac_removeACMatchLinear([T|R],L,[T|Erg2],Erg) :-
        ac_removeACMatchLinear(R,L,Erg2,Erg).


/*------------------------------------------------------------------*
 *                                                                  *
 *        ac_removeACM(Term,Liste,Liste1)                           *
 *                                                                  *
 *------------------------------------------------------------------*/

ac_removeACM(Term,[Term1|R],R) :-
        ac_matchLinear(Term,Term1).
ac_removeACM(Term,[Term1|R],[Term1|R1]) :-
        ac_removeACM(Term,R,R1).


/*------------------------------------------------------------------*
 *                                                                  *
 *        ac_mehrElemente(F,Tl,Vl)                                  *
 *                                                                  *
 *------------------------------------------------------------------*/
        
ac_mehrElemente(_,[],[]) :- !.
ac_mehrElemente(F,Tl,Vl) :-
	ac_zu(Vl,Tl,L),
	ac_makeAssoc(F,L).


ac_n([E|L],[E],L).
ac_n([E|L],[E|P],L1) :-
	ac_n(L,P,L1).
ac_n([E|L],P,[E|L1]) :-
	ac_n(L,P,L1).


ac_zu([E|L],L1,[(E,P)|R]) :-
	ac_n(L1,P,L2),
	ac_zu(L,L2,R).
ac_zu([],[],[]).


ac_makeAssoc(F,[(V,P)|R]) :-
	ac_makeTerm(F,P,V),
	ac_makeAssoc(F,R).
ac_makeAssoc(_,[]).


ac_makeTerm(_,[E],E) :- !.
ac_makeTerm(F,[A|R],T) :-
	ac_makeTerm(F,R,B),
	T =.. [F,A,B].


/*------------------------------------------------------------------*
 *                                                                  *
 *        ac_listACGleich(VarSubst)                                 *
 *                                                                  *
 *------------------------------------------------------------------*/

ac_listACGleich([[V,T|R]|R1]) :-
        ac_matchvf(V,T),
        ac_listACGleich(T,R),
        ac_listACGleich(R1).
ac_listACGleich([]).


/*------------------------------------------------------------------*
 *                                                                  *
 *        ac_listACGleich(Term,Liste)                               *
 *                                                                  *
 *------------------------------------------------------------------*/

ac_listACGleich(Term,[H|R]) :-
        ac_matchvf(Term,H),
        ac_listACGleich(Term,R).
ac_listACGleich(_,[]).


/*------------------------------------------------------------------*
 *                                                                  *
 *        ac_map(Xs,P,Ys)		                            *
 *                                                                  *
 *------------------------------------------------------------------*/

ac_map(Xs,P,Ys) :-
	P =.. [F|Args],
	ac_map(Xs,F,Args,Ys).

ac_map([],_,_,[]).
ac_map([X|Xs],F,Args,[Y|Ys]) :-
	Goal =.. [F,X,Y|Args],
	call(Goal),
	ac_map(Xs,F,Args,Ys).

/*------------------------------------------------------------------*
 *	ac_matchForOp is called from reduce clauses for rules	    *
 *------------------------------------------------------------------*/

ac_matchForOp(_,[],[]).
ac_matchForOp(F,[T|Ts],Ps) :-
	member(P,Ps),
	nonvar(P),
	!,
	(P=..[F|PArgs] ->
		ac_del_assoc(P,PArgs,PArgs1),
		append(PArgs1,Ps,Ps2),
                delete1(Ps2,P,Ps1),
		TsRest=[T|Ts]
	;
		ac_matchAnyOf([T|Ts],P,TsRest),
		delete1(Ps,P,Ps1)
	),
	ac_matchForOp(F,TsRest,Ps1).
ac_matchForOp(F,[T|Ts],[P]) :-
	var(P),
	!,
	ac_rreduce(F,[T|Ts],P).
ac_matchForOp(F,[T|Ts],[V|Vs]) :-
	var(V),
	partition([T1|TSome],[T2|TRest],[T|Ts]),
	ac_rreduce(F,[T1|TSome],V),
	ac_matchForOp(F,[T2|TRest],Vs).


ac_matchAnyOf(Ts,P,TsRest) :-
	member(T,Ts),
	ac_match(T,P),
	delete(Ts,T,TsRest).

/*------------------------------------------------------------------*/

ac_matchForOpvf([],[]).
ac_matchForOpvf(Ts,[P|Ps]) :-
	ac_matchAnyOfvf(Ts,P,TsRest),
	!,
	ac_matchForOpvf(TsRest,Ps).


ac_matchAnyOfvf([T|Ts],P,Ts) :-
	ac_matchvf(T,P),
	!.
ac_matchAnyOfvf([T|Ts],P,[T|Ts1]) :-
	ac_matchAnyOfvf(Ts,P,Ts1).

