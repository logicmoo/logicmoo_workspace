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
 *  900411 huba   Changed definition of ac_removeAC/4 to deal with
 *                     nonlinear patterns.
 *  900411 huba   Moved definition of ac_del_assoc/3 from
 *                     uniac.pl into this file.
 *  900412 huba   Added ac_removeACMatchNonLinear/4.
 *  900418 huba   Changed Implementation of ac_del_assoc to use 
 *                     difference lists.
 *  900419 huba   Moved definition of ac_occurs/2 from uniac.pl
 *                     to this file.
 *                     Added ac_splitOccurs/4, ac_vars/2, ac_all_disjunct/1
 *                     ac_identical_member/2 and ac_makeDifferenceList/2
 *  900424 huba   Removed ac_matchList/1, ac_mapACmatchvf/2
 *                     and ac_removeACMatchLinear/4
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

/*------------------------------------------------------------------*
 *								    *
 *	ac_removeAC(ACTerm,Skelet,Assoc,Assoc1)			    *
 *								    *
 *------------------------------------------------------------------*/

ac_removeAC(ACTerm,Skelet,Ts,Ps) :-
	ac_removeAC_dl(ACTerm,Skelet,Ts/[],Ps/[]).


ac_removeAC_dl(ACTerm,SkeletVar,[ACTerm|Ts]/Ts,[SkeletVar|Ps]/Ps) :-
	var(ACTerm),
	!.
ac_removeAC_dl(ACTerm,SkeletVar,[ACTerm|Ts]/Ts,[SkeletVar|Vs]/Vs) :-
	functor(ACTerm,F,2),
	ac_ist_C(F),
	!.
ac_removeAC_dl(ACTerm,SkeletTerm,T/Ts,P/Ps) :-
	ACTerm =.. [F|Args],
	ac_listRemoveAC_dl(Args,Args1,T/Ts,P/Ps),
	SkeletTerm =.. [F|Args1].


ac_listRemoveAC_dl([],[],Ts/Ts,Ps/Ps).
ac_listRemoveAC_dl([Arg|Args],[Arg1|Arg1s],T/Ts,P/Ps) :-
	ac_removeAC_dl(Arg,Arg1,T/Ts1,P/Ps1),
	ac_listRemoveAC_dl(Args,Arg1s,Ts1/Ts,Ps1/Ps).


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
 *        ac_removeACMatch(List,List1,Erg1,Erg)               *
 *                                                                  *
 *------------------------------------------------------------------*/

ac_removeACMatch([],List1,[],List1).
ac_removeACMatch([T|R],L,Erg2,Erg) :-
        ac_removeACMatch1(T,L,Erg1),
        ac_removeACMatch(R,Erg1,Erg2,Erg).
ac_removeACMatch([T|R],L,[T|Erg2],Erg) :-
        ac_removeACMatch(R,L,Erg2,Erg).

ac_removeACMatch1(Term,[Term1|R],R) :-
        ac_match(Term1,Term).
ac_removeACMatch1(Term,[Term1|R],[Term1|R1]) :-
        ac_removeACMatch1(Term,R,R1).

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

ac_listACGleich(Term,[H|R]) :-
        ac_matchvf(Term,H),
        ac_listACGleich(Term,R).
ac_listACGleich(_,[]).

/*------------------------------------------------------------------*
 *	ac_matchForOp is called from reduce clauses for rules	    *
 *------------------------------------------------------------------*/

ac_matchForOp(_,[],[]).
ac_matchForOp(F,[T|Ts],Ps) :-
	member(P,Ps),
	nonvar(P),
	!,
	(P=..[F|PArgs] ->
		ac_del_assoc(F,PArgs,PArgs1),
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

/*------------------------------------------------------------------*
 *       ac_del_assoc(Op,Args,FlattenArgs)                                           *
 *------------------------------------------------------------------*/
ac_del_assoc(Op,Args,FlattenArgs) :-
	ac_del_assoc_dl(Op,Args,FlattenArgs/[]).

ac_del_assoc_dl(Op,[E|Zl],Fl/Fl2):-
	\+atom(E),
	nonvar(E),
	E =.. [Op|Arg],
	!,
	ac_del_assoc_dl(Op,Arg,Fl/Fl1),
	ac_del_assoc_dl(Op,Zl,Fl1/Fl2),
	!.
ac_del_assoc_dl(Op,[E|Zl],[E|Fl]/Fl1):-
	ac_del_assoc_dl(Op,Zl,Fl/Fl1),
	!.
ac_del_assoc_dl(Op,[],FlattenArgs/FlattenArgs).

/*------------------------------------------------------------------*
 *       ac_occurs(Term,Var)                                           *
 *------------------------------------------------------------------*/

ac_occurs(T,V):-
	var(T),
	!,
	T==V.
ac_occurs(T,V):-
	functor(T,_,N),
	subterm(_,N,T,TJ),
	ac_occurs(TJ,V),
	!.

/*------------------------------------------------------------------*
 *       ac_splitOccurs(VarList,TermList,VarListRest1,VarListRest2)    
 *
 *			VarListRest1 contains all Variables of VarList which do not 
 *				occur in one of the Terms in TermList.
 *			VarListRest2 = VarList - VarListRest1
 * 
 *------------------------------------------------------------------*/
ac_splitOccurs([],_,[],[]).
ac_splitOccurs([Var|VarList],TermList, [Var|VarList1],VarList2):-
	ac_occurs(TermList,Var),
	!,
	ac_splitOccurs(VarList,TermList,VarList1,VarList2).
ac_splitOccurs([Var|VarList],TermList,VarList1,[Var|VarList2]):-
	ac_splitOccurs(VarList,TermList,VarList1,VarList2).

/*------------------------------------------------------------------*
 *       ac_vars(Term,VarSet)                                           *
 *------------------------------------------------------------------*/
ac_vars(Term,VarSet) :-
	ac_vars(Term,[],VarSet).

ac_vars(Term,In,In) :-
	var(Term),
	ac_identical_member(Term,In),
	!.
ac_vars(Term,In,[Term|In]) :-
	var(Term),
	!.
ac_vars(Term,In,Out):-
	Term =.. [Op|Args],
	ac_map_vars(Args, In,Out),
	!.

ac_map_vars([],In,In).
ac_map_vars([Term|TermSet],In,Out):-
	ac_vars(Term,In,Out1),
	ac_map_vars(TermSet,Out1,Out).

/*------------------------------------------------------------------*
 *       ac_all_disjunct(SetOfSets)                                           *
 *------------------------------------------------------------------*/
ac_all_disjunct(SetOfSets):-
	ac_appendAll(SetOfSets,List),
	ac_no_dublicates(List).

ac_appendAll([Set|SetOfSets],List):-
	ac_appendAll(SetOfSets,List1),
	append(Set,List1,List).
ac_appendAll([],[]).

ac_no_dublicates([]).
ac_no_dublicates([H|T]):-
	ac_identical_member(H,T),
	!,
	fail.
ac_no_dublicates([H|T]):-
	ac_no_dublicates(T).

/*------------------------------------------------------------------*
 *       ac_identical_member(Elem,List)                                           *
 *------------------------------------------------------------------*/
ac_identical_member(Elem,[H|T]):-
	Elem == H.
ac_identical_member(Elem,[H|T]):-
	\+ Elem == H,
	ac_identical_member(Elem,T).

/*------------------------------------------------------------------*
 *       ac_makeDifferenceList(List,List1)                                           *
 *------------------------------------------------------------------*/
ac_makeDifferenceList([],List/List).
ac_makeDifferenceList([H|T],[H|List]/List1):-
	ac_makeDifferenceList(T,List/List1).