/**********************************************************************

       name: mainPronounsDRT.pl (Chapter 9)
    version: Feb 3, 1999
description: Pronoun Resolution
    authors: Patrick Blackburn & Johan Bos

**********************************************************************/

:- module(mainPronounsDRT,[parse/0,resolveDrs/1]).

:- use_module(readLine,[readLine/1]),
   use_module(comsemOperators),
   use_module(comsemPredicates,[printRepresentations/1,simpleTerms/1,
                                compose/3, appendLists/3, memberList/2] ) ,
   use_module(betaConversion,[betaConvert/2]),
   use_module(bindingDRT,[potentialAntecedent/3,properBinding/3]) .

:- [englishGrammar, lambdaDRT].  %, [semMacrosLambdaDRT].

/*========================================================================
Driver Predicate
========================================================================*/

parse:-
	readLine(Discourse),
	d(MergeDrs,Discourse,[]),
	betaConvert(MergeDrs,Drs) ,
	resolveDrs([Drs]-[ResolvedDrs]),
	printRepresentations(ResolvedDrs) .

/*=====================================================================
Pronoun Resolution
=====================================================================*/

%resolveDrs(Drs):- numbervars(Drs,0,_), write(d:Drs), nl, fail.

resolveDrs([merge(B1,B2)|A1]-[drs(D,C)|A3]):-
	resolveDrs([B1|A1]-A2),
	resolveDrs([B2|A2]-[drs(D1,C1),drs(D2,C2)|A3]),
	appendLists(D1,D2,D),
	appendLists(C1,C2,C).

resolveDrs([alfa(Referent,Type,Gender,B1)|A1]-A2):-
	potentialAntecedent(A1,Referent,Gender) ,
	properBinding(Type,Referent,B1),
	resolveDrs([B1|A1]-A2).

resolveDrs([drs(D1,C1)|A1]-A2):-
	resolveConds(C1,[drs(D1,[])|A1]-A2).


%resolveConds(I,O):- numbervars((I,O),0,_), write(c:(I==O)), nl, fail.

resolveConds([~B1|Conds],A1-A3):-
	resolveDrs([B1|A1]-[B2,drs(D,C)|A2]) ,
	resolveConds(Conds,[drs(D,[~B2|C])|A2]-A3).

resolveConds([B1 > B2|Conds],A1-A4):-
	resolveDrs([B1|A1]-A2),
	resolveDrs([B2|A2]-[B4,B3,drs(D,C)|A3]),
	resolveConds(Conds,[drs(D,[B3 > B4|C])|A3]-A4).

resolveConds([B1 v B2|Conds],A1-A4):-
	resolveDrs([B1|A1]-[B3|A2]),
	resolveDrs([B2|A2]-[B4,drs(D,C)|A3]) ,
	resolveConds(Conds,[drs(D,[B3 v B4|C])|A3]-A4).

resolveConds([Basic|Conds],[drs(D,C)|A1]-A2):-
	compose(Basic,_Symbol,Arguments),
	simpleTerms(Arguments),
	resolveConds(Conds, [drs(D,[Basic|C])|A1]-A2).

resolveConds([],A-A).
