/**********************************************************************

         name: resolvePresupScore.pl
      version: Feb 6, 1999
  description: Presupposition Projection with Score Calculation
      authors: Patrick Blackburn & Johan Bos

**********************************************************************/

:- module(resolvePresupScore,[projectDrs/2,accommodate/2]).

:- ensure_loaded(comsemOperators).

:- use_module(comsemPredicates,[simpleTerms/1,
				compose/3,
				appendLists/3,
				selectFromList/3]).

:- use_module(bindingDRT,[properBinding/3]).

:- use_module(matchDRT,[matchDrs/4]).


/*=====================================================================
   Presupposition Projection for DRSs
=====================================================================*/

projectDrs([merge(B1,B2)|A1]-[drs(D3,C3)|A3],S1-S3):-
        projectDrs([B1|A1]-A2,S1-S2),
        projectDrs([B2|A2]-[drs(D1,C1),drs(D2,C2)|A3],S2-S3),
        appendLists(D1,D2,D3),
        appendLists(C1,C2,C3).

projectDrs([alfa(X,Type,B1,B3)|A1]-A4,S1-S4):-
	projectDrs([B1|A1]-[B2|A2],S1-S2), 
        resolveAlfa(X,Type,B2,A2-A3,S2-S3),
	properBinding(Type,X,B3),
	projectDrs([B3|A3]-A4,S3-S4).

projectDrs([drs(D1,C1)|A1]-A2,S1-S2):-
        projectConds(C1,[drs(D1,[])|A1]-A2,S1-S2).


/*=====================================================================
   Presupposition Projection for DRS-Conditions
=====================================================================*/

projectConds([~B1|Conds],A1-A3,S1-S3):-
        projectDrs([B1,pre([])|A1]-[B2,pre(Pres),drs(D,C)|A2],S1-S2),
	accommodate(Pres,B2-B3),
        projectConds(Conds,[drs(D,[~B3|C])|A2]-A3,S2-S3).

projectConds([B1 > B2|Conds],A1-A4,S1-S4):-
        projectDrs([B1,pre([])|A1]-A2,S1-S2),
        projectDrs([B2,pre([])|A2]-[B4,pre(P2),B3,pre(P1),drs(D,C)|A3],S2-S3),
	accommodate(P1,B3-B5),
	accommodate(P2,B4-B6),
        projectConds(Conds,[drs(D,[B5 > B6|C])|A3]-A4,S3-S4).

projectConds([B1 v B2|Conds],A1-A4,S1-S4):-
        projectDrs([B1,pre([])|A1]-[B3,pre(P1)|A2],S1-S2),
        projectDrs([B2,pre([])|A2]-[B4,pre(P2),drs(D,C)|A3],S2-S3),
	accommodate(P1,B3-B5),
	accommodate(P2,B4-B6),
        projectConds(Conds,[drs(D,[B5 v B6|C])|A3]-A4,S3-S4).

projectConds([question(X,B1,B2)|Conds],A1-A4,S1-S4):-
        projectDrs([B1,pre([])|A1]-A2,S1-S2),
        projectDrs([B2,pre([])|A2]-[B4,pre(P2),B3,pre(P1),drs(D,C)|A3],S2-S3),
	accommodate(P1,B3-B5),
	accommodate(P2,B4-B6),
        projectConds(Conds,[drs(D,[question(X,B5,B6)|C])|A3]-A4,S3-S4).

projectConds([Basic|Conds],[drs(D,C)|A1]-A2,S1-S2):-
        compose(Basic,_Symbol,Arguments),
        simpleTerms(Arguments),
        projectConds(Conds,[drs(D,[Basic|C])|A1]-A2,S1-S2).

projectConds([],A-A,S-S).


/*========================================================================
   Resolving Alfa-DRSs
========================================================================*/

resolveAlfa(X,Type,AlfaDrs,[drs(D,C)|Others]-[New|Others],S1-S2):-
	global(Others),
	matchDrs(X,AlfaDrs,drs(D,C),New),
	(Type=refl, S2 = S1;
	 Type=nonrefl, S2 = S1;
	 Type=def, S2 = S1;
	 Type=name, S2 = S1).

resolveAlfa(X,Type,AlfaDrs,[drs(D,C)|Others]-[New|Others],S1-S2):-
	nonglobal(Others),
	matchDrs(X,AlfaDrs,drs(D,C),New),
	(Type=refl, S2 is S1 * 1;
	 Type=nonrefl, S2 is S1 * 1;
	 Type=def, S2 is S1 * 0.5;
	 Type=name, S2 is S1 * 0.2).

resolveAlfa(_,Type,Alfa,[pre(A)]-[pre([Alfa|A])],S1-S2):-
	global([pre(A)]),
	(Type=nonrefl, S2 is S1 * 0.5;
	 Type=def, S2 is S1 * 0.9;
	 Type=name, S2 is S1 * 0.9).

resolveAlfa(_,Type,Alfa,[pre(A)|Others]-[pre([Alfa|A])|Others],S1-S2):-
	nonglobal([pre(A)|Others]),
	(
	%Type=nonrefl, S2 is S1 * 0.1;
	 Type=def, S2 is S1 * 0.7;
	 Type=name, S2 is S1 * 0.2).

resolveAlfa(X,Type,Alfa,[pre(A1)|Others]-[pre([New|A2])|Others],S1-S2):-
	global([pre(A1)|Others]),
	selectFromList(Drs,A1,A2),
	matchDrs(X,Alfa,Drs,New),
	(Type=refl, S2 = S1;
	 Type=nonrefl, S2 = S1;
	 Type=def, S2 = S1;
	 Type=name, S2 = S1).

resolveAlfa(X,Type,Alfa,[pre(A1)|Others]-[pre([New|A2])|Others],S1-S2):-
	nonglobal([pre(A1)|Others]),
	selectFromList(Drs,A1,A2),
	matchDrs(X,Alfa,Drs,New),
	(Type=refl, S2 is S1 * 1;
	 Type=nonrefl, S2 is S1 * 1;
	 Type=def, S2 is S1 * 0.5;
	 Type=name, S2 is S1 * 0.2).

resolveAlfa(X,Type,AlfaDrs,[AnteDrs|Others]-[AnteDrs|NewOthers],S1-S2):-
	resolveAlfa(X,Type,AlfaDrs,Others-NewOthers,S1-S2).

/*========================================================================
   Global or non-global position with respect to stack
========================================================================*/

global([pre(_)]).
global([drs(_,_)|Stack]):- global(Stack).

nonglobal([pre(_),drs(_,_)|_]).
nonglobal([drs(_,_)|Stack]):- nonglobal(Stack).

/*=====================================================================
     Accommodation
=====================================================================*/

accommodate([],B-B).

accommodate([drs(D1,C1)|Presups],drs(D2,C2)-B):-
	appendLists(D1,D2,D3),
        appendLists(C1,C2,C3),
	accommodate(Presups,drs(D3,C3)-B).
