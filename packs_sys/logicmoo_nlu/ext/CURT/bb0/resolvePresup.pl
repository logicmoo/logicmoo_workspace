/**********************************************************************

         name: resolvePresup.pl
      version: May 5, 1998
  description: Presupposition Resolution
      authors: Patrick Blackburn & Johan Bos

**********************************************************************/

:- module(resolvePresup,[projectDrs/1,accommodate/2]).

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

projectDrs([merge(B1,B2)|A1]-[drs(D3,C3)|A3]):-
        projectDrs([B1|A1]-A2),
        projectDrs([B2|A2]-[drs(D1,C1),drs(D2,C2)|A3]),
        appendLists(D1,D2,D3),
        appendLists(C1,C2,C3).

projectDrs([alfa(X,Type,B1,B3)|A1]-A4):-
	projectDrs([B1|A1]-[B2|A2]), 
        resolveAlfa(X,B2,A2-A3),
	properBinding(Type,X,B3),
	projectDrs([B3|A3]-A4).

projectDrs([drs(D1,C1)|A1]-A2):-
        projectConds(C1,[drs(D1,[])|A1]-A2).


/*=====================================================================
   Presupposition Projection for DRS-Conditions
=====================================================================*/

projectConds([~B1|Conds],A1-A3):-
        projectDrs([B1,pre([])|A1]-[B2,pre(Pres),drs(D,C)|A2]),
	accommodate(Pres,B2-B3),
        projectConds(Conds,[drs(D,[~B3|C])|A2]-A3).

projectConds([B1 > B2|Conds],A1-A4):-
        projectDrs([B1,pre([])|A1]-A2),
        projectDrs([B2,pre([])|A2]-[B4,pre(P2),B3,pre(P1),drs(D,C)|A3]),
	accommodate(P1,B3-B5),
	accommodate(P2,B4-B6),
        projectConds(Conds,[drs(D,[B5 > B6|C])|A3]-A4).

projectConds([question(X,B1,B2)|Conds],A1-A4):-
        projectDrs([B1,pre([])|A1]-A2),
        projectDrs([B2,pre([])|A2]-[B4,pre(P2),B3,pre(P1),drs(D,C)|A3]),
	accommodate(P1,B3-B5),
	accommodate(P2,B4-B6),
        projectConds(Conds,[drs(D,[question(X,B5,B6)|C])|A3]-A4).

projectConds([B1 v B2|Conds],A1-A4):-
        projectDrs([B1,pre([])|A1]-[B3,pre(P1)|A2]),
        projectDrs([B2,pre([])|A2]-[B4,pre(P2),drs(D,C)|A3]),
	accommodate(P1,B3-B5),
	accommodate(P2,B4-B6),
        projectConds(Conds,[drs(D,[B5 v B6|C])|A3]-A4).

projectConds([Basic|Conds],[drs(D,C)|A1]-A2):-
        compose(Basic,_Symbol,Arguments),
        simpleTerms(Arguments),
        projectConds(Conds,[drs(D,[Basic|C])|A1]-A2).

projectConds([],A-A).

/*========================================================================
   Resolving Alfa-DRSs
========================================================================*/

resolveAlfa(X,AlfaDrs,[drs(D,C)|Others]-[New|Others]):-
	matchDrs(X,AlfaDrs,drs(D,C),New).

resolveAlfa(_,AlfaDrs,[pre(A)|Others]-[pre([AlfaDrs|A])|Others]).

resolveAlfa(X,Alfa,[pre(A1)|Others]-[pre([New|A2])|Others]):-
	selectFromList(Drs,A1,A2),
	matchDrs(X,Alfa,Drs,New).
	
resolveAlfa(X,AlfaDrs,[AnteDrs|Others]-[AnteDrs|NewOthers]):-
	resolveAlfa(X,AlfaDrs,Others-NewOthers).

/*=====================================================================
     Accommodation
=====================================================================*/

accommodate([],B-B).

accommodate([drs(D1,C1)|Presups],drs(D2,C2)-B):-
	appendLists(D1,D2,D3),
        appendLists(C1,C2,C3),
	accommodate(Presups,drs(D3,C3)-B).
