/*************************************************************************

    File: alphaConversionDRT.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB2, version 1.0 (June 2004).

    BB2 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB2 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB2; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(alphaConversionDRT,[alphaConvertDRS/2]).

:- use_module(comsemPredicates,[compose/3,
				memberList/2]).


/*========================================================================
   Alpha Conversion (introducing substitutions)
========================================================================*/

alphaConvertDRS(B1,B2):-
	alphaConvertDRS(B1,[]-_,B2).


/*========================================================================
   Alpha Conversion (variables)
========================================================================*/

alphaConvertVar(X,Vars,New):-
   var(X), 
   (
      memberList(sub(Z,Y),Vars),
      X==Z, !,
      New=Y
   ;
      New=X
   ).


/*========================================================================
   Alpha Conversion (DRSs)
========================================================================*/

alphaConvertDRS(X1,Vars-Vars,X2):-
   var(X1), 
   alphaConvertVar(X1,Vars,X2).

alphaConvertDRS(Exp,Vars-Vars,lam(Y,B2)):-
   nonvar(Exp),
   Exp=lam(X,B1),
   alphaConvertDRS(B1,[sub(X,Y)|Vars]-_,B2).

alphaConvertDRS(Exp,Vars-Vars,drs([],[])):-
   nonvar(Exp),
   Exp=drs([],[]).

alphaConvertDRS(Exp,Vars1-Vars2,drs([],[C2|Conds2])):-
   nonvar(Exp),
   Exp=drs([],[C1|Conds1]),
   alphaConvertCondition(C1,Vars1,C2), 
   alphaConvertDRS(drs([],Conds1),Vars1-Vars2,drs([],Conds2)).

alphaConvertDRS(Exp,Vars1-Vars2,drs([New|L2],C2)):-
   nonvar(Exp),
   Exp=drs([Ref|L1],C1),
   alphaConvertDRS(drs(L1,C1),[sub(Ref,New)|Vars1]-Vars2,drs(L2,C2)).

alphaConvertDRS(Exp,Vars1-Vars3,alfa(Type,B3,B4)):-
   nonvar(Exp),
   Exp=alfa(Type,B1,B2),
   alphaConvertDRS(B1,Vars1-Vars2,B3),
   alphaConvertDRS(B2,Vars2-Vars3,B4).

alphaConvertDRS(Exp,Vars1-Vars3,merge(B3,B4)):-
   nonvar(Exp),
   Exp=merge(B1,B2),
   alphaConvertDRS(B1,Vars1-Vars2,B3),
   alphaConvertDRS(B2,Vars2-Vars3,B4).

alphaConvertDRS(Exp,Vars-Vars,app(E3,E4)):-
   nonvar(Exp),
   Exp=app(E1,E2),
   alphaConvertDRS(E1,Vars-_,E3),
   alphaConvertDRS(E2,Vars-_,E4).


/*========================================================================
   Alpha Conversion (DRS-Conditions)
========================================================================*/

alphaConvertCondition(not(B1),Vars,not(B2)):-
   alphaConvertDRS(B1,Vars-_,B2).

alphaConvertCondition(imp(B1,B2),Vars,imp(B3,B4)):-
    alphaConvertDRS(B1,Vars-Vars1,B3),
    alphaConvertDRS(B2,Vars1-_,B4).

alphaConvertCondition(or(B1,B2),Vars,or(B3,B4)):-
    alphaConvertDRS(B1,Vars-_,B3),
    alphaConvertDRS(B2,Vars-_,B4).

alphaConvertCondition(Cond1:F,Vars,Cond2:F):-
    alphaConvertCondition(Cond1,Vars,Cond2).

alphaConvertCondition(Cond1,Vars,Cond2):-
   \+ Cond1 = not(_),
   \+ Cond1 = imp(_,_),
   \+ Cond1 = or(_,_),
   \+ Cond1 = _:_,
   compose(Cond1,Symbol,Args1),
   alphaConvertList(Args1,Vars,Args2),
   compose(Cond2,Symbol,Args2).


/*========================================================================
   Alpha Conversion (listwise)
========================================================================*/

alphaConvertList([],_,[]).

alphaConvertList([X|L1],Vars,[Y|L2]):-
   var(X),
   alphaConvertVar(X,Vars,Y),
   alphaConvertList(L1,Vars,L2).

alphaConvertList([X|L1],Vars,[X|L2]):-
   atom(X),
   alphaConvertList(L1,Vars,L2).


