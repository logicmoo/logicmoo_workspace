/*************************************************************************

    File: freeVarCheck.pl
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

:- module(freeVarCheck,[freeVarCheckDrs/1]).

:- use_module(comsemPredicates,[compose/3,
                                memberList/2,
                                basicCondition/3]).


/*========================================================================
   Free Variable Check (main predicate)
========================================================================*/

freeVarCheckDrs(Drs):-
   freeVarCheckDrs(Drs,[]-_).


/*========================================================================
   Free Variable Check (DRSs)
========================================================================*/

freeVarCheckDrs(drs([X|D],C),L1-L2):-
   freeVarCheckDrs(drs(D,C),[X|L1]-L2).

freeVarCheckDrs(drs([],C),L-L):-
   freeVarCheckConds(C,L).

freeVarCheckDrs(merge(B1,B2),L1-L3):-
   freeVarCheckDrs(B1,L1-L2),
   freeVarCheckDrs(B2,L2-L3).

freeVarCheckDrs(alfa(_,B1,B2),L1-L3):-
   freeVarCheckDrs(B1,L1-L2),
   freeVarCheckDrs(B2,L2-L3).


/*========================================================================
   Free Variable Check (List of conditions)
========================================================================*/

freeVarCheckConds([],_).

freeVarCheckConds([X|C],L):-
   freeVarCheckCond(X,L),
   freeVarCheckConds(C,L).


/*========================================================================
   Free Variable Check (Conditions)
========================================================================*/

freeVarCheckCond(X:_,L):- !,
   freeVarCheckCond(X,L).

freeVarCheckCond(not(B),L):-
   freeVarCheckDrs(B,L-_).

freeVarCheckCond(imp(B1,B2),L1):-
   freeVarCheckDrs(B1,L1-L2),
   freeVarCheckDrs(B2,L2-_).

freeVarCheckCond(or(B1,B2),L):-
   freeVarCheckDrs(B1,L-_),
   freeVarCheckDrs(B2,L-_).

freeVarCheckCond(Basic,L):-
   basicCondition(Basic,_,Args),
   checkTerms(Args,L).


/*========================================================================
   Check Terms
========================================================================*/

checkTerms([],_).

checkTerms([X|T],L):-
   var(X),
   memberList(Y,L),
   X==Y,
   checkTerms(T,L).

checkTerms([X|T],L):-
   atom(X),
   checkTerms(T,L).
