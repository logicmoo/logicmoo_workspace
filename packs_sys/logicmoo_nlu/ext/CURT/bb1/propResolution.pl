/*************************************************************************

    File: propResolution.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB1, version 1.2 (August 2005).

    BB1 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB1; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(propResolution,[info/0,
                          infix/0,
                          prefix/0, 
                          rprove/1,
                          rprove/2,
			  rproveTestSuite/0]).

:- use_module(comsemPredicates,[infix/0,
                                prefix/0,
                                memberList/2,
				appendLists/3,
				unionSets/3,
				selectFromList/3]).

:- use_module(propTestSuite,[formula/2]).

:- use_module(cnf,[cnf/2]).


/*========================================================================
   Main Predicates
========================================================================*/

rprove(Formula):-
   cnf(not(Formula),CNF),
   refute(CNF).

rprove(Formula,Result):-
   (  
      rprove(Formula), !,
      Result='Theorem'
   ;
      Result='Not a theorem'
   ).
   

/*========================================================================
   Try all formulas from the test suite
========================================================================*/

rproveTestSuite:-
   format('~n~n>>>>> RESOLUTION PROVER ON TEST SUITE <<<<<~n',[]),
   formula(Formula,Status),
   format('~nInput formula: ~p~nStatus: ~p',[Formula,Status]),
   rprove(Formula,Result), 
   format('~nProver: ~p~n',[Result]),
   fail.

rproveTestSuite.


/*========================================================================
   Refute
========================================================================*/

refute(C):-
   memberList([],C).

refute(C):-
   \+ memberList([],C),
   resolveList(C,[],Output),
   unionSets(Output,C,NewC),
   \+ NewC = C, 
   refute(NewC).


/*========================================================================
    Resolve a list of clauses
========================================================================*/

resolveList([],Acc,Acc).

resolveList([Clause|List],Acc1,Acc3):-
   resolveClauseList(List,Clause,Acc1,Acc2),
   resolveList(List,Acc2,Acc3).


/*========================================================================
   Resolve a clause against a list
========================================================================*/

resolveClauseList([],_,Acc,Acc).

resolveClauseList([H|L],Clause,Acc1,Acc3):-
   resolve(Clause,H,Result), 
   unionSets([Result],Acc1,Acc2),
   resolveClauseList(L,Clause,Acc2,Acc3).

resolveClauseList([H|L],Clause,Acc1,Acc2):-
   \+ resolve(Clause,H,_),
   resolveClauseList(L,Clause,Acc1,Acc2).


/*========================================================================
   Resolve two clauses
========================================================================*/

resolve(Clause1,Clause2,NewClause):-
   selectFromList(Lit,Clause1,Temp1),
   selectFromList(not(Lit),Clause2,Temp2), 
   unionSets(Temp1,Temp2,NewClause).

resolve(Clause1,Clause2,NewClause):-
   selectFromList(not(Lit),Clause1,Temp1),
   selectFromList(Lit,Clause2,Temp2),
   unionSets(Temp1,Temp2,NewClause).


/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> -------------------------------------------------------------------- <',[]),
   format('~n> propResolution.pl, by Patrick Blackburn and Johan Bos                <',[]),
   format('~n>                                                                      <',[]),
   format('~n> ?- rprove(Formula).         - Succeeds if Formula is theorem         <',[]),
   format('~n> ?- rprove(Formula,Result).  - Try to prove Formula (always succeeds) <',[]),
   format('~n> ?- rproveTestSuite.         - Runs the test suite                    <',[]),
   format('~n> ?- infix.                   - switches to infix display mode         <',[]),
   format('~n> ?- prefix.                  - switches to prefix display mode        <',[]),
   format('~n> ?- info.                    - show this information                  <',[]),
   format('~n> -------------------------------------------------------------------- <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.

