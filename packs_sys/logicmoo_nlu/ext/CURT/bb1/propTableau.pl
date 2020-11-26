/*************************************************************************

    File: propTableau.pl
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

:- module(propTableau,[info/0,
                       infix/0,
                       prefix/0,
                       tprove/1,
		       tproveTestSuite/0]).

:- use_module(comsemPredicates,[infix/0,
                                prefix/0,
                                memberList/2,
                                removeFirst/3]).

:- use_module(propTestSuite,[formula/2]).


/*========================================================================
   Main Predicate
========================================================================*/

tprove(F):-
   (
    closedTableau([[f(F)]]), !,
    write('Theorem.'), nl
   ;  
    write('Not a theorem.'), nl
   ).


/*========================================================================
   Prove all formulas from the testsuite
========================================================================*/

tproveTestSuite:-
   nl,write('>>>>> TABLEAU PROVER ON TESTSUITE <<<<< '),nl,
   formula(Formula,X),
   nl, 
   write('Input formula: '),print(Formula), nl,
   write('Status: '),write(X),nl,
   write('Prover says: '),tprove(Formula), nl,
   fail.

tproveTestSuite.


/*========================================================================
   General Tableau Predicates
========================================================================*/

closedTableau([]).

closedTableau(OldTableau):-
   expand(OldTableau,TempTableau),
   removeClosedBranches(TempTableau,NewTableau),
   closedTableau(NewTableau).


/*========================================================================
   Remove all closed branches
========================================================================*/

removeClosedBranches([],[]).

removeClosedBranches([Branch|Rest],Tableau):-
   closedBranch(Branch), !,
   removeClosedBranches(Rest,Tableau).

removeClosedBranches([Branch|Rest],[Branch|Tableau]):-
   removeClosedBranches(Rest,Tableau).

closedBranch(Branch):-
    memberList(t(X),Branch),
    memberList(f(X),Branch).


/*========================================================================
    Expand by applying tableau expansion rules
========================================================================*/

expand([Branch|Tableau],[NewBranch|Tableau]):-
   unaryExpansion(Branch,NewBranch), !.

expand([Branch|Tableau],[NewBranch|Tableau]):-
   conjunctiveExpansion(Branch,NewBranch), !.

expand([Branch|Tableau],[NewBranch1,NewBranch2|Tableau]):-
   disjunctiveExpansion(Branch,NewBranch1,NewBranch2), !.

expand([Branch|Rest],[Branch|Newrest]):-
   expand(Rest,Newrest).


/*========================================================================
    The tableau expansion rules
========================================================================*/

unaryExpansion(Branch,[Component|Temp]):-
   unary(SignedFormula,Component),
   removeFirst(SignedFormula,Branch,Temp).

conjunctiveExpansion(Branch,[Comp1,Comp2|Temp]):-
   conjunctive(SignedFormula,Comp1,Comp2),
   removeFirst(SignedFormula,Branch,Temp).

disjunctiveExpansion(Branch,[Comp1|Temp],[Comp2|Temp]):-
   disjunctive(SignedFormula,Comp1,Comp2),
   removeFirst(SignedFormula,Branch,Temp).


/*========================================================================
   Formula Identification
========================================================================*/

conjunctive(t(and(X,Y)),t(X),t(Y)).
conjunctive(f(or(X,Y)),f(X),f(Y)).
conjunctive(f(imp(X,Y)),t(X),f(Y)).

disjunctive(f(and(X,Y)),f(X),f(Y)).
disjunctive(t(or(X,Y)),t(X),t(Y)).
disjunctive(t(imp(X,Y)),f(X),t(Y)).

unary(t(not(X)),f(X)).
unary(f(not(X)),t(X)). 


/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n> propTableau.pl, by Patrick Blackburn and Johan Bos                  <',[]),
   format('~n>                                                                     <',[]),
   format('~n> ?- tprove(Formula).   - Try to prove Formula                        <',[]),
   format('~n> ?- tproveTestSuite.   - runs the test suite for theorem proving     <',[]),
   format('~n> ?- infix.             - switches to infix display mode              <',[]),
   format('~n> ?- prefix.            - switches to prefix display mode             <',[]),
   format('~n> ?- info.              - show this information                       <',[]),
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.
