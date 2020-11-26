/*************************************************************************

    File: freeVarTabl.pl
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

:- module(freeVarTabl,[info/0,
                       infix/0,
                       prefix/0,
                       tprove/1,
                       tprove/2,
		       tprove/3,
		       tproveTestSuite/0]).

:- use_module(comsemPredicates,[infix/0,
                                prefix/0,
                                memberList/2,
				removeFirst/3,
				appendLists/3,
                                basicFormula/1,
                                compose/3,
				newFunctionCounter/1,
                                substitute/4]).

:- use_module(folTestSuite,[formula/2]).


/*========================================================================
   Expand Tableau until it is closed, allowing Qdepth 
   applications of the universal rule.
========================================================================*/

closedTableau([],_Q):- !.

closedTableau(OldTableau,Qdepth):-
   expand(OldTableau,Qdepth,TempTableau,NewQdepth), !,
   removeClosedBranches(TempTableau,NewTableau),
   closedTableau(NewTableau,NewQdepth).


/*========================================================================
   Remove all closed branches
========================================================================*/

removeClosedBranches([],[]).

removeClosedBranches([Branch|Rest],Tableau):-
   closedBranch(Branch), !,
   removeClosedBranches(Rest,Tableau).

removeClosedBranches([Branch|Rest],[Branch|Tableau]):-
   removeClosedBranches(Rest,Tableau).


/*========================================================================
   Check whether a branch is closed
========================================================================*/

closedBranch(Branch):-
   memberList(n(_,t(X)),Branch),
   memberList(n(_,f(Y)),Branch),
   basicFormula(X), 
   basicFormula(Y),
   unify_with_occurs_check(X,Y).


/*========================================================================
   VarList is a list of free variables, and SkolemTerm is a previously 
   unused Skolem function symbol fun(N) applied to those free variables.
========================================================================*/

skolemFunction(VarList,SkolemTerm):-
   newFunctionCounter(N),
   compose(SkolemTerm,fun,[N|VarList]).


/*========================================================================
   Try to create a tableau expansion for f(X) that is closed allowing 
   Qdepth applications of the universal rule.
========================================================================*/

tprove(X,Qdepth):-
   notatedFormula(NotatedFormula,[],f(X)),
   closedTableau([[NotatedFormula]],Qdepth).

tprove(X,Qdepth,Result):-
   notatedFormula(NotatedFormula,[],f(X)),
   (
      closedTableau([[NotatedFormula]],Qdepth), !,
      Result = theorem
   ;
      Result = unknown
   ).


/*========================================================================
   Prove a formula with Q-depth of 100 (default)
========================================================================*/

tprove(X):-
   tprove(X,1000).


/*========================================================================
   Notate the free variables of a formula
========================================================================*/

notatedFormula(n(Free,Formula),Free,Formula).


/*========================================================================
   Prove all formulas from the test suite 
========================================================================*/

tproveTestSuite:-
   format('~n~n>>>>> FREE VARIABLE TABLEAU ON TEST SUITE <<<<<',[]),
   formula(Formula,Status),
   format('~n~nInput formula: ~p~nStatus: ~p',[Formula,Status]),
   tprove(Formula,100,Result), 
   format('~nProver: ~p~n',[Result]),
   fail.

tproveTestSuite.


/*========================================================================
   Newtableau with Q-depth NewQdepth is the result of applying  
   a tableau expansion rule to Oldtableau with a Q-depth of OldQdepth.
========================================================================*/

expand([Branch|Tableau],QD,[NewBranch|Tableau],QD):-
   unaryExpansion(Branch,NewBranch).

expand([Branch|Tableau],QD,[NewBranch|Tableau],QD):-
   conjunctiveExpansion(Branch,NewBranch).

expand([Branch|Tableau],QD,[NewBranch|Tableau],QD):-
   existentialExpansion(Branch,NewBranch).

expand([Branch|Tableau],QD,[NewBranch1,NewBranch2|Tableau],QD):-
   disjunctiveExpansion(Branch,NewBranch1,NewBranch2).

expand([Branch|Tableau],OldQD,NewTableau,NewQD):-
   universalExpansion(Branch,OldQD,NewBranch,NewQD),
   appendLists(Tableau,[NewBranch],NewTableau).

expand([Branch|Rest],OldQD,[Branch|Newrest],NewQD):-
   expand(Rest,OldQD,Newrest,NewQD).


/*========================================================================
   Take Branch as input, and return NewBranches if a tableau rule
   allows unary expansion.
========================================================================*/

unaryExpansion(Branch,[NotatedComponent|Temp]) :-
   unary(SignedFormula,Component),
   notatedFormula(NotatedFormula,Free,SignedFormula),
   removeFirst(NotatedFormula,Branch,Temp),
   notatedFormula(NotatedComponent,Free,Component).


/*========================================================================
   Take Branch as input, and return the NewBranch if a tableau rule 
   allows conjunctive expansion. 
========================================================================*/

conjunctiveExpansion(Branch,[NotatedComp1,NotatedComp2|Temp]):-
   conjunctive(SignedFormula,Comp1,Comp2),
   notatedFormula(NotatedFormula,Free,SignedFormula),
   removeFirst(NotatedFormula,Branch,Temp),
   notatedFormula(NotatedComp1,Free,Comp1),
   notatedFormula(NotatedComp2,Free,Comp2).


/*========================================================================
   Take Branch as input, and return the NewBranch1 and NewBranch2 
   if a tableau rule allows disjunctive expansion. 
========================================================================*/

disjunctiveExpansion(Branch,[NotComp1|Temp],[NotComp2|Temp]):-
   disjunctive(SignedFormula,Comp1,Comp2),
   notatedFormula(NotatedFormula,Free,SignedFormula),
   removeFirst(NotatedFormula,Branch,Temp),
   notatedFormula(NotComp1,Free,Comp1),
   notatedFormula(NotComp2,Free,Comp2).


/*========================================================================
   Take Branch as input, and return the NewBranch if a tableau rule 
   allows existential expansion.
========================================================================*/

existentialExpansion(Branch,[NotatedInstance|Temp]):-
   notatedFormula(NotatedFormula,Free,SignedFormula),
   existential(SignedFormula),
   removeFirst(NotatedFormula,Branch,Temp),
   skolemFunction(Free,Term),
   instance(SignedFormula,Term,Instance),
   notatedFormula(NotatedInstance,Free,Instance).


/*========================================================================
   Take Branch and OldQD as input, and return the NewBranch and 
   NewQDepthif a tableau rule allow universal expansion.
========================================================================*/

universalExpansion(Branch,OldQD,NewBranch,NewQD):-
   OldQD > 0, 
   NewQD is OldQD - 1,
   memberList(NotatedFormula,Branch),
   notatedFormula(NotatedFormula,Free,SignedFormula),
   universal(SignedFormula),
   removeFirst(NotatedFormula,Branch,Temp),
   instance(SignedFormula,V,Instance),
   notatedFormula(NotatedInstance,[V|Free],Instance),
   appendLists([NotatedInstance|Temp],[NotatedFormula],NewBranch).


/*========================================================================
   Decompose conjunctive signed formula
========================================================================*/

conjunctive(t(and(X,Y)),t(X),t(Y)).
conjunctive(f(or(X,Y)),f(X),f(Y)).
conjunctive(f(imp(X,Y)),t(X),f(Y)).


/*========================================================================
   Decompose disjunctive signed formula
========================================================================*/

disjunctive(f(and(X,Y)),f(X),f(Y)).
disjunctive(t(or(X,Y)),t(X),t(Y)).
disjunctive(t(imp(X,Y)),f(X),t(Y)).


/*========================================================================
   Decompose unary signed formula
========================================================================*/

unary(t(not(X)),f(X)).
unary(f(not(X)),t(X)). 


/*========================================================================
   Universal Signed Formulas
========================================================================*/

universal(t(all(_,_))).
universal(f(some(_,_))).


/*========================================================================
   Existential Signed Formulas
========================================================================*/

existential(t(some(_,_))).
existential(f(all(_,_))).


/*========================================================================
   Remove quantifier from signed quantified formula, and replacing all
   free occurrences of the quantified variable by occurrences of Term.
========================================================================*/

instance(t(all(X,F)),Term,t(NewF)):- substitute(Term,X,F,NewF).
instance(f(some(X,F)),Term,f(NewF)):- substitute(Term,X,F,NewF).
instance(t(some(X,F)),Term,t(NewF)):- substitute(Term,X,F,NewF).
instance(f(all(X,F)),Term,f(NewF)):- substitute(Term,X,F,NewF).


/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> ----------------------------------------------------------------------------- <',[]),
   format('~n> freeVarTabl.pl, by Patrick Blackburn and Johan Bos                            <',[]),
   format('~n>                                                                               <',[]),
   format('~n> ?- tprove(Form).            - Try to prove Form                               <',[]),
   format('~n> ?- tprove(Form,QDepth).     - Try to prove Form using QDepth                  <',[]),
   format('~n> ?- tprove(Form,QDepth,Res). - Try to prove Form using QDepth, return Res      <',[]),
   format('~n> ?- tproveTestSuite.         - runs the test suite for theorem proving         <',[]),
   format('~n> ?- infix.                   - switches to infix display mode                  <',[]),
   format('~n> ?- prefix.                  - switches to prefix display mode                 <',[]),
   format('~n> ?- info.                    - show this information                           <',[]),
   format('~n> ----------------------------------------------------------------------------- <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.

