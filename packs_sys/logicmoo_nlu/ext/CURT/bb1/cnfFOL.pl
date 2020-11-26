/*************************************************************************

    File: cnfFOL.pl
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

:- module(cnfFOL,[info/0,
                  prefix/0,
                  infix/0,
                  nnf/2,
                  cnf/2,
                  cnfTestSuite/0]).

:- use_module(comsemPredicates,[infix/0,
                                prefix/0,
                                appendLists/3,
                                basicFormula/1,
				newFunctionCounter/1,
                                substitute/4,
                                compose/3,
				removeDuplicates/2]).

:- use_module(cnfTestSuite,[formulaClause/2]).


/*========================================================================
   Main Predicate
========================================================================*/

cnf(Formula,SetCNF):-
   nnf(Formula,NNF),
   skolemise(NNF,Skolemised,[]),
   cnf([[Skolemised]],[],CNF),
   setCnf(CNF,SetCNF).


/*========================================================================
   Running the Test Suite
========================================================================*/

cnfTestSuite:- 
   formulaClause(Formula,Cnf),
   format('~nInput formula: ~p',[Formula]),
   format('~nKnown cnf: ~p',[Cnf]),
   cnf(Formula,CNF),
   format('~nComputed cnf: ~p~n',[CNF]),
   fail.

cnfTestSuite.


/*========================================================================
   Convert to Negated Normal Form
========================================================================*/

nnf(not(all(X,F)),some(X,N)):-
   nnf(not(F),N).

nnf(all(X,F),all(X,N)):-
   nnf(F,N).

nnf(not(some(X,F)),all(X,N)):-
   nnf(not(F),N).

nnf(some(X,F),some(X,N)):-
   nnf(F,N).

nnf(not(and(F1,F2)),or(N1,N2)):- 
   nnf(not(F1),N1), 
   nnf(not(F2),N2).
   
nnf(and(F1,F2),and(N1,N2)):- 
   nnf(F1,N1),
   nnf(F2,N2).
   
nnf(not(or(F1,F2)),and(N1,N2)):-
   nnf(not(F1),N1),
   nnf(not(F2),N2).
   
nnf(or(F1,F2),or(N1,N2)):-
   nnf(F1,N1),
   nnf(F2,N2).

nnf(not(imp(F1,F2)),and(N1,N2)):- 
   nnf(F1,N1),
   nnf(not(F2),N2).
   
nnf(imp(F1,F2),or(N1,N2)):- 
   nnf(not(F1),N1),
   nnf(F2,N2).
   
nnf(not(not(F1)),N1):-
   nnf(F1,N1).
   
nnf(F1,F1):-
   literal(F1).


/*========================================================================
   Literals
========================================================================*/

literal(not(F)):- basicFormula(F).
literal(F):- basicFormula(F).


/*========================================================================
   Convert From Negative Normal Form to Conjunctive Normal Form
========================================================================*/

cnf([],_,[]).

cnf([[]|Tcon],Lit,[Lit|NewTcon]):-
   !,
   cnf(Tcon,[],NewTcon).

cnf([[and(F1,F2)|Tdis]|Tcon],Lits,Output):-
   !,
   appendLists(Tdis,Lits,Full),
   cnf([[F1|Full],[F2|Full]|Tcon],[],Output).

cnf([[or(F1,F2)|Tdis]|Tcon],Lits,Output):-
   !,
   cnf([[F1,F2|Tdis]|Tcon],Lits,Output).

cnf([[Lit|Tdis]|Tcon],Lits,Output):-
   cnf([Tdis|Tcon],[Lit|Lits],Output).


/*========================================================================
   Remove Duplicates and Sort
========================================================================*/

setCnf(Cnf1,Cnf2):-
   setCnf(Cnf1,[],Cnf2).

setCnf([],Cnf1,Cnf2):-
   removeDuplicates(Cnf1,Cnf2).

setCnf([X1|L1],L2,L3):-
   removeDuplicates(X1,X2),
   sort(X2,X3),
   setCnf(L1,[X3|L2],L3).


/*========================================================================
   Skolemise
========================================================================*/

skolemise(all(X,F),N,Vars):- 
   skolemise(F,N,[X|Vars]).

skolemise(some(X,F1),N,Vars):-
   skolemFunction(Vars,SkolemTerm),
   substitute(SkolemTerm,X,F1,F2),
   skolemise(F2,N,Vars).

skolemise(and(F1,F2),and(N1,N2),Vars):-
   skolemise(F1,N1,Vars),
   skolemise(F2,N2,Vars).

skolemise(or(F1,F2),or(N1,N2),Vars):-
   skolemise(F1,N1,Vars),
   skolemise(F2,N2,Vars).

skolemise(F,F,_):-
   literal(F).


/*========================================================================
   VarList is a list of free variables, and SkolemTerm is a previously 
   unused Skolem function symbol fun(N) applied to those free variables.
========================================================================*/

skolemFunction(VarList,SkolemTerm):-
   newFunctionCounter(N),
   compose(SkolemTerm,fun,[N|VarList]).


/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n> cnfFOL.pl, by Patrick Blackburn and Johan Bos                       <',[]),
   format('~n>                                                                     <',[]),
   format('~n> ?- cnf(Formula,CNF).  - converts formula in conjunctive normal form <',[]),
   format('~n> ?- nnf(Formula,CNF).  - converts formula in negative normal form    <',[]),
   format('~n> ?- cnfTestSuite.      - runs the test suite for CNF conversion      <',[]),
   format('~n> ?- infix.             - switches to infix display mode              <',[]),
   format('~n> ?- prefix.            - switches to prefix display mode             <',[]),
   format('~n> ?- info.              - show this information                       <',[]),
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.

