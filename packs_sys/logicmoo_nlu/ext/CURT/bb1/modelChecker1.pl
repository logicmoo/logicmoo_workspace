/*************************************************************************

    File: modelChecker1.pl
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

:- module(modelChecker1,[modelCheckerTestSuite/0,
                         info/0,
                         infix/0,
                         prefix/0,
			 evaluate/2,
			 evaluate/3,
			 satisfy/4]).

:- use_module(comsemPredicates,[memberList/2,
                                compose/3,
                                infix/0,
                                prefix/0,
                                printRepresentations/1]).

:- use_module(exampleModels,[example/2]).

:- use_module(modelCheckerTestSuite,[test/4]).


/*========================================================================
   Evaluate a formula in an example model
========================================================================*/

evaluate(Formula,Example):-
   evaluate(Formula,Example,[]).


/*========================================================================
   Evaluate a formula in an example model wrt an assignment
========================================================================*/

evaluate(Formula,Example,Assignment):-
   example(Example,Model),
   satisfy(Formula,Model,Assignment,Result),
   printStatus(Result).
   

/*========================================================================
   Test Suite
========================================================================*/

modelCheckerTestSuite:-
   format('~n>>>>> MODEL CHECKER 1 ON TEST SUITE <<<<<~n',[]),
   test(Formula,Example,Assignment,Status),
   format('~n~nInput formula:',[]),
   printRepresentations([Formula]),
   format('Example Model: ~p~nStatus: ',[Example]),
   printStatus(Status),
   format('~nModel Checker says: ',[]),
   evaluate(Formula,Example,Assignment),
   fail.

modelCheckerTestSuite.


/*========================================================================
   Print status of a testsuite example
========================================================================*/

printStatus(pos):- write('Satisfied in model. ').
printStatus(neg):- write('Not satisfied in model. ').
printStatus(undef):- write('Cannot be evaluated. ').


/*========================================================================
   Existential Quantification
========================================================================*/

satisfy(some(X,Formula),model(D,F),G,pos):-
   memberList(V,D),
   satisfy(Formula,model(D,F),[g(X,V)|G],pos).

satisfy(some(X,Formula),model(D,F),G,neg):-
   setof(V,(memberList(V,D),satisfy(Formula,model(D,F),[g(X,V)|G],neg)),Dom),
   setof(V,memberList(V,D),Dom).
   

/*========================================================================
   Universal Quantification
========================================================================*/

satisfy(all(X,Formula),Model,G,Pol):-
   satisfy(not(some(X,not(Formula))),Model,G,Pol).


/*========================================================================
   Conjunction
========================================================================*/

satisfy(and(Formula1,Formula2),Model,G,pos):-
   satisfy(Formula1,Model,G,pos),
   satisfy(Formula2,Model,G,pos).

satisfy(and(Formula1,Formula2),Model,G,neg):-
   satisfy(Formula1,Model,G,neg);
   satisfy(Formula2,Model,G,neg).


/*========================================================================
   Disjunction
========================================================================*/

satisfy(or(Formula1,Formula2),Model,G,pos):-
   satisfy(Formula1,Model,G,pos);
   satisfy(Formula2,Model,G,pos).

satisfy(or(Formula1,Formula2),Model,G,neg):-
   satisfy(Formula1,Model,G,neg),
   satisfy(Formula2,Model,G,neg).


/*========================================================================
   Implication
========================================================================*/

satisfy(imp(Formula1,Formula2),Model,G,Pol):-
   satisfy(or(not(Formula1),Formula2),Model,G,Pol).


/*========================================================================
   Negation
========================================================================*/

satisfy(not(Formula),Model,G,pos):-
   satisfy(Formula,Model,G,neg).

satisfy(not(Formula),Model,G,neg):-
   satisfy(Formula,Model,G,pos).


/*========================================================================
   Equality
========================================================================*/

satisfy(eq(X,Y),Model,G,pos):-
   i(X,Model,G,Value1),
   i(Y,Model,G,Value2),
   Value1=Value2.

satisfy(eq(X,Y),Model,G,neg):-
   i(X,Model,G,Value1),
   i(Y,Model,G,Value2),
   \+ Value1=Value2.


/*========================================================================
   One-place predicates
========================================================================*/

satisfy(Formula,model(D,F),G,pos):-
   compose(Formula,Symbol,[Argument]),
   i(Argument,model(D,F),G,Value),
   memberList(f(1,Symbol,Values),F),
   memberList(Value,Values).

satisfy(Formula,model(D,F),G,neg):-
   compose(Formula,Symbol,[Argument]),
   i(Argument,model(D,F),G,Value),
   memberList(f(1,Symbol,Values),F),
   \+ memberList(Value,Values).


/*========================================================================
   Two-place predicates
========================================================================*/

satisfy(Formula,model(D,F),G,pos):-
   compose(Formula,Symbol,[Arg1,Arg2]),
   i(Arg1,model(D,F),G,Value1),
   i(Arg2,model(D,F),G,Value2),
   memberList(f(2,Symbol,Values),F),
   memberList((Value1,Value2),Values).

satisfy(Formula,model(D,F),G,neg):-
   compose(Formula,Symbol,[Arg1,Arg2]),
   i(Arg1,model(D,F),G,Value1),
   i(Arg2,model(D,F),G,Value2),
   memberList(f(2,Symbol,Values),F),
   \+ memberList((Value1,Value2),Values).


/*========================================================================
   Interpretation of Constants and Variables
========================================================================*/

i(X,model(_,F),G,Value):-
   (
       var(X),
       memberList(g(Y,Value),G),
       Y==X, ! % IMPORTANT CUT!
   ;
       atom(X),
       memberList(f(0,X,Value),F)
   ).
   

/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> ------------------------------------------------------------------------- <',[]),
   format('~n> modelChecker1.pl, by Patrick Blackburn and Johan Bos                      <',[]),
   format('~n>                                                                           <',[]),
   format('~n> ?- evaluate(F,E).          - evaluate a formula in a model                <',[]),
   format('~n> ?- evaluate(F,E,A).        - evaluate a formula in a model wrt assignment <',[]),
   format('~n> ?- modelCheckerTestSuite.  - run the test suite                           <',[]),
   format('~n> ?- infix.                  - switch to infix printing mode                <',[]),
   format('~n> ?- prefix.                 - switch to prefix printing mode               <',[]),
   format('~n> ?- info.                   - prints this information                      <',[]),
   format('~n> ------------------------------------------------------------------------- <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.

