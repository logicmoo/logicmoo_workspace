/*************************************************************************

    File: modelChecker2.pl
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

:- module(modelChecker2,[modelCheckerTestSuite/0,
                         info/0,
                         infix/0,
                         prefix/0,
			 evaluate/2,
			 evaluate/3,
			 satisfy/4]).

:- use_module(comsemPredicates,[memberList/2,
                                compose/3, 
                                proveOnce/1,
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
   format('~n>>>>> MODEL CHECKER 2 ON TEST SUITE <<<<<~n~n',[]),
   test(Formula,Example,Assignment,Status),
   format('~n~nInput formula:',[]),
   printRepresentations([Formula]),
   format('Example Model: ~p~nStatus: ',[Example]),
   printStatus(Status),
   example(Example,Model),
   proveOnce(modelChecker2:satisfy(Formula,Model,Assignment,Result)),
   format('~nModel Checker says: ',[]),
   printStatus(Result),
   printComparison(Status,Result),
   fail.

modelCheckerTestSuite.


/*========================================================================
   Print status of a testsuite example
========================================================================*/

printStatus(pos):- write('Satisfied in model. ').
printStatus(neg):- write('Not satisfied in model. ').
printStatus(undef):- write('Cannot be evaluated. ').


/*========================================================================
   Print comparison of result and expected result
========================================================================*/

printComparison(Expected,Result):-
	\+ Expected=Result,
	write('UNEXPECTED RESULT!').

printComparison(Expected,Result):-
	Expected=Result,
	write('OK!').


/*========================================================================
   Variables or Atoms
========================================================================*/

satisfy(X,_,_,undef):-
   var(X), !.

satisfy(X,_,_,undef):-
   atomic(X), !.


/*========================================================================
   Existential Quantification
========================================================================*/

satisfy(Formula,model(D,F),G,pos):-
   nonvar(Formula),
   Formula = some(X,SubFormula),
   var(X),
   memberList(V,D),
   satisfy(SubFormula,model(D,F),[g(X,V)|G],pos).

satisfy(Formula,model(D,F),G,neg):-
   nonvar(Formula),
   Formula = some(X,SubFormula),
   var(X),
   setof(V,memberList(V,D),All),
   setof(V,
         (
          memberList(V,D),
          satisfy(SubFormula,model(D,F),[g(X,V)|G],neg)
         ),
         All).

satisfy(Formula,model(D,F),G,undef):- 
   nonvar(Formula),
   Formula = some(X,SubFormula),
   (
    nonvar(X)
   ;
    var(X),
    memberList(V,D), 
    satisfy(SubFormula,model(D,F),[g(X,V)|G],undef)
   ). 


/*========================================================================
   Universal Quantification
========================================================================*/

satisfy(Formula,Model,G,Polarity):- 
   nonvar(Formula),
   Formula = all(X,SubFormula),
   satisfy(not(some(X,not(SubFormula))),Model,G,Polarity).


/*========================================================================
   Conjunction
========================================================================*/

satisfy(Formula,Model,G,pos):- 
   nonvar(Formula),
   Formula = and(Formula1,Formula2),
   satisfy(Formula1,Model,G,pos),
   satisfy(Formula2,Model,G,pos).

satisfy(Formula,Model,G,neg):- 
   nonvar(Formula),
   Formula = and(Formula1,Formula2),
   satisfy(Formula1,Model,G,pos),
   satisfy(Formula2,Model,G,neg).

satisfy(Formula,Model,G,neg):- 
   nonvar(Formula),
   Formula = and(Formula1,Formula2),
   satisfy(Formula1,Model,G,neg),
   satisfy(Formula2,Model,G,pos).

satisfy(Formula,Model,G,neg):- 
   nonvar(Formula),
   Formula = and(Formula1,Formula2),
   satisfy(Formula1,Model,G,neg),
   satisfy(Formula2,Model,G,neg).

satisfy(Formula,Model,G,undef):- 
   nonvar(Formula),
   Formula = and(Formula1,Formula2),
   (
    satisfy(Formula1,Model,G,undef)
   ;
    satisfy(Formula2,Model,G,undef)
   ).


/*========================================================================
   Disjunction
========================================================================*/

satisfy(Formula,Model,G,pos):- 
   nonvar(Formula),
   Formula = or(Formula1,Formula2),
   satisfy(Formula1,Model,G,pos),
   satisfy(Formula2,Model,G,pos).

satisfy(Formula,Model,G,pos):- 
   nonvar(Formula),
   Formula = or(Formula1,Formula2),
   satisfy(Formula1,Model,G,pos),
   satisfy(Formula2,Model,G,neg).

satisfy(Formula,Model,G,pos):- 
   nonvar(Formula),
   Formula = or(Formula1,Formula2),
   satisfy(Formula1,Model,G,neg),
   satisfy(Formula2,Model,G,pos).

satisfy(Formula,Model,G,neg):- 
   nonvar(Formula),
   Formula = or(Formula1,Formula2),
   satisfy(Formula1,Model,G,neg),
   satisfy(Formula2,Model,G,neg).

satisfy(Formula,Model,G,undef):-
   nonvar(Formula),
   Formula = or(Formula1,Formula2),
   (
    satisfy(Formula1,Model,G,undef)
   ;
    satisfy(Formula2,Model,G,undef)
   ).


/*========================================================================
   Implication
========================================================================*/

satisfy(Formula,Model,G,Polarity):- 
   nonvar(Formula),
   Formula = imp(Formula1,Formula2),
   satisfy(or(not(Formula1),Formula2),Model,G,Polarity).


/*========================================================================
   Negation
========================================================================*/

satisfy(Formula,Model,G,pos):- 
   nonvar(Formula),
   Formula = not(SubFormula),
   satisfy(SubFormula,Model,G,neg).

satisfy(Formula,Model,G,neg):- 
   nonvar(Formula),
   Formula = not(SubFormula),
   satisfy(SubFormula,Model,G,pos).

satisfy(Formula,Model,G,undef):- 
   nonvar(Formula),
   Formula = not(SubFormula),
   satisfy(SubFormula,Model,G,undef).


/*========================================================================
   Equality
========================================================================*/

satisfy(Formula,Model,G,pos):- 
   nonvar(Formula),
   Formula = eq(X,Y),
   i(X,Model,G,Value1),
   i(Y,Model,G,Value2),
   Value1=Value2.

satisfy(Formula,Model,G,neg):- 
   nonvar(Formula),
   Formula = eq(X,Y),
   i(X,Model,G,Value1),
   i(Y,Model,G,Value2),
   \+ Value1=Value2.

satisfy(Formula,Model,G,undef):-
   nonvar(Formula),
   Formula = eq(X,Y),
   (
    \+ i(X,Model,G,_)
   ;
    \+ i(Y,Model,G,_)
   ).


/*========================================================================
   One-place predicates
========================================================================*/

satisfy(Formula,model(D,F),G,pos):-
   nonvar(Formula),
   compose(Formula,Symbol,[Argument]),
   \+ Symbol = not,
   i(Argument,model(D,F),G,Value), 
   memberList(f(1,Symbol,Values),F), 
   memberList(Value,Values).

satisfy(Formula,model(D,F),G,neg):-
   nonvar(Formula),
   compose(Formula,Symbol,[Argument]),
   \+ Symbol = not,
   i(Argument,model(D,F),G,Value), 
   memberList(f(1,Symbol,Values),F), 
   \+ memberList(Value,Values).

satisfy(Formula,model(D,F),G,undef):-
   nonvar(Formula),
   compose(Formula,Symbol,[Argument]),
   \+ Symbol = not,
   (
    \+ var(Argument), 
    \+ atom(Argument)
   ; 
    var(Argument),
    \+ i(Argument,model(D,F),G,_)
   ;
    atom(Argument),
    \+ i(Argument,model(D,F),G,_)
   ;
    \+ memberList(f(1,Symbol,_),F)
   ).


/*========================================================================
   Two-place predicates
========================================================================*/

satisfy(Formula,model(D,F),G,pos):-
   nonvar(Formula),
   compose(Formula,Symbol,[Arg1,Arg2]),
   \+ memberList(Symbol,[eq,imp,or,and,some,all]),
   i(Arg1,model(D,F),G,Value1),
   i(Arg2,model(D,F),G,Value2), 
   memberList(f(2,Symbol,Values),F), 
   memberList((Value1,Value2),Values).

satisfy(Formula,model(D,F),G,neg):-
   nonvar(Formula),
   compose(Formula,Symbol,[Arg1,Arg2]),
   \+ memberList(Symbol,[eq,imp,or,and,some,all]),
   i(Arg1,model(D,F),G,Value1),
   i(Arg2,model(D,F),G,Value2), 
   memberList(f(2,Symbol,Values),F), 
   \+ memberList((Value1,Value2),Values).

satisfy(Formula,model(D,F),G,undef):-
   nonvar(Formula),
   compose(Formula,Symbol,[Arg1,Arg2]),
   \+ memberList(Symbol,[eq,imp,or,and,some,all]),
   (
    \+ var(Arg1), 
    \+ atom(Arg1)
   ; 
    \+ var(Arg2), 
    \+ atom(Arg2)
   ; 
    var(Arg1),
    \+ i(Arg1,model(D,F),G,_)
   ; 
    var(Arg2),
    \+ i(Arg2,model(D,F),G,_)
   ;
    atom(Arg1),
    \+ i(Arg1,model(D,F),G,_)
   ;
    atom(Arg2),
    \+ i(Arg2,model(D,F),G,_)
   ;
    \+ memberList(f(2,Symbol,_),F)
   ).


/*========================================================================
   Interpretation of Constants and Variables
========================================================================*/

i(X,model(D,F),G,Value):-
   (
    var(X),
    memberList(g(Y,Value),G), 
    X==Y, !,
    memberList(Value,D)
   ;
    atom(X),
    memberList(f(0,X,Value),F)
   ).


/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> ------------------------------------------------------------------------- <',[]),
   format('~n> modelChecker2.pl, by Patrick Blackburn and Johan Bos                      <',[]),
   format('~n>                                                                           <',[]),
   format('~n> ?- evaluate(F,E).          - evaluate a formula in a model                <',[]),
   format('~n> ?- evaluate(F,E,A).        - evaluate a formula in a model wrt assignment <',[]),
   format('~n> ?- modelCheckerTestSuite.  - run the test suite                           <',[]),  
   format('~n> ?- infix.                  - switch to infix printing mode                <',[]),
   format('~n> ?- prefix.                 - switch to prefix printing mode               <',[]),
   format('~n> ?- info.                   - print this information                       <',[]),
   format('~n> ------------------------------------------------------------------------- <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.

