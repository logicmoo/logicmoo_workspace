/*************************************************************************

         name: modelChecker1.pl (Volume 1, Chapter 1)
      version: April 18, 2001
  description: The basic model checker
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(modelChecker1,[evaluate/0,
			 evaluate/2,
			 evaluate/3,
			 satisfy/4]).

:- ensure_loaded(comsemOperators).

:- use_module(comsemPredicates,[memberList/2,
                                compose/3,
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
   (	
       satisfy(Formula,Model,Assignment,pos), !,
       printStatus(pos)
   ;
       printStatus(neg)
   ).
   

/*========================================================================
   Testsuite
========================================================================*/

evaluate:-
   format('~n>>>>> MODEL CHECKER 1 ON TESTSUITE <<<<<~n',[]),
   test(Formula,Example,Assignment,Status),
   format('~n~nInput formula:',[]),
   printRepresentations([Formula]),
   format('Example Model: ~p~nStatus: ',[Example]),
   printStatus(Status),
   format('~nModel Checker says: ',[]),
   evaluate(Formula,Example,Assignment),
   fail.

evaluate.


/*========================================================================
   Print status of a testsuite example
========================================================================*/

printStatus(pos):- write('Satisfied in model. ').
printStatus(neg):- write('Not satisfied in model. ').
printStatus(undef):- write('Cannot be evaluated. ').


/*========================================================================
   Existential Quantification
========================================================================*/

satisfy(exists(X,Formula),model(D,F),G,pos):-
   memberList(V,D),
   satisfy(Formula,model(D,F),[g(X,V)|G],pos).

satisfy(exists(X,Formula),model(D,F),G,neg):-
   setof(V,(memberList(V,D),satisfy(Formula,model(D,F),[g(X,V)|G],neg)),D).
   

/*========================================================================
   Universal Quantification
========================================================================*/

satisfy(forall(X,Formula),Model,G,Pol):-
   satisfy(~ exists(X,~ Formula),Model,G,Pol).


/*========================================================================
   Conjunction
========================================================================*/

satisfy(Formula1 & Formula2,Model,G,pos):-
   satisfy(Formula1,Model,G,pos),
   satisfy(Formula2,Model,G,pos).

satisfy(Formula1 & Formula2,Model,G,neg):-
   satisfy(Formula1,Model,G,neg);
   satisfy(Formula2,Model,G,neg).


/*========================================================================
   Disjunction
========================================================================*/

satisfy(Formula1 v Formula2,Model,G,pos):-
   satisfy(Formula1,Model,G,pos);
   satisfy(Formula2,Model,G,pos).

satisfy(Formula1 v Formula2,Model,G,neg):-
   satisfy(Formula1,Model,G,neg),
   satisfy(Formula2,Model,G,neg).


/*========================================================================
   Implication
========================================================================*/

satisfy(Formula1 > Formula2,Model,G,Pol):-
   satisfy((~Formula1 v Formula2),Model,G,Pol).


/*========================================================================
   Negation
========================================================================*/

satisfy(~ Formula,Model,G,pos):-
   satisfy(Formula,Model,G,neg).

satisfy(~ Formula,Model,G,neg):-
   satisfy(Formula,Model,G,pos).


/*========================================================================
   Equality
========================================================================*/

satisfy(X=Y,Model,G,pos):-
   i(X,Model,G,Value1),
   i(Y,Model,G,Value2),
   Value1=Value2.

satisfy(X=Y,Model,G,neg):-
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
   



