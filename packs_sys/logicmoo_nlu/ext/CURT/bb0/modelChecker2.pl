/*************************************************************************

         name: modelChecker2.pl (Volume 1, Chapter 1)
      version: April 18, 2001
  description: The revised model checker
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(modelChecker2,[evaluate/0,
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
   satisfy(Formula,Model,Assignment,Result),
   printStatus(Result).
   

/*========================================================================
   Test Suite
========================================================================*/

evaluate:-
   format('~n>>>>> MODEL CHECKER 2 ON TEST SUITE <<<<<~n~n',[]),
   test(Formula,Example,Assignment,Status),
   format('~n~nInput formula:',[]),
   printRepresentations([Formula]),
   format('Example Model: ~p~nStatus: ',[Example]),
   printStatus(Status),
   example(Example,Model),
   satisfy(Formula,Model,Assignment,Result),
   format('~nModel Checker says: ',[]),
   printStatus(Result),
   printComparison(Status,Result),
   fail.

evaluate.


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
   Formula = exists(X,SubFormula),
   var(X),
   memberList(V,D),
   satisfy(SubFormula,model(D,F),[g(X,V)|G],pos).

satisfy(Formula,model(D,F),G,neg):-
   nonvar(Formula),
   Formula = exists(X,SubFormula),
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
   Formula = exists(X,SubFormula),
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
   Formula = forall(X,SubFormula),
   satisfy(~ exists(X,~ SubFormula),Model,G,Polarity).


/*========================================================================
   Conjunction
========================================================================*/

satisfy(Formula,Model,G,pos):- 
   nonvar(Formula),
   Formula = (Formula1 & Formula2),
   satisfy(Formula1,Model,G,pos),
   satisfy(Formula2,Model,G,pos).

satisfy(Formula,Model,G,neg):- 
   nonvar(Formula),
   Formula = (Formula1 & Formula2),
   satisfy(Formula1,Model,G,pos),
   satisfy(Formula2,Model,G,neg).

satisfy(Formula,Model,G,neg):- 
   nonvar(Formula),
   Formula = (Formula1 & Formula2),
   satisfy(Formula1,Model,G,neg),
   satisfy(Formula2,Model,G,pos).

satisfy(Formula,Model,G,neg):- 
   nonvar(Formula),
   Formula = (Formula1 & Formula2),
   satisfy(Formula1,Model,G,neg),
   satisfy(Formula2,Model,G,neg).

satisfy(Formula,Model,G,undef):- 
   nonvar(Formula),
   Formula = (Formula1 & Formula2),
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
   Formula = (Formula1 v Formula2),
   satisfy(Formula1,Model,G,pos),
   satisfy(Formula2,Model,G,pos).

satisfy(Formula,Model,G,pos):- 
   nonvar(Formula),
   Formula = (Formula1 v Formula2),
   satisfy(Formula1,Model,G,pos),
   satisfy(Formula2,Model,G,neg).

satisfy(Formula,Model,G,pos):- 
   nonvar(Formula),
   Formula = (Formula1 v Formula2),
   satisfy(Formula1,Model,G,neg),
   satisfy(Formula2,Model,G,pos).

satisfy(Formula,Model,G,neg):- 
   nonvar(Formula),
   Formula = (Formula1 v Formula2),
   satisfy(Formula1,Model,G,neg),
   satisfy(Formula2,Model,G,neg).

satisfy(Formula,Model,G,undef):-
   nonvar(Formula),
   Formula = (Formula1 v Formula2),
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
   Formula = (Formula1 > Formula2),
   satisfy((~Formula1 v Formula2),Model,G,Polarity).


/*========================================================================
   Negation
========================================================================*/

satisfy(Formula,Model,G,pos):- 
   nonvar(Formula),
   Formula = (~ SubFormula),
   satisfy(SubFormula,Model,G,neg).

satisfy(Formula,Model,G,neg):- 
   nonvar(Formula),
   Formula = (~ SubFormula),
   satisfy(SubFormula,Model,G,pos).

satisfy(Formula,Model,G,undef):- 
   nonvar(Formula),
   Formula = (~ SubFormula),
   satisfy(SubFormula,Model,G,undef).


/*========================================================================
   Equality
========================================================================*/

satisfy(Formula,Model,G,pos):- 
   nonvar(Formula),
   Formula = (X=Y),
   i(X,Model,G,Value1),
   i(Y,Model,G,Value2),
   Value1=Value2.

satisfy(Formula,Model,G,neg):- 
   nonvar(Formula),
   Formula = (X=Y),
   i(X,Model,G,Value1),
   i(Y,Model,G,Value2),
   \+ Value1=Value2.

satisfy(Formula,Model,G,undef):-
   nonvar(Formula),
   Formula = (X=Y),
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
   \+ Symbol = '~',
   i(Argument,model(D,F),G,Value), 
   memberList(f(1,Symbol,Values),F), 
   memberList(Value,Values).

satisfy(Formula,model(D,F),G,neg):-
   nonvar(Formula),
   compose(Formula,Symbol,[Argument]),
   \+ Symbol = '~',
   i(Argument,model(D,F),G,Value), 
   memberList(f(1,Symbol,Values),F), 
   \+ memberList(Value,Values).

satisfy(Formula,model(D,F),G,undef):-
   nonvar(Formula),
   compose(Formula,Symbol,[Argument]),
   \+ Symbol = '~',
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
   \+ memberList(Symbol,['=','>','v','&','<>',exists,forall]),
   i(Arg1,model(D,F),G,Value1),
   i(Arg2,model(D,F),G,Value2), 
   memberList(f(2,Symbol,Values),F), 
   memberList((Value1,Value2),Values).

satisfy(Formula,model(D,F),G,neg):-
   nonvar(Formula),
   compose(Formula,Symbol,[Arg1,Arg2]),
   \+ memberList(Symbol,['=','>','v','&','<>',exists,forall]),
   i(Arg1,model(D,F),G,Value1),
   i(Arg2,model(D,F),G,Value2), 
   memberList(f(2,Symbol,Values),F), 
   \+ memberList((Value1,Value2),Values).

satisfy(Formula,model(D,F),G,undef):-
   nonvar(Formula),
   compose(Formula,Symbol,[Arg1,Arg2]),
   \+ memberList(Symbol,['=','>','v','&','<>',exists,forall]),
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


