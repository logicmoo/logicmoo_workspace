/*************************************************************************

         name: modelCheckerDRT1.pl (Volume 2, Chapter 1)
      version: July 10, 2001
  description: The basic model checker for DRT
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(modelCheckerDRT1,[evaluate/0,
			    evaluate/2,
			    evaluate/3,
			    satisfyDrs/4]).

:- ensure_loaded(comsemOperators).

:- use_module(comsemPredicates,[memberList/2,
                                compose/3,
                                printRepresentations/1]).

:- use_module(exampleModels,[example/2]).

:- use_module(modelCheckerTestSuiteDRT,[test/4]).


/*========================================================================
   Evaluate a DRS in an example model
========================================================================*/

evaluate(DRS,Example):-
   evaluate(DRS,Example,[]-_).


/*========================================================================
   Evaluate a DRS in an example model wrt to assignments
========================================================================*/

evaluate(DRS,Example,AssignmentIn-AssignmentOut):-
   example(Example,Model),
   (	
       satisfyDrs(DRS,Model,AssignmentIn-AssignmentOut,pos), !,
       printStatus(pos)
   ;
       printStatus(neg)
   ).
   

/*========================================================================
   Testsuite
========================================================================*/

evaluate:-
   format('~n>>>>> DRT MODEL CHECKER 1 ON TESTSUITE <<<<<~n',[]),
   test(DRS,Example,Assignment,Status),
   format('~n~nInput DRS:',[]),
   printRepresentations([DRS]),
   format('Example Model: ~p~nStatus: ',[Example]),
   printStatus(Status),
   format('~nModel Checker says: ',[]),
   evaluate(DRS,Example,Assignment-_),
   fail.

evaluate.


/*========================================================================
   Print status of a testsuite example
========================================================================*/

printStatus(pos):- write('Satisfied in model. ').
printStatus(neg):- write('Not satisfied in model. ').
printStatus(undef):- write('Cannot be evaluated. ').


/*========================================================================
  Discourse Representation Structures
========================================================================*/

satisfyDrs(drs([],Conds),Model,G-G,Pol):-
   satisfyConditions(Conds,Model,G,Pol).

satisfyDrs(drs([X|L],Conds),model(D,F),G-H,pos):-
   memberList(V,D),
   satisfyDrs(drs(L,Conds),model(D,F),[g(X,V)|G]-H,pos).

satisfyDrs(drs([X|L],Conds),model(D,F),G-G,neg):-
   setof(V,memberList(V,D),All),
   setof(V,(memberList(V,D),
            satisfyDrs(drs(L,Conds),model(D,F),[g(X,V)|G]-_,neg)),All).


/*========================================================================
   Merge
========================================================================*/

satisfyDrs(merge(Drs1,Drs2),Model,G-I,pos):-
   satisfyDrs(Drs1,Model,G-H,pos),
   satisfyDrs(Drs2,Model,H-I,pos).

satisfyDrs(merge(Drs,_),Model,G-G,neg):-
   satisfyDrs(Drs,Model,G-_,neg).

satisfyDrs(merge(Drs1,Drs2),Model,G-G,neg):-
   satisfyDrs(Drs1,Model,G-H,pos),
   satisfyDrs(Drs2,Model,H-_,neg).


/*========================================================================
   DRS Conditions
========================================================================*/

satisfyConditions([],_,_,_).
satisfyConditions([C|L],Model,G,Pol):-
   satisfyCondition(C,Model,G,Pol),
   satisfyConditions(L,Model,G,Pol).


/*========================================================================
   Disjunction
========================================================================*/

satisfyCondition(Drs1 v Drs2,Model,G,pos):-
   satisfyDrs(Drs1,Model,G-_,pos);
   satisfyDrs(Drs2,Model,G-_,pos).

satisfyCondition(Drs1 v Drs2,Model,G,neg):-
   satisfyDrs(Drs1,Model,G-_,neg),
   satisfyDrs(Drs2,Model,G-_,neg).


/*========================================================================
   Implication
========================================================================*/

satisfyCondition(Drs1 > Drs2,Model,G,pos):-
   satisfyDrs(Drs1,Model,G-H,pos),
   satisfyDrs(Drs2,Model,H-_,pos).

satisfyCondition(Drs > _,Model,G,pos):-
   satisfyDrs(Drs,Model,G-_,neg).

satisfyCondition(Drs1 > Drs2,Model,G,neg):-
   satisfyDrs(Drs1,Model,G-H,pos),
   satisfyDrs(Drs2,Model,H-_,neg).



/*========================================================================
   Negation
========================================================================*/

satisfyCondition(~ Drs,Model,G,pos):-
   satisfyDrs(Drs,Model,G-_,neg).

satisfyCondition(~ Drs,Model,G,neg):-
   satisfyDrs(Drs,Model,G-_,pos).


/*========================================================================
   Equality
========================================================================*/

satisfyCondition(equal(X,Y),Model,G,pos):-
   i(X,Model,G,Value1),
   i(Y,Model,G,Value2),
   Value1=Value2.

satisfyCondition(equal(X,Y),Model,G,neg):-
   i(X,Model,G,Value1),
   i(Y,Model,G,Value2),
   \+ Value1=Value2.


/*========================================================================
   One-place predicates
========================================================================*/

satisfyCondition(Cond,model(D,F),G,pos):-
   compose(Cond,Symbol,[Argument]),
   i(Argument,model(D,F),G,Value),
   memberList(f(1,Symbol,Values),F),
   memberList(Value,Values).

satisfyCondition(Cond,model(D,F),G,neg):-
   compose(Cond,Symbol,[Argument]),
   i(Argument,model(D,F),G,Value),
   memberList(f(1,Symbol,Values),F),
   \+ memberList(Value,Values).


/*========================================================================
   Two-place predicates
========================================================================*/

satisfyCondition(Cond,model(D,F),G,pos):-
   compose(Cond,Symbol,[Arg1,Arg2]),
   i(Arg1,model(D,F),G,Value1),
   i(Arg2,model(D,F),G,Value2),
   memberList(f(2,Symbol,Values),F),
   memberList((Value1,Value2),Values).

satisfyCondition(Cond,model(D,F),G,neg):-
   compose(Cond,Symbol,[Arg1,Arg2]),
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
   
