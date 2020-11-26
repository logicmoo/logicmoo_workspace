/*************************************************************************

         name: modelCheckerDRT2.pl (Volume 2, Chapter 1)
      version: July 10, 2001
  description: The revised model checker for DRT
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(modelCheckerDRT2,[evaluate/0,
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

evaluate(Drs,Example):-
   evaluate(Drs,Example,[]-_).


/*========================================================================
   Evaluate a DRS in an example model wrt assignments
========================================================================*/

evaluate(DRS,Example,AssignmentIn-AssignmentOut):-
   example(Example,Model),
   satisfyDrs(DRS,Model,AssignmentIn-AssignmentOut,Result),
   printStatus(Result).
   

/*========================================================================
   Testsuite
========================================================================*/

evaluate:-
   format('~n>>>>> DRT MODEL CHECKER 2 ON TESTSUITE <<<<<~n',[]),
   test(DRS,Example,Assignment,Status),
   format('~n~nInput DRS:',[]),
   printRepresentations([DRS]),
   format('Example Model: ~p~nStatus: ',[Example]),
   printStatus(Status),
   example(Example,Model),
   satisfyDrs(DRS,Model,Assignment-_,Result),
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

satisfyDrs(X,_,_,undef):-
   var(X), !.

satisfyDrs(X,_,_,undef):-
   atomic(X), !.


/*========================================================================
  Discourse Representation Structures
========================================================================*/

satisfyDrs(Drs,Model,G-G,Pol):-
   nonvar(Drs),
   Drs = drs(Dom,Conds),
   nonvar(Dom),
   Dom = [],
   satisfyConditions(Conds,Model,G,Pol).

satisfyDrs(Drs,model(D,F),G-H,pos):-
   nonvar(Drs),
   Drs = drs(Dom,Conds),
   nonvar(Dom),
   Dom = [X|L],
   var(X),
   memberList(V,D),
   satisfyDrs(drs(L,Conds),model(D,F),[g(X,V)|G]-H,pos).

satisfyDrs(Drs,model(D,F),G-G,neg):-
   nonvar(Drs),
   Drs = drs(Dom,Conds),
   nonvar(Dom),
   Dom = [X|L],
   var(X),
   setof(V,memberList(V,D),All),
   setof(V,H^(memberList(V,D),satisfyDrs(drs(L,Conds),model(D,F),[g(X,V)|G]-H,neg)),All).

satisfyDrs(Drs,model(D,F),G-H,undef):-
   nonvar(Drs),
   Drs = drs(Dom,Conds),
   (
    var(Dom)
   ;
    nonvar(Dom),
    Dom = [X|L],
    nonvar(X)
   ;
    nonvar(Dom),
    Dom = [X|L],
    var(X),
    memberList(V,D),
    satisfyDrs(drs(L,Conds),model(D,F),[g(X,V)|G]-H,undef)
   ).
   
/*========================================================================
   Merge
========================================================================*/

satisfyDrs(Drs,Model,G-I,pos):-
   nonvar(Drs),
   Drs = merge(Drs1,Drs2),
   satisfyDrs(Drs1,Model,G-H,pos),
   satisfyDrs(Drs2,Model,H-I,pos).

satisfyDrs(Drs,Model,G-G,neg):-
   nonvar(Drs),
   Drs = merge(Drs1,_),
   satisfyDrs(Drs1,Model,G-_,neg).

satisfyDrs(Drs,Model,G-G,neg):-
   nonvar(Drs),
   Drs = merge(Drs1,Drs2),
   satisfyDrs(Drs1,Model,G-H,pos),
   satisfyDrs(Drs2,Model,H-_,neg).

satisfyDrs(Drs,Model,G-G,undef):-
   nonvar(Drs),
   Drs = merge(Drs1,_),
   satisfyDrs(Drs1,Model,G-_,undef).

satisfyDrs(Drs,Model,G-G,undef):-
   nonvar(Drs),
   Drs = merge(Drs1,Drs2),   
   satisfyDrs(Drs1,Model,G-H,pos),
   satisfyDrs(Drs2,Model,H-_,undef).


/*========================================================================
   DRS Conditions
========================================================================*/

satisfyConditions(Conds,_,_,undef):-
   var(Conds).

satisfyConditions(Conds,_,_,pos):-
   nonvar(Conds),
   Conds=[].

satisfyConditions(Conds,Model,G,neg):-
   nonvar(Conds),
   Conds=[Cond],
   satisfyCondition(Cond,Model,G,neg).

satisfyConditions(Conds,Model,G,pos):-
   nonvar(Conds),
   Conds=[C|L],
   satisfyCondition(C,Model,G,pos),
   satisfyConditions(L,Model,G,pos).

satisfyConditions(Conds,Model,G,neg):-
   nonvar(Conds),
   Conds=[C,_|_],
   satisfyCondition(C,Model,G,neg).

satisfyConditions(Conds,Model,G,neg):-
   nonvar(Conds),
   Conds=[_,C|L],
   satisfyConditions([C|L],Model,G,neg).

satisfyConditions(Conds,Model,G,undef):-
   nonvar(Conds),
   Conds=[C|_],
   satisfyCondition(C,Model,G,undef).

satisfyConditions(Conds,Model,G,undef):-
   nonvar(Conds),
   Conds=[_|L],
   satisfyConditions(L,Model,G,undef).



/*========================================================================
   DRS Conditions: Variables or Atoms
========================================================================*/

satisfyCondition(X,_,_,undef):-
   var(X), !.

satisfyCondition(X,_,_,undef):-
   atomic(X), !.


/*========================================================================
   Disjunction
========================================================================*/

satisfyCondition(Cond,Model,G,pos):-
   nonvar(Cond),
   Cond = (Drs1 v Drs2),
   (
    satisfyDrs(Drs1,Model,G-_,pos)
   ;   
    satisfyDrs(Drs2,Model,G-_,pos)
   ).

satisfyCondition(Drs1 v Drs2,Model,G,neg):-
   nonvar(Cond),
   Cond = (Drs1 v Drs2),
   satisfyDrs(Drs1,Model,G-_,neg),
   satisfyDrs(Drs2,Model,G-_,neg).

satisfyCondition(Cond,Model,G,undef):-
   nonvar(Cond),
   Cond = (Drs1 v Drs2),
   (
    satisfyDrs(Drs1,Model,G-_,undef)
   ;   
    satisfyDrs(Drs2,Model,G-_,undef)
   ).


/*========================================================================
   Implication
========================================================================*/

satisfyCondition(Cond,Model,G,pos):-
   nonvar(Cond),
   Cond = (Drs1 > Drs2),
   satisfyDrs(Drs1,Model,G-H,pos),
   satisfyDrs(Drs2,Model,H-_,pos).

satisfyCondition(Cond,Model,G,pos):-
   nonvar(Cond),
   Cond = (Drs > _),
   satisfyDrs(Drs,Model,G-_,neg).

satisfyCondition(Cond,Model,G,neg):-
   nonvar(Cond),
   Cond = (Drs1 > Drs2),
   satisfyDrs(Drs1,Model,G-H,pos),
   satisfyDrs(Drs2,Model,H-_,neg).

satisfyCondition(Cond,Model,G,undef):-
   nonvar(Cond),
   Cond = (Drs > _),
   satisfyDrs(Drs,Model,G-_,undef).

satisfyCondition(Cond,Model,G,undef):-
   nonvar(Cond),
   Cond = (Drs1 > Drs2),
   satisfyDrs(Drs1,Model,G-H,pos),
   satisfyDrs(Drs2,Model,H-_,undef).


/*========================================================================
   Negation
========================================================================*/

satisfyCondition(Cond,Model,G,pos):-
   nonvar(Cond),
   Cond = (~ Drs),
   satisfyDrs(Drs,Model,G-_,neg).

satisfyCondition(Cond,Model,G,neg):-
   nonvar(Cond),
   Cond = (~ Drs),
   satisfyDrs(Drs,Model,G-_,pos).

satisfyCondition(Cond,Model,G,undef):-
   nonvar(Cond),
   Cond = (~ Drs),
   satisfyDrs(Drs,Model,G-_,undef).


/*========================================================================
   Equality
========================================================================*/

satisfyCondition(Cond,Model,G,pos):- 
   nonvar(Cond),
   Cond= (X=Y),
   i(X,Model,G,Value1),
   i(Y,Model,G,Value2),
   Value1=Value2.

satisfyCondition(Cond,Model,G,neg):- 
   nonvar(Cond),
   Cond= (X=Y),
   i(X,Model,G,Value1),
   i(Y,Model,G,Value2),
   \+ Value1=Value2.

satisfyCondition(Cond,Model,G,undef):-
   nonvar(Cond),
   Cond= (X=Y),
   (
    \+ i(X,Model,G,_)
   ;
    \+ i(Y,Model,G,_)
   ).


/*========================================================================
   One-place predicates
========================================================================*/

satisfyCondition(Cond,model(D,F),G,pos):-
   nonvar(Cond),
   compose(Cond,Symbol,[Argument]),
   \+ memberList(Symbol,['.','~']),
   i(Argument,model(D,F),G,Value), 
   memberList(f(1,Symbol,Values),F), 
   memberList(Value,Values).

satisfyCondition(Cond,model(D,F),G,neg):-
   nonvar(Cond),
   compose(Cond,Symbol,[Argument]),
   \+ memberList(Symbol,['.','~']),
   i(Argument,model(D,F),G,Value), 
   memberList(f(1,Symbol,Values),F), 
   \+ memberList(Value,Values).

satisfyCondition(Cond,model(D,F),G,undef):-
   nonvar(Cond),
   compose(Cond,Symbol,[Argument]),
   \+ memberList(Symbol,['.','~']),
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

satisfyCondition(Cond,model(D,F),G,pos):-
   nonvar(Cond),
   compose(Cond,Symbol,[Arg1,Arg2]),
   \+ memberList(Symbol,['>','v',drs,'=',merge,'.']),
   i(Arg1,model(D,F),G,Value1),
   i(Arg2,model(D,F),G,Value2), 
   memberList(f(2,Symbol,Values),F), 
   memberList((Value1,Value2),Values).

satisfyCondition(Cond,model(D,F),G,neg):-
   nonvar(Cond),
   compose(Cond,Symbol,[Arg1,Arg2]),
   \+ memberList(Symbol,['>','v',drs,'=',merge,'.']),
   i(Arg1,model(D,F),G,Value1),
   i(Arg2,model(D,F),G,Value2), 
   memberList(f(2,Symbol,Values),F), 
   \+ memberList((Value1,Value2),Values).

satisfyCondition(Cond,model(D,F),G,undef):-
   nonvar(Cond),
   compose(Cond,Symbol,[Arg1,Arg2]),
   \+ memberList(Symbol,['>','v',drs,'=',merge,'.']),
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

