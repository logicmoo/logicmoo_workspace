
/*************************************************************************

         name: comsemPredicates.pl
      version: July 13, 2001
  description: Term Manipulation, Unification, Printing
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(comsemPredicates,
          [memberList/2,
	   selectFromList/3,
	   appendLists/3,
	   simpleTerms/1,
           compose/3,
	   basicFormula/1,
	   unify/2,
	   removeFirst/3,
	   removeDuplicates/2,
	   ordUnion/3,
           substitute/4,
	   variablesInTerm/2,
	   newFunctionCounter/1,
	   concatStrings/2,
           printRepresentations/1]).


/*========================================================================
   List membership
========================================================================*/

memberList(X,[X|_]).
memberList(X,[_|Tail]):- 
   memberList(X,Tail).


/*========================================================================
   Appending two lists
========================================================================*/

appendLists([],List,List).
appendLists([X|Tail1],List,[X|Tail2]):- 
   appendLists(Tail1,List,Tail2).


/*========================================================================
   Selecting (i.e. removing) a member of a list
========================================================================*/

selectFromList(X,[X|L],L).
selectFromList(X,[Y|L1],[Y|L2]):-
   selectFromList(X,L1,L2).


/*========================================================================
   Removing first member of a list
========================================================================*/

removeFirst(X,[X|Tail],Tail) :- !.
removeFirst(X,[Head|Tail],[Head|NewTail]):-
   removeFirst(X,Tail,NewTail).


/*========================================================================
   Remove Duplicates
========================================================================*/

removeDuplicates([],[]).

removeDuplicates([X|L],Pruned):-
	memberList(Y,L), X==Y, !,
	removeDuplicates(L,Pruned).

removeDuplicates([X|L],[X|Pruned]):-
	removeDuplicates(L,Pruned).


/*========================================================================
   ordUnion (from "The Craft of Prolog", Richard O'Keefe, Pages 156-157)
========================================================================*/

ordUnion([],Set2,Set2).
ordUnion([H1|T1],Set2,Union) :-
   ordUnion2(Set2,H1,T1,Union).

ordUnion2([],H1,T1,[H1|T1]).
ordUnion2([H2|T2],H1,T1,Union):-
   compare(Order,H1,H2),
   ordUnion3(Order,H1,T1,H2,T2,Union).

ordUnion3(<,H1,T1,H2,T2,[H1|Union]):-
   ordUnion2(T1,H2,T2,Union).
ordUnion3(=,H1,T1,_,T2,[H1|Union]):-
   ordUnion(T1,T2,Union).
ordUnion3(>,H1,T1,H2,T2,[H2|Union]):-
   ordUnion2(T2,H1,T1,Union).


/*========================================================================
   Simple Terms
========================================================================*/

simpleTerms([]).

simpleTerms([X|Rest]):-
   simpleTerm(X),
   simpleTerms(Rest).

simpleTerm(T):-
   (
    var(T)
   ;   
    atomic(T)
   ;
    nonvar(T),
    T =.. ['$VAR',_] 
   ).



/*========================================================================
   Compose predicate argument structure
========================================================================*/

compose(Term,Symbol,ArgList):-
    Term =.. [Symbol|ArgList].


/*========================================================================
   Basic Formula Syntax
========================================================================*/

basicFormula(F):-
	var(F), !, fail.

basicFormula(F):-
	compose(F,Symbol,Args),
	simpleTerms([Symbol|Args]).
		

/*========================================================================
   Collect all occurrences of variables in Term to a difference list
========================================================================*/

variablesInTerm(Term,Var1-Var2):-
   compose(Term,_,Args),
   countVar(Args,Var1-Var2).

countVar([],Var-Var).
countVar([X|Rest],Var1-Var2):-
   var(X),!,
   countVar(Rest,[X|Var1]-Var2).
countVar([X|Rest],Var1-Var3):-
   variablesInTerm(X,Var1-Var2),
   countVar(Rest,Var2-Var3).


/*========================================================================
   Unify with Occurs Check
========================================================================*/

unify(X,Y):-
   var(X), var(Y), X=Y.
unify(X,Y):-
   var(X), nonvar(Y), notOccursIn(X,Y), X=Y.
unify(X,Y):-
   var(Y), nonvar(X), notOccursIn(Y,X), X=Y.
unify(X,Y):-
   nonvar(X), nonvar(Y), atomic(X), atomic(Y), X=Y.
unify(X,Y):-
   nonvar(X), nonvar(Y), compound(X), compound(Y), termUnify(X,Y).

notOccursIn(X,Term):-
   var(Term), X \== Term.
notOccursIn(_,Term):-
   nonvar(Term), atomic(Term).
notOccursIn(X,Term):-
   nonvar(Term), compound(Term),
   functor(Term,_,Arity), notOccursInComplexTerm(Arity,X,Term).

notOccursInComplexTerm(N,X,Y):-
   N > 0, arg(N,Y,Arg), notOccursIn(X,Arg),
   M is N - 1, notOccursInComplexTerm(M,X,Y).
notOccursInComplexTerm(0,_,_).

termUnify(X,Y):-
   functor(X,Functor,Arity), functor(Y,Functor,Arity),
   unifyArgs(Arity,X,Y).

unifyArgs(N,X,Y):-
   N > 0, M is N - 1,
   arg(N,X,ArgX), arg(N,Y,ArgY), 
   unify(ArgX,ArgY), unifyArgs(M,X,Y).
unifyArgs(0,_,_).


/*========================================================================
   Substitution Predicates
========================================================================*/

substitute(Term,Var,Exp,Result):- 
   Exp==Var, !, Result=Term.
substitute(_Term,_Var,Exp,Result):- 
   \+ compound(Exp), !, Result=Exp.
substitute(Term,Var,Formula,Result):-
   compose(Formula,Functor,[Exp,F]),
   memberList(Functor,[lambda,forall,exists]), !, 
   (
    Exp==Var, !, 
    Result=Formula
   ; 
    substitute(Term,Var,F,R),
    compose(Result,Functor,[Exp,R])
   ).
substitute(Term,Var,Formula,Result):-
   compose(Formula,Functor,ArgList),
   substituteList(Term,Var,ArgList,ResultList),
   compose(Result,Functor,ResultList).

substituteList(_Term,_Var,[],[]).
substituteList(Term,Var,[Exp|Others],[Result|ResultOthers]):-
   substitute(Term,Var,Exp,Result),
   substituteList(Term,Var,Others,ResultOthers).


/*========================================================================
   Skolem Function Counter
========================================================================*/

:- dynamic(functionCounter/1).

functionCounter(1).

newFunctionCounter(N):-
   functionCounter(N), M is N+1,
   retract(functionCounter(N)),
   asserta(functionCounter(M)).


/*========================================================================
   Printing a set of representations
========================================================================*/

printRepresentations(Readings):-
   printRep(Readings,0).

printRep([],_):- nl.
printRep([Reading|OtherReadings],M):-
   N is M + 1, nl, write(N), tab(1), 
   \+ \+ (numbervars(Reading,0,_), write(Reading)),
   printRep(OtherReadings,N).


/*========================================================================
   Concatenate Strings
========================================================================*/

concatStrings(L,S):-
   concatStrings(L,[],S).

concatStrings([],Codes,String):- 
   name(String,Codes).

concatStrings([X|L],Codes1,String):-
   name(X,Codes2),
   appendLists(Codes1,Codes2,Codes3),
   concatStrings(L,Codes3,String).
   

