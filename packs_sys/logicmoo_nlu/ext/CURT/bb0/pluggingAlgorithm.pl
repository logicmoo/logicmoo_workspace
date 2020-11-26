/*************************************************************************

         name: pluggingAlgorithm.pl (Volume 1, Chapter 3)
      version: Nov 16, 2000
  description: Plugging Algorithm
       author: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(pluggingAlgorithm,[plugHole/4]).

:- use_module(comsemPredicates,[memberList/2,
				selectFromList/3,
				variablesInTerm/2]).

/*========================================================================
   Plugging Predicates
========================================================================*/

plugHole(Formula,LFs1-LFs3,Constraints,Scoped):-
   selectFromList(Label:Formula,LFs1,LFs2),
   checkConstraints(Label,Constraints,[Formula|Scoped]),
   variablesInTerm(Formula,[]-Arguments),
   checkArguments(Arguments,LFs2-LFs3,Constraints,[Formula|Scoped]).

checkArguments([],LFs-LFs,_,_).

checkArguments([Arg|Rest],LFs1-LFs3,Constraints,Scoped):-
   memberList(leq(_,Hole),Constraints),
   Hole==Arg, !,
   plugHole(Hole,LFs1-LFs2,Constraints,Scoped),
   checkArguments(Rest,LFs2-LFs3,Constraints,Scoped).

checkArguments([Arg|Rest],LFs1-LFs4,Constraints,Scoped):-
   selectFromList(Label:Formula,LFs1,LFs2),
   Label==Arg, !,
   checkConstraints(Label,Constraints,Scoped),
   Formula=Arg, 
   variablesInTerm(Formula,[]-Arguments), 
   checkArguments(Arguments,LFs2-LFs3,Constraints,Scoped),
   checkArguments(Rest,LFs3-LFs4,Constraints,Scoped).

checkArguments([_|Rest],LFs1-LFs2,Constraints,Scoped):-
   checkArguments(Rest,LFs1-LFs2,Constraints,Scoped).

/*========================================================================
   Constraint Checking
========================================================================*/

checkConstraints(_,[],_).

checkConstraints(Label,[leq(L,H)|Constraints],Scoped):-	
   Label==L, !,
   memberList(Formula,Scoped), Formula==H, 
   checkConstraints(Label,Constraints,Scoped).

checkConstraints(Label,[_|Constraints],Scoped):-
   checkConstraints(Label,Constraints,Scoped).

