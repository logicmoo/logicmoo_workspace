/*************************************************************************

         name: propTableaux2.pl (Chapter 4)
      version: April 26, 2001
  description: Propositional Tableaux Program
      authors: Patrick Blackburn & Johan Bos
  modified by: Christof Rumpf, 05.12.2002
 
*************************************************************************/

:- module(propTableaux,[tprove/1,
		        tproveTestSuite/0]).

:- ensure_loaded(comsemOperators).

:- use_module(comsemPredicates,[memberList/2,removeFirst/3]).

:- use_module(propTestSuite,[formula/2]).


/*========================================================================
   Main Predicate
========================================================================*/

tprove(F):-
   (
    closedTableau([[f(F)]]), !,
    write('Theorem.'), nl
   ;  
    write('Not a theorem.'), nl
   ).


/*========================================================================
   Prove all formulas from the test suite
========================================================================*/

tproveTestSuite:-
        format('~n~n>>>>> TABLEAUX PROVER ON TEST SUITE <<<<<~n',[]),
        formula(Formula,Status),
        format('~nInput formula: ~p~nStatus: ~p',[Formula,Status]),
        format('~nProver says: ',[]),
	tprove(Formula), 
        fail.

tproveTestSuite.



/*========================================================================

   General Tableau Predicates
   --------------------------

closedTableau(+Tableau) 
   Every branch of Tableau contains a contradiction.

closedBranch(+Branch)
   Branch contains a contradiction.
   
tprove(+Formula) 
   Try to create a closed tableau expansion for f(Formula). 

========================================================================*/

closedTableau([]).

closedTableau(OldTableau):-
   expand(OldTableau,NewTableau),
   closedTableau(NewTableau).

closedBranch(Branch):-
    memberBranch(t(X),Branch),
    memberBranch(f(X),Branch).
    
memberBranch(X,[X|_]).
memberBranch(X,[_|T]):- memberBranch(X,T).
memberBranch(X,[H|_]):- memberBranch(X,H).

/*========================================================================

   Tableau Expansion Predicates
   ----------------------------

expand(+Oldtableau,-Newtableau) 
   Newtableaux is the result of applying a tableaux expansion 
   rule to Oldtableaux.

unaryExpansion(+Branch,-NewBranch)
   Take Branch as input, and return NewBranches if a tableau rule
   allows unary expansion.

conjunctiveExpansion(+Branch,-NewBranch)
   Take Branch as input, and return the NewBranch if a tableau rule 
   allows conjunctive expansion. 

disjunctiveExpansion(+Branch,-NewBranch1,-NewBranch2)
   Take Branch as input, and return the NewNranch1 and NewBranch2 
   if a tableau rule allows disjunctive expansion. 

insert(+Branch,+Tableau,-NewTableau) 
   Take Branch and Tableau as input. If Branch is not closed, add Branch
   as new head to Tableau, and return result as NewTableau. Otherwise,
   New Tableau is just Tableau.

========================================================================*/

expand([Branch|Tableau],NewTableau):-
   unaryExpansion(Branch,NewBranch),!,
   insert(NewBranch,Tableau,NewTableau).

expand([Branch|Tableau],NewTableau):-
   conjunctiveExpansion(Branch,NewBranch),!,
   insert(NewBranch,Tableau,NewTableau).

expand([Branch|Tableau],NewTableau):-
   disjunctiveExpansion(Branch,NewBranch1,NewBranch2),!,
   insert(NewBranch1,Tableau,TempTableau), 
   insert(NewBranch2,TempTableau,NewTableau).

expand([Branch|Rest],[Branch|Newrest]):-
   expand(Rest,Newrest).

unaryExpansion(Branch,[Component|Temp]) :-
   unary(SignedFormula,Component),
   removeFirst(SignedFormula,Branch,Temp).

conjunctiveExpansion(Branch,[Comp1,Comp2|Temp]):-
   conjunctive(SignedFormula,Comp1,Comp2),
   removeFirst(SignedFormula,Branch,Temp).

disjunctiveExpansion(Branch,[Comp1|Temp],[Comp2|Temp]):-
   disjunctive(SignedFormula,Comp1,Comp2),
   removeFirst(SignedFormula,Branch,Temp).

insert(Branch,Tableau,NewTableau) :-
   ( closedBranch(Branch), 
      NewTableau = Tableau,!
    ;
      NewTableau = [Branch|Tableau]
   ).

removeFirstNode(Node,[Node|Tree],Tree):- !.
removeFirstNode(NodeX,[Node|Tree1],[Node|Tree2]):-
   removeFirstNode(NodeX,Tree1,Tree2).

branchSet2tree(BranchSet,Tree):- branchSet2tree(BranchSet,[],Tree).
branchSet2tree([],Tree,Tree).
branchSet2tree([Branch|BranchSet],OldTree,Tree):-
	insertBranch(Branch,OldTree,NewTree),
	branchSet2tree(BranchSet,NewTree,Tree).

insertBranch([],L,L).
insertBranch([Node|Nodes1],[Node|Nodes2],L):- !,
	insertBranch(Nodes1,Nodes2,L).
insertBranch([Node],

/*========================================================================

   Formula Identification
   ----------------------

conjunctive(?F,?Comp1,?Comp2)
   F is a conjunctive signed formula with components Comp1 and Comp2.

disjunctive(?F,?Comp1,?Comp2) 
   F is a disjunctive signed formula with components Comp1 and Comp2.

unary(?F,?Comp) 
   F is a signed formula with component Comp.

========================================================================*/

conjunctive(t(X & Y),t(X),t(Y)).
conjunctive(f(X v Y),f(X),f(Y)).
conjunctive(f(X > Y),t(X),f(Y)).

disjunctive(f(X & Y),f(X),f(Y)).
disjunctive(t(X v Y),t(X),t(Y)).
disjunctive(t(X > Y),f(X),t(Y)).

unary(t(~X),f(X)).
unary(f(~X),t(X)). 
