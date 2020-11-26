/*************************************************************************

         name: cnf.pl (Chapter 4)
      version: January 5, 2001
  description: Conversion to Conjunctive Normal Form
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(cnf,[nnf/2,cnf/2,cnfTestSuite/0]).

:- ensure_loaded(comsemOperators).

:- use_module(comsemPredicates,[appendLists/3,
				removeDuplicates/2]).

:- use_module(cnfTestSuite,[formulaClause/2]).


/*========================================================================
   Main Predicate
========================================================================*/

cnf(Formula,SetCNF):-
   nnf(Formula,NNF),
   cnf([[NNF]],[],CNF),
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

nnf(~(F1 & F2),N1 v N2):- 
   nnf(~ F1,N1), 
   nnf(~ F2,N2).
   
nnf(F1 & F2,N1 & N2):- 
   nnf(F1,N1),
   nnf(F2,N2).
   
nnf(~(F1 v F2),N1 & N2):-
   nnf(~ F1,N1),
   nnf(~ F2,N2).
   
nnf(F1 v F2,N1 v N2):-
   nnf(F1,N1),
   nnf(F2,N2).

nnf(~(F1 > F2),N1 & N2):- 
   nnf(F1,N1),
   nnf(~ F2,N2).
   
nnf(F1 > F2,N1 v N2):- 
   nnf(~ F1,N1),
   nnf(F2,N2).
   
nnf(~ ~ F1,N1):-
   nnf(F1,N1).
   
nnf(F1,F1):-
   literal(F1).


/*========================================================================
   Literals
========================================================================*/

literal(~ F):- atomic(F).
literal(F):- atomic(F).


/*========================================================================
   Convert From Negative Normal Form to Conjunctive Normal Form
========================================================================*/

cnf([],_,[]).

cnf([[]|Tcon],Lit,[Lit|NewTcon]):-
   !,
   cnf(Tcon,[],NewTcon).

cnf([[F1 & F2|Tdis]|Tcon],Lits,Output):-
   !,
   appendLists(Tdis,Lits,Full),
   cnf([[F1|Full],[F2|Full]|Tcon],[],Output).

cnf([[F1 v F2|Tdis]|Tcon],Lits,Output):-
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
