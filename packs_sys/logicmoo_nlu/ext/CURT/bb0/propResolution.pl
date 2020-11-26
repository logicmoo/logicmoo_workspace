/*************************************************************************

         name: propResolution.pl (Chapter 4)
      version: May 19, 2000
  description: Propositional Resolution Program
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(propResolution,[rprove/1,
			  rproveTestSuite/0]).

:- ensure_loaded(comsemOperators).

:- use_module(comsemPredicates,[memberList/2,
				appendLists/3,
				ordUnion/3,
				selectFromList/3]).

:- use_module(propTestSuite,[formula/2]).

:- use_module(cnf,[cnf/2]).


/*========================================================================
   Main Predicate
========================================================================*/

rprove(Formula):-
   cnf(~ Formula,CNF),
   refute(CNF,CNF).


/*========================================================================
   Prove all formulas from the test suite
========================================================================*/

rproveTestSuite:-
        format('~n~n>>>>> RESOLUTION PROVER ON TEST SUITE <<<<<~n',[]),
        formula(Formula,Status),
        format('~nInput formula: ~p~nStatus: ~p',[Formula,Status]),
        format('~nProver says: ',[]),
	rprove(Formula), 
        fail.

rproveTestSuite.


/*========================================================================
   Refute
========================================================================*/

refute(_,Old):-
   memberList([],Old), !,
   write('Input formula is a theorem.'), nl.

refute(Unused,_):-
   Unused = [], !,
   write('Input formula is not a theorem.'), nl.

refute(Unused,Old):-
   resolveListList(Unused,Old,Old,[],Output),
   ordUnion(Output,Old,NextOld),
   refute(Output,NextOld).


/*========================================================================
   Resolve a list against a list
========================================================================*/

resolveListList([],_,_,Acc,Acc).

resolveListList([H1|T1],[_|T2],Old,Acc,Output):-
   resolveClauseList(H1,T2,Old,Acc,Temp),
   resolveListList(T1,T2,Old,Temp,Output).


/*========================================================================
   Resolve a clause against a list
========================================================================*/

resolveClauseList(_,[],_,Acc,Acc).

resolveClauseList(Clause,[H|L],Old,Acc,Output):-
   resolve(Clause,H,Result), 
   !,
   (
       memberList(Result,Old), !,
       resolveClauseList(Clause,L,Old,Acc,Output)
   ;   
       memberList(Result,Acc), !,
       resolveClauseList(Clause,L,Old,Acc,Output)
   ;   
       ordUnion([Result],Acc,NewAcc),
       resolveClauseList(Clause,L,Old,NewAcc,Output)
   ).

resolveClauseList(Clause,[_|L],Old,Acc,Output):-
   resolveClauseList(Clause,L,Old,Acc,Output).


/*========================================================================
   Resolve two clauses
========================================================================*/

resolve(Clause1,Clause2,NewClause):-
   selectFromList(Lit,Clause1,Temp1),
   selectFromList(~ Lit,Clause2,Temp2), !, 
   ordUnion(Temp1,Temp2,NewClause).

resolve(Clause1,Clause2,NewClause):-
   selectFromList(~ Lit,Clause1,Temp1),
   selectFromList(Lit,Clause2,Temp2),
   ordUnion(Temp1,Temp2,NewClause).

