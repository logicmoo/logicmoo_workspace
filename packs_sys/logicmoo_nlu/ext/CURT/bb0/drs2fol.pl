/*************************************************************************

         name: drs2fol.pl (Volume 2, Chapter 1)
      version: June 8, 1998
  description: From DRSs to First-Order Logic 
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(drs2fol,[drs2fol/2]).

:- ensure_loaded(comsemOperators).

:- use_module(comsemPredicates,[compose/3,basicFormula/1]).

/*========================================================================

   Translation Predicates
   ----------------------

drs2fol(+Drs,?F)
   converts DRS to formula F. DRS is a term drs(Dom,Cond), where
   Dom is a list of discourse referents, and Cond is a non empty
   list of DRS-conditions. 

========================================================================*/

drs2fol(drs([],[Cond]),Formula):-
   cond2fol(Cond,Formula).

drs2fol(drs([],[Cond1,Cond2|Conds]),Formula1 & Formula2):-
   cond2fol(Cond1,Formula1),
   drs2fol(drs([],[Cond2|Conds]),Formula2).

drs2fol(drs([X|Referents],Conds),exists(X,Formula)):-
   drs2fol(drs(Referents,Conds),Formula).

cond2fol(~ Drs, ~ Formula):-
   drs2fol(Drs,Formula).

cond2fol(Drs1 v Drs2, Formula1 v Formula2):-
   drs2fol(Drs1,Formula1),
   drs2fol(Drs2,Formula2).

cond2fol(drs([],Conds) > Drs2, Formula1 > Formula2):-
   drs2fol(drs([],Conds),Formula1),
   drs2fol(Drs2,Formula2).

cond2fol(drs([X|Referents],Conds) > Drs2, forall(X,Formula)):-
   cond2fol(drs(Referents,Conds) > Drs2,Formula).

/*
cond2fol(question(X,drs([],Conds),Drs2),exists(X,(Formula1 & Formula2))):-
   drs2fol(drs([],Conds),Formula1),
   drs2fol(Drs2,Formula2).

cond2fol(question(Q,drs([X|Referents],Conds),Drs2), exists(X,Formula)):-
   cond2fol(question(Q,drs(Referents,Conds),Drs2),Formula).
*/

cond2fol(question(Q,B1,B2),Condition2):-
        B1 = drs(D,C),
	Condition1 = (Field1 v drs([],[Field2 v Field3])),
	Field1 = drs([X],[allpos(X),drs([Q|D],C) > B2]),
	Field2 = drs([X],[allneg(X),drs([Q|D],C) > drs([],[~B2])]),
	Field3 = drs([X,Q|D],[posneg(X),~drs([],[~B2]),~drs([],[~drs([Q|D],[~B2|C])])|C]),
	cond2fol(Condition1,Condition2).

cond2fol(BasicCondition,AtomicFormula):-
   basicFormula(BasicCondition),
   AtomicFormula=BasicCondition.



