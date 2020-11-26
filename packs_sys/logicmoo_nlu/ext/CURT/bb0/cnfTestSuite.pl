/*************************************************************************

         name: cnfTestSuite.pl
      version: April 19, 2001
  description: Testsuite with Propositional Formulas and known cnf forms
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(cnfTestSuite,[formulaClause/2]).

:- ensure_loaded(comsemOperators).


/*========================================================================
   Formulas and known cnf forms for them.
========================================================================*/

formulaClause(p>q,[[~p,q]]).

formulaClause((p & (q>r)) > s,[[s,~p,q],[s,~p,~r]]).

formulaClause((~(~p v q)) v (~r v p),[[p,~r],[~q,~r,p]]).

formulaClause( (~p > q) > (~r > s),[[~p,r,s],[~q,r,s]]).
