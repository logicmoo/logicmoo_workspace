
/*************************************************************************

         name: folTestSuite.pl
      version: July 27, 2001
  description: Test Suite with Propositional Formulas for Provers 
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(folTestSuite,[formula/2]).

:- ensure_loaded(comsemOperators).


/*========================================================================
   Formulas
========================================================================*/

formula(forall(X,p(X) v ~p(X)),'Theorem').

formula(exists(X,p(X) v ~p(X)),'Theorem').

formula(exists(X,p(X) v q(X)),'Non-theorem, Satisfiable').

formula(exists(X,p(X) & ~p(X)),'Non-theorem, Unsatisfiable').

formula(forall(X,p(X)) > ~ exists(Y,~ p(Y)),'Theorem').

formula(forall(X,p(X) > q(X)) > (forall(X,p(X)) > forall(X,q(X))),'Theorem').

formula(exists(X,man(X) & exists(Y,woman(Y))),'Not a theorem, Satisfiable').

formula((p > forall(X,q(X))) > forall(X,p > q(X)),'Theorem').
