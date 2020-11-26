/*************************************************************************

         name: modelCheckerTestSuite.pl
      version: January 15, 2002
  description: Testsuite for model checkers
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(modelCheckerTestSuite,[test/4]).

:- ensure_loaded(comsemOperators).


/*========================================================================
   Check given formula in given model with given assignment. 
   Correct answer recorded as fourth argument.
========================================================================*/

test(exists(X,robber(X)),1,[],pos).

test(exists(X,exists(Y,love(X,Y))),1,[],pos).

test(exists(X,exists(Y,love(X,Y))),2,[],neg).

test(forall(X,forall(Y,love(X,Y))),2,[],neg).

test(~(forall(X,forall(Y,love(X,Y)))),2,[],pos).

test(forall(X,forall(Y,~love(X,Y))),2,[],pos).

test(yolanda=honey_bunny,2,[],pos).

test(mia=honey_bunny,2,[],undef).

test(~yolanda=honey_bunny,2,[],neg).

test(~mia=honey_bunny,2,[],undef).

test(forall(X,robber(X) v customer(X)),2,[],pos).

test(~forall(X,robber(X) v customer(X)),2,[],neg).

test(robber(X) v customer(X),2,[],undef).

test(robber(X) v customer(X),2,[g(X,d3)],pos).

test(exists(X,man(X)&exists(X,woman(X))),3,[],pos).

test(exists(X,man(X))&exists(X,woman(X)),3,[],pos).

test(~ exists(X,woman(X)),3,[],neg).

test(exists(X,tasty(X) & burger(X)),3,[],undef).

test(~ exists(X,tasty(X) & burger(X)),3,[],undef).

test(exists(X,man(X) & ~ exists(Y,woman(Y))),3,[],neg).

test(exists(X,man(X) & ~ exists(X,woman(X))),3,[],neg).

test(exists(X,woman(X) & ~ exists(X,customer(X))),666,[],undef).

