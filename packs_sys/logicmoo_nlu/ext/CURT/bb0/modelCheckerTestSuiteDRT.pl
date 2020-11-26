/*************************************************************************

         name: modelCheckerTestSuiteDRT.pl
      version: July 10, 2001
  description: Test Suite for DRT model checkers
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(modelCheckerTestSuiteDRT,[test/4]).

:- ensure_loaded(comsemOperators).


/*========================================================================
   Check given DRS in given model with given assignment. 
   Correct answer recorded as fourth argument.
========================================================================*/

test(drs([X],[robber(X)]),1,[],pos).

test(drs([],[equal(yolanda,honey_bunny)]),2,[],pos).

test(drs([],[equal(mia,honey_bunny)]),2,[],neg).

test(drs([],[~ drs([],[equal(yolanda,honey_bunny)])]),2,[],neg).

test(drs([],[~ drs([],[equal(mia,honey_bunny)])]),2,[],pos).

test(drs([X,Y],[man(X),woman(Y)]),4,[],pos).

test(merge(drs([X],[man(X)]),drs([X],[woman(X)])),4,[],pos).

test(drs([],[~ drs([X],[woman(X)])]),4,[],neg).

test(drs([X],[tasty(X),burger(X)]),4,[],undef).

test(drs([],[~ drs([X],[tasty(X),burger(X)])]),4,[],undef).

test(drs([X],[~ drs([Y],[woman(Y)]),man(X)]),4,[],neg).

test(drs([X],[man(X), (~ drs([Y],[woman(Y)]))]),4,[],neg).

test(drs([X],[man(X), (~ drs([X],[woman(X)]))]),4,[],neg).

test(drs([X],[woman(X), (~ drs([X],[customer(X)]))]),5,[],undef).

test(drs([],[drs([X],[]) > drs([],[drs([],[robber(X)]) v drs([],[customer(X)])])]),2,[],pos).

test(drs([],[~ drs([],[drs([X],[]) > drs([],[drs([],[robber(X)]) v drs([],[customer(X)])])])]),2,[],neg).

test(drs([],[drs([],[robber(X)]) v drs([],[customer(X)])]),2,[],undef).

test(drs([],[drs([],[robber(X)]) v drs([],[customer(X)])]),2,[g(X,d3)],pos).


