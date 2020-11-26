/*************************************************************************

    File: modelCheckerTestSuite.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB1, version 1.2 (August 2005).

    BB1 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB1; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(modelCheckerTestSuite,[test/4]).


/*========================================================================
   Check given formula in given model with given assignment. 
   Correct answer recorded as fourth argument.
========================================================================*/

test(some(X,robber(X)),1,[],pos).

test(some(X,some(Y,love(X,Y))),1,[],pos).

test(some(X,some(Y,love(X,Y))),2,[],neg).

test(all(X,all(Y,love(X,Y))),2,[],neg).

test(not(all(X,all(Y,love(X,Y)))),2,[],pos).

test(all(X,all(Y,not(love(X,Y)))),2,[],pos).

test(eq(yolanda,honey_bunny),2,[],pos).

test(eq(mia,honey_bunny),2,[],undef).

test(not(eq(yolanda,honey_bunny)),2,[],neg).

test(not(eq(mia,honey_bunny)),2,[],undef).

test(all(X,or(robber(X),customer(X))),2,[],pos).

test(not(all(X,or(robber(X),customer(X)))),2,[],neg).

test(or(robber(X),customer(X)),2,[],undef).

test(or(robber(X),customer(X)),2,[g(X,d3)],pos).

test(some(X,and(man(X),some(X,woman(X)))),3,[],pos).

test(and(some(X,man(X)),some(X,woman(X))),3,[],pos).

test(not(some(X,woman(X))),3,[],neg).

test(some(X,and(tasty(X),burger(X))),3,[],undef).

test(not(some(X,and(tasty(X),burger(X)))),3,[],undef).

test(some(X,and(man(X),not(some(Y,woman(Y))))),3,[],neg).

test(some(X,and(man(X),not(some(X,woman(X))))),3,[],neg).

test(some(X,and(woman(X),not(some(X,customer(X))))),2,[],undef).

