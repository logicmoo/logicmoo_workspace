/*************************************************************************

    File: folTestSuite.pl
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

:- module(folTestSuite,[formula/2]).


/*========================================================================
   Formulas
========================================================================*/

formula(all(X,or(p(X),not(p(X)))),theorem).

formula(some(X,or(p(X),not(p(X)))),theorem).

formula(imp(all(X,p(X)),not(some(Y,not(p(Y))))),theorem).

formula(imp(all(X,imp(p(X),q(X))),imp(all(X,p(X)),all(X,q(X)))),theorem).

formula(imp(imp(p(a),all(X,q(X))),all(X,imp(p(a),q(X)))),theorem).

formula(imp(all(X,p(X)),p(c)),theorem).

formula(some(X,imp(p(X),all(X,p(X)))),theorem).

formula(imp(some(Y,all(X,r(X,Y))),all(X,some(Y,r(X,Y)))),theorem).

formula(imp(all(X,or(p(X),q(X))),or(some(X,p(X)),some(X,q(X)))),theorem).

formula(some(X,all(Y,all(Z,imp(imp(p(Y),q(Z)),imp(p(X),q(X)))))),theorem).

formula(some(X,all(Y,all(Z,imp(or(p(Y),q(Z)),or(p(X),q(X)))))),theorem).

formula(some(X,all(Y,all(Z,all(W,imp(or(p(Y),or(q(Z),r(W))), 
                                     or(p(X),or(q(X),r(X)))))))),theorem).

formula(imp(all(X,all(Y,and(p(X),p(Y)))),some(X,some(Y,or(p(X),p(Y))))),theorem).

formula(imp(all(X,all(Y,and(p(X),p(Y)))),all(X,all(Y,or(p(X),p(Y))))),theorem).

formula(all(X,some(Y,all(Z,some(W,or(r(X,Y),not(r(W,Z))))))),theorem).

formula(imp(and(all(X,e(X,X)),
                all(V1,all(V2,all(W1,all(W2,imp(and(e(V1,W1),e(V2,W2)), 
                                                imp(e(V1,V2),e(W1,W2)))))))),
            all(X,all(Y,all(Z,imp(e(X,Y),imp(e(Y,Z),e(X,Z))))))),theorem).


% Schubert's Steamroller
%
% Wolves, foxes, birds, caterpillars, and snails are animals, and
% there are some of each of them. Also there are some grains, and
% grains are plants. Every animal either likes to eat all plants
% or all animals much smaller than itself that like to eat some
% plants. Caterpillars and snails are much smaller than birds,
% which are much smaller than foxes, which in turn are much
% smaller than wolves. Wolves do not like to eat foxes or grains,
% while birds like to eat caterpillars but not snails.
% Caterpillars and snails like to eat some plants. Therefore
% there is an animal that likes to eat a grain eating animal.

formula(imp(and(all(X,imp(wolf(X),animal(X))),
            and(all(X,imp(fox(X),animal(X))),
            and(all(X,imp(bird(X),animal(X))),
            and(all(X,imp(caterpillar(X),animal(X))),
            and(all(X,imp(snail(X),animal(X))),
            and(all(X,imp(grain(X),plant(X))),
            and(some(Y,wolf(Y)),
            and(some(Y,fox(Y)),
            and(some(Y,bird(Y)),
            and(some(Y,caterpillar(Y)),
            and(some(Y,snail(Y)),
            and(some(Y,grain(Y)),
            and(all(X,imp(animal(X),or(all(Y,imp(plant(Y),eats(X,Y))),
                    all(Y1,imp(and(animal(Y1),and(much_smaller(Y1,X),
                        some(Z,and(plant(Z),eats(Y1,Z))))),eats(X,Y1)))))),
            and(all(X,all(Y,imp(and(bird(Y),or(snail(X),caterpillar(X))),
                much_smaller(X,Y)))),
            and(all(X,all(Y,imp(and(bird(X),fox(Y)),much_smaller(X,Y)))),
            and(all(X,all(Y,imp(and(fox(X),wolf(Y)),much_smaller(X,Y)))),
            and(all(X,all(Y,imp(and(wolf(X),or(fox(X),grain(Y))),not(eats(X,Y))))),
            and(all(X,all(Y,imp(and(bird(X),caterpillar(Y)),eats(X,Y)))),
            and(all(X,all(Y,imp(and(bird(X),snail(Y)),not(eats(X,Y))))),
                all(X,imp(or(caterpillar(X),snail(X)),
                      some(Y,and(plant(Y),eats(X,Y)))))))))))))))))))))))),
            some(X,some(Y,and(animal(X),and(animal(Y),some(Z,and(
                 grain(Z),and(eats(Y,Z),eats(X,Y))))))))),theorem).

formula(some(X,and(man(X),some(Y,woman(Y)))),satisfiable).

formula(some(X,and(man(X),all(Y,imp(woman(Y),love(X,Y))))),satisfiable).

formula(all(Y,imp(woman(Y),some(X,and(man(X),love(X,Y))))),satisfiable).

formula(some(X,or(p(X),q(X))),satisfiable).

formula(some(X,and(p(X),not(p(X)))),unsatisfiable).
