:-use_module(library(trill)).

:- trill. % or :- trillp. or :- tornado.

/*
An extract of the well-known Pizza KB, from
N. Drummond.A Practical Guide to Building Owl Ontologies, v1.2. University Manchester,2009.
*/

/** <examples>

?- unsat('tofu',Expls).
?- inconsistent_theory(Expls).

*/

% Axioms

subClassOf(soyCheeseTopping,cheeseTopping).
subClassOf(soyCheeseTopping,vegetableTopping).
subClassOf(tofu,soyCheeseTopping).
disjointClasses([cheeseTopping,vegetableTopping]).
equivalentClasses([pizza1,pizza2]). %pizza1 = pizza2

% classAssertion(tofu,'tofu-1').

