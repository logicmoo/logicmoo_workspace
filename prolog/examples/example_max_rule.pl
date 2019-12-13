:- use_module(library(trill)).

:- trill.

/*
% subClassOf('a', maxCardinality(2, 's', 'c')).
subClassOf('a', maxCardinality(1, 's', 'c')).
propertyAssertion('s', '1', '2').
propertyAssertion('s', '1', '3').
propertyAssertion('s', '1', '4').
% propertyAssertion('s', '1', '5').
classAssertion('a', '1').
classAssertion('c', '2').
classAssertion('g', '2').
classAssertion('c', '3').
classAssertion('f', '3').
classAssertion('c', '4').
classAssertion(complementOf('g'), '4').
classAssertion('c', '5').
classAssertion(complementOf('f'), '5').
*/

subClassOf('a', maxCardinality(1, 's', 'c')).
classAssertion('c', '2').
classAssertion('c', '3').
classAssertion('c', '4').
classAssertion('b', '2').
classAssertion('e', '3').
classAssertion('f', '4').
disjointClasses(['b','e','f']).
classAssertion('a', '1').
propertyAssertion('s', '1', '2').
propertyAssertion('s', '1', '3').
propertyAssertion('s', '1', '4').