/*

prob_instance('NatureLover','kevin',P).
P=0.348

*/

subClassOf(someValuesFrom('hasAnimal','Pet'),'NatureLover').
%subClassOf(allValuesFrom('hasAnimal','Pet'),'NatureLover').
propertyAssertion('hasAnimal','kevin','fuffy').
propertyAssertion('hasAnimal','kevin','tom').
classAssertion('Cat','fuffy').
classAssertion('Cat','tom').
subClassOf('Cat','Pet').

p(classAssertion('Cat','tom'),0.3).
p(classAssertion('Cat','fuffy'),0.4).
p(subClassOf('Cat','Pet'),0.6).
