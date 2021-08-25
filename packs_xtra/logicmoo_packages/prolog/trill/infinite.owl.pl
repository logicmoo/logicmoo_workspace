/*

QUERY 1:
instanceOf('someRA','a',LE).

EXPL:
LE = [(equivalentClasses([someRA,someValuesFrom('R','A')]),a),(equivalentClasses(['A',someValuesFrom('R','Thing')]),a),classAssertion('A',a),(equivalentClasses([onlyRA,allValuesFrom('R','A')]),a),(equivalentClasses(['Thing',onlyRA]),a),(subClassOf('A','Thing'),a)]

*/

equivalentClasses(['A',someValuesFrom('R','Thing')]).
equivalentClasses(['someRA',someValuesFrom('R','A')]).
equivalentClasses(['onlyRA',allValuesFrom('R','A')]).
equivalentClasses(['Thing','onlyRA']).
subClassOf('A','Thing').
subClassOf('someRA','Thing').
inverseFunctionalProperty('R').
subPropertyOf(S,R).
transitiveProperty(R).
functionalProperty('R').
classAssertion('A','a').
