:- use_module(library(trill)).

:-trill.

ontology('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10').


class('http://ml.unife.it/disponte#class10').
class('http://ml.unife.it/disponte#class11').
class('http://ml.unife.it/disponte#class5').
class('http://ml.unife.it/disponte#class6').
class('http://ml.unife.it/disponte#class7').
class('http://ml.unife.it/disponte#class8').
class('http://ml.unife.it/disponte#class9').
class('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class1').
class('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class2').
class('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class3').
class('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class4').
objectProperty('http://ml.unife.it/disponte#objprop10').
objectProperty('http://ml.unife.it/disponte#objprop4').
objectProperty('http://ml.unife.it/disponte#objprop5').
objectProperty('http://ml.unife.it/disponte#objprop6').
objectProperty('http://ml.unife.it/disponte#objprop7').
objectProperty('http://ml.unife.it/disponte#objprop8').
objectProperty('http://ml.unife.it/disponte#objprop9').
objectProperty('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1').
objectProperty('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop2').
objectProperty('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop3').
dataProperty('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#dataprop1').
dataProperty('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#dataprop2').
namedIndividual('http://ml.unife.it/disponte#ind1').
namedIndividual('http://ml.unife.it/disponte#ind2').
namedIndividual('http://ml.unife.it/disponte#ind3').
annotationProperty('http://ml.unife.it/disponte#probability').

 



inverseFunctionalProperty('http://ml.unife.it/disponte#objprop10').
asymmetricProperty('http://ml.unife.it/disponte#objprop10').
irreflexiveProperty('http://ml.unife.it/disponte#objprop10').

equivalentProperties(['http://ml.unife.it/disponte#objprop4', 'http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop2']).
inverseProperties('http://ml.unife.it/disponte#objprop4', 'http://ml.unife.it/disponte#objprop5').

propertyDomain('http://ml.unife.it/disponte#objprop5', 'http://ml.unife.it/disponte#class5').
propertyDomain('http://ml.unife.it/disponte#objprop5', 'http://ml.unife.it/disponte#class6').
propertyRange('http://ml.unife.it/disponte#objprop5', 'http://ml.unife.it/disponte#class9').
disjointProperties(['http://ml.unife.it/disponte#objprop5', 'http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1']).

functionalProperty('http://ml.unife.it/disponte#objprop9').
symmetricProperty('http://ml.unife.it/disponte#objprop9').
transitiveProperty('http://ml.unife.it/disponte#objprop9').
reflexiveProperty('http://ml.unife.it/disponte#objprop9').

subPropertyOf('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop3', 'http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1').



subClassOf('http://ml.unife.it/disponte#class10', allValuesFrom('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1', 'http://ml.unife.it/disponte#class9')).
subClassOf('http://ml.unife.it/disponte#class10', allValuesFrom('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop2', 'http://ml.unife.it/disponte#class9')).

subClassOf('http://ml.unife.it/disponte#class11', minCardinality(2, 'http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1', 'http://ml.unife.it/disponte#class9')).
subClassOf('http://ml.unife.it/disponte#class11', exactCardinality(3, 'http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop2', 'http://ml.unife.it/disponte#class9')).
subClassOf('http://ml.unife.it/disponte#class11', maxCardinality(4, 'http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1', 'http://ml.unife.it/disponte#class9')).

disjointClasses(['http://ml.unife.it/disponte#class7', unionOf(['http://ml.unife.it/disponte#class5', 'http://ml.unife.it/disponte#class6'])]).

%disjointUnion('http://ml.unife.it/disponte#class8', 'http://ml.unife.it/disponte#class5', 'http://ml.unife.it/disponte#class6']).

subClassOf('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class2', 'http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class1').
annotationAssertion('https://sites.google.com/a/unife.it/ml/disponte#probability', subClassOf('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class2', 'http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class1'), literal('0.7')).

equivalentClasses(['http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class4', intersectionOf(['http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class1', 'http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class3'])]).




classAssertion('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class1', 'http://ml.unife.it/disponte#ind1').
annotationAssertion('https://sites.google.com/a/unife.it/ml/disponte#probability', classAssertion('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class1', 'https://sites.google.com/a/unife.it/ml/disponte#ind1'), literal('0.3')).
classAssertion(someValuesFrom('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop2','http://ml.unife.it/disponte#class5'), 'http://ml.unife.it/disponte#ind1').
sameIndividual(['http://ml.unife.it/disponte#ind1', 'http://ml.unife.it/disponte#ind2']).
propertyAssertion('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1', 'http://ml.unife.it/disponte#ind1', 'http://ml.unife.it/disponte#ind2').
propertyAssertion('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#dataprop1', 'http://ml.unife.it/disponte#ind1', literal(type('http://www.w3.org/2001/XMLSchema#integer', '3'))).

differentIndividuals(['http://ml.unife.it/disponte#ind1', 'http://ml.unife.it/disponte#ind3']).
subPropertyOf(propertyChain(['http://ml.unife.it/disponte#objprop7', 'http://ml.unife.it/disponte#objprop8']), 'http://ml.unife.it/disponte#objprop6').


