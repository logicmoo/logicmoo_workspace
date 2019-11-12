equivalentClasses(['http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class4', intersectionOf(['http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class1', 'http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class3'])]).
disjointClasses(['http://ml.unife.it/disponte#class7', unionOf(['http://ml.unife.it/disponte#class5', 'http://ml.unife.it/disponte#class6'])]).
subClassOf('http://ml.unife.it/disponte#class10', allValuesFrom('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1', 'http://ml.unife.it/disponte#class9')).
subClassOf('http://ml.unife.it/disponte#class10', allValuesFrom('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop2', 'http://ml.unife.it/disponte#class9')).
subClassOf('http://ml.unife.it/disponte#class11', minCardinality(2, 'http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1', 'http://ml.unife.it/disponte#class9')).
subClassOf('http://ml.unife.it/disponte#class11', exactCardinality(3, 'http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop2', 'http://ml.unife.it/disponte#class9')).
subClassOf('http://ml.unife.it/disponte#class11', maxCardinality(4, 'http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1', 'http://ml.unife.it/disponte#class9')).
subClassOf('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class2', 'http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class1').
disjointUnion('http://ml.unife.it/disponte#class8', ['http://ml.unife.it/disponte#class5', 'http://ml.unife.it/disponte#class6']).
symmetricProperty('http://ml.unife.it/disponte#objprop9').
inverseFunctionalProperty('http://ml.unife.it/disponte#objprop10').
transitiveProperty('http://ml.unife.it/disponte#objprop9').
asymmetricProperty('http://ml.unife.it/disponte#objprop10').
subPropertyOf(propertyChain(['http://ml.unife.it/disponte#objprop7', 'http://ml.unife.it/disponte#objprop8']), 'http://ml.unife.it/disponte#objprop6').
subPropertyOf('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop3', 'http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1').
functionalProperty('http://ml.unife.it/disponte#objprop9').
irreflexiveProperty('http://ml.unife.it/disponte#objprop10').
disjointProperties(['http://ml.unife.it/disponte#objprop5', 'http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1']).
propertyDomain('http://ml.unife.it/disponte#objprop5', 'http://ml.unife.it/disponte#class5').
propertyDomain('http://ml.unife.it/disponte#objprop5', 'http://ml.unife.it/disponte#class6').
reflexiveProperty('http://ml.unife.it/disponte#objprop9').
propertyRange('http://ml.unife.it/disponte#objprop5', 'http://ml.unife.it/disponte#class9').
equivalentProperties(['http://ml.unife.it/disponte#objprop4', 'http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop2']).
inverseProperties('http://ml.unife.it/disponte#objprop4', 'http://ml.unife.it/disponte#objprop5').
differentIndividuals(['http://ml.unife.it/disponte#ind1', 'http://ml.unife.it/disponte#ind3']).
propertyAssertion('http://www.w3.org/2002/07/owl#onProperty', '_:Description9', 'http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop2').
propertyAssertion('http://www.w3.org/2002/07/owl#someValuesFrom', '_:Description9', 'http://ml.unife.it/disponte#class5').
propertyAssertion('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1', 'http://ml.unife.it/disponte#ind1', 'http://ml.unife.it/disponte#ind2').
propertyAssertion('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#dataprop1', 'http://ml.unife.it/disponte#ind1', literal(type('http://www.w3.org/2001/XMLSchema#integer', '3'))).
sameIndividual(['http://ml.unife.it/disponte#ind1', 'http://ml.unife.it/disponte#ind2']).
classAssertion('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class1', 'http://ml.unife.it/disponte#ind1').
classAssertion('http://www.w3.org/2002/07/owl#Restriction', '_:Description9').
namedIndividual('http://ml.unife.it/disponte#ind1').
namedIndividual('http://ml.unife.it/disponte#ind2').
namedIndividual('http://ml.unife.it/disponte#ind3').
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
annotationProperty('http://ml.unife.it/disponte#probability').
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
ontology('http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10').
 
