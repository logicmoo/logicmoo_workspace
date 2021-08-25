/*

QUERY 1:
instanceOf('petowner','Kevin',LE).
WITH: sameIndividual(['Fuffy','Tom'])

EXPL:
LE = [(subClassOf(someValuesFrom(has_animal,pet),petowner),'Kevin'),sameIndividuals(['Fuffy','Tom']),propertyAssertion(has_animal,'Kevin','Tom'),(subClassOf(cat,pet),sameIndividuals(['Fuffy','Tom'])),classAssertion(cat,'Fuffy')] ? ;

LE = [(subClassOf(someValuesFrom(has_animal,pet),petowner),'Kevin'),sameIndividuals(['Fuffy','Tom']),propertyAssertion(has_animal,'Kevin','Tom'),(subClassOf(cat,pet),sameIndividuals(['Fuffy','Tom'])),classAssertion(cat,'Tom')] ? ;

LE = [(subClassOf(someValuesFrom(has_animal,pet),petowner),'Kevin'),sameIndividuals(['Fuffy','Tom']),propertyAssertion(has_animal,'Kevin','Fuffy'),(subClassOf(cat,pet),sameIndividuals(['Fuffy','Tom'])),classAssertion(cat,'Fuffy')] ? ;

LE = [(subClassOf(someValuesFrom(has_animal,pet),petowner),'Kevin'),sameIndividuals(['Fuffy','Tom']),propertyAssertion(has_animal,'Kevin','Fuffy'),(subClassOf(cat,pet),sameIndividuals(['Fuffy','Tom'])),classAssertion(cat,'Tom')] ?

QUERY 2:
instanceOf('petowner','Kev',LE).
WITH sameIndividual('Kevin','Kev').

EXPL:
LE = [(subClassOf(someValuesFrom(has_animal,pet),petowner),sameIndividuals(['Kevin','Kev'])),sameIndividuals(['Kevin','Kev']),propertyAssertion(has_animal,'Kevin','Tom'),(subClassOf(cat,pet),'Tom'),classAssertion(cat,'Tom')] ? ;

LE = [(subClassOf(someValuesFrom(has_animal,pet),petowner),sameIndividuals(['Kevin','Kev'])),sameIndividuals(['Kevin','Kev']),propertyAssertion(has_animal,'Kevin','Fuffy'),(subClassOf(cat,pet),'Fuffy'),classAssertion(cat,'Fuffy')] ? 

WITH: propertyAssertion('has_animal','Kev','Fuffy')

EXPL:
LE = [(subClassOf(someValuesFrom(has_animal,pet),petowner),sameIndividuals(['Kevin','Kev'])),sameIndividuals(['Kevin','Kev']),propertyAssertion(has_animal,'Kevin','Tom'),(subClassOf(cat,pet),'Tom'),classAssertion(cat,'Tom')] ? ;

LE = [(subClassOf(someValuesFrom(has_animal,pet),petowner),sameIndividuals(['Kevin','Kev'])),sameIndividuals(['Kevin','Kev']),propertyAssertion(has_animal,'Kev','Fuffy'),(subClassOf(cat,pet),'Fuffy'),classAssertion(cat,'Fuffy')] ?

WITH: ('kevin','fuffy'),('kev','fuffy') both

EXPL:
LE = [(subClassOf(someValuesFrom(has_animal,pet),petowner),sameIndividuals(['Kevin','Kev'])),sameIndividuals(['Kevin','Kev']),propertyAssertion(has_animal,'Kevin','Tom'),(subClassOf(cat,pet),'Tom'),classAssertion(cat,'Tom')] ? ;

LE = [(subClassOf(someValuesFrom(has_animal,pet),petowner),sameIndividuals(['Kevin','Kev'])),sameIndividuals(['Kevin','Kev']),propertyAssertion(has_animal,'Kevin','Fuffy'),(subClassOf(cat,pet),'Fuffy'),classAssertion(cat,'Fuffy')] ? ;

LE = [(subClassOf(someValuesFrom(has_animal,pet),petowner),sameIndividuals(['Kevin','Kev'])),sameIndividuals(['Kevin','Kev']),propertyAssertion(has_animal,'Kev','Fuffy'),(subClassOf(cat,pet),'Fuffy'),classAssertion(cat,'Fuffy')] ? 

*/


subClassOf(someValuesFrom('has_animal','pet'),'petowner').
subClassOf('cat','pet').

propertyAssertion('has_animal','Kevin','Tom').
propertyAssertion('has_animal','Kevin','Fuffy').
propertyAssertion('has_animal','Kev','Fuffy').
classAssertion('cat','Tom').
classAssertion('cat','Fuffy').

%sameIndividuals(['Fuffy','Tom']).
sameIndividuals(['Kevin','Kev']).


