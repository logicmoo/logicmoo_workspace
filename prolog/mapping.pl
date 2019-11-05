prolog2function(class(IRI), 'Class(IRI)').

prolog2function(datatype(IRI), 'Datatype(IRI)').

prolog2function(objectProperty(IRI), 'Objectproperty(IRI)').

prolog2function(dataPropery(IRI), 'Dataproperty(IRI)').

prolog2function(annotationProperty(IRI), 'AnnotationProperty(IRI)').

prolog2function(namedIndividual(IRI), 'NamedIndividual(IRI)').

prolog2function(anonymousIndividual(IRI), 'AnonymousIndividual(nodeID)'). %% ObjectPropertyAssertion ?

prolog2function(subClassOf(ClassExpression, ClassExpression), 'SubClassOf(axiomAnnotations,subClassExpression, superClassExpression)'):-
  classExpression2function(ClassExpressionTrill,ClassExpressionFunctional).

classExpression2function(CE,CEF):-
 iri(CE,CEF); objectIntersectionOf(CE,CEF);objectSomeValuesFrom(CE,CEF),!.

iri(IRI,IRI) :- atomic(IRI).
objectIntersectionOf(intersectionOf(CEs),ClassExpressionFL):-
  ClassExpressionF = 'ObjectIntersectionOf('
  findall(classExpression2function(CE,CEF),member(CE,CEs),L),
  % append stringhe in L su classExpressionF
  appnedClasses(ClassExpressionF,L,ClassExpressionFL).


objectSomeValuesFrom(someValuesFrom(P,C),SVFFunc):-
 classExpression2function(C,CF),
propertyExpression2funtion(P,PF),
appendClasses('ObjectSomeValuesFrom(',[PF,CF], SVFFunc).
               
prolog2function(equivalentClasses(ListaClassExpression), ECFunc):-  %'EquivalentClasses(axiomAnnotations, ClassExpression, ClassExpression { ClassExpression } )'):-
  findall(CEF,(member(CE,ListaClassExpression),classExpression2function(CE,CEF)),L),
  appendClasses('EquivalentClasses(',L,ECFunc).

prolog2function(disjointClasses(set(classExpression)), 'DisjointClasses(axiomAnnotations, ClassExpression, ClassExpression { ClassExpression })').

prolog2function(disjointUnion(IRI), 'DisjointUnion(axiomAnnotations, Class disjointClassExpressions)'
                disjointClassExpressions := ClassExpression ClassExpression { ClassExpression }).

prolog2function(subPropertyOf(propertyExpression, objectPropertyExpression),'SubObjectPropertyOf (axiomAnnotations, subObjectPropertyExpression, superObjectPropertyExpression )
                subObjectPropertyExpression := ObjectPropertyExpression | propertyExpressionChain
                propertyExpressionChain := 'ObjectPropertyChain' '(' ObjectPropertyExpression ObjectPropertyExpression { ObjectPropertyExpression } ')'
                superObjectPropertyExpression := ObjectPropertyExpression').
                
prolog2function(equivalentProperties(set(propertyExpression)), 'EquivalentObjectProperties(axiomAnnotations, ObjectPropertyExpression, ObjectPropertyExpression { ObjectPropertyExpression })').
                
prolog2function(dijointProperties(set(propertyExpression)), 'DisjointObjectProperties(axiomAnnotations, ObjectPropertyExpression, ObjectPropertyExpression { ObjectPropertyExpression })').
                
prolog2function(inverseProperties(objectPropertyExpression, objectPropertyExpression), 'InverseObjectProperties(axiomAnnotations, ObjectPropertyExpression, ObjectPropertyExpression)').
                
prolog2function(propertyDomain(propertyExpression, classExpression), 'ObjectPropertyDomain(axiomAnnotations, ObjectPropertyExpression, ClassExpression)').
                
prolog2function(propertyRange(propertyExpression, classExpression),'ObjectPropertyRange(axiomAnnotations, ObjectPropertyExpression, ClassExpression)').
                
prolog2function(functionalProperty(propertyExpression),'FunctionalObjectProperty(axiomAnnotations, ObjectPropertyExpression)').
                
prolog2function(inverseFunctionalProperty(objectPropertyExpression),'InverseFunctionalObjectProperty(axiomAnnotations, ObjectPropertyExpression').
                
prolog2function(reflexiveProperty(objectPropertyExpression),'ReflexiveObjectProperty(axiomAnnotations, ObjectPropertyExpression)').
                
prolog2function(irreflexiveProperty(objectPropertyExpression),'IrreflexiveObjectProperty(axiomAnnotations, ObjectPropertyExpression)').
                
prolog2function(symmetricProperty(objectPropertyExpression),'SymmetricObjectProperty(axiomAnnotations, ObjectPropertyExpression)').
                
prolog2function(asymmetricProperty(objectPropertyExpression),'AsymmetricObjectProperty(axiomAnnotations, ObjectPropertyExpression)').
                
prolog2function(transitiveProperty(objectPropertyExpression),'TransitiveObjectProperty(axiomAnnotations, ObjectPropertyExpression)').
                
prolog2function(hasKey(classExpression,propertyExpression),'HasKey(axiomAnnotations ClassExpression({ ObjectPropertyExpression }) ({ DataPropertyExpression }))').
                
prolog2function(sameIndividual(set(individual),'SameIndividual(axiomAnnotations, Individual Individual { Individual })').
                
prolog2function(differentIndividual(set(Individual),'DifferentIndividuals(axiomAnnotations, Individual Individual { Individual })').
                
prolog2function(classAssertion(classExpression, individual),'ClassAssertion(axiomAnnotations, ClassExpression Individual)').
                
prolog2function(propertyAssertion(propertyExpression, individual, individual),'ObjectPropertyAssertion( axiomAnnotations, ObjectPropertyExpression, sourceIndividual, targetIndividual)').
                
prolog2function(negativePropertyAssertion(propertyExpression, individual, individual),'NegativeObjectPropertyAssertion(axiomAnnotations, ObjectPropertyExpression, sourceIndividual, targetIndividual)').
                
prolog2function(annotationAssertion(annotationProperty, annotationSubject, annotationValue),'AnnotationAssertion(axiomAnnotations, AnnotationProperty, AnnotationSubject AnnotationValue)'
                AnnotationSubject := IRI | AnonymousIndividual ).
                
prolog2function(annotation(iri,annotationProperty,annotationValue),'Annotation(annotationAnnotations, AnnotationProperty, AnnotationValue)'
                annotationAnnotations  := { Annotation }
                AnnotationValue := AnonymousIndividual | IRI | Literal ).
                
prolog2function(ontology(IRI),'ontologyIRI(IRI)'). %?
                
prolog2function(ontologyAxiom(ontology, axiom),''). %?
                
prolog2function(ontologyImport(ontology, IRI),''). %?
                
prolog2function(ontologyVersionInfo(ontology, IRI),''). %?


                

