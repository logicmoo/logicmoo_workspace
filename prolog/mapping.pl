prolog2function(class(ListIRI), ClFunc):-
  % qua IRI sarà una stringa che deve essere aggiunt via append a 'Class'
  append('Class', ListIRI, ClFunc). 

prolog2function(datatype(IRI), 'Datatype(IRI)').
  %come sopra

prolog2function(objectProperty(IRI), 'Objectproperty(IRI)').
  %come sopra
prolog2function(dataPropery(IRI), 'Dataproperty(IRI)').
  %come sopra
prolog2function(annotationProperty(IRI), 'AnnotationProperty(IRI)').
  %come sopra
prolog2function(namedIndividual(IRI), 'NamedIndividual(IRI)').
  %come sopra
prolog2function(anonymousIndividual(IRI), 'AnonymousIndividual(nodeID)'). %% ObjectPropertyAssertion ?
  %come sopra

prolog2function(subClassOf(ClassExpression1, ClassExpression2), SCFunc):- % per ora non consideriamo axiomAnnotation, append delle due classExpression
  classExpression2function(ClassExpressionTrill1,ClassExpressionFunctional1),
  classExpression2function(ClassExpressionTrill2,ClassExpressionFunctional2), %append SubClassOf ClassExpressionFunctional1 ClassExpressionFunctional2
  append('SubClassOf',[ClassExpressionFunctional1, ClassExpressionFunctional2],SCFunc).

prolog2function(equivalentClasses(ListaClassExpression), ECFunc):-  %'EquivalentClasses(axiomAnnotations, ClassExpression, ClassExpression { ClassExpression } )'):-
  findall(CEF,(member(CE,ListaClassExpression),classExpression2function(CE,CEF)),L),
  appendClasses('EquivalentClasses(',L,ECFunc).

prolog2function(disjointClasses(ListaClassExpression), DCFunc):- %'DisjointClasses(axiomAnnotations, ClassExpression, ClassExpression { ClassExpression })').
% come equivalent
  findall(CEF,(member(CE,ListaClassExpression),classExpression2function(CE,CEF)),L),
  appendClasses('DisjointClasses(',L,DCFunc).

prolog2function(disjointUnion(IRI,ListaClassExpression), ECFunc):- %'DisjointUnion(axiomAnnotations, Class disjointClassExpressions)'
 % disjointClassExpressions := ClassExpression ClassExpression { ClassExpression }).
  % misto fra subClass e equivalentClasses
  classExpression2function(IRI,ClassExpressionFunctional),
  findall(CEF,(member(CE,ListaClassExpression),classExpression2function(CE,CEF)),L),
  appendClasses('EquivalentClasses',[ClassExpressionFunctional|L],ECFunc).

prolog2function(subPropertyOf(PropertyExpression1, PropertyExpression2), SPFunc):- 
  propertyExpression2function(PropertyExpressionTrill1,PropertyExpressionFunctional1),
  propertyExpression2function(PropertyExpressionTrill2,PropertyExpressionFunctional2),
  append('SubPropertyOf',[PropertyExpressionFunctional1, PropertyExpressionFunctional2],SPFunc).
     
prolog2function(equivalentProperties(ListaPropertyExpression)), EPFunc):- %'EquivalentObjectProperties(axiomAnnotations, ObjectPropertyExpression, ObjectPropertyExpression { ObjectPropertyExpression })').
  findall(PEF,(menber(,PE,ListaPropertyExpression), propertyExpression2function(PE,PEF)),A),
  appendProperty('EquivalentObjectProperties(', A, EPFunc).
                
prolog2function(dijointProperties(ListaPropertyExpression)), DPFunc):- %'DisjointObjectProperties(axiomAnnotations, ObjectPropertyExpression, ObjectPropertyExpression { ObjectPropertyExpression })').
  % come disjoint classes ma su property
  findall(PEF,(member(PE, ListaPropertyExpression), propertyExpression2function(PE,PEF)),A),
  appendProperty('DisjointObjectProperties(', A, DPFunc).

prolog2function(inverseProperties(ObjectPropertyExpression1, ObjectPropertyExpression2), IOPFunc):- %'InverseObjectProperties(axiomAnnotations, ObjectPropertyExpression, ObjectPropertyExpression)').
  % come subclass
  ObjectPropertyExpression2function(ObjectPropertyExpressionTrill1,ObjectPropertyExpressionFunctional1),
  ObjectPropertyExpression2function(ObjectPropertyExpressionTrill2,ObjectPropertyExpressionFunctional2), %append SubClassOf ClassExpressionFunctional1 ClassExpressionFunctional2
  append('InverseObjectProperties',[ObjectPropertyExpressionFunctional1, ObjectPropertyExpressionFunctional2],IOPFunc).

prolog2function(propertyDomain(PropertyExpression, ClassExpression), OPDFunc):- %'ObjectPropertyDomain(axiomAnnotations, ObjectPropertyExpression, ClassExpression)').
  propertyExpression2function(PropertyExpression,PropertyExpressionF),
  classExpression2function(ClassExpression,ClassExpressionF),
  append('ObjectPropertyDomain',[PropertyExpressionF,ClassExpressionF],OPDFunc).

prolog2function(propertyRange(PropertyExpression, ClassExpression), OPRFunc) :- %'ObjectPropertyRange(axiomAnnotations, ObjectPropertyExpression, ClassExpression)').
  propertyExpression2function(PropertyExpression,PropertyExpressionF),
  classExpression2function(ClassExpression,ClassExpressionF),
  append('ObjectPropertyRange',[PropertyExpressionF,ClassExpressionF],OPRFunc).

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

classExpression2function(CE,CEF):- 
  iri(CE,CEF); objectIntersectionOf(CE,CEF);objectSomeValuesFrom(CE,CEF),!.

propertyExpression2function(PE, PEF):-
  iri(PE,PEF);
 
 iri(IRI,IRI) :- atomic(IRI).
 objectIntersectionOf(intersectionOf(CEs),ClassExpressionFL):-
   ClassExpressionF = 'ObjectIntersectionOf(',
   findall(CEF,(member(CE,ListaClassExpression),classExpression2function(CE,CEF)),L),
   % append stringhe in L su classExpressionF
   appendClasses(ClassExpressionF,L,ClassExpressionFL).
 
 objectSomeValuesFrom(someValuesFrom(P,C),SVFFunc):-
  classExpression2function(C,CF),
  propertyExpression2funtion(P,PF),
  appendClasses('ObjectSomeValuesFrom(',[PF,CF], SVFFunc).

appendFunctional(Pred, Lista, Ris):-
  % 1 append fra Pred e (
  % 2 append risultato 1 con tutti gli elementi della lista nell'ordine
  % 3 append del ris di 2 e ) => Ris