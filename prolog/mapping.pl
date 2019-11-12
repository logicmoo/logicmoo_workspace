prolog2function(class(IRI), ClFunc):- % qua IRI sarà una stringa che deve essere aggiunt via append a 'Class'
  appendFunctional('Class', [IRI], ClFunc). % potrebbe dare errore--> convertire IRI in stringa
  % NOTE usare predicato iri/2 che hai definito sotto. Eventualmente il controllo lo fai li

prolog2function(datatype(IRI), DtFunc):- %come sopra
  appendFunctional('Datatype', [IRI], DtFunc).

prolog2function(objectProperty(IRI), OpFunc) :-
  appendFunctional('Objectproperty', [IRI], OpFunc).

prolog2function(dataPropery(IRI), DPFunc):- 
  appendFunctional('Dataproperty',[IRI], DPFunc).

prolog2function(annotationProperty(IRI), APFunc ):- 
  appendFunctional('AnnotationProperty',[IRI], APFunc).

prolog2function(namedIndividual(IRI), NIFunc):- 
  appendFunctional('NamedIndividual',[IRI], NIFunc).

prolog2function(anonymousIndividual(IRI), AIFunc):- %'AnonymousIndividual(nodeID)'). 
  appendFunctional('AnonymousIndividual', [IRI], AIFunc).

prolog2function(subClassOf(ClassExpression1, ClassExpression2), SCFunc):- % per ora non consideriamo axiomAnnotation, appendFunctional delle due classExpression
  classExpression2function(ClassExpressionTrill1,ClassExpressionFunctional1),
  classExpression2function(ClassExpressionTrill2,ClassExpressionFunctional2), %appendFunctional SubClassOf ClassExpressionFunctional1 ClassExpressionFunctional2
  appendFunctional('SubClassOf',[ClassExpressionFunctional1, ClassExpressionFunctional2],SCFunc).

prolog2function(equivalentClasses(ListaClassExpression), ECFunc):-  %'EquivalentClasses(axiomAnnotations, ClassExpression, ClassExpression { ClassExpression } )'):-
  findall(CEF,(member(CE,ListaClassExpression),classExpression2function(CE,CEF)),L),
  appendFunctionalClasses('EquivalentClasses',L,ECFunc).

prolog2function(disjointClasses(ListaClassExpression), DCFunc):- %'DisjointClasses(axiomAnnotations, ClassExpression, ClassExpression { ClassExpression })'). % come equivalent
  findall(CEF,(member(CE,ListaClassExpression),classExpression2function(CE,CEF)),L),
  appendFunctionalClasses('DisjointClasses',L,DCFunc).

prolog2function(disjointUnion(IRI,ListaClassExpression), ECFunc):- %'DisjointUnion(axiomAnnotations, Class disjointClassExpressions)'% disjointClassExpressions := ClassExpression ClassExpression { ClassExpression }).% misto fra subClass e equivalentClasses
  classExpression2function(IRI,ClassExpressionFunctional),
  findall(CEF,(member(CE,ListaClassExpression),classExpression2function(CE,CEF)),L),
  appendFunctionalClasses('EquivalentClasses',[ClassExpressionFunctional|L],ECFunc).

prolog2function(subPropertyOf(PropertyExpression1, PropertyExpression2), SPFunc):- 
  propertyExpression2function(PropertyExpressionTrill1,PropertyExpressionFunctional1),
  propertyExpression2function(PropertyExpressionTrill2,PropertyExpressionFunctional2),
  appendFunctional('SubPropertyOf',[PropertyExpressionFunctional1, PropertyExpressionFunctional2],SPFunc).
     
prolog2function(equivalentProperties(ListaPropertyExpression), EPFunc):- %'EquivalentObjectProperties(axiomAnnotations, ObjectPropertyExpression, ObjectPropertyExpression { ObjectPropertyExpression })').
  findall(PEF,(member(PE,ListaPropertyExpression), propertyExpression2function(PE,PEF)),A),
  appendFunctionalProperty('EquivalentObjectProperties', A, EPFunc).
                
prolog2function(dijointProperties(ListaPropertyExpression), DPFunc):- %'DisjointObjectProperties(axiomAnnotations, ObjectPropertyExpression, ObjectPropertyExpression { ObjectPropertyExpression })'). % come disjoint classes ma su property
  findall(PEF,(member(PE, ListaPropertyExpression), propertyExpression2function(PE,PEF)),A),
  appendFunctionalProperty('DisjointObjectProperties', A, DPFunc).

prolog2function(inverseProperties(ObjectPropertyExpression1, ObjectPropertyExpression2), IOPFunc):- %'InverseObjectProperties(axiomAnnotations, ObjectPropertyExpression, ObjectPropertyExpression)'). % come subclass
  objectPropertyExpression2function(ObjectPropertyExpressionTrill1,ObjectPropertyExpressionFunctional1),
  objectPropertyExpression2function(ObjectPropertyExpressionTrill2,ObjectPropertyExpressionFunctional2), %appendFunctional SubClassOf ClassExpressionFunctional1 ClassExpressionFunctional2
  appendFunctional('InverseObjectProperties',[ObjectPropertyExpressionFunctional1, ObjectPropertyExpressionFunctional2],IOPFunc).

prolog2function(propertyDomain(PropertyExpression, ClassExpression), OPDFunc):- %'ObjectPropertyDomain(axiomAnnotations, ObjectPropertyExpression, ClassExpression)').
  propertyExpression2function(PropertyExpression,PropertyExpressionF),
  classExpression2function(ClassExpression,ClassExpressionF),
  appendFunctional('ObjectPropertyDomain',[PropertyExpressionF,ClassExpressionF],OPDFunc).

prolog2function(propertyRange(PropertyExpression, ClassExpression), OPRFunc) :- %'ObjectPropertyRange(axiomAnnotations, ObjectPropertyExpression, ClassExpression)').
  propertyExpression2function(PropertyExpression,PropertyExpressionF),
  classExpression2function(ClassExpression,ClassExpressionF),
  appendFunctional('ObjectPropertyRange',[PropertyExpressionF,ClassExpressionF],OPRFunc).

prolog2function(functionalProperty(PropertyExpression),FOPFunc) :- %'FunctionalObjectProperty(axiomAnnotations, ObjectPropertyExpression)'). %?
  appendFunctional('FunctionalObjectProperty',[PropertyExpression] ,FOPFunc).

prolog2function(inverseFunctionalProperty(PropertyExpression), IFPFunc):- %'InverseFunctionalObjectProperty(axiomAnnotations, ObjectPropertyExpression'). %? gestisco come quelle sopra
  appendFunctional('InverseFunctionalObjectProperty',[PropertyExpression] ,IFPFunc).

prolog2function(reflexiveProperty(PropertyExpression), RPFunc) :- % ReflexiveObjectProperty(axiomAnnotations, ObjectPropertyExpression)'). %? 
  appendFunctional('ReflexiveObjectProperty',[PropertyExpression] ,RPFunc).

prolog2function(irreflexiveProperty(PropertyExpression), IOPFunc):- %'IrreflexiveObjectProperty(axiomAnnotations, ObjectPropertyExpression)').  %? 
  appendFunctional('IrreflexiveObjectProperty', [PropertyExpression] ,IOPFunc).             

prolog2function(symmetricProperty(PropertyExpression), SOPFunc) :- %'SymmetricObjectProperty(axiomAnnotations, ObjectPropertyExpression)'). %?              
  appendFunctional('SymmetricObjectProperty', [PropertyExpression] ,SOPFunc).             

prolog2function(asymmetricProperty(PropertyExpression), AOPFunc):- %'AsymmetricObjectProperty(axiomAnnotations, ObjectPropertyExpression)').  %?             
  appendFunctional('AsymmetricObjectProperty', [PropertyExpression] ,AOPFunc).             

prolog2function(transitiveProperty(PropertyExpression), TOPFunc):- %'TransitiveObjectProperty(axiomAnnotations, ObjectPropertyExpression)').
  appendFunctional('TransitiveObjectProperty', [PropertyExpression] ,TOPFunc).             

prolog2function(hasKey(ClassExpression,PropertyExpression), HKFunc):- %'HasKey(axiomAnnotations ClassExpression({ ObjectPropertyExpression }) ({ DataPropertyExpression }))'). %? come proprerty domain ma al contrario
  classExpression2function(ClassExpression,ClassExpressionF),
  propertyExpression2function(PropertyExpression,PropertyExpressionF),
  appendFunctional('HasKey',[ClassExpressionF,PropertyExpressionF],HKFunc).

prolog2function(sameIndividual(ListIndividual), SIFunc) :- %'SameIndividual(axiomAnnotations, Individual Individual { Individual })').
  findall(IEF,(member(IE, ListIndividual), individual2function(IE,IEF)),A),
  appendFunctionalIndivudual('SameIndividual', A, SIFunc).
                
prolog2function(differentIndividual(ListIndividual), DIFunc ) :- %'DifferentIndividuals(axiomAnnotations, Individual Individual { Individual })').
  findall(IEF,(member(IE, ListIndividual), individual2function(IE,IEF)),A),
  appendFunctional('DifferentIndividual', A, DIFunc).
                
prolog2function(classAssertion(ClassExpression, Individual), CAFunc) :- %'ClassAssertion(axiomAnnotations, ClassExpression Individual)').
  classExpression2function(ClassExpression,ClassExpressionF),
  individual2function(IndividualExpression,IndividualExpressionF),
  appendFunctional('ClassAssertion',[ClassExpressionF,IndividualExpressionF],CAFunc).
                
prolog2function(propertyAssertion(PropertyExpression, Individual, Individual), OPAFunc ):- %'ObjectPropertyAssertion( axiomAnnotations, ObjectPropertyExpression, sourceIndividual, targetIndividual)'). %? 1property2expression 2individual2function
  propertyExpression2function(PropertyExpression,PropertyExpressionF),
  individual2function(IndividualExpression1, IndividualExpression1F),
  individual2function(IndividualExpression2, IndividualExpression2F),
  appendFunctional('ObjectPropertyAssertion', [PropertyExpressionF, IndividualExpression1F, IndividualExpression2F], OPAFunc).

prolog2function(negativePropertyAssertion(PropertyExpression, Individual, Individual), NOPAFunc ):- %'NegativeObjectPropertyAssertion(axiomAnnotations, ObjectPropertyExpression, sourceIndividual, targetIndividual)'). %?
  propertyExpression2function(PropertyExpression,PropertyExpressionF),
  individual2function(IndividualExpression1, IndividualExpression1F),
  individual2function(IndividualExpression2, IndividualExpression2F),
  appendFunctional('ObjectPropertyAssertion', [PropertyExpressionF, IndividualExpression1F, IndividualExpression2F], NOPAFunc).

% DA GUARDARE!!
prolog2function(annotationAssertion(AnnotationProperty, AnnotationSubject, AnnotationValue),AAFunc):- %'AnnotationAssertion(axiomAnnotations, AnnotationProperty, AnnotationSubject AnnotationValue)' %?
  propertyExpression2function(AnnotationSubject, AnnotationSubjectF),
  appendFunctional('AnnotationAssertion', [IRI,AnnotationSubject,IRI], AAFunc). 
                %AnnotationSubject := IRI | literal(IRI) ).%? annotationProperty: IRI, annotationSubject: lo considero un property2function, annotationValue: può essere un IRI e un literal (literal2function)'""'

% DA GUARDARE!!                
prolog2function(annotation(iri,annotationProperty,annotationValue),'Annotation(annotationAnnotations, AnnotationProperty, AnnotationValue)'
                annotationAnnotations  := { Annotation }
                AnnotationValue := AnonymousIndividual | IRI | Literal ). %?COME SOPRA MA è DA CAPIRE!
                
prolog2function(ontology(IRI), OIFunc ) :- 
  appendFunctional(ontologyIRI, [IRI], OIFunc).

%? "spigeazione-> le possiamo trasformare in annotation"
prolog2function(ontologyAxiom(ontology, axiom),''). 

prolog2function(ontologyImport(ontology(IRI)), OIMFunc):- 
  appendFunctional(ontologyImport, [IRI], OIMFunc).

%? La gestiamo alla fine
prolog2function(ontologyVersionInfo(ontology, IRI),''). 

classExpression2function(CE,CEF):- 
  iri(CE,CEF); 
  objectIntersectionOf(CE,CEF);
  objectSomeValuesFrom(CE,CEF); 
  objectUnionOf(CE, CEF);
  objectComplementOf(CE,CEF); %Da Controllare
  objectOneOf(CE,CEF);%Da Controllare
  objectAllValuesFrom(CE,CEF); 
  objectHasValue(CE,CEF); 
  objectHasSelf(CE,CEF) ;
  objectMinCardinality(CE,CEF); %Da fare
  objectMaxCardinality(CE,CEF); %Da fare
  objectExactCardinality(CE,CEF); %Da fare
  dataSomeValuesFrom(CE,CEF); %Da fare
  dataAllValuesFrom(CE,CEF); %Da fare
  dataHasValue(CE,CEF);%Da fare
  dataMinCardinality(CE,CEF); %Da fare
  dataMaxCardinality(CE,CEF); %Da fare
  dataExactCardinality(CE,CEF), %Da fare
  !.

/*
ObjectIntersectionOf := 'ObjectIntersectionOf' '(' ClassExpression ClassExpression { ClassExpression } ')'
ObjectUnionOf := 'ObjectUnionOf' '(' ClassExpression ClassExpression { ClassExpression } ')'
ObjectComplementOf := 'ObjectComplementOf' '(' ClassExpression ')'
ObjectOneOf := 'ObjectOneOf' '(' Individual { Individual }')'
ObjectSomeValuesFrom := 'ObjectSomeValuesFrom' '(' ObjectPropertyExpression ClassExpression ')'
ObjectAllValuesFrom := 'ObjectAllValuesFrom' '(' ObjectPropertyExpression ClassExpression ')'
ObjectHasValue := 'ObjectHasValue' '(' ObjectPropertyExpression Individual ')'
ObjectHasSelf := 'ObjectHasSelf' '(' ObjectPropertyExpression ')'
ObjectMinCardinality := 'ObjectMinCardinality' '(' nonNegativeInteger ObjectPropertyExpression [ ClassExpression ] ')'
ObjectMaxCardinality := 'ObjectMaxCardinality' '(' nonNegativeInteger ObjectPropertyExpression [ ClassExpression ] ')'
ObjectExactCardinality := 'ObjectExactCardinality' '(' nonNegativeInteger ObjectPropertyExpression [ ClassExpression ] ')'
DataSomeValuesFrom := 'DataSomeValuesFrom' '(' DataPropertyExpression { DataPropertyExpression } DataRange ')'
DataAllValuesFrom := 'DataAllValuesFrom' '(' DataPropertyExpression { DataPropertyExpression } DataRange ')'
DataHasValue := 'DataHasValue' '(' DataPropertyExpression Literal ')'
DataMinCardinality := 'DataMinCardinality' '(' nonNegativeInteger DataPropertyExpression [ DataRange ] ')'
DataMaxCardinality := 'DataMaxCardinality' '(' nonNegativeInteger DataPropertyExpression [ DataRange ] ')'
DataExactCardinality := 'DataExactCardinality' '(' nonNegativeInteger DataPropertyExpression [ DataRange ] ')'
*/



propertyExpression2function(PE, PEF):-
  iri(PE,PEF); 
 
iri(IRI,IRI) :- atomic(IRI).

objectIntersectionOf(intersectionOf(CEs),ClassExpressionFL):-
   ClassExpressionF = 'ObjectIntersectionOf',
   findall(CEF,(member(CE,ListaClassExpression),classExpression2function(CE,CEF)),L),% appendFunctional stringhe in L su classExpressionF
   appendFunctional(ClassExpressionF,L,ClassExpressionFL).
 
objectSomeValuesFrom(someValuesFrom(P,C),SVFFunc):-
  classExpression2function(C,CF),
  propertyExpression2funtion(P,PF),
  appendFunctional('ObjectSomeValuesFrom',[CF,PF], SVFFunc).

objectUnionOf(unionOf(CEs),ClassExpressionFL):-
  ClassExpressionF = 'ObjectUnionOf',
  findall(CEF,(member(CE,ListaClassExpression),classExpression2function(CE,CEF)),L),% appendFunctional stringhe in L su classExpressionF
  appendFunctional(ClassExpressionF,L,ClassExpressionFL).

%? DA RIGUARDARE! --> NOTE mi sembra aposto
objectComplementOf(complementOf(CE, CEF)):-
  appendFunctional('ObjectComplementOf', CE, CEF). 

%? DA RIGUARDARE!
objectOneOf(oneOf(CE, CEF)) :-
  findall(member(CE,CEF), L), % NOTE findall da sistemare, prende tre argomenti
    appendFunctional('ObjectUnionOf', CE, L). 

objectAllValuesFrom(allValueFrom(P, C), AVFFunc):-
  classExpression2function(C, CF),
  propertyExpression2function(P, PF),
  appendFunctional('ObjectAllValuesFrom',[PF,CF], AVFFunc).

objectHasValue(hasValue(P,I), HVFunc):-
  propertyExpression2function(P, PF),
  individual2function(I, IF),
  appendFunctional('ObjectHasValue', [PF, I], HVFunc).

objectHasValue(hasSelf(P), HVFunc):-
  propertyExpression2function(P, PF),
  appendFunctional('ObjectHasValue', PF, HVFunc).

objectMinCardinality(minCardinality(C, P), OMCFunc):-
  %number(C),
  %C>=0,
  %x: xsd:minExclusive , v
  % CONTROLLARE COME SI GESTISCONO GLI INTERI ?!?!?!?!?
  %xsd:nonNegativeInteger,
  %C: (minExclusive, 0),
  propertyExpression2function(P, PF),
  classExpression2function(C, CF),
  appendFunctional('ObjectMinCardinality',[PF,CF], OMCFunc).




appendFunctional(Pred, Lista, Ris):-
  atomic_list_concat([Pred,'('|Lista], Atom), 
  atomic_concat(Atom, ')', Ris).   
  