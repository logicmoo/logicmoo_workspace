program :-
  open('file.txt',write, Stream),
  (   man(Man), write(Stream, Man), fail
  ;   true
  ),
  close(Stream).
prolog2function(class(IRI), ClFunc):- 
  iri(IRI,IRIF),
  appendFunctional('Class', [IRIF], ClFunc). 

prolog2function(datatype(IRI), DtFunc):- 
  iri(IRI,IRIF),
  appendFunctional('Datatype', [IRIF], DtFunc).

prolog2function(objectProperty(IRI), OpFunc) :-
  iri(IRI,IRIF),
  appendFunctional('Objectproperty', [IRIF], OpFunc).

prolog2function(dataPropery(IRI), DPFunc):- 
  iri(IRI,IRIF),
  appendFunctional('Dataproperty', [IRIF], DPFunc).

prolog2function(annotationProperty(IRI), APFunc ):- 
  iri(IRI,IRIF),
  appendFunctional('AnnotationProperty', [IRIF], APFunc).

prolog2function(namedIndividual(IRI), NIFunc):- 
  iri(IRI,IRIF),
  appendFunctional('NamedIndividual', [IRIF], NIFunc).

prolog2function(anonymousIndividual(IRI), AIFunc):- 
  iri(IRI,IRIF),
  appendFunctional('AnonymousIndividual', [IRIF], AIFunc).

prolog2function(subClassOf(ClassExpression1, ClassExpression2), SCFunc):- % per ora non consideriamo axiomAnnotation, appendFunctional delle due classExpression
  classExpression2function(ClassExpression1,ClassExpressionFunctional1),
  classExpression2function(ClassExpression2,ClassExpressionFunctional2), %appendFunctional SubClassOf ClassExpressionFunctional1 ClassExpressionFunctional2
  appendFunctional('SubClassOf',[ClassExpressionFunctional1, ClassExpressionFunctional2],SCFunc).

prolog2function(equivalentClasses(ListaClassExpression), ECFunc):-  %'EquivalentClasses(axiomAnnotations, ClassExpression, ClassExpression { ClassExpression } )'):-
  findall(CEF,(member(CE,ListaClassExpression),classExpression2function(CE,CEF)),L),
  appendFunctional('EquivalentClasses',L,ECFunc).

prolog2function(disjointClasses(ListaClassExpression), DCFunc):- %'DisjointClasses(axiomAnnotations, ClassExpression, ClassExpression { ClassExpression })'). % come equivalent
  findall(CEF,(member(CE,ListaClassExpression),classExpression2function(CE,CEF)),L),
  appendFunctional('DisjointClasses',L,DCFunc).

prolog2function(disjointUnion(IRI,ListaClassExpression), ECFunc):- %'DisjointUnion(axiomAnnotations, Class disjointClassExpressions)'% disjointClassExpressions := ClassExpression ClassExpression { ClassExpression }).% misto fra subClass e equivalentClasses
  classExpression2function(IRI,ClassExpressionFunctional),
  findall(CEF,(member(CE,ListaClassExpression),classExpression2function(CE,CEF)),L),
  appendFunctional('DisjointUnion',[ClassExpressionFunctional|L],ECFunc).

prolog2function(subPropertyOf(PropertyExpression1, PropertyExpression2), SPFunc):- 
  propertyExpression2function(PropertyExpression1,PropertyExpressionFunctional1),
  propertyExpression2function(PropertyExpression2,PropertyExpressionFunctional2),
  appendFunctional('SubPropertyOf',[PropertyExpressionFunctional1, PropertyExpressionFunctional2],SPFunc).
     
prolog2function(equivalentProperties(ListaPropertyExpression), EPFunc):- %'EquivalentObjectProperties(axiomAnnotations, ObjectPropertyExpression, ObjectPropertyExpression { ObjectPropertyExpression })').
  findall(PEF,(member(PE,ListaPropertyExpression), propertyExpression2function(PE,PEF)),A),
  appendFunctional('EquivalentObjectProperties', A, EPFunc).
                
prolog2function(dijointProperties(ListaPropertyExpression), DPFunc):- %'DisjointObjectProperties(axiomAnnotations, ObjectPropertyExpression, ObjectPropertyExpression { ObjectPropertyExpression })'). % come disjoint classes ma su property
  findall(PEF,(member(PE, ListaPropertyExpression), propertyExpression2function(PE,PEF)),A),
  appendFunctional('DisjointObjectProperties', A, DPFunc).

prolog2function(inverseProperties(ObjectPropertyExpression1, ObjectPropertyExpression2), IOPFunc):- %'InverseObjectProperties(axiomAnnotations, ObjectPropertyExpression, ObjectPropertyExpression)'). % come subclass
  propertyExpression2function(ObjectPropertyExpression1,ObjectPropertyExpressionFunctional1),
  propertyExpression2function(ObjectPropertyExpression2,ObjectPropertyExpressionFunctional2), %appendFunctional SubClassOf ClassExpressionFunctional1 ClassExpressionFunctional2
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
  appendFunctional('SameIndividual', A, SIFunc).
                
prolog2function(differentIndividual(ListIndividual), DIFunc ) :- %'DifferentIndividuals(axiomAnnotations, Individual Individual { Individual })').
  findall(IEF,(member(IE, ListIndividual), individual2function(IE,IEF)),A),
  appendFunctional('DifferentIndividual', A, DIFunc).
                
prolog2function(classAssertion(ClassExpression, IndividualExpression), CAFunc) :- %'ClassAssertion(axiomAnnotations, ClassExpression Individual)').
  classExpression2function(ClassExpression,ClassExpressionF),
  individual2function(IndividualExpression,IndividualExpressionF),
  appendFunctional('ClassAssertion',[ClassExpressionF,IndividualExpressionF],CAFunc).
                
prolog2function(propertyAssertion(PropertyExpression, IndividualExpression1, IndividualExpression2), OPAFunc ):- %'ObjectPropertyAssertion( axiomAnnotations, ObjectPropertyExpression, sourceIndividual, targetIndividual)'). %? 1property2expression 2individual2function
  propertyExpression2function(PropertyExpression,PropertyExpressionF),
  individual2function(IndividualExpression1, IndividualExpression1F),
  individual2function(IndividualExpression2, IndividualExpression2F),
  appendFunctional('ObjectPropertyAssertion', [PropertyExpressionF, IndividualExpression1F, IndividualExpression2F], OPAFunc).

prolog2function(negativePropertyAssertion(PropertyExpression, IndividualExpression1, IndividualExpression2), NOPAFunc ):- %'NegativeObjectPropertyAssertion(axiomAnnotations, ObjectPropertyExpression, sourceIndividual, targetIndividual)'). %?
  propertyExpression2function(PropertyExpression,PropertyExpressionF),
  individual2function(IndividualExpression1, IndividualExpression1F),
  individual2function(IndividualExpression2, IndividualExpression2F),
  appendFunctional('ObjectPropertyAssertion', [PropertyExpressionF, IndividualExpression1F, IndividualExpression2F], NOPAFunc).

% DA GUARDARE!!
prolog2function(annotationAssertion(AnnotationProperty, AnnotationSubject, AnnotationValue),AAFunc):- %'AnnotationAssertion(axiomAnnotations, AnnotationProperty, AnnotationSubject AnnotationValue)' %?
  propertyExpression2function(AnnotationProperty, AnnotationPropertyF),
  propertyExpression2function(AnnotationSubject, AnnotationSubjectF),
  (
        % condition 
        iri(AnnotationValue,AnnotationValueF)
    ->
        % true 
        appendFunctional('AnnotationAssertion', [AnnotationPropertyF,AnnotationSubjectF,AnnotationValueF], AAFunc)
    ;
        % false 
        (literal2function(AnnotationValue, AnnotationValueF),
        appendFunctional('AnnotationAssertion', [AnnotationPropertyF,AnnotationSubjectF,AnnotationValueF], AAFunc))
  ).
              
prolog2function(annotation(AnnotationProperty, AnnotationProperty, AnnotationValue), AFunc):-%(iri,annotationProperty,annotationValue),'Annotation(annotationAnnotations, AnnotationProperty, AnnotationValue)'
  propertyExpression2function(AnnotationProperty, AnnotationPropertyF),
  (
        % condition 
        iri(AnnotationValue,AnnotationValueF)
    ->
        % true 
        appendFunctional('AnnotationAssertion', [AnnotationPropertyF,AnnotationPropertyF,AnnotationValueF], AFunc)
    ;
        % false 
        literal2function(AnnotationValue, AnnotationValueF),
        appendFunctional('AnnotationAssertion', [AnnotationPropertyF,AnnotationPropertyF,AnnotationValueF], AFunc)
  ).


prolog2function(ontology(IRI), OIFunc) :- 
  appendFunctional(ontologyIRI, [IRI], OIFunc).

%? "spigeazione-> le possiamo trasformare in annotation"
%prolog2function(ontologyAxiom(ontology, axiom),''). 

prolog2function(ontologyImport(ontology(IRI)), OIMFunc):- 
  appendFunctional(ontologyImport, [IRI], OIMFunc).

%? La gestiamo alla fine
%prolog2function(ontologyVersionInfo(ontology, IRI),''). 

classExpression2function(CE,CEF):- 
  iri(CE,CEF); 
  objectIntersectionOf(CE,CEF);
  objectSomeValuesFrom(CE,CEF); 
  objectUnionOf(CE, CEF);
  objectComplementOf(CE,CEF); 
  objectOneOf(CE,CEF);
  objectAllValuesFrom(CE,CEF); 
  objectHasValue(CE,CEF); 
  objectHasSelf(CE,CEF) ;
  objectMinCardinality(CE,CEF); 
  objectMaxCardinality(CE,CEF); 
  objectExactCardinality(CE,CEF); 
  dataSomeValuesFrom(CE,CEF);
  dataAllValuesFrom(CE,CEF); 
  dataHasValue(CE,CEF);
  dataMinCardinality(CE,CEF); 
  dataMaxCardinality(CE,CEF); 
  dataExactCardinality(CE,CEF), 
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

individual2function(PE, PEF):-
  iri(PE,PEF).

propertyExpression2function(PE, PEF):-
  iri(PE,PEF).
 
iri(IRI,IRIF) :- 
  atomic(IRI),
  atomic_list_concat(['<',IRI, '>  '],IRIF).

objectIntersectionOf(intersectionOf(CEs),ClassExpressionFL):-
   ClassExpressionF = 'ObjectIntersectionOf',
   findall(CEF,(member(CE,CEs),classExpression2function(CE,CEF)),L),% appendFunctional stringhe in L su classExpressionF
   appendFunctional(ClassExpressionF,L,ClassExpressionFL).
 
objectSomeValuesFrom(someValuesFrom(P,C),SVFFunc):-
  classExpression2function(C,CF),
  propertyExpression2function(P,PF),
  appendFunctional('ObjectSomeValuesFrom',[CF,PF], SVFFunc).

objectUnionOf(unionOf(CEs),ClassExpressionFL):-
  ClassExpressionF = 'ObjectUnionOf',
  findall(CEF,(member(CE,CEs),classExpression2function(CE,CEF)),L),% appendFunctional stringhe in L su classExpressionF
  appendFunctional(ClassExpressionF,L,ClassExpressionFL).

objectComplementOf(complementOf(CE), CEF):-
  classExpression2function(CE,CEs),
  appendFunctional('ObjectComplementOf', CEs, CEF). %[CEs]

objectOneOf(oneOf(List), CEFs) :-
  findall(CEF, (member(CE,List),classExpression2function(CE,CEF)), L),
    appendFunctional('ObjectOneOf', L, CEFs). 

objectAllValuesFrom(allValueFrom(P, C), AVFFunc):-
  classExpression2function(C, CF),
  propertyExpression2function(P, PF),
  appendFunctional('ObjectAllValuesFrom',[PF,CF], AVFFunc).

objectHasValue(hasValue(P,I), HVFunc):-
  propertyExpression2function(P, PF),
  individual2function(I, IF),
  appendFunctional('ObjectHasValue', [PF, IF], HVFunc).

objectHasSelf(hasSelf(P), HVFunc):-
  propertyExpression2function(P, PF),
  appendFunctional('ObjectHasSelf', PF, HVFunc).

objectMinCardinality(minCardinality(C, P, E), OMiCFunc):-
  number(C),
  C>=0,
  propertyExpression2function(P, PF),
  classExpression2function(E, EF),
  appendFunctional('ObjectMinCardinality',[C,PF,EF], OMiCFunc).
objectMinCardinality(minCardinality(C, P), OMiCFunc):-
  number(C),
  C>=0,
  propertyExpression2function(P, PF),
  appendFunctional('ObjectMinCardinality',[C,PF], OMiCFunc).

objectMaxCardinality(maxCardinality(C, P, E), OMaCFunc):-
  number(C),
  C>=0,
  propertyExpression2function(P, PF),
  classExpression2function(E, EF),
  appendFunctional('ObjectMaxCardinality',[C,PF,EF], OMaCFunc).
objectMaxCardinality(maxCardinality(C, P), OMaCFunc):-
  number(C),
  C>=0,
  propertyExpression2function(P, PF),
  appendFunctional('ObjectMaxCardinality',[C,PF], OMaCFunc).
  
objectExactCardinality(exactCardinality(C, P, E), OECFunc):-
  number(C),
  C>=0,
  propertyExpression2function(P, PF),
  classExpression2function(E, EF),
  appendFunctional('ObjectExactCardinality',[C,PF,EF], OECFunc).
objectExactCardinality(exactCardinality(C, P), OECFunc):-
  number(C),
  C>=0,
  propertyExpression2function(P, PF),
  appendFunctional('ObjectExactCardinality',[C,PF], OECFunc).

dataSomeValuesFrom(someValuesFrom(DE), DataPropertyExpressionFL):-
  DataPropertyExpressionF= 'DataSomeValuesFrom',
  dataExpression2function(DE,DEF),% appendFunctional stringhe in L su classExpressionF
	%? dataRange(DR). % todo
  appendFunctional(DataPropertyExpressionF, DEF, DataPropertyExpressionFL).

dataAllValuesFrom(allValuesFrom(DE), DataPropertyExpressionFL):-
  DataPropertyExpressionF= 'AllSomeValuesFrom',
  dataExpression2function(DE,DEF),
	%? dataRange(DR). % todo
  appendFunctional(DataPropertyExpressionF, DEF, DataPropertyExpressionFL).

dataHasValue(hasValue(P,I), DVFunc):-
  dataPropertyExpression2function(P, PF),
  literal2function(I, IF),
  appendFunctional('DataHasValue', [PF, IF], DVFunc).

dataMinCardinality(minCardinality(C, P), DMiCFunc):- 
  number(C),
  C>=0,
  propertyExpression2function(P, PF),
  appendFunctional('DataMinCardinality',[C,PF], DMiCFunc).

% DA FARE
dataMaxCardinality(maxCardinality(C, P), DMaCFunc):- 
  number(C),
  C>=0,
  propertyExpression2function(P, PF),
  appendFunctional('DataMaxCardinality',[C,PF], DMaCFunc).

dataExactCardinality(exactCardinality(C, P), DECFunc):-
  number(C),
  C>=0,
  propertyExpression2function(P, PF),
  appendFunctional('DataExactCardinality',[C,PF], DECFunc).

appendFunctional(Pred, Lista, Ris):-
  atomic_list_concat([Pred,'('|Lista], Atom), 
  atomic_concat(Atom, ')', Ris).   




  
