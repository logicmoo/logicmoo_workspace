/** <module> trill_2_funct

This module translates TRILL format into OWL functional syntax.

@author Riccardo Zese, Matilda Moro
@license Artistic License 2.0
@copyright Riccardo Zese
*/

:- module(trill_2_funct, [convert_explanations/2]).


% class(?IRI)
prolog2function(class(IRI), ClFunc):- 
  iri(IRI,IRIF),
  appendFunctional2('Class', [IRIF], ClFunc). 

% datatype(?IRI)
prolog2function(datatype(IRI), DtFunc):- 
  iri(IRI,IRIF),
  appendFunctional2('Datatype', [IRIF], DtFunc).

% objectProperty(?IRI)
prolog2function(objectProperty(IRI), OpFunc) :-
  iri(IRI,IRIF),
  appendFunctional2('ObjectProperty', [IRIF], OpFunc).

% dataProperty(?IRI)
prolog2function(dataPropery(IRI), DPFunc):- 
  iri(IRI,IRIF),
  appendFunctional2('Dataproperty', [IRIF], DPFunc).

% annotationProperty(?IRI)
prolog2function(annotationProperty(IRI), APFunc ):- 
  iri(IRI,IRIF),
  appendFunctional2('AnnotationProperty', [IRIF], APFunc).

% namedIndividual(?IRI)
prolog2function(namedIndividual(IRI), NIFunc):- 
  iri(IRI,IRIF),
  appendFunctional2('NamedIndividual', [IRIF], NIFunc).

% anonymousIndividual(?IRI)
prolog2function(anonymousIndividual(IRI), AIFunc):- 
  iri(IRI,IRIF),
  appendFunctional2('AnonymousIndividual', [IRIF], AIFunc).


/* ClassExpression e PropertyExpression */

% subClassOf(?SubClass:ClassExpression, ?SuperClass:ClassExpression)
prolog2function(subClassOf(ClassExpression1, ClassExpression2), SCFunc):- %appendFunctional SubClassOf ClassExpressionFunctional1 ClassExpressionFunctional2 
  classExpression2function(ClassExpression1,ClassExpressionFunctional1),
  classExpression2function(ClassExpression2,ClassExpressionFunctional2), 
  appendFunctional('SubClassOf',[ClassExpressionFunctional1, ClassExpressionFunctional2],SCFunc).

% equivalentClasses(?ClassExpressions:set(ClassExpression))
prolog2function(equivalentClasses(ListaClassExpression), ECFunc):-  %'EquivalentClasses(axiomAnnotations, ClassExpression, ClassExpression { ClassExpression } )'):-
  findall(CEF,(member(CE,ListaClassExpression),classExpression2function(CE,CEF)),L),
  appendFunctional('EquivalentClasses',L,ECFunc).

% disjointClasses(?ClassExpressions:set(ClassExpression))
prolog2function(disjointClasses(ListaClassExpression), DCFunc):- %'DisjointClasses(axiomAnnotations, ClassExpression, ClassExpression { ClassExpression })'). 
  findall(CEF,(member(CE,ListaClassExpression),classExpression2function(CE,CEF)),L),
  appendFunctional('DisjointClasses',L,DCFunc).

% disjointUnion(?ClassExpression, ?ClassExpressions:set(ClassExpression))
prolog2function(disjointUnion(IRI,ListaClassExpression), ECFunc):- %'DisjointUnion(axiomAnnotations, Class disjointClassExpressions)'% disjointClassExpressions := ClassExpression ClassExpression { ClassExpression })
  classExpression2function(IRI,ClassExpressionFunctional),
  findall(CEF,(member(CE,ListaClassExpression),classExpression2function(CE,CEF)),L),
  appendFunctional2('DisjointUnion',[ClassExpressionFunctional|L],ECFunc).

% subPropertyOf(?Sub:PropertyExpression, ?Super:ObjectPropertyExpression)
prolog2function(subPropertyOf(PropertyExpression1, PropertyExpression2), SPFunc):- 
  propertyExpression2function(PropertyExpression1,PropertyExpressionFunctional1),
  propertyExpression2function(PropertyExpression2,PropertyExpressionFunctional2),
  appendFunctional('SubObjectPropertyOf',[PropertyExpressionFunctional1, PropertyExpressionFunctional2],SPFunc).

% equivalentProperties(?PropertyExpressions:set(PropertyExpression))  
prolog2function(equivalentProperties(ListaPropertyExpression), EPFunc):- %'EquivalentObjectProperties(axiomAnnotations, ObjectPropertyExpression, ObjectPropertyExpression { ObjectPropertyExpression })').
  findall(PEF,(member(PE,ListaPropertyExpression), propertyExpression2function(PE,PEF)),A),
  appendFunctional('EquivalentObjectProperties', A, EPFunc).

% disjointProperties(?PropertyExpressions:set(PropertyExpression))                
prolog2function(dijointProperties(ListaPropertyExpression), DPFunc):- %'DisjointObjectProperties(axiomAnnotations, ObjectPropertyExpression, ObjectPropertyExpression { ObjectPropertyExpression })').
  findall(PEF,(member(PE, ListaPropertyExpression), propertyExpression2function(PE,PEF)),A),
  appendFunctional('DisjointObjectProperties', A, DPFunc).

% inverseProperties(?ObjectPropertyExpression1:ObjectPropertyExpression, ?ObjectPropertyExpression2:ObjectPropertyExpression)
prolog2function(inverseProperties(ObjectPropertyExpression1, ObjectPropertyExpression2), IOPFunc):- %'InverseObjectProperties(axiomAnnotations, ObjectPropertyExpression, ObjectPropertyExpression)'). 
  propertyExpression2function(ObjectPropertyExpression1,ObjectPropertyExpressionFunctional1),
  propertyExpression2function(ObjectPropertyExpression2,ObjectPropertyExpressionFunctional2), %appendFunctional SubClassOf ClassExpressionFunctional1 ClassExpressionFunctional2
  appendFunctional('InverseObjectProperties',[ObjectPropertyExpressionFunctional1, ObjectPropertyExpressionFunctional2],IOPFunc).

% propertyDomain(?PropertyExpression, ?CE)
prolog2function(propertyDomain(PropertyExpression, ClassExpression), OPDFunc):- %'ObjectPropertyDomain(axiomAnnotations, ObjectPropertyExpression, ClassExpression)').
  propertyExpression2function(PropertyExpression,PropertyExpressionF),
  classExpression2function(ClassExpression,ClassExpressionF),
  appendFunctional('ObjectPropertyDomain',[PropertyExpressionF,ClassExpressionF],OPDFunc).

% propertyRange(?PropertyExpression, ?ClassExpression)
prolog2function(propertyRange(PropertyExpression, ClassExpression), OPRFunc) :- %'ObjectPropertyRange(axiomAnnotations, ObjectPropertyExpression, ClassExpression)').
  propertyExpression2function(PropertyExpression,PropertyExpressionF),
  classExpression2function(ClassExpression,ClassExpressionF),
  appendFunctional('ObjectPropertyRange',[PropertyExpressionF,ClassExpressionF],OPRFunc).

% functionalProperty(?PropertyExpression)
prolog2function(functionalProperty(PropertyExpression),FOPFunc) :- %'FunctionalObjectProperty(axiomAnnotations, ObjectPropertyExpression)'). %?
  propertyExpression2function(PropertyExpression,IRI),
  appendFunctional('FunctionalObjectProperty',[IRI] ,FOPFunc).

% inverseFunctionalProperty(?ObjectPropertyExpression)
prolog2function(inverseFunctionalProperty(PropertyExpression), IFPFunc):- %'InverseFunctionalObjectProperty(axiomAnnotations, ObjectPropertyExpression').
  propertyExpression2function(PropertyExpression,IRI),
  appendFunctional('InverseFunctionalObjectProperty',[IRI] ,IFPFunc).

% reflexiveProperty(?ObjectPropertyExpression)
prolog2function(reflexiveProperty(PropertyExpression), RPFunc) :- % ReflexiveObjectProperty(axiomAnnotations, ObjectPropertyExpression)'). 
  propertyExpression2function(PropertyExpression,IRI),
  appendFunctional('ReflexiveObjectProperty',[IRI] ,RPFunc).

% irreflexiveProperty(?ObjectPropertyExpression)
prolog2function(irreflexiveProperty(PropertyExpression), IOPFunc):- %'IrreflexiveObjectProperty(axiomAnnotations, ObjectPropertyExpression)').  
  propertyExpression2function(PropertyExpression,IRI),
  appendFunctional('IrreflexiveObjectProperty', [IRI] ,IOPFunc).             

% symmetricProperty(?ObjectPropertyExpression)
prolog2function(symmetricProperty(PropertyExpression), SOPFunc) :- %'SymmetricObjectProperty(axiomAnnotations, ObjectPropertyExpression)').              
  propertyExpression2function(PropertyExpression,IRI),
  appendFunctional('SymmetricObjectProperty', [IRI] ,SOPFunc).             

% asymmetricProperty(?ObjectPropertyExpression)
prolog2function(asymmetricProperty(PropertyExpression), AOPFunc):- %'AsymmetricObjectProperty(axiomAnnotations, ObjectPropertyExpression)').             
  propertyExpression2function(PropertyExpression,IRI),
  appendFunctional('AsymmetricObjectProperty', [IRI] ,AOPFunc).             

% transitiveProperty(?ObjectPropertyExpression)
prolog2function(transitiveProperty(PropertyExpression), TOPFunc):- %'TransitiveObjectProperty(axiomAnnotations, ObjectPropertyExpression)').
  propertyExpression2function(PropertyExpression,IRI),
  appendFunctional('TransitiveObjectProperty', [IRI] ,TOPFunc).             

% hasKey(?ClassExpression,?PropertyExpression)
prolog2function(hasKey(ClassExpression,PropertyExpression), HKFunc):- %'HasKey(axiomAnnotations ClassExpression({ ObjectPropertyExpression }) ({ DataPropertyExpression }))'). 
  classExpression2function(ClassExpression,ClassExpressionF),
  propertyExpression2function(PropertyExpression,PropertyExpressionF),
  appendFunctional('HasKey',[ClassExpressionF,PropertyExpressionF],HKFunc).


/* Individual */

% sameIndividual(?Individuals:set(Individual))
prolog2function(sameIndividual(ListIndividual), SIFunc) :- %'SameIndividual(axiomAnnotations, Individual Individual { Individual })').
  findall(IEF,(member(IE, ListIndividual), individual2function(IE,IEF)),A),
  appendFunctional('SameIndividual', A, SIFunc).

% differentIndividuals(?Individuals:set(Individual))               
prolog2function(differentIndividual(ListIndividual), DIFunc ) :- %'DifferentIndividuals(axiomAnnotations, Individual Individual { Individual })').
  findall(IEF,(member(IE, ListIndividual), individual2function(IE,IEF)),A),
  appendFunctional('DifferentIndividual', A, DIFunc).


/* Assertion */

% classAssertion(?ClassExpression, ?Individual)               
prolog2function(classAssertion(ClassExpression, IndividualExpression), CAFunc) :- %'ClassAssertion(axiomAnnotations, ClassExpression Individual)').
  classExpression2function(ClassExpression,ClassExpressionF),
  individual2function(IndividualExpression,IndividualExpressionF),
  appendFunctional('ClassAssertion',[ClassExpressionF,IndividualExpressionF],CAFunc).

% propertyAssertion(?PropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)               
prolog2function(propertyAssertion(PropertyExpression, IndividualExpression1, IndividualExpression2), OPAFunc ):- %'ObjectPropertyAssertion( axiomAnnotations, ObjectPropertyExpression, sourceIndividual, targetIndividual)'). 
  propertyExpression2function(PropertyExpression,PropertyExpressionF),
  individual2function(IndividualExpression1, IndividualExpression1F),
  individual2function(IndividualExpression2, IndividualExpression2F),
  appendFunctional('ObjectPropertyAssertion', [PropertyExpressionF, IndividualExpression1F, IndividualExpression2F], OPAFunc).

% negativePropertyAssertion(?PropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)
prolog2function(negativePropertyAssertion(PropertyExpression, IndividualExpression1, IndividualExpression2), NOPAFunc ):- %'NegativeObjectPropertyAssertion(axiomAnnotations, ObjectPropertyExpression, sourceIndividual, targetIndividual)'). 
  propertyExpression2function(PropertyExpression,PropertyExpressionF),
  individual2function(IndividualExpression1, IndividualExpression1F),
  individual2function(IndividualExpression2, IndividualExpression2F),
  appendFunctional('ObjectPropertyAssertion', [PropertyExpressionF, IndividualExpression1F, IndividualExpression2F], NOPAFunc).


/* Annotation */

% annotationAssertion(?AnnotationProperty, ?AnnotationSubject, ?AnnotationValue)
prolog2function(annotationAssertion(AnnotationProperty, AnnotationSubject, AnnotationValue),AAFunc):- %'AnnotationAssertion(axiomAnnotations, AnnotationProperty, AnnotationSubject AnnotationValue)'.
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
        appendFunctional2('AnnotationAssertion', [AnnotationPropertyF,AnnotationSubjectF,AnnotationValueF], AAFunc))
  ).

% annotation(:IRI,?AnnotationProperty,?AnnotationValue)             
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
        appendFunctional2('AnnotationAssertion', [AnnotationPropertyF,AnnotationPropertyF,AnnotationValueF], AFunc)
  ).


/* Ontology */

% ontology(?IRI)
prolog2function(ontology(IRI), OIFunc) :- 
  appendFunctional1('Ontology', [IRI], OIFunc).

% ontologyImport(?Ontology, ?IRI)
prolog2function(ontologyImport(ontology(IRI)), OIMFunc):- 
  appendFunctional1('OntologyImport', [IRI], OIMFunc).

% ontologyVersionInfo(?Ontology, ?IRI)
prolog2function(ontologyVersionInfo(ontology(IRI), OVFunc)):-
  appendFunctional1('OntologyVersionInfo', [IRI], OVFunc).


/*Class expression*/

classExpression2function(CE,CEF):- 
  (iri(CE,CEF); 
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
  dataExactCardinality(CE,CEF)), 
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

/* Funzioni che controllano IRI */
individual2function(PE, PEF):-
  iri(PE,PEF).

propertyExpression2function(PE, PEF):-
  iri(PE,PEF).

/* Per ogni IRI inserisco < > e lascio uno spazio per rendere più leggibile la stampa */
iri(IRI,IRIF) :- 
  atomic(IRI),
  atomic_list_concat(['<',IRI,'>'],IRIL),
  atomic_list_concat([IRIL,' '],IRIF).

% objectIntersectionOf(+CE) is semidet
objectIntersectionOf(intersectionOf(CEs),ClassExpressionFL):-
   ClassExpressionF = 'ObjectIntersectionOf',
   findall(CEF,(member(CE,CEs),classExpression2function(CE,CEF)),L),
   appendFunctional(ClassExpressionF,L,ClassExpressionFL).

% objectSomeValuesFrom(+R) is semidet
objectSomeValuesFrom(someValuesFrom(P,C),SVFFunc):-
  classExpression2function(C,CF),
  propertyExpression2function(P,PF),
  appendFunctional('ObjectSomeValuesFrom',[CF,PF], SVFFunc).

% objectUnionOf(+CE) is semidet
objectUnionOf(unionOf(CEs),ClassExpressionFL):-
  ClassExpressionF = 'ObjectUnionOf',
  findall(CEF,(member(CE,CEs),classExpression2function(CE,CEF)),L),
  appendFunctional(ClassExpressionF,L,ClassExpressionFL).

% objectComplementOf(+CE) is semidet
objectComplementOf(complementOf(CE), CEF):-
  classExpression2function(CE,CEs),
  appendFunctional('ObjectComplementOf', CEs, CEF). 

% objectOneOf(+CE) is semidet
objectOneOf(oneOf(List), CEFs) :-
  findall(CEF, (member(CE,List),classExpression2function(CE,CEF)), L),
    appendFunctional('ObjectOneOf', L, CEFs). 

% objectAllValuesFrom(+R) is semidet
objectAllValuesFrom(allValueFrom(P, C), AVFFunc):-
  classExpression2function(C, CF),
  propertyExpression2function(P, PF),
  appendFunctional('ObjectAllValuesFrom',[PF,CF], AVFFunc).

% objectHasValue(+R) is semidet
objectHasValue(hasValue(P,I), HVFunc):-
  propertyExpression2function(P, PF),
  individual2function(I, IF),
  appendFunctional('ObjectHasValue', [PF, IF], HVFunc).

% objectHasSelf(+R) is semidet
objectHasSelf(hasSelf(P), HVFunc):-
  propertyExpression2function(P, PF),
  appendFunctional('ObjectHasSelf', PF, HVFunc).

% objectMinCardinality(+CR) is semide
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

% objectMaxCardinality(+CR) is semidet
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

% objectExactCardinality(+CR) is semidet  
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

% dataSomeValuesFrom(+DR) is semidet
dataSomeValuesFrom(someValuesFrom(DE), DataPropertyExpressionFL):-
  DataPropertyExpressionF= 'DataSomeValuesFrom',
  dataExpression2function(DE,DEF),
	% dataRange(DR) 
  appendFunctional(DataPropertyExpressionF, DEF, DataPropertyExpressionFL).

% dataAllValuesFrom(+DR) is semidet
dataAllValuesFrom(allValuesFrom(DE), DataPropertyExpressionFL):-
  DataPropertyExpressionF= 'AllSomeValuesFrom',
  dataExpression2function(DE,DEF),
	% dataRange(DR)
  appendFunctional(DataPropertyExpressionF, DEF, DataPropertyExpressionFL).

% dataHasValue(+DR) is semidet
dataHasValue(hasValue(P,I), DVFunc):-
  dataPropertyExpression2function(P, PF),
  literal2function(I, IF),
  appendFunctional('DataHasValue', [PF, IF], DVFunc).

% dataMinCardinality(+DR) is semidet
dataMinCardinality(minCardinality(C, P), DMiCFunc):- 
  number(C),
  C>=0,
  propertyExpression2function(P, PF),
  appendFunctional('DataMinCardinality',[C,PF], DMiCFunc).

% dataMaxCardinality(+DR) is semidet
dataMaxCardinality(maxCardinality(C, P), DMaCFunc):- 
  number(C),
  C>=0,
  propertyExpression2function(P, PF),
  appendFunctional('DataMaxCardinality',[C,PF], DMaCFunc).

% dataExactCardinality(+DR) is semidet
dataExactCardinality(exactCardinality(C, P), DECFunc):-
  number(C),
  C>=0,
  propertyExpression2function(P, PF),
  appendFunctional('DataExactCardinality',[C,PF], DECFunc).


/* Lists concatenation */

/* Axiom */
appendFunctional(Pred, Lista, Ris):-
  atomic_list_concat([Pred,'('|Lista], Atom), 
  atomic_concat(Atom, ')', Ris).   

/* Ontology */
appendFunctional1(Pred1, Lista1, Ris1):-
  atomic_list_concat([Pred1,'(<'|Lista1], Atom1), 
  atomic_concat(Atom1, '>', Ris1).

/* Declaration */
appendFunctional2(Pred2, Lista2, Ris2):-
    atomic_concat('Declaration(',Pred2, Atom2),
    atomic_list_concat([Atom2,'('|Lista2], Atom3), 
    atomic_concat(Atom3, '))', Ris2).


/* File writing kb_func.owl */

writefile:-
  
  /* File creation kb_funct.owl*/
  open('kb_funct.owl', write, Stream),
  nl(Stream),

  /* Prefixes writing */
  kb_prefixes(Le),
  foreach(member(K=P,Le), 
    (
      write(Stream, 'Prefix('), 
      write(Stream, K),
      write(Stream, ':=<'),
      write(Stream, P),
      write(Stream, '>)\n')
    )
  ),
  write(Stream,'\n'),

  /* Ontology writing */
  findall(PO, (axiom(ontology(Oiri)),prolog2function(ontology(Oiri),PO)),Lo),
  foreach(member(Os, Lo), writeln(Stream, Os)), 
  write(Stream,'\n'),

  /* Axiom writing */
  findall(OP,(axiom(Ax),Ax\=ontology(_),prolog2function(Ax,OP)),La),
  foreach(member(As,La), writeln(Stream,As)),

  /* Closing parenthesis and ending file writing */
  write(Stream,')'), 
  write(Stream,'\n'),
  close(Stream).
 
/**
 * convert_explanations(++TRILLExplanations:list,-OWLFunctExplanations:list) is det
 *
 * The predicate converts the axioms contained in the list of explanations
 * returned by TRILL into OWL Functional sytntax.
 */
convert_explanations([],[]).

convert_explanations([ExplTRILL|ExplsTRILL],[ExplFunct|ExplsFunct]):-
  convert_explanation(ExplTRILL,ExplFunct),
  convert_explanations(ExplsTRILL,ExplsFunct).

convert_explanation([],[]).

convert_explanation([TRILLAx|OtherTRILLAxs],[FunctAx|OtherFunctAxs]):-
  prolog2function(TRILLAx,FunctAx),
  convert_explanation(OtherTRILLAxs,OtherFunctAxs).
