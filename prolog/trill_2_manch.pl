/** <module> trill_2_manch

This module translates TRILL format into OWL manchester syntax.

@author Riccardo Zese
@license Artistic License 2.0
@copyright Riccardo Zese
*/

:- module(trill_2_manch, [convert_explanations/2]).

% simple: some better rendering
% full:   full Manchester Syntax recommendation TODO
trill_2_manch_setting(convertion_mode(simple)).

% class(?IRI)
prolog2manchester(class(IRI), ClFunc):- 
  iri(IRI,IRIF),
  appendManchester1('Class', [IRIF], ClFunc). 

% datatype(?IRI)
prolog2manchester(datatype(IRI), DtFunc):- 
  iri(IRI,IRIF),
  appendManchester1('Datatype', [IRIF], DtFunc).

% objectProperty(?IRI)
prolog2manchester(objectProperty(IRI), OpFunc) :-
  iri(IRI,IRIF),
  appendManchester1('ObjectProperty', [IRIF], OpFunc).

% dataProperty(?IRI)
prolog2manchester(dataPropery(IRI), DPFunc):- 
  iri(IRI,IRIF),
  appendManchester1('Dataproperty', [IRIF], DPFunc).

% annotationProperty(?IRI)
prolog2manchester(annotationProperty(IRI), APFunc ):- 
  iri(IRI,IRIF),
  appendManchester1('AnnotationProperty', [IRIF], APFunc).

% namedIndividual(?IRI)
prolog2manchester(namedIndividual(IRI), NIFunc):- 
  iri(IRI,IRIF),
  appendManchester1('Individual', [IRIF], NIFunc).

% anonymousIndividual(?IRI)
prolog2manchester(anonymousIndividual(IRI), AIFunc):- 
  iri(IRI,IRIF),
  appendManchester1('Individual', [IRIF], AIFunc).


/* ClassExpression e PropertyExpression */

% subClassOf(?SubClass:ClassExpression, ?SuperClass:ClassExpression)
prolog2manchester(subClassOf(ClassExpression1, ClassExpression2), SCFunc):- %appendManchester SubClassOf ClassExpressionFunctional1 ClassExpressionFunctional2 
  classExpression2manchester(ClassExpression1,ClassExpressionFunctional1),
  classExpression2manchester(ClassExpression2,ClassExpressionFunctional2), 
  trill_2_manch_setting(convertion_mode(simple)) ->
    appendManchester2([ClassExpressionFunctional1, 'subClassOf', ClassExpressionFunctional2],SCFunc) ;
    ( appendManchester('Class',[ClassExpressionFunctional1],SCFunc0),
      appendManchester('SubClassOf',[ClassExpressionFunctional2],SCFunc1),
      appendManchester2([SCFunc0, SCFunc1],SCFunc)
    ).

% equivalentClasses(?ClassExpressions:set(ClassExpression))
prolog2manchester(equivalentClasses(ListaClassExpression), ECFunc):-  %'EquivalentClasses(axiomAnnotations, ClassExpression, ClassExpression { ClassExpression } )'):-
  findall(CEF,(member(CE,ListaClassExpression),classExpression2manchester(CE,CEF)),L),
  length(L,2) ->
    ( L = [C1,C2],
      trill_2_manch_setting(convertion_mode(simple)) ->
       appendManchester2([C1,'equivalentTo',C2],ECFunc) ;
       ( appendManchester('Class',[C1],SCFunc0),
         appendManchester('EquivalentTo',[C2],SCFunc1),
         appendManchester2([SCFunc0, SCFunc1],ECFunc)
       )
    )
    ;
    ( trill_2_manch_setting(convertion_mode(simple)) ->
       appendManchester3('EquivalentTo',L,ECFunc);
       appendManchester('EquivalentTo',L,ECFunc)
    ).

% disjointClasses(?ClassExpressions:set(ClassExpression))
prolog2manchester(disjointClasses(ListaClassExpression), ECFunc):- %'DisjointClasses(axiomAnnotations, ClassExpression, ClassExpression { ClassExpression })'). 
  findall(CEF,(member(CE,ListaClassExpression),classExpression2manchester(CE,CEF)),L),
  length(L,2) ->
    ( L = [C1,C2],
      trill_2_manch_setting(convertion_mode(simple)) ->
       appendManchester2([C1,'disjointWith',C2],ECFunc) ;
       ( appendManchester('Class',[C1],SCFunc0),
         appendManchester('DisjointWith',[C2],SCFunc1),
         appendManchester2([SCFunc0, SCFunc1],ECFunc)
       )
    )
    ;
    ( trill_2_manch_setting(convertion_mode(simple)) ->
        appendManchester3('DisjointWith',L,ECFunc);
        appendManchester('DisjointWith',L,ECFunc)
    ).

% disjointUnion(?ClassExpression, ?ClassExpressions:set(ClassExpression))
prolog2manchester(disjointUnion(IRI,ListaClassExpression), ECFunc):- %'DisjointUnion(axiomAnnotations, Class disjointClassExpressions)'% disjointClassExpressions := ClassExpression ClassExpression { ClassExpression })
  classExpression2manchester(IRI,ClassExpressionFunctional),
  objectUnionOf(ListaClassExpression, LM),
  trill_2_manch_setting(convertion_mode(simple)) ->
    appendManchester2([ClassExpressionFunctional,'disjointWith',LM],ECFunc)
    ;
    (
      appendManchester('Class',[ClassExpressionFunctional],SCFunc0),
      appendManchester('DisjointWith',[LM],SCFunc1),
      appendManchester2([SCFunc0, SCFunc1],ECFunc)
    ).

% subPropertyOf(?Sub:PropertyExpression, ?Super:ObjectPropertyExpression)
prolog2manchester(subPropertyOf(PropertyExpression1, PropertyExpression2), SPFunc):- 
  propertyExpression2manchester(PropertyExpression1,PropertyExpressionFunctional1),
  propertyExpression2manchester(PropertyExpression2,PropertyExpressionFunctional2),
  trill_2_manch_setting(convertion_mode(simple)) ->
    appendManchester([PropertyExpressionFunctional1, 'subPropertyOf', PropertyExpressionFunctional2],SPFunc) ;
    ( appendManchester('ObjectProperty',[PropertyExpressionFunctional1],SPFunc0),
      appendManchester('SubPropertyOf',[PropertyExpressionFunctional1],SPFunc1),
      appendManchester2([SPFunc0, SPFunc1],SPFunc)
    ).

% equivalentProperties(?PropertyExpressions:set(PropertyExpression))  
prolog2manchester(equivalentProperties(ListaPropertyExpression), EPFunc):- %'EquivalentObjectProperties(axiomAnnotations, ObjectPropertyExpression, ObjectPropertyExpression { ObjectPropertyExpression })').
  findall(PEF,(member(PE,ListaPropertyExpression), propertyExpression2manchester(PE,PEF)),L),
  length(L,2) ->
    ( L = [P1,P2],
      trill_2_manch_setting(convertion_mode(simple)) ->
       appendManchester2([P1,'equivalentTo',P2],EPFunc) ;
       ( appendManchester('ObjectProperty',[P1],SPFunc0),
         appendManchester('EquivalentTo',[P2],SPFunc1),
         appendManchester2([SPFunc0, SPFunc1],EPFunc)
       )
    )
    ;
    ( trill_2_manch_setting(convertion_mode(simple)) ->
       appendManchester3('EquivalentTo',L,EPFunc);
       appendManchester('EquivalentTo',L,EPFunc)
    ).

% disjointProperties(?PropertyExpressions:set(PropertyExpression))                
prolog2manchester(dijointProperties(ListaPropertyExpression), DPFunc):- %'DisjointObjectProperties(axiomAnnotations, ObjectPropertyExpression, ObjectPropertyExpression { ObjectPropertyExpression })').
  findall(PEF,(member(PE, ListaPropertyExpression), propertyExpression2manchester(PE,PEF)),L),
  length(L,2) ->
    ( L = [P1,P2],
      trill_2_manch_setting(convertion_mode(simple)) ->
       appendManchester2([P1,'disjointWith',P2],DPFunc) ;
       ( appendManchester('ObjectProperty',[P1],SPFunc0),
         appendManchester('DisjointWith',[P2],SPFunc1),
         appendManchester2([SPFunc0, SPFunc1],DPFunc)
       )
    )
    ;
    ( trill_2_manch_setting(convertion_mode(simple)) ->
       appendManchester3('DisjointWith',L,DPFunc);
       appendManchester('DisjointWith',L,DPFunc)
    ).

% inverseProperties(?ObjectPropertyExpression1:ObjectPropertyExpression, ?ObjectPropertyExpression2:ObjectPropertyExpression)
prolog2manchester(inverseProperties(ObjectPropertyExpression1, ObjectPropertyExpression2), IOPFunc):- %'InverseObjectProperties(axiomAnnotations, ObjectPropertyExpression, ObjectPropertyExpression)'). 
  propertyExpression2manchester(ObjectPropertyExpression1,ObjectPropertyExpressionFunctional1),
  propertyExpression2manchester(ObjectPropertyExpression2,ObjectPropertyExpressionFunctional2), %appendManchester SubClassOf ClassExpressionFunctional1 ClassExpressionFunctional2
  trill_2_manch_setting(convertion_mode(simple)) ->
    appendManchester2([ObjectPropertyExpressionFunctional1, 'inverseOf',ObjectPropertyExpressionFunctional2],IOPFunc);
    ( appendManchester('ObjectProperty',[ObjectPropertyExpression1],IOPFunc0),
      appendManchester('Inverse',[ObjectPropertyExpression2],IOPFunc1),
      appendManchester2([IOPFunc0, IOPFunc1],IOPFunc)
    ).

% propertyDomain(?PropertyExpression, ?CE)
prolog2manchester(propertyDomain(PropertyExpression, ClassExpression), OPDFunc):- %'ObjectPropertyDomain(axiomAnnotations, ObjectPropertyExpression, ClassExpression)').
  propertyExpression2manchester(PropertyExpression,PropertyExpressionF),
  classExpression2manchester(ClassExpression,ClassExpressionF),
  trill_2_manch_setting(convertion_mode(simple)) ->
    appendManchester2([PropertyExpressionF, 'domain',ClassExpressionF],OPDFunc);
    ( appendManchester('ObjectProperty',[PropertyExpressionF],OPDFunc0),
      appendManchester('Domain',[ClassExpressionF],OPDFunc1),
      appendManchester2([OPDFunc0, OPDFunc1],OPDFunc)
    ).

% propertyRange(?PropertyExpression, ?ClassExpression)
prolog2manchester(propertyRange(PropertyExpression, ClassExpression), OPRFunc) :- %'ObjectPropertyRange(axiomAnnotations, ObjectPropertyExpression, ClassExpression)').
  propertyExpression2manchester(PropertyExpression,PropertyExpressionF),
  classExpression2manchester(ClassExpression,ClassExpressionF),
  trill_2_manch_setting(convertion_mode(simple)) ->
    appendManchester2([PropertyExpressionF, 'range',ClassExpressionF],OPRFunc);
    ( appendManchester('ObjectProperty',[PropertyExpressionF],OPRFunc0),
      appendManchester('Range',[ClassExpressionF],OPRFunc1),
      appendManchester2([OPRFunc0, OPRFunc1],OPRFunc)
    ).

% functionalProperty(?PropertyExpression)
prolog2manchester(functionalProperty(PropertyExpression),FOPFunc) :- %'FunctionalObjectProperty(axiomAnnotations, ObjectPropertyExpression)'). %?
  propertyExpression2manchester(PropertyExpression,PropertyExpressionF),
  trill_2_manch_setting(convertion_mode(simple)) ->
    appendManchester2([PropertyExpressionF, 'functional'],FOPFunc);
    ( appendManchester('ObjectProperty',[PropertyExpressionF],FOPFunc0),
      appendManchester('Characteristic','Functional',FOPFunc1),
      appendManchester2([FOPFunc0, FOPFunc1],FOPFunc)
    ).

% inverseFunctionalProperty(?ObjectPropertyExpression)
prolog2manchester(inverseFunctionalProperty(PropertyExpression), IFPFunc):- %'InverseFunctionalObjectProperty(axiomAnnotations, ObjectPropertyExpression').
  propertyExpression2manchester(PropertyExpression,PropertyExpressionF),
  trill_2_manch_setting(convertion_mode(simple)) ->
    appendManchester2([PropertyExpressionF, 'inverseFunctional'],IFPFunc);
    ( appendManchester('ObjectProperty',[PropertyExpressionF],IFPFunc0),
      appendManchester('Characteristic','InverseFunctional',IFPFunc1),
      appendManchester2([IFPFunc0, IFPFunc1],IFPFunc)
    ).

% reflexiveProperty(?ObjectPropertyExpression)
prolog2manchester(reflexiveProperty(PropertyExpression), RPFunc) :- % ReflexiveObjectProperty(axiomAnnotations, ObjectPropertyExpression)'). 
  propertyExpression2manchester(PropertyExpression,PropertyExpressionF),
  trill_2_manch_setting(convertion_mode(simple)) ->
    appendManchester2([PropertyExpressionF, 'reflexive'],RPFunc);
    ( appendManchester('ObjectProperty',[PropertyExpressionF],RPFunc0),
      appendManchester('Characteristic','Reflexive',RPFunc1),
      appendManchester2([RPFunc0, RPFunc1],RPFunc)
    ).

% irreflexiveProperty(?ObjectPropertyExpression)
prolog2manchester(irreflexiveProperty(PropertyExpression), IOPFunc):- %'IrreflexiveObjectProperty(axiomAnnotations, ObjectPropertyExpression)').  
  propertyExpression2manchester(PropertyExpression,PropertyExpressionF),
  trill_2_manch_setting(convertion_mode(simple)) ->
    appendManchester2([PropertyExpressionF, 'irreflexive'],IOPFunc);
    ( appendManchester('ObjectProperty',[PropertyExpressionF],IOPFunc0),
      appendManchester('Characteristic','Irreflexive',IOPFunc1),
      appendManchester2([IOPFunc0, IOPFunc1],IOPFunc)
    ).

% symmetricProperty(?ObjectPropertyExpression)
prolog2manchester(symmetricProperty(PropertyExpression), SOPFunc) :- %'SymmetricObjectProperty(axiomAnnotations, ObjectPropertyExpression)').              
  propertyExpression2manchester(PropertyExpression,PropertyExpressionF),
  trill_2_manch_setting(convertion_mode(simple)) ->
    appendManchester2([PropertyExpressionF, 'symmetric'],SOPFunc);
    ( appendManchester('ObjectProperty',[PropertyExpressionF],SOPFunc0),
      appendManchester('Characteristic','Symmetric',SOPFunc1),
      appendManchester2([SOPFunc0, SOPFunc1],SOPFunc)
    ).

% asymmetricProperty(?ObjectPropertyExpression)
prolog2manchester(asymmetricProperty(PropertyExpression), AOPFunc):- %'AsymmetricObjectProperty(axiomAnnotations, ObjectPropertyExpression)').             
  propertyExpression2manchester(PropertyExpression,PropertyExpressionF),
  trill_2_manch_setting(convertion_mode(simple)) ->
    appendManchester2([PropertyExpressionF, 'asymmetric'],AOPFunc);
    ( appendManchester('ObjectProperty',[PropertyExpressionF],AOPFunc0),
      appendManchester('Characteristic','Asymmetric',AOPFunc1),
      appendManchester2([AOPFunc0, AOPFunc1],AOPFunc)
    ).

% transitiveProperty(?ObjectPropertyExpression)
prolog2manchester(transitiveProperty(PropertyExpression), TOPFunc):- %'TransitiveObjectProperty(axiomAnnotations, ObjectPropertyExpression)').
  propertyExpression2manchester(PropertyExpression,PropertyExpressionF),
  trill_2_manch_setting(convertion_mode(simple)) ->
    appendManchester2([PropertyExpressionF, 'transitive'],TOPFunc);
    ( appendManchester('ObjectProperty',[PropertyExpressionF],TOPFunc0),
      appendManchester('Characteristic','Transitive',TOPFunc1),
      appendManchester2([TOPFunc0, TOPFunc1],TOPFunc)
    ).

% hasKey(?ClassExpression,?PropertyExpression)
prolog2manchester(hasKey(ClassExpression,PropertyExpression), HKFunc):- %'HasKey(axiomAnnotations ClassExpression({ ObjectPropertyExpression }) ({ DataPropertyExpression }))'). 
  classExpression2manchester(ClassExpression,ClassExpressionF),
  propertyExpression2manchester(PropertyExpression,PropertyExpressionF),
  trill_2_manch_setting(convertion_mode(simple)) ->
    appendManchester2([ClassExpressionF, 'hasKey', PropertyExpressionF],HKFunc) ;
    ( appendManchester('Class',[ClassExpressionF],HKFunc0),
      appendManchester('HasKey',[PropertyExpressionF],HKFunc1),
      appendManchester2([HKFunc0, HKFunc1],HKFunc)
    ).


/* Individual */

% sameIndividual(?Individuals:set(Individual))
prolog2manchester(sameIndividual(ListIndividual), SIFunc) :- %'SameIndividual(axiomAnnotations, Individual Individual { Individual })').
  findall(IEF,(member(IE, ListIndividual), individual2manchester(IE,IEF)),L),
  length(L,2) ->
    ( L = [I1,I2],
      trill_2_manch_setting(convertion_mode(simple)) ->
       appendManchester2([I1,'sameAs',I2],SIFunc) ;
       ( appendManchester('Individual',[I1],SIFunc0),
         appendManchester('SameAs',[I2],SIFunc1),
         appendManchester2([SIFunc0, SIFunc1],SIFunc)
       )
    )
    ;
    (trill_2_manch_setting(convertion_mode(simple)) ->
      appendManchester3('SameIndividual',L,SIFunc);
      appendManchester('SameIndividual',L,SIFunc)
    ).

% differentIndividuals(?Individuals:set(Individual))               
prolog2manchester(differentIndividual(ListIndividual), DIFunc ) :- %'DifferentIndividuals(axiomAnnotations, Individual Individual { Individual })').
  findall(IEF,(member(IE, ListIndividual), individual2manchester(IE,IEF)),A),
  length(L,2) ->
    ( L = [I1,I2],
      trill_2_manch_setting(convertion_mode(simple)) ->
       appendManchester2([I1,'differentFrom',I2],DIFunc) ;
       ( appendManchester('Individual',[I1],DIFunc0),
         appendManchester('DifferentFrom',[I2],DIFunc1),
         appendManchester2([DIFunc0, DIFunc1],DIFunc)
       )
    )
    ;
    (trill_2_manch_setting(convertion_mode(simple)) ->
      appendManchester3('DifferentIndividuals',L,DIFunc);
      appendManchester('DifferentIndividuals',L,DIFunc)
    ).


/* Assertion */

% classAssertion(?ClassExpression, ?Individual)               
prolog2manchester(classAssertion(ClassExpression, IndividualExpression), CAFunc) :- %'ClassAssertion(axiomAnnotations, ClassExpression Individual)').
  classExpression2manchester(ClassExpression,ClassExpressionF),
  individual2manchester(IndividualExpression,IndividualExpressionF),
  trill_2_manch_setting(convertion_mode(simple)) ->
    appendManchester2([IndividualExpressionF, 'type', ClassExpressionF],CAFunc) ;
    ( appendManchester('Individual',[IndividualExpressionF],CAFunc0),
      appendManchester('Type',[ClassExpressionF],CAFunc1),
      appendManchester2([CAFunc0, CAFunc1],CAFunc)
    ).

% propertyAssertion(?PropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)               
prolog2manchester(propertyAssertion(PropertyExpression, IndividualExpression1, IndividualExpression2), OPAFunc ):- %'ObjectPropertyAssertion( axiomAnnotations, ObjectPropertyExpression, sourceIndividual, targetIndividual)'). 
  propertyExpression2manchester(PropertyExpression,PropertyExpressionF),
  individual2manchester(IndividualExpression1, IndividualExpression1F),
  individual2manchester(IndividualExpression2, IndividualExpression2F),
  trill_2_manch_setting(convertion_mode(simple)) ->
    appendManchester2([IndividualExpression1F, PropertyExpressionF, IndividualExpression2F],OPAFunc) ;
    ( appendManchester('Individual',[IndividualExpression1F],OPAFunc0),
      appendManchester('Fact',[PropertyExpressionF, IndividualExpression2F],OPAFunc1),
      appendManchester2([OPAFunc0, OPAFunc1],OPAFunc)
    ).

% negativePropertyAssertion(?PropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)
prolog2manchester(negativePropertyAssertion(PropertyExpression, IndividualExpression1, IndividualExpression2), NOPAFunc ):- %'NegativeObjectPropertyAssertion(axiomAnnotations, ObjectPropertyExpression, sourceIndividual, targetIndividual)'). 
  propertyExpression2manchester(PropertyExpression,PropertyExpressionF),
  individual2manchester(IndividualExpression1, IndividualExpression1F),
  individual2manchester(IndividualExpression2, IndividualExpression2F),
  trill_2_manch_setting(convertion_mode(simple)) ->
    appendManchester2([IndividualExpression1F, PropertyExpressionF, IndividualExpression2F],NOPAFunc) ;
    ( appendManchester('Individual',[IndividualExpression1F],NOPAFunc0),
      appendManchester('Fact',['not', PropertyExpressionF, IndividualExpression2F],NOPAFunc1),
      appendManchester2([NOPAFunc0, NOPAFunc1],NOPAFunc)
    ).


/* Annotation */ 
/* TODO
% annotationAssertion(?AnnotationProperty, ?AnnotationSubject, ?AnnotationValue)
prolog2manchester(annotationAssertion(AnnotationProperty, AnnotationSubject, AnnotationValue),AAFunc):- %'AnnotationAssertion(axiomAnnotations, AnnotationProperty, AnnotationSubject AnnotationValue)'.
  propertyExpression2manchester(AnnotationProperty, AnnotationPropertyF),
  propertyExpression2manchester(AnnotationSubject, AnnotationSubjectF),
  (
        % condition 
        iri(AnnotationValue,AnnotationValueF)
    ->
        % true 
        appendManchester('AnnotationAssertion', [AnnotationPropertyF,AnnotationSubjectF,AnnotationValueF], AAFunc)
    ;
        % false 
        (literal2manchester(AnnotationValue, AnnotationValueF),
        appendManchester1('AnnotationAssertion', [AnnotationPropertyF,AnnotationSubjectF,AnnotationValueF], AAFunc))
  ).

% annotation(:IRI,?AnnotationProperty,?AnnotationValue)             
prolog2manchester(annotation(AnnotationProperty, AnnotationProperty, AnnotationValue), AFunc):-%(iri,annotationProperty,annotationValue),'Annotation(annotationAnnotations, AnnotationProperty, AnnotationValue)'
  propertyExpression2manchester(AnnotationProperty, AnnotationPropertyF),
  (
        % condition 
        iri(AnnotationValue,AnnotationValueF)
    ->
        % true 
        appendManchester('AnnotationAssertion', [AnnotationPropertyF,AnnotationPropertyF,AnnotationValueF], AFunc)
    ;
        % false 
        literal2manchester(AnnotationValue, AnnotationValueF),
        appendManchester1('AnnotationAssertion', [AnnotationPropertyF,AnnotationPropertyF,AnnotationValueF], AFunc)
  ).
*/

/* Ontology */

% ontology(?IRI)
prolog2manchester(ontology(IRI), OIFunc):- 
  iri(IRI,IRIM),
  get_ontology_imports(IRIs0),
  get_ontology_version(IRIs1),
  IRIs = [IRIM,IRIs1|IRIs0],
  appendManchester1('Ontology', IRIs, OIFunc).

get_ontology_imports(IRIs0):-
  findall(ontologyImport(ontology(ImportIRI)), axiom(ontologyImport(ontology(ImportIRI))), ImpAxs),
  ( dif(ImpAxs,[]) ->  
     findall(ImportIRIM,(member(Ax,ImpAxs),prolog2manchester(Ax, ImportIRIM)), IRIs0) ; 
     IRIs0 = [] 
  ).

get_ontology_version(IRIs1):-
  ( axiom(ontologyVersionInfo(ontology(VersIRI))) -> 
    (prolog2manchester(ontologyVersionInfo(ontology(VersIRI, VersIRIM))), IRIs1 = [VersIRIM]) ; 
     IRIs1 = []
  ).


% ontologyImport(?Ontology, ?IRI)
prolog2manchester(ontologyImport(ontology(IRI)), OIMFunc):- 
  iri(IRI,IRIM),
  appendManchester1('Import', [IRIM], OIMFunc).

% ontologyVersionInfo(?Ontology, ?IRI)
prolog2manchester(ontologyVersionInfo(ontology(IRI), IRIM)):-
  iri(IRI,IRIM).


/*Class expression*/

classExpression2manchester(CE,CEF):- 
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
individual2manchester(PE, PEF):-
  iri(PE,PEF).

propertyExpression2manchester(PE, PEF):-
  iri(PE,PEF).

/* Per ogni IRI inserisco < > e lascio uno spazio per rendere più leggibile la stampa */
iri(IRI,IRIF) :- 
  atomic(IRI),
  atomic_list_concat(['<',IRI,'>'],IRIL),
  atomic_list_concat([IRIL,' '],IRIF).

% objectIntersectionOf(+CE) is semidet
objectIntersectionOf(intersectionOf(CEs),ClassExpressionFL):-
   ClassExpressionF = 'and',
   findall(CEF,(member(CE,CEs),classExpression2manchester(CE,CEF)),L),
   appendManchester4(ClassExpressionF,L,ClassExpressionFL).

% objectSomeValuesFrom(+R) is semidet
objectSomeValuesFrom(someValuesFrom(P,C),SVFFunc):-
  classExpression2manchester(C,CF),
  propertyExpression2manchester(P,PF),
  appendManchester4('some',[CF,PF], SVFFunc).

% objectUnionOf(+CE) is semidet
objectUnionOf(unionOf(CEs),ClassExpressionFL):-
  ClassExpressionF = 'or',
  findall(CEF,(member(CE,CEs),classExpression2manchester(CE,CEF)),L),
  appendManchester4(ClassExpressionF,L,ClassExpressionFL).

% objectComplementOf(+CE) is semidet
objectComplementOf(complementOf(CE), CEF):-
  classExpression2manchester(CE,CEs),
  appendManchester4('not', [CEs], CEF). 

% objectOneOf(+CE) is semidet
objectOneOf(oneOf(List), CEFs) :-
  findall(CEF, (member(CE,List),classExpression2manchester(CE,CEF)), L),
  appendManchester5(L, CEFs). 

% objectAllValuesFrom(+R) is semidet
objectAllValuesFrom(allValueFrom(P, C), AVFFunc):-
  classExpression2manchester(C, CF),
  propertyExpression2manchester(P, PF),
  appendManchester4('only',[PF,CF], AVFFunc).

% objectHasValue(+R) is semidet
objectHasValue(hasValue(P,I), HVFunc):-
  propertyExpression2manchester(P, PF),
  individual2manchester(I, IF),
  appendManchester4('value', [PF, IF], HVFunc).

% objectHasSelf(+R) is semidet
objectHasSelf(hasSelf(P), HVFunc):-
  propertyExpression2manchester(P, PF),
  appendManchester4(PF, 'Self', HVFunc).

% objectMinCardinality(+CR) is semide
objectMinCardinality(minCardinality(C, P, E), OMiCFunc):-
  number(C),
  C>=0,
  propertyExpression2manchester(P, PF),
  classExpression2manchester(E, EF),
  appendManchester6('min',[C,PF,EF], OMiCFunc).

objectMinCardinality(minCardinality(C, P), OMiCFunc):-
  number(C),
  C>=0,
  propertyExpression2manchester(P, PF),
  appendManchester4('min',[PF,C], OMiCFunc).

% objectMaxCardinality(+CR) is semidet
objectMaxCardinality(maxCardinality(C, P, E), OMaCFunc):-
  number(C),
  C>=0,
  propertyExpression2manchester(P, PF),
  classExpression2manchester(E, EF),
  appendManchester6('max',[C,PF,EF], OMaCFunc).

objectMaxCardinality(maxCardinality(C, P), OMaCFunc):-
  number(C),
  C>=0,
  propertyExpression2manchester(P, PF),
  appendManchester4('max',[PF,C], OMaCFunc).

% objectExactCardinality(+CR) is semidet  
objectExactCardinality(exactCardinality(C, P, E), OECFunc):-
  number(C),
  C>=0,
  propertyExpression2manchester(P, PF),
  classExpression2manchester(E, EF),
  appendManchester6('exactly',[C,PF,EF], OECFunc).

objectExactCardinality(exactCardinality(C, P), OECFunc):-
  number(C),
  C>=0,
  propertyExpression2manchester(P, PF),
  appendManchester4('exactly',[C,PF], OECFunc).

% dataSomeValuesFrom(+DR) is semidet
dataSomeValuesFrom(someValuesFrom(DE), DataPropertyExpressionFL):-
  DataPropertyExpressionF= 'some',
  dataExpression2manchester(DE,DEF),
	% dataRange(DR) 
  appendManchester4(DataPropertyExpressionF, DEF, DataPropertyExpressionFL).

% dataAllValuesFrom(+DR) is semidet
dataAllValuesFrom(allValuesFrom(DE), DataPropertyExpressionFL):-
  DataPropertyExpressionF= 'only',
  dataExpression2manchester(DE,DEF),
	% dataRange(DR)
  appendManchester4(DataPropertyExpressionF, DEF, DataPropertyExpressionFL).

% dataHasValue(+DR) is semidet
dataHasValue(hasValue(P,I), DVFunc):-
  dataPropertyExpression2manchester(P, PF),
  literal2manchester(I, IF),
  appendManchester4('value', [PF, IF], DVFunc).

% dataMinCardinality(+DR) is semidet
dataMinCardinality(minCardinality(C, P), DMiCFunc):- 
  number(C),
  C>=0,
  propertyExpression2manchester(P, PF),
  appendManchester4('min',[C,PF], DMiCFunc).

% dataMaxCardinality(+DR) is semidet
dataMaxCardinality(maxCardinality(C, P), DMaCFunc):- 
  number(C),
  C>=0,
  propertyExpression2manchester(P, PF),
  appendManchester4('max',[C,PF], DMaCFunc).

% dataExactCardinality(+DR) is semidet
dataExactCardinality(exactCardinality(C, P), DECFunc):-
  number(C),
  C>=0,
  propertyExpression2manchester(P, PF),
  appendManchester4('exactly',[C,PF], DECFunc).


/* Lists concatenation */

/* Axiom */
appendManchester(Pred, Lista, Ris):-
  atomic_list_concat([Pred,': '|Lista], Ris).   

/* Ontology and Declaration */
appendManchester1(Pred2, Lista2, Ris2):-
    atomic_list_concat([Pred2,': '|Lista2], Ris2).

/* General concat */
appendManchester2(Lista, Ris):-
    atomic_list_concat(Lista, Ris). 

/* Declaration */
appendManchester3(Pred2, Lista2, Ris2):-
  atomic_list_concat([Pred2,'('|Lista2], Atom3), 
  atomic_concat(Atom3, ')', Ris2).

/* List concat */
appendManchester4(Pred, List, Ris4):-
  atomic_list_concat([' ', Pred, ' '], PredSp),
  divide_with_pred(PredSp,List,ListPred),
  atomic_list_concat(ListPred,Ris4).

divide_with_pred(Pred, [El1], [Pred, El1]):- !.

divide_with_pred(Pred, El1, [Pred, El1]):- 
  atom(El1),!.

divide_with_pred(Pred, [El1, El2], [El1, Pred, El2]):- !.

divide_with_pred(Pred, [H| T], [H, Pred| T1]):- 
  divide_with_pred(Pred,T,T1).

appendManchester5(Lista2, Ris2):-
  atomic_list_concat(['{'|Lista2], Atom3), 
  atomic_concat(Atom3, '}', Ris2).

appendManchester6(Pred, [C,PF,EF], Ris2):-
  atomic_list_concat([' ', Pred, ' '], PredSp),
  atomic_list_concat([PF, PredSp, C, EF], Ris2).


/* File writing kb_func.owl */

writefile:-
  
  /* File creation kb_funct.owl*/
  open('kb_funct.owl', write, Stream),
  nl(Stream),

  /* Prefixes writing */
  kb_prefixes(Le),
  foreach(member(K=P,Le), 
    (
      write(Stream, 'Prefix: '), 
      write(Stream, K),
      write(Stream, ': '),
      write(Stream, P),
      write(Stream, '>)\n')
    )
  ),
  write(Stream,'\n'),

  /* Ontology writing */
  findall(PO, (axiom(ontology(Oiri)),prolog2manchester(ontology(Oiri),PO)),Lo),
  foreach(member(Os, Lo), writeln(Stream, Os)), 
  write(Stream,'\n'),

  /* Axiom writing */
  findall(OP,(axiom(Ax),Ax\=ontology(_),prolog2manchester(Ax,OP)),La),
  foreach(member(As,La), writeln(Stream,As)),

  /* Closing parenthesis and ending file writing */
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
  prolog2manchester(TRILLAx,FunctAx),
  convert_explanation(OtherTRILLAxs,OtherFunctAxs).
