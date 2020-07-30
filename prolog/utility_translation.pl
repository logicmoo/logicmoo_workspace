/** <module> utility_translation

This module translates OWL/RDF axioms into TRILL format and 
loads the knowledge base to be queried by TRILL.

The translation form OWL/RDF is based on the Thea OWL library.
Thea OWL library is available under the GNU/GPL license.
http://vangelisv.github.io/thea/

@author Riccardo Zese
@license Artistic License 2.0
@copyright Riccardo Zese
*/

:- module(utility_translation, [load_owl/1, load_owl_from_string/1, expand_all_ns/4, expand_all_ns/5, is_axiom/1]).

:- dynamic trill_input_mode/1.

:- use_module(library(lists),[member/2]).
:- use_module(library(pengines)).

:- use_module(library(sandbox)).

:- discontiguous(valid_axiom/1).
:- discontiguous(axiompred/1).
:- discontiguous(axiom_arguments/2).
:- discontiguous(expand_axiom/4).

/*****************************
  MESSAGES
******************************/
:- multifile prolog:message/1.

prolog:message(under_development) -->
  [ 'NOTE: This function is under development. It may not work properly or may not work at all.' ].



builtin_class('http://www.w3.org/2002/07/owl#Thing').
builtin_class('http://www.w3.org/2002/07/owl#Nothing').
builtin_datatype('http://www.w3.org/2002/07/owl#real').
builtin_datatype('http://www.w3.org/2002/07/owl#rational').
builtin_datatype('http://www.w3.org/2001/XMLSchema#decimal').
builtin_datatype('http://www.w3.org/2001/XMLSchema#integer').
builtin_datatype('http://www.w3.org/2001/XMLSchema#nonNegativeInteger').
builtin_datatype('http://www.w3.org/2001/XMLSchema#nonPositiveInteger').
builtin_datatype('http://www.w3.org/2001/XMLSchema#positiveInteger').
builtin_datatype('http://www.w3.org/2001/XMLSchema#negativeInteger').
builtin_datatype('http://www.w3.org/2001/XMLSchema#long').
builtin_datatype('http://www.w3.org/2001/XMLSchema#int').
builtin_datatype('http://www.w3.org/2001/XMLSchema#short').
builtin_datatype('http://www.w3.org/2001/XMLSchema#byte').
builtin_datatype('http://www.w3.org/2001/XMLSchema#unsignedLong').
builtin_datatype('http://www.w3.org/2001/XMLSchema#unsignedInt').
builtin_datatype('http://www.w3.org/2001/XMLSchema#unsignedShort').
builtin_datatype('http://www.w3.org/2001/XMLSchema#unsignedByte').
builtin_datatype('http://www.w3.org/2001/XMLSchema#double').
builtin_datatype('http://www.w3.org/2001/XMLSchema#float').
builtin_datatype('http://www.w3.org/2001/XMLSchema#string').
builtin_datatype('http://www.w3.org/2001/XMLSchema#normalizedString').
builtin_datatype('http://www.w3.org/2001/XMLSchema#token').
builtin_datatype('http://www.w3.org/2001/XMLSchema#language').
builtin_datatype('http://www.w3.org/2001/XMLSchema#Name').
builtin_datatype('http://www.w3.org/2001/XMLSchema#NCName').
builtin_datatype('http://www.w3.org/2001/XMLSchema#NMTOKEN').
builtin_datatype('http://www.w3.org/2001/XMLSchema#boolean').
builtin_datatype('http://www.w3.org/2001/XMLSchema#hexBinary').
builtin_datatype('http://www.w3.org/2001/XMLSchema#base64Binary').
builtin_datatype('http://www.w3.org/2001/XMLSchema#minLength').
builtin_datatype('http://www.w3.org/2001/XMLSchema#maxLength').
builtin_datatype('http://www.w3.org/2001/XMLSchema#length').
builtin_datatype('http://www.w3.org/2001/XMLSchema#dateTime').
builtin_datatype('http://www.w3.org/2001/XMLSchema#dateTimeStamp').
builtin_datatype('http://www.w3.org/2000/01/rdf-schema#Literal').

is_class(C) :- get_module(M),M:class(C).
is_class(C) :- builtin_class(C).

/****************************************
  UTILITY
  ****************************************/
set_trdf(Setting,Value):-
  get_module(M),
  retractall(M:trdf_setting(Setting,_)),
  assert(M:trdf_setting(Setting,Value)).

% TODO: hasKey

/****************************************
  AXIOMS
  ****************************************/

%% entity(:IRI)
% the fundamental building blocks of owl 2 ontologies, and they define the vocabulary (the named terms) of an ontology
%
% @see individual/1, property/1, class/1, datatype/1
:- meta_predicate entity(:).

entity(M:A) :- individual(M:A).
entity(M:A) :- property(M:A).
entity(M:A) :- M:class(A).
entity(M:A) :- M:datatype(A).
axiom_arguments(entity,[iri]).
valid_axiom(entity(A)) :- subsumed_by([A],[iri]).

% declarationAxiom(M:individual(A)) :- individual(M:A).
declarationAxiom(M:namedIndividual(A)) :- M:namedIndividual(A).
declarationAxiom(M:objectProperty(A)) :- M:objectProperty(A).
declarationAxiom(M:dataProperty(A)) :- M:dataProperty(A).
declarationAxiom(M:annotationProperty(A)) :- M:annotationProperty(A).
declarationAxiom(M:class(A)) :- M:class(A).
declarationAxiom(M:datatype(A)) :- M:datatype(A).
% TODO: check. here we treat the ontology declaration as an axiom;
% this liberal definition of axiom allows us to iterate over axiom/1
% to find every piece of information in the ontology.
declarationAxiom(M:ontology(A)) :- M:ontology(A).

%% class(?IRI)
% Classes can be understood as sets of individuals
% :- thread_local(class/1).

axiompred(class/1).
axiom_arguments(class,[iri]).

expand_class(M,C,NSList,ExpC) :- 
  expand_iri(M,C,NSList,ExpC),
  \+ builtin_datatype(ExpC).

valid_axiom(class(A)) :- subsumed_by([A],[iri]).
expand_axiom(M,class(A),NSList,class(A_full_URL)) :- 
  expand_iri(M,A,NSList,A_full_URL),
  ( M:addKBName -> add_kb_atoms(M,class,[A_full_URL]) ; true).

%% datatype(?IRI)
% Datatypes are entities that refer to sets of values described by a datatype map
% :- thread_local(datatype/1).

axiompred(datatype/1).
axiom_arguments(datatype,[iri]).
valid_axiom(datatype(A)) :- subsumed_by([A],[iri]).
expand_axiom(M,datatype(A),NSList,datatype(A_full_URL)) :- 
  expand_iri(M,A,NSList,A_full_URL),
  \+ name(A_full_URL,[95, 58, 68, 101, 115, 99, 114, 105, 112, 116, 105, 111, 110|_]),
  ( M:addKBName -> add_kb_atoms(M,datatype,[A_full_URL]) ; true).

%% property(?IRI)
% Properties connect individuals with either other individuals or with literals
%
% @see dataProperty/1, objectProperty/1, annotationProperty/1
:- meta_predicate property(:).

property(M:A) :- M:dataProperty(A).
property(M:A) :- M:objectProperty(A).
property(M:A) :- M:annotationProperty(A).
axiom_arguments(property,[iri]).
valid_axiom(property(A)) :- subsumed_by([A],[iri]).

%% objectProperty(?IRI)
% Object properties connect pairs of individuals
%:- thread_local(objectProperty/1).

axiompred(objectProperty/1).
axiom_arguments(objectProperty,[iri]).

expand_objectProperty(M,P,NSList,ExpP) :- 
  expand_iri(M,P,NSList,ExpP),
  ( M:addKBName -> add_kb_atoms(M,objectProperty,[ExpP]) ; true ).

valid_axiom(objectProperty(A)) :- subsumed_by([A],[iri]).
expand_axiom(M,objectProperty(A),NSList,objectProperty(A_full_URL)) :- 
  expand_iri(M,A,NSList,A_full_URL),
  ( M:addKBName -> add_kb_atoms(M,objectProperty,[A_full_URL]) ; true).

%% dataProperty(?IRI)
% Data properties connect individuals with literals. In some knowledge representation systems, functional data properties are called attributes.
%:- thread_local(dataProperty/1).

axiompred(dataProperty/1).
axiom_arguments(dataProperty,[iri]).

expand_dataProperty(M,P,NSList,ExpP) :- 
  expand_iri(M,P,NSList,ExpP),
  ( M:addKBName -> add_kb_atoms(M,dataProperty,[ExpP]) ; true).


valid_axiom(dataProperty(A)) :- subsumed_by([A],[iri]).
expand_axiom(M,dataProperty(A),NSList,dataProperty(A_full_URL)) :- 
  expand_iri(M,A,NSList,A_full_URL),
  ( M:addKBName -> add_kb_atoms(M,dataProperty,[A_full_URL]) ; true).

%% annotationProperty(?IRI)
% Annotation properties can be used to provide an annotation for an ontology, axiom, or an IRI
%:- thread_local(annotationProperty/1).

axiompred(annotationProperty/1).
axiom_arguments(annotationProperty,[iri]).

expand_annotationProperty(M,P,NSList,ExpP) :- 
  expand_iri(M,P,NSList,ExpP),
  ( M:addKBName -> add_kb_atoms(M,annotationProperty,[ExpP]) ; true ).

expand_annotationSubject(M,P,NSList,ExpP) :- 
  (expand_classExpression(M,P,NSList,ExpP),!) ;
  (expand_individual(M,P,NSList,ExpP),!) ;
  (expand_propertyExpression(M,P,NSList,ExpP),!) ;
  (expand_axiom(M,P,NSList,ExpP),!).

expand_annotationValue(M,P,NSList,ExpP) :- 
  (expand_literal(M,P,NSList,ExpP),!) ;
  (expand_classExpression(M,P,NSList,ExpP),!) ;
  (expand_individual(M,P,NSList,ExpP),!) ;
  (expand_propertyExpression(M,P,NSList,ExpP),!) ;
  (expand_axiom(M,P,NSList,ExpP),!) .


valid_axiom(annotationProperty(A)) :- subsumed_by([A],[iri]).
expand_axiom(M,annotationProperty(A),NSList,annotationProperty(A_full_URL)) :- 
  expand_iri(M,A,NSList,A_full_URL),
  ( M:addKBName -> add_kb_atoms(M,annotationProperty,[A_full_URL]) ; true).

expand_axiom(M,annotation(A,B,C),NSList,annotation(A_full_URL,B_full_URL,C_full_URL)) :-
  ( M:addKBName -> (retractall(M:addKBName), Add=true) ; Add=false ),
  expand_argument(M,A,NSList,A_full_URL),
  expand_argument(M,B,NSList,B_full_URL),
  expand_argument(M,C,NSList,C_full_URL),
  ( Add=true -> assert(M:addKBName) ; true ).


%% individual(:IRI)
% Individuals represent actual objects from the domain being modeled
% @see anonymousIndividual/1, namedIndividual/1
:- meta_predicate individual(:).

individual(M:A) :- M:anonymousIndividual(A).
individual(M:A) :- M:namedIndividual(A).
%individual(A) :- nonvar(A),iri(A),\+property(A),\+class(A),\+ontology(A). % TODO: check: make individuals the default
axiom_arguments(individual,[iri]).
valid_axiom(individual(A)) :- subsumed_by([A],[iri]).

expand_individuals(_M,[],_NSList,[]) :- !.
expand_individuals(M,[H|T],NSList,[ExpH|ExpT]) :-
  expand_individual(M,H,NSList,ExpH),
  expand_individuals(M,T,NSList,ExpT).

expand_individual(M,I,NSList,ExpI) :- 
  expand_iri(M,I,NSList,ExpI),
  \+ builtin_datatype(ExpI),
  ( M:addKBName -> add_kb_atoms(M,individual,[ExpI]) ; true ).

%% namedIndividual(?IRI)
% Named individuals are given an explicit name that can be used in any ontology in the import closure to refer to the same individual
%:- thread_local(namedIndividual/1).

axiompred(namedIndividual/1).
axiom_arguments(namedIndividual,[iri]).
valid_axiom(namedIndividual(A)) :- subsumed_by([A],[iri]).
expand_axiom(M,namedIndividual(A),NSList,namedIndividual(A_full_URL)) :- 
  expand_iri(M,A,NSList,A_full_URL),
  ( M:addKBName -> add_kb_atoms(M,individual,[A_full_URL]) ; true).

%% anonymousIndividual(?IRI)
% Anonymous individuals are local to the ontology they are contained in. Analagous to bnodes
% @see construct/1
%:- thread_local(anonymousIndividual/1).

axiompred(anonymousIndividual/1).
axiom_arguments(anonymousIndividual,[iri]).
valid_axiom(anonymousIndividual(A)) :- subsumed_by([A],[iri]).
expand_axiom(M,anonymousIndividual(A),NSList,anonymousIndividual(A_full_URL)) :- 
  expand_iri(M,A,NSList,A_full_URL),
  ( M:addKBName -> add_kb_atoms(M,individual,[A_full_URL]) ; true).

%% construct(:IRI)
% @see axiom/1, annotation/1, ontology/1
:- meta_predicate costruct(:).

construct(M:A) :- trill:axiom(M:A).
construct(M:A) :- annotation(M:A).
construct(M:A) :- M:ontology(A).
axiom_arguments(construct,[iri]).
valid_axiom(construct(A)) :- subsumed_by([A],[iri]).

%% axiom(:Axiom)
% The main component of an OWL 2 ontology is a set of axioms - statements that say what is true in the domain being modeled.
% @see classAxiom/1, propertyAxiom/1, fact/1
:- multifile trill:axiom/1.

trill:axiom(M:A) :- classAxiom(M:A).
trill:axiom(M:A) :- propertyAxiom(M:A).
trill:axiom(M:hasKey(A,B)) :- M:hasKey(A,B).
trill:axiom(M:A) :- fact(M:A).
trill:axiom(M:A) :- declarationAxiom(M:A).
%axiom(annotation(A,B,C)) :-
%	annotation(A,B,C). % CJM-treat annotations as axioms
axiom_arguments(axiom,[axiom]).
valid_axiom(axiom(A)) :- subsumed_by([A],[axiom]).

%% classAxiom(:Axiom)
% OWL 2 provides axioms that allow relationships to be established between class expressions. This predicate reifies the actual axiom
% @see equivalentClasses/1, disjointClasses/1, subClassOf/2, disjointUnion/2
:- meta_predicate classAxiom(:).

classAxiom(M:equivalentClasses(A)) :- M:equivalentClasses(A).
classAxiom(M:disjointClasses(A)) :- M:disjointClasses(A).
classAxiom(M:subClassOf(A, B)) :- M:subClassOf(A, B).
classAxiom(M:disjointUnion(A, B)) :- M:disjointUnion(A, B).
axiom_arguments(classAxiom,[axiom]).
valid_axiom(classAxiom(A)) :- subsumed_by([A],[axiom]).

%% subClassOf(?SubClass:ClassExpression, ?SuperClass:ClassExpression)
% A subclass axiom SubClassOf( CE1 CE2 ) states that the class expression CE1 is a subclass of the class expression CE2
%
%   @param SubClass a classExpression/1 representing the more specific class
%   @param SuperClass a classExpression/1 representing the more general class
%:- thread_local(subClassOf/2).

axiompred(subClassOf/2).
axiom_arguments(subClassOf,[classExpression, classExpression]).
valid_axiom(subClassOf(A, B)) :- subsumed_by([A, B],[classExpression, classExpression]).
expand_axiom(M,subClassOf(A,B),NSList,subClassOf(A_full_URL,B_full_URL)) :- 
  expand_classExpression(M,A,NSList,A_full_URL),
  expand_classExpression(M,B,NSList,B_full_URL).


%% equivalentClasses(?ClassExpressions:set(ClassExpression))
% An equivalent classes axiom EquivalentClasses( CE1 ... CEn ) states that all of the class expressions CEi, 1 <= i <= n, are semantically equivalent to each other.
%:- thread_local(equivalentClasses/1).

axiompred(equivalentClasses/1).
axiom_arguments(equivalentClasses,[set(classExpression)]).
valid_axiom(equivalentClasses(A)) :- subsumed_by([A],[set(classExpression)]).
expand_axiom(M,equivalentClasses(A),NSList,equivalentClasses(A_full_URL)) :- 
  expand_classExpressions(M,A,NSList,A_full_URL).

%% disjointClasses(?ClassExpressions:set(ClassExpression))
% A disjoint classes axiom DisjointClasses( CE1 ... CEn ) states that all of the class expressions CEi, 1 <= i <= n, are pairwise disjoint; that is, no individual can be at the same time an instance of both CEi and CEj for i != j
%:- thread_local(disjointClasses/1).

axiompred(disjointClasses/1).
axiom_arguments(disjointClasses,[set(classExpression)]).
valid_axiom(disjointClasses(A)) :- subsumed_by([A],[set(classExpression)]).
expand_axiom(M,disjointClasses(A),NSList,disjointClasses(A_full_URL)) :- 
  expand_classExpressions(M,A,NSList,A_full_URL).

%% disjointUnion(?ClassExpression, ?ClassExpressions:set(ClassExpression))
% A disjoint union axiom DisjointUnion( C CE1 ... CEn ) states that a class C is a disjoint union of the class expressions CEi, 1 <= i <= n, all of which are pairwise disjoint.
%:- thread_local(disjointUnion/2).

axiompred(disjointUnion/2).
axiom_arguments(disjointUnion,[classExpression,set(classExpression)]).
valid_axiom(disjointUnion(A,B)) :- subsumed_by([A,B],[classExpression,set(classExpression)]).
expand_axiom(M,disjointUnion(A,B),NSList,disjointUnion(A_full_URL,B_full_URL)) :- 
  expand_classExpression(M,A,NSList,A_full_URL),
  expand_classExpressions(M,B,NSList,B_full_URL).

%% propertyAxiom(:Axiom)
% OWL 2 provides axioms that can be used to characterize and establish relationships between object property expressions. This predicate reifies the actual axiom
%
% @see symmetricProperty/1, inverseFunctionalProperty/1, transitiveProperty/1, asymmetricProperty/1, subPropertyOf/2, functionalProperty/1, irreflexiveProperty/1, disjointProperties/1, propertyDomain/2, reflexiveProperty/1, propertyRange/2, equivalentProperties/1, inverseProperties/2
:- meta_predicate propertyAxiom(:).

propertyAxiom(M:symmetricProperty(A)) :- M:symmetricProperty(A).
propertyAxiom(M:inverseFunctionalProperty(A)) :- M:inverseFunctionalProperty(A).
propertyAxiom(M:transitiveProperty(A)) :- M:transitiveProperty(A).
propertyAxiom(M:asymmetricProperty(A)) :- M:asymmetricProperty(A).
propertyAxiom(M:subPropertyOf(A, B)) :- M:subPropertyOf(A, B).
propertyAxiom(M:functionalProperty(A)) :- M:functionalProperty(A).
propertyAxiom(M:irreflexiveProperty(A)) :- M:irreflexiveProperty(A).
propertyAxiom(M:disjointProperties(A)) :- M:disjointProperties(A).
propertyAxiom(M:propertyDomain(A, B)) :- M:propertyDomain(A, B).
propertyAxiom(M:reflexiveProperty(A)) :- M:reflexiveProperty(A).
propertyAxiom(M:propertyRange(A, B)) :- M:propertyRange(A, B).
propertyAxiom(M:equivalentProperties(A)) :- M:equivalentProperties(A).
propertyAxiom(M:inverseProperties(A, B)) :- M:inverseProperties(A, B).
axiom_arguments(propertyAxiom,[axiom]).
valid_axiom(propertyAxiom(A)) :- subsumed_by([A],[axiom]).


%% subPropertyOf(?Sub:PropertyExpression, ?Super:ObjectPropertyExpression)
% subproperty axioms are analogous to subclass axioms
% (extensional predicate - can be asserted)
%:- thread_local(subPropertyOf/2).

axiompred(subPropertyOf/2).
axiom_arguments(subPropertyOf,[propertyExpression, objectPropertyExpression]).
valid_axiom(subPropertyOf(A, B)) :- subsumed_by([A, B],[propertyExpression, objectPropertyExpression]).
%expand_axiom(M,subPropertyOf(A,B),NSList,subPropertyOf(A_full_URL,B_full_URL)) :- %TODO: fix for data properties
%  expand_propertyExpression(M,A,NSList,A_full_URL),
%  expand_objectPropertyExpression(M,B,NSList,B_full_URL).

%% subObjectPropertyOf(?Sub:ObjectPropertyExpressionOrChain, ?Super:ObjectPropertyExpression)
% The basic form is SubPropertyOf( OPE1 OPE2 ). This axiom states that the object property expression OPE1 is a subproperty of the object property expression OPE2 - that is, if an individual x is connected by OPE1 to an individual y, then x is also connected by OPE2 to y. The more complex form is SubPropertyOf( PropertyChain( OPE1 ... OPEn ) OPE ). This axiom states that, if an individual x is connected by a sequence of object property expressions OPE1, ..., OPEn with an individual y, then x is also connected with y by the object property expression OPE
subObjectPropertyOf(A, B) :- get_module(M),M:subPropertyOf(A, B),subsumed_by([A, B],[objectPropertyExpressionOrChain, objectPropertyExpression]).
axiom_arguments(subObjectPropertyOf,[objectPropertyExpressionOrChain, objectPropertyExpression]).
valid_axiom(subObjectPropertyOf(A, B)) :- subsumed_by([A, B],[objectPropertyExpressionOrChain, objectPropertyExpression]).
expand_axiom(M,subPropertyOf(A,B),NSList,subPropertyOf(A_full_URL,B_full_URL)) :- 
  expand_objectPropertyExpressionOrChain(M,A,NSList,A_full_URL),
  expand_objectPropertyExpression(M,B,NSList,B_full_URL).
  %add_expressivity(M,h).

%% subDataPropertyOf(?Sub:DataPropertyExpression, ?Super:DataPropertyExpression)
% A data subproperty axiom SubPropertyOf( DPE1 DPE2 ) states that the data property expression DPE1 is a subproperty of the data property expression DPE2 - that is, if an individual x is connected by OPE1 to a literal y, then x is connected by OPE2 to y as well.
subDataPropertyOf(A, B) :- get_module(M),M:subPropertyOf(A, B),subsumed_by([A, B],[dataPropertyExpression, dataPropertyExpression]).
axiom_arguments(subDataPropertyOf,[dataPropertyExpression, dataPropertyExpression]).
valid_axiom(subDataPropertyOf(A, B)) :- subsumed_by([A, B],[dataPropertyExpression, dataPropertyExpression]).

%% subAnnotationPropertyOf(?Sub:AnnotationProperty, ?Super:AnnotationProperty)
% An annotation subproperty axiom SubPropertyOf( AP1 AP2 ) states that the annotation property AP1 is a subproperty of the annotation property AP2
subAnnotationPropertyOf(A, B) :- get_module(M),M:subPropertyOf(A, B),subsumed_by([A, B],[annotationProperty, annotationProperty]).
axiom_arguments(subAnnotationPropertyOf,[annotationProperty, annotationProperty]).
valid_axiom(subAnnotationPropertyOf(A, B)) :- subsumed_by([A, B],[annotationProperty, annotationProperty]).

%% equivalentProperties(?PropertyExpressions:set(PropertyExpression))
% An equivalent object properties axiom EquivalentProperties( OPE1 ... OPEn ) states that all of the object property expressions OPEi, 1 <= i <= n, are semantically equivalent to each other
% (extensional predicate - can be asserted)
%:- thread_local(equivalentProperties/1).

axiompred(equivalentProperties/1).
axiom_arguments(equivalentProperties,[set(propertyExpression)]).
valid_axiom(equivalentProperties(A)) :- subsumed_by([A],[set(propertyExpression)]).
expand_axiom(M,equivalentProperties(A),NSList,equivalentProperties(A_full_URL)) :- 
  expand_propertyExpressions(M,A,NSList,A_full_URL).

%% equivalentObjectProperties(?PropertyExpressions:set(ObjectPropertyExpression))
% An equivalent object properties axiom EquivalentObjectProperties( OPE1 ... OPEn ) states that all of the object property expressions OPEi, 1 <= i <= n, are semantically equivalent to each other
equivalentObjectProperties(A) :- get_module(M),M:equivalentProperties(A),subsumed_by([A],[set(objectPropertyExpression)]).
axiom_arguments(equivalentObjectProperties,[set(objectPropertyExpression)]).
valid_axiom(equivalentObjectProperties(A)) :- subsumed_by([A],[set(objectPropertyExpression)]).

%% equivalentDataProperties(?PropertyExpressions:set(DataPropertyExpression))
% An equivalent data properties axiom EquivalentProperties( DPE1 ... DPEn ) states that all the data property expressions DPEi, 1 <= i <= n, are semantically equivalent to each other. This axiom allows one to use each DPEi as a synonym for each DPEj - that is, in any expression in the ontology containing such an axiom, DPEi can be replaced with DPEj without affecting the meaning of the ontology
equivalentDataProperties(A) :- get_module(M),M:equivalentProperties(A),subsumed_by([A],[set(dataPropertyExpression)]).
axiom_arguments(equivalentDataProperties,[set(dataPropertyExpression)]).
valid_axiom(equivalentDataProperties(A)) :- subsumed_by([A],[set(dataPropertyExpression)]).

%% disjointProperties(?PropertyExpressions:set(PropertyExpression))
% A disjoint properties axiom DisjointProperties( PE1 ... PEn ) states that all of the property expressions PEi, 1 <= i <= n, are pairwise disjoint
% (extensional predicate - can be asserted)
%:- thread_local(disjointProperties/1).

axiompred(disjointProperties/1).
axiom_arguments(disjointProperties,[set(propertyExpression)]).
valid_axiom(disjointProperties(A)) :- subsumed_by([A],[set(propertyExpression)]).
expand_axiom(M,disjointProperties(A),NSList,disjointProperties(A_full_URL)) :- 
  expand_propertyExpressions(M,A,NSList,A_full_URL).

%% disjointObjectProperties(?PropertyExpressions:set(ObjectPropertyExpression))
% A disjoint object properties axiom DisjointProperties( OPE1 ... OPEn ) states that all of the object property expressions OPEi, 1 <= i <= n, are pairwise disjoint; that is, no individual x can be connected to an individual y by both OPEi and OPEj for i != j.
disjointObjectProperties(A) :- get_module(M),M:disjointProperties(A),subsumed_by([A],[set(objectPropertyExpression)]).
axiom_arguments(disjointObjectProperties,[set(objectPropertyExpression)]).
valid_axiom(disjointObjectProperties(A)) :- subsumed_by([A],[set(objectPropertyExpression)]).

%% disjointDataProperties(?PropertyExpressions:set(DataPropertyExpression))
% A disjoint data properties axiom DisjointProperties( DPE1 ... DPEn ) states that all of the data property expressions DPEi, 1 <= i <= n, are pairwise disjoint; that is, no individual x can be connected to a literal y by both DPEi and DPEj for i !- j.
disjointDataProperties(A) :- get_module(M),M:disjointProperties(A),subsumed_by([A],[set(dataPropertyExpression)]).
axiom_arguments(disjointDataProperties,[set(dataPropertyExpression)]).
valid_axiom(disjointDataProperties(A)) :- subsumed_by([A],[set(dataPropertyExpression)]).

%% inverseProperties(?ObjectPropertyExpression1:ObjectPropertyExpression, ?ObjectPropertyExpression2:ObjectPropertyExpression)
% An inverse object properties axiom InverseProperties( OPE1 OPE2 ) states that the object property expression OPE1 is an inverse of the object property expression OPE2
% (note there are no inverse data properties, as literals are not connected to individuals)
% Example:
% =|inverseProperties(partOf,hasPart)|=
% (extensional predicate - can be asserted)
%:- thread_local(inverseProperties/2).

axiompred(inverseProperties/2).
axiom_arguments(inverseProperties,[objectPropertyExpression, objectPropertyExpression]).
valid_axiom(inverseProperties(A, B)) :- subsumed_by([A, B],[objectPropertyExpression, objectPropertyExpression]).
expand_axiom(M,inverseProperties(A,B),NSList,inverseProperties(A_full_URL,B_full_URL)) :- 
  expand_objectPropertyExpression(M,A,NSList,A_full_URL),
  expand_objectPropertyExpression(M,B,NSList,B_full_URL).
  %add_expressivity(M,i).

%% propertyDomain(?PropertyExpression, ?CE)
%  A property domain axiom PropertyDomain( PE CE ) states that the
%  domain of the property expression PE is CE
% (extensional predicate - can be asserted)

%:- thread_local(propertyDomain/2).

axiompred(propertyDomain/2).
axiom_arguments(propertyDomain,[propertyExpression, classExpression]).
valid_axiom(propertyDomain(A, B)) :- subsumed_by([A, B],[propertyExpression, classExpression]).
expand_axiom(M,propertyDomain(A,B),NSList,propertyDomain(A_full_URL,B_full_URL)) :- 
  expand_propertyExpression(M,A,NSList,A_full_URL),
  expand_classExpression(M,B,NSList,B_full_URL).

%% objectPropertyDomain(?ObjectPropertyExpression, ?ClassExpression)
% An object property domain axiom PropertyDomain( OPE CE ) states that the domain of the object property expression OPE is the class expression CE - that is, if an individual x is connected by OPE with some other individual, then x is an instance of CE
objectPropertyDomain(A, B) :- get_module(M),M:propertyDomain(A, B),subsumed_by([A, B],[objectPropertyExpression, classExpression]).
axiom_arguments(objectPropertyDomain,[objectPropertyExpression, classExpression]).
valid_axiom(objectPropertyDomain(A, B)) :- subsumed_by([A, B],[objectPropertyExpression, classExpression]).

%% dataPropertyDomain(?DataPropertyExpression, ?ClassExpression)
% A data property domain axiom PropertyDomain( DPE CE ) states that the domain of the data property expression DPE is the class expression CE - that is, if an individual x is connected by DPE with some literal, then x is an instance of CE
dataPropertyDomain(A, B) :- get_module(M),M:propertyDomain(A, B),subsumed_by([A, B],[dataPropertyExpression, classExpression]).
axiom_arguments(dataPropertyDomain,[dataPropertyExpression, classExpression]).
valid_axiom(dataPropertyDomain(A, B)) :- subsumed_by([A, B],[dataPropertyExpression, classExpression]).

%% annotationPropertyDomain(?AnnotationProperty, ?IRI)
% An annotation property domain axiom PropertyDomain( AP U ) states that the domain of the annotation property AP is the IRI U. Such axioms have no effect on the Direct Semantics of OWL 2
annotationPropertyDomain(A, B) :- get_module(M),M:propertyDomain(A, B),subsumed_by([A, B],[annotationProperty, iri]).
axiom_arguments(annotationPropertyDomain,[annotationProperty, iri]).
valid_axiom(annotationPropertyDomain(A, B)) :- subsumed_by([A, B],[annotationProperty, iri]).

%% propertyRange(?PropertyExpression, ?ClassExpression)
% An object property domain axiom PropertyRange( OPE CE ) states that the domain of the object property expression OPE is the class expression CE - that is, if an individual x is connected by OPE with some other individual, then x is an instance of CE
% (extensional predicate - can be asserted)
%:- thread_local(propertyRange/2).

axiompred(propertyRange/2).
axiom_arguments(propertyRange,[propertyExpression, classExpression]).
valid_axiom(propertyRange(A, B)) :- subsumed_by([A, B],[propertyExpression, classExpression]).
expand_axiom(M,propertyRange(A,B),NSList,propertyRange(A_full_URL,B_full_URL)) :- 
  expand_iri(M,B,NSList,Datatype),
  builtin_datatype(Datatype),!,
  expand_dataRange(M,B,NSList,B_full_URL),
  expand_dataPropertyExpression(M,A,NSList,A_full_URL).
expand_axiom(M,propertyRange(A,B),NSList,propertyRange(A_full_URL,B_full_URL)) :- 
  expand_propertyExpression(M,A,NSList,A_full_URL),
  expand_classExpression(M,B,NSList,B_full_URL).

%% objectPropertyRange(?ObjectPropertyExpression, ?ClassExpression)
% An object property domain axiom PropertyRange( OPE CE ) states that the domain of the object property expression OPE is the class expression CE - that is, if an individual x is connected by OPE with some other individual, then x is an instance of CE
objectPropertyRange(A, B) :- propertyRange(A, B),subsumed_by([A, B],[objectPropertyExpression, classExpression]).
axiom_arguments(objectPropertyRange,[objectPropertyExpression, classExpression]).
valid_axiom(objectPropertyRange(A, B)) :- subsumed_by([A, B],[objectPropertyExpression, classExpression]).

%% dataPropertyRange(?ObjectPropertyExpression, ?DataRange)
% A data property range axiom PropertyRange( DPE DR ) states that the range of the data property expression DPE is the data range DR - that is, if some individual is connected by DPE with a literal x, then x is in DR. The arity of DR MUST be one
dataPropertyRange(A, B) :- get_module(M),M:propertyRange(A, B),subsumed_by([A, B],[dataPropertyExpression, dataRange]).
axiom_arguments(dataPropertyRange,[objectPropertyExpression, dataRange]).
valid_axiom(dataPropertyRange(A, B)) :- subsumed_by([A, B],[objectPropertyExpression, dataRange]).

%% annotationPropertyRange(?AnnotationProperty, ?IRI)
% An annotation property range axiom PropertyRange( AP U ) states that the range of the annotation property AP is the IRI U. Such axioms have no effect on the Direct Semantics of OWL 2
annotationPropertyRange(A, B) :- get_module(M),M:propertyRange(A, B),subsumed_by([A, B],[annotationProperty, iri]).
axiom_arguments(annotationPropertyRange,[annotationProperty, iri]).
valid_axiom(annotationPropertyRange(A, B)) :- subsumed_by([A, B],[annotationProperty, iri]).

%% functionalProperty(?PropertyExpression)
% An object property functionality axiom FunctionalProperty( OPE ) states that the object property expression OPE is functional - that is, for each individual x, there can be at most one distinct individual y such that x is connected by OPE to y
% (extensional predicate - can be asserted)
%:- thread_local(functionalProperty/1).

axiompred(functionalProperty/1).
axiom_arguments(functionalProperty,[propertyExpression]).
valid_axiom(functionalProperty(A)) :- subsumed_by([A],[propertyExpression]).
expand_axiom(M,functionalProperty(A),NSList,functionalProperty(A_full_URL)) :- 
  expand_propertyExpression(M,A,NSList,A_full_URL).
  %add_expressivity(M,f).

%% functionalObjectProperty(?ObjectPropertyExpression)
% An object property functionality axiom FunctionalProperty( OPE ) states that the object property expression OPE is functional - that is, for each individual x, there can be at most one distinct individual y such that x is connected by OPE to y
functionalObjectProperty(A) :- get_module(M),M:functionalProperty(A),subsumed_by([A],[objectPropertyExpression]).
axiom_arguments(functionalObjectProperty,[objectPropertyExpression]).
valid_axiom(functionalObjectProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).

%% functionalDataProperty(?DataPropertyExpression)
% A data property functionality axiom FunctionalProperty( DPE ) states that the data property expression DPE is functional - that is, for each individual x, there can be at most one distinct literal y such that x is connected by DPE with y
functionalDataProperty(A) :- get_module(M),M:functionalProperty(A),subsumed_by([A],[dataPropertyExpression]).
axiom_arguments(functionalDataProperty,[dataPropertyExpression]).
valid_axiom(functionalDataProperty(A)) :- subsumed_by([A],[dataPropertyExpression]).

%% inverseFunctionalProperty(?ObjectPropertyExpression)
% An object property inverse functionality axiom InverseFunctionalProperty( OPE ) states that the object property expression OPE is inverse-functional - that is, for each individual x, there can be at most one individual y such that y is connected by OPE with x. Note there are no InverseFunctional DataProperties
%:- thread_local(inverseFunctionalProperty/1).

axiompred(inverseFunctionalProperty/1).
axiom_arguments(inverseFunctionalProperty,[objectPropertyExpression]).
valid_axiom(inverseFunctionalProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).
expand_axiom(M,inverseFunctionalProperty(A),NSList,inverseFunctionalProperty(A_full_URL)) :- 
  expand_objectPropertyExpression(M,A,NSList,A_full_URL).
  %add_expressivity(M,i),
  %add_expressivity(M,f).

%% reflexiveProperty(?ObjectPropertyExpression)
% An object property reflexivity axiom ReflexiveProperty( OPE ) states that the object property expression OPE is reflexive - that is, each individual is connected by OPE to itself
%:- thread_local(reflexiveProperty/1).

axiompred(reflexiveProperty/1).
axiom_arguments(reflexiveProperty,[objectPropertyExpression]).
valid_axiom(reflexiveProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).
expand_axiom(M,reflexiveProperty(A),NSList,reflexiveProperty(A_full_URL)) :- 
  expand_objectPropertyExpression(M,A,NSList,A_full_URL).

%% irreflexiveProperty(?ObjectPropertyExpression)
% An object property reflexivity axiom ReflexiveProperty( OPE ) states that the object property expression OPE is reflexive - that is, no individual is connected by OPE to itsel
%:- thread_local(irreflexiveProperty/1).

axiompred(irreflexiveProperty/1).
axiom_arguments(irreflexiveProperty,[objectPropertyExpression]).
valid_axiom(irreflexiveProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).
expand_axiom(M,irreflexiveProperty(A),NSList,irreflexiveProperty(A_full_URL)) :- 
  expand_objectPropertyExpression(M,A,NSList,A_full_URL).

%% symmetricProperty(?ObjectPropertyExpression)
% An object property symmetry axiom SymmetricProperty( OPE ) states that the object property expression OPE is symmetric - that is, if an individual x is connected by OPE to an individual y, then y is also connected by OPE to x
%:- thread_local(symmetricProperty/1).

axiompred(symmetricProperty/1).
axiom_arguments(symmetricProperty,[objectPropertyExpression]).
valid_axiom(symmetricProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).
expand_axiom(M,symmetricProperty(A),NSList,symmetricProperty(A_full_URL)) :- 
  expand_objectPropertyExpression(M,A,NSList,A_full_URL).

%% asymmetricProperty(?ObjectPropertyExpression)
% An object property asymmetry axiom AsymmetricProperty( OPE ) states that the object property expression OPE is asymmetric - that is, if an individual x is connected by OPE to an individual y, then y cannot be connected by OPE to x
%:- thread_local(asymmetricProperty/1).

axiompred(asymmetricProperty/1).
axiom_arguments(asymmetricProperty,[objectPropertyExpression]).
valid_axiom(asymmetricProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).
expand_axiom(M,asymmetricProperty(A),NSList,asymmetricProperty(A_full_URL)) :- 
  expand_objectPropertyExpression(M,A,NSList,A_full_URL).

%% transitiveProperty(?ObjectPropertyExpression)
% An object property transitivity axiom TransitiveProperty( OPE ) states that the object property expression OPE is transitive - that is, if an individual x is connected by OPE to an individual y that is connected by OPE to an individual z, then x is also connected by OPE to z
%:- thread_local(transitiveProperty/1).

axiompred(transitiveProperty/1).
axiom_arguments(transitiveProperty,[objectPropertyExpression]).
valid_axiom(transitiveProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).
expand_axiom(M,transitiveProperty(A),NSList,transitiveProperty(A_full_URL)) :- 
  expand_objectPropertyExpression(M,A,NSList,A_full_URL).
  %add_rule(M,forall_plus_rule),
  %add_expressivity(M,s).

%% hasKey(?ClassExpression,?PropertyExpression)
% A key axiom HasKey( CE PE1 ... PEn ) states that each (named) instance of the class expression CE is uniquely identified by the (data or object) property expressions PEi - that is, no two distinct (named) instances of CE can coincide on the values of all property expressions PEi
%:- thread_local(hasKey/2).

axiompred(hasKey/2).
axiom_arguments(hasKey,[classExpression,propertyExpression]).
valid_axiom(hasKey(CE,PE)) :- subsumed_by([CE,PE],[classExpression,propertyExpression]).
expand_axiom(M,hasKey(A,B),NSList,hasKey(A_full_URL,B_full_URL)) :- 
  expand_classExpression(M,A,NSList,A_full_URL),
  expand_propertyExpression(M,B,NSList,B_full_URL).


%% fact(:Axiom)
% OWL 2 supports a rich set of axioms for stating assertions - axioms about individuals that are often also called facts. The fact/1 predicate reifies the fact predicate
%
% @see annotationAssertion/3, differentIndividuals/1, negativePropertyAssertion/3, propertyAssertion/3, sameIndividual/1, classAssertion/2
:- meta_predicate fact(:).

fact(M:annotationAssertion(A, B, C)) :- M:annotationAssertion(A, B, C).
fact(M:differentIndividuals(A)) :- M:differentIndividuals(A).
fact(M:negativePropertyAssertion(A, B, C)) :- M:negativePropertyAssertion(A, B, C).
fact(M:propertyAssertion(A, B, C)) :- M:propertyAssertion(A, B, C).
fact(M:sameIndividual(A)) :- M:sameIndividual(A).
fact(M:classAssertion(A, B)) :- M:classAssertion(A, B).
axiom_arguments(fact,[axiom]).
valid_axiom(fact(A)) :- subsumed_by([A],[axiom]).

%% sameIndividual(?Individuals:set(Individual))
% An individual equality axiom SameIndividual( a1 ... an ) states that all of the individuals ai, 1 <= i <= n, are equal to each other.
% note that despite the name of this predicate, it accepts a list of individuals as argument
%:- thread_local(sameIndividual/1).

axiompred(sameIndividual/1).
axiom_arguments(sameIndividual,[set(individual)]).
valid_axiom(sameIndividual(A)) :- subsumed_by([A],[set(individual)]).
expand_axiom(M,sameIndividual(A),NSList,sameIndividual(A_full_URL)) :- 
  expand_individuals(M,A,NSList,A_full_URL).

%% differentIndividuals(?Individuals:set(Individual))
% An individual inequality axiom DifferentIndividuals( a1 ... an ) states that all of the individuals ai, 1 <= i <= n, are different from each other
%:- thread_local(differentIndividuals/1).

axiompred(differentIndividuals/1).
axiom_arguments(differentIndividuals,[set(individual)]).
valid_axiom(differentIndividuals(A)) :- subsumed_by([A],[set(individual)]).
expand_axiom(M,differentIndividuals(A),NSList,differentIndividuals(A_full_URL)) :- 
  expand_individuals(M,A,NSList,A_full_URL).

%% classAssertion(?ClassExpression, ?Individual)
% A class assertion ClassAssertion( CE a ) states that the individual a is an instance of the class expression CE
%:- thread_local(classAssertion/2).

axiompred(classAssertion/2).
axiom_arguments(classAssertion,[classExpression, individual]).
valid_axiom(classAssertion(A, B)) :- subsumed_by([A, B],[classExpression, individual]).
expand_axiom(M,classAssertion(A,B),NSList,B_full_URL) :- 
  expand_iri(M,A,NSList,'http://www.w3.org/2000/01/rdf-schema#Datatype'),!,
  ( expand_axiom(M,datatype(B),NSList,B_full_URL) -> true ; B_full_URL='none' ).
expand_axiom(M,classAssertion(A,B),NSList,classAssertion(A_full_URL,B_full_URL)) :- 
  expand_classExpression(M,A,NSList,A_full_URL),
  expand_individual(M,B,NSList,B_full_URL).

%% propertyAssertion(?PropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)
% A positive object property assertion PropertyAssertion( OPE a1 a2 ) states that the individual a1 is connected by the object property expression OPE to the individual a2
% (extensional predicate - can be asserted)
%:- thread_local(propertyAssertion/3).

axiompred(propertyAssertion/3).
axiom_arguments(propertyAssertion,[propertyExpression, individual, individual]).
valid_axiom(propertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[propertyExpression, individual, individual]).
expand_axiom(M,propertyAssertion(A,B,C),NSList,propertyAssertion(IRI,B_full_URL,C_full_URL)) :- 
  expand_iri(M,A,NSList,IRI),
  ( IRI='http://www.w3.org/2000/01/rdf-schema#label' ; IRI='http://www.w3.org/2000/01/rdf-schema#comment' ),!,
  expand_iri(M,B,NSList,B_full_URL),
  ( expand_iri(M,C,NSList,C_full_URL) ; expand_literal(M,C,NSList,C_full_URL) ), !.
expand_axiom(M,propertyAssertion(A,B,C),NSList,propertyAssertion(A_full_URL,B_full_URL,C_full_URL)) :- 
  expand_individual(M,C,NSList,C_full_URL),!,
  expand_individual(M,B,NSList,B_full_URL),
  expand_objectPropertyExpression(M,A,NSList,A_full_URL).
expand_axiom(M,propertyAssertion(A,B,C),NSList,propertyAssertion(A_full_URL,B_full_URL,C_full_URL)) :- 
  expand_literal(M,C,NSList,C_full_URL),
  expand_individual(M,B,NSList,B_full_URL),
  expand_dataPropertyExpression(M,A,NSList,A_full_URL).


%% objectPropertyAssertion(?ObjectPropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)
% A positive object property assertion PropertyAssertion( OPE a1 a2 ) states that the individual a1 is connected by the object property expression OPE to the individual a2
objectPropertyAssertion(A, B, C) :- get_module(M),M:propertyAssertion(A, B, C),subsumed_by([A, B, C],[objectPropertyExpression, individual, individual]).
axiom_arguments(objectPropertyAssertion,[objectPropertyExpression, individual, individual]).
valid_axiom(objectPropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[objectPropertyExpression, individual, individual]).

%% dataPropertyAssertion(?ObjectPropertyExpression, ?SourceIndividual:Individual, ?TargetValue:Literal)
% A positive data property assertion PropertyAssertion( DPE a lt ) states that the individual a is connected by the data property expression DPE to the literal lt
dataPropertyAssertion(A, B, C) :- get_module(M),M:propertyAssertion(A, B, C),subsumed_by([A, B, C],[dataPropertyExpression, individual, literal]).
axiom_arguments(dataPropertyAssertion,[objectPropertyExpression, individual, literal]).
valid_axiom(dataPropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[dataPropertyExpression, individual, literal]).

%% negativePropertyAssertion(?PropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)
% A negative object property assertion NegativePropertyAssertion( OPE a1 a2 ) states that the individual a1 is not connected by the object property expression OPE to the individual a2
% (extensional predicate - can be asserted)
%:- thread_local(negativePropertyAssertion/3).

axiompred(negativePropertyAssertion/3).
axiom_arguments(negativePropertyAssertion,[propertyExpression, individual, individual]).
valid_axiom(negativePropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[propertyExpression, individual, individual]).
expand_axiom(M,negativePropertyAssertion(A,B,C),NSList,negativePropertyAssertion(A_full_URL,B_full_URL,C_full_URL)) :- 
  expand_individual(M,C,NSList,C_full_URL),!,
  expand_individual(M,B,NSList,B_full_URL),
  expand_objectPropertyExpression(M,A,NSList,A_full_URL).
expand_axiom(M,negativePropertyAssertion(A,B,C),NSList,negativePropertyAssertion(A_full_URL,B_full_URL,C_full_URL)) :- 
  expand_literal(M,C,NSList,C_full_URL),
  expand_individual(M,B,NSList,B_full_URL),
  expand_dataPropertyExpression(M,A,NSList,A_full_URL).

%% negativeObjectPropertyAssertion(?ObjectPropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)
% A negative object property assertion NegativePropertyAssertion( OPE a1 a2 ) states that the individual a1 is not connected by the object property expression OPE to the individual a2
negativeObjectPropertyAssertion(A, B, C) :- get_module(M),M:negativePropertyAssertion(A, B, C),subsumed_by([A, B, C],[objectPropertyExpression, individual, individual]).
axiom_arguments(negativeObjectPropertyAssertion,[objectPropertyExpression, individual, individual]).
valid_axiom(negativeObjectPropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[objectPropertyExpression, individual, individual]).

%% negativeDataPropertyAssertion(?DataPropertyExpression, ?SourceIndividual:Individual, ?TargetValue:Literal)
% A negative data property assertion NegativePropertyAssertion( DPE a lt ) states that the individual a is not connected by the data property expression DPE to the literal lt
negativeDataPropertyAssertion(A, B, C) :- get_module(M),M:negativePropertyAssertion(A, B, C),subsumed_by([A, B, C],[dataPropertyExpression, individual, literal]).
axiom_arguments(negativeDataPropertyAssertion,[dataPropertyExpression, individual, literal]).
valid_axiom(negativeDataPropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[dataPropertyExpression, individual, literal]).

%% annotationAssertion(?AnnotationProperty, ?AnnotationSubject, ?AnnotationValue)
% An annotation assertion AnnotationAssertion( AP as av ) states that the annotation subject as - an IRI or an anonymous individual - is annotated with the annotation property AP and the annotation value av
%:- thread_local(annotationAssertion/3).

axiompred(annotationAssertion/3).
axiom_arguments(annotationAssertion,[annotationProperty, annotationSubject, annotationValue]).
valid_axiom(annotationAssertion(A, B, C)) :- subsumed_by([A, B, C],[annotationProperty, annotationSubject, annotationValue]).
annotationSubject(_).
annotationValue(_).
expand_axiom(M,annotationAssertion(A,B,C),NSList,annotationAssertion(A_full_URL,B_full_URL,C_full_URL)) :-
  expand_annotationProperty(M,A,NSList,A_full_URL),
  expand_annotationSubject(M,B,NSList,B_full_URL),
  expand_annotationValue(M,C,NSList,C_full_URL).

%% annotation(:IRI,?AnnotationProperty,?AnnotationValue)
%
% @see annotationAnnotation/3, ontologyAnnotation/3, axiomAnnotation/3
%:- thread_local(annotation/3).

axiompred(annotation/3).

annotation(M:annotationAnnotation(A, B, C)) :- M:annotationAnnotation(M:A, B, C).
annotation(M:axiomAnnotation(A, B, C)) :- M:axiomAnnotation(M:A, B, C).
axiom_arguments(annotation,[iri,annotationProperty,annotationValue]).
valid_axiom(annotation(A,B,C)) :- subsumed_by([A,B,C],[iri,annotationProperty,annotationValue]).
expand_axiom(M,annotationAnnotation(A,B,C),NSList,annotationAnnotation(A_full_URL,B_full_URL,C_full_URL)) :- 
  expand_iri(M,A,NSList,A_full_URL),
  expand_annotationProperty(M,B,NSList,B_full_URL),
  expand_annotationValue(M,C,NSList,C_full_URL),
  ( M:addKBName -> add_kb_atoms(M,annotationProperty,[A_full_URL]) ; true ).

%% ontologyAnnotation(?Ontology, ?AnnotationProperty, ?AnnotationValue)
ontologyAnnotation(M:Ontology,AP,AV) :-
	M:annotation(Ontology,AP,AV),
	M:ontology(Ontology).
axiom_arguments(ontologyAnnotation,[ontology, annotationProperty, annotationValue]).
valid_axiom(ontologyAnnotation(A, B, C)) :- subsumed_by([A, B, C],[ontology, annotationProperty, annotationValue]).

%% axiomAnnotation(?Axiom, ?AnnotationProperty, ?AnnotationValue)
axiomAnnotation(M:Axiom,AP,AV) :-
	M:annotation(Axiom,AP,AV),
	M:axiom(Axiom).
axiom_arguments(axiomAnnotation,[axiom, annotationProperty, annotationValue]).
valid_axiom(axiomAnnotation(A, B, C)) :- subsumed_by([A, B, C],[axiom, annotationProperty, annotationValue]).

%% annotationAnnotation(?Annotation, ?AnnotationProperty, ?AnnotationValue)
annotationAnnotation(M:Annotation,AP,AV) :-
	M:annotation(Annotation,AP,AV),
	annotation(M:Annotation).
axiom_arguments(annotationAnnotation,[annotation, annotationProperty, annotationValue]).
valid_axiom(annotationAnnotation(A, B, C)) :- subsumed_by([A, B, C],[annotation, annotationProperty, annotationValue]).

%% ontology(?IRI)
% An ontology in OWL2 is a collection of OWL Axioms
%:- thread_local(ontology/1).

expand_ontology(M,A,NSList,A_full_URL) :-
  expand_iri(M,A,NSList,A_full_URL).

axiompred(ontology/1).
axiom_arguments(ontology,[iri]).
valid_axiom(ontology(A)) :- subsumed_by([A],[iri]).
expand_axiom(M,ontology(A),NSList,ontology(A_full_URL)) :- 
  expand_iri(M,A,NSList,A_full_URL).

%% ontologyDirective(:OntologyIRI,?IRI)
% @see ontologyImport/2, ontologyAxiom/2
:- meta_predicate ontologyDirective(:,?).

ontologyDirective(M:A, B) :- M:ontologyImport(A, B).
ontologyDirective(M:A, B) :- M:ontologyAxiom(A, B).
ontologyDirective(M:A, B) :- M:ontologyVersionInfo(A, B).
axiom_arguments(ontologyDirective,[ontology, iri]).
valid_axiom(ontologyDirective(A, B)) :- subsumed_by([A, B],[ontology, iri]).

%% ontologyAxiom(?Ontology, ?Axiom)
% True if Ontology contains Axiom.
% Axiom is a prolog term that is typically asserted and separately and can thus can be executed as a goal.
% For example, an ontology http://example.org# will contain redundant assertions:
% ==
% subClassOf('http://example.org#a', 'http://example.org#b').
% ontologyAxiom('http://example.org#', subClassOf('http://example.org#a','http://example.org#b')).
% ==
%:- thread_local(ontologyAxiom/2).

axiompred(ontologyAxiom/2).
axiom_arguments(ontologyAxiom,[ontology, axiom]).
valid_axiom(ontologyAxiom(A, B)) :- subsumed_by([A, B],[ontology, axiom]).
expand_axiom(M,ontologyAxiom(A,B),NSList,ontology(A_full_URL,B_full_URL)) :- 
  expand_ontology(M,A,NSList,A_full_URL),
  expand_axiom(M,B,NSList,B_full_URL).

%% ontologyImport(?Ontology, ?IRI)
% True of Ontology imports document IRI
%:- thread_local(ontologyImport/2).

axiompred(ontologyImport/2).
axiom_arguments(ontologyImport,[ontology, iri]).
valid_axiom(ontologyImport(A, B)) :- subsumed_by([A, B],[ontology, iri]).
expand_axiom(M,ontologyImport(A,B),NSList,ontology(A_full_URL,B)) :- 
  expand_iri(M,A,NSList,A_full_URL),
  M:consult(B).

%% ontologyVersionInfo(?Ontology, ?IRI)
%:- thread_local(ontologyVersionInfo/2).

axiompred(ontologyVersionInfo/2).
axiom_arguments(ontologyVersionInfo,[ontology, iri]).
valid_axiom(ontologyVersionInfo(A, B)) :- subsumed_by([A, B],[ontology, iri]).

/****************************************
  RESTRICTIONS ON AXIOMS
  ****************************************/

% 11.1
% An object property expression OPE is simple in Ax if, for each object property expression OPE' such that OPE' ->* OPE holds, OPE' is not composite.
% (The property hierarchy relation ->* is the reflexive-transitive closure of ->)
%simpleObjectPropertyExpresion(OPE) :-
%        objectPropertyExpression(OPE),


/****************************************
  EXPRESSIONS
  ****************************************/

subsumed_by(X,_) :- var(X),!.
subsumed_by([],[]) :- !.
subsumed_by([I|IL],[T|TL]) :-
	!,
	subsumed_by(I,T),
	subsumed_by(IL,TL).
subsumed_by(L,set(T)):-
        !,
        forall(member(I,L),
               subsumed_by(I,T)).
subsumed_by(I,T):-
        !,
	G=..[T,I],
	get_module(M),
	M:G.


%% iri(?IRI)
% true if IRI is an IRI. TODO: currently underconstrained, any atomic term can be an IRI
iri(IRI) :- atomic(IRI).	%
expand_iri(_M,NS_URL,NSList,Full_URL):-
  atomic(NS_URL),
  NS_URL \= literal(_),
  uri_split(NS_URL,Short_NS,Term, ':'),
  member((Short_NS=Long_NS),NSList),
  concat_atom([Long_NS,Term],Full_URL),!.

expand_iri(_M,NS_URL,NSList,Full_URL):- 
  atomic(NS_URL),
  NS_URL \= literal(_),
  \+ sub_atom(NS_URL,_,_,_,':'),
  member(([]=Long_NS),NSList),
  concat_atom([Long_NS,NS_URL],Full_URL),!.

expand_iri(_M,IRI,_NSList,IRI):- atomic(IRI).
  

%% literal(?Lit)
% true if Lit is an rdf literal
%literal(_).			% TODO
literal(literal(_)).			% TODO
expand_literal(M,literal(type(Type,Val)),NSList,literal(type(ExpType,Val))) :-
  expand_datatype(M,Type,NSList,ExpType),!.
expand_literal(_M,literal(Literal),_NSList,literal(Literal)).

propertyExpression(E) :- objectPropertyExpression(E) ; dataPropertyExpression(E).

expand_propertyExpressions(_M,[],_NSList,[]) :- !.
expand_propertyExpressions(M,[CE|T],NSList,[ExpCE|ExpT]) :-
  expand_propertyExpression(M,CE,NSList,ExpCE),
  expand_propertyExpressions(M,T,NSList,ExpT).
  
% expand_propertyExpression(M,E,NSList,ExpE):- expand_objectPropertyExpression(M,E,NSList,ExpE) ; expand_dataPropertyExpression(M,E,NSList,ExpE). % TODO: support for datatype to implement
expand_propertyExpression(M,inverseOf(OP),NSList,inverseOf(ExpOP)) :- !,
  expand_objectProperty(M,OP,NSList,ExpOP).
  %add_expressivity(M,i).
expand_propertyExpression(M,E,NSList,ExpE) :- expand_objectProperty(M,E,NSList,ExpE).

%% objectPropertyExpression(?OPE)
% true if OPE is an ObjectPropertyExpression
% ObjectPropertyExpression := ObjectProperty | InverseObjectProperty
objectPropertyExpression(E) :- objectProperty(E) ; inverseObjectProperty(E).
% expand_objectPropertyExpression(M,E,NSList,ExpE) :- expand_objectProperty(M,E,NSList,ExpE) ; expand_inverseObjectProperty(M,E,NSList,ExpE).
expand_objectPropertyExpression(M,inverseOf(OP),NSList,inverseOf(ExpOP)) :- !,expand_objectProperty(M,OP,NSList,ExpOP).
  %add_expressivity(M,i).
expand_objectPropertyExpression(M,E,NSList,ExpE) :- expand_objectProperty(M,E,NSList,ExpE).

% give benefit of doubt; e.g. rdfs:label
% in the OWL2 spec we have DataProperty := IRI
% here dataProperty/1 is an asserted fact
objectPropertyExpression(E) :- nonvar(E),iri(E).

objectPropertyExpressionOrChain(propertyChain(PL)) :- forall(member(P,PL),objectPropertyExpression(P)).
objectPropertyExpressionOrChain(PE) :- objectPropertyExpression(PE).
expand_objectPropertyExpressionOrChain(M,propertyChain(PL),NSList,propertyChain(ExpPL)):- !,
  expand_propertyExpressions(M,PL,NSList,ExpPL).
  %add_expressivity(M,r).
expand_objectPropertyExpressionOrChain(M,P,NSList,ExpP):-
  expand_objectPropertyExpression(M,P,NSList,ExpP).



inverseObjectProperty(inverseOf(OP)) :- objectProperty(OP).
expand_inverseObjectProperty(M,inverseOf(OP),NSList,inverseOf(ExpOP)) :- expand_objectProperty(M,OP,NSList,ExpOP).
  %add_expressivity(M,i).

expand_dataPropertyExpressions(M,DPEs,NSList,ExpDPEs) :- expand_dataPropertyExpression(M,DPEs,NSList,ExpDPEs).

dataPropertyExpression(E) :- dataProperty(E).
expand_dataPropertyExpression(M,E,NSList,ExpE) :- expand_dataProperty(M,E,NSList,ExpE).

dataPropertyExpression(DPEs) :-
	(   is_list(DPEs)
	->  forall(member(DPE,DPEs),
		   dataPropertyExpression(DPE))
	;   dataPropertyExpression(DPEs)).

expand_dataPropertyExpression(_M,[],_NSList,[]) :- !.
expand_dataPropertyExpression(M,[DPE|T],NSList,[ExpDPE|ExpT]) :-
  expand_dataPropertyExpression(M,DPE,NSList,ExpDPE),
  expand_dataPropertyExpression(M,T,NSList,ExpT).

% give benefit of doubt; e.g. rdfs:label
% in the OWL2 spec we have DataProperty := IRI
% here dataProperty/1 is an asserted fact
dataPropertyExpression(E) :- nonvar(E),iri(E).

%already declared as entity
%datatype(IRI) :- iri(IRI).
expand_datatype(M,DT,NSList,ExpDT) :- 
  expand_iri(M,DT,NSList,ExpDT),
  builtin_datatype(ExpDT).

expand_dataRanges(_M,[],_NSList,[]) :- !.
expand_dataRanges(M,[H|T],NSList,[ExpH|ExpT]) :-
  expand_dataRange(M,H,NSList,ExpH),
  expand_dataRanges(M,T,NSList,ExpT).

%% dataRange(+DR) is semidet
dataRange(DR) :-
    (datatype(DR) ;
    dataIntersectionOf(DR );
    dataUnionOf(DR) ;
    dataComplementOf(DR) ;
    dataOneOf(DR) ;
    datatypeRestriction(DR)),!.
expand_dataRange(M,intersectionOf(DRs),NSList,intersectionOf(ExpDRs)) :- !,
  expand_dataRanges(M,DRs,NSList,ExpDRs).
expand_dataRange(M,unionOf(DRs),NSList,unionOf(ExpDRs)) :- !,
	expand_dataRanges(M,DRs,NSList,ExpDRs).
expand_dataRange(M,complementOf(DR),NSList,complementOf(ExpDR)) :- !,
	expand_dataRange(M,DR,NSList,ExpDR).
expand_dataRange(M,oneOf(DRs),NSList,oneOf(ExpDRs)) :- !,
	expand_dataRanges(M,DRs,NSList,ExpDRs).
expand_dataRange(M,datatypeRestriction(DR,FacetValues),NSList,datatypeRestriction(DRs,FacetValues)):- !,
	expand_datatype(M,DR,NSList,DRs),
	FacetValues=[_|_].
expand_dataRange(M,literal(DR),NSList,ExpDR):- !,
  expand_literal(M,literal(DR),NSList,ExpDR).
expand_dataRange(M,DR,NSList,ExpDR) :-
  expand_datatype(M,DR,NSList,ExpDR),
  ( M:addKBName -> add_kb_atoms(M,datatype,[ExpDR]) ; true ).


%% classExpression(+CE) is semidet
%
% true if CE is a class expression term, as defined in OWL2
%
% Example: =|classExpression(intersectionOf([car,someValuesFrom(hasColor,blue)])))|=
%
% Union of:
%
%    class/1 | objectIntersectionOf/1 | objectUnionOf/1 |
%    objectComplementOf/1 | objectOneOf/1 | objectSomeValuesFrom/1 |
%    objectAllValuesFrom/1 | objectHasValue/1 | objectHasSelf/1 |
%    objectMinCardinality/1 | objectMaxCardinality/1 |
%    objectExactCardinality/1 | dataSomeValuesFrom/1 |
%    dataAllValuesFrom/1 | dataHasValue/1 | dataMinCardinality/1 |
%    dataMaxCardinality/1 | dataExactCardinality/1
expand_classExpressions(_M,[],_NSList,[]) :- !.
expand_classExpressions(M,[CE|T],NSList,[ExpCE|ExpT]) :-
  expand_classExpression(M,CE,NSList,ExpCE),
  expand_classExpressions(M,T,NSList,ExpT).

classExpression(CE):-
        (iri(CE) ;               % NOTE: added to allow cases where class is not imported
    class(CE) ;
    objectIntersectionOf(CE) ; objectUnionOf(CE) ; objectComplementOf(CE) ; objectOneOf(CE) ;
    objectSomeValuesFrom(CE) ; objectAllValuesFrom(CE) ; objectHasValue(CE) ; objectHasSelf(CE) ;
    objectMinCardinality(CE) ; objectMaxCardinality(CE) ; objectExactCardinality(CE) ;
    dataSomeValuesFrom(CE) ; dataAllValuesFrom(CE) ; dataHasValue(CE) ;
    dataMinCardinality(CE) ; dataMaxCardinality(CE) ; dataExactCardinality(CE)),!.
/*
expand_classExpression(M,CE,NSList,ExpCE):-			 % TODO: add management datatype
    (expand_class(M,CE,NSList,ExpCE) ;               % NOTE: added to allow cases where class is not imported
    expand_objectIntersectionOf(M,CE,NSList,ExpCE) ; expand_objectUnionOf(M,CE,NSList,ExpCE) ; expand_objectComplementOf(M,CE,NSList,ExpCE) ; expand_objectOneOf(M,CE,NSList,ExpCE) ;
    expand_objectSomeValuesFrom(M,CE,NSList,ExpCE) ; expand_objectAllValuesFrom(M,CE,NSList,ExpCE) ; expand_objectHasValue(M,CE,NSList,ExpCE) ; expand_objectHasSelf(M,CE,NSList,ExpCE) ;
    expand_objectMinCardinality(M,CE,NSList,ExpCE) ; expand_objectMaxCardinality(M,CE,NSList,ExpCE) ; expand_objectExactCardinality(M,CE,NSList,ExpCE) ;
    expand_dataSomeValuesFrom(M,CE,NSList,ExpCE) ; expand_dataAllValuesFrom(M,CE,NSList,ExpCE) ; expand_dataHasValue(M,CE,NSList,ExpCE) ;
    expand_dataMinCardinality(M,CE,NSList,ExpCE) ; expand_dataMaxCardinality(M,CE,NSList,ExpCE) ; expand_dataExactCardinality(M,CE,NSList,ExpCE)),
    ( M:addKBName -> add_kb_atoms(M,class,[ExpCE]) ; true ).
*/
expand_classExpression(M,intersectionOf(CEs),NSList,intersectionOf(ExpCEs)):- !,
  expand_classExpressions(M,CEs,NSList,ExpCEs),
  ( M:addKBName -> add_kb_atoms(M,class,[intersectionOf(ExpCEs)]) ; true ).
expand_classExpression(M,unionOf(CEs),NSList,unionOf(ExpCEs)) :- !,
  expand_classExpressions(M,CEs,NSList,ExpCEs),
  ( M:addKBName -> add_kb_atoms(M,class,[unionOf(ExpCEs)]) ; true ).
  %add_rule(M,or_rule),
  %add_expressivity(M,a).
expand_classExpression(M,complementOf(CE),NSList,complementOf(ExpCE)) :- !,
  expand_classExpression(M,CE,NSList,ExpCE),
  ( M:addKBName -> add_kb_atoms(M,class,[complementOf(ExpCE)]) ; true ).
  %add_expressivity(M,a).
expand_classExpression(M,oneOf(Is),NSList,oneOf(ExpIs)) :- !,  % TODO check in trill
  expand_individuals(M,Is,NSList,ExpIs),
  ( M:addKBName -> add_kb_atoms(M,class,[oneOf(ExpIs)]) ; true ).
  %add_rule(M,o_rule),
  %add_expressivity(M,o).
expand_classExpression(M,someValuesFrom(OPE,CE),NSList,someValuesFrom(ExpOPE,ExpCE)) :- !,
  expand_objectPropertyExpression(M,OPE,NSList,ExpOPE),
  expand_classExpression(M,CE,NSList,ExpCE),
  ( M:addKBName -> add_kb_atoms(M,class,[someValuesFrom(ExpOPE,ExpCE)]) ; true ).
  %add_rule(M,exists_rule).
expand_classExpression(M,allValuesFrom(OPE,CE),NSList,allValuesFrom(ExpOPE,ExpCE)) :- !,
	expand_objectPropertyExpression(M,OPE,NSList,ExpOPE),
	expand_classExpression(M,CE,NSList,ExpCE),
    ( M:addKBName -> add_kb_atoms(M,class,[allValuesFrom(ExpOPE,ExpCE)]) ; true ).
  %add_rule(M,forall_rule),
  %add_expressivity(M,a).
expand_classExpression(M,hasValue(OPE,I),NSList,hasValue(ExpOPE,ExpI)) :- !,  % TODO: add in trill
	expand_objectPropertyExpression(M,OPE,NSList,ExpOPE),
	expand_individual(M,I,NSList,ExpI),
    ( M:addKBName -> add_kb_atoms(M,class,[hasValue(ExpOPE,ExpI)]) ; true ).
expand_classExpression(M,hasSelf(OPE),NSList,hasSelf(ExpOPE)) :- !,  % TODO: add in trill
	expand_objectPropertyExpression(M,OPE,NSList,ExpOPE),
    ( M:addKBName -> add_kb_atoms(M,class,[hasSelf(ExpOPE)]) ; true ).
expand_classExpression(M,minCardinality(C,OPE,CE),NSList,minCardinality(C,ExpOPE,ExpCE)):- !,
	number(C),
	C>=0,
	expand_objectPropertyExpression(M,OPE,NSList,ExpOPE),
	expand_classExpression(M,CE,NSList,ExpCE),
    ( M:addKBName -> add_kb_atoms(M,class,[minCardinality(C,ExpOPE,ExpCE)]) ; true ).
  %add_rule(M,min_rule),
  %add_expressivity(M,q).
expand_classExpression(M,minCardinality(C,OPE),NSList,minCardinality(C,ExpOPE)):- !,
	number(C),
	C>=0,
	expand_objectPropertyExpression(M,OPE,NSList,ExpOPE),
    ( M:addKBName -> add_kb_atoms(M,class,[minCardinality(C,ExpOPE)]) ; true ).
  %add_rule(M,min_rule),
  %add_expressivity(M,n).
expand_classExpression(M,maxCardinality(C,OPE,CE),NSList,maxCardinality(C,ExpOPE,ExpCE)):- !,
	number(C),
	C>=0,
	expand_objectPropertyExpression(M,OPE,NSList,ExpOPE),
	expand_classExpression(M,CE,NSList,ExpCE),
    ( M:addKBName -> add_kb_atoms(M,class,[maxCardinality(C,ExpOPE,ExpCE)]) ; true ).
  %add_rule(M,max_rule),
  %add_expressivity(M,q).
expand_classExpression(M,maxCardinality(C,OPE),NSList,maxCardinality(C,ExpOPE)):- !,
	number(C),
	C>=0,
	expand_objectPropertyExpression(M,OPE,NSList,ExpOPE),
    ( M:addKBName -> add_kb_atoms(M,class,[maxCardinality(C,ExpOPE)]) ; true ).
  %add_rule(M,max_rule),
  %add_expressivity(M,n).
expand_classExpression(M,exactCardinality(C,OPE,CE),NSList,exactCardinality(C,ExpOPE,ExpCE)):- !,
	number(C),
	C>=0,
	expand_objectPropertyExpression(M,OPE,NSList,ExpOPE),
	expand_classExpression(M,CE,NSList,ExpCE),
    ( M:addKBName -> add_kb_atoms(M,class,[exactCardinality(C,ExpOPE,ExpCE)]) ; true ).
  %add_rule(M,min_rule),add_rule(M,max_rule),
  %add_expressivity(M,q).
expand_classExpression(M,exactCardinality(C,OPE),NSList,exactCardinality(C,ExpOPE)):- !,
	number(C),
	C>=0,
	expand_objectPropertyExpression(M,OPE,NSList,ExpOPE),
    ( M:addKBName -> add_kb_atoms(M,class,[exactCardinality(C,ExpOPE)]) ; true ).
  %add_rule(M,min_rule),add_rule(M,max_rule),
  %add_expressivity(M,n).
expand_classExpression(M,CE,NSList,ExpCE):-
    expand_class(M,CE,NSList,ExpCE),
    ( M:addKBName -> add_kb_atoms(M,class,[ExpCE]) ; true ).

%% objectIntersectionOf(+CE) is semidet
% true if CE is a term intersectionOf(ClassExpression:list)
%
% An intersection class expression IntersectionOf( CE1 ... CEn ) contains all individuals that are instances of all class expressions CEi for 1 <= i <= n.
objectIntersectionOf(intersectionOf(CEs)) :-
	forall(member(CE,CEs),
	       classExpression(CE)).
expand_objectIntersectionOf(M,intersectionOf(CEs),NSList,intersectionOf(ExpCEs)) :-
  expand_classExpressions(M,CEs,NSList,ExpCEs).

%% objectUnionOf(+CE) is semidet
% A union class expression UnionOf( CE1 ... CEn ) contains all individuals that are instances of at least one class expression CEi for 1 <= i <= n
objectUnionOf(unionOf(CEs)) :-
	forall(member(CE,CEs),
	       classExpression(CE)).
expand_objectUnionOf(M,unionOf(CEs),NSList,unionOf(ExpCEs)) :-
  expand_classExpressions(M,CEs,NSList,ExpCEs).

%% objectComplementOf(+CE) is semidet
%
objectComplementOf(complementOf(CE)) :-
	classExpression(CE).
expand_objectComplementOf(M,complementOf(CE),NSList,complementOf(ExpCE)) :-
	expand_classExpression(M,CE,NSList,ExpCE).

%% objectOneOf(+CE) is semidet
% An enumeration of individuals OneOf( a1 ... an ) contains exactly the individuals ai with 1 <= i <= n.
objectOneOf(oneOf(Is)) :-
        is_list(Is). % TODO: check if we need to strengthen this check
%objectOneOf(oneOf(Is)) :-
%	forall(member(I,Is),
%	       individual(I)).
expand_objectOneOf(M,oneOf(Is),NSList,oneOf(ExpIs)) :-
  expand_individuals(M,Is,NSList,ExpIs).

%% objectSomeValuesFrom(+R) is semidet
% An existential class expression SomeValuesFrom( OPE CE ) consists of an object property expression OPE and a class expression CE, and it contains all those individuals that are connected by OPE to an individual that is an instance of CE
objectSomeValuesFrom(someValuesFrom(OPE,CE)) :-
	objectPropertyExpression(OPE),
	classExpression(CE).
expand_objectSomeValuesFrom(M,someValuesFrom(OPE,CE),NSList,someValuesFrom(ExpOPE,ExpCE)) :-
	expand_objectPropertyExpression(M,OPE,NSList,ExpOPE),
	expand_classExpression(M,CE,NSList,ExpCE).

%% objectAllValuesFrom(+R) is semidet
% A universal class expression AllValuesFrom( OPE CE ) consists of an object property expression OPE and a class expression CE, and it contains all those individuals that are connected by OPE only to individuals that are instances of CE
objectAllValuesFrom(allValuesFrom(OPE,CE)) :-
	objectPropertyExpression(OPE),
	classExpression(CE).
expand_objectAllValuesFrom(M,allValuesFrom(OPE,CE),NSList,allValuesFrom(ExpOPE,ExpCE)) :-
	expand_objectPropertyExpression(M,OPE,NSList,ExpOPE),
	expand_classExpression(M,CE,NSList,ExpCE).

%% objectHasValue(+R) is semidet
% A has-value class expression HasValue( OPE a ) consists of an object property expression OPE and an individual a, and it contains all those individuals that are connected by OPE to a
objectHasValue(hasValue(OPE,I)) :-
	objectPropertyExpression(OPE),
	individual(I).
expand_objectHasValue(M,hasValue(OPE,I),NSList,hasValue(ExpOPE,ExpI)) :-
	expand_objectPropertyExpression(M,OPE,NSList,ExpOPE),
	expand_individual(M,I,NSList,ExpI).

%% objectHasSelf(+R) is semidet
% A self-restriction HasSelf( OPE ) consists of an object property expression OPE, and it contains all those individuals that are connected by OPE to themselves
objectHasSelf(hasSelf(OPE)) :-
	objectPropertyExpression(OPE).
expand_objectHasSelf(M,hasSelf(OPE),NSList,hasSelf(ExpOPE)) :-
	expand_objectPropertyExpression(M,OPE,NSList,ExpOPE).	

%% objectMinCardinality(+CR) is semidet
% A minimum cardinality expression MinCardinality( n OPE CE ) consists of a nonnegative integer n, an object property expression OPE, and a class expression CE, and it contains all those individuals that are connected by OPE to at least n different individuals that are instances of CE. If CE is missing, it is taken to be owl:Thing
objectMinCardinality(minCardinality(C,OPE,CE)):-
	number(C),
	C>=0,
	objectPropertyExpression(OPE),
	classExpression(CE).
objectMinCardinality(minCardinality(C,OPE)):-
	number(C),
	C>=0,
	objectPropertyExpression(OPE).
expand_objectMinCardinality(M,minCardinality(C,OPE,CE),NSList,minCardinality(C,ExpOPE,ExpCE)):-
	number(C),
	C>=0,
	expand_objectPropertyExpression(M,OPE,NSList,ExpOPE),
	expand_classExpression(M,CE,NSList,ExpCE).
expand_objectMinCardinality(M,minCardinality(C,OPE),NSList,minCardinality(C,ExpOPE)):-
	number(C),
	C>=0,
	expand_objectPropertyExpression(M,OPE,NSList,ExpOPE).

%% objectMaxCardinality(+CR) is semidet
% A maximum cardinality expression MaxCardinality( n OPE CE ) consists of a nonnegative integer n, an object property expression OPE, and a class expression CE, and it contains all those individuals that are connected by OPE to at most n different individuals that are instances of CE. If CE is missing, it is taken to be owl:Thing
objectMaxCardinality(maxCardinality(C,OPE,CE)):-
	number(C),
	C>=0,
	objectPropertyExpression(OPE),
	classExpression(CE).
objectMaxCardinality(maxCardinality(C,OPE)):-
	number(C),
	C>=0,
	objectPropertyExpression(OPE).
expand_objectMaxCardinality(M,maxCardinality(C,OPE,CE),NSList,maxCardinality(C,ExpOPE,ExpCE)):-
	number(C),
	C>=0,
	expand_objectPropertyExpression(M,OPE,NSList,ExpOPE),
	expand_classExpression(M,CE,NSList,ExpCE).
expand_objectMaxCardinality(M,maxCardinality(C,OPE),NSList,maxCardinality(C,ExpOPE)):-
	number(C),
	C>=0,
	expand_objectPropertyExpression(M,OPE,NSList,ExpOPE).

%% objectExactCardinality(+CR) is semidet
% An exact cardinality expression ExactCardinality( n OPE CE ) consists of a nonnegative integer n, an object property expression OPE, and a class expression CE, and it contains all those individuals that are connected by OPE to exactly n different individuals that are instances of CE. If CE is missing, it is taken to be owl:Thing
objectExactCardinality(exactCardinality(C,OPE,CE)):-
	number(C),
	C>=0,
	objectPropertyExpression(OPE),
	classExpression(CE).
objectExactCardinality(exactCardinality(C,OPE)):-
	number(C),
	C>=0,
	objectPropertyExpression(OPE).
% NON-NORMATIVE: we accept this in order to maximize compatibility with Thea1
objectExactCardinality(cardinality(C,OPE)):-
	number(C),
	C>=0,
	objectPropertyExpression(OPE).
expand_objectExactCardinality(M,exactCardinality(C,OPE,CE),NSList,exactCardinality(C,ExpOPE,ExpCE)):-
	number(C),
	C>=0,
	expand_objectPropertyExpression(M,OPE,NSList,ExpOPE),
	expand_classExpression(M,CE,NSList,ExpCE).
expand_objectExactCardinality(M,exactCardinality(C,OPE),NSList,exactCardinality(C,ExpOPE)):-
	number(C),
	C>=0,
	expand_objectPropertyExpression(M,OPE,NSList,ExpOPE).

%% dataIntersectionOf(+DR:dataIntersectionOf) is semidet
% An intersection data range IntersectionOf( DR1 ... DRn ) contains all data values that are contained in the value space of every data range DRi for 1 <= i <= n. All data ranges DRi must be of the same arity
dataIntersectionOf(intersectionOf(DRs)) :-
	forall(member(DR,DRs),
	       dataRange(DR)).
expand_dataIntersectionOf(M,intersectionOf(DRs),NSList,intersectionOf(ExpDRs)) :-
	expand_dataRanges(M,DRs,NSList,ExpDRs).

%% dataUnionOf(+DR:dataUnionOf) is semidet
% A union data range UnionOf( DR1 ... DRn ) contains all data values that are contained in the value space of at least one data range DRi for 1 <= i <= n. All data ranges DRi must be of the same arity
dataUnionOf(unionOf(DRs)) :-
	forall(member(DR,DRs),
	       dataRange(DR)).
expand_dataUnionOf(M,unionOf(DRs),NSList,unionOf(ExpDRs)) :-
	expand_dataRanges(M,DRs,NSList,ExpDRs).

%% dataComplementOf(+DR:dataComplementOf) is semidet
% A complement data range ComplementOf( DR ) contains all literals that are not contained in the data range DR
dataComplementOf(complementOf(DR)) :-
	dataRange(DR).
expand_dataComplementOf(M,complementOf(DR),NSList,complementOf(ExpDR)) :-
	expand_dataRange(M,DR,NSList,ExpDR).

%% dataOneOf(+DR:dataOneOf) is semidet
% An enumeration of literals OneOf( lt1 ... ltn ) contains exactly the explicitly specified literals lti with 1 <= i <= n
dataOneOf(oneOf(DRs)) :-
	forall(member(DR,DRs),
	       dataRange(DR)).
expand_dataOneOf(M,oneOf(DRs),NSList,oneOf(ExpDRs)) :-
	expand_dataRanges(M,DRs,NSList,ExpDRs).

%% datatypeRestriction(+DR) is semidet
%
% TODO: multiple args
datatypeRestriction(datatypeRestriction(DR,FacetValues)):-
	datatype(DR),
	FacetValues=[_|_].
expand_datatypeRestriction(M,datatypeRestriction(DR,FacetValues),NSList,datatypeRestriction(DRs,FacetValues)):-
	expand_datatype(M,DR,NSList,DRs),
	FacetValues=[_|_].

%% dataSomeValuesFrom(+DR) is semidet
dataSomeValuesFrom(someValuesFrom(DPE,DR)):-
	dataPropertyExpression(DPE),
	dataRange(DR).
expand_dataSomeValuesFrom(M,someValuesFrom(DPE,DR),NSList,someValuesFrom(ExpDPE,ExpDR)):-
	expand_dataRange(M,DR,NSList,ExpDR),
	expand_dataPropertyExpression(M,DPE,NSList,ExpDPE).

%% dataAllValuesFrom(+DR) is semidet
dataAllValuesFrom(allValuesFrom(DPE,DR)):-
	dataPropertyExpression(DPE),
	dataRange(DR).
expand_dataAllValuesFrom(M,allValuesFrom(DPE,DR),NSList,allValuesFrom(ExpDPE,ExpDR)):-
	expand_dataRange(M,DR,NSList,ExpDR),
	expand_dataPropertyExpression(M,DPE,NSList,ExpDPE).

%% dataHasValue(+DR) is semidet
% A has-value class expression HasValue( DPE lt ) consists of a data property expression DPE and a literal lt, and it contains all those individuals that are connected by DPE to lt. Each such class expression can be seen as a syntactic shortcut for the class expression SomeValuesFrom( DPE OneOf( lt ) )
dataHasValue(hasValue(DPE,L)):-
	dataPropertyExpression(DPE),
	literal(L).
expand_dataHasValue(M,hasValue(DPE,L),NSList,hasValue(ExpDPE,ExpL)):-
	expand_literal(M,L,NSList,ExpL),
	expand_dataPropertyExpression(M,DPE,NSList,ExpDPE).

%% dataMinCardinality(+DR) is semidet
% A minimum cardinality expression MinCardinality( n DPE DR ) consists of a nonnegative integer n, a data property expression DPE, and a unary data range DR, and it contains all those individuals that are connected by DPE to at least n different literals in DR. If DR is not present, it is taken to be rdfs:Literal
dataMinCardinality(minCardinality(C,DPE,DR)):-
	number(C),
	C>=0,
	dataPropertyExpression(DPE),
	dataRange(DR).
dataMinCardinality(minCardinality(C,DPE)):-
	number(C),
	C>=0,
	dataPropertyExpression(DPE).
expand_dataMinCardinality(M,minCardinality(C,DPE,DR),NSList,minCardinality(C,ExpDPE,ExpDR)):-
	number(C),
	C>=0,
	expand_dataRange(M,DR,NSList,ExpDR),
	expand_dataPropertyExpression(M,DPE,NSList,ExpDPE).
expand_dataMinCardinality(M,minCardinality(C,DPE),NSList,minCardinality(C,ExpDPE)):-
	number(C),
	C>=0,
	expand_dataPropertyExpression(M,DPE,NSList,ExpDPE).


%% dataMaxCardinality(+DR) is semidet
% A maximum cardinality expression MaxCardinality( n DPE DR ) consists of a nonnegative integer n, a data property expression DPE, and a unary data range DR, and it contains all those individuals that are connected by DPE to at most n different literals in DR. If DR is not present, it is taken to be rdfs:Literal.
dataMaxCardinality(maxCardinality(C,DPE,DR)):-
	number(C),
	C>=0,
	dataPropertyExpression(DPE),
	dataRange(DR).
dataMaxCardinality(maxCardinality(C,DPE)):-
	number(C),
	C>=0,
	dataPropertyExpression(DPE).
expand_dataMaxCardinality(M,maxCardinality(C,DPE,DR),NSList,maxCardinality(C,ExpDPE,ExpDR)):-
	number(C),
	C>=0,
	expand_dataRange(M,DR,NSList,ExpDR),
	expand_dataPropertyExpression(M,DPE,NSList,ExpDPE).
expand_dataMaxCardinality(M,maxCardinality(C,DPE),NSList,maxCardinality(C,ExpDPE)):-
	number(C),
	C>=0,
	expand_dataPropertyExpression(M,DPE,NSList,ExpDPE).


%% dataExactCardinality(+DR) is semidet
% An exact cardinality expression ExactCardinality( n DPE DR ) consists of a nonnegative integer n, a data property expression DPE, and a unary data range DR, and it contains all those individuals that are connected by DPE to exactly n different literals in DR. If DR is not present, it is taken to be rdfs:Literal
dataExactCardinality(exactCardinality(C,DPE,DR)):-
	number(C),
	C>=0,
	dataPropertyExpression(DPE),
	dataRange(DR).
dataExactCardinality(exactCardinality(C,DPE)):-
	number(C),
	C>=0,
	dataPropertyExpression(DPE).
% NON-NORMATIVE: we accept this in order to maximize compatibility with Thea1
dataExactCardinality(cardinality(C,OPE)):-
	number(C),
	C>=0,
	objectPropertyExpression(OPE).
expand_dataExactCardinality(M,exactCardinality(C,DPE,DR),NSList,exactCardinality(C,ExpDPE,ExpDR)):-
	number(C),
	C>=0,
	expand_dataRange(M,DR,NSList,ExpDR),
	expand_dataPropertyExpression(M,DPE,NSList,ExpDPE).
expand_dataExactCardinality(M,exactCardinality(C,DPE),NSList,exactCardinality(C,ExpDPE)):-
	number(C),
	C>=0,
	expand_dataPropertyExpression(M,DPE,NSList,ExpDPE).

%% valid_axiom(?Axiom) is nondet
% true if Axiom passes typechecking


%% is_valid_axiom(?Axiom) is semidet
% true if Axiom passes typechecking
is_valid_axiom(Axiom) :- \+ \+ valid_axiom(Axiom).


/****************************************
  VIEW PREDICATES
  ****************************************/

%% equivalent_to(?X,?Y)
% note: this is currently slow for bound values of X and Y
equivalent_to(X,Y) :- equivalentClasses(L),member(X,L),member(Y,L),X\=Y.
equivalent_to(X,Y) :- equivalentProperties(L),member(X,L),member(Y,L),X\=Y.

disjoint_with(X,Y) :- disjointClasses(L),member(X,L),member(Y,L),X\=Y.

%% anyPropertyAssertion(?Property,?Entity,?Value)
% subsumes propertyAssertion/3 and annotationAssertion/3
anyPropertyAssertion(P,E,V) :- propertyAssertion(P,E,V).
anyPropertyAssertion(P,E,V) :- annotationAssertion(P,E,V).


%% labelAnnotation_value(?X,?Val)
labelAnnotation_value(X,Val) :-
        anyPropertyAssertion('http://www.w3.org/2000/01/rdf-schema#label', X, literal(type(_,Val))),atom(Val).
labelAnnotation_value(X,Val) :-
        anyPropertyAssertion('http://www.w3.org/2000/01/rdf-schema#label', X, literal(lang(_,Val))),atom(Val).
labelAnnotation_value(X,Val) :-
        anyPropertyAssertion('http://www.w3.org/2000/01/rdf-schema#label', X, literal(Val)),atom(Val).

/****************************************
  META-PREDICATES
  ****************************************/


%% axiom_directly_about(?Ax,?About)
% true if Ax is an axiom whose first argument is equal to About.
%
% e.g. axiom_directly_about( subClassOf(X,_), X).
%
% also include property assertions whose second argument is equal to About.
%
% e.g. axiom_directly_about( propertyAssertion(P,X,_), X).
%
axiom_directly_about(Ax,About) :-
        trill:axiom(Ax),
        Ax =.. [_,Arg1|_],
        (   is_list(Arg1)
        ->  member(About,Arg1)
        ;   About=Arg1).
axiom_directly_about(Ax,About) :-
	Ax=propertyAssertion(_,About,_),
        trill:axiom(Ax).
axiom_directly_about(Ax,About) :-
	Ax=annotationAssertion(_,About,_),
        trill:axiom(Ax).
axiom_directly_about(Ax,About) :-
	Ax=classAssertion(_,About),
        trill:axiom(Ax).


%% axiom_directly_references(?Ax:axiom,?Ref)
%
% Ref may be
%  - an axiom
%  - a named entity
%  - an expression
axiom_directly_references(Ax,Ref) :-
        trill:axiom(Ax),
        axiom_or_expression_references(Ax,Ref).

axiom_or_expression_references(X,Ref) :-
        X =.. [P|Args],
        P\=literal,
        member(Arg,Args),
        (   is_list(Arg)
        ->  member(Ref,Arg)
        ;   Ref=Arg).

axiom_about(Ax,About) :-
        axiom_directly_about(Ax,About).
axiom_about(Ax,About) :-
        axiom_directly_about(Ax,X),
        axiom_about(X,About).

axiom_references(Ax,Ref) :-
        axiom_directly_references(Ax,Ref).
axiom_references(Ax,Ref) :-
        axiom_directly_references(Ax,X),
        axiom_or_expression_references(X,Ref).

axiom_contains_expression(Ax,Ex) :-
        axiom_contains_expression(Ax,Ex,_).
axiom_contains_expression(Ax,Ex,D) :-
        trill:axiom(Ax),
        expression_has_subexpression(Ax,Ex,[],Chain),
        length(Chain,D).

expression_has_subexpression(Ex,Ex,Accum,Accum).
expression_has_subexpression(Ex,SubEx,Accum,Results) :-
        Ex =.. [F|Args],
        member(A,Args),
        expression_has_subexpression(A,SubEx,[F|Accum],Results).



%% referenced_description(?Desc) is nondet
% true if Desc is either a class or a class expression using the set of ontologies loaded.
% Example: if the ontology contains
% ==
% subClassOf(a,intersectionOf([b,someValuesFrom(p,c)]))
% ==
% then Desc will be a member of [a, b, c, b and p some c, p some c]
referenced_description(C) :-
        setof(C,referenced_description_1(C),Cs),
        member(C,Cs).

referenced_description_1(C) :- class(C).
referenced_description_1(C) :-
        subClassOf(A,B),
        (   referenced_description(A,C)
        ;   referenced_description(B,C)).
referenced_description_1(C) :-
        equivalentClasses(L),
        member(A,L),
        referenced_description(A,C).
referenced_description_1(C) :-
        classAssertion(A,_),
        referenced_description(A,C).

% TODO - this is incomplete
referenced_description(X,X) :- ground(X).
referenced_description(someValuesFrom(_,X),Y) :- referenced_description(X,Y).
referenced_description(allValuesFrom(_,X),Y) :- referenced_description(X,Y).
referenced_description(intersectionOf(L),Y) :- member(X,L),referenced_description(X,Y).
referenced_description(unionOf(L),Y) :- member(X,L),referenced_description(X,Y).


/****************************************
  UTILITY
  ****************************************/


%:- thread_local assert_axiom_hook/1.

%% assert_axiom(+Module,+Axiom:axiom)
%
% writes an axiom to the prolog database.
% typically this will just be a matter of calling assert/1. However, in future we
% will have different backing stores (rdf_db, sql), and in these cases calls to
% this predicate will perform the appropriate actions.
%
% this also asserts ontologyAxiom/2, using trdf_setting with current_ontology
assert_axiom(M,Axiom) :-
		( M:ns4query(NSList) -> true; NSList = []),
  		expand_axiom(M,Axiom,NSList,ExpAxiom),
  		dif(ExpAxiom,'none'),
        ( M:ExpAxiom -> true
          ;
          ( assert(M:ExpAxiom),
			(   M:trdf_setting(current_ontology,O)
        		->  assert(M:ontologyAxiom(O,ExpAxiom))
        		;   true)
       	  )
       	), !.
assert_axiom(_M,_Axiom).
  
%% assert_axiom(+Module,+Axiom:axiom,+Ontology:ontology) is det
%
% as assert_axiom/1, but also asserts to ontologyAxiom/2
assert_axiom(M,Axiom,_) :-
        M:Axiom,
        !.
assert_axiom(M,Axiom,O) :-
        assert(M:Axiom),
	assert(M:ontologyAxiom(O,Axiom)),
        !.


%% retract_axiom(+Module,+Axiom:axiom)
%
% removes an axiom from the prolog database.
% typically this will just be a matter of calling retract/1. However, in future we
% will have different backing stores (rdf_db, sql), and in these cases calls to
% this predicate will perform the appropriate actions.
%
% also removes ontologyAxiom/2 from ALL ontologies
retract_axiom(M,Axiom) :-
        retractall(M:Axiom),
	retractall(M:ontologyAxiom(_,Axiom)),
        !.

%% retract_axiom(+Module,+Axiom:axiom,+Ontology)
% retracts axioms from a specified ontology
retract_axiom(M,Axiom,Ontology) :-
        \+ var(Ontology),
	retractall(M:ontologyAxiom(Ontology,Axiom)),
        (   \+ M:ontologyAxiom(_,Axiom)
        ->  retractall(M:Axiom)
        ;   true),              % still exists in other ontology..
        !.


retract_all_axioms(M) :-
        findall(M:A,trill:axiom(M:A),Axioms),
        maplist(retract,Axioms),
        findall(M:ontologyAxiom(O,A),M:ontologyAxiom(O,A),OAxioms),
        maplist(retract,OAxioms),
	!.


utility_translation_init(M) :-
	assert(M:annotationProperty('http://www.w3.org/2000/01/rdf-schema#label')),
	assert(M:annotationProperty('http://www.w3.org/2000/01/rdf-schema#comment')),
	assert(M:annotationProperty('https://sites.google.com/a/unife.it/ml/disponte#probability')), % Retro-compatibility
	assert(M:annotationProperty('http://ml.unife.it/disponte#probability')).

consult_axioms(File) :-
        consult(File).

axiom_type(A,T) :- functor(A,T,_).

:- use_module(library(debug)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdf_edit')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('url')).
:- use_module(library('http/http_open')).
:- use_module(library(charsio)).

:- thread_local(owl/4).
:- thread_local(owl/3).
:- thread_local(owl/2).
:- dynamic owl/2.
%% blanknode(Node,Description,Used)
% see owl_get_bnode/2
% Node - bNodeId
% Description - prolog term corresponding to owl Description
% Used - used | shared
:- thread_local(blanknode/3).
:- thread_local(outstream/1).

:- thread_local(aNN/3). % implements the ANN(X) function.
:- thread_local(annotation_r_node/4).  % annotation_r_node(S,P,O,Node)
:- thread_local(axiom_r_node/4).       % axiom_r_node(S,P,O,Node)
:- thread_local(owl_repository/2). % implements a simple OWL repository: if URL not found, Ontology is read from a repository (local) RURL


% we make this discontiguous so that the code can follow the structure of the document as much as possible

:- discontiguous owl_parse_axiom/4.
:- discontiguous dothislater/1.

% hookable


% -----------------------------------------------------------------------
%                                UTILITY Predicates
% -----------------------------------------------------------------------


%%       owl_clear_as
%
%       Clears the prolog terms that store the Abstract Syntax
%       implementation of the OWL ontology.

owl_clear_as :-
        debug(owl_parser,'Clearing abstract syntax',[]),
        forall((axiompred(PredSpec),predspec_head(PredSpec,Head)),
               retractall(Head)).

predspec_head(Pred/A,Head) :- functor(Head,Pred,A).

u_assert(M,Term) :-
	call(M:Term), !; assert(M:Term).


convert(T,V,typed_value(T,V)).


%%	rdf_2_owl(+Base, +Ont) is det
%
%       Converts RDF triples to OWL/4 triples so that
%	their use can tracked by the OWL parser.


rdf_2_owl(M,Ont) :-
	debug(owl_parser, 'Removing existing owl triples',[]),
%	retractall(owl(_,_,_,Ont)),
	debug(owl_parser,'Copying RDF triples to OWL triples for Ontology ~w',[Ont]),
	M:rdf(X,Y,Z),
	assert(M:owl(X,Y,Z,Ont)), fail.

rdf_2_owl(M,Ont) :-
	owl_count(M,Ont,Z),
	debug(owl_parser,'Number of owl triples copied: ~w',[Z]).


%%	owl_count(+Module,+Ontology,?Number).
%       Returns/Checks the number of unused OWL triples.

owl_count(M,O,U) :-
	findall(1,M:owl(_,_,_,O),X), length(X,U).

%% expand_and_assert(M,S,P,O) is det
%
% adds a M:owl(S,P,O,not_used) after expanding namespaces.
% this is required for the triple replacement rules,
% which use shortened rdfs/owl namespaces.
% (or we could just use the expanded forms here which
%  may be faster..)
expand_and_assert(M,X1,Y1,Z1) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),!,
	retractall(M:owl(X,Y,Z, used1)),
	assert(M:owl(X,Y,Z, not_used)).


%%       test_use_owl(+Module,+Triples:list) is nondet
%
%       As use_owl/1, but does not consume the triple.  If owl(S,P,O)
%       in Triples has a non-ground variable then this will succeed
%       non-deterministically.  If all variables are ground, then this
%       will succeed semi-deterministically.
test_use_owl(_M,[]).
test_use_owl(M,[owl(S,P,O)|Rest]) :-
	test_use_owl(M,S,P,O),
	test_use_owl(M,Rest).


%%       test_use_owl(+M,?S,?P,?O)
%	As use_owl/3, but does not consume the triple. Expands the S,P,O.
%
%       If any of S, P or O is non-ground then this will succeed
%       non-deterministically.  If all variables are ground, then this
%       will succeed semi-deterministically.
test_use_owl(M,X1,Y1,Z1) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),!,
	M:owl(X,Y,Z, not_used).

test_use_owl(M,X1,Y1,Z1,named) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),
	M:owl(X,Y,Z, not_used),
	\+ sub_string(X,0,1,_,'_').


%%       use_owl(+Module,+Triples:list)
%	Marks a list of OWL triples as used, but only if all match. Expands the S,P,O.

use_owl(M,Triples) :-
        test_use_owl(M,Triples),
        use_owl_2(M,Triples).

% consume all triples; we have already tested the list and know that all match
use_owl_2(_M,[]).
use_owl_2(M,[owl(S,P,O)|Triples]) :-
        use_owl(M,S,P,O),
        use_owl_2(M,Triples).


use_owl(M,X1,Y1,Z1) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),
	M:owl(X,Y,Z, not_used),
	debug(owl_parser_detail,'using ~w ~w ~w',[X,Y,Z]),
	retract(M:owl(X,Y,Z, not_used)),
	assert(M:owl(X,Y,Z,used1)).

use_owl(M,X1,Y1,Z1,named) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),
	M:owl(X,Y,Z, not_used),
	\+ sub_string(X,0,1,_,'_'),
	retract(M:owl(X,Y,Z, not_used)),
	assert(M:owl(X,Y,Z,used2)).

use_owl(M,X1,Y1,Z1,Term) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),
	M:owl(X,Y,Z, not_used),
	debug(owl_parser_detail,'using ~w ~w ~w',[X,Y,Z]),
	retract(M:owl(X,Y,Z, not_used)),
	assert(M:owl(X,Y,Z,used(Term))).


%%	use_owl(+Module,?S,?P,?O,+Named,Term)
%
%       Named = named: Same as use_owl/3, but marks only if S 	is Named URI (i.e. non blank node).

use_owl(M,X1,Y1,Z1,named,Term) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),
	M:owl(X,Y,Z, not_used),
	\+ sub_string(X,0,1,_,'_'),
	retract(M:owl(X,Y,Z, not_used)),
	assert(M:owl(X,Y,Z,used(Term))).


%%       expand_ns(+NS_URL, ?Full_URL)
%
%       Expands a 'namespaced' URI of the form ns:fragment to a full URI
%       substituting the full expansion for ns from the ns/2 facts
expand_ns(NS_URL, Full_URL) :-
	nonvar(NS_URL),
	NS_URL \= literal(_),
	uri_split(NS_URL,Short_NS,Term, ':'),
	rdf_db:ns(Short_NS,Long_NS),!,
	concat_atom([Long_NS,Term],Full_URL).

expand_ns(URL, URL).


%%       collapse_ns(+FullURL, ?NSURL, +Char, +Options)
%
%	Collapses a full URI of the form Path#fragment to a Namespaced
%	URI NS:fragment substituting the full expansion for ns from
%	the ns/2 facts
%	Char is either ':' for normal ns notation or '_' for building
%	prolog terms.
%	Options supported: no_base(ShortNs): Use only term!


collapse_ns(FullURL, NSURL,Char,Options) :-
	nonvar(FullURL),
	FullURL \= literal(_),
	uri_split(FullURL,LongNS, Term, '#'),
	concat(LongNS,'#',LongNS1),
	rdf_db:ns(ShortNS,LongNS1),
	(   member(no_base(ShortNS),Options), ! , NSURL = Term
	;
	concat_atom([ShortNS,Char,Term],NSURL)
	),!.
% CJM
collapse_ns(FullURL, NSURL,_Char,Options) :-
	nonvar(FullURL),
	\+ FullURL = literal(_),
	uri_split(FullURL,LongNS, Term, '#'),
	member(no_base(LongNS),Options),
        !,
        NSURL = Term.


collapse_ns(URL, URL,_,_).



%%       uri_split(+URI,-Namespace,-Term,+Split_Char) is det
%
%       Splits a URI into the Namespace and the Term parts
%       separated by the Split_Char character.
%       It supposes URI = concat(Namespace,Split_Char,Term)

uri_split(URI,Namespace,Term,Split_Char) :-
	sub_atom(URI,Start,_,After,Split_Char),
	sub_atom(URI,0,Start,_,Namespace),
	Start1 is Start + 1,
	sub_atom(URI,Start1,After,_,Term).


%%       owl_collect_linked_nodes(+Node,+Predicate, +InList,-OutList)

%	Appends Node to the InList, and recursively, all other
%	Nodes that are linked with the Predicate to the Node. The
%	result is returned to OutList.

owl_collect_linked_nodes(Node,Predicate,InList,OutList) :-
    get_module(M),
	use_owl(M,Node,Predicate,A),!,
	owl_collect_linked_nodes(Node,Predicate,InList,List1),
	owl_collect_linked_nodes(A,Predicate,List1,OutList).

owl_collect_linked_nodes(Node,Predicate,InList,OutList) :-
	get_module(M),
	use_owl(M,A,Predicate,Node),!,
	owl_collect_linked_nodes(Node,Predicate,InList,List1),
	owl_collect_linked_nodes(A,Predicate,List1,OutList).

owl_collect_linked_nodes(Node,_,List, [Node|List]) :-
	\+ memberchk(Node, List),!.

owl_collect_linked_nodes(_,_,List, List) :- !.


% ----------------------------------------------------------------
%                OWL Parser implementation predicates
% ----------------------------------------------------------------


%%       owl_get_bnode(+Module,+Node,+Description)
%
%	if Node is a blank (not named) node, then it is asserted in
%	the database as a blanknode(Node,Description,used) term.
%	The purpose is to record when a blank node has been used, so
%	subsequent uses of it will result in structure sharing.

owl_get_bnode(M,Node,Description) :-
	sub_string(Node,0,1,_,'_'),!,
	\+ M:blanknode(Node,_,_),
	assert(M:blanknode(Node,Description, used)).

owl_get_bnode(_,_,_).



% -----------------------------------------------------------------------
%                                Top Level  Predicates
% -----------------------------------------------------------------------

/*
%% owl_parse(+URL, +RDF_Load_Mode, +OWL_Parse_Mode, +ImportFlag:boolean)
%
%  Top level: parse a set of RDF triples and produce an
%  AS representation of an OWL ontology.
%
%	Calls the rdf_load_stream predicate to parse RDF stream in URL.
%       If RDF_Load_Mode = complete it first retacts all rdf triples.
%       If ImportFlag = true it handles owl:import clause at RDF level.
%
% This implements the mapping defined here:
% http://www.w3.org/TR/2008/WD-owl2-mapping-to-rdf-20081202/
owl_parse(URL, RDF_Load_Mode, OWL_Parse_Mode,ImportFlag) :-
	(   RDF_Load_Mode=complete
	->  rdf_retractall(_,_,_), retractall(rdf_db:rdf_source(_,_,_,_))
        ;   true),
	(   OWL_Parse_Mode=complete
        ->  owl_clear_as,retractall(blanknode(_,_,_)), retractall(owl(_,_,_,_))
        ;   true),
        !,
        debug(owl_parser,'Loading stream ~w',[URL]),
	owl_canonical_parse_2([URL],URL,ImportFlag,[],ProcessedIRIs),
        debug(owl_parser,'rdf_db populated, the following IRIs were processed: ~w',[ProcessedIRIs]),
	utility_translation_init,
	owl_canonical_parse_3(ProcessedIRIs).


%% owl_canonical_parse_2(+IRIs:list,+ParentIRI,+ImportFlag:boolean,+ProcessedURIsIn:list,?ProcessedURIsOut:list) is det
% recursively parses all ontologies in IRIs into rdf_db, ensuring none are processed twice.
owl_canonical_parse_2([],_,_,Processed,Processed) :- !.

owl_canonical_parse_2([IRI|ToProcessRest],Parent,ImportFlag,ProcessedIn,ProcessedOut) :-
	member(IRI,ProcessedIn),
        !,
	owl_canonical_parse_2(ToProcessRest,Parent,ImportFlag,ProcessedIn,ProcessedOut).

owl_canonical_parse_2([IRI|ToProcessRest],Parent,ImportFlag,ProcessedIn,ProcessedOut) :-
	% Get rdf triples, *Ontology* and Imports
	rdf_load_stream(IRI,O,BaseURI,Imports),
	(   nonvar(O)
        ->  Ont = O
        ;   Ont = Parent), % in the include case we may need to remove the import...
        debug(owl_parser,'Commencing rdf_2_owl. Generating owl/4',[]),
	rdf_2_owl(BaseURI,Ont),  	% move the RDF triples into the owl-Ont/4 facts
	(   ImportFlag = true
        ->  owl_canonical_parse_2(Imports,Ont,ImportFlag,[Ont|ProcessedIn],ProcessedIn1)
        ;   ProcessedIn1=[Ont|ProcessedIn]),
	owl_canonical_parse_2(ToProcessRest,Parent,ImportFlag,ProcessedIn1,ProcessedOut).
*/

%% owl_canonical_parse_3(+Module,+IRIs:list) is det
% translate the current rdf_db into owl2_model axioms.
% First owl/4 facts are populated, and then these are translated
% according to:
% http://www.w3.org/TR/2008/WD-owl2-mapping-to-rdf-20081202/
% (table references refer to this document).
% we use an intermediate owl/4 database because the mapping
% is non-monotonic, and triples are 'consumed'
owl_canonical_parse_3(_,[]).

owl_canonical_parse_3(M,[IRI|Rest]) :-
	% Remove any existing not used owl fact
	retractall(M:owl(_,_,_,not_used)),
	% Copy the owl facts of the IRI document to the 'not_used'
	forall(M:owl(S,P,O,IRI),assert(M:owl(S,P,O,not_used))),

        debug(owl_parser,'Anon individuals in reification [see table 8]',[]),

	collect_r_nodes(M),
	
	% Removed
	%forall(M:axiom_r_node(S,P,O,_Node),assert(M:owl(S,P,O,not_used))),

	% First parse the Ontology axiom
        owl_parse_annotated_axioms(M,ontology/1),

        debug(owl_parser,'Replacing patterns [see table 5]',[]),%QUA
	% remove triples based on pattern match (Table 5)
	(   forall((triple_remove(Pattern,Remove), test_use_owl(M,Pattern)),
	        forall(member(owl(S,P,O),Remove),use_owl(M,S,P,O,removed))) -> true ; true),


        % temporary fix to make up for bug in rdf parsing
        % see email to JanW July-1-2009
        forall((test_use_owl(M,S,P,BNode),
                atom(BNode),
                sub_atom(BNode,0,1,_,'_'),
                test_use_owl(M,BNode,'http://www.w3.org/1999/02/22-rdf-syntax-ns#datatype',literal(_))),
               (   use_owl(M,S,P,BNode,datatype_fix),
                   use_owl(M,BNode,'http://www.w3.org/1999/02/22-rdf-syntax-ns#datatype',literal(_)),
                   expand_and_assert(M,S,P,literal('')))),

	% replace matched patterns (Table 6)
        debug(owl_parser,'Replacing patterns [see table 6]',[]),
	(   setof(ReplaceWith,
                  Pattern^(   triple_replace(Pattern,ReplaceWith), % +Triples:list, ?Triples:list
                              use_owl(M,Pattern),
                              debug(owl_parser,'Replacing ~w ==> ~w [see table 6]',[Pattern,ReplaceWith])),
                  ReplacementSetList)
        ->  forall((member(ReplacementSet,ReplacementSetList),member(owl(S,P,O),ReplacementSet)),
                   expand_and_assert(M,S,P,O))
        ;   debug(owl_parser,'No replacements required',[])),

        /*
	forall(triple_replace(Pattern,ReplaceWith),
               forall(use_owl(M,Pattern),
                      forall(member(owl(S,P,O),ReplaceWith),
                             (   expand_and_assert(M,S,P,O),
                                 debug(owl_parser,'Replacing ~w ==> ~w [see table 6]',[Pattern,owl(S,P,O)]))))),
        */

	% continue with parsing using the rules...
	% Table 8, get the set of RIND - anonymous individuals in reification
	findall(X, (member(Y,['owl:Axiom','owl:Annotation',
			      'owl:AllDisjointClasses','owl:AllDisjointProperties',
			      'owl:AllDifferent','owl:NegativePropertyAssertion']),
                    test_use_owl(M,X,'rdf:type',Y)
                   ),
                RIND),
	set_trdf(rind,RIND),

        % Table 9, row 5
	% VV 10/3/2010 get the annotation properties before collecting the annotations.
        debug(owl_parser,'asserting annotationProperty/1 for all APs',[]),
	forall( test_use_owl(M,D,'rdf:type','owl:AnnotationProperty'),
		assert_axiom(M,annotationProperty(D))),

        % TODO - make this faster
        debug(owl_parser,'Implements function ANN(x) 3.2.2 Table 10.',[]),
	findall(_,ann(M,_,_),_), % find all annotations, assert annotation(X,AP,AV) axioms.

        debug(owl_parser,'Commencing parse of annotated axioms',[]),
        forall((axiompred(PredSpec),\+dothislater(PredSpec),\+omitthis(PredSpec)),
               owl_parse_annotated_axioms(M,PredSpec)),
        forall((axiompred(PredSpec),dothislater(PredSpec),\+omitthis(PredSpec)),
               owl_parse_annotated_axioms(M,PredSpec)),

	% annotated complex axioms, s.a., equivalentClasses([a,intersectionOf(..)]) that are
	% seen in axiom_r_node as axiom_r_node(a,intersectionOf,_:DescriptionX,_:DescriptionY)
	
	

        debug(owl_parser_detail,'Commencing parse of unannotated axioms',[]),
        forall((axiompred(PredSpec),\+dothislater(PredSpec),\+omitthis(PredSpec)),
               owl_parse_nonannotated_axioms(M,PredSpec)),
        forall((axiompred(PredSpec),dothislater(PredSpec),\+omitthis(PredSpec)),
               owl_parse_nonannotated_axioms(M,PredSpec)),!,
   
	% annotation Assertion
	parse_annotation_assertions(M),
	forall(owl_parse_compatibility_DL(M,Axiom),assert_axiom(M,Axiom)),
	owl_canonical_parse_3(M,Rest).

omitthis(ontology/1).


owl_parse_annotated_axioms(M,Pred/Arity) :-
        debug(owl_parser_detail,'[ann] Parsing all of type: ~w',[Pred]),
        functor(Head,Pred,Arity),
%        forall(owl_parse_axiom(M,Mod:Head),
%               (   debug(owl_parser_detail,' parsed: [~w] ~w',[Mod,Head]),
%                   assert(Mod:Head))).
	forall(owl_parse_axiom(M,Head,true,Annotations),
	       (   assert_axiom(M,Head),
	           debug(owl_parser_detail_anns,' parsed: ~w : anns: ~w',[Head,Annotations]),
		   forall(member(X,Annotations),
			  forall(M:aNN(X,AP,AV),
				 assert_axiom(M,annotation(Head,AP,AV))
		          )
			 )
	       )
	      ),
        debug(owl_parser_detail,'[ann] Done parsing all of type: ~w',[Pred]).

owl_parse_nonannotated_axioms(M,Pred/Arity) :-
        debug(owl_parser_detail,'[unann] Parsing all of type: ~w',[Pred]),
        functor(Head,Pred,Arity),
	forall(owl_parse_axiom(M,Head,false,_),
	       assert_axiom(M,Head)
	      ).



%%       rdf_load_stream(+URL, -Ontology, -BaseURI, -Imports:list) is det
%
%	This predicate calls the rdf parser to parse the RDF/XML URL
%	into RDF triples. URL can be a local file or a URL.
%	The predicate returns all Imports based on the 	owl:imports predicate.
%	Also the Ontology of the URL if an owl:Ontology exists, var
%	otherise.
%
%       If owl_repository/2 is defined, then this is used to map URLs
%       prior to loading.


rdf_load_stream(URL,Ontology,BaseURI,Imports) :-
        owl_repository(URL,RURL),
        !,
        % note: users responsibility to avoid infinite loops by avoid cycles in repository mappings!
        rdf_load_stream(RURL,Ontology,BaseURI,Imports).

rdf_load_stream(URL,Ontology,BaseURI,Imports) :-
	BaseURI = URL,
  	(   sub_atom(URL,0,4,_,'http')
        ->  catch((http_open(URL,RDF_Stream,[]),
	      rdf_load(RDF_Stream,[if(true),base_uri(BaseURI),blank_nodes(noshare),
				   result(Action, Triples, MD5),register_namespaces(true)]),
		   debug(owl_parser,' Loaded ~w stream: ~w Action: ~w Triples:~w MD5: ~w',[URL,RDF_Stream,Action,Triples,MD5]),
                   close(RDF_Stream)),
                  Message,
                  throw(io_error(URL,'rdf_load/2 failed',Message))) % re-throw with more information
        ;  RDF_Stream = URL, rdf_load(RDF_Stream,[blank_nodes(noshare),if(true),base_uri(BaseURI),register_namespaces(true)])
	),
        % collect all imports directives
	(   rdf(Ontology,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#Ontology',BaseURI:_)
        ->  findall(I,rdf(Ontology,'http://www.w3.org/2002/07/owl#imports',I,BaseURI:_),Imports)
	;   Imports = []
	).



% ----------------------------------------------------------------
% 3 Mapping from RDF Graphs to the Structural Specification
% ----------------------------------------------------------------

/*

  This section specifies the results of steps CP-2.2 and CP-3.3 of the
  canonical parsing process from Section 3.6 of the OWL 2
  Specification [OWL 2 Specification] on an ontology document D that
  can be parsed into an RDF graph G. ...

  */

%       owl_description_list(+Module,+Node, -List)
%
%       If +Node is defined as rdf:type rdf:List, then List returns
%       a prolog list of descriptions for this Node.

owl_description_list(_M,'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.

owl_description_list(M,X,[F|R]) :-
	% use_owl(M,X,'rdf:type','rdf:List',list), % this is now removed from graph
	use_owl(M,X,'rdf:first',Element,first),
	owl_description(M,Element,F),
	use_owl(M,X,'rdf:rest',Y,rest),
	!,owl_description_list(M,Y,R).


%       owl_individual_list(+Module,+Node, -List)
%
%       If +Node is defined as rdf:type rdf:List, then List returns
%       a prolog list of individuals for this Node.

owl_individual_list(_M,'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.

owl_individual_list(M,X,[F|R]) :-
	% use_owl(M,X,'rdf:type','rdf:List',list), % this is now removed from graph
	use_owl(M,X,'rdf:first',F,first),
	use_owl(M,X,'rdf:rest',Y,rest),
	!,owl_individual_list(M,Y,R).

%       owl_property_list(+Module,+Node, -List)
%
%       If +Node is defined as rdf:type rdf:List, then List returns
%       a prolog list of properties for this Node.

owl_property_list(_M,'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.

owl_property_list(M,X,[F|R]) :-
	% use_owl(M,X,'rdf:type','rdf:List',list), % this is now removed from graph
	use_owl(M,X,'rdf:first',Element,first),
	owl_property_expression(M,Element,F),
	use_owl(M,X,'rdf:rest',Y,rest),
	!,owl_property_list(M,Y,R).

%       owl_datarange_list(+Module,+Node, -List)
%
%       If +Node is defined as rdf:type rdf:List, then List returns
%       a prolog list of dataranges for this Node.

owl_datarange_list(_,'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.

owl_datarange_list(M,X,[F|R]) :-
	% use_owl(M,X,'rdf:type','rdf:List',list), % this is now removed from graph
	use_owl(M,X,'rdf:first',Element,first),
	owl_datarange(M,Element,F),
	use_owl(M,X,'rdf:rest',Y,rest),
	!,owl_datarange_list(M,Y,R).

%       owl_datatype_restriction_list(+Node, -List)
%
%       If +Node is defined as rdf:type rdf:List, then List returns
%       a prolog list of datatype restrictions for this Node.

owl_datatype_restriction_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.

owl_datatype_restriction_list(X,[facetRestriction(W2,L)|R]) :-
	% use_owl(M,X,'rdf:type','rdf:List'), % this is now removed from graph
	use_owl(M,X,'rdf:first',Element,first_datatype_restr),
	use_owl(M,Element,W,L,datatype_restr),
	(   concat_atom([_,W2],'#',W)
	->  true
	;   W2=W),
	use_owl(M,X,'rdf:rest',Y,rest_datatype_restr),
	!,owl_datatype_restriction_list(Y,R).


% 3.1 Extracting Declarations and the IRIs of the Directly Imported Ontology Documents
% This section specifies the result of step CP-2.2 of the canonical parsing process on an RDF graph G


% 3.1.2 Parsing of the Ontology Header and Declarations

%  Table 4.
owl_parse_axiom(M,ontology(O),AnnMode,List) :-
        test_use_owl(M,O,'rdf:type','owl:Ontology'),
	\+ test_use_owl(M,[owl(U,_W,O),owl(U,'rdf:type','owl:Ontology')]),
	valid_axiom_annotation_mode(AnnMode,M,O,'rdf:type','owl:Ontology',List),
        use_owl(M,O,'rdf:type','owl:Ontology',ontology),
        set_trdf(current_ontology,O),
	forall(use_owl(M,O,'owl:imports',IRI,ontology_import), assert_axiom(M,ontologyImport(O,IRI))),
	forall(use_owl(M,O,'owl:versionInfo',IRI2,ontology_version_info), assert_axiom(M,ontologyVersionInfo(O,IRI2))),!. % Do Once


% See table 5.
% triple_remove(Pattern:list,Remove:list)
% if Pattern is present, remove triples in Remove
triple_remove([owl(X,'rdf:type','owl:Ontology')],[owl(X,'rdf:type','owl:Ontology')]).
triple_remove([owl(X,'rdf:type','owl:Class'),owl(X,'rdf:type','rdfs:Class')],[owl(X,'rdf:type','rdfs:Class')]).
triple_remove([owl(X,'rdf:type','rdfs:Datatype'),owl(X,'rdf:type','rdfs:Class')],[owl(X,'rdf:type','rdfs:Class')]).
triple_remove([owl(X,'rdf:type','owl:DataRange'),owl(X,'rdf:type','rdfs:Class')],[owl(X,'rdf:type','rdfs:Class')]).
triple_remove([owl(X,'rdf:type','owl:Restriction'),owl(X,'rdf:type','rdfs:Class')],[owl(X,'rdf:type','rdfs:Class')]).
triple_remove([owl(X,'rdf:type','owl:Restriction'),owl(X,'rdf:type','owl:Class')],[owl(X,'rdf:type','owl:Class')]).
triple_remove([owl(X,'rdf:type','owl:ObjectProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).
triple_remove([owl(X,'rdf:type','owl:FunctionalProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).
triple_remove([owl(X,'rdf:type','owl:InverseFunctionalProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).
triple_remove([owl(X,'rdf:type','owl:TransitiveProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).
triple_remove([owl(X,'rdf:type','owl:DatatypeProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).
triple_remove([owl(X,'rdf:type','owl:AnnotationProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).
triple_remove([owl(X,'rdf:type','owl:OntologyProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).
triple_remove([owl(X,'rdf:type','rdf:List'),owl(X,'rdf:first',_Y),owl(X,'rdf:rest',_Z)],[owl(X,'rdf:type','rdf:List')]).
/*
   triple_remove([owl(X,'rdf:type','owl:Thing')],[owl(X,'rdf:type','owl:Thing')]).
*/
% See table 6.
% http://www.w3.org/TR/2008/WD-owl2-mapping-to-rdf-20081202/
triple_replace([owl(X,'rdf:type','owl:OntologyProperty')],[owl(X,'rdf:type','owl:AnnotationProperty')]).
triple_replace([owl(X,'rdf:type','owl:InverseFunctionalProperty')],[owl(X,'rdf:type','owl:ObjectProperty'),owl(X,'rdf:type','owl:InverseFunctionalProperty')]).
triple_replace([owl(X,'rdf:type','owl:TransitiveProperty')],[owl(X,'rdf:type','owl:ObjectProperty'),owl(X,'rdf:type','owl:TransitiveProperty')]).
triple_replace([owl(X,'rdf:type','owl:SymmetricProperty')],[owl(X,'rdf:type','owl:ObjectProperty'),owl(X,'rdf:type','owl:SymmetricProperty')]).

% NOTE: this is not specified in table 6. However, we treat rdfs:Classes as equivalent to owl:Classes
triple_replace([owl(X,'rdf:type','rdfs:Class')],[owl(X,'rdf:type','owl:Class')]).

% DECLARATIONS
%
% See table 7.
% http://www.w3.org/TR/2008/WD-owl2-mapping-to-rdf-20081202/

%% owl_parse_axiom(+Module,+AxiomSpec,+AnnMode:boolean,?AnnList:list) is det
%
% None
%
owl_parse_axiom(M,class(C),AnnMode,List) :-
	test_use_owl(M,C,'rdf:type','owl:Class'),
	valid_axiom_annotation_mode(AnnMode,M,C,'rdf:type','owl:Class',List),
        (   use_owl(M,C,'rdf:type','owl:Class',named,class(C)) -> true ; use_owl(M,C,'rdf:type','rdfs:Class',named,class(C))),
	\+ M:class(C).


owl_parse_axiom(M,datatype(D), AnnMode, List) :-
        test_use_owl(M,D,'rdf:type','rdf:Datatype'),
        valid_axiom_annotation_mode(AnnMode,M,D,'rdf:type','rdf:Datatype',List),
        use_owl(M,D,'rdf:type','rdf:Datatype',datatype(D)).


owl_parse_axiom(M,objectProperty(D), AnnMode, List) :-
        test_use_owl(M,D,'rdf:type','owl:ObjectProperty'),
        valid_axiom_annotation_mode(AnnMode,M,D,'rdf:type','owl:ObjectProperty',List),
        use_owl(M,D,'rdf:type','owl:ObjectProperty',objectProperty(D)),
	\+ M:objectProperty(D).


% note the difference in names between syntax and rdf
owl_parse_axiom(M,dataProperty(D), AnnMode, List) :-
        test_use_owl(M,D,'rdf:type','owl:DatatypeProperty'),
        valid_axiom_annotation_mode(AnnMode,M,D,'rdf:type','rdf:DatatypeProperty',List),
        use_owl(M,D,'rdf:type','owl:DatatypeProperty',dataProperty(D)),
	\+ M:dataProperty(D).

owl_parse_axiom(M,annotationProperty(D), AnnMode, List) :-
        test_use_owl(M,D,'rdf:type','owl:AnnotationProperty'),
        valid_axiom_annotation_mode(AnnMode,M,D,'rdf:type','rdf:AnnotationProperty',List),
        use_owl(M,D,'rdf:type','owl:AnnotationProperty',annotationProperty(D)),
	\+ M:annotationProperty(D).


% TODO: check this. do we need to assert individual axioms if all we have is an rdf:type?
owl_parse_axiom(M,namedIndividual(D), AnnMode, List) :-
        test_use_owl(M,D,'rdf:type','owl:NamedIndividual'),
        valid_axiom_annotation_mode(AnnMode,M,D,'rdf:type','rdf:NamedIndividual',List),
        use_owl(M,D,'rdf:type','owl:NamedIndividual',namedIndividual(D)).


% Table 8. Identifying Anonymous Individuals in Reification
% TODO


% 3.2 Populating an Ontology


% 3.2.1 Analyzing Declarations

% 3.2.2 Parsing of Annotations

%
%       ann(+Module,?X, -Extension List)
%
%       Implements function ANN(x) 3.2.2 Table 10
%
%     The annotations in G are parsed next. The function ANN assigns a
%     set of annotations ANN(x) to each IRI or blank node x. This
%     function is initialized by setting ANN(x) = a.. for each each IRI
%     or blank node x. Next, the triple patterns from Table 10 are
%     matched in G and, for each matched pattern, ANN(x) is extended
%     with an annotation from the right column. Each time one of these
%     triple patterns is matched, the matched triples are removed from
%     G. This process is repeated until no further matches are
%     possible

ann(M,X,Y) :-
	ann(M,X,X,Y).



ann(M,X,X1, annotation(X1,Y,Z)) :-
	M:annotationProperty(Y),
        debug(owl_parser_detail,'annotation property: ~w',[Y]),
        M:owl(X,Y,Z,not_used),
        use_owl(M,X,Y,Z,annotationProperty(Y)),
	u_assert(M,aNN(X1,Y,Z)),
	ann2(M,X,Y,Z,X1).


ann2(M,X,Y,Z,X1) :-
	M:annotation_r_node(X,Y,Z,W),
	ann(M,W,annotation(X1,Y,Z),Term),
        u_assert(M,Term).

ann2(M,X,Y,Z,X1) :-
	M:axiom_r_node(X,Y,Z,W),
	ann(M,W,annotation(X1,Y,Z),Term),
        u_assert(M,Term).


ann2(_,_,_,_,_).


% 3.2.4 Parsing of Expressions

is_bnode(C) :-
	atom(C),
	sub_atom(C,0,1,_,'_').


	% Table 11. Parsing Object Property Expressions
owl_property_expression(_M,C,C) :-
	\+ is_bnode(C), % better: IRI(C).
	% VV added 10/3/2011
	C\='http://www.w3.org/1999/02/22-rdf-syntax-ns#first',
	C\='http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',
        !.

owl_property_expression(M,C,D) :-
	M:blanknode(C,D,Use),
	(   Use = used,
	    retractall(M:blanknode(C,D,used)),
	    assert(M:blanknode(C,D,shared))
	;
	    true).

owl_property_expression(M,P,inverseOf(Q)) :-
        use_owl(M,P,'owl:inverseOf',Q,inverseof(P,Q)),
        owl_get_bnode(M,P,inverseOf(Q)).


% Table 12. Parsing of Data Ranges

owl_datarange(_M,D,D) :-
	\+ is_bnode(D),!.  % better: IRI(C).

owl_datarange(M,C,D) :-
	M:blanknode(C,D,Use),
	(   Use = used,
	    retractall(M:blanknode(C,D,used)),
	    assert(M:blanknode(C,D,shared))
	;
	true).

owl_datarange(M,D,intersectionOf(L)) :-
	use_owl(M,D,'rdf:type','rdfs:Datatype',datarange(D)),
	use_owl(M,D,'owl:intersectionOf',Y,datarange(D)),
	%print(D-inter-Y),nl,
        owl_datarange_list(M,Y,L),
	owl_get_bnode(M,D,intersectionOf(L)).

owl_datarange(M,D,unionOf(L)) :-
	use_owl(M,D,'rdf:type','rdfs:Datatype',datarange(D)),
	use_owl(M,D,'owl:unionOf',Y,datarange(D)),
        owl_datarange_list(M,Y,L),
	owl_get_bnode(M,D,unionOf(L)).


owl_datarange(M,D,complementOf(DY)) :-
	use_owl(M,D,'rdf:type','rdfs:Datatype',dataRange(D)),
	use_owl(M,D,'owl:datatypeComplementOf',Y,datacomplement(D)),
        owl_datarange(M,Y,DY),
	owl_get_bnode(M,D,complementOf(DY)).

% Table 14, case 2
 owl_datarange(M,D,complementOf('rdfs:Literal')) :-
	use_owl(M,D,'rdf:type','rdfs:DataRange',dataRange(D)),
	use_owl(M,D,'owl:oneOf',[],oneOf(D)),
	owl_get_bnode(M,D,complementOf('rdfs:Literal')).

owl_datarange(M,D,oneOf(L)) :-
	use_owl(M,D,'rdf:type','rdfs:Datatype',dataType(D)),
	use_owl(M,D,'owl:oneOf',L1,oneOf(D)),
	owl_individual_list(M,L1,L),
	owl_get_bnode(M,D,oneOf(L)).

% Table 14, case 1
owl_datarange(M,D,oneOf(L)) :-
	use_owl(M,D,'rdf:type','rdfs:DataRange',datarange(D)),
	use_owl(M,D,'owl:oneOf',L1,datarange(D)),
	owl_individual_list(M,L1,L),
	owl_get_bnode(M,D,oneOf(L)).


owl_datarange(M,D,datatypeRestriction(DY,L)) :-
	use_owl(M,D,'rdf:type','rdfs:Datatype',datarange(D)),
	use_owl(M,D,'owl:onDatatype',Y,datarange(D)),
	owl_datarange(M,Y,DY),
	use_owl(M,D,'owl:withRestrictions',L1,datarange(D)),
	owl_datatype_restriction_list(L1,L),
	owl_get_bnode(M,D,datatypeRestriction(DY,L)).

% Table 13. Parsing of Class Expressions

% ----------------------------------------------------------------------
%       owl_description(+Module,+Node,-Description).
%
%	It implements OWL AS production rules for Descriptions.
%         During the construction of the Description any blank node
%         is recorded for later structure sharing checks.

owl_description(_M,C,C) :-
	\+ is_bnode(C),!. % better: IRI(C).


owl_description(M,C,D) :-
	M:blanknode(C,D,Use),
	(   Use = used,
	    retractall(M:blanknode(C,D,used)),
	    assert(M:blanknode(C,D,shared))
	;
	    true),!.

% TODO: this leaves behind classAssertions of type owlClass for the bnodes
owl_description(M,D,intersectionOf(L)) :-
	use_owl(M,D,'owl:intersectionOf',L1,intersectionOf(D)),
	owl_description_list(M,L1,L),
	\+L = [],
	owl_get_bnode(M,D,intersectionOf(L)),!.

owl_description(M,D,unionOf(L)) :-
	use_owl(M,D,'owl:unionOf',L1,union(D)),
	owl_description_list(M,L1,L),
	owl_get_bnode(M,D,unionOf(L)),!.


owl_description(M,D,complementOf(Descr)) :-
	use_owl(M,D,'owl:complementOf',D1,complementOf(D)),
	owl_description(M,D1,Descr),
	owl_get_bnode(M,D,complementOf(Descr)),!.

owl_description(M,D,oneOf(L)) :-
	use_owl(M,D,'owl:oneOf',L1,oneOf(D)),
	(   use_owl(M,D,'rdf:type','owl:Class',oneOf(D,L)) ; true),
	owl_individual_list(M,L1,L),
	owl_get_bnode(M,D,oneOf(L)),!.

owl_description(M,D,datatypeRestriction(DY,L)) :-
	use_owl(M,D,'rdf:type','rdfs:Datatype',datatypeRestr(D)),
	use_owl(M,D,'owl:onDatatype',Y,dataType(D)),
	owl_datarange(M,Y,DY),
	use_owl(M,D,'owl:withRestrictions',L1,withRestrictions(D)),
	owl_datatype_restriction_list(L1,L),
	owl_get_bnode(M,D,datatypeRestriction(DY,L)).

owl_description(M,D,Restriction) :-
	owl_restriction(M,D, Restriction),
	owl_get_bnode(M,D,Restriction),!.


% Table 15 - OWL DL compatibility class expressions
%
owl_description(M,D,Result) :-
	\+ is_bnode(D), % better: IRI(C).
	use_owl(M,D,'rdf:type','owl:Class',description(D)),
	use_owl(M,D,'owl:unionOf',L,unionOf(L)),
	owl_description_list(M,L,DL),
	(   DL = [], Result = 'owl:Nothing' ;
	    DL = [D1], Result = D1),
	owl_get_bnode(M,D,Result),!.

owl_description(M,D,Result) :-
	\+ is_bnode(D), % better: IRI(C).
	use_owl(M,D,'rdf:type','owl:Class',dl_compatibility_descr(D)),
	use_owl(M,D,'owl:intersectionOf',L,intersectionOf(D)),
	owl_description_list(M,L,DL),
	(   DL = [], Result = 'owl:Thing' ;
	    DL = [D1], Result = D1),
	owl_get_bnode(M,D,Result),!.

owl_description(M,D,Result) :-
	\+ is_bnode(D),!, % better: IRI(C).
	use_owl(M,D,'rdf:type','owl:Class',dl_compatibility_descr(D)),
	use_owl(M,D,'owl:oneOf',[],oneOf(D)),
	Result = 'owl:Nothing',
	owl_get_bnode(M,D,Result).

% support older deprecated versions of OWL2 spec. See for example hydrology.owl
onClass(M,E,D) :- use_owl(M,E,'http://www.w3.org/2006/12/owl2#onClass',D,onClass(E)).
onClass(M,E,D) :- use_owl(M,E,'owl:onClass',D,onClass(E)).

onDataRange(M,E,D) :- use_owl(M,E, 'owl:onDataRange',D,onDatarange(E)).


%       owl_restriction(+Module,+Element,-Restriction).
%
%       If Element is defined as a owl:Restriction on property P then
%       Restriction binds to a restriction(Property,Type) term,
%	according to OWL Abstract syntax specification.

owl_restriction(M,Element,Restriction) :-
	use_owl(M,Element,'rdf:type','owl:Restriction',restriction(Element)),
	(   use_owl(M,Element, 'owl:onProperty',PropertyID,onProperty(Element,PropertyID)) ;
    	    use_owl(M,Element, 'owl:onProperties',PropertyID,onProperties(Element,PropertyID))
	),
	owl_restriction_type(M,Element,PropertyID, Restriction),
        debug(owl_parser_detail,'Restriction: ~w',[Restriction]).



owl_restriction_type(M,E, P, someValuesFrom(PX, DX)) :-
	use_owl(M,E, 'owl:someValuesFrom',D,someValuesFrom(E,P)),
	(   owl_description(M,D, DX) ; owl_datarange(M,D,DX)),
        (   P = [_|_], owl_property_list(M,P,PX) ;  owl_property_expression(M,P, PX)).


owl_restriction_type(M,E, P, allValuesFrom(PX,DX)) :-
	use_owl(M,E, 'owl:allValuesFrom',D,allValuesFrom(E,P)),
	(   owl_description(M,D, DX) ; owl_datarange(M,D,DX)),
        (   P = [_|_], owl_property_list(M,P,PX) ;  owl_property_expression(M,P, PX)).


% changed from thea value-->hasValue
owl_restriction_type(M,E, P, hasValue(PX,Value)) :-
	use_owl(M,E, 'owl:hasValue',Value,hasValue(E)),
        owl_property_expression(M,P, PX).

% VV:check if RDF parser returns a triple with O=true for
owl_restriction_type(M,E, P, hasSelf(PX)) :-
	use_owl(M,E, 'owl:hasSelf', true,hasSelf(E)),
        owl_property_expression(M,P, PX).

% Support of deprecated translations:
% in the OWL2 RDF mapping, unqualified CRs use owl:{min,max}Cardinality
% and QCQs use owl:{min,ax}QualifiedCardinality
%
% however, there appear to be some ontologies; e.g. Hydrology.owl.
% that use an older mapping, where the same properties are used
% for QCR and unqCR
%
% it is relatively easy to support this legacy ontologies; however
% we must process these BEFORE unqualified cardinality restrictions.

owl_restriction_type(M,E, P, exactCardinality(N,PX,DX)) :-
	test_use_owl(M,E, 'owl:cardinality',Lit),
        onClass(M,E,D),
	owl_description(M,D, DX),!,
	use_owl(M,E, 'owl:cardinality',Lit,cardinality(E)),
        literal_integer(Lit,N),
        owl_property_expression(M,P, PX).

owl_restriction_type(M,E, P, minCardinality(N,PX,DX)) :-
	test_use_owl(M,E, 'owl:minCardinality',Lit),
        (   onClass(M,E,D),owl_description(M,D, DX)
        ;   onDataRange(M,E,D), owl_datarange(M,D,DX)),
	!,
        % we are sure this is an old-style unqualified CR - now consume triples
	use_owl(M,E, 'owl:minCardinality',Lit,minCardinality(E)),
        literal_integer(Lit,N),
        owl_property_expression(M,P, PX).

owl_restriction_type(M,E, P, maxCardinality(N,PX,DX)) :-
	test_use_owl(M,E, 'owl:maxCardinality',Lit),
        (   onClass(M,E,D),owl_description(M,D, DX)
        ;   onDataRange(M,E,D), owl_datarange(M,D,DX)),
	!,
        % we are sure this is an old-style unqualified CR - now consume triples
	use_owl(M,E, 'owl:maxCardinality',Lit,maxCard(E)),
        literal_integer(Lit,N),
        owl_property_expression(M,P, PX).

% END OF Support of deprecated translations:

% the following are all in the spec:

% changed from Thea1->2: cardinality->exactCardinality
owl_restriction_type(M,E, P,exactCardinality(N,PX)) :-
	use_owl(M,E, 'owl:cardinality',Lit,cardinality(E)),
        literal_integer(Lit,N),
        owl_property_expression(M,P, PX).

owl_restriction_type(M,E, P,exactCardinality(N,PX,DX)) :-
	use_owl(M,E, 'owl:qualifiedCardinality',Lit),literal_integer(Lit,N),
	(   onClass(M,E,D),owl_description(M,D, DX) ;
	    onDataRange(M,E,D), owl_datarange(M,D,DX)
	),
        owl_property_expression(M,P, PX).


owl_restriction_type(M,E, P, minCardinality(N,PX)) :-
	use_owl(M,E, 'owl:minCardinality',Lit,cardinality(E)),literal_integer(Lit,N),
        owl_property_expression(M,P, PX).

owl_restriction_type(M,E, P, minCardinality(N,PX,DX)) :-
	use_owl(M,E, 'owl:minQualifiedCardinality',Lit,cardinality(E)),literal_integer(Lit,N),
	(   onClass(M,E,D),owl_description(M,D, DX);
	    onDataRange(M,E,D), owl_datarange(M,D,DX)
	),
        owl_property_expression(M,P, PX).


owl_restriction_type(M,E, P, maxCardinality(N,PX)) :-
	use_owl(M,E, 'owl:maxCardinality',Lit,maxCardinality(E)),literal_integer(Lit,N),
        owl_property_expression(M,P, PX).

owl_restriction_type(M,E, P, maxCardinality(N,PX,DX)) :-
	use_owl(M,E, 'owl:maxQualifiedCardinality',Lit,cardinality(E,Lit)),
	literal_integer(Lit,N),
	(   onClass(M,E,D),owl_description(M,D, DX);
	    onDataRange(M,E,D), owl_datarange(M,D,DX)),
        owl_property_expression(M,P, PX).


% Table 14. Parsing of Data Ranges for Compatibility with OWL DL
% Included into owl_datarange clauses above

% Table 15. Parsing of Class Expressions for Compatibility with OWL DL
% Included into owl_dexcription clauses above

% Table 16. Parsing of Axioms without Annotations
% Declarations handled previously
% CLASS AXIOMS
% valid_axiom_annotation_mode: add clauses for the disjoint etc ....

collect_r_nodes(M) :-
	retractall(M:axiom_r_node(_,_,_,_)),
	forall(( test_use_owl(M,Node,'rdf:type','owl:Axiom'),
		 test_use_owl(M,Node,'owl:annotatedSource',S),
		 test_use_owl(M,Node,'owl:annotatedProperty',P),
		 test_use_owl(M,Node,'owl:annotatedTarget',O)),
	       (assert(M:axiom_r_node(S,P,O,Node)),
	        assert(M:owl(S,P,O,not_used)),
                debug(owl_parser_detail,'~w',[axiom_r_node(S,P,O,Node)]),
		use_owl(M,[owl(Node,'rdf:type','owl:Axiom'),
			 owl(Node,'owl:annotatedSource',S),
			 owl(Node,'owl:annotatedProperty',P),
			 owl(Node,'owl:annotatedTarget',O)]))),

	retractall(M:annotation_r_node(_,_,_,_)),
	forall(( test_use_owl(M,W,'rdf:type','owl:Annotation'),
		 test_use_owl(M,W,'owl:annotatedSource',S),
		 test_use_owl(M,W,'owl:annotatedProperty',P),
		 test_use_owl(M,W,'owl:annotatedTarget',O)),
	       (assert(M:annotation_r_node(S,P,O,Node)),
                debug(owl_parser_detail,'~w',[annotation_r_node(S,P,O,Node)]),
		use_owl(M,[owl(W,'rdf:type','owl:Annotation'),
			 owl(W,'owl:annotatedSource',S),
			 owl(W,'owl:annotatedProperty',P),
			 owl(W,'owl:annotatedTarget',O)]))).

%% valid_axiom_annotation_mode(+AnnMode,+S,+P,+O,?AnnotationNodes:list) is det
% if AnnMode is true and annotation triples can be found then
% unify AnnotationNodes with the Nodes that annotate the triple,
% otherwise []

valid_axiom_annotation_mode(true,M,S,P,O,List) :-
        expand_ns(P,PE),
        findall(Node,M:axiom_r_node(S,PE,O,Node),List).

valid_axiom_annotation_mode(false,_M,_S,_P,_O,[]).


owl_parse_axiom(M,subClassOf(DX,DY),AnnMode,List) :-
	test_use_owl(M,X,'rdfs:subClassOf',Y),
	valid_axiom_annotation_mode(AnnMode,M,X,'rdfs:subClassOf',Y,List),
	use_owl(M,X,'rdfs:subClassOf',Y,subclassOf(X,Y)),
        owl_description(M,X,DX),
	owl_description(M,Y,DY).

% Process each equivalentClass pair separately in order to capture
% annotations. Block the maximally connected subgraph.
% TODO. Process the equivalent(L) axioms to generate maximally connected
% equivalentClasses(L) axioms. (but without annotations?)

owl_parse_axiom(M,equivalentClasses(DL),AnnMode,List) :-
	test_use_owl(M,X,'owl:equivalentClass',Y),
	valid_axiom_annotation_mode(AnnMode,M,X,'owl:equivalentClass',Y,List),
	use_owl(M,X,'owl:equivalentClass',Y,equivalentClass(X,Y)),
        % maximally_connected_subgraph_over('owl:equivalentClass',L),
        maplist(owl_description(M),[X,Y],DL),
        debug(owl_parser_detail,'equivalentClasses Descs: ~w',[DL]).


owl_parse_axiom(M,equivalentClasses([C,intersectionOf(D)]),AnnMode,List) :-
	M:class(C),
	test_use_owl(M,C,'owl:intersectionOf',D1),
	debug(owl_parser,'equivalent collection; intersection for ~w',[C]),
	valid_axiom_annotation_mode(AnnMode,M,C,'owl:intersectionOf',D1,List),
	owl_description(M,C,intersectionOf(D)).

owl_parse_axiom(M,equivalentClasses([C,unionOf(D)]),AnnMode,List) :-
	M:class(C),
	test_use_owl(M,C,'owl:unionOf',D1),
	debug(owl_parser,'equivalent collection; union for ~w',[C]),
	valid_axiom_annotation_mode(AnnMode,M,C,'owl:unionOf',D1,List),
	owl_description(M,C,unionOf(D)).

owl_parse_axiom(M,equivalentClasses([C,oneOf(D)]),AnnMode,List) :-
	M:class(C),
	test_use_owl(M,C,'owl:oneOf',D1),
	debug(owl_parser,'equivalent collection; one of for ~w',[C]),
	valid_axiom_annotation_mode(AnnMode,M,C,'owl:oneOf',D1,List),
	owl_description(M,C,oneOf(D)).


owl_parse_axiom(M,equivalentClasses([C,D])) :-
        % TODO: this could be made more efficient by enforcing order of building
        (   test_use_owl(M,C,'rdf:type','owl:Class',named)
        ;   test_use_owl(M,C,'rdf:type','rdfs:Class',named)
        ;   M:class(C)),
        owl_description(M,C,D),
        C\=D.

% TODO. Process the disjointClasses(L) axioms to generate
% larger set of disjoint: ie if N classes are pairwise DisJoint
% then we can assert a disjointClasses for all N

owl_parse_axiom(M,disjointClasses([DX,DY]),AnnMode,List) :-
	test_use_owl(M,X,'owl:disjointWith',Y),
	valid_axiom_annotation_mode(AnnMode,M,X,'owl:disjointWith',Y,List),
	use_owl(M,X,'owl:disjointWith',Y,disjointWith(X,Y)),
        owl_description(M,X,DX),
	owl_description(M,Y,DY).

% One of the cases where annotations are those of _x and we do not seek
% for further annotation axioms. Par. 3.2.5.
% Whatever the AnnNode, _x is returned (will be ignored if mode false

owl_parse_axiom(M,disjointClasses(L),_AnnMode,[X]) :-
        % TODO: X may be referred to in an annotation axiom??
	use_owl(M,X,'rdf:type','owl:AllDisjointClasses',allDisjointClasses(X)),
        use_owl(M,X,'owl:members',L1,members(L1)),
        owl_description_list(M,L1,L).


owl_parse_axiom(M,disjointUnion(DX,DY),AnnMode,List) :-
	test_use_owl(M,X,'owl:disjointUnionOf',Y),
	valid_axiom_annotation_mode(AnnMode,M,X,'owl:disjointUnionOf',Y,List),
	use_owl(M,X,'owl:disjointUnionOf',Y,disjointUnionOf(X,Y)),
        owl_description(M,X,DX),
        owl_description_list(M,Y,DY).


% PROPERTY AXIOMS


% introduces bnode
owl_parse_axiom(M,subPropertyOf(propertyChain(PL),QX),AnnMode,List) :-
	test_use_owl(M,Q,'owl:propertyChainAxiom',L1),
	valid_axiom_annotation_mode(AnnMode,M,Q,'owl:propertyChainAxiom',L1,List),
	use_owl(M,Q,'owl:propertyChainAxiom',L1,propertyChainAxiom(Q)),
	owl_property_list(M,L1,PL),
        owl_property_expression(M,Q,QX).

owl_parse_axiom(M,subPropertyOf(PX,QX),AnnMode,List) :-
	test_use_owl(M,P,'rdfs:subPropertyOf',Q),
	valid_axiom_annotation_mode(AnnMode,M,P,'rdfs:subPropertyOf',Q,List),
	use_owl(M,P,'rdfs:subPropertyOf',Q,subPropertyOf(P,Q)),
        owl_property_expression(M,P,PX),
        owl_property_expression(M,Q,QX).


% Process each equivalentProperty pair separately in order to capture
% annotations. Block the maximally connected subgraph.
% TODO. Process the equivalent(L) axioms to generate maximally connected
% equivalentProperties(L) axioms. (but without annotations?)

owl_parse_axiom(M,equivalentProperties(OPEL),AnnMode,List) :-
	test_use_owl(M,X,'owl:equivalentProperty',Y),
	valid_axiom_annotation_mode(AnnMode,M,X,'owl:equivalentProperty',Y,List),
	use_owl(M,X,'owl:equivalentProperty',Y,equivProperty(X,Y)),
	% maximally_connected_subgraph_over('owl:equivalentProperty',L),
	maplist(owl_property_expression(M),[X,Y],OPEL).


% TODO. Process the disjointProperties(L) axioms to generate
% larger set of disjoint: ie if N properties are pairwise DisJoint
% then we can assert a disjointClasses for all N

owl_parse_axiom(M,disjointProperties([DX,DY]),AnnMode,List) :-
	test_use_owl(M,X,'owl:propertyDisjointWith',Y),
	valid_axiom_annotation_mode(AnnMode,M,X,'owl:propertyDisjointWith',Y,List),
	use_owl(M,X,'owl:propertyDisjointWith',Y,propertyDisjointWith(X,Y)),
        owl_description(M,X,DX),
	owl_description(M,Y,DY).

% One more of the cases where annotations are those of _x and we do not
% seek for further annotation axioms. Par. 3.2.5. Whatever the AnnNode,
% _x is returned (will be ignored if mode false)

owl_parse_axiom(M,disjointProperties(L),_AnnMode,[X]) :-
        % TODO: X may be referred to in an annotation axiom??
	use_owl(M,X,'rdf:type','owl:AllDisjointProperties',allDisjointProps(X,L1)),
        use_owl(M,X,'owl:members',L1,members(L1)),
        L1 = [_,_|_],           % length >= 2
        owl_property_list(M,L1,L).


owl_parse_axiom(M,propertyDomain(PX,CX),AnnMode,List) :-
	test_use_owl(M,P,'rdfs:domain',C),
	valid_axiom_annotation_mode(AnnMode,M,P,'rdfs:domain',C,List),
        use_owl(M,P,'rdfs:domain',C,domain(P,C)),
	(   M:annotationProperty(P),CX = C ;
	    owl_property_expression(M,P,PX),
	    owl_description(M,C,CX)
	).

% We need to distinguish here between object and data property
% Currently we first test if the range is a class, this means OPE
% otherwise if it is a datarange it means a DPE.
% Ideally we should also check possible declarations of OPE or DPE.

owl_parse_axiom(M,propertyRange(PX,CX),AnnMode,List) :-
	test_use_owl(M,P,'rdfs:range',C),
	valid_axiom_annotation_mode(AnnMode,M,P,'rdfs:range',C,List),
        use_owl(M,P,'rdfs:range',C,range(P,C)),
	(   M:annotationProperty(P) -> PX = P, CX = C ;
	    owl_property_expression(M,P,PX),
            (   owl_description(M,C,CX) -> true ; owl_datarange(M,C,CX))
	).

owl_parse_axiom(M,inverseProperties(PX,QX),AnnMode,List) :-
	test_use_owl(M,P,'owl:inverseOf',Q),
	valid_axiom_annotation_mode(AnnMode,M,P,'owl:inverseOf',Q,List),
	use_owl(M,P,'owl:inverseOf',Q,inverseOf(P,Q)),
        owl_property_expression(M,P,PX),
        owl_property_expression(M,Q,QX).

owl_parse_axiom(M,functionalProperty(P),AnnMode,List) :-
	test_use_owl(M,P,'rdf:type','owl:FunctionalProperty'),
	valid_axiom_annotation_mode(AnnMode,M,P,'rdf:type','owl:FunctionalProperty',List),
        use_owl(M,P,'rdf:type','owl:FunctionalProperty',functionalProperty(P)).

owl_parse_axiom(M,inverseFunctionalProperty(P),AnnMode,List) :-
	test_use_owl(M,P,'rdf:type','owl:InverseFunctionalProperty'),
	valid_axiom_annotation_mode(AnnMode,M,P,'rdf:type','owl:InverseFunctionalProperty',List),
        use_owl(M,P,'rdf:type','owl:InverseFunctionalProperty',inverseFunctionalProperty(P)).

owl_parse_axiom(M,reflexiveProperty(P),AnnMode,List) :-
	test_use_owl(M,P,'rdf:type','owl:ReflexiveProperty'),
	valid_axiom_annotation_mode(AnnMode,M,P,'rdf:type','owl:ReflexiveProperty',List),
        use_owl(M,P,'rdf:type','owl:ReflexiveProperty',reflexiveProperty(P)).

owl_parse_axiom(M,irreflexiveProperty(P),AnnMode,List) :-
	test_use_owl(M,P,'rdf:type','owl:IrreflexiveProperty'),
	valid_axiom_annotation_mode(AnnMode,M,P,'rdf:type','owl:IrreflexiveProperty',List),
        use_owl(M,P,'rdf:type','owl:IrreflexiveProperty',irreflexiveProperty(P)).

owl_parse_axiom(M,symmetricProperty(P),AnnMode,List) :-
	test_use_owl(M,P,'rdf:type','owl:SymmetricProperty'),
	valid_axiom_annotation_mode(AnnMode,M,P,'rdf:type','owl:SymmetricProperty',List),
        use_owl(M,P,'rdf:type','owl:SymmetricProperty',symmetricProperty(P)).

owl_parse_axiom(M,asymmetricProperty(P),AnnMode,List) :-
	test_use_owl(M,P,'rdf:type','owl:AsymmetricProperty'),
	valid_axiom_annotation_mode(AnnMode,M,P,'rdf:type','owl:AsymmetricProperty',List),
        use_owl(M,P,'rdf:type','owl:AsymmetricProperty',assymetricProperty(P)).

owl_parse_axiom(M,transitiveProperty(P),AnnMode,List) :-
	test_use_owl(M,P,'rdf:type','owl:TransitiveProperty'),
	valid_axiom_annotation_mode(AnnMode,M,P,'rdf:type','owl:TransitiveProperty',List),
	use_owl(M,P,'rdf:type','owl:TransitiveProperty',transitiveProperty(P)).

owl_parse_axiom(M,hasKey(CX,L),AnnMode,List) :-
	test_use_owl(M,C,'owl:hasKey',L1),
	valid_axiom_annotation_mode(AnnMode,M,C,'owl:hasKey',L1,List),
	use_owl(M,C,'owl:hasKey',L1,hasKey(C)),
	owl_description(M,C,CX),
        L1 = [_,_|_],           % length >= 2
        owl_property_list(M,L1,L).

% INDIVIDUALS

owl_parse_axiom(M,sameIndividual([X,Y]),AnnMode,List) :-
	test_use_owl(M,X,'owl:sameAs',Y),
	valid_axiom_annotation_mode(AnnMode,M,X,'owl:sameAs',Y,List),
	use_owl(M,X,'owl:sameAs',Y,sameAs(X,Y)).

owl_parse_axiom(M,differentIndividuals([X,Y]),AnnMode,List) :-
	test_use_owl(M,X,'owl:differentFrom',Y),
	valid_axiom_annotation_mode(AnnMode,M,X,'owl:differentFrom',Y,List),
	use_owl(M,X,'owl:differentFrom',Y,differentFrom(X,Y)).

owl_parse_axiom(M,differentIndividuals(L),_AnnMode,[X]) :-
	use_owl(M,X,'rdf:type','owl:AllDifferent',allDifferent(L)),
	use_owl(M,X,'owl:distinctMembers',L1,distinctMembers(L)),
        owl_individual_list(M,L1,L).

owl_parse_axiom(M,differentIndividuals(L),_AnnMode,[X]) :-
	use_owl(M,X,'rdf:type','owl:AllDifferent',allDifferent(X)),
	use_owl(M,X,'owl:members',L1,members(L)),
        owl_individual_list(M,L1,L).

% make sure this is done before fetching classAssertion/2;
% -- the annotationAssertion matching clause should preceded the classAssertion/2 matching clause
owl_parse_axiom(M,annotationAssertion('owl:deprecated', X, true),AnnMode,List) :-
	test_use_owl(M,X, 'rdf:type', 'owl:DeprecatedClass'),
	valid_axiom_annotation_mode(AnnMode,M,X,'rdf:type','owl:DeprecatedClass',List),
	use_owl(M,X, 'rdf:type', 'owl:DeprecatedClass',deprecatedClass(X)).

% make sure this is done before fetching propertyAssertion/3
% this clause should precede it
owl_parse_axiom(M,annotationAssertion('owl:deprecated', X, true),AnnMode,List) :-
	test_use_owl(M,X, 'rdf:type', 'owl:DeprecatedProperty'),
	valid_axiom_annotation_mode(AnnMode,M,X,'rdf:type','owl:DeprecatedProperty',List),
	use_owl(M,X, 'rdf:type', 'owl:DeprecatedProperty',deprecatedProperty(X)).

% Table 17. Parsing of Annotated Axioms

dothislater(annotationAssertion/3).
% TODO - only on unnannotated pass?
%

owl_parse_axiom(M,annotationAssertion(P,A,B),AnnMode,List) :-
        M:annotationProperty(P),
        test_use_owl(M,A,P,B),         % B can be literal or individual
        valid_axiom_annotation_mode(AnnMode,M,A,P,B,List),
        use_owl(M,A,P,B,annotationProperty(P)).


dothislater(classAssertion/2).
owl_parse_axiom(M,classAssertion(CX,X),AnnMode,List) :-
	test_use_owl(M,X,'rdf:type',C),
        C\='http://www.w3.org/2002/07/owl#DeprecatedClass',
	% note: some ontologies may include a rdf:type with no
	%  explicit class declaration. See testfiles/test_undeclared.owl
	%class(C),
	valid_axiom_annotation_mode(AnnMode,M,X,'rdf:type',C,List),
	use_owl(M,X,'rdf:type',C,classAssertion(CX,X)),
        % I added this to avoid class assertions for bNodes. Perhaps a better
        % way is to simply consume the owl4/ triple at the time of translating
        % the description? --CJM
        C\='http://www.w3.org/2002/07/owl#Class',
        %
        C\='http://www.w3.org/1999/02/22-rdf-syntax-ns#Property',
        owl_description(M,C,CX).

dothislater(propertyAssertion/3).
owl_parse_axiom(M,propertyAssertion(PX,A,BX),AnnMode,List) :-
        test_use_owl(M,A,P,B), % B can be literal or individual
        P\='http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
	% note: some ontologies may include a triples with no
	%  explicit property declaration. See testfiles/test_undeclared.owl
	%property(P),
	valid_axiom_annotation_mode(AnnMode,M,A,P,B,List),
        \+ M:annotationProperty(P), % these triples should have been removed before, during ann parsing
	owl_property_expression(M,P,PX), % can also be inverse
	% next line added by VV 9/3/2011 for Jochem Liem to support ID-lists as PA objects
	(   owl_individual_list(M,B,BX) -> true ; BX = B),
        use_owl(M,A,P,B,propertyAssertion(PX,A,BX)).


owl_parse_axiom(M,negativePropertyAssertion(PX,A,B),_,X) :-
        use_owl(M,X,'rdf:type','owl:NegativePropertyAssertion',negPropertyAssertion(PX,A,B)),
        use_owl(M,X,'owl:sourceIndividual',A,negPropertyAssertion(PX,A,B)),
        use_owl(M,X,'owl:assertionProperty',P,negPropertyAssertion(PX,A,B)),
        use_owl(M,X,'owl:targetValue',B,negPropertyAssertion(PX,A,B)),
        owl_property_expression(M,P,PX).


% process hooks; SWRL etc

% Parsing annotationAssertions
%

parse_annotation_assertions(M) :- 
	( M:trdf_setting(rind,RIND) -> true ; RIND = []),!,
	forall((M:aNN(X,AP,AV),findall( aNN(annotation(X,AP,AV),AP1,AV1),
				      M:aNN(annotation(X,AP,AV),AP1,AV1),ANN), \+member(X,RIND), \+name(X,[95, 58, 68, 101, 115, 99, 114, 105, 112, 116, 105, 111, 110|_])),
	       (   assert_axiom(M,annotationAssertion(AP,X,AV)),
		  %  VV 10/3/2010 keep annotation/3
		  % retract(annotation(X,AP,AV)),
		   forall(member(aNN(_,AP1,AV1),ANN),
			    assert_axiom(M,annotation(annotationAssertion(AP,X,AV),AP1,AV1))
			 )
	       )
	      ),
	% forall(aNN(X,Y,Z),assert(annotation(X,Y,Z))), VV remove 25/1/11
	% annotation/3 axioms created already during owl_parse_annotated_axioms/1
	retractall(M:aNN(_,_,_)).

% Table 18. Parsing of Axioms for Compatibility with OWL DL

owl_parse_compatibility_DL(M,equivalentClasses([CEX,complementOf(CEY)])) :-
	use_owl(M,X,'owl:complementOf',Y,eq_classes),
	owl_description(M,X,CEX),
	owl_description(M,Y,CEY).


owl_parse_compatibility_DL(M,equivalentClasses([CEX,CEY])) :-
	use_owl(M,X,'owl:unionOf',Y,eq_classes),
	owl_description(M,X,CEX),
	owl_description_list(M,Y,DL),
	(   DL = [] -> CEY = 'owl:Nothing' ; (DL=[CEY]->true;CEY = unionOf(DL))).

owl_parse_compatibility_DL(M,equivalentClasses([CEX,CEY])) :-
	use_owl(M,X,'owl:intersectionOf',Y,eq_classes),
	owl_description(M,X,CEX),
	owl_description_list(M,Y,DL),
	(   DL = [] -> CEY = 'owl:Thing' ; (DL=[CEY]->true;CEY = intersectionOf(DL))).

owl_parse_compatibility_DL(M,equivalentClasses([CEX,CEY])) :-
	use_owl(M,X,'owl:oneOf',Y,eq_classes),
	owl_description(M,X,CEX),
	owl_description_list(M,Y,DL),
	(   DL = [] -> CEY = 'owl:Nothing' ; CEY = oneOf(DL)).

% UTIL

%% maximally_connected_subgraph_over(+P,?ConnectedSets) is semidet
maximally_connected_subgraph_over(P,CSet):-
        maximally_connected_subgraph_over(P,[],CSetL),
        member(CSet,CSetL).

%% maximally_connected_subgraph_over(+P,+Used,?ListOfConnectedSets) is det
maximally_connected_subgraph_over(P,Used,[CSet|All]):-
        test_use_owl(M,X,P,Y), % seed
        \+ member(X,Used),
        \+ member(Y,Used),
        use_owl(M,X,P,Y,maximally_conected), % seed
        !,
        extend_set_over(P,[X,Y],CSet),
        append(CSet,Used,Used2),
        maximally_connected_subgraph_over(P,Used2,All).
maximally_connected_subgraph_over(_,_,[]).


% det
extend_set_over(P,L,L2):-
        member(X,L),
        test_use_owl(M,X,P,Y),
        \+ member(Y,L),
        use_owl(M,X,P,Y,extend_set_over),
        !,extend_set_over(P,[Y|L],L2).
extend_set_over(P,L,L2):-
        member(X,L),
        test_use_owl(M,Y,P,X),
        \+ member(Y,L),
        use_owl(M,Y,P,X,extend_set_over),
        !,extend_set_over(P,[Y|L],L2).
extend_set_over(_,L,L):- !.

literal_integer(literal(type,A),N) :- atom_number(A,N).
literal_integer(literal(type(_,A)),N) :- atom_number(A,N).

%% time_goal(+Goal,?Time)
%  calls Goal and unifies Time with the cputime taken
time_goal(Goal,Time):-
        statistics(cputime,T1), Goal,
        statistics(cputime,T2), Time is T2-T1.

timed_forall(Cond,Action) :-
        forall(Cond,
               (   time_goal(Action,Time),
                   debug(owl2_bench,'Goal: ~w Time:~w',[Action,Time]))).


/** <module> Translates an RDF database to OWL2 axioms
  ---+ Synopsis 1
==
:- use_module(bio(owl2_from_rdf)).
%
==
---+ Details
---++ Hooks
* owl_parse_axiom_hook/3
---+ See Also
The file owl2_from_rdf.plt has some examples
*/
%:- thread_local ns4query/1.

/**
 * load_owl(++FileName:kb_file_name) is det
 *
 * The predicate loads the knowledge base contained in the given file. 
 * The knowledge base must be defined in pure OWL/RDF format.
 */
load_owl(String):-
  get_module(M),
  retractall(M:ns4query(_)),
  open(String,read,S),
  load_owl_from_stream(S),!.
  
/**
 * load_owl_from_string(++KB:string) is det
 *
 * The predicate loads the knowledge base contained in the given string. 
 * The knowledge base must be defined in pure OWL/RDF format.
 */
load_owl_from_string(String):-
  open_chars_stream(String,S),
  load_owl_from_stream(S).
  
load_owl_from_stream(S):-
  get_module(M),
  retractall(M:trdf_setting(_,_)),
  process_rdf(stream(S), assert_list(M), [namespaces(NSList)]),
  close(S),
  trill:add_kb_prefixes(M:NSList),
  rdf_2_owl(M,'ont'),
  utility_translation_init(M),
  owl_canonical_parse_3(M,['ont']),
  parse_probabilistic_annotation_assertions(M).

% Get the KB's prefixes contained into ns4query
:- multifile trill:kb_prefixes/1.

trill:kb_prefixes(M:L):-
  M:ns4query(L),!.

% Adds a list of kb prefixes into ns4query
:- multifile trill:add_kb_prefixes/1.

trill:add_kb_prefixes(_:[]):-!.

trill:add_kb_prefixes(M:[(H=H1)|T]):-
  trill:add_kb_prefix(M:H,H1),
  trill:add_kb_prefixes(M:T).

% Adds a prefix into ns4query
:- multifile trill:add_kb_prefix/2.

trill:add_kb_prefix(M:'',B):- !,
  trill:add_kb_prefix(M:[],B).

trill:add_kb_prefix(M:A,B):-
  M:ns4query(L),!,
  (\+ member((A=_),L) ->
      (retract(M:ns4query(L)),
       append(L,[(A=B)],NL),
       assert(M:ns4query(NL))
      )
    ;
      true
   ).
   
trill:add_kb_prefix(M:A,B):-
  assert(M:ns4query([(A=B)])).

% Removes a prefix from ns4query
:- multifile trill:remove_kb_prefix/2.
trill:remove_kb_prefix(M:A,B):-
  M:ns4query(L),!,
  (member((A=B),L) ->
      (retract(M:ns4query(L)),
       delete(L,(A=B),NL),
       assert(M:ns4query(NL))
      )
    ;
      true
   ).

:- multifile trill:remove_kb_prefix/1.
trill:remove_kb_prefix(M:A):-
  M:ns4query(L),!,
  (member((A=B),L) *->
      (retract(M:ns4query(L)),
       delete(L,(A=B),NL),
       assert(M:ns4query(NL))
      )
    ;
      (member((B=A),L),! *->
        (retract(M:ns4query(L)),
         delete(L,(B=A),NL),
         assert(M:ns4query(NL))
        )
      ;
        true
     )
   ).


assert_list(_M,[], _):-!.
assert_list(M,[H|T], Source) :-
    %H=..[_|Args],
    %H1=..[rdf|Args],
    assert(M:H),
    %add_atoms_from_axiom(M,Args),
    assert_list(M,T, Source).

find_all_probabilistic_annotations(M,An,Ax,PV):-
	M:annotation(Ax,An,literal(lang(_Lang, PV))),
	atom(PV).

find_all_probabilistic_annotations(M,An,Ax,PV):-
	M:annotation(Ax,An,literal(type(_Type, PV))),
	atom(PV).

find_all_probabilistic_annotations(M,An,Ax,PV):-
	M:annotation(Ax,An,literal(PV)),
	atom(PV).
  

parse_probabilistic_annotation_assertions(M) :-
  forall(find_all_probabilistic_annotations(M,An,Ax,PV),
       (assert_axiom(M,annotationAssertion(An,Ax,literal(PV))))
  ),
  % forall(aNN(X,Y,Z),assert(annotation(X,Y,Z))), VV remove 25/1/11
  % annotation/3 axioms created already during owl_parse_annotated_axioms/1
  retractall(M:annotation(_,_,_)).

/*
query_is([Q|_],0,Q):-!.
query_is([_|T],N,Q):-
  NN is N - 1,
  query_is(T,NN,Q).

set_new_query([_|T],0,NQ,[NQ|T]):-!.
set_new_query([Q|T],N,NQ,[Q|NT]):-
  NN is N - 1,
  set_new_query(T,NN,NQ,NT).


query_expand(CQ):-
  CQ =.. [CQP | CQArgs],
  member((CQP,PosQ),[(aggregate_all,1), (limit,1)]),!,
  query_is(CQArgs,PosQ,Q),
  Q =.. [P|Args],
  get_module(M),
  M:ns4query(NSList),!,
  %retract(M:ns4query(NSList)),
  expand_all_ns(M,Args,NSList,NewArgs),!,
  NQ =.. [P|NewArgs],
  set_new_query(CQArgs,PosQ,NQ,CQNewArgs),
  NCQ =.. [CQP|CQNewArgs],
  call(NCQ).
  
query_expand(Q):-
  Q =.. [P|Args],
  get_module(M),
  M:ns4query(NSList),!,
  %retract(M:ns4query(NSList)),
  expand_all_ns(M,Args,NSList,NewArgs),!,
  NQ =.. [P|NewArgs],
  call(NQ).
*/



expand_argument(M,literal(P),NSList,ExpP) :- !,
  expand_literal(M,literal(P),NSList,ExpP).
expand_argument(M,P,NSList,ExpP) :- 
  (expand_classExpression(M,P,NSList,ExpP) ;
   expand_individual(M,P,NSList,ExpP) ;
   expand_propertyExpression(M,P,NSList,ExpP) ;
   expand_axiom(M,P,NSList,ExpP) ; 
   expand_annotationProperty(M,P,NSList,ExpP) ;
   expand_dataRange(M,P,NSList,ExpP) ; 
   expand_ontology(M,P,NSList,ExpP) ), !.



/**
 * expand_all_ns(++Module:string,++Args:list,++NSList:list,--ExpandedArgs:list) is det
 *
 * The predicate takes as input a list containing strings and expands these strings
 * using the list of prefixes. Finally, it returns the list of expanded strings.
 * It adds names in Args to the list of known elements.
 */
expand_all_ns(M,Args,NSList,ExpandedArgs):-
  expand_all_ns(M,Args,NSList,true,ExpandedArgs).

/**
 * expand_all_ns(++Module:string,++Args:list,++NSList:list,++AddName:boolean,--ExpandedArgs:list) is det
 *
 * The predicate takes as input a list containing strings and expands these strings
 * using the list of prefixes. Finally, it returns the list of expanded strings.
 * If AddName is set true it adds names in Args in the list of known elements.
 */
expand_all_ns(_M,[],_,_,[]):- !.

expand_all_ns(M,[P|T],NSList,AddName,[PNewArgs|NewArgs]):-
  is_list(P),!,
  expand_all_ns(M,P,NSList,AddName,PNewArgs),
  expand_all_ns(M,T,NSList,AddName,NewArgs).

expand_all_ns(M,[P|T],NSList,AddName,[NP|NewArgs]):-
  expand_argument(M,P,NSList,NP),
  expand_all_ns(M,T,NSList,AddName,NewArgs).

/*
expand_all_ns(M,[P|T],NSList,AddName,[NP|NewArgs]):-
  compound(P),
  P =.. [N | Args],!,
  expand_all_ns(M,Args,NSList,AddName,NewPArgs),
  NP =.. [N| NewPArgs],
  expand_all_ns(M,T,NSList,AddName,NewArgs).

expand_all_ns(M,[H|T],NSList,AddName,[H|NewArgs]):-
  check_query_arg(M,H),!,
  expand_all_ns(M,T,NSList,AddName,NewArgs).

expand_all_ns(M,[H|T],NSList,AddName,[NewArg|NewArgs]):-
  expand_ns4query(M,H,NSList,AddName,NewArg),
  expand_all_ns(M,T,NSList,AddName,NewArgs).

check_query_arg(M,Arg) :-
  atomic(Arg),!,
  trill:axiom(M:Ax),
  in_axiom(Arg,[Ax]),!,
  add_kb_atom(M,Arg).

expand_ns4query(M,NS_URL,NSList,AddName, Full_URL):- 
	nonvar(NS_URL),
	NS_URL \= literal(_),
	uri_split(NS_URL,Short_NS,Term, ':'),
	member((Short_NS=Long_NS),NSList),
	concat_atom([Long_NS,Term],Full_URL),!,
	( AddName == true *-> add_kb_atom(M,Full_URL) ; true).

expand_ns4query(M,NS_URL,NSList,AddName, Full_URL):- 
	nonvar(NS_URL),
	NS_URL \= literal(_),
	\+ sub_atom(NS_URL,_,_,_,':'),
	member(([]=Long_NS),NSList),
	concat_atom([Long_NS,NS_URL],Full_URL),!,
	( AddName == true *-> add_kb_atom(M,Full_URL) ; true).

expand_ns4query(_M,URL,_,_,URL).
*/
/*
expand_ns4query(_M,URL,_,_,URL):-
    var(URL),!.
*/

% check whether the given atom is present in an axiom
in_axiom(Atom,[Atom|_]):- !.

in_axiom(Atom,[literal(_)|T]):-!,
	in_axiom(Atom,T).

in_axiom(Atom,[Axiom|_]):-
	is_list(Axiom),
	in_axiom(Atom,Axiom),!.

	
in_axiom(Atom,[Axiom|_]):-
	\+ is_list(Axiom),
	compound(Axiom),
	Axiom=..[_|Args],
	in_axiom(Atom,Args),!.

in_axiom(Atom,[_|T]):-
	in_axiom(Atom,T).

% save atoms in kb for checking existence when querying
add_atoms_from_axiom(_M,[]):-!.

add_atoms_from_axiom(M,[H|T]):-
  compound(H),
  H =.. ['literal' | _],!,
  add_atoms_from_axiom(M,T).

add_atoms_from_axiom(M,[H|T]):-
  compound(H),
  H =.. [_N, Args],!,
  ( is_list(Args) ->
      add_atoms_from_axiom(M,Args)
    ;
      add_atoms_from_axiom(M,[Args])
  ),
  add_atoms_from_axiom(M,T).

add_atoms_from_axiom(M,[H|T]):-
  compound(H),
  H =.. [_N | Args],!,
  add_atoms_from_axiom(M,Args),
  add_atoms_from_axiom(M,T).

add_atoms_from_axiom(M,[H|T]):-
  add_kb_atom(M,H),!,
  add_atoms_from_axiom(M,T).


add_kb_atom(M,IRI):-
  M:kb_atom(L),
  ( (member(IRI,L),!) *->
      true
    ;
      (retract(M:kb_atom(_)),
       assert(M:kb_atom([IRI|L]))
      )
  ).


add_kb_atoms(_M,_Type,[]):-!.

add_kb_atoms(M,Type,[H|T]):-
  M:kb_atom(KBA0),
  L=KBA0.Type,
  ( memberchk(H,L) -> 
      true
    ;
      ( retractall(M:kb_atom(_)),
        KBA=KBA0.put(Type,[H|L]),
        assert(M:kb_atom(KBA))
      )
  ),
  add_kb_atoms(M,Type,T).

% TODO remove this => dataproperty always as dataproperty, object property as property (for retrocompatibility) or objectproperty
fix_wrongly_classified_atoms(M):-
  M:kb_atom(KBA0),
  findall(OP,M:objectProperty(OP),ObjPs),
  findall(DP,M:dataProperty(DP),DataPs),
  fix_wrongly_classified_properties(ObjPs,objectProperty,KBA0,KBA1),
  fix_wrongly_classified_properties(DataPs,dataProperty,KBA1,KBA2),
  fix_duplicated_wrongly_classified_properties(KBA2.objectProperty,KBA2.dataProperty,KBA2,KBA),
  retractall(M:kb_atom(_)),
  assert(M:kb_atom(KBA)).

fix_wrongly_classified_properties([],_Type,KBA,KBA).

fix_wrongly_classified_properties([H|T],Type,KBA0,KBA):-
  RP=KBA0.Type,
  ( Type=objectProperty -> OtherType=dataProperty ; OtherType=objectProperty ),
  WP=KBA0.OtherType,
  ( memberchk(H,RP) -> NRP=RP ; NRP=[H|RP] ),
  ( memberchk(H,WP) -> delete(WP,H,NWP) ; NWP=WP ),
  KBA1=KBA0.put(Type,NRP),
  KBA2=KBA1.put(OtherType,NWP),
  fix_wrongly_classified_properties(T,Type,KBA2,KBA).

fix_duplicated_wrongly_classified_properties([],_DP,KBA,KBA).

fix_duplicated_wrongly_classified_properties([H|T],DP,KBA0,KBA):-
  memberchk(H,DP),!,
  delete(DP,H,NDP),
  KBA1=KBA0.put(dataProperty,NDP),
  fix_duplicated_wrongly_classified_properties(T,DP,KBA1,KBA).

fix_duplicated_wrongly_classified_properties([_H|T],DP,KBA0,KBA):-
  fix_duplicated_wrongly_classified_properties(T,DP,KBA0,KBA).


:- multifile trill:add_axiom/1.
trill:add_axiom(M:Ax):-
  assert(M:addKBName),
  init_kb_atom(M),
  create_and_assert_axioms(M,Ax),
  retractall(M:addKBName),
  utility_kb:update_kb(M,add,Ax).

:- multifile trill:add_axioms/1.
trill:add_axioms(_:[]).

trill:add_axioms(M:[H|T]) :-
  trill:add_axiom(M:H),
  trill:add_axioms(M:T).

:- multifile trill:remove_axiom/1.
trill:remove_axiom(M:Ax):-
  %print_message(warning,under_development),
  ( M:ns4query(NSList) -> true; NSList = []),
  expand_axiom(M,Ax,NSList,ExpAx),
  retract_axiom(M,ExpAx),
  retractall(M:owl(ExpAx,'ont')),!,
  trill:reload_kb(M:false).


/*
trill:remove_axiom(M:subClassOf(C,D)):-
  print_message(warning,under_development),
  ( M:ns4query(NSList) -> true; NSList = []),
  expand_axiom(M,subClassOf(C,D),NSList,subClassOf(ExpC,ExpD)),
  remove_subClassOf(M,ExpC,ExpD),
  retract_axiom(M,subClassOf(ExpC,ExpD)),
  retractall(M:owl(subClassOf(ExpC,ExpD),'ont')),!.

trill:remove_axiom(M:Ax):-
  print_message(warning,under_development),
  ( M:ns4query(NSList) *-> true; NSList = []),
  Ax =.. [P|Args],
  ( (length(Args,1), Args = [IntArgs], is_list(IntArgs)) -> 
       ( expand_all_ns(M,IntArgs,NSList,false,ArgsExp),
         AxEx =.. [P,ArgsExp]
       )
     ;
       ( expand_all_ns(M,Args,NSList,false,ArgsExp),
         AxEx =.. [P|ArgsExp]
       )
  ),
  retract_axiom(M,AxEx),
  retractall(M:owl(AxEx,'ont')),!.
*/

:- multifile trill:remove_axioms/1.
trill:remove_axioms(_:[]):-!.

trill:remove_axioms(M:[H|T]) :-
  trill:remove_axiom(M:H),
  trill:remove_axioms(M:T).

test_and_assert(M,Ax,O):-
  (\+ M:owl(Ax,O) ->
    (assert_axiom(M,Ax,O), assert(M:owl(Ax,O)))
   ;
    true
  ).

get_module(M):-
  pengine_self(Self),
  pengine_property(Self,module(M)),!.  
get_module(M):- !,
  prolog_load_context(module,M).

parse_rdf_from_owl_rdf_pred(String):-
  open_chars_stream(String,S),
  load_owl_from_stream(S).

/*
create_and_assert_axioms(M,Axiom) :-
  Axiom=..[P|Args],
  ( M:ns4query(NSList) -> true; NSList = []),
  ( (length(Args,1), Args = [IntArgs], is_list(IntArgs)) -> 
       ( expand_all_ns(M,IntArgs,NSList,ArgsExp),
         ExpAxiom =.. [P,ArgsExp]
       )
     ;
       ( expand_axiom(M,Axiom,NSList,ExpAxiom)
         %NewTRILLAxiom =.. [P|ArgsExp]
       )
  ),
  test_and_assert(M,ExpAxiom,'ont').
*/

create_and_assert_axioms(M,Axiom) :-
  ( M:ns4query(NSList) -> true; NSList = []),
  expand_axiom(M,Axiom,NSList,ExpAxiom),
  test_and_assert(M,ExpAxiom,'ont').


/**
 * add_rule(+Module:string, +Rule:string) is det
 *
 * This predicate adds to the rules list the rule in Rule
 */
add_rule(M,max_rule):- !,
  M:rules(D,ND),
  ( memberchk(max_rule,ND) -> true ;
    ( retractall(M:rules(_,_)),
      assert(M:rules(D,[max_rule|ND]))
    )
  ), !.
  
add_rule(M,or_rule):- !,
  M:rules(D,ND),
  ( memberchk(or_rule,ND) -> true ;
    ( retractall(M:rules(_,_)),
      assert(M:rules(D,[or_rule|ND]))
    )
  ), !.
  
add_rule(M,Rule):-
  M:rules(D,ND),
  ( memberchk(Rule,D) -> true ;
    ( retractall(M:rules(_,_)),
      assert(M:rules([Rule|D],ND))
    )
  ), !.

/**
 * add_expressivity(+Module:string, +L:string) is det
 *
 * This predicate collects expressivity info
 * expressivity(I,R) -> I=1|2|3 (EL|ALC|S)
 *						R=[0|1,0|1,0|1,0|1,0|1|2,0|1] ([H,R,O,I,N|Q,F])
 */
add_expressivity(M,a):-
  M:expressivity(I,R),
  ( I > 1 ; ( retractall(M:expressivity(_,_)),assert(M:expressivity(2,R)))), !.

add_expressivity(M,s):-
  M:expressivity(I,R),
  ( I > 2 ; ( retractall(M:expressivity(_,_)),assert(M:expressivity(3,R)))), !.

add_expressivity(M,h):-
  M:expressivity(I,[H,R,O,I,Res,F]),
  ( H=1 ; ( retractall(M:expressivity(_,_)),assert(M:expressivity(I,[1,R,O,I,Res,F])))), !.

add_expressivity(M,r):-
  M:expressivity(I,[H,R,O,I,Res,F]),
  ( R=1 ; ( retractall(M:expressivity(_,_)),assert(M:expressivity(I,[H,1,O,I,Res,F])))), !.

add_expressivity(M,o):-
  M:expressivity(I,[H,R,O,I,Res,F]),
  ( O=1 ; ( retractall(M:expressivity(_,_)),assert(M:expressivity(I,[H,R,1,I,Res,F])))), !.

add_expressivity(M,i):-
  M:expressivity(I,[H,R,O,I,Res,F]),
  ( I=1 ; ( retractall(M:expressivity(_,_)),assert(M:expressivity(I,[H,R,O,1,Res,F])))), !.

add_expressivity(M,n):-
  M:expressivity(I,[H,R,O,I,Res,F]),
  ( Res>0 ; ( retractall(M:expressivity(_,_)),assert(M:expressivity(I,[H,R,O,I,1,F])))), !.

add_expressivity(M,q):-
  M:expressivity(I,[H,R,O,I,Res,F]),
  ( Res>1 ; ( retractall(M:expressivity(_,_)),assert(M:expressivity(I,[H,R,O,I,2,F])))), !.

add_expressivity(M,f):-
  M:expressivity(I,[H,R,O,I,Res,F]),
  ( F=1 ; ( retractall(M:expressivity(_,_)),assert(M:expressivity(I,[H,R,O,I,Res,1])))), !.

/**
 * is_axiom(?Axiom:string) is det
 *
 * This predicate unifies Pred with one of the possible type of axioms managed by TRILL and 
 * by the translation module.
 */
is_axiom(Axiom) :-
	functor(Axiom,Pred,Arity),
	axiompred(Pred/Arity),!.

clean_up(M):-
  rdf_reset_db,
  M:(dynamic class/1, datatype/1, objectProperty/1, dataProperty/1, annotationProperty/1),
  M:(dynamic namedIndividual/1, anonymousIndividual/1, subClassOf/2, equivalentClasses/1, disjointClasses/1, disjointUnion/2),
  M:(dynamic subPropertyOf/2, equivalentProperties/1, disjointProperties/1, inverseProperties/2, propertyDomain/2, propertyRange/2),
  M:(dynamic functionalProperty/1, inverseFunctionalProperty/1, reflexiveProperty/1, irreflexiveProperty/1, symmetricProperty/1, asymmetricProperty/1, transitiveProperty/1, hasKey/2),
  M:(dynamic sameIndividual/1, differentIndividuals/1, classAssertion/2, propertyAssertion/3, negativePropertyAssertion/3),
  M:(dynamic annotationAssertion/3, annotation/3, ontology/1, ontologyAxiom/2, ontologyImport/2, ontologyVersionInfo/2),
  M:(dynamic owl/4, owl/3, owl/2, blanknode/3, outstream/1, aNN/3, annotation_r_node/4, axiom_r_node/4, owl_repository/2, trdf_setting/2),
  M:(dynamic ns4query/1),
  retractall(M:kb_atom([])),
  forall(trill:axiom(M:A),retractall(M:A)),
  retractall(M:blanknode(_,_,_)),
  retractall(M:aNN(_,_,_)),
  retractall(M:annotation_r_node(_,_,_)),
  retractall(M:axiom_r_node(_,_,_)),
  retractall(M:annotation(_,_,_)),
  retractall(M:owl(_,_,_)),
  retractall(M:owl(_,_,_,_)),
  retractall(M:owl(_,_)),
  retractall(M:ontologyAxiom(_,_)),
  retractall(M:ontologyImport(_,_)),
  retractall(M:ontologyVersionInfo(_,_)),
  retractall(M:rdf(_,_,_)).

set_up(M):-
  M:(dynamic class/1, datatype/1, objectProperty/1, dataProperty/1, annotationProperty/1),
  M:(dynamic namedIndividual/1, anonymousIndividual/1, subClassOf/2, equivalentClasses/1, disjointClasses/1, disjointUnion/2),
  M:(dynamic subPropertyOf/2, equivalentProperties/1, disjointProperties/1, inverseProperties/2, propertyDomain/2, propertyRange/2),
  M:(dynamic functionalProperty/1, inverseFunctionalProperty/1, reflexiveProperty/1, irreflexiveProperty/1, symmetricProperty/1, asymmetricProperty/1, transitiveProperty/1, hasKey/2),
  M:(dynamic sameIndividual/1, differentIndividuals/1, classAssertion/2, propertyAssertion/3, negativePropertyAssertion/3),
  M:(dynamic annotationAssertion/3, annotation/3, ontology/1, ontologyAxiom/2, ontologyImport/2, ontologyVersionInfo/2),
  M:(dynamic owl/4, owl/3, owl/2, blanknode/3, outstream/1, aNN/3, annotation_r_node/4, axiom_r_node/4, owl_repository/2, trdf_setting/2),
  M:(dynamic ns4query/1, addKBName/0),
  retractall(M:addKBName).
  %retractall(M:rules(_,_)),
  %assert(M:rules([],[])),
  %retractall(M:expressivity(_,_)),
  %assert(M:expressivity(1,[0,0,0,0,0,0])).

set_up_kb_loading(M):-
  retractall(M:kb_atom(_)),
  init_kb_atom(M),
  retractall(M:addKBName),
  assert(M:addKBName),
  assert(trill_input_mode(M)).
  %format("Loading knowledge base...~n",[]),
  %statistics(walltime,[_,_]).

init_kb_atom(M):-
  assert(M:kb_atom(kbatoms{annotationProperty:[],class:[],dataProperty:[],datatype:[],individual:[],objectProperty:[]})).

init_kb_atom(M,AnnProps,Classes,DataProps,Datatypes,Inds,ObjectProps):-
  assert(M:kb_atom(kbatoms{annotationProperty:AnnProps,class:Classes,dataProperty:DataProps,datatype:Datatypes,individual:Inds,objectProperty:ObjectProps})).

init_kb_atom(M,KB):-
  assert(M:kb_atom(kbatoms{annotationProperty:KB.annotationProperties,class:KB.classesName,dataProperty:KB.dataProperties,datatype:KB.datatypes,individual:KB.individuals,objectProperty:KB.objectProperties})).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(utility_translation:load_owl(_)).
sandbox:safe_primitive(utility_translation:load_owl_from_string(_)).
sandbox:safe_primitive(utility_translation:expand_all_ns(_,_,_,_)).
sandbox:safe_primitive(utility_translation:expand_all_ns(_,_,_,_,_)).
%sandbox:safe_primitive(utility_translation:query_expand(_)).

user:term_expansion(kb_prefix(A,B),[]):-
  get_module(M),
  assert(M:addKBName),
  trill:add_kb_prefix(M:A,B).

user:term_expansion(owl_rdf(String),[]):-
  parse_rdf_from_owl_rdf_pred(String).

user:term_expansion(end_of_file, end_of_file) :-
  rdf_reset_db,
  retractall(M:blanknode(_,_,_)),
  retractall(M:aNN(_,_,_)),
  retractall(M:annotation_r_node(_,_,_)),
  retractall(M:axiom_r_node(_,_,_)),
  retractall(M:annotation(_,_,_)),
  retractall(M:owl(_,_,_)),
  retractall(M:owl(_,_,_,_)),
  retractall(M:owl(_,_)),
  retractall(M:ontologyAxiom(_,_)),
  retractall(M:ontologyImport(_,_)),
  retractall(M:ontologyVersionInfo(_,_)),
  retractall(M:rdf(_,_,_)),
  retractall(M:trdf_setting(_,_)),
  get_module(M),
  trill_input_mode(M),
  dif(M,trill),
  dif(M,utility_translation),
  fix_wrongly_classified_atoms(M),
  retractall(M:addKBName),
  retractall(trill_input_mode(_)).
  %statistics(walltime,[_,KBLM]),
  %KBLS is KBLM / 1000,
  %format("Knowledge base loaded in ~f seconds.~n",[KBLS]).

user:term_expansion(TRILLAxiom,[]):-
  get_module(M),
  is_axiom(TRILLAxiom),
  create_and_assert_axioms(M,TRILLAxiom).


/*
class/1,datatype/1,objectProperty/1,dataProperty/1,annotationProperty/1,namedIndividual/1,anonymousIndividual/1,
subClassOf/2,equivalentClasses/1,disjointClasses/1,disjointUnion/2,subPropertyOf/2,equivalentProperties/1,
disjointProperties/1,inverseProperties/2,propertyDomain/2,propertyRange/2,functionalProperty/1,
inverseFunctionalProperty/1,reflexiveProperty/1,irreflexiveProperty/1,symmetricProperty/1,asymmetricProperty/1,
transitiveProperty/1,hasKey/2,sameIndividual/1,differentIndividuals/1,classAssertion/2,propertyAssertion/3,
negativePropertyAssertion/3,annotationAssertion/3,annotation/3,ontology/1,ontologyAxiom/2,ontologyImport/2,
ontologyVersionInfo/2,owl/4,owl/3,owl/2,blanknode/3,outstream/1,aNN/3,annotation_r_node/4,axiom_r_node/4,
owl_repository/2,trdf_setting/2,
*/


