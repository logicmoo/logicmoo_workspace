:- module(owl2_model, [load_owl/1, load_owl_from_string/1, expand_all_ns/3]).

:- use_module(library(lists),[member/2]).
:- use_module(library(pengines)).

:- use_module(library(sandbox)).

:- discontiguous(valid_axiom/1).
:- discontiguous(axiompred/1).
:- discontiguous(axiom_arguments/2).


builtin_class('http://www.w3.org/2002/07/owl#Thing').
builtin_class('http://www.w3.org/2002/07/owl#Nothing').
is_class(C) :- class(C).
is_class(C) :- builtin_class(C).


% TODO: hasKey

/****************************************
  AXIOMS
  ****************************************/

%% entity(?IRI)
% the fundamental building blocks of owl 2 ontologies, and they define the vocabulary (the named terms) of an ontology
%
% @see individual/1, property/1, class/1, datatype/1
entity(A) :- individual(A).
entity(A) :- property(A).
entity(A) :- class(A).
entity(A) :- datatype(A).
axiom_arguments(entity,[iri]).
valid_axiom(entity(A)) :- subsumed_by([A],[iri]).

%declarationAxiom(individual(A)) :- individual(A). % TODO - check this
declarationAxiom(namedIndividual(A)) :- namedIndividual(A).
declarationAxiom(objectProperty(A)) :- objectProperty(A).
declarationAxiom(dataProperty(A)) :- dataProperty(A).
declarationAxiom(annotationProperty(A)) :- annotationProperty(A).  % VV added 9/3/2010
declarationAxiom(class(A)) :- class(A).
declarationAxiom(datatype(A)) :- datatype(A).
% TODO: check. here we treat the ontology declaration as an axiom;
% this liberal definition of axiom allows us to iterate over axiom/1
% to find every piece of information in the ontology.
declarationAxiom(ontology(A)) :- ontology(A).

%% class(?IRI)
% Classes can be understood as sets of individuals
:- thread_local(class/1).

axiompred(class/1).
axiom_arguments(class,[iri]).
valid_axiom(class(A)) :- subsumed_by([A],[iri]).

%% datatype(?IRI)
% Datatypes are entities that refer to sets of values described by a datatype map
:- thread_local(datatype/1).

axiompred(datatype/1).
axiom_arguments(datatype,[iri]).
valid_axiom(datatype(A)) :- subsumed_by([A],[iri]).

%% property(?IRI)
% Properties connect individuals with either other individuals or with literals
%
% @see dataProperty/1, objectProperty/1, annotationProperty/1
property(A) :- dataProperty(A).
property(A) :- objectProperty(A).
property(A) :- annotationProperty(A).
axiom_arguments(property,[iri]).
valid_axiom(property(A)) :- subsumed_by([A],[iri]).

%% objectProperty(?IRI)
% Object properties connect pairs of individuals
:- thread_local(objectProperty/1).

axiompred(objectProperty/1).
axiom_arguments(objectProperty,[iri]).
valid_axiom(objectProperty(A)) :- subsumed_by([A],[iri]).

%% dataProperty(?IRI)
% Data properties connect individuals with literals. In some knowledge representation systems, functional data properties are called attributes.
:- thread_local(dataProperty/1).

axiompred(dataProperty/1).
axiom_arguments(dataProperty,[iri]).
valid_axiom(dataProperty(A)) :- subsumed_by([A],[iri]).

%% annotationProperty(?IRI)
% Annotation properties can be used to provide an annotation for an ontology, axiom, or an IRI
:- thread_local(annotationProperty/1).

axiompred(annotationProperty/1).
axiom_arguments(annotationProperty,[iri]).
valid_axiom(annotationProperty(A)) :- subsumed_by([A],[iri]).


%% individual(?IRI)
% Individuals represent actual objects from the domain being modeled
% @see anonymousIndividual/1, namedIndividual/1
individual(A) :- anonymousIndividual(A).
individual(A) :- namedIndividual(A).
%individual(A) :- nonvar(A),iri(A),\+property(A),\+class(A),\+ontology(A). % TODO: check: make individuals the default
axiom_arguments(individual,[iri]).
valid_axiom(individual(A)) :- subsumed_by([A],[iri]).

%% namedIndividual(?IRI)
% Named individuals are given an explicit name that can be used in any ontology in the import closure to refer to the same individual
:- thread_local(namedIndividual/1).

axiompred(namedIndividual/1).
axiom_arguments(namedIndividual,[iri]).
valid_axiom(namedIndividual(A)) :- subsumed_by([A],[iri]).

%% anonymousIndividual(?IRI)
% Anonymous individuals are local to the ontology they are contained in. Analagous to bnodes
% @see construct/1
:- thread_local(anonymousIndividual/1).

axiompred(anonymousIndividual/1).
axiom_arguments(anonymousIndividual,[iri]).
valid_axiom(anonymousIndividual(A)) :- subsumed_by([A],[iri]).

%% construct(?IRI)
% @see axiom/1, annotation/1, ontology/1
construct(A) :- axiom(A).
construct(A) :- annotation(A).
construct(A) :- ontology(A).
axiom_arguments(construct,[iri]).
valid_axiom(construct(A)) :- subsumed_by([A],[iri]).

%% axiom(?Axiom)
% The main component of an OWL 2 ontology is a set of axioms - statements that say what is true in the domain being modeled.
% @see classAxiom/1, propertyAxiom/1, fact/1
axiom(A) :- classAxiom(A).
axiom(A) :- propertyAxiom(A).
axiom(hasKey(A,B)) :- hasKey(A,B).
axiom(A) :- fact(A).
axiom(A) :- declarationAxiom(A).
%axiom(annotation(A,B,C)) :-
%	annotation(A,B,C). % CJM-treat annotations as axioms
axiom_arguments(axiom,[axiom]).
valid_axiom(axiom(A)) :- subsumed_by([A],[axiom]).

%% classAxiom(?Axiom)
% OWL 2 provides axioms that allow relationships to be established between class expressions. This predicate reifies the actual axiom
% @see equivalentClasses/1, disjointClasses/1, subClassOf/2, disjointUnion/2
classAxiom(equivalentClasses(A)) :- equivalentClasses(A).
classAxiom(disjointClasses(A)) :- disjointClasses(A).
classAxiom(subClassOf(A, B)) :- subClassOf(A, B).
classAxiom(disjointUnion(A, B)) :- disjointUnion(A, B).
axiom_arguments(classAxiom,[axiom]).
valid_axiom(classAxiom(A)) :- subsumed_by([A],[axiom]).

%% subClassOf(?SubClass:ClassExpression, ?SuperClass:ClassExpression)
% A subclass axiom SubClassOf( CE1 CE2 ) states that the class expression CE1 is a subclass of the class expression CE2
%
%   @param SubClass a classExpression/1 representing the more specific class
%   @param SuperClass a classExpression/1 representing the more general class
:- thread_local(subClassOf/2).

axiompred(subClassOf/2).
axiom_arguments(subClassOf,[classExpression, classExpression]).
valid_axiom(subClassOf(A, B)) :- subsumed_by([A, B],[classExpression, classExpression]).


%% equivalentClasses(?ClassExpressions:set(ClassExpression))
% An equivalent classes axiom EquivalentClasses( CE1 ... CEn ) states that all of the class expressions CEi, 1 <= i <= n, are semantically equivalent to each other.
:- thread_local(equivalentClasses/1).

axiompred(equivalentClasses/1).
axiom_arguments(equivalentClasses,[set(classExpression)]).
valid_axiom(equivalentClasses(A)) :- subsumed_by([A],[set(classExpression)]).

%% disjointClasses(?ClassExpressions:set(ClassExpression))
% A disjoint classes axiom DisjointClasses( CE1 ... CEn ) states that all of the class expressions CEi, 1 <= i <= n, are pairwise disjoint; that is, no individual can be at the same time an instance of both CEi and CEj for i != j
:- thread_local(disjointClasses/1).

axiompred(disjointClasses/1).
axiom_arguments(disjointClasses,[set(classExpression)]).
valid_axiom(disjointClasses(A)) :- subsumed_by([A],[set(classExpression)]).

%% disjointUnion(?ClassExpression, ?ClassExpressions:set(ClassExpression))
% A disjoint union axiom DisjointUnion( C CE1 ... CEn ) states that a class C is a disjoint union of the class expressions CEi, 1 <= i <= n, all of which are pairwise disjoint.
:- thread_local(disjointUnion/2).

axiompred(disjointUnion/2).
axiom_arguments(disjointUnion,[classExpression,set(classExpression)]).
valid_axiom(disjointUnion(A,B)) :- subsumed_by([A,B],[classExpression,set(classExpression)]).

%% propertyAxiom(?Axiom)
% OWL 2 provides axioms that can be used to characterize and establish relationships between object property expressions. This predicate reifies the actual axiom
%
% @see symmetricProperty/1, inverseFunctionalProperty/1, transitiveProperty/1, asymmetricProperty/1, subPropertyOf/2, functionalProperty/1, irreflexiveProperty/1, disjointProperties/1, propertyDomain/2, reflexiveProperty/1, propertyRange/2, equivalentProperties/1, inverseProperties/2
propertyAxiom(symmetricProperty(A)) :- symmetricProperty(A).
propertyAxiom(inverseFunctionalProperty(A)) :- inverseFunctionalProperty(A).
propertyAxiom(transitiveProperty(A)) :- transitiveProperty(A).
propertyAxiom(asymmetricProperty(A)) :- asymmetricProperty(A).
propertyAxiom(subPropertyOf(A, B)) :- subPropertyOf(A, B).
propertyAxiom(functionalProperty(A)) :- functionalProperty(A).
propertyAxiom(irreflexiveProperty(A)) :- irreflexiveProperty(A).
propertyAxiom(disjointProperties(A)) :- disjointProperties(A).
propertyAxiom(propertyDomain(A, B)) :- propertyDomain(A, B).
propertyAxiom(reflexiveProperty(A)) :- reflexiveProperty(A).
propertyAxiom(propertyRange(A, B)) :- propertyRange(A, B).
propertyAxiom(equivalentProperties(A)) :- equivalentProperties(A).
propertyAxiom(inverseProperties(A, B)) :- inverseProperties(A, B).
axiom_arguments(propertyAxiom,[axiom]).
valid_axiom(propertyAxiom(A)) :- subsumed_by([A],[axiom]).


%% subPropertyOf(?Sub:PropertyExpression, ?Super:ObjectPropertyExpression)
% subproperty axioms are analogous to subclass axioms
% (extensional predicate - can be asserted)
:- thread_local(subPropertyOf/2).

axiompred(subPropertyOf/2).
axiom_arguments(subPropertyOf,[propertyExpression, objectPropertyExpression]).
valid_axiom(subPropertyOf(A, B)) :- subsumed_by([A, B],[propertyExpression, objectPropertyExpression]).

%% subObjectPropertyOf(?Sub:ObjectPropertyExpressionOrChain, ?Super:ObjectPropertyExpression)
% The basic form is SubPropertyOf( OPE1 OPE2 ). This axiom states that the object property expression OPE1 is a subproperty of the object property expression OPE2 - that is, if an individual x is connected by OPE1 to an individual y, then x is also connected by OPE2 to y. The more complex form is SubPropertyOf( PropertyChain( OPE1 ... OPEn ) OPE ). This axiom states that, if an individual x is connected by a sequence of object property expressions OPE1, ..., OPEn with an individual y, then x is also connected with y by the object property expression OPE
subObjectPropertyOf(A, B) :- subPropertyOf(A, B),subsumed_by([A, B],[objectPropertyExpressionOrChain, objectPropertyExpression]).
axiom_arguments(subObjectPropertyOf,[objectPropertyExpressionOrChain, objectPropertyExpression]).
valid_axiom(subObjectPropertyOf(A, B)) :- subsumed_by([A, B],[objectPropertyExpressionOrChain, objectPropertyExpression]).

%% subDataPropertyOf(?Sub:DataPropertyExpression, ?Super:DataPropertyExpression)
% A data subproperty axiom SubPropertyOf( DPE1 DPE2 ) states that the data property expression DPE1 is a subproperty of the data property expression DPE2 - that is, if an individual x is connected by OPE1 to a literal y, then x is connected by OPE2 to y as well.
subDataPropertyOf(A, B) :- subPropertyOf(A, B),subsumed_by([A, B],[dataPropertyExpression, dataPropertyExpression]).
axiom_arguments(subDataPropertyOf,[dataPropertyExpression, dataPropertyExpression]).
valid_axiom(subDataPropertyOf(A, B)) :- subsumed_by([A, B],[dataPropertyExpression, dataPropertyExpression]).

%% subAnnotationPropertyOf(?Sub:AnnotationProperty, ?Super:AnnotationProperty)
% An annotation subproperty axiom SubPropertyOf( AP1 AP2 ) states that the annotation property AP1 is a subproperty of the annotation property AP2
subAnnotationPropertyOf(A, B) :- subPropertyOf(A, B),subsumed_by([A, B],[annotationProperty, annotationProperty]).
axiom_arguments(subAnnotationPropertyOf,[annotationProperty, annotationProperty]).
valid_axiom(subAnnotationPropertyOf(A, B)) :- subsumed_by([A, B],[annotationProperty, annotationProperty]).

%% equivalentProperties(?PropertyExpressions:set(PropertyExpression))
% An equivalent object properties axiom EquivalentProperties( OPE1 ... OPEn ) states that all of the object property expressions OPEi, 1 <= i <= n, are semantically equivalent to each other
% (extensional predicate - can be asserted)
:- thread_local(equivalentProperties/1).

axiompred(equivalentProperties/1).
axiom_arguments(equivalentProperties,[set(propertyExpression)]).
valid_axiom(equivalentProperties(A)) :- subsumed_by([A],[set(propertyExpression)]).

%% equivalentObjectProperties(?PropertyExpressions:set(ObjectPropertyExpression))
% An equivalent object properties axiom EquivalentObjectProperties( OPE1 ... OPEn ) states that all of the object property expressions OPEi, 1 <= i <= n, are semantically equivalent to each other
equivalentObjectProperties(A) :- equivalentProperties(A),subsumed_by([A],[set(objectPropertyExpression)]).
axiom_arguments(equivalentObjectProperties,[set(objectPropertyExpression)]).
valid_axiom(equivalentObjectProperties(A)) :- subsumed_by([A],[set(objectPropertyExpression)]).

%% equivalentDataProperties(?PropertyExpressions:set(DataPropertyExpression))
% An equivalent data properties axiom EquivalentProperties( DPE1 ... DPEn ) states that all the data property expressions DPEi, 1 <= i <= n, are semantically equivalent to each other. This axiom allows one to use each DPEi as a synonym for each DPEj - that is, in any expression in the ontology containing such an axiom, DPEi can be replaced with DPEj without affecting the meaning of the ontology
equivalentDataProperties(A) :- equivalentProperties(A),subsumed_by([A],[set(dataPropertyExpression)]).
axiom_arguments(equivalentDataProperties,[set(dataPropertyExpression)]).
valid_axiom(equivalentDataProperties(A)) :- subsumed_by([A],[set(dataPropertyExpression)]).

%% disjointProperties(?PropertyExpressions:set(PropertyExpression))
% A disjoint properties axiom DisjointProperties( PE1 ... PEn ) states that all of the property expressions PEi, 1 <= i <= n, are pairwise disjoint
% (extensional predicate - can be asserted)
:- thread_local(disjointProperties/1).

axiompred(disjointProperties/1).
axiom_arguments(disjointProperties,[set(propertyExpression)]).
valid_axiom(disjointProperties(A)) :- subsumed_by([A],[set(propertyExpression)]).

%% disjointObjectProperties(?PropertyExpressions:set(ObjectPropertyExpression))
% A disjoint object properties axiom DisjointProperties( OPE1 ... OPEn ) states that all of the object property expressions OPEi, 1 <= i <= n, are pairwise disjoint; that is, no individual x can be connected to an individual y by both OPEi and OPEj for i != j.
disjointObjectProperties(A) :- disjointProperties(A),subsumed_by([A],[set(objectPropertyExpression)]).
axiom_arguments(disjointObjectProperties,[set(objectPropertyExpression)]).
valid_axiom(disjointObjectProperties(A)) :- subsumed_by([A],[set(objectPropertyExpression)]).

%% disjointDataProperties(?PropertyExpressions:set(DataPropertyExpression))
% A disjoint data properties axiom DisjointProperties( DPE1 ... DPEn ) states that all of the data property expressions DPEi, 1 <= i <= n, are pairwise disjoint; that is, no individual x can be connected to a literal y by both DPEi and DPEj for i !- j.
disjointDataProperties(A) :- disjointProperties(A),subsumed_by([A],[set(dataPropertyExpression)]).
axiom_arguments(disjointDataProperties,[set(dataPropertyExpression)]).
valid_axiom(disjointDataProperties(A)) :- subsumed_by([A],[set(dataPropertyExpression)]).

%% inverseProperties(?ObjectPropertyExpression1:ObjectPropertyExpression, ?ObjectPropertyExpression2:ObjectPropertyExpression)
% An inverse object properties axiom InverseProperties( OPE1 OPE2 ) states that the object property expression OPE1 is an inverse of the object property expression OPE2
% (note there are no inverse data properties, as literals are not connected to individuals)
% Example:
% =|inverseProperties(partOf,hasPart)|=
% (extensional predicate - can be asserted)
:- thread_local(inverseProperties/2).

axiompred(inverseProperties/2).
axiom_arguments(inverseProperties,[objectPropertyExpression, objectPropertyExpression]).
valid_axiom(inverseProperties(A, B)) :- subsumed_by([A, B],[objectPropertyExpression, objectPropertyExpression]).

%% propertyDomain(?PropertyExpression, ?CE)
%  A property domain axiom PropertyDomain( PE CE ) states that the
%  domain of the property expression PE is CE
% (extensional predicate - can be asserted)

:- thread_local(propertyDomain/2).

axiompred(propertyDomain/2).
axiom_arguments(propertyDomain,[propertyExpression, classExpression]).
valid_axiom(propertyDomain(A, B)) :- subsumed_by([A, B],[propertyExpression, classExpression]).

%% objectPropertyDomain(?ObjectPropertyExpression, ?ClassExpression)
% An object property domain axiom PropertyDomain( OPE CE ) states that the domain of the object property expression OPE is the class expression CE - that is, if an individual x is connected by OPE with some other individual, then x is an instance of CE
objectPropertyDomain(A, B) :- propertyDomain(A, B),subsumed_by([A, B],[objectPropertyExpression, classExpression]).
axiom_arguments(objectPropertyDomain,[objectPropertyExpression, classExpression]).
valid_axiom(objectPropertyDomain(A, B)) :- subsumed_by([A, B],[objectPropertyExpression, classExpression]).

%% dataPropertyDomain(?DataPropertyExpression, ?ClassExpression)
% A data property domain axiom PropertyDomain( DPE CE ) states that the domain of the data property expression DPE is the class expression CE - that is, if an individual x is connected by DPE with some literal, then x is an instance of CE
dataPropertyDomain(A, B) :- propertyDomain(A, B),subsumed_by([A, B],[dataPropertyExpression, classExpression]).
axiom_arguments(dataPropertyDomain,[dataPropertyExpression, classExpression]).
valid_axiom(dataPropertyDomain(A, B)) :- subsumed_by([A, B],[dataPropertyExpression, classExpression]).

%% annotationPropertyDomain(?AnnotationProperty, ?IRI)
% An annotation property domain axiom PropertyDomain( AP U ) states that the domain of the annotation property AP is the IRI U. Such axioms have no effect on the Direct Semantics of OWL 2
annotationPropertyDomain(A, B) :- propertyDomain(A, B),subsumed_by([A, B],[annotationProperty, iri]).
axiom_arguments(annotationPropertyDomain,[annotationProperty, iri]).
valid_axiom(annotationPropertyDomain(A, B)) :- subsumed_by([A, B],[annotationProperty, iri]).

%% propertyRange(?PropertyExpression, ?ClassExpression)
% An object property domain axiom PropertyRange( OPE CE ) states that the domain of the object property expression OPE is the class expression CE - that is, if an individual x is connected by OPE with some other individual, then x is an instance of CE
% (extensional predicate - can be asserted)
:- thread_local(propertyRange/2).

axiompred(propertyRange/2).
axiom_arguments(propertyRange,[propertyExpression, classExpression]).
valid_axiom(propertyRange(A, B)) :- subsumed_by([A, B],[propertyExpression, classExpression]).

%% objectPropertyRange(?ObjectPropertyExpression, ?ClassExpression)
% An object property domain axiom PropertyRange( OPE CE ) states that the domain of the object property expression OPE is the class expression CE - that is, if an individual x is connected by OPE with some other individual, then x is an instance of CE
objectPropertyRange(A, B) :- propertyRange(A, B),subsumed_by([A, B],[objectPropertyExpression, classExpression]).
axiom_arguments(objectPropertyRange,[objectPropertyExpression, classExpression]).
valid_axiom(objectPropertyRange(A, B)) :- subsumed_by([A, B],[objectPropertyExpression, classExpression]).

%% dataPropertyRange(?ObjectPropertyExpression, ?DataRange)
% A data property range axiom PropertyRange( DPE DR ) states that the range of the data property expression DPE is the data range DR - that is, if some individual is connected by DPE with a literal x, then x is in DR. The arity of DR MUST be one
dataPropertyRange(A, B) :- propertyRange(A, B),subsumed_by([A, B],[dataPropertyExpression, dataRange]).
axiom_arguments(dataPropertyRange,[objectPropertyExpression, dataRange]).
valid_axiom(dataPropertyRange(A, B)) :- subsumed_by([A, B],[objectPropertyExpression, dataRange]).

%% annotationPropertyRange(?AnnotationProperty, ?IRI)
% An annotation property range axiom PropertyRange( AP U ) states that the range of the annotation property AP is the IRI U. Such axioms have no effect on the Direct Semantics of OWL 2
annotationPropertyRange(A, B) :- propertyRange(A, B),subsumed_by([A, B],[annotationProperty, iri]).
axiom_arguments(annotationPropertyRange,[annotationProperty, iri]).
valid_axiom(annotationPropertyRange(A, B)) :- subsumed_by([A, B],[annotationProperty, iri]).

%% functionalProperty(?PropertyExpression)
% An object property functionality axiom FunctionalProperty( OPE ) states that the object property expression OPE is functional - that is, for each individual x, there can be at most one distinct individual y such that x is connected by OPE to y
% (extensional predicate - can be asserted)
:- thread_local(functionalProperty/1).

axiompred(functionalProperty/1).
axiom_arguments(functionalProperty,[propertyExpression]).
valid_axiom(functionalProperty(A)) :- subsumed_by([A],[propertyExpression]).

%% functionalObjectProperty(?ObjectPropertyExpression)
% An object property functionality axiom FunctionalProperty( OPE ) states that the object property expression OPE is functional - that is, for each individual x, there can be at most one distinct individual y such that x is connected by OPE to y
functionalObjectProperty(A) :- functionalProperty(A),subsumed_by([A],[objectPropertyExpression]).
axiom_arguments(functionalObjectProperty,[objectPropertyExpression]).
valid_axiom(functionalObjectProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).

%% functionalDataProperty(?DataPropertyExpression)
% A data property functionality axiom FunctionalProperty( DPE ) states that the data property expression DPE is functional - that is, for each individual x, there can be at most one distinct literal y such that x is connected by DPE with y
functionalDataProperty(A) :- functionalProperty(A),subsumed_by([A],[dataPropertyExpression]).
axiom_arguments(functionalDataProperty,[dataPropertyExpression]).
valid_axiom(functionalDataProperty(A)) :- subsumed_by([A],[dataPropertyExpression]).

%% inverseFunctionalProperty(?ObjectPropertyExpression)
% An object property inverse functionality axiom InverseFunctionalProperty( OPE ) states that the object property expression OPE is inverse-functional - that is, for each individual x, there can be at most one individual y such that y is connected by OPE with x. Note there are no InverseFunctional DataProperties
:- thread_local(inverseFunctionalProperty/1).

axiompred(inverseFunctionalProperty/1).
axiom_arguments(inverseFunctionalProperty,[objectPropertyExpression]).
valid_axiom(inverseFunctionalProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).

%% reflexiveProperty(?ObjectPropertyExpression)
% An object property reflexivity axiom ReflexiveProperty( OPE ) states that the object property expression OPE is reflexive - that is, each individual is connected by OPE to itself
:- thread_local(reflexiveProperty/1).

axiompred(reflexiveProperty/1).
axiom_arguments(reflexiveProperty,[objectPropertyExpression]).
valid_axiom(reflexiveProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).

%% irreflexiveProperty(?ObjectPropertyExpression)
% An object property reflexivity axiom ReflexiveProperty( OPE ) states that the object property expression OPE is reflexive - that is, no individual is connected by OPE to itsel
:- thread_local(irreflexiveProperty/1).

axiompred(irreflexiveProperty/1).
axiom_arguments(irreflexiveProperty,[objectPropertyExpression]).
valid_axiom(irreflexiveProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).

%% symmetricProperty(?ObjectPropertyExpression)
% An object property symmetry axiom SymmetricProperty( OPE ) states that the object property expression OPE is symmetric - that is, if an individual x is connected by OPE to an individual y, then y is also connected by OPE to x
:- thread_local(symmetricProperty/1).

axiompred(symmetricProperty/1).
axiom_arguments(symmetricProperty,[objectPropertyExpression]).
valid_axiom(symmetricProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).

%% asymmetricProperty(?ObjectPropertyExpression)
% An object property asymmetry axiom AsymmetricProperty( OPE ) states that the object property expression OPE is asymmetric - that is, if an individual x is connected by OPE to an individual y, then y cannot be connected by OPE to x
:- thread_local(asymmetricProperty/1).

axiompred(asymmetricProperty/1).
axiom_arguments(asymmetricProperty,[objectPropertyExpression]).
valid_axiom(asymmetricProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).

%% transitiveProperty(?ObjectPropertyExpression)
% An object property transitivity axiom TransitiveProperty( OPE ) states that the object property expression OPE is transitive - that is, if an individual x is connected by OPE to an individual y that is connected by OPE to an individual z, then x is also connected by OPE to z
:- thread_local(transitiveProperty/1).

axiompred(transitiveProperty/1).
axiom_arguments(transitiveProperty,[objectPropertyExpression]).
valid_axiom(transitiveProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).

%% hasKey(?ClassExpression,?PropertyExpression)
% A key axiom HasKey( CE PE1 ... PEn ) states that each (named) instance of the class expression CE is uniquely identified by the (data or object) property expressions PEi - that is, no two distinct (named) instances of CE can coincide on the values of all property expressions PEi
:- thread_local(hasKey/2).

axiompred(hasKey/2).
axiom_arguments(hasKey,[classExpression,propertyExpression]).
valid_axiom(hasKey(CE,PE)) :- subsumed_by([CE,PE],[classExpression,propertyExpression]).


%% fact(?Axiom)
% OWL 2 supports a rich set of axioms for stating assertions - axioms about individuals that are often also called facts. The fact/1 predicate reifies the fact predicate
%
% @see annotationAssertion/3, differentIndividuals/1, negativePropertyAssertion/3, propertyAssertion/3, sameIndividual/1, classAssertion/2
fact(annotationAssertion(A, B, C)) :- annotationAssertion(A, B, C).
fact(differentIndividuals(A)) :- differentIndividuals(A).
fact(negativePropertyAssertion(A, B, C)) :- negativePropertyAssertion(A, B, C).
fact(propertyAssertion(A, B, C)) :- propertyAssertion(A, B, C).
fact(sameIndividual(A)) :- sameIndividual(A).
fact(classAssertion(A, B)) :- classAssertion(A, B).
axiom_arguments(fact,[axiom]).
valid_axiom(fact(A)) :- subsumed_by([A],[axiom]).

%% sameIndividual(?Individuals:set(Individual))
% An individual equality axiom SameIndividual( a1 ... an ) states that all of the individuals ai, 1 <= i <= n, are equal to each other.
% note that despite the name of this predicate, it accepts a list of individuals as argument
:- thread_local(sameIndividual/1).

axiompred(sameIndividual/1).
axiom_arguments(sameIndividual,[set(individual)]).
valid_axiom(sameIndividual(A)) :- subsumed_by([A],[set(individual)]).

%% differentIndividuals(?Individuals:set(Individual))
% An individual inequality axiom DifferentIndividuals( a1 ... an ) states that all of the individuals ai, 1 <= i <= n, are different from each other
:- thread_local(differentIndividuals/1).

axiompred(differentIndividuals/1).
axiom_arguments(differentIndividuals,[set(individual)]).
valid_axiom(differentIndividuals(A)) :- subsumed_by([A],[set(individual)]).

%% classAssertion(?ClassExpression, ?Individual)
% A class assertion ClassAssertion( CE a ) states that the individual a is an instance of the class expression CE
:- thread_local(classAssertion/2).

axiompred(classAssertion/2).
axiom_arguments(classAssertion,[classExpression, individual]).
valid_axiom(classAssertion(A, B)) :- subsumed_by([A, B],[classExpression, individual]).

%% propertyAssertion(?PropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)
% A positive object property assertion PropertyAssertion( OPE a1 a2 ) states that the individual a1 is connected by the object property expression OPE to the individual a2
% (extensional predicate - can be asserted)
:- thread_local(propertyAssertion/3).

axiompred(propertyAssertion/3).
axiom_arguments(propertyAssertion,[propertyExpression, individual, individual]).
valid_axiom(propertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[propertyExpression, individual, individual]).

%% objectPropertyAssertion(?ObjectPropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)
% A positive object property assertion PropertyAssertion( OPE a1 a2 ) states that the individual a1 is connected by the object property expression OPE to the individual a2
objectPropertyAssertion(A, B, C) :- propertyAssertion(A, B, C),subsumed_by([A, B, C],[objectPropertyExpression, individual, individual]).
axiom_arguments(objectPropertyAssertion,[objectPropertyExpression, individual, individual]).
valid_axiom(objectPropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[objectPropertyExpression, individual, individual]).

%% dataPropertyAssertion(?ObjectPropertyExpression, ?SourceIndividual:Individual, ?TargetValue:Literal)
% A positive data property assertion PropertyAssertion( DPE a lt ) states that the individual a is connected by the data property expression DPE to the literal lt
dataPropertyAssertion(A, B, C) :- propertyAssertion(A, B, C),subsumed_by([A, B, C],[dataPropertyExpression, individual, literal]).
axiom_arguments(dataPropertyAssertion,[objectPropertyExpression, individual, literal]).
valid_axiom(dataPropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[dataPropertyExpression, individual, literal]).

%% negativePropertyAssertion(?PropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)
% A negative object property assertion NegativePropertyAssertion( OPE a1 a2 ) states that the individual a1 is not connected by the object property expression OPE to the individual a2
% (extensional predicate - can be asserted)
:- thread_local(negativePropertyAssertion/3).

axiompred(negativePropertyAssertion/3).
axiom_arguments(negativePropertyAssertion,[propertyExpression, individual, individual]).
valid_axiom(negativePropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[propertyExpression, individual, individual]).

%% negativeObjectPropertyAssertion(?ObjectPropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)
% A negative object property assertion NegativePropertyAssertion( OPE a1 a2 ) states that the individual a1 is not connected by the object property expression OPE to the individual a2
negativeObjectPropertyAssertion(A, B, C) :- negativePropertyAssertion(A, B, C),subsumed_by([A, B, C],[objectPropertyExpression, individual, individual]).
axiom_arguments(negativeObjectPropertyAssertion,[objectPropertyExpression, individual, individual]).
valid_axiom(negativeObjectPropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[objectPropertyExpression, individual, individual]).

%% negativeDataPropertyAssertion(?DataPropertyExpression, ?SourceIndividual:Individual, ?TargetValue:Literal)
% A negative data property assertion NegativePropertyAssertion( DPE a lt ) states that the individual a is not connected by the data property expression DPE to the literal lt
negativeDataPropertyAssertion(A, B, C) :- negativePropertyAssertion(A, B, C),subsumed_by([A, B, C],[dataPropertyExpression, individual, literal]).
axiom_arguments(negativeDataPropertyAssertion,[dataPropertyExpression, individual, literal]).
valid_axiom(negativeDataPropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[dataPropertyExpression, individual, literal]).

%% annotationAssertion(?AnnotationProperty, ?AnnotationSubject, ?AnnotationValue)
% An annotation assertion AnnotationAssertion( AP as av ) states that the annotation subject as - an IRI or an anonymous individual - is annotated with the annotation property AP and the annotation value av
:- thread_local(annotationAssertion/3).

axiompred(annotationAssertion/3).
axiom_arguments(annotationAssertion,[annotationProperty, annotationSubject, annotationValue]).
valid_axiom(annotationAssertion(A, B, C)) :- subsumed_by([A, B, C],[annotationProperty, annotationSubject, annotationValue]).
annotationSubject(_).
annotationValue(_).

%% annotation(?IRI,?AnnotationProperty,?AnnotationValue)
%
% @see annotationAnnotation/3, ontologyAnnotation/3, axiomAnnotation/3
:- thread_local(annotation/3).

axiompred(annotation/3).

annotation(annotationAnnotation(A, B, C)) :- annotationAnnotation(A, B, C).
annotation(axiomAnnotation(A, B, C)) :- axiomAnnotation(A, B, C).
axiom_arguments(annotation,[iri,annotationProperty,annotationValue]).
valid_axiom(annotation(A,B,C)) :- subsumed_by([A,B,C],[iri,annotationProperty,annotationValue]).

%% ontologyAnnotation(?Ontology, ?AnnotationProperty, ?AnnotationValue)
ontologyAnnotation(Ontology,AP,AV) :-
	annotation(Ontology,AP,AV),
	ontology(Ontology).
axiom_arguments(ontologyAnnotation,[ontology, annotationProperty, annotationValue]).
valid_axiom(ontologyAnnotation(A, B, C)) :- subsumed_by([A, B, C],[ontology, annotationProperty, annotationValue]).

%% axiomAnnotation(?Axiom, ?AnnotationProperty, ?AnnotationValue)
axiomAnnotation(Axiom,AP,AV) :-
	annotation(Axiom,AP,AV),
	axiom(Axiom).
axiom_arguments(axiomAnnotation,[axiom, annotationProperty, annotationValue]).
valid_axiom(axiomAnnotation(A, B, C)) :- subsumed_by([A, B, C],[axiom, annotationProperty, annotationValue]).

%% annotationAnnotation(?Annotation, ?AnnotationProperty, ?AnnotationValue)
annotationAnnotation(Annotation,AP,AV) :-
	annotation(Annotation,AP,AV),
	annotation(Annotation).
axiom_arguments(annotationAnnotation,[annotation, annotationProperty, annotationValue]).
valid_axiom(annotationAnnotation(A, B, C)) :- subsumed_by([A, B, C],[annotation, annotationProperty, annotationValue]).

%% ontology(?IRI)
% An ontology in OWL2 is a collection of OWL Axioms
:- thread_local(ontology/1).

axiompred(ontology/1).
axiom_arguments(ontology,[iri]).
valid_axiom(ontology(A)) :- subsumed_by([A],[iri]).

%% ontologyDirective(?OntologyIRI,?IRI)
% @see ontologyImport/2, ontologyAxiom/2
ontologyDirective(A, B) :- ontologyImport(A, B).
ontologyDirective(A, B) :- ontologyAxiom(A, B).
ontologyDirective(A, B) :- ontologyVersionInfo(A, B).
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
:- thread_local(ontologyAxiom/2).

axiompred(ontologyAxiom/2).
axiom_arguments(ontologyAxiom,[ontology, axiom]).
valid_axiom(ontologyAxiom(A, B)) :- subsumed_by([A, B],[ontology, axiom]).

%% ontologyImport(?Ontology, ?IRI)
% True of Ontology imports document IRI
:- thread_local(ontologyImport/2).

axiompred(ontologyImport/2).
axiom_arguments(ontologyImport,[ontology, iri]).
valid_axiom(ontologyImport(A, B)) :- subsumed_by([A, B],[ontology, iri]).

%% ontologyVersionInfo(?Ontology, ?IRI)
:- thread_local(ontologyVersionInfo/2).

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
	G.


%% iri(?IRI)
% true if IRI is an IRI. TODO: currently underconstrained, any atomic term can be an IRI
iri(IRI) :- atomic(IRI).	%

%% literal(?Lit)
% true if Lit is an rdf literal
%literal(_).			% TODO
literal(literal(_)).			% TODO

propertyExpression(E) :- objectPropertyExpression(E) ; dataPropertyExpression(E).

%% objectPropertyExpression(?OPE)
% true if OPE is an ObjectPropertyExpression
% ObjectPropertyExpression := ObjectProperty | InverseObjectProperty
objectPropertyExpression(E) :- objectProperty(E) ; inverseObjectProperty(E).

% give benefit of doubt; e.g. rdfs:label
% in the OWL2 spec we have DataProperty := IRI
% here dataProperty/1 is an asserted fact
objectPropertyExpression(E) :- nonvar(E),iri(E).

objectPropertyExpressionOrChain(propertyChain(PL)) :- forall(member(P,PL),objectPropertyExpression(P)).
objectPropertyExpressionOrChain(PE) :- objectPropertyExpression(PE).


inverseObjectProperty(inverseOf(OP)) :- objectProperty(OP).

dataPropertyExpression(E) :- dataProperty(E).

dataPropertyExpression(DPEs) :-
	(   is_list(DPEs)
	->  forall(member(DPE,DPEs),
		   dataPropertyExpression(DPE))
	;   dataPropertyExpression(DPEs)).

% give benefit of doubt; e.g. rdfs:label
% in the OWL2 spec we have DataProperty := IRI
% here dataProperty/1 is an asserted fact
dataPropertyExpression(E) :- nonvar(E),iri(E).

%already declared as entity
%datatype(IRI) :- iri(IRI).

%% dataRange(+DR) is semidet
dataRange(DR) :-
    datatype(DR) ;
    dataIntersectionOf(DR );
    dataUnionOf(DR) ;
    dataComplementOf(DR) ;
    dataOneOf(DR) ;
    datatypeRestriction(DR).

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
classExpression(CE):-
        iri(CE) ;               % NOTE: added to allow cases where class is not imported
    class(CE) ;
    objectIntersectionOf(CE) ; objectUnionOf(CE) ; objectComplementOf(CE) ; objectOneOf(CE) ;
    objectSomeValuesFrom(CE) ; objectAllValuesFrom(CE) ; objectHasValue(CE) ; objectHasSelf(CE) ;
    objectMinCardinality(CE) ; objectMaxCardinality(CE) ; objectExactCardinality(CE) ;
    dataSomeValuesFrom(CE) ; dataAllValuesFrom(CE) ; dataHasValue(CE) ;
    dataMinCardinality(CE) ; dataMaxCardinality(CE) ; dataExactCardinality(CE).

%% objectIntersectionOf(+CE) is semidet
% true if CE is a term intersectionOf(ClassExpression:list)
%
% An intersection class expression IntersectionOf( CE1 ... CEn ) contains all individuals that are instances of all class expressions CEi for 1 <= i <= n.
objectIntersectionOf(intersectionOf(CEs)) :-
	forall(member(CE,CEs),
	       classExpression(CE)).

%% objectUnionOf(+CE) is semidet
% A union class expression UnionOf( CE1 ... CEn ) contains all individuals that are instances of at least one class expression CEi for 1 <= i <= n
objectUnionOf(unionOf(CEs)) :-
	forall(member(CE,CEs),
	       classExpression(CE)).

%% objectComplementOf(+CE) is semidet
%
objectComplementOf(complementOf(CE)) :-
	classExpression(CE).

%% objectOneOf(+CE) is semidet
% An enumeration of individuals OneOf( a1 ... an ) contains exactly the individuals ai with 1 <= i <= n.
objectOneOf(oneOf(Is)) :-
        is_list(Is). % TODO: check if we need to strengthen this check
%objectOneOf(oneOf(Is)) :-
%	forall(member(I,Is),
%	       individual(I)).

%% objectSomeValuesFrom(+R) is semidet
% An existential class expression SomeValuesFrom( OPE CE ) consists of an object property expression OPE and a class expression CE, and it contains all those individuals that are connected by OPE to an individual that is an instance of CE
objectSomeValuesFrom(someValuesFrom(OPE,CE)) :-
	objectPropertyExpression(OPE),
	classExpression(CE).

%% objectAllValuesFrom(+R) is semidet
% A universal class expression AllValuesFrom( OPE CE ) consists of an object property expression OPE and a class expression CE, and it contains all those individuals that are connected by OPE only to individuals that are instances of CE
objectAllValuesFrom(allValuesFrom(OPE,CE)) :-
	objectPropertyExpression(OPE),
	classExpression(CE).

%% objectHasValue(+R) is semidet
% A has-value class expression HasValue( OPE a ) consists of an object property expression OPE and an individual a, and it contains all those individuals that are connected by OPE to a
objectHasValue(hasValue(OPE,I)) :-
	objectPropertyExpression(OPE),
	individual(I).

%% objectHasSelf(+R) is semidet
% A self-restriction HasSelf( OPE ) consists of an object property expression OPE, and it contains all those individuals that are connected by OPE to themselves
objectHasSelf(hasSelf(OPE)) :-
	objectPropertyExpression(OPE).

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


%% dataIntersectionOf(+DR:dataIntersectionOf) is semidet
% An intersection data range IntersectionOf( DR1 ... DRn ) contains all data values that are contained in the value space of every data range DRi for 1 <= i <= n. All data ranges DRi must be of the same arity
dataIntersectionOf(intersectionOf(DRs)) :-
	forall(member(DR,DRs),
	       dataRange(DR)).

%% dataUnionOf(+DR:dataUnionOf) is semidet
% A union data range UnionOf( DR1 ... DRn ) contains all data values that are contained in the value space of at least one data range DRi for 1 <= i <= n. All data ranges DRi must be of the same arity
dataUnionOf(unionOf(DRs)) :-
	forall(member(DR,DRs),
	       dataRange(DR)).

%% dataComplementOf(+DR:dataComplementOf) is semidet
% A complement data range ComplementOf( DR ) contains all literals that are not contained in the data range DR
dataComplementOf(complementOf(DR)) :-
	dataRange(DR).

%% dataOneOf(+DR:dataOneOf) is semidet
% An enumeration of literals OneOf( lt1 ... ltn ) contains exactly the explicitly specified literals lti with 1 <= i <= n
dataOneOf(oneOf(DRs)) :-
	forall(member(DR,DRs),
	       dataRange(DR)).

%% datatypeRestriction(+DR) is semidet
%
% TODO: multiple args
datatypeRestriction(datatypeRestriction(DR,FacetValues)):-
	datatype(DR),
	FacetValues=[_|_].

%% dataSomeValuesFrom(+DR) is semidet
dataSomeValuesFrom(someValuesFrom(DPE,DR)):-
	dataPropertyExpression(DPE),
	dataRange(DR).

%% dataAllValuesFrom(+DR) is semidet
dataAllValuesFrom(allValuesFrom(DPE,DR)):-
	dataPropertyExpression(DPE),
	dataRange(DR).

%% dataHasValue(+DR) is semidet
% A has-value class expression HasValue( DPE lt ) consists of a data property expression DPE and a literal lt, and it contains all those individuals that are connected by DPE to lt. Each such class expression can be seen as a syntactic shortcut for the class expression SomeValuesFrom( DPE OneOf( lt ) )
dataHasValue(hasValue(DPE,L)):-
	dataPropertyExpression(DPE),
	literal(L).

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
        axiom(Ax),
        Ax =.. [_,Arg1|_],
        (   is_list(Arg1)
        ->  member(About,Arg1)
        ;   About=Arg1).
axiom_directly_about(Ax,About) :-
	Ax=propertyAssertion(_,About,_),
        axiom(Ax).
axiom_directly_about(Ax,About) :-
	Ax=annotationAssertion(_,About,_),
        axiom(Ax).
axiom_directly_about(Ax,About) :-
	Ax=classAssertion(_,About),
        axiom(Ax).


%% axiom_directly_references(?Ax:axiom,?Ref)
%
% Ref may be
%  - an axiom
%  - a named entity
%  - an expression
axiom_directly_references(Ax,Ref) :-
        axiom(Ax),
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
        axiom(Ax),
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


:- thread_local assert_axiom_hook/1.

%% assert_axiom(+Axiom:axiom)
%
% writes an axiom to the prolog database.
% typically this will just be a matter of calling assert/1. However, in future we
% will have different backing stores (rdf_db, sql), and in these cases calls to
% this predicate will perform the appropriate actions.
%
% this also asserts ontologyAxiom/2, using nb_getval with current_ontology
assert_axiom(Axiom) :-
        Axiom,
        !.
assert_axiom(Axiom) :-
        assert(Axiom),
	(   nb_current(current_ontology,O)
        ->  assert(ontologyAxiom(O,Axiom))
        ;   true),
        !.
  
%% assert_axiom(+Axiom:axiom,+Ontology:ontology) is det
%
% as assert_axiom/1, but also asserts to ontologyAxiom/2
assert_axiom(Axiom,_) :-
        Axiom,
        !.
assert_axiom(Axiom,O) :-
        assert(Axiom),
	assert(ontologyAxiom(O,Axiom)),
        !.


:- thread_local retract_axiom_hook/1.

%% retract_axiom(+Axiom:axiom)
%
% removes an axiom from the prolog database.
% typically this will just be a matter of calling retract/1. However, in future we
% will have different backing stores (rdf_db, sql), and in these cases calls to
% this predicate will perform the appropriate actions.
%
% also removes ontologyAxiom/2 from ALL ontologies
retract_axiom(Axiom) :-
        retract_axiom_hook(Axiom),
        !.
retract_axiom(Axiom) :-
        retractall(Axiom),
	retractall(ontologyAxiom(_,Axiom)),
        !.

%% retract_axiom(+Axiom:axiom,+Ontology)
% retracts axioms from a specified ontology
retract_axiom(Axiom,Ontology) :-
        \+ var(Ontology),
	retractall(ontologyAxiom(Ontology,Axiom)),
        (   \+ ontologyAxiom(_,Axiom)
        ->  retractall(Axiom)
        ;   true),              % still exists in other ontology..
        !.


retract_all_axioms :-
        findall(A,axiom(A),Axioms),
        maplist(retract,Axioms),
        findall(ontologyAxiom(O,A),ontologyAxiom(O,A),OAxioms),
        maplist(retract,OAxioms),
	!.


owl2_model_init :-
	assert(annotationProperty('http://www.w3.org/2000/01/rdf-schema#label')),
	assert(annotationProperty('http://www.w3.org/2000/01/rdf-schema#comment')).

consult_axioms(File) :-
        consult(File).

axiom_type(A,T) :- functor(A,T,_).

/** <module> Ontology language axioms and expressions

---+ Synopsis

    Example OWL2 ontology as a prolog database:
==
class(organism).
class(animal).
class(carnivore).
class(herbivore).
objectProperty(eats).
subClassOf(animal,organism).
equivalentClasses([carnivore,intersectionOf([animal,someValuesFrom(eats,animal)])]).
disjointClasses([herbivore,carnivore]).
==

Example of use:

==
:- use_module(library(thea2/owl2_io)).
:- use_module(library(thea2/owl2_model)).

show_superclasses(OntFile,Class) :-
        load_axioms(OntFile),
        forall(subClassOf(Class,Super),
               writeln(Super)).
==

---+ Details

This module is a prolog model of the OWL2 language. It consists of predicates for both OWL2 axioms and expressions.

This model is intended to closely parallel Structural Specification and Functional-Style Syntax for OWL2 (http://www.w3.org/TR/owl2-syntax).

* Axioms and Declarations are modeled as prolog predicates (e.g. SubClassOf --> subClassOf/2)
* Class and Property Expressions are modeled as prolog terms. These can be checked via semi-deterministic predicates (e.g. objectIntersectionOf/1)
* Axiom Annotations are modeled as prolog predicates taking a reified axiom clause head as an argument (axiomAnnotation/3)
* The names should correspond exactly, with the leading uppercase character substituted for a lowercase (to avoid quoting in prolog) - the one exception is Import, which maps to the prolog predicate ontologyImport/2 (to avoid confusion with prolog import/1)
* Axioms with variable arguments are modeled as prolog predicates that take prolog lists as arguments (e.g. equivalentClasses/1)
* For programmatic convenience we provide additional abstract predicates that do not necessarily correspond to the OWL syntax (e.g. property/1,fact/1)


---++ Axioms

Extensional predicates are declared for all terminal axiom symbols in the functional syntax;  i.e. subPropertyOf/2, subClassOf/2.
These can be directly asserted, or compiled from a prolog file.

The terms from the OWL2 structural syntax are taken as primitive,
i.e. they are extensional / unit-clauses, and designed to be asserted
or compiled.

Some predicates such as property/1 are intensional - these generalize
over the OWL2 axioms are defined by prolog rules, and should not be
asserted.  In this case property/1 is defined as annotationProperty/1
or dataProperty/1 or objectProperty/1.

For the strong typed model, we also provide intensional predicates
such as subObjectPropertyOf/2 - satisfaction of this predicate is
determined at runtime based on type-checking, if subPropertyOf/2 holds.


---++ Expressions and Type checking

OWL Axioms can take either entities or expressions as arguments.
 Entities are simply prolog atoms corresponding to the IRI.
 Expressions are prolog terms;
 e.g. =|intersectionOf(a,someValuesFrom(inverseOf(p),b))|=

 (Class expressions are also known as Descriptions)

 Optional run-time checking of predicates using valid_axiom/1.

 For example =|subClassOf(intersectionOf(c1,c2),unionOf(c3,c4))|= is
 valid if c1,c2,c3,c4 are all class expressions, but
 =|subClassOf(p1,c1)|= is not valid if p1 is a property

 We can also make checks for specific types: e.g objectIntersectionOf/1

---++ Annotations

  In OWL Syntax, axiom annotations are written using an optional annotation list argument.
  We opt not to do this here; instead we use axiomAnnotation/3 where the first argument is the reified predicate head.
  E.g.

==
subClassOf(cat,mammal).
axiomAnnotation(SubClassOf(cat,mammal),author,linnaeus).
==


  ---++ Ontologies

  We use a similar scheme for annotations:

==
subClassOf(cat,mammal).
ontologyAxiom(linnaenTaxonomy,SubClassOf(cat,mammal)).
ontology(linnaenTaxonomy).
==

TODO: check edge cases, eg two ontologies have the same axioms but different annotations

---++ IRIs

By default there is no type checking of IRIs, so =|class(polarBear)|=
is allowed, even though polarBear is not an IRI - this makes for
convenience in working with example ontologies.

See prefix_IRIs/1 in owl2_util.pl for converting between short names
and valid IRIs.

---+ Open Issues

---++ Enumeration of expressions

We provide semi-deterministic predicates of the form
  ?type(+Expression).  Should the mode be extended to allow
  enumeration of all descriptions/expressions? This would probably
  require forcing all expressions to be bnodes OR possibly recursively
  analyzing the term Axiom in a call axiom(Axiom)

---++ Type checking

  Is Tom Schrijvers type checking system going to be integrated into SWI and Yap? Should we use that?

  I am attempting to put as much typing info in the pldoc comments,
  but unsure of the conventions for complex terms.

  LATEST: see owl2_metamodel.pl

---++ Ontologies

  continue using ontologyAxiom/2? Alternatively use builtin prolog module mechanism..?

---+ See Also

* owl2_from_rdf.pl
* swrl.pl

---+ Additional Information

@see     README
@license License

*/

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

:- discontiguous owl_parse_axiom/3.
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

u_assert(Term) :-
	call(Term), !; assert(Term).


convert(T,V,typed_value(T,V)).


%%	rdf_2_owl(+Base, +Ont) is det
%
%       Converts RDF triples to OWL/4 triples so that
%	their use can tracked by the OWL parser.


rdf_2_owl(_Base,Ont) :-
	debug(owl_parser, 'Removing existing owl triples',[]),
%	retractall(owl(_,_,_,Ont)),
	get_module(M),
	debug(owl_parser,'Copying RDF triples to OWL triples for Ontology ~w',[Ont]),
	M:myrdf(X,Y,Z),
	assert(owl(X,Y,Z,Ont)), fail.

rdf_2_owl(_,Ont) :-
	owl_count(Ont,Z),
	debug(owl_parser,'Number of owl triples copied: ~w',[Z]).


%%	owl_count(?U).
%       Returns/Checks the number of unused OWL triples.

owl_count(O,U) :-
	findall(1,owl(_,_,_,O),X), length(X,U).

%% expand_and_assert(S,P,O) is det
%
% adds a owl(S,P,O,not_used) after expanding namespaces.
% this is required for the triple replacement rules,
% which use shortened rdfs/owl namespaces.
% (or we could just use the expanded forms here which
%  may be faster..)
expand_and_assert(X1,Y1,Z1) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),!,
	retractall(owl(X,Y,Z, used1)),
	assert(owl(X,Y,Z, not_used)).


%%       test_use_owl(+Triples:list) is nondet
%
%       As use_owl/1, but does not consume the triple.  If owl(S,P,O)
%       in Triples has a non-ground variable then this will succeed
%       non-deterministically.  If all variables are ground, then this
%       will succeed semi-deterministically.
test_use_owl([]).
test_use_owl([owl(S,P,O)|Rest]) :-
	test_use_owl(S,P,O),
	test_use_owl(Rest).


%%       test_use_owl(?S,?P,?O)
%	As use_owl/3, but does not consume the triple. Expands the S,P,O.
%
%       If any of S, P or O is non-ground then this will succeed
%       non-deterministically.  If all variables are ground, then this
%       will succeed semi-deterministically.
test_use_owl(X1,Y1,Z1) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),!,
	owl(X,Y,Z, not_used).

test_use_owl(X1,Y1,Z1,named) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),
	owl(X,Y,Z, not_used),
	\+ sub_string(X,0,1,_,'_').


%%       use_owl(+Triples:list)
%	Marks a list of OWL triples as used, but only if all match. Expands the S,P,O.

use_owl(Triples) :-
        test_use_owl(Triples),
        use_owl_2(Triples).

% consume all triples; we have already tested the list and know that all match
use_owl_2([]).
use_owl_2([owl(S,P,O)|Triples]) :-
        use_owl(S,P,O),
        use_owl_2(Triples).


use_owl(X1,Y1,Z1) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),
	owl(X,Y,Z, not_used),
	debug(owl_parser_detail,'using ~w ~w ~w',[X,Y,Z]),
	retract(owl(X,Y,Z, not_used)),
	assert(owl(X,Y,Z,used1)).

use_owl(X1,Y1,Z1,named) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),
	owl(X,Y,Z, not_used),
	\+ sub_string(X,0,1,_,'_'),
	retract(owl(X,Y,Z, not_used)),
	assert(owl(X,Y,Z,used2)).

use_owl(X1,Y1,Z1,Term) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),
	owl(X,Y,Z, not_used),
	debug(owl_parser_detail,'using ~w ~w ~w',[X,Y,Z]),
	retract(owl(X,Y,Z, not_used)),
	assert(owl(X,Y,Z,used(Term))).


%%	use_owl(?S,?P,?O,+Named,Term)
%
%       Named = named: Same as use_owl/3, but marks only if S 	is Named URI (i.e. non blank node).

use_owl(X1,Y1,Z1,named,Term) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),
	owl(X,Y,Z, not_used),
	\+ sub_string(X,0,1,_,'_'),
	retract(owl(X,Y,Z, not_used)),
	assert(owl(X,Y,Z,used(Term))).


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
%	Char is either ':' for normal ns notation or '_' for builing
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
	use_owl(Node,Predicate,A),!,
	owl_collect_linked_nodes(Node,Predicate,InList,List1),
	owl_collect_linked_nodes(A,Predicate,List1,OutList).

owl_collect_linked_nodes(Node,Predicate,InList,OutList) :-
	use_owl(A,Predicate,Node),!,
	owl_collect_linked_nodes(Node,Predicate,InList,List1),
	owl_collect_linked_nodes(A,Predicate,List1,OutList).

owl_collect_linked_nodes(Node,_,List, [Node|List]) :-
	\+ memberchk(Node, List),!.

owl_collect_linked_nodes(_,_,List, List) :- !.


% ----------------------------------------------------------------
%                OWL Parser implementation predicates
% ----------------------------------------------------------------


%%       owl_get_bnode(+Node,+Description)
%
%	if Node is a blank (not named) node, then it is asserted in
%	the database as a blanknode(Node,Description,used) term.
%	The purpose is to record when a blank node has been used, so
%	subsequent uses of it will result in structure sharing.

owl_get_bnode(Node,Description) :-
	sub_string(Node,0,1,_,'_'),!,
	\+ blanknode(Node,_,_),
	assert(blanknode(Node,Description, used)).

owl_get_bnode(_,_).



% -----------------------------------------------------------------------
%                                Top Level  Predicates
% -----------------------------------------------------------------------

/*:- multifile owl2_io:load_axioms_hook/3.

owl2_io:load_axioms_hook(File,owl,Opts) :-
        owl_parse_rdf(File,Opts).

owl2_io:load_axioms_hook(File,ttl,Opts) :-
        ensure_loaded(library('semweb/rdf_turtle')),
        owl_parse_rdf(File,Opts).
*/
%% owl_parse_rdf(+File)
% as owl_parse_rdf/1 with empty Opts
owl_parse_rdf(F):-
	owl_parse_rdf(F,[]).

%% owl_parse_rdf(+File,+Opts:list)
% @param Opts
%  * imports(ImportFlag:Boolean) if true, follow imports
%  * clear(Clear) if Clear=complete, clears all axioms in owl2_model
owl_parse_rdf(F,Opts):-
	(   member(imports(Imports),Opts)
	->  true
	;   Imports=false),
	(   member(clear(Clear),Opts)
	->  true
	;   Clear=false),
	owl_parse(F,Clear,Clear,Imports),
	debug(owl_parser,'parsed ~w',[F]).




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
	owl2_model_init,
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


%% owl_canonical_parse_3(+IRIs:list) is det
% translate the current rdf_db into owl2_model axioms.
% First owl/4 facts are populated, and then these are translated
% according to:
% http://www.w3.org/TR/2008/WD-owl2-mapping-to-rdf-20081202/
% (table references refer to this document).
% we use an intermediate owl/4 database because the mapping
% is non-monotonic, and triples are 'consumed'
owl_canonical_parse_3([]).

owl_canonical_parse_3([IRI|Rest]) :-
	% Remove any existing not used owl fact
	retractall(owl(_,_,_,not_used)),
	% Copy the owl facts of the IRI document to the 'not_used'
	forall(owl(S,P,O,IRI),assert(owl(S,P,O,not_used))),

        debug(owl_parser,'Anon individuals in reification [see table 8]',[]),


	collect_r_nodes,

	% First parse the Ontology axiom
        owl_parse_annotated_axioms(ontology/1),

        debug(owl_parser,'Replacing patterns [see table 5]',[]),
	% remove triples based on pattern match (Table 5)
	(   forall((triple_remove(Pattern,Remove), test_use_owl(Pattern)),
	        forall(member(owl(S,P,O),Remove),use_owl(S,P,O,removed))) -> true ; true),


        % temporary fix to make up for bug in rdf parsing
        % see email to JanW July-1-2009
        forall((test_use_owl(S,P,BNode),
                atom(BNode),
                sub_atom(BNode,0,1,_,'_'),
                test_use_owl(BNode,'http://www.w3.org/1999/02/22-rdf-syntax-ns#datatype',literal(_))),
               (   use_owl(S,P,BNode,datatype_fix),
                   use_owl(BNode,'http://www.w3.org/1999/02/22-rdf-syntax-ns#datatype',literal(_)),
                   expand_and_assert(S,P,literal('')))),

	% replace matched patterns (Table 6)
        debug(owl_parser,'Replacing patterns [see table 6]',[]),
	(   setof(ReplaceWith,
                  Pattern^(   triple_replace(Pattern,ReplaceWith), % +Triples:list, ?Triples:list
                              use_owl(Pattern),
                              debug(owl_parser,'Replacing ~w ==> ~w [see table 6]',[Pattern,ReplaceWith])),
                  ReplacementSetList)
        ->  forall((member(ReplacementSet,ReplacementSetList),member(owl(S,P,O),ReplacementSet)),
                   expand_and_assert(S,P,O))
        ;   debug(owl_parser,'No replacements required',[])),

        /*
	forall(triple_replace(Pattern,ReplaceWith),
               forall(use_owl(Pattern),
                      forall(member(owl(S,P,O),ReplaceWith),
                             (   expand_and_assert(S,P,O),
                                 debug(owl_parser,'Replacing ~w ==> ~w [see table 6]',[Pattern,owl(S,P,O)]))))),
        */

	% continue with parsing using the rules...
	% Table 8, get the set of RIND - anonymous individuals in reification
	findall(X, (member(Y,['owl:Axiom','owl:Annotation',
			      'owl:AllDisjointClasses','owl:AllDisjointProperties',
			      'owl:AllDifferent','owl:NegativePropertyAssertion']),
                    test_use_owl(X,'rdf:type',Y)
                   ),
                RIND),
	nb_setval(rind,RIND),

        % Table 9, row 5
	% VV 10/3/2010 get the annotation properties before collecting the annotations.
        debug(owl_parser,'asserting annotationProperty/1 for all APs',[]),
	forall( test_use_owl(D,'rdf:type','owl:AnnotationProperty'),
		assert_axiom(annotationProperty(D))),

        % TODO - make this faster
        debug(owl_parser,'Implements function ANN(x) 3.2.2 Table 10.',[]),
	findall(_,ann(_,_),_), % find all annotations, assert annotation(X,AP,AV) axioms.

        debug(owl_parser,'Commencing parse of annotated axioms',[]),
        forall((axiompred(PredSpec),\+dothislater(PredSpec),\+omitthis(PredSpec)),
               owl_parse_annotated_axioms(PredSpec)),
        forall((axiompred(PredSpec),dothislater(PredSpec),\+omitthis(PredSpec)),
               owl_parse_annotated_axioms(PredSpec)),

        debug(owl_parser_detail,'Commencing parse of unannotated axioms',[]),
        forall((axiompred(PredSpec),\+dothislater(PredSpec),\+omitthis(PredSpec)),
               owl_parse_nonannotated_axioms(PredSpec)),
        forall((axiompred(PredSpec),dothislater(PredSpec),\+omitthis(PredSpec)),
               owl_parse_nonannotated_axioms(PredSpec)),!,
	% annotation Assertion
	parse_annotation_assertions,
	forall(owl_parse_compatibility_DL(Axiom),assert_axiom(Axiom)),
	owl_canonical_parse_3(Rest).

rdf_db_to_owl :-
	owl2_model_init,
        findall(BaseURI,
                (   rdf(Ont,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#Ontology',BaseURI:_),
                    rdf_2_owl(BaseURI,Ont),
                    owl_canonical_parse_3(IRIs)),
                IRIs).

%% translate_rdf_db(+IRI)
% translates a graph in current rdf_db instance into an owl2_model.pl set of facts.
% assumes that IRI has already been loaded using the semweb package
translate_rdf_db(BaseURI) :-
        rdf(Ont,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#Ontology',BaseURI:_),
        !,
        rdf_2_owl(BaseURI,Ont),
        owl2_model_init,
        owl_canonical_parse_3(BaseURI).


omitthis(ontology/1).


owl_parse_annotated_axioms(Pred/Arity) :-
        debug(owl_parser_detail,'[ann] Parsing all of type: ~w',[Pred]),
        functor(Head,Pred,Arity),
%        forall(owl_parse_axiom(Mod:Head),
%               (   debug(owl_parser_detail,' parsed: [~w] ~w',[Mod,Head]),
%                   assert(Mod:Head))).
	forall(owl_parse_axiom(Head,true,Annotations),
	       (   assert_axiom(Head),
	           debug(owl_parser_detail_anns,' parsed: ~w : anns: ~w',[Head,Annotations]),
		   forall(member(X,Annotations),
			  forall(aNN(X,AP,AV),
				 assert_axiom(annotation(Head,AP,AV))
		          )
			 )
	       )
	      ),
        debug(owl_parser_detail,'[ann] Done parsing all of type: ~w',[Pred]).

owl_parse_nonannotated_axioms(Pred/Arity) :-
        debug(owl_parser_detail,'[unann] Parsing all of type: ~w',[Pred]),
        functor(Head,Pred,Arity),
	forall(owl_parse_axiom(Head,false,_),
	       assert_axiom(Head)
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

%       owl_description_list(+Node, -List)
%
%       If +Node is defined as rdf:type rdf:List, then List returns
%       a prolog list of descriptions for this Node.

owl_description_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.

owl_description_list(X,[F|R]) :-
	% use_owl(X,'rdf:type','rdf:List',list), % this is now removed from graph
	use_owl(X,'rdf:first',Element,first),
	owl_description(Element,F),
	use_owl(X,'rdf:rest',Y,rest),
	!,owl_description_list(Y,R).


%       owl_individual_list(+Node, -List)
%
%       If +Node is defined as rdf:type rdf:List, then List returns
%       a prolog list of individuals for this Node.

owl_individual_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.

owl_individual_list(X,[F|R]) :-
	% use_owl(X,'rdf:type','rdf:List',list), % this is now removed from graph
	use_owl(X,'rdf:first',F,first),
	use_owl(X,'rdf:rest',Y,rest),
	!,owl_individual_list(Y,R).

%       owl_property_list(+Node, -List)
%
%       If +Node is defined as rdf:type rdf:List, then List returns
%       a prolog list of properties for this Node.

owl_property_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.

owl_property_list(X,[F|R]) :-
	% use_owl(X,'rdf:type','rdf:List',list), % this is now removed from graph
	use_owl(X,'rdf:first',Element,first),
	owl_property_expression(Element,F),
	use_owl(X,'rdf:rest',Y,rest),
	!,owl_property_list(Y,R).

%       owl_datarange_list(+Node, -List)
%
%       If +Node is defined as rdf:type rdf:List, then List returns
%       a prolog list of dataranges for this Node.

owl_datarange_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.

owl_datarange_list(X,[F|R]) :-
	% use_owl(X,'rdf:type','rdf:List',list), % this is now removed from graph
	use_owl(X,'rdf:first',Element,first),
	owl_datarange(Element,F),
	use_owl(X,'rdf:rest',Y,rest),
	!,owl_datarange_list(Y,R).

%       owl_datatype_restriction_list(+Node, -List)
%
%       If +Node is defined as rdf:type rdf:List, then List returns
%       a prolog list of datatype restrictions for this Node.

owl_datatype_restriction_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.

owl_datatype_restriction_list(X,[facetRestriction(W2,L)|R]) :-
	% use_owl(X,'rdf:type','rdf:List'), % this is now removed from graph
	use_owl(X,'rdf:first',Element,first_datatype_restr),
	use_owl(Element,W,L,datatype_restr),
	(   concat_atom([_,W2],'#',W)
	->  true
	;   W2=W),
	use_owl(X,'rdf:rest',Y,rest_datatype_restr),
	!,owl_datatype_restriction_list(Y,R).


% 3.1 Extracting Declarations and the IRIs of the Directly Imported Ontology Documents
% This section specifies the result of step CP-2.2 of the canonical parsing process on an RDF graph G


% 3.1.2 Parsing of the Ontology Header and Declarations

%  Table 4.
owl_parse_axiom(ontology(O),AnnMode,List) :-
        test_use_owl(O,'rdf:type','owl:Ontology'),
	\+ test_use_owl([owl(U,_W,O),owl(U,'rdf:type','owl:Ontology')]),
	valid_axiom_annotation_mode(AnnMode,O,'rdf:type','owl:Ontology',List),
        use_owl(O,'rdf:type','owl:Ontology',ontology),
        nb_setval(current_ontology,O),
	forall(use_owl(O,'owl:imports',IRI,ontology_import), assert_axiom(ontologyImport(O,IRI))),
	forall(use_owl(O,'owl:versionInfo',IRI2,ontology_version_info), assert_axiom(ontologyVersionInfo(O,IRI2))),!. % Do Once


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

%% owl_parse_axiom(+AxiomSpec,+AnnMode:boolean,?AnnList:list) is det
%
% None
%
owl_parse_axiom(class(C),AnnMode,List) :-
	test_use_owl(C,'rdf:type','owl:Class'),
	valid_axiom_annotation_mode(AnnMode,C,'rdf:type','owl:Class',List),
        (   use_owl(C,'rdf:type','owl:Class',named,class(C)) -> true ; use_owl(C,'rdf:type','rdfs:Class',named,class(C))),
	\+ class(C).


owl_parse_axiom(datatype(D), AnnMode, List) :-
        test_use_owl(D,'rdf:type','rdf:Datatype'),
        valid_axiom_annotation_mode(AnnMode,D,'rdf:type','rdf:Datatype',List),
        use_owl(D,'rdf:type','rdf:Datatype',datatype(D)).


owl_parse_axiom(objectProperty(D), AnnMode, List) :-
        test_use_owl(D,'rdf:type','owl:ObjectProperty'),
        valid_axiom_annotation_mode(AnnMode,D,'rdf:type','owl:ObjectProperty',List),
        use_owl(D,'rdf:type','owl:ObjectProperty',objectProperty(D)),
	\+ objectProperty(D).


% note the difference in names between syntax and rdf
owl_parse_axiom(dataProperty(D), AnnMode, List) :-
        test_use_owl(D,'rdf:type','owl:DatatypeProperty'),
        valid_axiom_annotation_mode(AnnMode,D,'rdf:type','rdf:DatatypeProperty',List),
        use_owl(D,'rdf:type','owl:DatatypeProperty',dataProperty(D)),
	\+ dataProperty(D).

owl_parse_axiom(annotationProperty(D), AnnMode, List) :-
        test_use_owl(D,'rdf:type','owl:AnnotationProperty'),
        valid_axiom_annotation_mode(AnnMode,D,'rdf:type','rdf:AnnotationProperty',List),
        use_owl(D,'rdf:type','owl:AnnotationProperty',annotationProperty(D)),
	\+ annotationProperty(D).


% TODO: check this. do we need to assert individual axioms if all we have is an rdf:type?
owl_parse_axiom(namedIndividual(D), AnnMode, List) :-
        test_use_owl(D,'rdf:type','owl:NamedIndividual'),
        valid_axiom_annotation_mode(AnnMode,D,'rdf:type','rdf:NamedIndividual',List),
        use_owl(D,'rdf:type','owl:NamedIndividual',namedIndividual(D)).


% Table 8. Identifying Anonymous Individuals in Reification
% TODO


% 3.2 Populating an Ontology


% 3.2.1 Analyzing Declarations

% 3.2.2 Parsing of Annotations

%
%       ann(?X, -Extension List)
%
%       Implements function ANN(x) 3.2.2 Table 10
%
%     The annotations in G are parsed next. The function ANN assigns a
%     set of annotations ANN(x) to each IRI or blank node x. This
%     function is initialized by setting ANN(x) = â.. for each each IRI
%     or blank node x. Next, the triple patterns from Table 10 are
%     matched in G and, for each matched pattern, ANN(x) is extended
%     with an annotation from the right column. Each time one of these
%     triple patterns is matched, the matched triples are removed from
%     G. This process is repeated until no further matches are
%     possible

ann(X,Y) :-
	ann(X,X,Y).



ann(X,X1, annotation(X1,Y,Z)) :-
	annotationProperty(Y),
        debug(owl_parser_detail,'annotation property: ~w',[Y]),
        owl(X,Y,Z,not_used),
        use_owl(X,Y,Z,annotationProperty(Y)),
	u_assert(aNN(X1,Y,Z)),
	ann2(X,Y,Z,X1).


ann2(X,Y,Z,X1) :-
	annotation_r_node(X,Y,Z,W),
	ann(W,annotation(X1,Y,Z),Term),
        u_assert(Term).

ann2(X,Y,Z,X1) :-
	axiom_r_node(X,Y,Z,W),
	ann(W,annotation(X1,Y,Z),Term),
        u_assert(Term).


ann2(_,_,_,_).


% 3.2.4 Parsing of Expressions

is_bnode(C) :-
	atom(C),
	sub_atom(C,0,1,_,'_').


	% Table 11. Parsing Object Property Expressions
owl_property_expression(C,C) :-
	\+ is_bnode(C), % better: IRI(C).
	% VV added 10/3/2011
	C\='http://www.w3.org/1999/02/22-rdf-syntax-ns#first',
	C\='http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',
        !.

owl_property_expression(C,D) :-
	blanknode(C,D,Use),
	(   Use = used,
	    retractall(blanknode(C,D,used)),
	    assert(blanknode(C,D,shared))
	;
	    true).

owl_property_expression(P,inverseOf(Q)) :-
        use_owl(P,'owl:inverseOf',Q,inverseof(P,Q)),
        owl_get_bnode(P,inverseOf(Q)).


% Table 12. Parsing of Data Ranges

owl_datarange(D,D) :-
	\+ is_bnode(D),!.  % better: IRI(C).

owl_datarange(C,D) :-
	blanknode(C,D,Use),
	(   Use = used,
	    retractall(blanknode(C,D,used)),
	    assert(blanknode(C,D,shared))
	;
	true).

owl_datarange(D,intersectionOf(L)) :-
	use_owl(D,'rdf:type','rdfs:Datatype',datarange(D)),
	use_owl(D,'owl:intersectionOf',Y,datarange(D)),
	%print(D-inter-Y),nl,
        owl_datarange_list(Y,L),
	owl_get_bnode(D,intersectionOf(L)).

owl_datarange(D,unionOf(L)) :-
	use_owl(D,'rdf:type','rdfs:Datatype',datarange(D)),
	use_owl(D,'owl:unionOf',Y,datarange(D)),
        owl_datarange_list(Y,L),
	owl_get_bnode(D,unionOf(L)).


owl_datarange(D,complementOf(DY)) :-
	use_owl(D,'rdf:type','rdfs:Datatype',dataRange(D)),
	use_owl(D,'owl:datatypeComplementOf',Y,datacomplement(D)),
        owl_datarange(Y,DY),
	owl_get_bnode(D,complementOf(DY)).

% Table 14, case 2
 owl_datarange(D,complementOf('rdfs:Literal')) :-
	use_owl(D,'rdf:type','rdfs:DataRange',dataRange(D)),
	use_owl(D,'owl:oneOf',[],oneOf(D)),
	owl_get_bnode(D,complementOf('rdfs:Literal')).

owl_datarange(D,oneOf(L)) :-
	use_owl(D,'rdf:type','rdfs:Datatype',dataType(D)),
	use_owl(D,'owl:oneOf',L1,oneOf(D)),
	owl_individual_list(L1,L),
	owl_get_bnode(D,oneOf(L)).

% Table 14, case 1
owl_datarange(D,oneOf(L)) :-
	use_owl(D,'rdf:type','rdfs:DataRange',datarange(D)),
	use_owl(D,'owl:oneOf',L1,datarange(D)),
	owl_individual_list(L1,L),
	owl_get_bnode(D,oneOf(L)).


owl_datarange(D,datatypeRestriction(DY,L)) :-
	use_owl(D,'rdf:type','rdfs:Datatype',datarange(D)),
	use_owl(D,'owl:onDatatype',Y,datarange(D)),
	owl_datarange(Y,DY),
	use_owl(D,'owl:withRestrictions',L1,datarange(D)),
	owl_datatype_restriction_list(L1,L),
	owl_get_bnode(D,datatypeRestriction(DY,L)).

% Table 13. Parsing of Class Expressions

% ----------------------------------------------------------------------
%       owl_description(+Node,-Description).
%
%	It implements OWL AS production rules for Descriptions.
%         During the construction of the Description any blank node
%         is recorded for later structure sharing checks.

owl_description(C,C) :-
	\+ is_bnode(C),!. % better: IRI(C).


owl_description(C,D) :-
	blanknode(C,D,Use),
	(   Use = used,
	    retractall(blanknode(C,D,used)),
	    assert(blanknode(C,D,shared))
	;
	    true),!.

% TODO: this leaves behind classAssertions of type owlClass for the bnodes
owl_description(D,intersectionOf(L)) :-
	use_owl(D,'owl:intersectionOf',L1,intersectionOf(D)),
	owl_description_list(L1,L),
	\+L = [],
	owl_get_bnode(D,intersectionOf(L)),!.

owl_description(D,unionOf(L)) :-
	use_owl(D,'owl:unionOf',L1,union(D)),
	owl_description_list(L1,L),
	owl_get_bnode(D,unionOf(L)),!.


owl_description(D,complementOf(Descr)) :-
	use_owl(D,'owl:complementOf',D1,complementOf(D)),
	owl_description(D1,Descr),
	owl_get_bnode(D,complementOf(Descr)),!.

owl_description(D,oneOf(L)) :-
	use_owl(D,'owl:oneOf',L1,oneOf(D)),
	(   use_owl(D,'rdf:type','owl:Class',oneOf(D,L)) ; true),
	owl_individual_list(L1,L),
	owl_get_bnode(D,oneOf(L)),!.

owl_description(D,datatypeRestriction(DY,L)) :-
	use_owl(D,'rdf:type','rdfs:Datatype',datatypeRestr(D)),
	use_owl(D,'owl:onDatatype',Y,dataType(D)),
	owl_datarange(Y,DY),
	use_owl(D,'owl:withRestrictions',L1,withRestrictions(D)),
	owl_datatype_restriction_list(L1,L),
	owl_get_bnode(D,datatypeRestriction(DY,L)).

owl_description(D,Restriction) :-
	owl_restriction(D, Restriction),
	owl_get_bnode(D,Restriction),!.


% Table 15 - OWL DL compatibility class expressions
%
owl_description(D,Result) :-
	\+ is_bnode(D), % better: IRI(C).
	use_owl(D,'rdf:type','owl:Class',description(D)),
	use_owl(D,'owl:unionOf',L,unionOf(L)),
	owl_description_list(L,DL),
	(   DL = [], Result = 'owl:Nothing' ;
	    DL = [D1], Result = D1),
	owl_get_bnode(D,Result),!.

owl_description(D,Result) :-
	\+ is_bnode(D), % better: IRI(C).
	use_owl(D,'rdf:type','owl:Class',dl_compatibility_descr(D)),
	use_owl(D,'owl:intersectionOf',L,intersectionOf(D)),
	owl_description_list(L,DL),
	(   DL = [], Result = 'owl:Thing' ;
	    DL = [D1], Result = D1),
	owl_get_bnode(D,Result),!.

owl_description(D,Result) :-
	\+ is_bnode(D),!, % better: IRI(C).
	use_owl(D,'rdf:type','owl:Class',dl_compatibility_descr(D)),
	use_owl(D,'owl:oneOf',[],oneOf(D)),
	Result = 'owl:Nothing',
	owl_get_bnode(D,Result).

% support older deprecated versions of OWL2 spec. See for example hydrology.owl
onClass(E,D) :- use_owl(E,'http://www.w3.org/2006/12/owl2#onClass',D,onClass(E)).
onClass(E,D) :- use_owl(E,'owl:onClass',D,onClass(E)).

onDataRange(E,D) :- use_owl(E, 'owl:onDataRange',D,onDatarange(E)).


%       owl_restriction(+Element,-Restriction).
%
%       If Element is defined as a owl:Restriction on property P then
%       Restriction binds to a restriction(Property,Type) term,
%	according to OWL Abstract syntax specification.

owl_restriction(Element,Restriction) :-
	use_owl(Element,'rdf:type','owl:Restriction',restriction(Element)),
	(   use_owl(Element, 'owl:onProperty',PropertyID,onProperty(Element,PropertyID)) ;
    	    use_owl(Element, 'owl:onProperties',PropertyID,onProperties(Element,PropertyID))
	),
	owl_restriction_type(Element,PropertyID, Restriction),
        debug(owl_parser_detail,'Restriction: ~w',[Restriction]).



owl_restriction_type(E, P, someValuesFrom(PX, DX)) :-
	use_owl(E, 'owl:someValuesFrom',D,someValuesFrom(E,P)),
	(   owl_description(D, DX) ; owl_datarange(D,DX)),
        (   P = [_|_], owl_property_list(P,PX) ;  owl_property_expression(P, PX)).


owl_restriction_type(E, P, allValuesFrom(PX,DX)) :-
	use_owl(E, 'owl:allValuesFrom',D,allValuesFrom(E,P)),
	(   owl_description(D, DX) ; owl_datarange(D,DX)),
        (   P = [_|_], owl_property_list(P,PX) ;  owl_property_expression(P, PX)).


% changed from thea value-->hasValue
owl_restriction_type(E, P, hasValue(PX,Value)) :-
	use_owl(E, 'owl:hasValue',Value,hasValue(E)),
        owl_property_expression(P, PX).

% VV:check if RDF parser returns a triple with O=true for
owl_restriction_type(E, P, hasSelf(PX)) :-
	use_owl(E, 'owl:hasSelf', true,hasSelf(E)),
        owl_property_expression(P, PX).

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

owl_restriction_type(E, P, exactCardinality(N,PX,DX)) :-
	test_use_owl(E, 'owl:cardinality',Lit),
        onClass(E,D),
	owl_description(D, DX),!,
	use_owl(E, 'owl:cardinality',Lit,cardinality(E)),
        literal_integer(Lit,N),
        owl_property_expression(P, PX).

owl_restriction_type(E, P, minCardinality(N,PX,DX)) :-
	test_use_owl(E, 'owl:minCardinality',Lit),
        (   onClass(E,D),owl_description(D, DX)
        ;   onDataRange(E,D), owl_datarange(D,DX)),
	!,
        % we are sure this is an old-style unqualified CR - now consume triples
	use_owl(E, 'owl:minCardinality',Lit,minCardinality(E)),
        literal_integer(Lit,N),
        owl_property_expression(P, PX).

owl_restriction_type(E, P, maxCardinality(N,PX,DX)) :-
	test_use_owl(E, 'owl:maxCardinality',Lit),
        (   onClass(E,D),owl_description(D, DX)
        ;   onDataRange(E,D), owl_datarange(D,DX)),
	!,
        % we are sure this is an old-style unqualified CR - now consume triples
	use_owl(E, 'owl:maxCardinality',Lit,maxCard(E)),
        literal_integer(Lit,N),
        owl_property_expression(P, PX).

% END OF Support of deprecated translations:

% the following are all in the spec:

% changed from Thea1->2: cardinality->exactCardinality
owl_restriction_type(E, P,exactCardinality(N,PX)) :-
	use_owl(E, 'owl:cardinality',Lit,cardinality(E)),
        literal_integer(Lit,N),
        owl_property_expression(P, PX).

owl_restriction_type(E, P,exactCardinality(N,PX,DX)) :-
	use_owl(E, 'owl:qualifiedCardinality',Lit),literal_integer(Lit,N),
	(   onClass(E,D),owl_description(D, DX) ;
	    onDataRange(E,D), owl_datarange(D,DX)
	),
        owl_property_expression(P, PX).


owl_restriction_type(E, P, minCardinality(N,PX)) :-
	use_owl(E, 'owl:minCardinality',Lit,cardinality(E)),literal_integer(Lit,N),
        owl_property_expression(P, PX).

owl_restriction_type(E, P, minCardinality(N,PX,DX)) :-
	use_owl(E, 'owl:minQualifiedCardinality',Lit,cardinality(E)),literal_integer(Lit,N),
	(   onClass(E,D),owl_description(D, DX);
	    onDataRange(E,D), owl_datarange(D,DX)
	),
        owl_property_expression(P, PX).


owl_restriction_type(E, P, maxCardinality(N,PX)) :-
	use_owl(E, 'owl:maxCardinality',Lit,maxCardinality(E)),literal_integer(Lit,N),
        owl_property_expression(P, PX).

owl_restriction_type(E, P, maxCardinality(N,PX,DX)) :-
	use_owl(E, 'owl:maxQualifiedCardinality',Lit,cardinality(E,Lit)),
	literal_integer(Lit,N),
	(   onClass(E,D),owl_description(D, DX);
	    onDataRange(E,D), owl_datarange(D,DX)),
        owl_property_expression(P, PX).


% Table 14. Parsing of Data Ranges for Compatibility with OWL DL
% Included into owl_datarange clauses above

% Table 15. Parsing of Class Expressions for Compatibility with OWL DL
% Included into owl_dexcription clauses above

% Table 16. Parsing of Axioms without Annotations
% Declarations handled previously
% CLASS AXIOMS
% valid_axiom_annotation_mode: add clauses for the disjoint etc ....

collect_r_nodes :-
	retractall(axiom_r_node(_,_,_,_)),
	forall(( test_use_owl(Node,'rdf:type','owl:Axiom'),
		 test_use_owl(Node,'owl:annotatedSource',S),
		 test_use_owl(Node,'owl:annotatedProperty',P),
		 test_use_owl(Node,'owl:annotatedTarget',O)),
	       (assert(axiom_r_node(S,P,O,Node)),
                debug(owl_parser_detail,'~w',[axiom_r_node(S,P,O,Node)]),
		use_owl([owl(Node,'rdf:type','owl:Axiom'),
			 owl(Node,'owl:annotatedSource',S),
			 owl(Node,'owl:annotatedProperty',P),
			 owl(Node,'owl:annotatedTarget',O)]))),

	retractall(annotation_r_node(_,_,_,_)),
	forall(( test_use_owl(W,'rdf:type','owl:Annotation'),
		 test_use_owl(W,'owl:annotatedSource',S),
		 test_use_owl(W,'owl:annotatedProperty',P),
		 test_use_owl(W,'owl:annotatedTarget',O)),
	       (assert(annotation_r_node(S,P,O,Node)),
                debug(owl_parser_detail,'~w',[annotation_r_node(S,P,O,Node)]),
		use_owl([owl(W,'rdf:type','owl:Annotation'),
			 owl(W,'owl:annotatedSource',S),
			 owl(W,'owl:annotatedProperty',P),
			 owl(W,'owl:annotatedTarget',O)]))).

%% valid_axiom_annotation_mode(+AnnMode,+S,+P,+O,?AnnotationNodes:list) is det
% if AnnMode is true and annotation triples can be found then
% unify AnnotationNodes with the Nodes that annotate the triple,
% otherwise []

valid_axiom_annotation_mode(_Mode,S,P,O,List) :-
        expand_ns(P,PE),
        findall(Node,axiom_r_node(S,PE,O,Node),List).


owl_parse_axiom(subClassOf(DX,DY),AnnMode,List) :-
	test_use_owl(X,'rdfs:subClassOf',Y),
	valid_axiom_annotation_mode(AnnMode,X,'rdfs:subClassOf',Y,List),
	use_owl(X,'rdfs:subClassOf',Y,subclassOf(X,Y)),
        owl_description(X,DX),
	owl_description(Y,DY).

% Process each equivalentClass pair separately in order to capture
% annotations. Block the maximally connected subgraph.
% TODO. Process the equivalent(L) axioms to generate maximally connected
% equivalentClasses(L) axioms. (but without annotations?)

owl_parse_axiom(equivalentClasses(DL),AnnMode,List) :-
	test_use_owl(X,'owl:equivalentClass',Y),
	valid_axiom_annotation_mode(AnnMode,X,'owl:equivalentClass',Y,List),
	use_owl(X,'owl:equivalentClass',Y,equivalentClass(X,Y)),
        % maximally_connected_subgraph_over('owl:equivalentClass',L),
        maplist(owl_description,[X,Y],DL),
        debug(owl_parser_detail,'equivalentClasses Descs: ~w',[DL]).


owl_parse_axiom(equivalentClasses([C,intersectionOf(D)]),AnnMode,List) :-
	class(C),
	test_use_owl(C,'owl:intersectionOf',D1),
	debug(owl_parser,'equivalent collection; intersection for ~w',[C]),
	valid_axiom_annotation_mode(AnnMode,C,'owl:intersectionOf',D1,List),
	owl_description(C,intersectionOf(D)).

owl_parse_axiom(equivalentClasses([C,unionOf(D)]),AnnMode,List) :-
	class(C),
	test_use_owl(C,'owl:unionOf',D1),
	debug(owl_parser,'equivalent collection; union for ~w',[C]),
	valid_axiom_annotation_mode(AnnMode,C,'owl:unionOf',D1,List),
	owl_description(C,unionOf(D)).

owl_parse_axiom(equivalentClasses([C,oneOf(D)]),AnnMode,List) :-
	class(C),
	test_use_owl(C,'owl:oneOf',D1),
	debug(owl_parser,'equivalent collection; one of for ~w',[C]),
	valid_axiom_annotation_mode(AnnMode,C,'owl:oneOf',D1,List),
	owl_description(C,oneOf(D)).


owl_parse_axiom(equivalentClasses([C,D])) :-
        % TODO: this could be made more efficient by enforcing order of building
        (   test_use_owl(C,'rdf:type','owl:Class',named)
        ;   test_use_owl(C,'rdf:type','rdfs:Class',named)
        ;   class(C)),
        owl_description(C,D),
        C\=D.

% TODO. Process the disjointClasses(L) axioms to generate
% larger set of disjoint: ie if N classes are pairwise DisJoint
% then we can assert a disjointClasses for all N

owl_parse_axiom(disjointClasses([DX,DY]),AnnMode,List) :-
	test_use_owl(X,'owl:disjointWith',Y),
	valid_axiom_annotation_mode(AnnMode,X,'owl:disjointWith',Y,List),
	use_owl(X,'owl:disjointWith',Y,disjointWith(X,Y)),
        owl_description(X,DX),
	owl_description(Y,DY).

% One of the cases where annotations are those of _x and we do not seek
% for further annotation axioms. Par. 3.2.5.
% Whatever the AnnNode, _x is returned (will be ignored if mode false

owl_parse_axiom(disjointClasses(L),_AnnMode,[X]) :-
        % TODO: X may be referred to in an annotation axiom??
	use_owl(X,'rdf:type','owl:AllDisjointClasses',allDisjointClasses(X)),
        use_owl(X,'owl:members',L1,members(L1)),
        owl_description_list(L1,L).


owl_parse_axiom(disjointUnion(DX,DY),AnnMode,List) :-
	test_use_owl(X,'owl:disjointUnionOf',Y),
	valid_axiom_annotation_mode(AnnMode,X,'owl:disjointUnionOf',Y,List),
	use_owl(X,'owl:disjointUnionOf',Y,disjointUnionOf(X,Y)),
        owl_description(X,DX),
        owl_description_list(Y,DY).


% PROPERTY AXIOMS


% introduces bnode
owl_parse_axiom(subPropertyOf(propertyChain(PL),QX),AnnMode,List) :-
	test_use_owl(Q,'owl:propertyChainAxiom',L1),
	valid_axiom_annotation_mode(AnnMode,Q,'owl:propertyChainAxiom',L1,List),
	use_owl(Q,'owl:propertyChainAxiom',L1,propertyChainAxiom(Q)),
	owl_property_list(L1,PL),
        owl_property_expression(Q,QX).

owl_parse_axiom(subPropertyOf(PX,QX),AnnMode,List) :-
	test_use_owl(P,'rdfs:subPropertyOf',Q),
	valid_axiom_annotation_mode(AnnMode,P,'rdfs:subPropertyOf',Q,List),
	use_owl(P,'rdfs:subPropertyOf',Q,subPropertyOf(P,Q)),
        owl_property_expression(P,PX),
        owl_property_expression(Q,QX).


% Process each equivalentProperty pair separately in order to capture
% annotations. Block the maximally connected subgraph.
% TODO. Process the equivalent(L) axioms to generate maximally connected
% equivalentProperties(L) axioms. (but without annotations?)

owl_parse_axiom(equivalentProperties(OPEL),AnnMode,List) :-
	test_use_owl(X,'owl:equivalentProperty',Y),
	valid_axiom_annotation_mode(AnnMode,X,'owl:equivalentProperty',Y,List),
	use_owl(X,'owl:equivalentProperty',Y,equivProperty(X,Y)),
	% maximally_connected_subgraph_over('owl:equivalentProperty',L),
	maplist(owl_property_expression,[X,Y],OPEL).


% TODO. Process the disjointProperties(L) axioms to generate
% larger set of disjoint: ie if N properties are pairwise DisJoint
% then we can assert a disjointClasses for all N

owl_parse_axiom(disjointProperties([DX,DY]),AnnMode,List) :-
	test_use_owl(X,'owl:propertyDisjointWith',Y),
	valid_axiom_annotation_mode(AnnMode,X,'owl:propertyDisjointWith',Y,List),
	use_owl(X,'owl:propertyDisjointWith',Y,propertyDisjointWith(X,Y)),
        owl_description(X,DX),
	owl_description(Y,DY).

% One more of the cases where annotations are those of _x and we do not
% seek for further annotation axioms. Par. 3.2.5. Whatever the AnnNode,
% _x is returned (will be ignored if mode false)

owl_parse_axiom(disjointProperties(L),_AnnMode,[X]) :-
        % TODO: X may be referred to in an annotation axiom??
	use_owl(X,'rdf:type','owl:AllDisjointProperties',allDisjointProps(X,L1)),
        use_owl(X,'owl:members',L1,members(L1)),
        L1 = [_,_|_],           % length >= 2
        owl_property_list(L1,L).


owl_parse_axiom(propertyDomain(PX,CX),AnnMode,List) :-
	test_use_owl(P,'rdfs:domain',C),
	valid_axiom_annotation_mode(AnnMode,P,'rdfs:domain',C,List),
        use_owl(P,'rdfs:domain',C,domain(P,C)),
	(   annotationProperty(P),CX = C ;
	    owl_property_expression(P,PX),
	    owl_description(C,CX)
	).

% We need to distinguish here between object and data property
% Currently we first test if the range is a class, this means OPE
% otherwise if it is a datarange it means a DPE.
% Ideally we should also check possible declarations of OPE or DPE.

owl_parse_axiom(propertyRange(PX,CX),AnnMode,List) :-
	test_use_owl(P,'rdfs:range',C),
	valid_axiom_annotation_mode(AnnMode,P,'rdfs:range',C,List),
        use_owl(P,'rdfs:range',C,range(P,C)),
	(   annotationProperty(P) -> PX = P, CX = C ;
	    owl_property_expression(P,PX),
            (   owl_description(C,CX) -> true ; owl_datarange(C,CX))
	).

owl_parse_axiom(inverseProperties(PX,QX),AnnMode,List) :-
	test_use_owl(P,'owl:inverseOf',Q),
	valid_axiom_annotation_mode(AnnMode,P,'owl:inverseOf',Q,List),
	use_owl(P,'owl:inverseOf',Q,inverseOf(P,Q)),
        owl_property_expression(P,PX),
        owl_property_expression(Q,QX).

owl_parse_axiom(functionalProperty(P),AnnMode,List) :-
	test_use_owl(P,'rdf:type','owl:FunctionalProperty'),
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:FunctionalProperty',List),
        use_owl(P,'rdf:type','owl:FunctionalProperty',functionalProperty(P)).

owl_parse_axiom(inverseFunctionalProperty(P),AnnMode,List) :-
	test_use_owl(P,'rdf:type','owl:InverseFunctionalProperty'),
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:InverseFunctionalProperty',List),
        use_owl(P,'rdf:type','owl:InverseFunctionalProperty',inverseFunctionalProperty(P)).

owl_parse_axiom(reflexiveProperty(P),AnnMode,List) :-
	test_use_owl(P,'rdf:type','owl:ReflexiveProperty'),
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:ReflexiveProperty',List),
        use_owl(P,'rdf:type','owl:ReflexiveProperty',reflexiveProperty(P)).

owl_parse_axiom(irreflexiveProperty(P),AnnMode,List) :-
	test_use_owl(P,'rdf:type','owl:IrreflexiveProperty'),
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:IrreflexiveProperty',List),
        use_owl(P,'rdf:type','owl:IrreflexiveProperty',irreflexiveProperty(P)).

owl_parse_axiom(symmetricProperty(P),AnnMode,List) :-
	test_use_owl(P,'rdf:type','owl:SymmetricProperty'),
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:SymmetricProperty',List),
        use_owl(P,'rdf:type','owl:SymmetricProperty',symmetricProperty(P)).

owl_parse_axiom(asymmetricProperty(P),AnnMode,List) :-
	test_use_owl(P,'rdf:type','owl:AsymmetricProperty'),
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:AsymmetricProperty',List),
        use_owl(P,'rdf:type','owl:AsymmetricProperty',assymetricProperty(P)).

owl_parse_axiom(transitiveProperty(P),AnnMode,List) :-
	test_use_owl(P,'rdf:type','owl:TransitiveProperty'),
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:TransitiveProperty',List),
	use_owl(P,'rdf:type','owl:TransitiveProperty',transitiveProperty(P)).

owl_parse_axiom(hasKey(CX,L),AnnMode,List) :-
	test_use_owl(C,'owl:hasKey',L1),
	valid_axiom_annotation_mode(AnnMode,C,'owl:hasKey',L1,List),
	use_owl(C,'owl:hasKey',L1,hasKey(C)),
	owl_description(C,CX),
        L1 = [_,_|_],           % length >= 2
        owl_property_list(L1,L).

% INDIVIDUALS

owl_parse_axiom(sameIndividual([X,Y]),AnnMode,List) :-
	test_use_owl(X,'owl:sameAs',Y),
	valid_axiom_annotation_mode(AnnMode,X,'owl:sameAs',Y,List),
	use_owl(X,'owl:sameAs',Y,sameAs(X,Y)).

owl_parse_axiom(differentIndividuals([X,Y]),AnnMode,List) :-
	test_use_owl(X,'owl:differentFrom',Y),
	valid_axiom_annotation_mode(AnnMode,X,'owl:differentFrom',Y,List),
	use_owl(X,'owl:differentFrom',Y,differentFrom(X,Y)).

owl_parse_axiom(differentIndividuals(L),_AnnMode,[X]) :-
	use_owl(X,'rdf:type','owl:AllDifferent',allDifferent(L)),
	use_owl(X,'owl:distinctMembers',L1,distinctMembers(L)),
        owl_individual_list(L1,L).

owl_parse_axiom(differentIndividuals(L),_AnnMode,[X]) :-
	use_owl(X,'rdf:type','owl:AllDifferent',allDifferent(X)),
	use_owl(X,'owl:members',L1,members(L)),
        owl_individual_list(L1,L).

% make sure this is done before fetching classAssertion/2;
% -- the annotationAssertion matching clause should preceded the classAssertion/2 matching clause
owl_parse_axiom(annotationAssertion('owl:deprecated', X, true),AnnMode,List) :-
	test_use_owl(X, 'rdf:type', 'owl:DeprecatedClass'),
	valid_axiom_annotation_mode(AnnMode,X,'rdf:type','owl:DeprecatedClass',List),
	use_owl(X, 'rdf:type', 'owl:DeprecatedClass',deprecatedClass(X)).

% make sure this is done before fetching propertyAssertion/3
% this clause should precede it
owl_parse_axiom(annotationAssertion('owl:deprecated', X, true),AnnMode,List) :-
	test_use_owl(X, 'rdf:type', 'owl:DeprecatedProperty'),
	valid_axiom_annotation_mode(AnnMode,X,'rdf:type','owl:DeprecatedProperty',List),
	use_owl(X, 'rdf:type', 'owl:DeprecatedProperty',deprecatedProperty(X)).

% Table 17. Parsing of Annotated Axioms

dothislater(annotationAssertion/3).
% TODO - only on unnannotated pass?
%

owl_parse_axiom(annotationAssertion(P,A,B),AnnMode,List) :-
        annotationProperty(P),
        test_use_owl(A,P,B),         % B can be literal or individual
        valid_axiom_annotation_mode(AnnMode,A,P,B,List),
        use_owl(A,P,B,annotationProperty(P)).


dothislater(classAssertion/2).
owl_parse_axiom(classAssertion(CX,X),AnnMode,List) :-
	test_use_owl(X,'rdf:type',C),
        C\='http://www.w3.org/2002/07/owl#DeprecatedClass',
	% note: some ontologies may include a rdf:type with no
	%  explicit class declaration. See testfiles/test_undeclared.owl
	%class(C),
	valid_axiom_annotation_mode(AnnMode,X,'rdf:type',C,List),
	use_owl(X,'rdf:type',C,classAssertion(CX,X)),
        % I added this to avoid class assertions for bNodes. Perhaps a better
        % way is to simply consume the owl4/ triple at the time of translating
        % the description? --CJM
        C\='http://www.w3.org/2002/07/owl#Class',
        %
        C\='http://www.w3.org/1999/02/22-rdf-syntax-ns#Property',
        owl_description(C,CX).

dothislater(propertyAssertion/3).
owl_parse_axiom(propertyAssertion(PX,A,BX),AnnMode,List) :-
        test_use_owl(A,P,B), % B can be literal or individual
        P\='http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
	% note: some ontologies may include a triples with no
	%  explicit property declaration. See testfiles/test_undeclared.owl
	%property(P),
	valid_axiom_annotation_mode(AnnMode,A,P,B,List),
        \+ annotationProperty(P), % these triples should have been removed before, during ann parsing
	owl_property_expression(P,PX), % can also be inverse
	% next line added by VV 9/3/2011 for Jochem Liem to support ID-lists as PA objects
	(   owl_individual_list(B,BX) -> true ; BX = B),
        use_owl(A,P,B,propertyAssertion(PX,A,BX)).


owl_parse_axiom(negativePropertyAssertion(PX,A,B),_,X) :-
        use_owl(X,'rdf:type','owl:NegativePropertyAssertion',negPropertyAssertion(PX,A,B)),
        use_owl(X,'owl:sourceIndividual',A,negPropertyAssertion(PX,A,B)),
        use_owl(X,'owl:assertionProperty',P,negPropertyAssertion(PX,A,B)),
        use_owl(X,'owl:targetValue',B,negPropertyAssertion(PX,A,B)),
        owl_property_expression(P,PX).


% process hooks; SWRL etc

% Parsing annotationAssertions
%

parse_annotation_assertions :-
	( nb_current(rind,RIND) -> true ; RIND = []),!,
	forall((aNN(X,AP,AV),findall( aNN(annotation(X,AP,AV),AP1,AV1),
				      aNN(annotation(X,AP,AV),AP1,AV1),ANN), \+member(X,RIND), \+name(X,[95, 95, 68, 101, 115, 99, 114, 105, 112, 116, 105, 111, 110|_])),
	       (   assert_axiom(annotationAssertion(AP,X,AV)),
		  %  VV 10/3/2010 keep annotation/3
		  % retract(annotation(X,AP,AV)),
		   forall(member(aNN(_,AP1,AV1),ANN),
			    assert_axiom(annotation(annotationAssertion(AP,X,AV),AP1,AV1))
			 )
	       )
	      ),
	% forall(aNN(X,Y,Z),assert(annotation(X,Y,Z))), VV remove 25/1/11
	% annotation/3 axioms created already during owl_parse_annotated_axioms/1
	retractall(aNN(_,_,_)).

% Table 18. Parsing of Axioms for Compatibility with OWL DL

owl_parse_compatibility_DL(equivalentClasses([CEX,complementOf(CEY)])) :-
	use_owl(X,'owl:complementOf',Y,eq_classes),
	owl_description(X,CEX),
	owl_description(Y,CEY).


owl_parse_compatibility_DL(equivalentClasses([CEX,CEY])) :-
	use_owl(X,'owl:unionOf',Y,eq_classes),
	owl_description(X,CEX),
	owl_description_list(Y,DL),
	(   DL = [] -> CEY = 'owl:Nothing' ; (DL=[CEY]->true;CEY = unionOf(DL))).

owl_parse_compatibility_DL(equivalentClasses([CEX,CEY])) :-
	use_owl(X,'owl:intersectionOf',Y,eq_classes),
	owl_description(X,CEX),
	owl_description_list(Y,DL),
	(   DL = [] -> CEY = 'owl:Thing' ; (DL=[CEY]->true;CEY = intersectionOf(DL))).

owl_parse_compatibility_DL(equivalentClasses([CEX,CEY])) :-
	use_owl(X,'owl:oneOf',Y,eq_classes),
	owl_description(X,CEX),
	owl_description_list(Y,DL),
	(   DL = [] -> CEY = 'owl:Nothing' ; CEY = oneOf(DL)).

% UTIL

%% maximally_connected_subgraph_over(+P,?ConnectedSets) is semidet
maximally_connected_subgraph_over(P,CSet):-
        maximally_connected_subgraph_over(P,[],CSetL),
        member(CSet,CSetL).

%% maximally_connected_subgraph_over(+P,+Used,?ListOfConnectedSets) is det
maximally_connected_subgraph_over(P,Used,[CSet|All]):-
        test_use_owl(X,P,Y), % seed
        \+ member(X,Used),
        \+ member(Y,Used),
        use_owl(X,P,Y,maximally_conected), % seed
        !,
        extend_set_over(P,[X,Y],CSet),
        append(CSet,Used,Used2),
        maximally_connected_subgraph_over(P,Used2,All).
maximally_connected_subgraph_over(_,_,[]).


% det
extend_set_over(P,L,L2):-
        member(X,L),
        test_use_owl(X,P,Y),
        \+ member(Y,L),
        use_owl(X,P,Y,extend_set_over),
        !,extend_set_over(P,[Y|L],L2).
extend_set_over(P,L,L2):-
        member(X,L),
        test_use_owl(Y,P,X),
        \+ member(Y,L),
        use_owl(Y,P,X,extend_set_over),
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
:- thread_local ns4query/1.

load_owl(String):-
  retractall(ns4query(_)),
  open(String,read,S),
  load_owl_from_stream(S).
  
load_owl_from_string(String):-
  open_chars_stream(String,S),
  load_owl_from_stream(S).
  
load_owl_from_stream(S):-
  get_module(M),
  process_rdf(stream(S), assert_list(M), [namespaces(NSList)]),
  close(S),
  add_kb_prefixes(NSList),
  rdf_2_owl('ont','ont'),
  owl_canonical_parse_3(['ont']),
  parse_probabilistic_annotation_assertions.

% Adds a list of kb prefixes into ns4query
add_kb_prefixes([]).
add_kb_prefixes([(H=H1)|T]):-
  trill:add_kb_prefix(H,H1),
  add_kb_prefixes(T).

% Adds a prefix into ns4query
:- multifile trill:add_kb_prefix/2.

trill:add_kb_prefix('',B):- !,
  trill:add_kb_prefix([],B).

trill:add_kb_prefix(A,B):-
  ns4query(L),!,
  (\+ member((A=_),L) ->
      (retract(ns4query(L)),
       append(L,[(A=B)],NL),
       assert(ns4query(NL))
      )
    ;
      true
   ).
trill:add_kb_prefix(A,B):-
  assert(ns4query([(A=B)])).

% Removes a prefix from ns4query
:- multifile trill:remove_kb_prefix/2.
trill:remove_kb_prefix(A,B):-
  ns4query(L),!,
  (member((A=B),L) ->
      (retract(ns4query(L)),
       delete(L,(A=B),NL),
       assert(ns4query(NL))
      )
    ;
      true
   ).

:- multifile trill:remove_kb_prefix/1.
trill:remove_kb_prefix(A):-
  ns4query(L),!,
  (member((A=B),L) *->
      (retract(ns4query(L)),
       delete(L,(A=B),NL),
       assert(ns4query(NL))
      )
    ;
      (member((B=A),L),! *->
        (retract(ns4query(L)),
         delete(L,(B=A),NL),
         assert(ns4query(NL))
        )
      ;
        true
     )
   ).


assert_list(_M,[], _):-!.
assert_list(M,[H|T], Source) :-
    H=..[_|Args],
    H1=..[myrdf|Args],
	assert(M:H1),
        assert_list(M,T, Source).

find_all_probabilistic_annotations(Ax,PV):-
	annotation(Ax,'https://sites.google.com/a/unife.it/ml/disponte#probability',literal(type(_Type, PV))),
	atomic(PV).

find_all_probabilistic_annotations(Ax,PV):-
	annotation(Ax,'https://sites.google.com/a/unife.it/ml/disponte#probability',literal(PV)),
	atomic(PV).
  

parse_probabilistic_annotation_assertions :-
  forall(find_all_probabilistic_annotations(Ax,PV),
       (assert_axiom(annotationAssertion('https://sites.google.com/a/unife.it/ml/disponte#probability',Ax,literal(PV))))
  ),
  % forall(aNN(X,Y,Z),assert(annotation(X,Y,Z))), VV remove 25/1/11
  % annotation/3 axioms created already during owl_parse_annotated_axioms/1
  retractall(annotation(_,'https://sites.google.com/a/unife.it/ml/disponte#probability',_)).

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
  expand_all_ns(Args,NSList,NewArgs),!,
  NQ =.. [P|NewArgs],
  set_new_query(CQArgs,PosQ,NQ,CQNewArgs),
  NCQ =.. [CQP|CQNewArgs],
  call(NCQ).
  
query_expand(Q):-
  Q =.. [P|Args],
  get_module(M),
  M:ns4query(NSList),!,
  %retract(M:ns4query(NSList)),
  expand_all_ns(Args,NSList,NewArgs),!,
  NQ =.. [P|NewArgs],
  call(NQ).
*/

expand_all_ns([],_,[]).

expand_all_ns([P|T],NSList,[P|NewArgs]):-
  compound(P),
  P =.. ['literal' | _],!,
  expand_all_ns(T,NSList,NewArgs).

expand_all_ns([P|T],NSList,[NP|NewArgs]):-
  compound(P),
  P =.. [N, Args],!,
  ( is_list(Args) ->
      expand_all_ns(Args,NSList,NewPArgs)
    ;
      expand_all_ns([Args],NSList,[NewPArgs])
  ),
  NP =.. [N, NewPArgs],
  expand_all_ns(T,NSList,NewArgs).

expand_all_ns([P|T],NSList,[NP|NewArgs]):-
  compound(P),
  P =.. [N | Args],!,
  expand_all_ns(Args,NSList,NewPArgs),
  NP =.. [N| NewPArgs],
  expand_all_ns(T,NSList,NewArgs).

expand_all_ns([H|T],NSList,[H|NewArgs]):-
  check_query_arg(H),!,
  expand_all_ns(T,NSList,NewArgs).

expand_all_ns([H|T],NSList,[NewArg|NewArgs]):-
  expand_ns4query(H,NSList,NewArg),
  expand_all_ns(T,NSList,NewArgs).

check_query_arg(Arg) :-
  atomic(Arg),!,
  axiom(Ax),
  Ax =.. [_|L],
  flatten(L,L1),
  member(Arg,L1),!.

expand_ns4query(NS_URL,NSList, Full_URL):- 
	nonvar(NS_URL),
	NS_URL \= literal(_),
	uri_split(NS_URL,Short_NS,Term, ':'),
	member((Short_NS=Long_NS),NSList),
	concat_atom([Long_NS,Term],Full_URL),!.

expand_ns4query(NS_URL,NSList, Full_URL):- 
	nonvar(NS_URL),
	NS_URL \= literal(_),
	\+ sub_atom(NS_URL,_,_,_,':'),
	member(([]=Long_NS),NSList),
	concat_atom([Long_NS,NS_URL],Full_URL),!.

expand_ns4query(URL,_,URL).

:- multifile trill:add_axiom/1.
trill:add_axiom(Ax):-
  ( ns4query(NSList) *-> true; NSList = []),
  Ax =.. [P|Args],
  expand_all_ns(Args,NSList,ArgsEx),
  AxEx =.. [P|ArgsEx],
  test_and_assert(AxEx,'ont').

:- multifile trill:add_axioms/1.
trill:add_axioms([]).

trill:add_axioms([H|T]) :-
  trill:add_axiom(H),
  trill:add_axioms(T).

:- multifile trill:remove_axiom/1.
trill:remove_axiom(Ax):-
  ( ns4query(NSList) *-> true; NSList = []),
  Ax =.. [P|Args],
  expand_all_ns(Args,NSList,ArgsEx),
  AxEx =.. [P|ArgsEx],
  retract(owl2_model:AxEx),
  retract(owl2_model:owl(AxEx,'ont')).

:- multifile trill:remove_axioms/1.
trill:remove_axioms([]).

trill:remove_axioms([H|T]) :-
  trill:remove_axiom(H),
  trill:remove_axioms(T).

test_and_assert(Ax,O):-
  (\+ owl(Ax,O) ->
    (assert_axiom(Ax,O), assert(owl(Ax,O)))
   ;
    true
  ).

get_module(M):-
  pengine_self(Self),
  pengine_property(Self,module(M)),!.  
get_module('user'):- !.

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(owl2_model:load_owl(_)).
sandbox:safe_primitive(owl2_model:load_owl_from_string(_)).
sandbox:safe_primitive(owl2_model:expand_all_ns(_,_,_)).
%sandbox:safe_primitive(owl2_model:query_expand(_)).

user:term_expansion((:- trill),[]):-
  trill:add_kb_prefix('disponte','https://sites.google.com/a/unife.it/ml/disponte#').

user:term_expansion(kb_prefix(A,B),[]):-
  trill:add_kb_prefix(A,B).

user:term_expansion(owl_rdf(String),[]):-
  get_module(M),
  open_chars_stream(String,S),
  process_rdf(stream(S), assert_list(M), [namespaces(NSList)]),
  close(S),
  add_kb_prefixes(NSList),
  rdf_2_owl('ont','ont'),
  owl_canonical_parse_3(['ont']),
  parse_probabilistic_annotation_assertions.
  
user:term_expansion(TRILLAxiom,[]):-
  TRILLAxiom =.. [P|Args],
  member(P, [class,datatype,objectProperty,dataProperty,annotationProperty,namedIndividual,subClassOf,equivalentClasses,disjointClasses,disjointUnion,subPropertyOf,equivalentProperties,disjointProperties,
inverseProperties,propertyDomain,propertyRange,functionalProperty,inverseFunctionalProperty,reflexiveProperty,irreflexiveProperty,symmetricProperty,asymmetricProperty,transitiveProperty,hasKey,
sameIndividual,differentIndividuals,classAssertion,propertyAssertion,negativePropertyAssertion,annotationAssertion]),
  ns4query(NSList),
  ( (length(Args,1), Args = [IntArgs], is_list(IntArgs)) -> 
       ( expand_all_ns(IntArgs,NSList,ArgsExp),
         NewTRILLAxiom =.. [P,ArgsExp]
       )
     ;
       ( expand_all_ns(Args,NSList,ArgsExp),
         NewTRILLAxiom =.. [P|ArgsExp]
       )
  ),
  test_and_assert(NewTRILLAxiom,'ont').
