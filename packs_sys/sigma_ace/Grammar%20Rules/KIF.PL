% Copyright © 2002 Teknowledge Corporation
% This software released under the GNU Public License <http://www.gnu.org/copyleft/gpl.html>.

% This file is the Prolog translation
% of SUMO 1.37, saved and translated
% on 9/6/02.

% Style declarations for KIF.pl files

:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement
:- style_check(-atom).           % allows strings more than 5 lines long

instance(instance, 'BinaryPredicate'):-true.
instance(instance, 'AsymmetricRelation'):-true.
instance(instance, 'IntransitiveRelation'):-true.
domain(instance, 1, 'Entity'):-true.
domain(instance, 2, 'SetOrClass'):-true.
documentation(instance, "An object is an &%instance of a &%SetOrClass if 
it is included in that &%SetOrClass.  An individual may be an instance of many 
classes, some of which may be subclasses of others.  Thus, there is no 
assumption in the meaning of &%instance about specificity or uniqueness."):-true.
subrelation('immediateInstance', instance):-true.
instance('immediateInstance', 'AsymmetricRelation'):-true.
instance('immediateInstance', 'IntransitiveRelation'):-true.
documentation('immediateInstance', "An object is an &%immediateInstance of 
a &%SetOrClass if it is an instance of the &%SetOrClass and there does not exist a 
subclass of &%SetOrClass such that it is an instance of the subclass."):-true.
=>(['immediateInstance', '?ENTITY', '?CLASS'], [not, [exists, ['?SUBCLASS'], [and, [subclass, '?SUBCLASS', '?CLASS'], [instance, '?ENTITY', '?SUBCLASS']]]]):-true.
instance(inverse, 'BinaryPredicate'):-true.
domain(inverse, 1, 'BinaryRelation'):-true.
domain(inverse, 2, 'BinaryRelation'):-true.
documentation(inverse, "The inverse of a &%BinaryRelation is a relation 
in which all the tuples of the original relation are reversed.  In 
other words, one &%BinaryRelation is the inverse of another if they are 
equivalent when their arguments are swapped."):-true.
=>([inverse, '?REL1', '?REL2'], [forall, ['?INST1', '?INST2'], [<=>, [holds, '?REL1', '?INST1', '?INST2'], [holds, '?REL2', '?INST2', '?INST1']]]):-true.
instance(subclass, 'BinaryPredicate'):-true.
instance(subclass, 'PartialOrderingRelation'):-true.
domain(subclass, 1, 'SetOrClass'):-true.
domain(subclass, 2, 'SetOrClass'):-true.
documentation(subclass, "(&%subclass ?CLASS1 ?CLASS2) means that ?CLASS1 is 
a subclass of ?CLASS2, i.e. every instance of ?CLASS1 is also an instance of 
?CLASS2.  A class may have multiple superclasses and subclasses."):-true.
=>([subclass, '?SUBCLASS', '?CLASS'], [forall, ['?INST'], [=>, [instance, '?INST', '?SUBCLASS'], [instance, '?INST', '?CLASS']]]):-true.
subrelation('immediateSubclass', subclass):-true.
instance('immediateSubclass', 'AsymmetricRelation'):-true.
instance('immediateSubclass', 'IntransitiveRelation'):-true.
documentation('immediateSubclass', "A &%SetOrClass ?CLASS1 is an &%immediateSubclass 
of another &%SetOrClass ?CLASS2 just in case ?CLASS1 is a subclass of ?CLASS2 and 
there is no other subclass of ?CLASS2 such that ?CLASS1 is also a subclass of it."):-true.
=>(['immediateSubclass', '?CLASS1', '?CLASS2'], [not, [exists, ['?CLASS3'], [and, [subclass, '?CLASS3', '?CLASS2'], [subclass, '?CLASS1', '?CLASS3'], [not, [equal, '?CLASS2', '?CLASS3']], [not, [equal, '?CLASS1', '?CLASS3']]]]]):-true.
instance(subrelation, 'BinaryPredicate'):-true.
instance(subrelation, 'PartialOrderingRelation'):-true.
domain(subrelation, 1, 'Relation'):-true.
domain(subrelation, 2, 'Relation'):-true.
documentation(subrelation, "A &%Relation R is a &%subrelation 
&%Relation R' if R is a &%subclass R'.  This implies that every 
tuple of R is also a tuple of R'.  Again, if R holds for some arguments 
arg_1, arg_2, ... arg_n, then R' holds for the same arguments.  Thus, a 
&%Relation and its subrelation must have the same valence. In CycL, 
&%subrelation is called #$genlPreds."):-true.
=>([and, [subrelation, '?PRED1', '?PRED2'], [valence, '?PRED1', '?NUMBER']], [valence, '?PRED2', '?NUMBER']):-true.
=>([and, [subrelation, '?PRED1', '?PRED2'], [domain, '?PRED2', '?NUMBER', '?CLASS2'], [domain, '?PRED1', '?NUMBER', '?CLASS1']], [subclass, '?CLASS1', '?CLASS2']):-true.
=>([and, [subrelation, '?REL1', '?REL2'], [holds, '?REL1', '@ROW']], [holds, '?REL2', '@ROW']):-true.
=>([and, [subrelation, '?PRED1', '?PRED2'], [instance, '?PRED2', '?CLASS'], [instance, '?CLASS', 'InheritableRelation']], [instance, '?PRED1', '?CLASS']):-true.
instance(domain, 'TernaryPredicate'):-true.
domain(domain, 1, 'Relation'):-true.
domain(domain, 2, 'PositiveInteger'):-true.
domain(domain, 3, 'SetOrClass'):-true.
documentation(domain, "Provides a computationally and heuristically
convenient mechanism for declaring the argument types of a given relation.  
The formula (&%domain ?REL ?INT ?CLASS) means that the ?INT'th element of each 
tuple in the relation ?REL must be an instance of ?CLASS.  Specifying argument
types is very helpful in maintaining ontologies.  Representation systems can 
use these specifications to classify terms and check integrity constraints.  
If the restriction on the argument type of a &%Relation is not captured by a 
&%SetOrClass already defined in the ontology, one can specify a &%SetOrClass 
compositionally with the functions &%UnionFn, &%IntersectionFn, etc."):-true.
instance('domainSubclass', 'TernaryPredicate'):-true.
domain('domainSubclass', 1, 'Relation'):-true.
domain('domainSubclass', 2, 'PositiveInteger'):-true.
domain('domainSubclass', 3, 'SetOrClass'):-true.
documentation('domainSubclass', "&%Predicate used to specify argument 
type restrictions of &%Predicates.  The formula (&%domainSubclass 
?REL ?INT ?CLASS) means that the ?INT'th element of each tuple in the 
relation ?REL must be a subclass of ?CLASS."):-true.
=>([and, [subrelation, '?REL1', '?REL2'], ['domainSubclass', '?REL1', '?NUMBER', '?CLASS1'], ['domainSubclass', '?REL2', '?NUMBER', '?CLASS2']], [subclass, '?CLASS1', '?CLASS2']):-true.
instance(equal, 'BinaryPredicate'):-true.
instance(equal, 'EquivalenceRelation'):-true.
instance(equal, 'RelationExtendedToQuantities'):-true.
domain(equal, 1, 'Entity'):-true.
domain(equal, 2, 'Entity'):-true.
documentation(equal, "(equal ?ENTITY1 ?ENTITY2) is true just in case 
?ENTITY1 is identical with ?ENTITY2."):-true.
=>([equal, '?THING1', '?THING2'], [forall, ['?ATTR'], [<=>, [attribute, '?THING1', '?ATTR'], [attribute, '?THING2', '?ATTR']]]):-true.
=>([equal, '?THING1', '?THING2'], [forall, ['?CLASS'], [<=>, [instance, '?THING1', '?CLASS'], [instance, '?THING2', '?CLASS']]]):-true.
instance(range, 'BinaryPredicate'):-true.
instance(range, 'AsymmetricRelation'):-true.
domain(range, 1, 'Function'):-true.
domain(range, 2, 'SetOrClass'):-true.
documentation(range, "Gives the range of a function.  In other words, 
(&%range ?FUNCTION ?CLASS) means that all of the values assigned by 
?FUNCTION are &%instances of ?CLASS."):-true.
=>([and, [range, '?FUNCTION', '?CLASS'], [equal, ['AssignmentFn', '?FUNCTION', '@ROW'], '?VALUE']], [instance, '?VALUE', '?CLASS']):-true.
=>([and, [subrelation, '?REL1', '?REL2'], [range, '?REL1', '?CLASS1'], [range, '?REL2', '?CLASS2']], [subclass, '?CLASS1', '?CLASS2']):-true.
instance('rangeSubclass', 'BinaryPredicate'):-true.
instance('rangeSubclass', 'AsymmetricRelation'):-true.
domain('rangeSubclass', 1, 'Function'):-true.
'domainSubclass'('rangeSubclass', 2, 'SetOrClass'):-true.
documentation('rangeSubclass', "(&%rangeSubclass ?FUNCTION ?CLASS) means that 
all of the values assigned by ?FUNCTION are &%subclasses of ?CLASS."):-true.
=>([and, ['rangeSubclass', '?FUNCTION', '?CLASS'], [equal, ['AssignmentFn', '?FUNCTION', '@ROW'], '?VALUE']], [subclass, '?VALUE', '?CLASS']):-true.
=>([and, [subrelation, '?REL1', '?REL2'], ['rangeSubclass', '?REL1', '?CLASS1'], ['rangeSubclass', '?REL2', '?CLASS2']], [subclass, '?CLASS1', '?CLASS2']):-true.
instance('DomainFn', 'UnaryFunction'):-true.
domain('DomainFn', 1, 'BinaryRelation'):-true.
range('DomainFn', 'SetOrClass'):-true.
documentation('DomainFn', "The domain of a &%BinaryRelation ?REL is
the &%SetOrClass of all things that bear ?REL to something."):-true.
<=>([instance, '?INST1', ['DomainFn', '?REL']], [exists, ['?INST2'], [holds, '?REL', '?INST1', '?INST2']]):-true.
instance('RangeFn', 'UnaryFunction'):-true.
domain('RangeFn', 1, 'BinaryRelation'):-true.
range('RangeFn', 'SetOrClass'):-true.
documentation('RangeFn', "The range of a &%BinaryRelation ?REL is the
&%SetOrClass of all things such that something bears ?REL to them."):-true.
<=>([instance, '?INST1', ['RangeFn', '?REL']], [exists, ['?INST2'], [holds, '?REL', '?INST2', '?INST1']]):-true.
instance(valence, 'BinaryPredicate'):-true.
instance(valence, 'AsymmetricRelation'):-true.
instance(valence, 'SingleValuedRelation'):-true.
domain(valence, 1, 'Relation'):-true.
domain(valence, 2, 'PositiveInteger'):-true.
documentation(valence, "Specifies the number of arguments that a 
relation can take.  If a relation does not have a fixed number of 
arguments, it does not have a valence and it is an instance of 
&%VariableArityRelation.  For example, &%holds is a 
&%VariableArityRelation."):-true.
instance(documentation, 'BinaryPredicate'):-true.
instance(documentation, 'AsymmetricRelation'):-true.
domain(documentation, 1, 'Entity'):-true.
domain(documentation, 2, 'SymbolicString'):-true.
documentation(documentation, "A relation between objects in the domain 
of discourse and strings of natural language text.  The domain of 
&%documentation is not constants (names), but the objects themselves.  
This means that one does not quote the names when associating them with 
their documentation."):-true.
instance(disjoint, 'BinaryPredicate'):-true.
instance(disjoint, 'SymmetricRelation'):-true.
instance(disjoint, 'IrreflexiveRelation'):-true.
domain(disjoint, 1, 'SetOrClass'):-true.
domain(disjoint, 2, 'SetOrClass'):-true.
documentation(disjoint, "&%Classes are &%disjoint only if they share no 
instances, i.e. just in case the result of applying &%IntersectionFn to
them is empty."):-true.
=>([disjoint, '?CLASS1', '?CLASS2'], [forall, ['?INST'], [not, [and, [instance, '?INST', '?CLASS1'], [instance, '?INST', '?CLASS2']]]]):-true.
instance('disjointRelation', 'BinaryPredicate'):-true.
instance('disjointRelation', 'SymmetricRelation'):-true.
instance('disjointRelation', 'IrreflexiveRelation'):-true.
domain('disjointRelation', 1, 'Relation'):-true.
domain('disjointRelation', 2, 'Relation'):-true.
'relatedInternalConcept'('disjointRelation', disjoint):-true.
documentation('disjointRelation', "(&%disjointRelation ?REL1 ?REL2) means that the 
two relations ?REL1 and ?REL2 have no tuples in common.  In other words, the 
intersection of ?REL1 and ?REL2 is the null set."):-true.
=>([and, ['disjointRelation', '?REL1', '?REL2'], [valence, '?REL1', '?NUMBER']], [valence, '?REL2', '?NUMBER']):-true.
=>([and, [domain, '?REL1', '?NUMBER', '?CLASS1'], [domain, '?REL2', '?NUMBER', '?CLASS2'], [disjoint, '?CLASS1', '?CLASS2']], ['disjointRelation', '?REL1', '?REL2']):-true.
=>([and, ['disjointRelation', '?REL1', '?REL2'], [holds, '?REL1', '@ROW']], [not, [holds, '?REL2', '@ROW']]):-true.
instance('contraryAttribute', 'BinaryPredicate'):-true.
instance('contraryAttribute', 'SymmetricRelation'):-true.
instance('contraryAttribute', 'IrreflexiveRelation'):-true.
domain('contraryAttribute', 1, 'Attribute'):-true.
domain('contraryAttribute', 2, 'Attribute'):-true.
documentation('contraryAttribute', "Means that the two arguments are 
properties that are opposed to one another, e.g. &%Pliable versus 
&%Rigid."):-true.
=>([and, [attribute, '?OBJ', '?ATTR1'], ['contraryAttribute', '?ATTR1', '?ATTR2']], [not, [attribute, '?OBJ', '?ATTR2']]):-true.
instance('exhaustiveDecomposition', 'Predicate'):-true.
instance('exhaustiveDecomposition', 'VariableArityRelation'):-true.
domain('exhaustiveDecomposition', 1, 'Class'):-true.
'relatedInternalConcept'('exhaustiveDecomposition', partition):-true.
documentation('exhaustiveDecomposition', "An &%exhaustiveDecomposition of a 
&%Class C is a set of subclasses of C such that every subclass of C either 
is an element of the set or is a subclass of an element of the set.  Note: 
this does not necessarily mean that the elements of the set are disjoint 
(see &%partition - a &%partition is a disjoint exhaustive decomposition.)"):-true.
for_all(['?INT'], [domain, 'exhaustiveDecomposition', '?INT', 'Class']):-true.
instance('disjointDecomposition', 'Predicate'):-true.
instance('disjointDecomposition', 'VariableArityRelation'):-true.
domain('disjointDecomposition', 1, 'Class'):-true.
'relatedInternalConcept'('disjointDecomposition', 'exhaustiveDecomposition'):-true.
'relatedInternalConcept'('disjointDecomposition', disjoint):-true.
documentation('disjointDecomposition', "A &%disjointDecomposition of a &%Class 
C is a set of subclasses of C that are mutually &%disjoint."):-true.
for_all(['?INT'], [domain, 'disjointDecomposition', '?INT', 'Class']):-true.
instance(partition, 'Predicate'):-true.
instance(partition, 'VariableArityRelation'):-true.
domain(partition, 1, 'Class'):-true.
documentation(partition, "A &%partition of a class C is a set of 
mutually &%disjoint classes (a subclass partition) which covers C.  
Every instance of C is an instance of exactly one of the subclasses 
in the partition."):-true.
<=>([partition, '@ROW'], [and, ['exhaustiveDecomposition', '@ROW'], ['disjointDecomposition', '@ROW']]):-true.
instance('relatedInternalConcept', 'BinaryPredicate'):-true.
instance('relatedInternalConcept', 'EquivalenceRelation'):-true.
domain('relatedInternalConcept', 1, 'Entity'):-true.
domain('relatedInternalConcept', 2, 'Entity'):-true.
documentation('relatedInternalConcept', "Means that the two arguments are 
related concepts within the SUMO, i.e. there is a significant similarity 
of meaning between them.  To indicate a meaning relation between a SUMO 
concept and a concept from another source, use the Predicate 
&%relatedExternalConcept."):-true.
instance('relatedExternalConcept', 'TernaryPredicate'):-true.
domain('relatedExternalConcept', 1, 'Entity'):-true.
domain('relatedExternalConcept', 2, 'SymbolicString'):-true.
domain('relatedExternalConcept', 3, 'Language'):-true.
'relatedInternalConcept'('relatedExternalConcept', 'relatedInternalConcept'):-true.
documentation('relatedExternalConcept', "Used to signify a three-place 
relation between a concept in an external knowledge source, a concept 
in the SUMO, and the name of the other knowledge source."):-true.
subrelation('synonymousExternalConcept', 'relatedExternalConcept'):-true.
'disjointRelation'('synonymousExternalConcept', 'subsumedExternalConcept'):-true.
'disjointRelation'('synonymousExternalConcept', 'subsumingExternalConcept'):-true.
documentation('synonymousExternalConcept', "(&%synonymousExternalConcept 
?STRING ?THING ?LANGUAGE) means that the SUMO concept ?THING has the 
same meaning as ?STRING in ?LANGUAGE."):-true.
subrelation('subsumingExternalConcept', 'relatedExternalConcept'):-true.
'disjointRelation'('subsumingExternalConcept', 'subsumedExternalConcept'):-true.
documentation('subsumingExternalConcept', "(&%subsumingExternalConcept 
?STRING ?THING ?LANGUAGE) means that the SUMO concept ?THING subsumes 
the meaning of ?STRING in ?LANGUAGE, i.e. the concept ?THING is broader 
in meaning than ?STRING."):-true.
subrelation('subsumedExternalConcept', 'relatedExternalConcept'):-true.
documentation('subsumedExternalConcept', "(&%subsumedExternalConcept 
?STRING ?THING ?LANGUAGE) means that the SUMO concept ?THING is subsumed 
by the meaning of ?STRING in ?LANGUAGE, i.e. the concept ?THING is narrower 
in meaning than ?STRING."):-true.
instance(name, 'BinaryPredicate'):-true.
domain(name, 1, 'Entity'):-true.
domain(name, 2, 'SymbolicString'):-true.
documentation(name, "(&%name ?ENTITY ?STRING) means that the thing ?ENTITY 
has the &%SymbolicString ?STRING as its name."):-true.
subrelation('uniqueIdentifier', name):-true.
instance('uniqueIdentifier', 'SingleValuedRelation'):-true.
documentation('uniqueIdentifier', "The class of &%names that uniquely identify 
an instance of &%Entity.  Some examples of &%uniqueIdentifiers are the keys 
of tables in database applications and the ISBN (International Standard Book 
Number)."):-true.
instance('subAttribute', 'BinaryPredicate'):-true.
instance('subAttribute', 'PartialOrderingRelation'):-true.
domain('subAttribute', 1, 'Attribute'):-true.
domain('subAttribute', 2, 'Attribute'):-true.
'disjointRelation'('subAttribute', 'successorAttribute'):-true.
documentation('subAttribute', "Means that the second argument can be 
ascribed to everything which has the first argument ascribed to it."):-true.
<=>(['subAttribute', '?ATTR1', '?ATTR2'], [forall, ['?OBJ'], [=>, [attribute, '?OBJ', '?ATTR1'], [attribute, '?OBJ', '?ATTR2']]]):-true.
=>([and, ['subAttribute', '?ATTR1', '?ATTR2'], [instance, '?ATTR2', '?CLASS']], [instance, '?ATTR1', '?CLASS']):-true.
instance('successorAttribute', 'BinaryPredicate'):-true.
instance('successorAttribute', 'AsymmetricRelation'):-true.
domain('successorAttribute', 1, 'Attribute'):-true.
domain('successorAttribute', 2, 'Attribute'):-true.
documentation('successorAttribute', "(&%successorAttribute ?ATTR1 ?ATTR2) 
means that ?ATTR2 is the attribute that comes immediately after ?ATTR1 
on the scale that they share."):-true.
instance('successorAttributeClosure', 'BinaryPredicate'):-true.
instance('successorAttributeClosure', 'TransitiveRelation'):-true.
instance('successorAttributeClosure', 'IrreflexiveRelation'):-true.
domain('successorAttributeClosure', 1, 'Attribute'):-true.
domain('successorAttributeClosure', 2, 'Attribute'):-true.
'relatedInternalConcept'('successorAttributeClosure', 'successorAttribute'):-true.
documentation('successorAttributeClosure', "The transitive closure of 
&%successorAttribute.  (&%successorAttributeClosure ?ATTR1 ?ATTR2) means 
that there is a chain of &%successorAttribute assertions connecting 
?ATTR1 and ?ATTR2."):-true.
=>(['successorAttribute', '?ATTR1', '?ATTR2'], ['successorAttributeClosure', '?ATTR1', '?ATTR2']):-true.
instance(and, 'LogicalOperator'):-true.
domain(and, 1, 'Formula'):-true.
domain(and, 2, 'Formula'):-true.
documentation(and, "The truth-functional connective of conjunction."):-true.
instance(or, 'LogicalOperator'):-true.
domain(or, 1, 'Formula'):-true.
domain(or, 2, 'Formula'):-true.
documentation(or, "The truth-functional connective of disjunction."):-true.
instance(=>, 'LogicalOperator'):-true.
domain(=>, 1, 'Formula'):-true.
domain(=>, 2, 'Formula'):-true.
documentation(=>, "The truth-functional connective of implication."):-true.
instance(<=>, 'LogicalOperator'):-true.
domain(<=>, 1, 'Formula'):-true.
domain(<=>, 2, 'Formula'):-true.
documentation(<=>, "The truth-functional connective of bi-implication."):-true.
instance(not, 'LogicalOperator'):-true.
domain(not, 1, 'Formula'):-true.
documentation(not, "The truth-functional connective of negation."):-true.
instance(forall, 'LogicalOperator'):-true.
domain(forall, 1, 'List'):-true.
domain(forall, 2, 'Formula'):-true.
documentation(forall, "The universal quantifier of predicate logic."):-true.
instance(exists, 'LogicalOperator'):-true.
domain(exists, 1, 'List'):-true.
domain(exists, 2, 'Formula'):-true.
documentation(exists, "The existential quantifier of predicate logic."):-true.
instance(entails, 'LogicalOperator'):-true.
domain(entails, 1, 'Formula'):-true.
domain(entails, 2, 'Formula'):-true.
documentation(entails, "The operator of logical entailment.  (&%entails 
?FORMULA1 ?FORMULA2) means that ?FORMULA2 can be derived from ?FORMULA1 
by means of the proof theory of SUO-KIF."):-true.
instance('AssignmentFn', 'Function'):-true.
instance('AssignmentFn', 'VariableArityRelation'):-true.
domain('AssignmentFn', 1, 'Function'):-true.
range('AssignmentFn', 'Entity'):-true.
documentation('AssignmentFn', "If F is a function with a value for the 
objects denoted by N1,..., NK, then the term (AssignmentFn F N1 ... NK) 
denotes the value of applying F to the objects denoted by N1,..., NK.  
Otherwise, the value is undefined."):-true.
instance(holds, 'Predicate'):-true.
instance(holds, 'VariableArityRelation'):-true.
domain(holds, 1, 'Relation'):-true.
documentation(holds, "(holds P N1 ... NK) is true just in case 
the tuple of objects denoted by N1,..., NK is an element of 
the &%Relation P."):-true.
<=>([and, [holds, '?REL', '@ROW', '?INST'], [instance, '?REL', 'Function']], [equal, ['AssignmentFn', '?REL', '@ROW'], '?INST']):-true.
=>([instance, '?REL', 'Predicate'], [<=>, [holds, '?REL', '@ROW'], ['?REL', '@ROW']]):-true.
instance('PowerSetFn', 'UnaryFunction'):-true.
domain('PowerSetFn', 1, 'SetOrClass'):-true.
'rangeSubclass'('PowerSetFn', 'SetOrClass'):-true.
documentation('PowerSetFn', "(&%PowerSetFn ?CLASS) maps the &%SetOrClass 
?CLASS to the &%SetOrClass of all &%subclasses of ?CLASS."):-true.
<=>([instance, '?SUBCLASS', ['PowerSetFn', '?CLASS']], [subclass, '?SUBCLASS', '?CLASS']):-true.
documentation('Entity', "The universal class of individuals.  This is the root 
node of the ontology."):-true.
for_all(['?THING'], [instance, '?THING', 'Entity']):-true.
exists(['?THING'], [instance, '?THING', 'Entity']):-true.
<=>([instance, '?CLASS', 'Class'], [subclass, '?CLASS', 'Entity']):-true.
subclass('Physical', 'Entity'):-true.
partition('Physical', 'Object', 'Process'):-true.
documentation('Physical', "An entity that has a location in space-time.  
Note that locations are themselves understood to have a location in 
space-time"):-true.
<=>([instance, '?PHYS', 'Physical'], [exists, ['?LOC', '?TIME'], [and, [located, '?PHYS', '?LOC'], [time, '?PHYS', '?TIME']]]):-true.
subclass('Object', 'Physical'):-true.
documentation('Object', "Corresponds roughly to the class of ordinary 
objects.  Examples include normal physical objects, geographical regions, 
and locations of &%Processes, the complement of &%Objects in the &%Physical 
class.  In a 4D ontology, an &%Object is something whose spatiotemporal 
extent is thought of as dividing into spatial parts roughly parallel to the 
time-axis."):-true.
subclass('SelfConnectedObject', 'Object'):-true.
documentation('SelfConnectedObject', "A &%SelfConnectedObject is any 
&%Object that does not consist of two or more disconnected parts."):-true.
instance('FrontFn', 'UnaryFunction'):-true.
instance('FrontFn', 'AsymmetricRelation'):-true.
instance('FrontFn', 'IrreflexiveRelation'):-true.
domain('FrontFn', 1, 'Object'):-true.
range('FrontFn', 'Object'):-true.
documentation('FrontFn', "A &%Function that maps an &%Object to the side 
that generally receives the most attention or that typically faces the 
direction in which the &%Object moves.  Note that this is a partial 
function, since some &%Objects do not have sides, e.g. apples and 
spheres.  Note too that the &%range of this &%Function is indefinite in 
much the way that &%ImmediateFutureFn and &%ImmediatePastFn are indefinite.  
Although this indefiniteness is undesirable from a theoretical standpoint, 
it does not have significant practical implications, since there is 
widespread intersubjective agreement about the most common cases."):-true.
part(['FrontFn', '?OBJ'], '?OBJ'):-true.
instance('BackFn', 'UnaryFunction'):-true.
instance('BackFn', 'AsymmetricRelation'):-true.
instance('BackFn', 'IrreflexiveRelation'):-true.
domain('BackFn', 1, 'Object'):-true.
range('BackFn', 'Object'):-true.
documentation('BackFn', "A &%Function that maps an &%Object to the side 
that is opposite the &%FrontFn of the &%Object.  Note that this is a 
partial function, since some &%Objects do not have sides, e.g. apples 
and spheres.  Note too that the &%range of this &%Function is indefinite in 
much the way that &%ImmediateFutureFn and &%ImmediatePastFn are indefinite.  
Although this indefiniteness is undesirable from a theoretical standpoint, 
it does not have significant practical implications, since there is 
widespread intersubjective agreement about the most common cases."):-true.
part(['BackFn', '?OBJ'], '?OBJ'):-true.
subrelation(part, located):-true.
instance(part, 'SpatialRelation'):-true.
instance(part, 'PartialOrderingRelation'):-true.
domain(part, 1, 'Object'):-true.
domain(part, 2, 'Object'):-true.
documentation(part, "The basic mereological relation.  All other 
mereological relations are defined in terms of this one.  
(&%part ?PART ?WHOLE) simply means that the &%Object ?PART is part
of the &%Object ?WHOLE.  Note that, since &%part is a 
&%ReflexiveRelation, every &%Object is a part of itself."):-true.
instance('properPart', 'AsymmetricRelation'):-true.
instance('properPart', 'TransitiveRelation'):-true.
subrelation('properPart', part):-true.
documentation('properPart', "(&%properPart ?OBJ1 ?OBJ2) means that 
?OBJ1 is a part of ?OBJ2 other than ?OBJ2 itself. This is a 
&%TransitiveRelation and &%AsymmetricRelation (hence an 
&%IrreflexiveRelation)."):-true.
<=>(['properPart', '?OBJ1', '?OBJ2'], [and, [part, '?OBJ1', '?OBJ2'], [not, [part, '?OBJ2', '?OBJ1']]]):-true.
subrelation(piece, part):-true.
domain(piece, 1, 'Substance'):-true.
domain(piece, 2, 'Substance'):-true.
documentation(piece, "A specialized common sense notion of part for 
arbitrary parts of &%Substances.  Quasi-synonyms are: chunk, hunk, bit, 
etc.  Compare &%component, the other subrelation of &%part."):-true.
=>([piece, '?SUBSTANCE1', '?SUBSTANCE2'], [forall, ['?CLASS'], [=>, [instance, '?SUBSTANCE1', '?CLASS'], [instance, '?SUBSTANCE2', '?CLASS']]]):-true.
subrelation(component, part):-true.
domain(component, 1, 'CorpuscularObject'):-true.
domain(component, 2, 'CorpuscularObject'):-true.
documentation(component, "A specialized common sense notion of part 
for heterogeneous parts of complexes. (&%component ?COMPONENT ?WHOLE) 
means that ?COMPONENT is a component of ?WHOLE. Examples of component 
include the doors and walls of a house, the states or provinces of a 
country, or the limbs and organs of an animal.  Compare &%piece, which 
is also a subrelation of &%part."):-true.
instance(material, 'BinaryPredicate'):-true.
'domainSubclass'(material, 1, 'Substance'):-true.
domain(material, 2, 'CorpuscularObject'):-true.
documentation(material, "(&%material ?SUBSTANCE ?OBJECT) means that 
?OBJECT is structurally made up in part of ?SUBSTANCE. This relation 
encompasses the concepts of 'composed of', 'made of', and 'formed of'.  
For example, plastic is a &%material of my computer monitor.  Compare 
&%part and its subrelations, viz &%component and &%piece."):-true.
subclass('Substance', 'SelfConnectedObject'):-true.
documentation('Substance', "An &%Object in which every part is similar to 
every other in every relevant respect.  More precisely, something is a 
&%Substance when it has only arbitrary pieces as parts - any parts have 
properties which are similar to those of the whole.  Note that a &%Substance 
may nonetheless have physical properties that vary.  For example, the 
temperature, chemical constitution, density, etc. may change from one part 
to another.  An example would be a body of water."):-true.
=>([and, [subclass, '?OBJECTTYPE', 'Substance'], [instance, '?OBJECT', '?OBJECTTYPE'], [part, '?PART', '?OBJECT']], [instance, '?PART', '?OBJECTTYPE']):-true.
subclass('PureSubstance', 'Substance'):-true.
partition('PureSubstance', 'CompoundSubstance', 'ElementalSubstance'):-true.
documentation('PureSubstance', "The &%Class of &%Substances with constant 
composition.  A &%PureSubstance can be either an element (&%ElementalSubstance) 
or a compound of elements (&%CompoundSubstance).  Examples: Table salt 
(sodium chloride, NaCl), sugar (sucrose, C_{12}H_{22}O_{11}), water (H_2O), 
iron (Fe), copper (Cu), and oxygen (O_2)."):-true.
subclass('ElementalSubstance', 'PureSubstance'):-true.
documentation('ElementalSubstance', "The &%Class of &%PureSubstances that 
cannot be separated into two or more &%Substances by ordinary chemical 
(or physical) means. This excludes nuclear reactions. &%ElementalSubstances 
are composed of only one kind of atom. Examples: Iron (Fe), copper (Cu), 
and oxygen (O_2).  &%ElementalSubstances are the simplest 
&%PureSubstances."):-true.
subclass('Metal', 'ElementalSubstance'):-true.
documentation('Metal', "A &%Metal is an &%ElementalSubstance that conducts heat 
and electricity, is shiny and reflects many colors of light, and can be hammered 
into sheets or drawn into wire.  About 80% of the known chemical elements 
(&%ElementalSubstances) are metals."):-true.
subclass('Atom', 'ElementalSubstance'):-true.
documentation('Atom', "An extremely small unit of matter that retains its 
identity in Chemical reactions.  It consists of an &%AtomicNucleus and 
&%Electrons surrounding the &%AtomicNucleus."):-true.
=>([instance, '?ATOM', 'Atom'], [exists, ['?PROTON', '?ELECTRON'], [and, [component, '?PROTON', '?ATOM'], [component, '?ELECTRON', '?ATOM'], [instance, '?PROTON', 'Proton'], [instance, '?ELECTRON', 'Electron']]]):-true.
=>([instance, '?ATOM', 'Atom'], [forall, ['?NUCLEUS1', '?NUCLEUS2'], [=>, [and, [component, '?NUCLEUS1', '?ATOM'], [component, '?NUCLEUS2', '?ATOM'], [instance, '?NUCLEUS1', 'AtomicNucleus'], [instance, '?NUCLEUS2', 'AtomicNucleus']], [equal, '?NUCLEUS1', '?NUCLEUS2']]]):-true.
subclass('SubatomicParticle', 'ElementalSubstance'):-true.
documentation('SubatomicParticle', "The class of &%ElementalSubstances that 
are smaller than &%Atoms and compose &%Atoms."):-true.
=>([instance, '?PARTICLE', 'SubatomicParticle'], [exists, ['?ATOM'], [and, [instance, '?ATOM', 'Atom'], [part, '?PARTICLE', '?ATOM']]]):-true.
subclass('AtomicNucleus', 'SubatomicParticle'):-true.
documentation('AtomicNucleus', "The core of the &%Atom.  It is composed of 
&%Protons and &%Neutrons."):-true.
=>([instance, '?NUCLEUS', 'AtomicNucleus'], [exists, ['?NEUTRON', '?PROTON'], [and, [component, '?NEUTRON', '?NUCLEUS'], [component, '?PROTON', '?NUCLEUS'], [instance, '?NEUTRON', 'Neutron'], [instance, '?PROTON', 'Proton']]]):-true.
subclass('Electron', 'SubatomicParticle'):-true.
documentation('Electron', "&%SubatomicParticles that surround the 
&%AtomicNucleus.  They have a negative charge."):-true.
subclass('Proton', 'SubatomicParticle'):-true.
documentation('Proton', "Components of the &%AtomicNucleus.  They have a 
positive charge."):-true.
subclass('Neutron', 'SubatomicParticle'):-true.
documentation('Neutron', "Components of the &%AtomicNucleus.  They have no 
charge."):-true.
subclass('CompoundSubstance', 'PureSubstance'):-true.
documentation('CompoundSubstance', "The &%Class of &%Substances that contain 
two or more elements (&%ElementalSubstances), in definite proportion by weight. 
The composition of a pure compound will be invariant, regardless of the method 
of preparation. Compounds are composed of more than one kind of atom (element). 
The term molecule is often used for the smallest unit of a compound that still 
retains all of the properties of the compound.  Examples: Table salt (sodium 
chloride, NaCl), sugar (sucrose, C_{12}H_{22}O_{11}), and water (H_2O). "):-true.
subclass('Mixture', 'Substance'):-true.
documentation('Mixture', "A &%Mixture is two or more &%PureSubstances, 
combined in varying proportions - each retaining its own specific properties. 
The components of a &%Mixture can be separated by physical means, i.e. without 
the making and breaking of chemical bonds. Examples: Air, table salt thoroughly 
dissolved in water, milk, wood, and concrete. "):-true.
=>([instance, '?MIXTURE', 'Mixture'], [exists, ['?PURE1', '?PURE2'], [and, [subclass, '?PURE1', 'PureSubstance'], [subclass, '?PURE2', 'PureSubstance'], [not, [equal, '?PURE1', '?PURE2']], [piece, '?PURE1', '?MIXTURE'], [piece, '?PURE2', '?MIXTURE']]]):-true.
subclass('Solution', 'Mixture'):-true.
documentation('Solution', "A liquid mixture. The most abundant component in 
a solution is called the solvent. Other components are called solutes. 
A solution, though homogeneous, may nonetheless have variable composition. 
Any amount of salt, up to a maximum limit, can be dissolved in a given 
amount of water."):-true.
subclass('CorpuscularObject', 'SelfConnectedObject'):-true.
disjoint('CorpuscularObject', 'Substance'):-true.
documentation('CorpuscularObject', "A &%SelfConnectedObject whose parts have 
properties that are not shared by the whole."):-true.
=>([instance, '?OBJ', 'CorpuscularObject'], [exists, ['?SUBSTANCE1', '?SUBSTANCE2'], [and, [subclass, '?SUBSTANCE1', 'Substance'], [subclass, '?SUBSTANCE2', 'Substance'], [material, '?SUBSTANCE1', '?OBJ'], [material, '?SUBSTANCE2', '?OBJ'], [not, [equal, '?SUBSTANCE1', '?SUBSTANCE2']]]]):-true.
subclass('Region', 'Object'):-true.
documentation('Region', "A topographic location.  &%Regions encompass 
surfaces of &%Objects, imaginary places, and &%GeographicAreas.  Note 
that a &%Region is the only kind of &%Object which can be located at 
itself.  Note too that &%Region is not a subclass of &%SelfConnectedObject, 
because some &%Regions, e.g. archipelagos, have &%parts which are not 
&%connected with one another."):-true.
=>([instance, '?REGION', 'Region'], [exists, ['?PHYS'], [located, '?PHYS', '?REGION']]):-true.
subclass('Collection', 'Object'):-true.
disjoint('Collection', 'SelfConnectedObject'):-true.
documentation('Collection', "Collections have &%members like &%Classes, but, 
unlike &%Classes, they have a position in space-time and &%members can be 
added and subtracted without thereby changing the identity of the 
&%Collection.  Some examples are toolkits, football teams, and flocks 
of sheep."):-true.
=>([instance, '?COLL', 'Collection'], [exists, ['?OBJ'], [member, '?OBJ', '?COLL']]):-true.
subrelation(member, part):-true.
instance(member, 'AsymmetricRelation'):-true.
instance(member, 'IntransitiveRelation'):-true.
domain(member, 1, 'SelfConnectedObject'):-true.
domain(member, 2, 'Collection'):-true.
'relatedInternalConcept'(member, instance):-true.
'relatedInternalConcept'(member, element):-true.
documentation(member, "A specialized common sense notion of part for 
uniform parts of &%Collections.  For example, each sheep in a flock of 
sheep would have the relationship of member to the flock."):-true.
instance('subCollection', 'BinaryPredicate'):-true.
instance('subCollection', 'TransitiveRelation'):-true.
instance('subCollection', 'IrreflexiveRelation'):-true.
domain('subCollection', 1, 'Collection'):-true.
domain('subCollection', 2, 'Collection'):-true.
documentation('subCollection', "(&%subCollection ?COLL1 ?COLL2) means that 
the &%Collection ?COLL1 is a proper part of the &%Collection ?COLL2."):-true.
subclass('ContentBearingObject', 'SelfConnectedObject'):-true.
'relatedInternalConcept'('ContentBearingObject', 'containsInformation'):-true.
documentation('ContentBearingObject', "Any &%SelfConnectedObject that expresses 
information."):-true.
subclass('SymbolicString', 'ContentBearingObject'):-true.
documentation('SymbolicString', "The &%Class of alphanumeric sequences."):-true.
subclass('Character', 'SymbolicString'):-true.
documentation('Character', "An element of an alphabet, a set of numerals, etc.  
Note that a &%Character may or may not be part of a &%Language.  &%Character 
is a subclass of &%SymbolicString, because every instance of &%Character is 
an alphanumeric sequence consisting of a single element."):-true.
=>([instance, '?STRING', 'SymbolicString'], [exists, ['?PART'], [and, [part, '?PART', '?STRING'], [instance, '?PART', 'Character']]]):-true.
instance('containsInformation', 'BinaryPredicate'):-true.
instance('containsInformation', 'AsymmetricRelation'):-true.
subrelation('containsInformation', represents):-true.
domain('containsInformation', 1, 'ContentBearingObject'):-true.
domain('containsInformation', 2, 'Proposition'):-true.
documentation('containsInformation', "A subrelation of &%represents.  This 
predicate relates a &%ContentBearingObject to the &%Proposition that is 
expressed by the &%ContentBearingObject. Examples include the relationships 
between a physical novel and its story and between a printed score and its 
musical content."):-true.
subclass('Icon', 'ContentBearingObject'):-true.
documentation('Icon', "This is the subclass of &%ContentBearingObjects 
which are not part of a &%Language and which have some sort of similarity 
with the &%Objects that they represent.  This &%Class would include symbolic 
roadway signs, 'icons' in a graphical computer operating system, etc."):-true.
subclass('MotionPicture', 'Text'):-true.
documentation('MotionPicture', "A &%ContentBearingObject which depicts motion 
(and which may have an audio or text component as well).  This &%Class covers 
films, videos, etc."):-true.
subclass('LinguisticExpression', 'ContentBearingObject'):-true.
disjoint('LinguisticExpression', 'Icon'):-true.
documentation('LinguisticExpression', "This is the subclass of 
&%ContentBearingObjects which are language-related.  Note that this &%Class 
encompasses both &%Language and the the elements of &%Languages, 
e.g. &%Words."):-true.
subclass('Language', 'LinguisticExpression'):-true.
'disjointDecomposition'('Language', 'AnimalLanguage', 'HumanLanguage', 'ComputerLanguage'):-true.
documentation('Language', "A system of signs for expressing thought.  The 
system can be either natural or artificial, i.e. something that emerges 
gradually as a cultural artifact or something that is intentionally created 
by a person or group of people."):-true.
subclass('AnimalLanguage', 'Language'):-true.
documentation('AnimalLanguage', "The &%subclass of &%Languages used by 
&%Animals other than &%Humans."):-true.
=>([and, [instance, '?LANG', 'AnimalLanguage'], [agent, '?PROC', '?AGENT'], [instrument, '?PROC', '?LANG']], [and, [instance, '?AGENT', 'Animal'], [not, [instance, '?AGENT', 'Human']]]):-true.
subclass('ArtificialLanguage', 'Language'):-true.
documentation('ArtificialLanguage', "The &%subclass of &%Languages that are 
designed by &%Humans."):-true.
subclass('ComputerLanguage', 'ArtificialLanguage'):-true.
documentation('ComputerLanguage', "The class of &%Languages designed for 
and interpreted by a computer."):-true.
=>([and, [instance, '?LANG', 'ComputerLanguage'], [agent, '?PROC', '?AGENT'], [instrument, '?PROC', '?LANG']], [instance, '?AGENT', 'Machine']):-true.
subclass('HumanLanguage', 'Language'):-true.
documentation('HumanLanguage', "The &%subclass of &%Languages used by 
&%Humans."):-true.
=>([and, [instance, '?LANG', 'HumanLanguage'], [agent, '?PROC', '?AGENT'], [instrument, '?PROC', '?LANG']], [instance, '?AGENT', 'Human']):-true.
subclass('ConstructedLanguage', 'HumanLanguage'):-true.
subclass('ConstructedLanguage', 'ArtificialLanguage'):-true.
documentation('ConstructedLanguage', "The &%subclass of &%HumanLanguages 
that did not originate diachronically, but were designed by a &%Human.  
This &%Class includes languages like Esperanto that were created to 
facilitate international communication."):-true.
=>([instance, '?LANG', 'ConstructedLanguage'], [exists, ['?PLAN'], [and, [instance, '?PLAN', 'Planning'], [result, '?PLAN', '?LANG']]]):-true.
subclass('NaturalLanguage', 'HumanLanguage'):-true.
disjoint('NaturalLanguage', 'ConstructedLanguage'):-true.
documentation('NaturalLanguage', "The &%subclass of &%HumanLanguages which 
are not designed and which evolve from generation to generation.  This 
&%Class includes all of the national languages, e.g. English, Spanish, 
Japanese, etc.  Note that this class includes dialects of natural 
languages."):-true.
subclass('Word', 'LinguisticExpression'):-true.
documentation('Word', "A term of a &%Language that represents a concept."):-true.
subclass('Formula', 'Sentence'):-true.
documentation('Formula', "A syntactically well-formed formula in the 
SUO-KIF knowledge representation language."):-true.
subclass('Agent', 'Object'):-true.
documentation('Agent', "Something or someone that can act on its own and 
produce changes in the world."):-true.
<=>([instance, '?AGENT', 'Agent'], [exists, ['?PROC'], [agent, '?PROC', '?AGENT']]):-true.
subclass('SentientAgent', 'Agent'):-true.
documentation('SentientAgent', "An &%Agent that has rights but may or may 
not have responsibilities and the ability to reason.  If the latter are 
present, then the &%Agent is also an instance of &%CognitiveAgent.  
Domesticated animals are an example of &%SentientAgents that are not 
also &%CognitiveAgents."):-true.
subclass('CognitiveAgent', 'SentientAgent'):-true.
documentation('CognitiveAgent', "A &%SentientAgent with responsibilities 
and the ability to reason, deliberate, make plans, etc.  This is 
essentially the legal/ethical notion of a person.  Note that, although 
&%Human is a subclass of &%CognitiveAgent, there may be instances of 
&%CognitiveAgent which are not also instances of &%Human.  For example, 
chimpanzees, gorillas, dolphins, whales, and some extraterrestrials 
(if they exist) may be &%CognitiveAgents."):-true.
subclass('Process', 'Physical'):-true.
documentation('Process', "Intuitively, the class of things that happen 
and have temporal parts or stages.  Examples include extended events 
like a football match or a race, actions like &%Searching and &%Reading, 
and biological processes. The formal definition is: anything that lasts 
for a time but is not an &%Object.  Note that a &%Process may have 
participants 'inside' it which are &%Objects, such as the players 
in a football match.  In a 4D ontology, a &%Process is something whose 
spatiotemporal extent is thought of as dividing into temporal stages 
roughly perpendicular to the time-axis."):-true.
subclass('DualObjectProcess', 'Process'):-true.
documentation('DualObjectProcess', "Any &%Process that requires two, 
nonidentical &%patients."):-true.
=>([instance, '?PROCESS', 'DualObjectProcess'], [exists, ['?OBJ1', '?OBJ2'], [and, [patient, '?PROCESS', '?OBJ1'], [patient, '?PROCESS', '?OBJ2'], [not, [equal, '?OBJ1', '?OBJ2']]]]):-true.
subclass('Abstract', 'Entity'):-true.
disjoint('Abstract', 'Physical'):-true.
documentation('Abstract', "Properties or qualities as distinguished from any 
particular embodiment of the properties/qualities in a physical medium.  
Instances of Abstract can be said to exist in the same sense as mathematical 
objects such as sets and relations, but they cannot exist at a particular 
place and time without some physical encoding or embodiment."):-true.
<=>([instance, '?ABS', 'Abstract'], [not, [exists, ['?POINT'], [or, [located, '?ABS', '?POINT'], [time, '?ABS', '?POINT']]]]):-true.
subclass('Quantity', 'Abstract'):-true.
documentation('Quantity', "Any specification of how many or how much of 
something there is.  Accordingly, there are two subclasses of &%Quantity:  
&%Number (how many) and &%PhysicalQuantity (how much)."):-true.
subclass('Attribute', 'Abstract'):-true.
disjoint('Attribute', 'Quantity'):-true.
partition('Attribute', 'InternalAttribute', 'RelationalAttribute'):-true.
documentation('Attribute', "Qualities which we cannot or choose not to 
reify into subclasses of &%Object."):-true.
instance(property, 'BinaryPredicate'):-true.
domain(property, 1, 'Entity'):-true.
domain(property, 2, 'Attribute'):-true.
documentation(property, "This &%Predicate holds between an instance of 
&%Entity and an instance of &%Attribute.  (property ?ENTITY ?ATTR) 
means that ?ENTITY has the &%Attribute ?ATTR."):-true.
instance(attribute, 'AsymmetricRelation'):-true.
instance(attribute, 'IrreflexiveRelation'):-true.
subrelation(attribute, property):-true.
domain(attribute, 1, 'Object'):-true.
domain(attribute, 2, 'Attribute'):-true.
documentation(attribute, "(attribute ?OBJECT ?PROPERTY) means that 
?PROPERTY is a &%Attribute of ?OBJECT.  For example, 
(attribute MyLittleRedWagon Red)."):-true.
instance(manner, 'AsymmetricRelation'):-true.
instance(manner, 'IrreflexiveRelation'):-true.
subrelation(manner, property):-true.
domain(manner, 1, 'Process'):-true.
domain(manner, 2, 'Attribute'):-true.
'disjointRelation'(manner, attribute):-true.
documentation(manner, "(&%manner ?PROCESS ?MANNER) means that the 
&%Process ?PROCESS is qualified by the &%Attribute ?MANNER.  The &%Attributes 
of &%Processes are usually denoted by adverbs and include things like the 
speed of the wind, the style of a dance, or the intensity of a sports 
competition."):-true.
instance('AbstractionFn', 'UnaryFunction'):-true.
domain('AbstractionFn', 1, 'Class'):-true.
range('AbstractionFn', 'Attribute'):-true.
documentation('AbstractionFn', "A &%UnaryFunction that maps a &%Class into 
the instance of &%Attribute that specifies the condition(s) for membership 
in the &%Class."):-true.
<=>([equal, ['AbstractionFn', '?CLASS'], '?ATTR'], [forall, ['?INST'], [<=>, [instance, '?INST', '?CLASS'], [attribute, '?INST', '?ATTR']]]):-true.
instance('ExtensionFn', 'UnaryFunction'):-true.
domain('ExtensionFn', 1, 'Attribute'):-true.
range('ExtensionFn', 'Class'):-true.
documentation('ExtensionFn', "A &%UnaryFunction that maps an &%Attribute 
into the &%Class whose condition for membership is the &%Attribute."):-true.
<=>([equal, ['ExtensionFn', '?ATTRIBUTE'], '?CLASS'], [equal, ['AbstractionFn', '?CLASS'], '?ATTRIBUTE']):-true.
subclass('InternalAttribute', 'Attribute'):-true.
documentation('InternalAttribute', "Any &%Attribute of an &%Entity that is an 
internal property of the &%Entity, e.g. its shape, its color, its fragility, 
etc."):-true.
subclass('RelationalAttribute', 'Attribute'):-true.
documentation('RelationalAttribute', "Any &%Attribute that an &%Entity has by 
virtue of a relationship that it bears to another &%Entity or set of &%Entities, 
e.g. &%SocialRoles and &%PositionalAttributes."):-true.
subclass('Number', 'Quantity'):-true.
documentation('Number', "A measure of how many things there are, or how
much there is, of a certain kind.  &%Numbers are subclassed into 
&%RealNumber, &%ComplexNumber, and &%ImaginaryNumber."):-true.
instance('lessThan', 'BinaryPredicate'):-true.
instance('lessThan', 'TransitiveRelation'):-true.
instance('lessThan', 'IrreflexiveRelation'):-true.
instance('lessThan', 'RelationExtendedToQuantities'):-true.
'trichotomizingOn'('lessThan', 'RealNumber'):-true.
domain('lessThan', 1, 'Quantity'):-true.
domain('lessThan', 2, 'Quantity'):-true.
documentation('lessThan', "(&%lessThan ?NUMBER1 ?NUMBER2) is true just 
in case the &%Quantity ?NUMBER1 is less than the &%Quantity ?NUMBER2."):-true.
instance('greaterThan', 'BinaryPredicate'):-true.
instance('greaterThan', 'TransitiveRelation'):-true.
instance('greaterThan', 'IrreflexiveRelation'):-true.
instance('greaterThan', 'RelationExtendedToQuantities'):-true.
'trichotomizingOn'('greaterThan', 'RealNumber'):-true.
domain('greaterThan', 1, 'Quantity'):-true.
domain('greaterThan', 2, 'Quantity'):-true.
inverse('greaterThan', 'lessThan'):-true.
documentation('greaterThan', "(&%greaterThan ?NUMBER1 ?NUMBER2) is true 
just in case the &%Quantity ?NUMBER1 is greater than the &%Quantity 
?NUMBER2."):-true.
instance('lessThanOrEqualTo', 'BinaryPredicate'):-true.
instance('lessThanOrEqualTo', 'PartialOrderingRelation'):-true.
instance('lessThanOrEqualTo', 'RelationExtendedToQuantities'):-true.
'trichotomizingOn'('lessThanOrEqualTo', 'RealNumber'):-true.
domain('lessThanOrEqualTo', 1, 'Quantity'):-true.
domain('lessThanOrEqualTo', 2, 'Quantity'):-true.
documentation('lessThanOrEqualTo', "(&%lessThanOrEqualTo ?NUMBER1 ?NUMBER2) 
is true just in case the &%Quantity ?NUMBER1 is less than or equal to 
the &%Quantity ?NUMBER2."):-true.
<=>(['lessThanOrEqualTo', '?NUMBER1', '?NUMBER2'], [or, [equal, '?NUMBER1', '?NUMBER2'], ['lessThan', '?NUMBER1', '?NUMBER2']]):-true.
instance('greaterThanOrEqualTo', 'BinaryPredicate'):-true.
instance('greaterThanOrEqualTo', 'PartialOrderingRelation'):-true.
instance('greaterThanOrEqualTo', 'RelationExtendedToQuantities'):-true.
'trichotomizingOn'('greaterThanOrEqualTo', 'RealNumber'):-true.
domain('greaterThanOrEqualTo', 1, 'Quantity'):-true.
domain('greaterThanOrEqualTo', 2, 'Quantity'):-true.
inverse('greaterThanOrEqualTo', 'lessThanOrEqualTo'):-true.
documentation('greaterThanOrEqualTo', "(&%greaterThanOrEqualTo ?NUMBER1 
?NUMBER2) is true just in case the &%Quantity ?NUMBER1 is greater 
than the &%Quantity ?NUMBER2."):-true.
<=>(['greaterThanOrEqualTo', '?NUMBER1', '?NUMBER2'], [or, [equal, '?NUMBER1', '?NUMBER2'], ['greaterThan', '?NUMBER1', '?NUMBER2']]):-true.
subclass('RealNumber', 'Number'):-true.
partition('RealNumber', 'NegativeRealNumber', 'NonnegativeRealNumber'):-true.
partition('RealNumber', 'RationalNumber', 'IrrationalNumber'):-true.
documentation('RealNumber', "Any &%Number that can be expressed as a 
(possibly infinite) decimal, i.e. any &%Number that has a position 
on the number line."):-true.
subclass('ImaginaryNumber', 'Number'):-true.
documentation('ImaginaryNumber', "Any &%Number that is the result of 
multiplying a &%RealNumber by the square root of -1."):-true.
=>([instance, '?NUMBER', 'ImaginaryNumber'], [exists, ['?REAL'], [and, [instance, '?REAL', 'RealNumber'], [equal, '?NUMBER', ['MultiplicationFn', '?REAL', ['SquareRootFn', -1]]]]]):-true.
subclass('RationalNumber', 'RealNumber'):-true.
documentation('RationalNumber', "Any &%RealNumber that is the product of 
dividing two &%Integers."):-true.
subclass('IrrationalNumber', 'RealNumber'):-true.
documentation('IrrationalNumber', "Any &%RealNumber that is not also a 
&%RationalNumber."):-true.
subclass('NonnegativeRealNumber', 'RealNumber'):-true.
documentation('NonnegativeRealNumber', "A &%RealNumber that is greater than 
or equal to zero."):-true.
=>([instance, '?NUMBER', 'NonnegativeRealNumber'], ['greaterThanOrEqualTo', '?NUMBER', 0]):-true.
subclass('PositiveRealNumber', 'NonnegativeRealNumber'):-true.
documentation('PositiveRealNumber', "A &%RealNumber that is greater than 
zero."):-true.
=>([instance, '?NUMBER', 'PositiveRealNumber'], ['greaterThan', '?NUMBER', 0]):-true.
subclass('NegativeRealNumber', 'RealNumber'):-true.
documentation('NegativeRealNumber', "A &%RealNumber that is less than 
zero."):-true.
=>([instance, '?NUMBER', 'NegativeRealNumber'], ['lessThan', '?NUMBER', 0]):-true.
subclass('Integer', 'RationalNumber'):-true.
partition('Integer', 'OddInteger', 'EvenInteger'):-true.
partition('Integer', 'NegativeInteger', 'NonnegativeInteger'):-true.
documentation('Integer', "A negative or nonnegative whole number."):-true.
subclass('EvenInteger', 'Integer'):-true.
documentation('EvenInteger', "An &%Integer that is evenly divisible 
by 2."):-true.
subclass('OddInteger', 'Integer'):-true.
documentation('OddInteger', "An &%Integer that is not evenly divisible 
by 2."):-true.
subclass('PrimeNumber', 'Integer'):-true.
documentation('PrimeNumber', "An &%Integer that is evenly divisible only 
by itself and 1."):-true.
subclass('NonnegativeInteger', 'Integer'):-true.
subclass('NonnegativeInteger', 'NonnegativeRealNumber'):-true.
documentation('NonnegativeInteger', "An &%Integer that is greater than 
or equal to zero."):-true.
subclass('NegativeInteger', 'Integer'):-true.
subclass('NegativeInteger', 'NegativeRealNumber'):-true.
documentation('NegativeInteger', "An &%Integer that is less than zero."):-true.
subclass('PositiveInteger', 'NonnegativeInteger'):-true.
subclass('PositiveInteger', 'PositiveRealNumber'):-true.
documentation('PositiveInteger', "An &%Integer that is greater than zero."):-true.
subclass('BinaryNumber', 'RealNumber'):-true.
documentation('BinaryNumber', "Elements from the number system with base 2.  
Every &%BinaryNumber is expressed as a sequence of the digits 1 and 0."):-true.
subclass('ComplexNumber', 'Number'):-true.
disjoint('ComplexNumber', 'RealNumber'):-true.
documentation('ComplexNumber', "A &%Number that has the form: x + yi, where x 
and y are &%RealNumbers and i is the square root of -1."):-true.
=>([instance, '?NUMBER', 'ComplexNumber'], [exists, ['?REAL1', '?REAL2'], [and, [instance, '?REAL1', 'RealNumber'], [instance, '?REAL2', 'RealNumber'], [equal, '?NUMBER', ['AdditionFn', '?REAL1', ['MultiplicationFn', '?REAL2', ['SquareRootFn', -1]]]]]]):-true.
subclass('PhysicalQuantity', 'Quantity'):-true.
partition('PhysicalQuantity', 'ConstantQuantity', 'FunctionQuantity'):-true.
documentation('PhysicalQuantity', "&%A PhysicalQuantity is a measure of 
some quantifiable aspect of the modeled world, such as 'the earth's 
diameter' (a constant length) and 'the stress in a loaded deformable 
solid' (a measure of stress, which is a function of three spatial 
coordinates).  All &%PhysicalQuantities are either &%ConstantQuantities 
or &%FunctionQuantities.  Instances of &%ConstantQuantity are dependent 
on a &%UnitOfMeasure, while instances of &%FunctionQuantity are 
&%Functions that map instances of &%ConstantQuantity to other instances 
of &%ConstantQuantity (e.g., &%TimeDependentQuantities are 
&%FunctionQuantities).  Although the name and definition of 
&%PhysicalQuantity is borrowed from physics, &%PhysicalQuantities need 
not be material.  Aside from the dimensions of length, time, velocity, 
etc., nonphysical dimensions such as currency are also possible.  
Accordingly, amounts of money would be instances of &%PhysicalQuantity.  
&%PhysicalQuantities are distinguished from &%Numbers by the fact that 
the former are associated with a dimension of measurement."):-true.
subclass('ConstantQuantity', 'PhysicalQuantity'):-true.
documentation('ConstantQuantity', "A &%ConstantQuantity is a 
&%PhysicalQuantity which has a constant value, e.g. 3 meters and 5 hours.  
The magnitude (see &%MagnitudeFn) of every &%ConstantQuantity is a 
&%RealNumber.  &%ConstantQuantities are distinguished from 
&%FunctionQuantities, which map &%ConstantQuantities to other 
&%ConstantQuantities.  All &%ConstantQuantites are expressed with the 
&%BinaryFunction &%MeasureFn, which takes a &%Number and a &%UnitOfMeasure 
as arguments.  For example, 3 &%Meters can be expressed as (&%MeasureFn 3 
&%Meter).  &%ConstantQuantities form a partial order (see 
&%PartialOrderingRelation) with the &%lessThan relation, since &%lessThan 
is a &%RelationExtendedToQuantities and &%lessThan is defined over the 
&%RealNumbers.  The &%lessThan relation is not a total order (see 
&%TotalOrderingRelation) over the class &%ConstantQuantity since elements 
of some subclasses of &%ConstantQuantity (such as length quantities) 
are incomparable to elements of other subclasses of &%ConstantQuantity 
(such as mass quantities)."):-true.
subclass('TimeMeasure', 'ConstantQuantity'):-true.
documentation('TimeMeasure', "The class of temporal durations (instances 
of &%TimeDuration) and positions of &%TimePoints and &%TimeIntervals along 
the universal timeline (instances of &%TimePosition)."):-true.
subclass('TimeDuration', 'TimeMeasure'):-true.
documentation('TimeDuration', "Any measure of length of time, 
with or without respect to the universal timeline."):-true.
subclass('TimePosition', 'TimeMeasure'):-true.
partition('TimePosition', 'TimeInterval', 'TimePoint'):-true.
documentation('TimePosition', "Any &%TimePoint or &%TimeInterval 
along the universal timeline from &%NegativeInfinity to 
&%PositiveInfinity."):-true.
subclass('TimeInterval', 'TimePosition'):-true.
documentation('TimeInterval', "An interval of time.  Note that a 
&%TimeInterval has both an extent and a location on the universal 
timeline.  Note too that a &%TimeInterval has no gaps, i.e. this 
class contains only convex time intervals."):-true.
subclass('TimePoint', 'TimePosition'):-true.
documentation('TimePoint', "An extensionless point on the universal timeline. 
The &%TimePoints at which &%Processes occur can be known with various 
degrees of precision and approximation, but conceptually &%TimePoints are 
point-like and not interval-like.  That is, it doesn't make sense to talk 
about what happens during a &%TimePoint, or how long the &%TimePoint lasts."):-true.
subclass('FunctionQuantity', 'PhysicalQuantity'):-true.
subclass('FunctionQuantity', 'Function'):-true.
documentation('FunctionQuantity', "A &%FunctionQuantity is a &%Function that 
maps from one or more instances of &%ConstantQuantity to another instance 
of &%ConstantQuantity.  For example, the velocity of a particle would be 
represented by a &%FunctionQuantity mapping values of time (which are 
&%ConstantQuantities) to values of distance (also &%ConstantQuantities).  
Note that all instances of &%FunctionQuantity are &%Functions with a fixed 
arity.  Note too that all elements of the range of a &%FunctionQuantity 
have the same physical dimension as the &%FunctionQuantity itself."):-true.
subclass('UnaryConstantFunctionQuantity', 'FunctionQuantity'):-true.
subclass('UnaryConstantFunctionQuantity', 'UnaryFunction'):-true.
documentation('UnaryConstantFunctionQuantity', "The class of &%UnaryFunctions 
that map from the &%Class &%ConstantQuantity to the &%Class 
&%ConstantQuantity."):-true.
=>([instance, '?FUNCTION', 'UnaryConstantFunctionQuantity'], [and, [domain, '?FUNCTION', 1, 'ConstantQuantity'], [range, '?FUNCTION', 'ConstantQuantity']]):-true.
subclass('TimeDependentQuantity', 'UnaryConstantFunctionQuantity'):-true.
subclass('TimeDependentQuantity', 'ContinuousFunction'):-true.
documentation('TimeDependentQuantity', "A &%UnaryConstantFunction of continuous 
time.  All instances of this &%Class map a time quantity into another 
&%ConstantQuantity such as temperature.  For example, 'the temperature at 
the top of the Empire State Building' is a &%TimeDependentQuantity since 
its value depends on the time."):-true.
=>([instance, '?FUNCTION', 'TimeDependentQuantity'], [domain, '?FUNCTION', 1, 'TimeMeasure']):-true.
subclass('SetOrClass', 'Abstract'):-true.
partition('SetOrClass', 'Set', 'Class'):-true.
documentation('SetOrClass', "The &%SetOrClass of &%Sets and &%Classes, i.e. any instance 
of &%Abstract that has &%elements or &%instances."):-true.
subclass('Class', 'SetOrClass'):-true.
documentation('Class', "&%Classes differ from &%Sets in two important respects.  
First, &%Classes are not assumed to be extensional.  That is, distinct 
&%Classes might well have exactly the same instances.  Second, &%Classes typically 
have an associated `condition' that determines the instances of the &%Class.  So, 
for example, the condition `human' determines the &%Class of &%Humans.  Note that 
some &%Classes might satisfy their own condition (e.g., the &%Class of &%Abstract 
things is &%Abstract) and hence be instances of themselves."):-true.
subclass('Set', 'SetOrClass'):-true.
documentation('Set', "A &%SetOrClass that satisfies extensionality as well as
other constraints specified by some choice of set theory.  &%Sets differ 
from &%Classes in two important respects.  First, &%Sets are extensional - 
two &%Sets with the same &%elements are identical.  Second, a &%Set can be 
an arbitrary stock of objects.  That is, there is no requirement that &%Sets 
have an associated condition that determines their membership."):-true.
subclass('Relation', 'Abstract'):-true.
'disjointDecomposition'('Relation', 'BinaryRelation', 'TernaryRelation', 'QuaternaryRelation', 'QuintaryRelation', 'VariableArityRelation'):-true.
partition('Relation', 'Predicate', 'Function', 'List'):-true.
partition('Relation', 'TotalValuedRelation', 'PartialValuedRelation'):-true.
documentation('Relation', "The &%Class of relations.  There are three kinds 
of &%Relation:  &%Predicate, &%Function, and &%List.  &%Predicates and 
&%Functions both denote sets of ordered n-tuples.  The difference between 
these two &%Classes is that &%Predicates cover formula-forming operators, while 
&%Functions cover term-forming operators.  A &%List, on the other hand, is a 
particular ordered n-tuple."):-true.
subclass('SingleValuedRelation', 'Relation'):-true.
instance('SingleValuedRelation', 'InheritableRelation'):-true.
documentation('SingleValuedRelation', "A &%Relation is a &%SingleValuedRelation 
just in case an assignment of values to every argument position except the last 
one determines at most one assignment for the last argument position.  Note 
that not all &%SingleValuedRelations are total functions.  To declare that a 
&%Relation is a total function, use both &%SingleValuedFunction and 
&%TotalValuedFunction."):-true.
<=>([instance, '?REL', 'SingleValuedRelation'], [forall, ['@ROW', '?ITEM1', '?ITEM2'], [=>, [and, [holds, '?REL', '@ROW', '?ITEM1'], [holds, '?REL', '@ROW', '?ITEM2']], [equal, '?ITEM1', '?ITEM2']]]):-true.
subclass('TotalValuedRelation', 'Relation'):-true.
documentation('TotalValuedRelation', "A &%Relation is a &%TotalValuedRelation 
just in case there exists an assignment for the last argument position of the 
&%Relation given any assignment of values to every argument position except 
the last one.  Note that declaring a &%Relation to be both a &%TotalValuedRelation 
and a &%SingleValuedRelation means that it is a total function."):-true.
<=>([instance, '?REL', 'TotalValuedRelation'], [forall, ['?VALENCE', '@ROW'], [=>, [and, [valence, '?REL', '?VALENCE'], [forall, ['?NUMBER', '?CLASS'], [=>, [and, ['lessThan', '?NUMBER', '?VALENCE'], [domain, '?REL', '?NUMBER', '?CLASS']], [instance, ['ListOrderFn', ['ListFn', '@ROW'], '?NUMBER'], '?CLASS']]]], [exists, ['?ITEM'], [holds, '?REL', '@ROW', '?ITEM']]]]):-true.
subclass('PartialValuedRelation', 'Relation'):-true.
documentation('PartialValuedRelation', "A &%Relation is a &%PartialValuedRelation 
just in case it is not a &%TotalValuedRelation, i.e. just in case assigning values 
to every argument position except the last one does not necessarily mean that there 
is a value assignment for the last argument position.  Note that, if a &%Relation 
is both a &%PartialValuedRelation and a &%SingleValuedRelation, then it is a partial 
function."):-true.
subclass('BinaryRelation', 'Relation'):-true.
instance('BinaryRelation', 'InheritableRelation'):-true.
documentation('BinaryRelation', "&%BinaryRelations are relations that are 
true only of pairs of things.  &%BinaryRelations are represented as slots 
in frame systems."):-true.
=>([instance, '?REL', 'BinaryRelation'], [not, [exists, ['?ITEM1', '?ITEM2', '?ITEM3', '@ROW'], [holds, '?REL', '?ITEM1', '?ITEM2', '?ITEM3', '@ROW']]]):-true.
subclass('ReflexiveRelation', 'BinaryRelation'):-true.
documentation('ReflexiveRelation', "&%Relation ?REL is reflexive if 
(?REL ?INST ?INST) for all ?INST."):-true.
=>([instance, '?REL', 'ReflexiveRelation'], [forall, ['?INST'], [holds, '?REL', '?INST', '?INST']]):-true.
subclass('IrreflexiveRelation', 'BinaryRelation'):-true.
documentation('IrreflexiveRelation', "&%Relation ?REL is irreflexive 
if (?REL ?INST ?INST) holds for no value of ?INST."):-true.
=>([instance, '?REL', 'IrreflexiveRelation'], [forall, ['?INST'], [not, [holds, '?REL', '?INST', '?INST']]]):-true.
subclass('SymmetricRelation', 'BinaryRelation'):-true.
documentation('SymmetricRelation', "A &%BinaryRelation ?REL is
symmetric just in case (?REL ?INST1 ?INST2) imples (?REL 
?INST2 ?INST1), for all ?INST1 and ?INST2."):-true.
=>([instance, '?REL', 'SymmetricRelation'], [forall, ['?INST1', '?INST2'], [=>, [holds, '?REL', '?INST1', '?INST2'], [holds, '?REL', '?INST2', '?INST1']]]):-true.
subclass('AsymmetricRelation', 'IrreflexiveRelation'):-true.
subclass('AsymmetricRelation', 'AntisymmetricRelation'):-true.
documentation('AsymmetricRelation', "A &%BinaryRelation is asymmetric only 
if it is both an &%AntisymmetricRelation and an &%IrreflexiveRelation."):-true.
=>([and, [instance, '?PRED', 'BinaryPredicate'], [domain, '?PRED', 1, '?CLASS1'], [domain, '?PRED', 2, '?CLASS2'], [disjoint, '?CLASS1', '?CLASS2']], [instance, '?PRED', 'AsymmetricRelation']):-true.
=>([and, [instance, '?FUNCTION', 'UnaryFunction'], [domain, '?FUNCTION', 1, '?CLASS1'], [range, '?FUNCTION', '?CLASS2'], [disjoint, '?CLASS1', '?CLASS2']], [instance, '?FUNCTION', 'AsymmetricRelation']):-true.
subclass('AntisymmetricRelation', 'BinaryRelation'):-true.
documentation('AntisymmetricRelation', "&%BinaryRelation ?REL is an 
&%AntisymmetricRelation if for distinct ?INST1 and ?INST2, (?REL ?INST1 
?INST2) implies not (?REL ?INST2 ?INST1).  In other words, for all ?INST1 
and ?INST2, (?REL ?INST1 ?INST2) and (?REL ?INST2 ?INST1) imply that ?INST1 
and ?INST2 are identical.  Note that it is possible for an 
&%AntisymmetricRelation to be a &%ReflexiveRelation."):-true.
=>([instance, '?REL', 'AntisymmetricRelation'], [forall, ['?INST1', '?INST2'], [=>, [and, [holds, '?REL', '?INST1', '?INST2'], [holds, '?REL', '?INST2', '?INST1']], [equal, '?INST1', '?INST2']]]):-true.
subclass('TrichotomizingRelation', 'BinaryRelation'):-true.
documentation('TrichotomizingRelation', "A &%BinaryRelation ?REL is a 
&%TrichotomizingRelation just in case all ordered pairs consisting of 
distinct individuals are elements of ?REL."):-true.
=>([instance, '?REL', 'TrichotomizingRelation'], [forall, ['?INST1', '?INST2'], [or, [holds, '?REL', '?INST1', '?INST2'], [equal, '?INST1', '?INST2'], [holds, '?REL', '?INST2', '?INST1']]]):-true.
subclass('TransitiveRelation', 'BinaryRelation'):-true.
documentation('TransitiveRelation', "A &%BinaryRelation ?REL is transitive 
if (?REL ?INST1 ?INST2) and (?REL ?INST2 ?INST3) imply (?REL ?INST1 ?INST3), 
for all ?INST1, ?INST2, and ?INST3."):-true.
=>([instance, '?REL', 'TransitiveRelation'], [forall, ['?INST1', '?INST2', '?INST3'], [=>, [and, [holds, '?REL', '?INST1', '?INST2'], [holds, '?REL', '?INST2', '?INST3']], [holds, '?REL', '?INST1', '?INST3']]]):-true.
subclass('IntransitiveRelation', 'BinaryRelation'):-true.
documentation('IntransitiveRelation', "A &%BinaryRelation ?REL is 
intransitive only if (?REL ?INST1 ?INST2) and (?REL ?INST2 ?INST3) imply not 
(?REL ?INST1 ?INST3), for all ?INST1, ?INST2, and ?INST3."):-true.
=>([instance, '?REL', 'IntransitiveRelation'], [forall, ['?INST1', '?INST2', '?INST3'], [=>, [and, [holds, '?REL', '?INST1', '?INST2'], [holds, '?REL', '?INST2', '?INST3']], [not, [holds, '?REL', '?INST1', '?INST3']]]]):-true.
subclass('PartialOrderingRelation', 'TransitiveRelation'):-true.
subclass('PartialOrderingRelation', 'AntisymmetricRelation'):-true.
subclass('PartialOrderingRelation', 'ReflexiveRelation'):-true.
documentation('PartialOrderingRelation', "A &%BinaryRelation is a partial 
ordering if it is a &%ReflexiveRelation, an &%AntisymmetricRelation, and 
a &%TransitiveRelation."):-true.
subclass('TotalOrderingRelation', 'PartialOrderingRelation'):-true.
subclass('TotalOrderingRelation', 'TrichotomizingRelation'):-true.
documentation('TotalOrderingRelation', "A &%BinaryRelation is a 
&%TotalOrderingRelation if it is a &%PartialOrderingRelation 
and a &%TrichotomizingRelation."):-true.
=>([instance, '?REL', 'TotalOrderingRelation'], [forall, ['?INST1', '?INST2'], [or, [holds, '?REL', '?INST1', '?INST2'], [holds, '?REL', '?INST2', '?INST1']]]):-true.
subclass('EquivalenceRelation', 'TransitiveRelation'):-true.
subclass('EquivalenceRelation', 'SymmetricRelation'):-true.
subclass('EquivalenceRelation', 'ReflexiveRelation'):-true.
documentation('EquivalenceRelation', "A &%BinaryRelation is an equivalence 
relation if it is a &%ReflexiveRelation, a &%SymmetricRelation, and a 
&%TransitiveRelation."):-true.
subclass('CaseRole', 'BinaryPredicate'):-true.
instance('CaseRole', 'InheritableRelation'):-true.
subclass('CaseRole', 'AsymmetricRelation'):-true.
documentation('CaseRole', "The &%Class of &%Predicates relating the 
spatially distinguished parts of a &%Process. &%CaseRoles include, for 
example, the &%agent, &%patient or &%destination of an action, the flammable 
substance in a burning process, or the water that falls in rain."):-true.
instance(agent, 'CaseRole'):-true.
domain(agent, 1, 'Process'):-true.
domain(agent, 2, 'Agent'):-true.
documentation(agent, "(&%agent ?PROCESS ?AGENT) means that ?AGENT is 
an active determinant, either animate or inanimate, of the &%Process 
?PROCESS, with or without voluntary intention.  For example, water is 
the &%agent of erosion in the following proposition:  the water 
eroded the coastline.  For another example, Eve is an &%agent in the 
following proposition: Eve bit an apple."):-true.
=>([instance, '?PROCESS', 'Process'], [exists, ['?CAUSE'], [agent, '?PROCESS', '?CAUSE']]):-true.
instance(destination, 'CaseRole'):-true.
domain(destination, 1, 'Process'):-true.
domain(destination, 2, 'Entity'):-true.
documentation(destination, "(destination ?PROCESS ?GOAL) means that 
?GOAL is the target or goal of the Process ?PROCESS.  For example, 
Danbury would be the destination in the following proposition:  Bob went 
to Danbury.  Note that this is a very general &%CaseRole and, in 
particular, that it covers the concepts of 'recipient' and 'beneficiary'.  
Thus, John would be the &%destination in the following proposition:  
Tom gave a book to John."):-true.
instance(experiencer, 'CaseRole'):-true.
domain(experiencer, 1, 'Process'):-true.
domain(experiencer, 2, 'Agent'):-true.
documentation(experiencer, "(&%experiencer ?PROCESS ?AGENT) means 
that ?AGENT experiences the &%Process ?PROCESS.  For example, Yojo 
is the &%experiencer of seeing in the following proposition:  Yojo 
sees the fish.  Note that &%experiencer, unlike &%agent, does 
not entail a causal relation between its arguments."):-true.
subrelation(instrument, patient):-true.
domain(instrument, 1, 'Process'):-true.
domain(instrument, 2, 'Object'):-true.
documentation(instrument, "(instrument ?EVENT ?TOOL) means that ?TOOL 
is used by an agent in bringing about ?EVENT and that ?TOOL is not 
changed by ?EVENT.  For example, the key is an &%instrument in the 
following proposition: The key opened the door.  Note that &%instrument 
and &%resource cannot be satisfied by the same ordered pair."):-true.
instance(origin, 'CaseRole'):-true.
domain(origin, 1, 'Process'):-true.
domain(origin, 2, 'Object'):-true.
documentation(origin, "(&%origin ?PROCESS ?SOURCE) means that ?SOURCE 
indicates where the ?Process began.  Note that this relation implies 
that ?SOURCE is present at the beginning of the process, but need not 
participate throughout the process.  For example, the submarine is the 
&%origin in the following proposition: the missile was launched from a 
submarine."):-true.
instance(patient, 'CaseRole'):-true.
domain(patient, 1, 'Process'):-true.
domain(patient, 2, 'Entity'):-true.
documentation(patient, "(&%patient ?PROCESS ?ENTITY) means that ?ENTITY 
is a participant in ?PROCESS that may be moved, said, experienced, etc.  
For example, the direct objects in the sentences 'The cat swallowed the 
canary' and 'Billy likes the beer' would be examples of &%patients.  Note 
that the &%patient of a &%Process may or may not undergo structural 
change as a result of the &%Process.  The &%CaseRole of &%patient is used 
when one wants to specify as broadly as possible the object of a 
&%Process."):-true.
subrelation(resource, patient):-true.
domain(resource, 1, 'Process'):-true.
domain(resource, 2, 'Object'):-true.
'disjointRelation'(resource, instrument):-true.
'disjointRelation'(resource, result):-true.
documentation(resource, "(&%resource ?PROCESS ?RESOURCE) means that 
?RESOURCE is present at the beginning of ?PROCESS, is used by ?PROCESS, 
and as a consequence is changed by ?PROCESS.  For example, soap is a 
&%resource in the following proposition:  the gun was carved out of soap.  
Note that &%resource differs from &%instrument, another subrelation of 
&%patient, in that its internal or physical properties are altered in 
some way by the &%Process."):-true.
subrelation(result, patient):-true.
domain(result, 1, 'Process'):-true.
domain(result, 2, 'Object'):-true.
documentation(result, "(result ?ACTION ?OUTPUT) means that ?OUTPUT is 
a product of ?ACTION.  For example, house is a &%result in the 
following proposition: Eric built a house."):-true.
instance('InheritableRelation', 'Class'):-true.
documentation('InheritableRelation', "This is a &%Class of &%Classes.  Each 
&%instance of &%InheritableRelation is a &%subclass of &%Relation whose 
properties can be inherited downward in the class hierarchy via the 
&%subrelation &%Predicate."):-true.
subclass('ProbabilityRelation', 'Relation'):-true.
instance('ProbabilityRelation', 'InheritableRelation'):-true.
documentation('ProbabilityRelation', "The &%Class of &%Relations that 
permit assessment of the probability of an event or situation."):-true.
instance('ProbabilityFn', 'ProbabilityRelation'):-true.
instance('ProbabilityFn', 'UnaryFunction'):-true.
domain('ProbabilityFn', 1, 'Formula'):-true.
range('ProbabilityFn', 'RealNumber'):-true.
instance('ProbabilityFn', 'AsymmetricRelation'):-true.
documentation('ProbabilityFn', "One of the basic &%ProbabilityRelations, 
&%ProbabilityFn is used to state the a priori probability of a state of 
affairs.  (&%ProbabilityFn ?FORMULA) denotes the a priori probability 
of ?FORMULA."):-true.
instance('conditionalProbability', 'ProbabilityRelation'):-true.
instance('conditionalProbability', 'TernaryPredicate'):-true.
domain('conditionalProbability', 1, 'Formula'):-true.
domain('conditionalProbability', 2, 'Formula'):-true.
domain('conditionalProbability', 3, 'RealNumber'):-true.
documentation('conditionalProbability', "One of the basic &%ProbabilityRelations.  
&%conditionalProbability is used to state the numeric value of a conditional 
probability.  (&%conditionalProbability ?FORMULA1 ?FORMULA2 ?NUMBER) means 
that the probability of ?FORMULA2 being true given that ?FORMULA1 is true is 
?NUMBER."):-true.
instance('increasesLikelihood', 'ProbabilityRelation'):-true.
instance('increasesLikelihood', 'BinaryPredicate'):-true.
instance('increasesLikelihood', 'IrreflexiveRelation'):-true.
domain('increasesLikelihood', 1, 'Formula'):-true.
domain('increasesLikelihood', 2, 'Formula'):-true.
'disjointRelation'('increasesLikelihood', 'decreasesLikelihood'):-true.
'disjointRelation'('increasesLikelihood', 'independentProbability'):-true.
documentation('increasesLikelihood', "One of the basic &%ProbabilityRelations.  
(&%increasesLikelihood ?FORMULA1 ?FORMULA2) means that ?FORMULA2 is more 
likely to be true if ?FORMULA1 is true."):-true.
=>([and, ['increasesLikelihood', '?FORMULA1', '?FORMULA2'], [equal, ['ProbabilityFn', '?FORMULA2'], '?NUMBER1'], ['conditionalProbability', '?FORMULA1', '?FORMULA2', '?NUMBER2']], ['greaterThan', '?NUMBER2', '?NUMBER1']):-true.
instance('decreasesLikelihood', 'ProbabilityRelation'):-true.
instance('decreasesLikelihood', 'BinaryPredicate'):-true.
instance('decreasesLikelihood', 'IrreflexiveRelation'):-true.
domain('decreasesLikelihood', 1, 'Formula'):-true.
domain('decreasesLikelihood', 2, 'Formula'):-true.
'disjointRelation'('decreasesLikelihood', 'independentProbability'):-true.
documentation('decreasesLikelihood', "One of the basic &%ProbabilityRelations.  
(&%decreasesLikelihood ?FORMULA1 ?FORMULA2) means that ?FORMULA2 is less 
likely to be true if ?FORMULA1 is true."):-true.
=>([and, ['decreasesLikelihood', '?FORMULA1', '?FORMULA2'], [equal, ['ProbabilityFn', '?FORMULA2'], '?NUMBER1'], ['conditionalProbability', '?FORMULA1', '?FORMULA2', '?NUMBER2']], ['lessThan', '?NUMBER2', '?NUMBER1']):-true.
instance('independentProbability', 'ProbabilityRelation'):-true.
instance('independentProbability', 'BinaryPredicate'):-true.
instance('independentProbability', 'SymmetricRelation'):-true.
domain('independentProbability', 1, 'Formula'):-true.
domain('independentProbability', 2, 'Formula'):-true.
documentation('independentProbability', "One of the basic &%ProbabilityRelations.  
(&%independentProbability ?FORMULA1 ?FORMULA2) means that the probabilities of 
?FORMULA1 and ?FORMULA2 being true are independent."):-true.
=>([and, ['independentProbability', '?FORMULA1', '?FORMULA2'], [equal, ['ProbabilityFn', '?FORMULA2'], '?NUMBER1'], ['conditionalProbability', '?FORMULA1', '?FORMULA2', '?NUMBER2']], [equal, '?NUMBER2', '?NUMBER1']):-true.
or(['increasesLikelihood', '?FORMULA1', '?FORMULA2'], ['decreasesLikelihood', '?FORMULA1', '?FORMULA2'], ['independentProbability', '?FORMULA1', '?FORMULA2']):-true.
subclass('SpatialRelation', 'Relation'):-true.
instance('SpatialRelation', 'InheritableRelation'):-true.
documentation('SpatialRelation', "The &%Class of &%Relations that are 
spatial in a wide sense.  This &%Class includes mereological relations 
and topological relations."):-true.
subclass('TemporalRelation', 'Relation'):-true.
instance('TemporalRelation', 'InheritableRelation'):-true.
documentation('TemporalRelation', "The &%Class of temporal &%Relations.  
This &%Class includes notions of (temporal) topology of intervals, 
(temporal) schemata, and (temporal) extension."):-true.
instance('IntentionalRelation', 'InheritableRelation'):-true.
documentation('IntentionalRelation', "The &%Class of &%Relations between 
an &%Agent and one or more &%Entities, where the &%Relation requires that 
the &%Agent have awareness of the &%Entity."):-true.
=>([and, [instance, '?REL', 'IntentionalRelation'], [holds, '?REL', '?AGENT', '@ROW'], ['inList', '?OBJ', ['ListFn', '@ROW']]], ['inScopeOfInterest', '?AGENT', '?OBJ']):-true.
instance(prefers, 'TernaryPredicate'):-true.
instance(prefers, 'IntentionalRelation'):-true.
domain(prefers, 1, 'CognitiveAgent'):-true.
domain(prefers, 2, 'Formula'):-true.
domain(prefers, 3, 'Formula'):-true.
documentation(prefers, "(&%prefers ?AGENT ?FORMULA1 ?FORMULA2) means that 
&%CognitiveAgent ?AGENT prefers the state of affairs expressed by ?FORMULA1
over the state of affairs expressed by ?FORMULA2 all things being equal."):-true.
=>([prefers, '?AGENT', '?FORMULA1', '?FORMULA2'], [=>, [true, '?FORMULA1', 'True'], [true, '?FORMULA2', 'False']]):-true.
subclass('PropositionalAttitude', 'IntentionalRelation'):-true.
subclass('PropositionalAttitude', 'AsymmetricRelation'):-true.
instance('PropositionalAttitude', 'InheritableRelation'):-true.
documentation('PropositionalAttitude', "The &%Class of 
&%IntentionalRelations where the &%Agent has awareness of a 
&%Proposition."):-true.
subclass('ObjectAttitude', 'IntentionalRelation'):-true.
subclass('ObjectAttitude', 'AsymmetricRelation'):-true.
instance('ObjectAttitude', 'InheritableRelation'):-true.
disjoint('ObjectAttitude', 'PropositionalAttitude'):-true.
documentation('ObjectAttitude', "The &%Class of &%IntentionalRelations 
where the &%Agent has awareness of an instance of &%Physical."):-true.
instance('inScopeOfInterest', 'BinaryPredicate'):-true.
instance('inScopeOfInterest', 'IntentionalRelation'):-true.
instance('inScopeOfInterest', 'AsymmetricRelation'):-true.
domain('inScopeOfInterest', 1, 'CognitiveAgent'):-true.
domain('inScopeOfInterest', 2, 'Entity'):-true.
documentation('inScopeOfInterest', "A very general &%Predicate.  
(&%inScopeOfInterest ?AGENT ?ENTITY) means that ?ENTITY is within the 
scope of interest of ?AGENT.  Note that the interest indicated can be 
either positive or negative, i.e. the ?AGENT can have an interest in 
avoiding or promoting ?ENTITY."):-true.
instance(notices, 'ObjectAttitude'):-true.
subrelation(notices, 'InScopeOfInterest'):-true.
domain(notices, 1, 'CognitiveAgent'):-true.
domain(notices, 2, 'Object'):-true.
documentation(notices, "(&%notices ?AGENT ?OBJECT) means that ?AGENT is 
paying attention to ?OBJECT, that ?OBJECT is currently the object of 
?AGENT's conscious awareness."):-true.
instance(needs, 'ObjectAttitude'):-true.
subrelation(needs, 'inScopeOfInterest'):-true.
domain(needs, 1, 'CognitiveAgent'):-true.
domain(needs, 2, 'Object'):-true.
documentation(needs, "(&%needs ?AGENT ?OBJECT) means that ?OBJECT is 
physically required for the continued existence of ?AGENT."):-true.
=>([needs, '?AGENT', '?OBJECT'], [wants, '?AGENT', '?OBJECT']):-true.
instance(wants, 'ObjectAttitude'):-true.
subrelation(wants, 'inScopeOfInterest'):-true.
'relatedInternalConcept'(wants, desires):-true.
domain(wants, 1, 'CognitiveAgent'):-true.
domain(wants, 2, 'Object'):-true.
documentation(wants, "(&%wants ?AGENT ?OBJECT) means that ?OBJECT is desired by ?AGENT, 
i.e. ?AGENT believes that ?OBJECT will satisfy one of its goals.  Note that there is 
no implication that what is wanted by an agent is not already possessed by the agent."):-true.
=>([wants, '?AGENT', '?OBJ'], [exists, ['?PURP'], ['hasPurposeForAgent', '?OBJ', '?PURP', '?AGENT']]):-true.
=>([wants, '?AGENT', '?OBJ'], [desires, '?AGENT', [possesses, '?AGENT', '?OBJ']]):-true.
instance(desires, 'PropositionalAttitude'):-true.
subrelation(desires, 'inScopeOfInterest'):-true.
'relatedInternalConcept'(desires, wants):-true.
domain(desires, 1, 'CognitiveAgent'):-true.
domain(desires, 2, 'Formula'):-true.
documentation(desires, "(&%desires ?AGENT ?FORMULA) means that ?AGENT wants 
to bring about the state of affairs expressed by ?FORMULA.  Note that there 
is no implication that what is desired by the agent is not already true.  
Note too that &%desires is distinguished from &%wants only in that the former 
is a &%PropositionalAttitude, while &%wants is an &%ObjectAttitude."):-true.
instance(considers, 'PropositionalAttitude'):-true.
subrelation(considers, 'inScopeOfInterest'):-true.
domain(considers, 1, 'CognitiveAgent'):-true.
domain(considers, 2, 'Formula'):-true.
documentation(considers, "(&%considers ?AGENT ?FORMULA) means that ?AGENT 
considers or wonders about the truth of the proposition expressed by 
?FORMULA."):-true.
instance(believes, 'PropositionalAttitude'):-true.
subrelation(believes, 'inScopeOfInterest'):-true.
domain(believes, 1, 'CognitiveAgent'):-true.
domain(believes, 2, 'Formula'):-true.
documentation(believes, "The epistemic predicate of belief.  
(&%believes ?AGENT ?FORMULA) means that ?AGENT believes the proposition 
expressed by ?FORMULA."):-true.
=>([believes, '?AGENT', '?FORMULA'], [exists, ['?TIME'], ['holdsDuring', '?TIME', [considers, '?AGENT', '?FORMULA']]]):-true.
instance(knows, 'PropositionalAttitude'):-true.
subrelation(knows, 'inScopeOfInterest'):-true.
domain(knows, 1, 'CognitiveAgent'):-true.
domain(knows, 2, 'Formula'):-true.
documentation(knows, "The epistemic predicate of knowing.  (&%knows 
?AGENT ?FORMULA) means that ?AGENT knows the proposition expressed by 
?FORMULA.  Note that &%knows entails conscious awareness, so this 
&%Predicate cannot be used to express tacit or subconscious or 
unconscious knowledge."):-true.
=>([knows, '?AGENT', '?FORMULA'], [believes, '?AGENT', '?FORMULA']):-true.
=>([knows, '?AGENT', '?FORMULA'], [true, '?FORMULA', 'True']):-true.
subclass('TernaryRelation', 'Relation'):-true.
instance('TernaryRelation', 'InheritableRelation'):-true.
documentation('TernaryRelation', "&%TernaryRelations relate three items.  
The two &%subclasses of &%TernaryRelation are &%TernaryPredicate and 
&%BinaryFunction."):-true.
=>([instance, '?REL', 'TernaryRelation'], [not, [exists, ['?ITEM1', '?ITEM2', '?ITEM3', '?ITEM4', '@ROW'], [holds, '?REL', '?ITEM1', '?ITEM2', '?ITEM3', '?ITEM4', '@ROW']]]):-true.
subclass('QuaternaryRelation', 'Relation'):-true.
instance('QuaternaryRelation', 'InheritableRelation'):-true.
documentation('QuaternaryRelation', "&%QuaternaryRelations relate four 
items.  The two &%subclasses of &%QuaternaryRelation are 
&%QuaternaryPredicate and &%TernaryFunction."):-true.
=>([instance, '?REL', 'QuaternaryRelation'], [not, [exists, ['?ITEM1', '?ITEM2', '?ITEM3', '?ITEM4', '?ITEM5', '@ROW'], [holds, '?REL', '?ITEM1', '?ITEM2', '?ITEM3', '?ITEM4', '?ITEM5', '@ROW']]]):-true.
subclass('QuintaryRelation', 'Relation'):-true.
instance('QuintaryRelation', 'InheritableRelation'):-true.
documentation('QuintaryRelation', "&%QuintaryRelations relate five items.  
The two &%subclasses of &%QuintaryRelation are &%QuintaryPredicate and 
&%QuaternaryFunction."):-true.
=>([instance, '?REL', 'QuintaryRelation'], [not, [exists, ['?ITEM1', '?ITEM2', '?ITEM3', '?ITEM4', '?ITEM5', '?ITEM6', '@ROW'], [holds, '?REL', '?ITEM1', '?ITEM2', '?ITEM3', '?ITEM4', '?ITEM5', '?ITEM6', '@ROW']]]):-true.
subclass('List', 'Relation'):-true.
documentation('List', "Every &%List is a particular ordered n-tuple of 
items.  Generally speaking, &%Lists are created by means of the &%ListFn 
&%Function, which takes any number of items as arguments and returns a 
&%List with the items in the same order.  Anything, including other 
&%Lists, may be an item in a &%List.  Note too that &%Lists are 
extensional - two lists that have the same items in the same order are 
identical.  Note too that a &%List may contain no items.  In that case, 
the &%List is the &%NullList."):-true.
=>([instance, '?LIST', 'List'], [exists, ['?NUMBER1'], [exists, ['?ITEM1'], [and, [not, [equal, ['ListOrderFn', '?LIST', '?NUMBER1'], '?ITEM1']], [forall, ['?NUMBER2'], [=>, [and, [instance, '?NUMBER2', 'PositiveInteger'], ['lessThan', '?NUMBER2', '?NUMBER1']], [exists, ['?ITEM2'], [equal, ['ListOrderFn', '?LIST', '?NUMBER2'], '?ITEM2']]]]]]]):-true.
subclass('UniqueList', 'List'):-true.
documentation('UniqueList', "A &%List in which no item appears more than once, 
i.e. a &%List for which there are no distinct numbers ?NUMBER1 and ?NUMBER2 
such that (&%ListOrderFn ?LIST ?NUMBER1) and (&%ListOrderFn ?LIST ?NUMBER2) 
return the same value."):-true.
<=>([instance, '?LIST', 'UniqueList'], [forall, ['?NUMBER1', '?NUMBER2'], [=>, [equal, ['ListOrderFn', '?LIST', '?NUMBER1'], ['ListOrderFn', '?LIST', '?NUMBER2']], [equal, '?NUMBER1', '?NUMBER2']]]):-true.
instance('NullList', 'List'):-true.
documentation('NullList', "The &%List that has no items.  The uniqueness of 
&%NullList follows from the extensionality of &%Lists, i.e. the fact that 
two &%Lists with the same items in the same order are identical."):-true.
<=>([equal, '?LIST', 'NullList'], [not, [exists, ['?ITEM'], ['inList', '?ITEM', '?LIST']]]):-true.
instance('ListFn', 'Function'):-true.
instance('ListFn', 'VariableArityRelation'):-true.
range('ListFn', 'List'):-true.
documentation('ListFn', "A &%Function that takes any number of arguments and 
returns the &%List containing those arguments in exactly the same order."):-true.
=>(['exhaustiveDecomposition', '?CLASS', '@ROW'], [forall, ['?OBJ'], [=>, [instance, '?OBJ', '?CLASS'], [exists, ['?ITEM'], [and, ['inList', '?ITEM', ['ListFn', '@ROW']], [instance, '?OBJ', '?ITEM']]]]]):-true.
=>(['disjointDecomposition', '?CLASS', '@ROW'], [forall, ['?ITEM'], [=>, ['inList', '?ITEM', ['ListFn', '@ROW']], [subclass, '?ITEM', '?CLASS']]]):-true.
=>(['disjointDecomposition', '?CLASS', '@ROW'], [forall, ['?NUMBER1', '?NUMBER2', '?ITEM1', '?ITEM2'], [=>, [and, [equal, ['ListOrderFn', ['ListFn', '@ROW'], '?NUMBER1'], '?ITEM1'], [equal, ['ListOrderFn', ['ListFn', '@ROW'], '?NUMBER2'], '?ITEM2'], [not, [equal, '?NUMBER1', '?NUMBER2']]], [disjoint, '?ITEM1', '?ITEM2']]]):-true.
instance('ListOrderFn', 'BinaryFunction'):-true.
domain('ListOrderFn', 1, 'List'):-true.
domain('ListOrderFn', 2, 'PositiveInteger'):-true.
range('ListOrderFn', 'Entity'):-true.
documentation('ListOrderFn', "(&%ListOrderFn ?LIST ?NUMBER) denotes the item 
that is in the ?NUMBER position in the &%List ?LIST.  For example, 
(&%ListOrderFn (&%ListFn &%Monday &%Tuesday &%Wednesday) 2) would return the 
value &%Tuesday."):-true.
=>([and, [instance, '?LIST1', 'List'], [instance, '?LIST2', 'List'], [forall, ['?NUMBER'], [equal, ['ListOrderFn', '?LIST1', '?NUMBER'], ['ListOrderFn', '?LIST2', '?NUMBER']]]], [equal, '?LIST1', '?LIST2']):-true.
=>([and, [domain, '?REL', '?NUMBER', '?CLASS'], [holds, '?REL', '@ROW']], [instance, ['ListOrderFn', ['ListFn', '@ROW'], '?NUMBER'], '?CLASS']):-true.
=>([and, ['domainSubclass', '?REL', '?NUMBER', '?CLASS'], [holds, '?REL', '@ROW']], [subclass, ['ListOrderFn', ['ListFn', '@ROW'], '?NUMBER'], '?CLASS']):-true.
instance('ListLengthFn', 'UnaryFunction'):-true.
domain('ListLengthFn', 1, 'List'):-true.
range('ListLengthFn', 'NonnegativeInteger'):-true.
documentation('ListLengthFn', "A &%Function that takes a &%List as its sole 
argument and returns the number of items in the &%List.  For example, 
(&%ListLengthFn (&%ListFn &%Monday &%Tuesday &%Wednesday)) would return the 
value 3."):-true.
=>([equal, ['ListLengthFn', '?LIST'], '?NUMBER1'], [forall, ['?NUMBER2'], [<=>, [exists, ['?ITEM'], [equal, ['ListOrderFn', '?LIST', '?NUMBER2'], '?ITEM']], ['lessThanOrEqualTo', '?NUMBER2', '?NUMBER1']]]):-true.
equal(['ListLengthFn', ['ListFn', '@ROW', '?ITEM']], ['SuccessorFn', ['ListLengthFn', ['ListFn', '@ROW']]]):-true.
equal(['ListOrderFn', ['ListFn', '@ROW', '?ITEM'], ['ListLengthFn', ['ListFn', '@ROW', '?ITEM']]], '?ITEM'):-true.
=>([valence, '?REL', '?NUMBER'], [forall, ['@ROW'], [=>, [holds, '?REL', '@ROW'], [equal, ['ListLengthFn', ['ListFn', '@ROW']], '?NUMBER']]]):-true.
=>([equal, ['ListLengthFn', '?LIST1'], '?NUMBER'], [exists, ['?LIST2'], [and, ['initialList', '?LIST1', '?LIST2'], [equal, ['SuccessorFn', '?NUMBER'], ['ListLengthFn', '?LIST2']], [equal, ['ListOrderFn', '?LIST2', ['SuccessorFn', '?NUMBER']], '?ITEM']]]):-true.
instance('ListConcatenateFn', 'BinaryFunction'):-true.
domain('ListConcatenateFn', 1, 'List'):-true.
domain('ListConcatenateFn', 2, 'List'):-true.
range('ListConcatenateFn', 'List'):-true.
documentation('ListConcatenateFn', "A &%Function that returns the concatenation 
of the two &%Lists that are given as arguments.  For example, the value of 
(&%ListConcatenateFn (&%ListFn &%Monday &%Tuesday) (&%ListFn &%Wednesday 
&%Thursday)) would be (&%ListFn &%Monday &%Tuesday &%Wednesday &%Thursday)."):-true.
<=>([equal, '?LIST3', ['ListConcatenateFn', '?LIST1', '?LIST2']], [forall, ['?NUMBER1', '?NUMBER2'], [=>, [and, ['lessThanOrEqualTo', '?NUMBER1', ['ListLengthFn', '?LIST1']], ['lessThanOrEqualTo', '?NUMBER2', ['ListLengthFn', '?LIST2']], [instance, '?NUMBER1', 'PositiveInteger'], [instance, '?NUMBER2', 'PositiveInteger']], [and, [equal, ['ListOrderFn', '?LIST3', '?NUMBER1'], ['ListOrderFn', '?LIST1', '?NUMBER1']], [equal, ['ListOrderFn', '?LIST3', ['AdditionFn', ['ListLengthFn', '?LIST1'], '?NUMBER2']], ['ListOrderFn', '?LIST2', '?NUMBER2']]]]]):-true.
instance('inList', 'BinaryPredicate'):-true.
instance('inList', 'IrreflexiveRelation'):-true.
instance('inList', 'AsymmetricRelation'):-true.
domain('inList', 1, 'Entity'):-true.
domain('inList', 2, 'List'):-true.
documentation('inList', "The analog of &%element and &%instance for &%Lists.  
(&%inList ?OBJ ?LIST) means that ?OBJ is in the &%List ?LIST.  For example, 
(&%inList &%Tuesday (&%ListFn &%Monday &%Tuesday &%Wednesday)) would be true."):-true.
<=>(['inList', '?ITEM', '?LIST'], [exists, ['?NUMBER'], [equal, ['ListOrderFn', '?LIST', '?NUMBER'], '?ITEM']]):-true.
instance('subList', 'BinaryPredicate'):-true.
instance('subList', 'PartialOrderingRelation'):-true.
domain('subList', 1, 'List'):-true.
domain('subList', 2, 'List'):-true.
documentation('subList', "(&%subList ?LIST1 ?LIST2) means that ?LIST1 is a 
sublist of ?LIST2, i.e. every element of ?LIST1 is an element of ?LIST2 and 
the elements that are common to both &%Lists have the same order in both 
&%Lists."):-true.
=>(['subList', '?LIST1', '?LIST2'], [forall, ['?ITEM'], [=>, ['inList', '?ITEM', '?LIST1'], ['inList', '?ITEM', '?LIST2']]]):-true.
=>(['subList', '?LIST1', '?LIST2'], [exists, ['?NUMBER3'], [forall, ['?ITEM'], [=>, ['inList', '?ITEM', '?LIST1'], [exists, ['?NUMBER1', '?NUMBER2'], [and, [equal, ['ListOrderFn', '?LIST1', '?NUMBER1'], '?ITEM'], [equal, ['ListOrderFn', '?LIST2', '?NUMBER2'], '?ITEM'], [equal, '?NUMBER2', ['AdditionFn', '?NUMBER1', '?NUMBER3']]]]]]]):-true.
instance('initialList', 'BinaryPredicate'):-true.
subrelation('initialList', 'subList'):-true.
documentation('initialList', "(&%initialList ?LIST1 ?LIST2) means that ?LIST1 
is a &%subList of ?LIST2 and (&%ListOrderFn ?LIST1 ?NUMBER) returns the same 
value as (&%ListOrderFn ?LIST2 ?NUMBER) for all of the values of ?NUMBER over 
which (&%ListOrderFn ?LIST1 ?NUMBER) is defined."):-true.
=>(['initialList', '?LIST1', '?LIST2'], [forall, ['?NUMBER1', '?NUMBER2'], [=>, [and, [equal, ['ListLengthFn', '?LIST1'], '?NUMBER1'], ['lessThanOrEqualTo', '?NUMBER2', '?NUMBER1']], [equal, ['ListOrderFn', '?LIST1', '?NUMBER2'], ['ListOrderFn', '?LIST2', '?NUMBER2']]]]):-true.
'initialList'(['ListFn', '@ROW'], ['ListFn', '@ROW', '?ITEM']):-true.
subclass('Predicate', 'Relation'):-true.
instance('Predicate', 'InheritableRelation'):-true.
documentation('Predicate', "A &%Predicate is a sentence-forming &%Relation.  
Each tuple in the &%Relation is a finite, ordered sequence of objects.  
The fact that a particular tuple is an element of a &%Predicate is denoted 
by '(*predicate* arg_1 arg_2 .. arg_n)', where the arg_i are the 
objects so related.  In the case of &%BinaryPredicates, the fact can
be read as `arg_1 is *predicate* arg_2' or `a *predicate* of
arg_1 is arg_2'."):-true.
subclass('Function', 'SingleValuedRelation'):-true.
instance('Function', 'InheritableRelation'):-true.
documentation('Function', "A &%Function is a term-forming &%Relation that 
maps from a n-tuple of arguments to a range and that associates this 
n-tuple with at most one range element.  Note that the range is a &%SetOrClass, 
and each element of the range is an instance of the &%SetOrClass."):-true.
subclass('UnaryFunction', 'Function'):-true.
subclass('UnaryFunction', 'BinaryRelation'):-true.
instance('UnaryFunction', 'InheritableRelation'):-true.
documentation('UnaryFunction', "The &%Class of &%Functions that require a 
single argument."):-true.
=>([instance, '?FUNCTION', 'UnaryFunction'], [valence, '?FUNCTION', 1]):-true.
subclass('OneToOneFunction', 'UnaryFunction'):-true.
documentation('OneToOneFunction', "The &%Class of &%UnaryFunctions which 
are one to one.  A function F is one to one just in case for all X, Y in the 
domain of F, if X is not identical to Y, then F(X) is not identical to F(Y)."):-true.
<=>([instance, '?FUN', 'OneToOneFunction'], [forall, ['?ARG1', '?ARG2'], [=>, [and, [instance, '?ARG1', ['DomainFn', '?FUN']], [instance, '?ARG2', ['DomainFn', '?FUN']], [not, [equal, '?ARG1', '?ARG2']]], [not, [equal, ['AssignmentFn', '?FUN', '?ARG1'], ['AssignmentFn', '?FUN', '?ARG2']]]]]):-true.
subclass('SequenceFunction', 'OneToOneFunction'):-true.
documentation('SequenceFunction', "The &%Class of &%OneToOneFunctions whose range 
is a subclass of the &%PositiveIntegers."):-true.
=>([instance, '?SEQ', 'SequenceFunction'], [subclass, ['RangeFn', '?SEQ'], 'Integer']):-true.
subclass('BinaryFunction', 'Function'):-true.
subclass('BinaryFunction', 'TernaryRelation'):-true.
instance('BinaryFunction', 'InheritableRelation'):-true.
documentation('BinaryFunction', "The &%Class of &%Functions that require 
two arguments."):-true.
=>([instance, '?FUNCTION', 'BinaryFunction'], [valence, '?FUNCTION', 2]):-true.
subclass('AssociativeFunction', 'BinaryFunction'):-true.
documentation('AssociativeFunction', "A &%BinaryFunction is associative if 
bracketing has no effect on the value returned by the &%Function.  More 
precisely, a &%Function ?FUNCTION is associative just in case 
(?FUNCTION ?INST1 (?FUNCTION ?INST2 ?INST3)) is equal to 
(?FUNCTION (?FUNCTION ?INST1 ?INST2) ?INST3), for all ?INST1, ?INST2, 
and ?INST3."):-true.
=>([instance, '?FUNCTION', 'AssociativeFunction'], [forall, ['?INST1', '?INST2', '?INST3'], [=>, [and, [instance, '?INST1', ['DomainFn', '?FUNCTION']], [instance, '?INST2', ['DomainFn', '?FUNCTION']], [instance, '?INST3', ['DomainFn', '?FUNCTION']]], [equal, ['AssignmentFn', '?FUNCTION', '?INST1', ['AssignmentFn', '?FUNCTION', '?INST2', '?INST3']], ['AssignmentFn', '?FUNCTION', ['AssignmentFn', '?FUNCTION', '?INST1', '?INST2'], '?INST3']]]]):-true.
subclass('CommutativeFunction', 'BinaryFunction'):-true.
documentation('CommutativeFunction', "A &%BinaryFunction is commutative if 
the ordering of the arguments of the function has no effect on the value 
returned by the function.  More precisely, a function ?FUNCTION is 
commutative just in case (?FUNCTION ?INST1 ?INST2) is equal to (?FUNCTION 
?INST2 ?INST1), for all ?INST1 and ?INST2."):-true.
=>([instance, '?FUNCTION', 'CommutativeFunction'], [forall, ['?INST1', '?INST2'], [=>, [and, [instance, '?INST1', ['DomainFn', '?FUNCTION']], [instance, '?INST2', ['DomainFn', '?FUNCTION']]], [equal, ['AssignmentFn', '?FUNCTION', '?INST1', '?INST2'], ['AssignmentFn', '?FUNCTION', '?INST2', '?INST1']]]]):-true.
subclass('TernaryFunction', 'Function'):-true.
subclass('TernaryFunction', 'QuaternaryRelation'):-true.
instance('TernaryFunction', 'InheritableRelation'):-true.
documentation('TernaryFunction', "The &%Class of &%Functions that require 
exactly three arguments."):-true.
=>([instance, '?FUNCTION', 'TernaryFunction'], [valence, '?FUNCTION', 3]):-true.
subclass('QuaternaryFunction', 'Function'):-true.
subclass('QuaternaryFunction', 'QuintaryRelation'):-true.
instance('QuaternaryFunction', 'InheritableRelation'):-true.
documentation('QuaternaryFunction', "The &%Class of &%Functions that require 
exactly four arguments."):-true.
=>([instance, '?FUNCTION', 'QuaternaryFunction'], [valence, '?FUNCTION', 4]):-true.
subclass('ContinuousFunction', 'Function'):-true.
documentation('ContinuousFunction', "&%Functions which are continuous.  
This concept is taken as primitive until representations for limits 
are devised."):-true.
subclass('LogicalOperator', 'Predicate'):-true.
documentation('LogicalOperator', "This &%Class currently comprises all 
of the logical operators (viz. 'and', 'or', 'not', '=>', and '<=>')."):-true.
subclass('BinaryPredicate', 'Predicate'):-true.
subclass('BinaryPredicate', 'BinaryRelation'):-true.
instance('BinaryPredicate', 'InheritableRelation'):-true.
documentation('BinaryPredicate', "A &%Predicate relating two items - its 
valence is two."):-true.
=>([instance, '?REL', 'BinaryPredicate'], [valence, '?REL', 2]):-true.
subclass('TernaryPredicate', 'Predicate'):-true.
subclass('TernaryPredicate', 'TernaryRelation'):-true.
instance('TernaryPredicate', 'InheritableRelation'):-true.
documentation('TernaryPredicate', "The &%Class of &%Predicates that require 
exactly three arguments."):-true.
=>([instance, '?REL', 'TernaryPredicate'], [valence, '?REL', 3]):-true.
subclass('QuaternaryPredicate', 'Predicate'):-true.
subclass('QuaternaryPredicate', 'QuaternaryRelation'):-true.
instance('QuaternaryPredicate', 'InheritableRelation'):-true.
documentation('QuaternaryPredicate', "The &%Class of &%Predicates that 
require four arguments."):-true.
=>([instance, '?REL', 'QuaternaryPredicate'], [valence, '?REL', 4]):-true.
subclass('QuintaryPredicate', 'Predicate'):-true.
subclass('QuintaryPredicate', 'QuintaryRelation'):-true.
instance('QuintaryPredicate', 'InheritableRelation'):-true.
documentation('QuintaryPredicate', "The &%Class of &%Predicates that 
require five arguments."):-true.
=>([instance, '?REL', 'QuintaryPredicate'], [valence, '?REL', 5]):-true.
subclass('VariableArityRelation', 'Relation'):-true.
documentation('VariableArityRelation', "The &%Class of &%Relations that 
do not have a fixed number of arguments."):-true.
=>([instance, '?REL', 'VariableArityRelation'], [not, [exists, ['?INT'], [valence, '?REL', '?INT']]]):-true.
subclass('RelationExtendedToQuantities', 'Relation'):-true.
instance('RelationExtendedToQuantities', 'InheritableRelation'):-true.
documentation('RelationExtendedToQuantities', "A 
&%RelationExtendedToQuantities is a &%Relation that, when it is true on 
a sequence of arguments that are &%RealNumbers, it is also true on a 
sequence of &%ConstantQuantites with those magnitudes in some unit of 
measure.  For example, the &%lessThan relation is extended to quantities.  
This means that for all pairs of quantities ?QUANTITY1 and ?QUANTITY2, 
(lessThan ?QUANTITY1 ?QUANTITY2) if and only if, for some ?NUMBER1, 
?NUMBER2, and ?UNIT, ?QUANTITY1 = (MeasureFn ?NUMBER1 ?UNIT), 
?QUANTITY2 = (MeasureFn ?NUMBER2 ?UNIT), and (lessThan ?NUMBER1 ?NUMBER2), 
for all units ?UNIT on which ?QUANTITY1 and ?QUANTITY2 can be measured.  
Note that, when a &%RelationExtendedToQuantities is extended from 
&%RealNumbers to &%ConstantQuantities, the &%ConstantQuantities must be 
measured along the same physical dimension."):-true.
subclass('Proposition', 'Abstract'):-true.
documentation('Proposition', "&%Propositions are &%Abstract entities that 
express a complete thought or a set of such thoughts.  As an example, 
the formula '(instance Yojo Cat)' expresses the &%Proposition that the 
entity named Yojo is an element of the &%Class of Cats.  Note that 
propositions are not restricted to the content expressed by individual 
sentences of a &%Language.  They may encompass the content expressed by 
theories, books, and even whole libraries.  It is important to distinguish 
&%Propositions from the &%ContentBearingObjects that express them.  A 
&%Proposition is a piece of information, e.g. that the cat is on the mat, 
but a &%ContentBearingObject is an &%Object that represents this information.  
A &%Proposition is an abstraction that may have multiple representations: 
strings, sounds, icons, etc.  For example, the &%Proposition that the cat is 
on the mat is represented here as a string of graphical characters displayed 
on a monitor and/or printed on paper, but it can be represented by a sequence 
of sounds or by some non-latin alphabet or by some cryptographic form"):-true.
instance('closedOn', 'BinaryPredicate'):-true.
instance('closedOn', 'AsymmetricRelation'):-true.
domain('closedOn', 1, 'Function'):-true.
domain('closedOn', 2, 'SetOrClass'):-true.
documentation('closedOn', "A &%BinaryFunction is closed on a &%SetOrClass 
if it is defined for all instances of the &%SetOrClass and its value is 
always an instance of the &%SetOrClass."):-true.
=>([and, ['closedOn', '?FUNCTION', '?CLASS'], [instance, '?FUNCTION', 'UnaryFunction']], [forall, ['?INST'], [=>, [instance, '?INST', '?CLASS'], [instance, ['AssignmentFn', '?FUNCTION', '?INST'], '?CLASS']]]):-true.
=>([and, ['closedOn', '?FUNCTION', '?CLASS'], [instance, '?FUNCTION', 'BinaryFunction']], [forall, ['?INST1', '?INST2'], [=>, [and, [instance, '?INST1', '?CLASS'], [instance, '?INST2', '?CLASS']], [instance, ['AssignmentFn', '?FUNCTION', '?INST1', '?INST2'], '?CLASS']]]):-true.
instance('reflexiveOn', 'BinaryPredicate'):-true.
instance('reflexiveOn', 'AsymmetricRelation'):-true.
domain('reflexiveOn', 1, 'BinaryRelation'):-true.
domain('reflexiveOn', 2, 'SetOrClass'):-true.
documentation('reflexiveOn', "A &%BinaryRelation is reflexive on a 
&%SetOrClass only if every instance of the &%SetOrClass bears the relation 
to itself."):-true.
=>(['reflexiveOn', '?RELATION', '?CLASS'], [forall, ['?INST'], [=>, [instance, '?INST', '?CLASS'], [holds, '?RELATION', '?INST', '?INST']]]):-true.
instance('irreflexiveOn', 'BinaryPredicate'):-true.
instance('irreflexiveOn', 'AsymmetricRelation'):-true.
domain('irreflexiveOn', 1, 'BinaryRelation'):-true.
domain('irreflexiveOn', 2, 'SetOrClass'):-true.
documentation('irreflexiveOn', "A &%BinaryRelation is irreflexive on a 
&%SetOrClass only if no instance of the &%SetOrClass bears the relation to 
itself."):-true.
=>(['irreflexiveOn', '?RELATION', '?CLASS'], [forall, ['?INST'], [=>, [instance, '?INST', '?CLASS'], [not, [holds, '?RELATION', '?INST', '?INST']]]]):-true.
instance('partialOrderingOn', 'BinaryPredicate'):-true.
instance('partialOrderingOn', 'AsymmetricRelation'):-true.
domain('partialOrderingOn', 1, 'BinaryRelation'):-true.
domain('partialOrderingOn', 2, 'SetOrClass'):-true.
documentation('partialOrderingOn', "A &%BinaryRelation is a partial 
ordering on a &%SetOrClass only if the relation is &%reflexiveOn the 
&%SetOrClass, and it is both an &%AntisymmetricRelation, and a 
&%TransitiveRelation."):-true.
=>(['partialOrderingOn', '?RELATION', '?CLASS'], [and, ['reflexiveOn', '?RELATION', '?CLASS'], [instance, '?RELATION', 'TransitiveRelation'], [instance, '?RELATION', 'AntisymmetricRelation']]):-true.
instance('totalOrderingOn', 'BinaryPredicate'):-true.
instance('totalOrderingOn', 'AsymmetricRelation'):-true.
domain('totalOrderingOn', 1, 'BinaryRelation'):-true.
domain('totalOrderingOn', 2, 'SetOrClass'):-true.
documentation('totalOrderingOn', "A &%BinaryRelation ?REL is a total 
ordering on a &%SetOrClass only if it is a partial ordering for which either 
(?REL ?INST1 ?INST2) or (?REL ?INST2 ?INST1) for every ?INST1 and ?INST2 
in the &%SetOrClass."):-true.
<=>(['totalOrderingOn', '?RELATION', '?CLASS'], [and, ['partialOrderingOn', '?RELATION', '?CLASS'], ['trichotomizingOn', '?RELATION', '?CLASS']]):-true.
instance('trichotomizingOn', 'BinaryPredicate'):-true.
instance('trichotomizingOn', 'AsymmetricRelation'):-true.
domain('trichotomizingOn', 1, 'BinaryRelation'):-true.
domain('trichotomizingOn', 2, 'SetOrClass'):-true.
documentation('trichotomizingOn', "A &%BinaryRelation ?REL is 
trichotomizing on a &%SetOrClass only if, for all instances ?INST1 and ?INST2 
of the &%SetOrClass, at least one of the following holds:  (?REL ?INST1 ?INST2),
(?REL ?INST2 ?INST1) or (equal ?INST1 ?INST2)."):-true.
=>(['trichotomizingOn', '?RELATION', '?CLASS'], [forall, ['?INST1', '?INST2'], [=>, [and, [instance, '?INST1', '?CLASS'], [instance, '?INST2', '?CLASS']], [or, [holds, '?RELATION', '?INST1', '?INST2'], [holds, '?RELATION', '?INST2', '?INST1'], [equal, '?INST1', '?INST2']]]]):-true.
instance('equivalenceRelationOn', 'BinaryPredicate'):-true.
instance('equivalenceRelationOn', 'AsymmetricRelation'):-true.
domain('equivalenceRelationOn', 1, 'BinaryRelation'):-true.
domain('equivalenceRelationOn', 2, 'SetOrClass'):-true.
documentation('equivalenceRelationOn', "A &%BinaryRelation is an 
&%equivalenceRelationOn a &%SetOrClass only if the relation is &%reflexiveOn 
the &%SetOrClass and it is both a &%TransitiveRelation and a 
&%SymmetricRelation."):-true.
=>(['equivalenceRelationOn', '?RELATION', '?CLASS'], [and, [instance, '?RELATION', 'TransitiveRelation'], [instance, '?RELATION', 'SymmetricRelation'], ['reflexiveOn', '?RELATION', '?CLASS']]):-true.
instance(distributes, 'BinaryPredicate'):-true.
instance(distributes, 'BinaryRelation'):-true.
domain(distributes, 1, 'BinaryFunction'):-true.
domain(distributes, 2, 'BinaryFunction'):-true.
documentation(distributes, "A &%BinaryFunction ?FUNCTION1 is 
distributive over another &%BinaryFunction ?FUNCTION2 just in case 
(?FUNCTION1 ?INST1 (?FUNCTION2 ?INST2 ?INST3)) is equal to 
(?FUNCTION2 (?FUNCTION1 ?INST1 ?INST2) (?FUNCTION1 ?INST1 ?INST3)), 
for all ?INST1, ?INST2, and ?INST3."):-true.
=>([distributes, '?FUNCTION1', '?FUNCTION2'], [forall, ['?INST1', '?INST2', '?INST3'], [=>, [and, [instance, '?INST1', ['DomainFn', '?FUNCTION1']], [instance, '?INST2', ['DomainFn', '?FUNCTION1']], [instance, '?INST3', ['DomainFn', '?FUNCTION1']], [instance, '?INST1', ['DomainFn', '?FUNCTION2']], [instance, '?INST2', ['DomainFn', '?FUNCTION2']], [instance, '?INST3', ['DomainFn', '?FUNCTION2']]], [equal, ['AssignmentFn', '?FUNCTION1', '?INST1', ['AssignmentFn', '?FUNCTION2', '?INST2', '?INST3']], ['AssignmentFn', '?FUNCTION2', ['AssignmentFn', '?FUNCTION1', '?INST1', '?INST2'], ['AssignmentFn', '?FUNCTION1', '?INST1', '?INST3']]]]]):-true.
instance(causes, 'BinaryPredicate'):-true.
instance(causes, 'AsymmetricRelation'):-true.
domain(causes, 1, 'Process'):-true.
domain(causes, 2, 'Process'):-true.
'relatedInternalConcept'(causes, 'causesSubclass'):-true.
documentation(causes, "The causation relation between instances of &%Process.  
(&%causes ?PROCESS1 ?PROCESS2) means that the instance of &%Process ?PROCESS1 
brings about the instance of &%Process ?PROCESS2, e.g. (&%causes &%Killing &%Death)."):-true.
=>([instance, '?PROC1', 'Process'], [exists, ['?PROC2'], [causes, '?PROC2', '?PROC1']]):-true.
instance('causesSubclass', 'BinaryPredicate'):-true.
instance('causesSubclass', 'AsymmetricRelation'):-true.
'domainSubclass'('causesSubclass', 1, 'Process'):-true.
'domainSubclass'('causesSubclass', 2, 'Process'):-true.
documentation('causesSubclass', "The causation relation between subclasses of &%Process.  
(&%causesSubclass ?PROCESS1 ?PROCESS2) means that the subclass of &%Process ?PROCESS1 
brings about the subclass of &%Process ?PROCESS2, e.g. (&%causes &%Killing &%Death)."):-true.
=>(['causesSubclass', '?PROC1', '?PROC2'], [forall, ['?INST2'], [=>, [instance, '?INST2', '?PROC2'], [exists, ['?INST1'], [and, [instance, '?INST1', '?PROC1'], [causes, '?INST1', '?INST2']]]]]):-true.
instance(copy, 'BinaryPredicate'):-true.
instance(copy, 'EquivalenceRelation'):-true.
domain(copy, 1, 'Object'):-true.
domain(copy, 2, 'Object'):-true.
documentation(copy, "relates an &%Object to an exact copy of the 
&%Object, where an exact copy is indistinguishable from the original 
with regard to every property except (possibly) spatial and/or temporal 
location."):-true.
=>([copy, '?OBJ1', '?OBJ2'], [forall, ['?ATTR'], [=>, [attribute, '?OBJ1', '?ATTR'], [attribute, '?OBJ2', '?ATTR']]]):-true.
instance(time, 'BinaryPredicate'):-true.
instance(time, 'TemporalRelation'):-true.
instance(time, 'AsymmetricRelation'):-true.
domain(time, 1, 'Physical'):-true.
domain(time, 2, 'TimePosition'):-true.
documentation(time, "This relation holds between an instance of 
&%Physical and an instance of &%TimePosition just in case the temporal 
lifespan of the former includes the latter.  The constants &%located 
and &%time are the basic spatial and temporal predicates, 
respectively."):-true.
instance('holdsDuring', 'BinaryPredicate'):-true.
instance('holdsDuring', 'AsymmetricRelation'):-true.
domain('holdsDuring', 1, 'TimePosition'):-true.
domain('holdsDuring', 2, 'Formula'):-true.
documentation('holdsDuring', "(&%holdsDuring ?TIME ?FORMULA) means that the 
proposition denoted by ?FORMULA is true in the time frame ?TIME.  Note 
that this implies that ?FORMULA is true at every &%TimePoint which is a 
&%temporalPart of ?TIME."):-true.
=>([and, ['holdsDuring', '?TIME', '?SITUATION1'], [entails, '?SITUATION1', '?SITUATION2']], ['holdsDuring', '?TIME', '?SITUATION2']):-true.
=>(['holdsDuring', '?TIME', [not, '?SITUATION']], [not, ['holdsDuring', '?TIME', '?SITUATION']]):-true.
instance(capability, 'TernaryPredicate'):-true.
'domainSubclass'(capability, 1, 'Process'):-true.
domain(capability, 2, 'CaseRole'):-true.
domain(capability, 3, 'Object'):-true.
documentation(capability, "(&%capability ?PROCESS ?ROLE ?OBJ) means 
that ?OBJ has the ability to play the role of ?ROLE in &%Processes of 
type ?PROCESS."):-true.
=>([and, [instance, '?ROLE', 'CaseRole'], [holds, '?ROLE', '?ARG1', '?ARG2'], [instance, '?ARG1', '?PROC']], [capability, '?PROC', '?ROLE', '?ARG2']):-true.
instance(exploits, 'BinaryPredicate'):-true.
instance(exploits, 'AsymmetricRelation'):-true.
domain(exploits, 1, 'Object'):-true.
domain(exploits, 2, 'Agent'):-true.
documentation(exploits, "(&%exploits ?OBJ ?AGENT) means that ?OBJ is used 
by ?AGENT as a &%resource in an unspecified instance of &%Process.  This 
&%Predicate, as its corresponding axiom indicates, is a composition of the 
relations &%agent and &%resource."):-true.
=>([exploits, '?OBJ', '?AGENT'], [exists, ['?PROCESS'], [and, [agent, '?PROCESS', '?AGENT'], [resource, '?PROCESS', '?OBJ']]]):-true.
instance('hasPurpose', 'BinaryPredicate'):-true.
instance('hasPurpose', 'AsymmetricRelation'):-true.
domain('hasPurpose', 1, 'Physical'):-true.
domain('hasPurpose', 2, 'Formula'):-true.
documentation('hasPurpose', "This &%Predicate expresses the concept of a 
conventional goal, i.e. a goal with a neutralized agent's intention.  
Accordingly, (&%hasPurpose ?THING ?FORMULA) means that the instance of 
&%Physical ?THING has, as its purpose, the &%Proposition expressed by 
?FORMULA.  Note that there is an important difference in meaning between 
the &%Predicates &%hasPurpose and &%result.  Although the second argument 
of the latter can satisfy the second argument of the former, 
a conventional goal is an expected and desired outcome, while a result 
may be neither expected nor desired.  For example, a machine process may 
have outcomes but no goals, aimless wandering may have an outcome but no 
goal; a learning process may have goals with no outcomes, and so on."):-true.
instance('hasPurposeForAgent', 'TernaryPredicate'):-true.
domain('hasPurposeForAgent', 1, 'Physical'):-true.
domain('hasPurposeForAgent', 2, 'Formula'):-true.
domain('hasPurposeForAgent', 3, 'Agent'):-true.
documentation('hasPurposeForAgent', "Expresses a cognitive attitude of an 
agent with respect to a particular instance of Physical.  More precisely, 
(&%hasPurposeForAgent ?THING ?FORMULA ?AGENT) means that the purpose of 
?THING for ?AGENT is the proposition expressed by ?FORMULA.  Very complex 
issues are involved here.  In particular, the rules of inference of the 
first order predicate calculus are not truth-preserving for the second 
argument position of this &%Predicate."):-true.
=>(['hasPurpose', '?THING', '?PURPOSE'], [exists, ['?AGENT'], ['hasPurposeForAgent', '?THING', '?PURPOSE', '?AGENT']]):-true.
instance('hasSkill', 'BinaryPredicate'):-true.
instance('hasSkill', 'AsymmetricRelation'):-true.
'domainSubclass'('hasSkill', 1, 'Process'):-true.
domain('hasSkill', 2, 'Agent'):-true.
documentation('hasSkill', "Similar to the &%capability &%Predicate 
with the additional restriction that the ability be practised/
demonstrated to some measurable degree."):-true.
=>(['hasSkill', '?PROC', '?AGENT'], [capability, '?PROC', agent, '?AGENT']):-true.
instance('holdsRight', 'BinaryPredicate'):-true.
instance('holdsRight', 'AsymmetricRelation'):-true.
'domainSubclass'('holdsRight', 1, 'Process'):-true.
domain('holdsRight', 2, 'CognitiveAgent'):-true.
documentation('holdsRight', "Expresses a relationship between a subclass 
of &%Process and a &%CognitiveAgent whereby the &%CognitiveAgent has 
the right to perform instances of the &%Process type specified, 
i.e. to be an &%agent of instances of the &%Process type."):-true.
=>(['holdsRight', '?PROCESS', '?AGENT'], [capability, '?PROCESS', agent, '?AGENT']):-true.
instance('confersRight', 'TernaryPredicate'):-true.
'domainSubclass'('confersRight', 1, 'Process'):-true.
domain('confersRight', 2, 'Entity'):-true.
domain('confersRight', 3, 'CognitiveAgent'):-true.
documentation('confersRight', "Expresses the relationship between a subclass 
of &%Process, an &%Entity, and a &%CognitiveAgent when the &%Entity 
authorizes the &%CognitiveAgent to perform instances of the &%Process type 
specified, i.e. to be an &%agent of instances of the &%Process type."):-true.
=>(['confersRight', '?PROCESS', '?AGENT1', '?AGENT2'], ['holdsRight', '?PROCESS', '?AGENT2']):-true.
instance('holdsObligation', 'BinaryPredicate'):-true.
instance('holdsObligation', 'AsymmetricRelation'):-true.
'domainSubclass'('holdsObligation', 1, 'Process'):-true.
domain('holdsObligation', 2, 'CognitiveAgent'):-true.
'relatedInternalConcept'('holdsObligation', 'holdsRight'):-true.
documentation('holdsObligation', "Expresses a relationship between a 
subclass of &%Process and a &%CognitiveAgent whereby the &%CognitiveAgent 
has the obligation to perform instances of the &%Process type specified, 
i.e. to be an &%agent of instances of the &%Process type."):-true.
=>(['holdsObligation', '?PROCESS', '?AGENT'], [capability, '?PROCESS', agent, '?AGENT']):-true.
instance('confersObligation', 'TernaryPredicate'):-true.
'domainSubclass'('confersObligation', 1, 'Process'):-true.
domain('confersObligation', 2, 'Entity'):-true.
domain('confersObligation', 3, 'CognitiveAgent'):-true.
'relatedInternalConcept'('confersObligation', 'confersRight'):-true.
documentation('confersObligation', "Expresses the relationship between a 
subclass of &%Process, an &%Entity, and a &%CognitiveAgent when the 
&%Entity obligates the &%CognitiveAgent to perform instances of the &%Process 
type specified, i.e. to be an &%agent of instances of the &%Process type."):-true.
=>(['confersObligation', '?PROCESS', '?AGENT1', '?AGENT2'], ['holdsObligation', '?PROCESS', '?AGENT2']):-true.
instance(located, 'PartialOrderingRelation'):-true.
subrelation(located, 'partlyLocated'):-true.
domain(located, 1, 'Physical'):-true.
domain(located, 2, 'Object'):-true.
'relatedInternalConcept'(located, time):-true.
documentation(located, "A very general predicate.  (&%located 
?PHYS ?OBJ) means that ?PHYS is situated at ?OBJ, in some sense.  The 
&%Predicates &%located and &%time are spatial and temporal 
predicates, respectively."):-true.
=>([located, '?OBJ', '?REGION'], [forall, ['?SUBOBJ'], [=>, [part, '?SUBOBJ', '?OBJ'], [located, '?SUBOBJ', '?REGION']]]):-true.
instance('partlyLocated', 'SpatialRelation'):-true.
instance('partlyLocated', 'ReflexiveRelation'):-true.
instance('partlyLocated', 'BinaryPredicate'):-true.
domain('partlyLocated', 1, 'Object'):-true.
domain('partlyLocated', 2, 'Region'):-true.
documentation('partlyLocated', "The predicate of partial localization.  
For example, Istanbul is partly located in Asia.  Note that this is the 
most basic localization relation:  &%located and &%exactlyLocated 
are both subrelations of &%partlyLocated."):-true.
=>(['partlyLocated', '?OBJ', '?REGION'], [exists, ['?SUBOBJ'], [and, [part, '?SUBOBJ', '?OBJ'], ['exactlyLocated', '?SUBOBJ', '?REGION']]]):-true.
subrelation('exactlyLocated', located):-true.
documentation('exactlyLocated', "The actual, minimal location of an 
Object.  This is a subrelation of the more general Predicate 
&%located."):-true.
=>(['exactlyLocated', '?OBJ', '?REGION'], [not, [exists, ['?OTHEROBJ'], [and, ['exactlyLocated', '?OTHEROBJ', '?REGION'], [not, [equal, '?OTHEROBJ', '?OBJ']]]]]):-true.
instance(between, 'SpatialRelation'):-true.
instance(between, 'TernaryPredicate'):-true.
domain(between, 1, 'Object'):-true.
domain(between, 2, 'Object'):-true.
domain(between, 3, 'Object'):-true.
documentation(between, "(between ?OBJ1 ?OBJ2 ?OBJ3) means that ?OBJ2 is 
spatially located between ?OBJ1 and ?OBJ3.  Note that this implies that 
?OBJ2 is directly between ?OBJ1 and ?OBJ3, i.e. the projections of ?OBJ1 
and ?OBJ3 overlap with ?OBJ2."):-true.
instance(traverses, 'SpatialRelation'):-true.
domain(traverses, 1, 'Object'):-true.
domain(traverses, 2, 'Object'):-true.
documentation(traverses, "(&%traverses ?OBJ1 ?OBJ2) means that ?OBJ1 
crosses or extends across ?OBJ2.  Note that &%crosses and 
&%penetrates are subrelations of &%traverses."):-true.
=>([traverses, '?OBJ1', '?OBJ2'], [or, [crosses, '?OBJ1', '?OBJ2'], [penetrates, '?OBJ1', '?OBJ2']]):-true.
subrelation(crosses, traverses):-true.
instance(crosses, 'AsymmetricRelation'):-true.
instance(crosses, 'TransitiveRelation'):-true.
'disjointRelation'(crosses, connected):-true.
documentation(crosses, "(crosses ?OBJ1 ?OBJ2) means that 
&%Object ?OBJ1 &%traverses Object ?OBJ2, without being &%connected 
to it."):-true.
subrelation(penetrates, traverses):-true.
subrelation(penetrates, 'meetsSpatially'):-true.
instance(penetrates, 'AsymmetricRelation'):-true.
instance(penetrates, 'IntransitiveRelation'):-true.
documentation(penetrates, "(penetrates ?OBJ1 ?OBJ2) means that 
?OBJ1 is &%connected to ?OBJ2 along at least one whole dimension (length, 
width or depth)."):-true.
instance('WhereFn', 'BinaryFunction'):-true.
instance('WhereFn', 'SpatialRelation'):-true.
domain('WhereFn', 1, 'Physical'):-true.
domain('WhereFn', 2, 'TimePoint'):-true.
range('WhereFn', 'Region'):-true.
'relatedInternalConcept'('WhereFn', 'WhenFn'):-true.
documentation('WhereFn', "Maps an &%Object and a &%TimePoint at which the 
&%Object exists to the &%Region where the &%Object existed at that 
&%TimePoint."):-true.
<=>([equal, ['WhereFn', '?THING', '?TIME'], '?REGION'], ['holdsDuring', '?TIME', ['exactlyLocated', '?THING', '?REGION']]):-true.
instance(possesses, 'BinaryPredicate'):-true.
instance(possesses, 'AsymmetricRelation'):-true.
domain(possesses, 1, 'Agent'):-true.
domain(possesses, 2, 'Object'):-true.
documentation(possesses, "&%Relation that holds between an &%Agent and 
an &%Object when the &%Agent has ownership of the &%Object."):-true.
=>([possesses, '?PERSON', '?OBJ'], [exists, ['?TYPE'], [and, ['holdsRight', '?PERSON', '?TYPE'], [forall, ['?PROCESS'], [=>, [instance, '?PROCESS', '?TYPE'], [patient, '?PROCESS', '?OBJ']]]]]):-true.
=>([and, [instance, '?TIME', 'TimePosition'], ['holdsDuring', '?TIME', [possesses, '?AGENT1', '?OBJ']], ['holdsDuring', '?TIME', [possesses, '?AGENT2', '?OBJ']]], [equal, '?AGENT1', '?AGENT2']):-true.
instance('PropertyFn', 'UnaryFunction'):-true.
domain('PropertyFn', 1, 'Agent'):-true.
range('PropertyFn', 'Set'):-true.
documentation('PropertyFn', "A &%UnaryFunction that maps an &%Agent to the &%Set of &%Property owned by the &%Agent."):-true.
<=>([instance, '?OBJ', ['PropertyFn', '?PERSON']], [possesses, '?PERSON', '?OBJ']):-true.
instance(precondition, 'BinaryPredicate'):-true.
instance(precondition, 'AsymmetricRelation'):-true.
instance(precondition, 'TransitiveRelation'):-true.
'domainSubclass'(precondition, 1, 'Process'):-true.
'domainSubclass'(precondition, 2, 'Process'):-true.
documentation(precondition, "A very general &%Predicate.  (&%precondition 
?PROC1 ?PROC2) means that ?PROC2 can exist or be true only if 
?PROC1 exists or is true.  At some point, this &%Predicate should 
probably be broken up into more specific &%Predicates with more 
restrictive &%domain restrictions."):-true.
=>([precondition, '?PROC1', '?PROC2'], [=>, [exists, ['?INST2'], [instance, '?INST2', '?PROC2']], [exists, ['?INST1'], [instance, '?INST1', '?PROC1']]]):-true.
instance(inhibits, 'BinaryPredicate'):-true.
instance(inhibits, 'IrreflexiveRelation'):-true.
'domainSubclass'(inhibits, 1, 'Process'):-true.
'domainSubclass'(inhibits, 2, 'Process'):-true.
documentation(inhibits, "A very general &%Predicate.  (&%inhibits 
?PROC1 ?PROC2) means that the &%Process ?PROC1 inhibits or hinders 
the occurrence of the &%Process ?PROC2.  For example, obstructing an 
object inhibits moving it.  Note that this is a relation between types 
of &%Processes, not between instances."):-true.
=>([inhibits, '?PROC1', '?PROC2'], [forall, ['?TIME', '?PLACE'], ['decreasesLikelihood', ['holdsDuring', '?TIME', [exists, ['?INST1'], [and, [instance, '?INST1', '?PROC1'], [located, '?INST1', '?PLACE']]]], ['holdsDuring', '?TIME', [exists, ['?INST2'], [and, [instance, '?INST2', '?PROC2'], [located, '?INST2', '?PLACE']]]]]]):-true.
instance(prevents, 'BinaryPredicate'):-true.
instance(prevents, 'IrreflexiveRelation'):-true.
'domainSubclass'(prevents, 1, 'Process'):-true.
'domainSubclass'(prevents, 2, 'Process'):-true.
'relatedInternalConcept'(prevents, inhibits):-true.
documentation(prevents, "A very general &%Predicate.  (&%prevents ?PROC1 
?PROC2) means that ?PROC1 prevents the occurrence of ?PROC2.  In other 
words, if ?PROC1 is occurring in a particular time and place, ?PROC2 
cannot occur at the same time and place.  For example, innoculating 
prevents contracting disease.  Note that this is a relation between types 
of &%Processes, not between instances."):-true.
=>([prevents, '?PROC1', '?PROC2'], [forall, ['?TIME', '?PLACE'], [=>, ['holdsDuring', '?TIME', [exists, ['?INST1'], [and, [instance, '?INST1', '?PROC1'], [located, '?INST1', '?PLACE']]]], [not, ['holdsDuring', '?TIME', [exists, ['?INST2'], [and, [instance, '?INST2', '?PROC2'], [located, '?INST2', '?PLACE']]]]]]]):-true.
instance(refers, 'BinaryPredicate'):-true.
domain(refers, 1, 'Physical'):-true.
domain(refers, 2, 'Entity'):-true.
documentation(refers, "(&%refers ?OBJ1 ?OBJ2) means that ?OBJ1 
mentions or includes a reference to ?OBJ2. Note that &%refers is 
more general in meaning than &%represents, because presumably something 
can represent something else only if it refers to this other thing.  
For example, an article whose topic is a recent change in the price of 
oil may refer to many other things, e.g. the general state of the economy, 
the weather in California, the prospect of global warming, the options 
for alternative energy sources, the stock prices of various oil companies, 
etc."):-true.
instance(represents, 'BinaryPredicate'):-true.
domain(represents, 1, 'Physical'):-true.
domain(represents, 2, 'Entity'):-true.
subrelation(represents, refers):-true.
documentation(represents, "A very general semiotics &%Predicate.  
(&%represents ?THING ?ENTITY) means that ?THING in some way indicates, 
expresses, connotes, pictures, describes, etc. ?ENTITY.  The &%Predicates 
&%containsInformation and &%realization are subrelations of &%represents.  
Note that &%represents is a subrelation of &%refers, since something can 
represent something else only if it refers to this other thing."):-true.
instance('representsForAgent', 'TernaryPredicate'):-true.
domain('representsForAgent', 1, 'Physical'):-true.
domain('representsForAgent', 2, 'Entity'):-true.
domain('representsForAgent', 3, 'Agent'):-true.
documentation('representsForAgent', "A very general predicate.  
(&%representsForAgent ?THING ?ENTITY ?AGENT) means that the ?AGENT 
chooses to use the &%instance of &%Physical ?THING to 'stand for' 
?ENTITY."):-true.
=>(['representsForAgent', '?REP', '?ENTITY', '?AGENT'], [represents, '?REP', '?ENTITY']):-true.
instance('representsInLanguage', 'TernaryPredicate'):-true.
domain('representsInLanguage', 1, 'Physical'):-true.
domain('representsInLanguage', 2, 'Entity'):-true.
domain('representsInLanguage', 3, 'Language'):-true.
documentation('representsInLanguage', "A very general predicate.  
(&%representsInLanguage ?THING ?ENTITY ?LANGUAGE) means that the 
&%instance of &%Physical ?THING stands for ?ENTITY in the language 
?LANGUAGE."):-true.
=>(['representsInLanguage', '?REP', '?ENTITY', '?LANGUAGE'], [exists, ['?AGENT'], ['representsForAgent', '?REP', '?ENTITY', '?AGENT']]):-true.
subrelation('equivalentContentClass', 'subsumesContentClass'):-true.
instance('equivalentContentClass', 'EquivalenceRelation'):-true.
'domainSubclass'('equivalentContentClass', 1, 'ContentBearingObject'):-true.
'domainSubclass'('equivalentContentClass', 2, 'ContentBearingObject'):-true.
documentation('equivalentContentClass', "A &%BinaryPredicate that relates two 
subclasses of &%ContentBearingObject.  (&%equivalentContentClass ?CLASS1 
?CLASS2) means that the content expressed by each instance of ?CLASS1 is 
also expressed by each instance of ?CLASS2, and vice versa.  An example 
would be the relationship between English and Russian editions of Agatha 
Christie's 'Murder on the Orient Express'.  Note that 
(&%equivalentContentClass ?CLASS1 ?CLASS2) implies (&%subsumesContentClass 
?CLASS1 ?CLASS2) and (&%subsumesContentClass ?CLASS2 ?CLASS1)."):-true.
<=>([and, ['subsumesContentClass', '?CLASS1', '?CLASS2'], ['subsumesContentClass', '?CLASS2', '?CLASS1']], ['equivalentContentClass', '?CLASS1', '?CLASS2']):-true.
instance('subsumesContentClass', 'BinaryPredicate'):-true.
instance('subsumesContentClass', 'PartialOrderingRelation'):-true.
'domainSubclass'('subsumesContentClass', 1, 'ContentBearingObject'):-true.
'domainSubclass'('subsumesContentClass', 2, 'ContentBearingObject'):-true.
documentation('subsumesContentClass', "A &%BinaryPredicate that relates two 
subclasses of &%ContentBearingObject.  (&%subsumesContentClass ?CLASS1 
?CLASS2) means that the content expressed by each instance of ?CLASS2 is 
also expressed by each instance of ?CLASS1.  Examples include the 
relationship between a poem and one of its stanzas or between a book and 
one of its chapters.  Note that this is a relation between subclasses of 
&%ContentBearingObject, rather than instances.  If one wants to relate 
instances, the &%Predicate &%subsumesContentInstance can be used.  Note 
that &%subsumesContentClass is needed in many cases.  Consider, for 
example, the relation between the King James edition of the Bible and its 
Book of Genesis.  This relation holds for every copy of this edition and 
not just for a single instance."):-true.
<=>(['subsumesContentClass', '?CLASS1', '?CLASS2'], [forall, ['?INFO', '?OBJ1', '?OBJ2'], [=>, [and, [instance, '?OBJ1', '?CLASS1'], [instance, '?OBJ2', '?CLASS2'], ['containsInformation', '?OBJ2', '?INFO']], ['containsInformation', '?OBJ1', '?INFO']]]):-true.
subrelation('equivalentContentInstance', 'subsumesContentInstance'):-true.
instance('equivalentContentInstance', 'EquivalenceRelation'):-true.
domain('equivalentContentInstance', 1, 'ContentBearingObject'):-true.
domain('equivalentContentInstance', 2, 'ContentBearingObject'):-true.
'relatedInternalConcept'('equivalentContentInstance', 'equivalentContentClass'):-true.
documentation('equivalentContentInstance', "A &%BinaryPredicate relating two 
instances of &%ContentBearingObject.  (&%equivalentContentInstance 
?OBJ1 ?OBJ2) means that the content expressed by ?OBJ1 is identical to 
the content expressed by ?OBJ2.  An example would be the relationship 
between a handwritten draft of a letter to one's lawyer and a typed 
copy of the same letter.  Note that (&%equivalentContentInstance ?OBJ1 
?OBJ2) implies (&%subsumesContentInstance ?OBJ1 ?OBJ2) and 
(&%subsumesContentInstance ?OBJ2 ?OBJ2)."):-true.
<=>([and, ['subsumesContentInstance', '?OBJ1', '?OBJ2'], ['subsumesContentInstance', '?OBJ2', '?OBJ1']], ['equivalentContentInstance', '?OBJ1', '?OBJ2']):-true.
instance('subsumesContentInstance', 'BinaryPredicate'):-true.
instance('subsumesContentInstance', 'PartialOrderingRelation'):-true.
domain('subsumesContentInstance', 1, 'ContentBearingObject'):-true.
domain('subsumesContentInstance', 2, 'ContentBearingObject'):-true.
'relatedInternalConcept'('subsumesContentInstance', 'subsumesContentClass'):-true.
documentation('subsumesContentInstance', "A &%BinaryPredicate relating two 
instances of &%ContentBearingObject.  (&%subsumesContentInstance ?OBJ1 ?OBJ2) 
means that the content expressed by ?OBJ2 is part of the content expressed 
by ?OBJ1.  An example is the relationship between a handwritten poem and 
one of its stanzas.  Note that this is a relation between instances, 
rather than &%Classes.  If one wants to assert a content relationship 
between &%Classes, e.g. between the version of an intellectual work and a 
part of that work, the relation &%subsumesContentClass should be used."):-true.
<=>(['subsumesContentInstance', '?OBJ1', '?OBJ2'], [forall, ['?INFO'], [=>, ['containsInformation', '?OBJ2', '?INFO'], ['containsInformation', '?OBJ1', '?INFO']]]):-true.
subrelation(realization, represents):-true.
instance(realization, 'AsymmetricRelation'):-true.
domain(realization, 1, 'Process'):-true.
domain(realization, 2, 'Proposition'):-true.
'relatedInternalConcept'(realization, 'equivalentContentInstance'):-true.
'relatedInternalConcept'(realization, 'containsInformation'):-true.
documentation(realization, "A subrelation of &%represents.  
(&%realization ?PROCESS ?PROP) means that ?PROCESS is a Process which 
expresses the content of ?PROP. Examples include a particular musical 
performance, which realizes the content of a musical score, or the 
reading of a poem."):-true.
=>([realization, '?PROCESS', '?PROP'], [exists, ['?OBJ'], [and, [instance, '?OBJ', 'ContentBearingObject'], ['containsInformation', '?OBJ', '?PROP']]]):-true.
instance('expressedInLanguage', 'BinaryPredicate'):-true.
instance('expressedInLanguage', 'AsymmetricRelation'):-true.
domain('expressedInLanguage', 1, 'LinguisticExpression'):-true.
domain('expressedInLanguage', 2, 'Language'):-true.
documentation('expressedInLanguage', "(&%expressedInLanguage ?EXPRESS ?LANG) 
means that ?EXPRESS is expressed in &%Language ?LANG."):-true.
<=>(['expressedInLanguage', '?EXPRESS', '?LANGUAGE'], [exists, ['?PROP'], ['representsInLanguage', '?EXPRESS', '?PROP', '?LANGUAGE']]):-true.
instance('subProposition', 'BinaryPredicate'):-true.
instance('subProposition', 'TransitiveRelation'):-true.
instance('subProposition', 'IrreflexiveRelation'):-true.
domain('subProposition', 1, 'Proposition'):-true.
domain('subProposition', 2, 'Proposition'):-true.
documentation('subProposition', "(&%subProposition ?PROP1 ?PROP2) means that 
?PROP1 is a &%Proposition which is a proper part of the &%Proposition ?PROP2.  
In other words, &%subProposition is the analogue of &%properPart for chunks 
of abstract content."):-true.
=>(['subProposition', '?PROP1', '?PROP2'], [forall, ['?OBJ1', '?OBJ2'], [=>, [and, ['containsInformation', '?OBJ1', '?PROP1'], ['containsInformation', '?OBJ2', '?PROP2']], ['subsumesContentInstance', '?OBJ2', '?OBJ1']]]):-true.
subrelation('subPlan', 'subProposition'):-true.
instance('subPlan', 'TransitiveRelation'):-true.
instance('subPlan', 'IrreflexiveRelation'):-true.
domain('subPlan', 1, 'Plan'):-true.
domain('subPlan', 2, 'Plan'):-true.
documentation('subPlan', "(&%subPlan ?PLAN1 ?PLAN2) means that ?PLAN1 
is a &%Plan which is a proper part of ?PLAN2.  This relation is generally 
used to relate a supporting &%Plan to the overall &%Plan in a particular 
context."):-true.
instance(uses, 'BinaryPredicate'):-true.
instance(uses, 'AsymmetricRelation'):-true.
domain(uses, 1, 'Object'):-true.
domain(uses, 2, 'Agent'):-true.
documentation(uses, "(&%uses ?OBJECT AGENT) means that ?OBJECT is used by 
?AGENT as an instrument in an unspecified &%Process.  This &%Predicate, 
as its corresponding axiom indicates, is a composition of the &%CaseRoles 
&%agent and &%instrument."):-true.
=>([uses, '?OBJ', '?AGENT'], [exists, ['?PROC'], [and, [agent, '?PROC', '?AGENT'], [instrument, '?PROC', '?OBJ']]]):-true.
instance('MultiplicationFn', 'BinaryFunction'):-true.
instance('MultiplicationFn', 'AssociativeFunction'):-true.
instance('MultiplicationFn', 'CommutativeFunction'):-true.
instance('MultiplicationFn', 'RelationExtendedToQuantities'):-true.
domain('MultiplicationFn', 1, 'Quantity'):-true.
domain('MultiplicationFn', 2, 'Quantity'):-true.
range('MultiplicationFn', 'Quantity'):-true.
documentation('MultiplicationFn', "If ?NUMBER1 and ?NUMBER2 are &%Numbers, 
then (&%MultiplicationFn ?NUMBER1 ?NUMBER2) is the arithmetical product 
of these numbers."):-true.
instance('AdditionFn', 'BinaryFunction'):-true.
instance('AdditionFn', 'AssociativeFunction'):-true.
instance('AdditionFn', 'CommutativeFunction'):-true.
instance('AdditionFn', 'RelationExtendedToQuantities'):-true.
domain('AdditionFn', 1, 'Quantity'):-true.
domain('AdditionFn', 2, 'Quantity'):-true.
range('AdditionFn', 'Quantity'):-true.
documentation('AdditionFn', "If ?NUMBER1 and ?NUMBER2 are &%Numbers, then 
(&%AdditionFn ?NUMBER1 ?NUMBER2) is the arithmetical sum of these 
numbers."):-true.
instance('SubtractionFn', 'BinaryFunction'):-true.
instance('SubtractionFn', 'AssociativeFunction'):-true.
instance('SubtractionFn', 'RelationExtendedToQuantities'):-true.
domain('SubtractionFn', 1, 'Quantity'):-true.
domain('SubtractionFn', 2, 'Quantity'):-true.
range('SubtractionFn', 'Quantity'):-true.
documentation('SubtractionFn', "If ?NUMBER1 and ?NUMBER2 are &%Numbers, 
then (&%SubtractionFn ?NUMBER1 ?NUMBER2) is the arithmetical difference 
between ?NUMBER1 and ?NUMBER2, i.e. ?NUMBER1 minus ?NUMBER2.  An 
exception occurs when ?NUMBER1 is equal to 0, in which case 
(&%SubtractionFn ?NUMBER1 ?NUMBER2) is the negation of ?NUMBER2."):-true.
instance('DivisionFn', 'BinaryFunction'):-true.
instance('DivisionFn', 'AssociativeFunction'):-true.
instance('DivisionFn', 'RelationExtendedToQuantities'):-true.
domain('DivisionFn', 1, 'Quantity'):-true.
domain('DivisionFn', 2, 'Quantity'):-true.
range('DivisionFn', 'Quantity'):-true.
documentation('DivisionFn', "If ?NUMBER1 and ?NUMBER2 are &%Numbers, then 
(&%DivisionFn ?NUMBER1 ?NUMBER2) is the result of dividing ?NUMBER1 by 
?NUMBER2.  An exception occurs when ?NUMBER1 = 1, in which case 
(&%DivisionFn ?NUMBER1 ?NUMBER2) is the reciprocal of ?NUMBER2."):-true.
=>([instance, '?NUMBER', 'RationalNumber'], [exists, ['?INT1', '?INT2'], [and, [instance, '?INT1', 'Integer'], [instance, '?INT2', 'Integer'], [equal, '?NUMBER', ['DivisionFn', '?INT1', '?INT2']]]]):-true.
instance('AbsoluteValueFn', 'UnaryFunction'):-true.
domain('AbsoluteValueFn', 1, 'RealNumber'):-true.
range('AbsoluteValueFn', 'PositiveRealNumber'):-true.
documentation('AbsoluteValueFn', "The value of (&%AbsoluteValueFn ?NUMBER) 
is the absolute value of the &%RealNumber ?NUMBER."):-true.
<=>([equal, ['AbsoluteValueFn', '?NUMBER1'], '?NUMBER2'], [or, [and, [instance, '?NUMBER1', 'PositiveInteger'], [equal, '?NUMBER1', '?NUMBER2']], [and, [instance, '?NUMBER1', 'NegativeInteger'], [equal, '?NUMBER2', ['SubtractionFn', 0, '?NUMBER1']]]]):-true.
instance('ArcCosineFn', 'UnaryFunction'):-true.
inverse('ArcCosineFn', 'CosineFn'):-true.
domain('ArcCosineFn', 1, 'RealNumber'):-true.
range('ArcCosineFn', 'PlaneAngleMeasure'):-true.
documentation('ArcCosineFn', "(&%ArcCosineFn ?NUMBER) returns the arc 
cosine of the &%RealNumber ?NUMBER.  It is the &%inverse of &%CosineFn."):-true.
instance('ArcSineFn', 'UnaryFunction'):-true.
inverse('ArcSineFn', 'SineFn'):-true.
domain('ArcSineFn', 1, 'RealNumber'):-true.
range('ArcSineFn', 'PlaneAngleMeasure'):-true.
documentation('ArcSineFn', "(&%ArcSineFn ?NUMBER) returns the arc sine of 
the &%RealNumber ?NUMBER.  It is the &%inverse of &%SineFn."):-true.
instance('ArcTangentFn', 'UnaryFunction'):-true.
inverse('ArcTangentFn', 'TangentFn'):-true.
domain('ArcTangentFn', 1, 'RealNumber'):-true.
range('ArcTangentFn', 'PlaneAngleMeasure'):-true.
documentation('ArcTangentFn', "(&%ArcTangentFn ?NUMBER) returns the arc 
tangent of the &%RealNumber ?NUMBER.  It is the &%inverse of &%TangentFn."):-true.
instance('CeilingFn', 'UnaryFunction'):-true.
domain('CeilingFn', 1, 'RealNumber'):-true.
range('CeilingFn', 'Integer'):-true.
documentation('CeilingFn', "(&%CeilingFn ?NUMBER) returns the smallest 
&%Integer greater than or equal to the &%RealNumber ?NUMBER."):-true.
=>([equal, ['CeilingFn', '?NUMBER'], '?INT'], [not, [exists, ['?OTHERINT'], [and, [instance, '?OTHERINT', 'Integer'], ['greaterThanOrEqualTo', '?OTHERINT', '?NUMBER'], ['lessThan', '?OTHERINT', '?INT']]]]):-true.
instance('CosineFn', 'UnaryFunction'):-true.
domain('CosineFn', 1, 'PlaneAngleMeasure'):-true.
range('CosineFn', 'RealNumber'):-true.
documentation('CosineFn', "(&%CosineFn ?DEGREE) returns the cosine of the 
&%PlaneAngleMeasure ?DEGREE.  The cosine of ?DEGREE is the ratio of the 
side next to ?DEGREE to the hypotenuse in a right-angled triangle."):-true.
instance('DenominatorFn', 'UnaryFunction'):-true.
domain('DenominatorFn', 1, 'RealNumber'):-true.
range('DenominatorFn', 'Integer'):-true.
documentation('DenominatorFn', "(&%DenominatorFn ?NUMBER) returns the 
denominator of the canonical reduced form of the &%RealNumber ?NUMBER."):-true.
instance('ExponentiationFn', 'BinaryFunction'):-true.
instance('ExponentiationFn', 'RelationExtendedToQuantities'):-true.
domain('ExponentiationFn', 1, 'Quantity'):-true.
domain('ExponentiationFn', 2, 'Integer'):-true.
range('ExponentiationFn', 'Quantity'):-true.
documentation('ExponentiationFn', "(&%ExponentiationFn ?NUMBER ?INT) returns 
the &%RealNumber ?NUMBER raised to the power of the &%Integer ?INT."):-true.
instance('FloorFn', 'UnaryFunction'):-true.
domain('FloorFn', 1, 'RealNumber'):-true.
range('FloorFn', 'Integer'):-true.
documentation('FloorFn', "(&%FloorFn ?NUMBER) returns the largest &%Integer 
less than or equal to the &%RealNumber ?NUMBER."):-true.
=>([equal, ['FloorFn', '?NUMBER'], '?INT'], [not, [exists, ['?OTHERINT'], [and, [instance, '?OTHERINT', 'Integer'], ['lessThanOrEqualTo', '?OTHERINT', '?NUMBER'], ['greaterThan', '?OTHERINT', '?INT']]]]):-true.
instance('GreatestCommonDivisorFn', 'Function'):-true.
instance('GreatestCommonDivisorFn', 'VariableArityRelation'):-true.
range('GreatestCommonDivisorFn', 'Integer'):-true.
documentation('GreatestCommonDivisorFn', "(&%GreatestCommonDivisorFn 
?NUMBER1 ?NUMBER2 ... ?NUMBER) returns the greatest common divisor of 
?NUMBER1 through ?NUMBER."):-true.
=>([equal, ['GreatestCommondDivisorFn', '@ROW'], '?NUMBER'], [forall, ['?ELEMENT'], [=>, ['inList', '?ELEMENT', ['ListFn', '@ROW']], [equal, ['RemainderFn', '?ELEMENT', '?NUMBER'], 0]]]):-true.
=>([equal, ['GreatestCommondDivisorFn', '@ROW'], '?NUMBER'], [not, [exists, ['?GREATER'], [and, ['greaterThan', '?GREATER', '?NUMBER'], [forall, ['?ELEMENT'], [=>, ['inList', '?ELEMENT', ['ListFn', '@ROW']], [equal, ['RemainderFn', '?ELEMENT', '?GREATER'], 0]]]]]]):-true.
instance('ImaginaryPartFn', 'UnaryFunction'):-true.
domain('ImaginaryPartFn', 1, 'ComplexNumber'):-true.
range('ImaginaryPartFn', 'ImaginaryNumber'):-true.
documentation('ImaginaryPartFn', "(&%ImaginaryPartFn ?NUMBER) returns 
the part of ?NUMBER that has the square root of -1 as its factor."):-true.
=>([instance, '?NUMBER', 'ComplexNumber'], [exists, ['?PART1', '?PART2'], [and, [equal, '?PART1', ['RealNumberFn', '?NUMBER']], [equal, '?PART2', ['ImaginaryPartFn', '?NUMBER']]]]):-true.
instance('IntegerSquareRootFn', 'UnaryFunction'):-true.
domain('IntegerSquareRootFn', 1, 'RealNumber'):-true.
range('IntegerSquareRootFn', 'NonnegativeInteger'):-true.
documentation('IntegerSquareRootFn', "(&%IntegerSquareRootFn ?NUMBER) 
returns the integer square root of ?NUMBER."):-true.
instance('LeastCommonMultipleFn', 'Function'):-true.
instance('LeastCommonMultipleFn', 'VariableArityRelation'):-true.
range('LeastCommonMultipleFn', 'Integer'):-true.
documentation('LeastCommonMultipleFn', "(&%LeastCommonMultipleFn 
?NUMBER1 ?NUMBER2 ... ?NUMBER) returns the least common multiple of 
?NUMBER1 through ?NUMBER."):-true.
=>([equal, ['LeastCommonMultipleFn', '@ROW'], '?NUMBER'], [forall, ['?ELEMENT'], [=>, ['inList', '?ELEMENT', ['ListFn', '@ROW']], [equal, ['RemainderFn', '?NUMBER', '?ELEMENT'], 0]]]):-true.
=>([equal, ['LeastCommonMultipleFn', '@ROW'], '?NUMBER'], [not, [exists, ['?LESS'], [and, ['lessThan', '?LESS', '?NUMBER'], [forall, ['?ELEMENT'], [=>, ['inList', '?ELEMENT', ['ListFn', '@ROW']], [equal, ['RemainderFn', '?LESS', '?ELEMENT'], 0]]]]]]):-true.
instance('LogFn', 'BinaryFunction'):-true.
domain('LogFn', 1, 'RealNumber'):-true.
domain('LogFn', 2, 'PositiveInteger'):-true.
range('LogFn', 'RealNumber'):-true.
documentation('LogFn', "(LogFn ?NUMBER ?INT) returns the logarithm of the 
&%RealNumber ?NUMBER in the base denoted by the &%Integer ?INT."):-true.
instance('MaxFn', 'BinaryFunction'):-true.
instance('MaxFn', 'AssociativeFunction'):-true.
instance('MaxFn', 'CommutativeFunction'):-true.
instance('MaxFn', 'RelationExtendedToQuantities'):-true.
domain('MaxFn', 1, 'Quantity'):-true.
domain('MaxFn', 2, 'Quantity'):-true.
range('MaxFn', 'Quantity'):-true.
documentation('MaxFn', "(&%MaxFn ?NUMBER1 ?NUMBER2) is the largest of 
?NUMBER1 and ?NUMBER2.  In cases where ?NUMBER1 is equal to ?NUMBER2, 
&%MaxFn returns one of its arguments."):-true.
=>([equal, ['MaxFn', '?NUMBER1', '?NUMBER2'], '?NUMBER'], [or, [and, [equal, '?NUMBER', '?NUMBER1'], ['greaterThan', '?NUMBER1', '?NUMBER2']], [and, [equal, '?NUMBER', '?NUMBER2'], ['greaterThan', '?NUMBER2', '?NUMBER1']], [and, [equal, '?NUMBER', '?NUMBER1'], [equal, '?NUMBER', '?NUMBER2']]]):-true.
instance('MinFn', 'BinaryFunction'):-true.
instance('MinFn', 'AssociativeFunction'):-true.
instance('MinFn', 'CommutativeFunction'):-true.
instance('MinFn', 'RelationExtendedToQuantities'):-true.
domain('MinFn', 1, 'Quantity'):-true.
domain('MinFn', 2, 'Quantity'):-true.
range('MinFn', 'Quantity'):-true.
documentation('MinFn', "(&%MinFn ?NUMBER1 ?NUMBER2) is the smallest of 
?NUMBER1 and ?NUMBER2.  In cases where ?NUMBER1 is equal to ?NUMBER2, 
&%MinFn returns one of its arguments."):-true.
=>([equal, ['MinFn', '?NUMBER1', '?NUMBER2'], '?NUMBER'], [or, [and, [equal, '?NUMBER', '?NUMBER1'], ['lessThan', '?NUMBER1', '?NUMBER2']], [and, [equal, '?NUMBER', '?NUMBER2'], ['lessThan', '?NUMBER2', '?NUMBER1']], [and, [equal, '?NUMBER', '?NUMBER1'], [equal, '?NUMBER', '?NUMBER2']]]):-true.
instance('NumeratorFn', 'UnaryFunction'):-true.
domain('NumeratorFn', 1, 'RealNumber'):-true.
range('NumeratorFn', 'Integer'):-true.
documentation('NumeratorFn', "(&%NumeratorFn ?NUMBER) returns the numerator 
of the canonical reduced form ?NUMBER."):-true.
instance('Pi', 'PositiveRealNumber'):-true.
documentation('Pi', "&%Pi is the &%RealNumber that 
is the ratio of the perimeter of a circle to its diameter.  It is 
approximately equal to 3.141592653589793."):-true.
instance('NumberE', 'PositiveRealNumber'):-true.
documentation('NumberE', "&%NumberE is the &%RealNumber that is the base for 
natural logarithms.  It is approximately equal to 2.718282."):-true.
instance('RationalNumberFn', 'UnaryFunction'):-true.
domain('RationalNumberFn', 1, 'Number'):-true.
range('RationalNumberFn', 'RationalNumber'):-true.
documentation('RationalNumberFn', "(&%RationalNumberFn ?NUMBER) returns 
the rational representation of ?NUMBER."):-true.
instance('RealNumberFn', 'UnaryFunction'):-true.
domain('RealNumberFn', 1, 'Number'):-true.
range('RealNumberFn', 'RealNumber'):-true.
documentation('RealNumberFn', "(RealNumberFn ?NUMBER) returns the part of 
?NUMBER that is a &%RealNumber."):-true.
instance('ReciprocalFn', 'UnaryFunction'):-true.
instance('ReciprocalFn', 'RelationExtendedToQuantities'):-true.
domain('ReciprocalFn', 1, 'Quantity'):-true.
range('ReciprocalFn', 'Quantity'):-true.
documentation('ReciprocalFn', "(ReciprocalFn ?NUMBER) is the reciprocal 
element of ?NUMBER with respect to the multiplication operator 
(&%MultiplicationFn), i.e. 1/?NUMBER.  Not all numbers have a reciprocal 
element.  For example the number 0 does not.  If a number ?NUMBER has a 
reciprocal ?RECIP, then the product of ?NUMBER and ?RECIP will be 
1, e.g. 3*1/3 = 1.  The reciprocal of an element is &%equal to 
applying the &%ExponentiationFn function to the element to the power 
-1."):-true.
equal(['ReciprocalFn', '?NUMBER'], ['ExponentiationFn', '?NUMBER', -1]):-true.
equal(1, ['MultiplicationFn', '?NUMBER', ['ReciprocalFn', '?NUMBER']]):-true.
instance('RemainderFn', 'BinaryFunction'):-true.
instance('RemainderFn', 'RelationExtendedToQuantities'):-true.
domain('RemainderFn', 1, 'Quantity'):-true.
domain('RemainderFn', 2, 'Quantity'):-true.
range('RemainderFn', 'Quantity'):-true.
documentation('RemainderFn', "(RemainderFn ?NUMBER ?DIVISOR) is the 
remainder of the number ?NUMBER divided by the number ?DIVISOR.  
The result has the same sign as ?DIVISOR."):-true.
<=>([equal, ['RemainderFn', '?NUMBER1', '?NUMBER2'], '?NUMBER'], [equal, ['AdditionFn', ['MultiplicationFn', ['FloorFn', ['DivisionFn', '?NUMBER1', '?NUMBER2']], '?NUMBER2'], '?NUMBER'], '?NUMBER1']):-true.
=>([equal, ['RemainderFn', '?NUMBER1', '?NUMBER2'], '?NUMBER'], [equal, ['SignumFn', '?NUMBER2'], ['SignumFn', '?NUMBER']]):-true.
=>([instance, '?NUMBER', 'EvenInteger'], [equal, ['RemainderFn', '?NUMBER', 2], 0]):-true.
=>([instance, '?NUMBER', 'OddInteger'], [equal, ['RemainderFn', '?NUMBER', 2], 1]):-true.
=>([instance, '?PRIME', 'PrimeNumber'], [forall, ['?NUMBER'], [=>, [equal, ['RemainderFn', '?PRIME', '?NUMBER'], 0], [or, [equal, '?NUMBER', 1], [equal, '?NUMBER', '?PRIME']]]]):-true.
instance('RoundFn', 'UnaryFunction'):-true.
instance('RoundFn', 'RelationExtendedToQuantities'):-true.
domain('RoundFn', 1, 'Quantity'):-true.
range('RoundFn', 'Quantity'):-true.
documentation('RoundFn', "(&%RoundFn ?NUMBER) is the &%Integer closest 
to ?NUMBER on the number line.  If ?NUMBER is halfway between two 
&%Integers (for example 3.5), it denotes the larger &%Integer."):-true.
=>([equal, ['RoundFn', '?NUMBER1'], '?NUMBER2'], [or, [=>, ['lessThan', ['SubtractionFn', '?NUMBER1', ['FloorFn', '?NUMBER1']], 0.5], [equal, '?NUMBER2', ['FloorFn', '?NUMBER1']]], [=>, ['greaterThanOrEqualTo', ['SubtractionFn', '?NUMBER1', ['FloorFn', '?NUMBER1']], 0.5], [equal, '?NUMBER2', ['CeilingFn', '?NUMBER1']]]]):-true.
instance('SignumFn', 'UnaryFunction'):-true.
domain('SignumFn', 1, 'RealNumber'):-true.
range('SignumFn', 'Integer'):-true.
documentation('SignumFn', "(SignumFn ?NUMBER) denotes the sign of ?NUMBER.  
This is one of the following values:  -1, 1, or 0."):-true.
=>([instance, '?NUMBER', 'NonnegativeRealNumber'], [or, [equal, ['SignumFn', '?NUMBER'], 1], [equal, ['SignumFn', '?NUMBER'], 0]]):-true.
=>([instance, '?NUMBER', 'PositiveRealNumber'], [equal, ['SignumFn', '?NUMBER'], 1]):-true.
=>([instance, '?NUMBER', 'NegativeRealNumber'], [equal, ['SignumFn', '?NUMBER'], -1]):-true.
instance('SineFn', 'UnaryFunction'):-true.
domain('SineFn', 1, 'PlaneAngleMeasure'):-true.
range('SineFn', 'RealNumber'):-true.
documentation('SineFn', "(&%SineFn ?DEGREE) is the sine of the 
&%PlaneAngleMeasure ?DEGREE.  The sine of ?DEGREE is the ratio of the side 
opposite ?DEGREE to the hypotenuse in a right-angled triangle."):-true.
instance('SquareRootFn', 'UnaryFunction'):-true.
domain('SquareRootFn', 1, 'RealNumber'):-true.
range('SquareRootFn', 'Number'):-true.
documentation('SquareRootFn', "(SquareRootFn ?NUMBER) is the principal 
square root of ?NUMBER."):-true.
=>([equal, ['SquareRootFn', '?NUMBER1'], '?NUMBER2'], [equal, ['MultiplicationFn', '?NUMBER2', '?NUMBER2'], '?NUMBER1']):-true.
instance('TangentFn', 'UnaryFunction'):-true.
domain('TangentFn', 1, 'PlaneAngleMeasure'):-true.
range('TangentFn', 'RealNumber'):-true.
documentation('TangentFn', "(&%TangentFn ?DEGREE) is the tangent of the 
&%PlaneAngleMeasure ?DEGREE.  The tangent of ?DEGREE is the ratio of 
the side opposite ?DEGREE to the side next to ?DEGREE in a right-angled 
triangle."):-true.
equal(['TangentFn', '?DEGREE'], ['DivisionFn', ['SineFn', '?DEGREE'], ['CosineFn', '?DEGREE']]):-true.
instance('IdentityFn', 'UnaryFunction'):-true.
domain('IdentityFn', 1, 'Entity'):-true.
range('IdentityFn', 'Entity'):-true.
documentation('IdentityFn', "The value of the identity function is 
just its argument."):-true.
equal(['IdentityFn', '?INST'], '?INST'):-true.
instance('identityElement', 'BinaryPredicate'):-true.
instance('identityElement', 'AsymmetricRelation'):-true.
domain('identityElement', 1, 'BinaryFunction'):-true.
domain('identityElement', 2, 'Entity'):-true.
documentation('identityElement', "An object ?ID is the identity element 
for BinaryFunction ?FUNCTION just in case, for every instance ?INST, 
applying ?FUNCTION to ?INST and ?ID results in ?INST."):-true.
=>(['identityElement', '?FUNCTION', '?ID'], [forall, ['?INST'], [=>, [instance, '?INST', ['DomainFn', '?FUNCTION']], [equal, ['AssignmentFn', '?FUNCTION', '?ID', '?INST'], '?INST']]]):-true.
'identityElement'('MultiplicationFn', 1):-true.
'identityElement'('AdditionFn', 0):-true.
'identityElement'('SubtractionFn', 0):-true.
'identityElement'('DivisionFn', 1):-true.
instance('SuccessorFn', 'UnaryFunction'):-true.
domain('SuccessorFn', 1, 'Integer'):-true.
range('SuccessorFn', 'Integer'):-true.
documentation('SuccessorFn', "A &%UnaryFunction that maps an &%Integer to 
its successor, e.g. the successor of 5 is 6."):-true.
=>([equal, ['SuccessorFn', '?INT1'], ['SuccessorFn', '?INT2']], [equal, '?INT1', '?INT2']):-true.
=>([instance, '?INT', 'Integer'], ['lessThan', '?INT', ['SuccessorFn', '?INT']]):-true.
=>([and, [instance, '?INT1', 'Integer'], [instance, '?INT2', 'Integer']], [not, [and, ['lessThan', '?INT1', '?INT2'], ['lessThan', '?INT2', ['SuccessorFn', '?INT1']]]]):-true.
=>([instance, '?INT', 'Integer'], [equal, '?INT', ['SuccessorFn', ['PredecessorFn', '?INT']]]):-true.
=>([instance, '?INT', 'Integer'], [equal, '?INT', ['PredecessorFn', ['SuccessorFn', '?INT']]]):-true.
instance('PredecessorFn', 'UnaryFunction'):-true.
domain('PredecessorFn', 1, 'Integer'):-true.
range('PredecessorFn', 'Integer'):-true.
documentation('PredecessorFn', "A &%UnaryFunction that maps an &%Integer to 
its predecessor, e.g. the predecessor of 5 is 4."):-true.
=>([equal, ['PredecessorFn', '?INT1'], ['PredecessorFn', '?INT2']], [equal, '?INT1', '?INT2']):-true.
=>([instance, '?INT', 'Integer'], ['greaterThan', '?INT', ['PredecessorFn', '?INT']]):-true.
=>([and, [instance, '?INT1', 'Integer'], [instance, '?INT2', 'Integer']], [not, [and, ['lessThan', '?INT2', '?INT1'], ['lessThan', ['PredecessorFn', '?INT1'], '?INT2']]]):-true.
subrelation(subset, subclass):-true.
domain(subset, 1, 'Set'):-true.
domain(subset, 2, 'Set'):-true.
documentation(subset, "(subset ?SET1 ?SET2) is true just in case the 
&%elements of the &%Set ?SET1 are also &%elements of the &%Set ?SET2."):-true.
=>([subset, '?SUBSET', '?SET'], [forall, ['?ELEMENT'], [=>, [element, '?ELEMENT', '?SUBSET'], [element, '?ELEMENT', '?SET']]]):-true.
instance(element, 'BinaryPredicate'):-true.
instance(element, 'AsymmetricRelation'):-true.
instance(element, 'IntransitiveRelation'):-true.
subrelation(element, instance):-true.
domain(element, 1, 'Entity'):-true.
domain(element, 2, 'Set'):-true.
documentation(element, "(element ?ENTITY ?SET) is true just in case 
?ENTITY is contained in the &%Set ?SET.  An &%Entity can be an &%element 
of another &%Entity only if the latter is a &%Set."):-true.
=>([forall, ['?ELEMENT'], [<=>, [element, '?ELEMENT', '?SET1'], [element, '?ELEMENT', '?SET2']]], [equal, '?SET1', '?SET2']):-true.
instance('UnionFn', 'BinaryFunction'):-true.
domain('UnionFn', 1, 'SetOrClass'):-true.
domain('UnionFn', 2, 'SetOrClass'):-true.
range('UnionFn', 'SetOrClass'):-true.
documentation('UnionFn', "A &%BinaryFunction that maps two &%Classes to 
the union of these &%Classes.  An object is an instance of the union of 
two &%Classes just in case it is an instance of either &%SetOrClass."):-true.
<=>([instance, '?ENTITY', ['UnionFn', '?CLASS1', '?CLASS2']], [or, [instance, '?ENTITY', '?CLASS1'], [instance, '?ENTITY', '?CLASS2']]):-true.
instance('IntersectionFn', 'BinaryFunction'):-true.
domain('IntersectionFn', 1, 'SetOrClass'):-true.
domain('IntersectionFn', 2, 'SetOrClass'):-true.
range('IntersectionFn', 'SetOrClass'):-true.
documentation('IntersectionFn', "A &%BinaryFunction that maps two 
%Classes to the intersection of these &%Classes.  An object is an instance 
of the intersection of two &%Classes just in case it is an instance of 
both of those &%Classes."):-true.
<=>([instance, '?ENTITY', ['IntersectionFn', '?CLASS1', '?CLASS2']], [and, [instance, '?ENTITY', '?CLASS1'], [instance, '?ENTITY', '?CLASS2']]):-true.
instance('RelativeComplementFn', 'BinaryFunction'):-true.
domain('RelativeComplementFn', 1, 'SetOrClass'):-true.
domain('RelativeComplementFn', 2, 'SetOrClass'):-true.
range('RelativeComplementFn', 'SetOrClass'):-true.
documentation('RelativeComplementFn', "A &%BinaryFunction that maps two 
&%Classes to the difference between these &%Classes.  More precisely, 
the relative complement of one class C1 relative to another C2 consists 
of the instances of C1 that are instances of the &%ComplementFn of C2."):-true.
equal(['RelativeComplementFn', '?CLASS1', '?CLASS2'], ['IntersectionFn', '?CLASS1', ['ComplementFn', '?CLASS2']]):-true.
=>([instance, '?NUMBER', 'ImaginaryNumber'], [instance, '?NUMBER', ['RelativeComplementFn', 'Number', 'RealNumber']]):-true.
instance('ComplementFn', 'UnaryFunction'):-true.
domain('ComplementFn', 1, 'SetOrClass'):-true.
range('ComplementFn', 'SetOrClass'):-true.
documentation('ComplementFn', "The complement of a given &%SetOrClass C is the 
&%SetOrClass of all things that are not instances of C.  In other words, an 
object is an instance of the complement of a &%SetOrClass C just in case it 
is not an instance of C."):-true.
<=>([instance, '?ENTITY', ['ComplementFn', '?CLASS']], [not, [instance, '?ENTITY', '?CLASS']]):-true.
instance('GeneralizedUnionFn', 'UnaryFunction'):-true.
'domainSubclass'('GeneralizedUnionFn', 1, 'SetOrClass'):-true.
range('GeneralizedUnionFn', 'SetOrClass'):-true.
documentation('GeneralizedUnionFn', "A &%UnaryFunction that takes a &%SetOrClass 
of &%Classes as its single argument and returns a &%SetOrClass which is the 
merge of all of the &%Classes in the original &%SetOrClass, i.e. the &%SetOrClass 
containing just those instances which are instances of an instance of the 
original &%SetOrClass."):-true.
<=>([instance, '?ENTITY', ['GeneralizedUnionFn', '?SUPERCLASS']], [exists, ['?CLASS'], [and, [instance, '?CLASS', '?SUPERCLASS'], [instance, '?ENTITY', '?CLASS']]]):-true.
instance('GeneralizedIntersectionFn', 'UnaryFunction'):-true.
'domainSubclass'('GeneralizedIntersectionFn', 1, 'SetOrClass'):-true.
range('GeneralizedIntersectionFn', 'SetOrClass'):-true.
documentation('GeneralizedIntersectionFn', "A &%UnaryFunction that takes a 
&%SetOrClass of &%Classes as its single argument and returns a &%SetOrClass which 
is the intersection of all of the &%Classes in the original &%SetOrClass, i.e. 
the &%SetOrClass containing just those instances which are instances of all 
instances of the original &%SetOrClass."):-true.
<=>([instance, '?ENTITY', ['GeneralizedIntersectionFn', '?SUPERCLASS']], [forall, ['?CLASS'], [=>, [instance, '?CLASS', '?SUPERCLASS'], [instance, '?ENTITY', '?CLASS']]]):-true.
instance('CardinalityFn', 'UnaryFunction'):-true.
instance('CardinalityFn', 'AsymmetricRelation'):-true.
domain('CardinalityFn', 1, ['UnionFn', 'SetOrClass', 'Collection']):-true.
domain('CardinalityFn', 2, 'Number'):-true.
documentation('CardinalityFn', "(CardinalityFn ?CLASS) returns the 
number of instances in the &%SetOrClass or &%Collection ?CLASS."):-true.
instance('NullSet', 'Set'):-true.
documentation('NullSet', "The &%Set that contains no instances."):-true.
equal('NullSet', ['ComplementFn', 'Entity']):-true.
subclass('FiniteSet', 'Set'):-true.
documentation('FiniteSet', "A &%Set containing a finite number of elements."):-true.
=>([instance, '?SET', 'FiniteSet'], [exists, ['?NUMBER'], [and, [instance, '?NUMBER', 'NonnegativeInteger'], [equal, '?NUMBER', ['CardinalityFn', '?SET']]]]):-true.
subclass('PairwiseDisjointClass', 'SetOrClass'):-true.
documentation('PairwiseDisjointClass', "A &%SetOrClass of &%Classes is a 
&%PairwiseDisjointClass just in case every instance of the &%SetOrClass 
is either &%equal to or &%disjoint from every other instance of the 
&%SetOrClass."):-true.
=>([instance, '?SUPERCLASS', 'PairwiseDisjointClass'], [forall, ['?CLASS1', '?CLASS2'], [=>, [and, [instance, '?CLASS1', '?SUPERCLASS'], [instance, '?CLASS2', '?SUPERCLASS']], [or, [equal, '?CLASS1', '?CLASS2'], [disjoint, '?CLASS1', '?CLASS2']]]]):-true.
subclass('MutuallyDisjointClass', 'SetOrClass'):-true.
documentation('MutuallyDisjointClass', "A &%SetOrClass of &%Classes is a 
&%MutuallyDisjointClass just in case there exists no instance of an 
instance of the original &%SetOrClass which is an instance of all of the 
instances of the original &%SetOrClass."):-true.
=>([instance, '?CLASS', 'MutuallyDisjointClass'], [equal, ['GeneralizedIntersectionFn', '?CLASS'], 'NullSet']):-true.
instance('KappaFn', 'BinaryFunction'):-true.
domain('KappaFn', 1, 'SymbolicString'):-true.
domain('KappaFn', 2, 'Formula'):-true.
range('KappaFn', 'SetOrClass'):-true.
documentation('KappaFn', "A class-forming operator that takes two 
arguments:  a variable and a formula containing at least one unbound 
occurrence of the variable.  The result of applying &%KappaFn to a 
variable and a formula is the &%SetOrClass of things that satisfy the formula.  
For example, we can denote the &%SetOrClass of prime numbers that are less 
than 100 with the following expression:  (KappaFn ?NUMBER 
(and (instance ?NUMBER PrimeNumber) (lessThan ?NUMBER 100))).  Note that 
the use of this function is discouraged, since there is currently no 
axiomatic support for it."):-true.
subclass('Graph', 'Abstract'):-true.
documentation('Graph', "The &%Class of graphs, where a graph is understood 
to be a set of &%GraphNodes connected by &%GraphArcs.  Note that this 
&%Class includes only connected graphs, i.e. graphs in which there is a 
&%GraphPath between any two &%GraphNodes.  Note too that every &%Graph 
is assumed to contain at least two &%GraphArcs and three &%GraphNodes."):-true.
=>([and, [instance, '?GRAPH', 'Graph'], [instance, '?NODE1', 'GraphNode'], [instance, '?NODE2', 'GraphNode'], ['graphPart', '?NODE1', '?GRAPH'], ['graphPart', '?NODE2', '?GRAPH'], [not, [equal, '?NODE1', '?NODE2']]], [exists, ['?ARC', '?PATH'], [or, [links, '?NODE1', '?NODE2', '?ARC'], [and, ['subGraph', '?PATH', '?GRAPH'], [instance, '?PATH', 'GraphPath'], [or, [and, [equal, ['BeginNodeFn', '?PATH'], '?NODE1'], [equal, ['EndNodeFn', '?PATH'], '?NODE2']], [and, [equal, ['BeginNodeFn', '?PATH'], '?NODE2'], [equal, ['EndNodeFn', '?PATH'], '?NODE1']]]]]]):-true.
=>([instance, '?GRAPH', 'Graph'], [exists, ['?NODE1', '?NODE2', '?NODE3', '?ARC1', '?ARC2'], [and, ['graphPart', '?NODE1', '?GRAPH'], ['graphPart', '?NODE2', '?GRAPH'], ['graphPart', '?NODE3', '?GRAPH'], ['graphPart', '?ARC1', '?GRAPH'], ['graphPart', '?ARC2', '?GRAPH'], [links, '?ARC1', '?NODE1', '?NODE2'], [links, '?ARC2', '?NODE2', '?NODE3'], [not, [equal, '?NODE1', '?NODE2']], [not, [equal, '?NODE2', '?NODE3']], [not, [equal, '?NODE1', '?NODE3']], [not, [equal, '?ARC1', '?ARC2']]]]):-true.
subclass('DirectedGraph', 'Graph'):-true.
documentation('DirectedGraph', "The &%Class of directed graphs.  A 
directed graph is a &%Graph in which all &%GraphArcs
have direction, i.e. every &%GraphArc has an initial node (see 
&%InitialNodeFn) and a terminal node (see &%TerminalNodeFn)."):-true.
=>([and, [instance, '?GRAPH', 'DirectedGraph'], [instance, '?ARC', 'GraphArc'], ['graphPart', '?ARC', '?GRAPH']], [exists, ['?NODE1', '?NODE2'], [and, [equal, ['InitialNodeFn', '?ARC'], '?NODE1'], [equal, ['TerminalNodeFn', '?ARC'], '?NODE2']]]):-true.
subclass('Tree', 'Graph'):-true.
documentation('Tree', "A Tree is a &%DirectedGraph that has no 
&%GraphLoops."):-true.
=>([instance, '?GRAPH', 'Tree'], [not, [exists, ['?LOOP'], [and, [instance, '?LOOP', 'GraphLoop'], ['graphPart', '?LOOP', '?GRAPH']]]]):-true.
subclass('GraphPath', 'DirectedGraph'):-true.
documentation('GraphPath', "Informally, a single, directed route between 
two &%GraphNodes in a &%Graph.  Formally, a &%DirectedGraph that is a 
&%subGraph of the original &%Graph and such that no two &%GraphArcs in 
the &%DirectedGraph have the same intial node (see &%InitialNodeFn) or 
the same terminal node (see &%TerminalNodeFn)."):-true.
=>([and, [instance, '?GRAPH', 'GraphPath'], [instance, '?ARC', 'GraphArc'], ['graphPart', '?ARC', '?GRAPH']], [=>, [equal, ['InitialNodeFn', '?ARC'], '?NODE'], [not, [exists, ['?OTHER'], [and, [equal, ['InitialNodeFn', '?OTHER'], '?NODE'], [not, [equal, '?OTHER', '?ARC']]]]]]):-true.
=>([and, [instance, '?GRAPH', 'GraphPath'], [instance, '?ARC', 'GraphArc'], ['graphPart', '?ARC', '?GRAPH']], [=>, [equal, ['TerminalNodeFn', '?ARC'], '?NODE'], [not, [exists, ['?OTHER'], [and, [equal, ['TerminalNodeFn', '?OTHER'], '?NODE'], [not, [equal, '?OTHER', '?ARC']]]]]]):-true.
subclass('GraphCircuit', 'GraphPath'):-true.
documentation('GraphCircuit', "A &%GraphPath that begins (see 
&%BeginNodeFn) and ends (see &%EndNodeFn) at the same 
&%GraphNode."):-true.
<=>([instance, '?GRAPH', 'GraphCircuit'], [exists, ['?NODE'], [and, [equal, ['BeginNodeFn', '?GRAPH'], '?NODE'], [equal, ['EndNodeFn', '?GRAPH'], '?NODE']]]):-true.
subclass('MultiGraph', 'Graph'):-true.
documentation('MultiGraph', "The &%Class of multigraphs.  A multigraph 
is a &%Graph containing at least one pair of &%GraphNodes that are 
connected by more than one &%GraphArc."):-true.
<=>([instance, '?GRAPH', 'MultiGraph'], [exists, ['?ARC1', '?ARC2', '?NODE1', '?NODE2'], [and, ['graphPart', '?ARC1', '?GRAPH'], ['graphPart', '?ARC2', '?GRAPH'], ['graphPart', '?NODE1', '?GRAPH'], ['graphPart', '?NODE2', '?GRAPH'], [links, '?NODE1', '?NODE2', '?ARC1'], [links, '?NODE1', '?NODE2', '?ARC2'], [not, [equal, '?ARC1', '?ARC2']]]]):-true.
subclass('PseudoGraph', 'Graph'):-true.
documentation('PseudoGraph', "The &%Class of pseudographs.  A pseudograph 
is a &%Graph containing at least one &%GraphLoop."):-true.
<=>([instance, '?GRAPH', 'PseudoGraph'], [exists, ['?LOOP'], [and, [instance, '?LOOP', 'GraphLoop'], ['graphPart', '?LOOP', '?GRAPH']]]):-true.
subclass('GraphElement', 'Abstract'):-true.
disjoint('GraphElement', 'Graph'):-true.
partition('GraphElement', 'GraphNode', 'GraphArc'):-true.
documentation('GraphElement', "Noncompositional parts of &%Graphs.  
These parts are restricted to &%GraphNodes and &%GraphArcs."):-true.
=>([instance, '?PART', 'GraphElement'], [exists, ['?GRAPH'], [and, [instance, '?GRAPH', 'Graph'], ['graphPart', '?PART', '?GRAPH']]]):-true.
subclass('GraphNode', 'GraphElement'):-true.
documentation('GraphNode', "&%Graphs are comprised of &%GraphNodes 
and &%GraphArcs.  Every &%GraphNode is linked by a &%GraphArc."):-true.
=>([instance, '?NODE', 'GraphNode'], [exists, ['?OTHER', '?ARC'], [links, '?NODE', '?OTHER', '?ARC']]):-true.
subclass('GraphArc', 'GraphElement'):-true.
documentation('GraphArc', "&%Graphs are comprised of &%GraphNodes 
and &%GraphArcs.  Every &%GraphArc links two &%GraphNodes."):-true.
=>([instance, '?ARC', 'GraphArc'], [exists, ['?NODE1', '?NODE2'], [links, '?NODE1', '?NODE2', '?ARC']]):-true.
subclass('GraphLoop', 'GraphArc'):-true.
documentation('GraphLoop', "A &%GraphArc in which a &%GraphNode is 
linked to itself."):-true.
<=>([instance, '?LOOP', 'GraphLoop'], [exists, ['?NODE'], [links, '?NODE', '?NODE', '?LOOP']]):-true.
=>([and, [equal, ['InitialNodeFn', '?ARC'], '?NODE'], [equal, ['TerminalNodeFn', '?ARC'], '?NODE']], [instance, '?ARC', 'GraphLoop']):-true.
instance(links, 'TernaryPredicate'):-true.
domain(links, 1, 'GraphNode'):-true.
domain(links, 2, 'GraphNode'):-true.
domain(links, 3, 'GraphArc'):-true.
documentation(links, "a &%TernaryPredicate that specifies the 
&%GraphArc connecting two &%GraphNodes."):-true.
=>([links, '?NODE1', '?NODE2', '?ARC'], [links, '?NODE2', '?NODE1', '?ARC']):-true.
instance('graphPart', 'BinaryPredicate'):-true.
instance('graphPart', 'AsymmetricRelation'):-true.
instance('graphPart', 'IrreflexiveRelation'):-true.
domain('graphPart', 1, 'GraphElement'):-true.
domain('graphPart', 2, 'Graph'):-true.
documentation('graphPart', "A basic relation for &%Graphs and their 
parts.  (&%graphPart ?PART ?GRAPH) means that ?PART is a &%GraphArc 
or &%GraphNode of the &%Graph ?GRAPH."):-true.
instance('subGraph', 'BinaryPredicate'):-true.
instance('subGraph', 'ReflexiveRelation'):-true.
instance('subGraph', 'TransitiveRelation'):-true.
domain('subGraph', 1, 'Graph'):-true.
domain('subGraph', 2, 'Graph'):-true.
documentation('subGraph', "The relation between two &%Graphs when one 
&%Graph is a part of the other.  (&%subGraph ?GRAPH1 ?GRAPH2) means 
that ?GRAPH1 is a part of ?GRAPH2."):-true.
=>([and, ['subGraph', '?GRAPH1', '?GRAPH2'], ['graphPart', '?ELEMENT', '?GRAPH1']], ['graphPart', '?ELEMENT', '?GRAPH2']):-true.
instance('pathLength', 'BinaryPredicate'):-true.
instance('pathLength', 'AsymmetricRelation'):-true.
instance('pathLength', 'IrreflexiveRelation'):-true.
domain('pathLength', 1, 'GraphPath'):-true.
domain('pathLength', 2, 'PositiveInteger'):-true.
documentation('pathLength', "A &%BinaryPredicate that specifies the 
length (in number of &%GraphNodes) of a &%GraphPath.
(&%pathLength ?PATH ?NUMBER) means that there are ?NUMBER nodes in 
the &%GraphPath ?PATH."):-true.
instance('InitialNodeFn', 'UnaryFunction'):-true.
domain('InitialNodeFn', 1, 'GraphArc'):-true.
range('InitialNodeFn', 'GraphNode'):-true.
documentation('InitialNodeFn', "A &%UnaryFunction that maps a 
&%GraphArc to the initial node of the &%GraphArc.  Note
that this is a partial function.  In particular, the function is 
undefined for &%GraphArcs that are not part of a &%DirectedGraph."):-true.
instance('TerminalNodeFn', 'UnaryFunction'):-true.
domain('TerminalNodeFn', 1, 'GraphArc'):-true.
range('TerminalNodeFn', 'GraphNode'):-true.
documentation('TerminalNodeFn', "A &%UnaryFunction that maps a 
&%GraphArc to the terminal node of the &%GraphArc.  Note that this 
is a partial function.  In particular, the function is undefined 
for &%GraphArcs that are not part of a &%DirectedGraph."):-true.
instance('BeginNodeFn', 'UnaryFunction'):-true.
domain('BeginNodeFn', 1, 'GraphPath'):-true.
range('BeginNodeFn', 'GraphNode'):-true.
'relatedInternalConcept'('BeginNodeFn', 'InitialNodeFn'):-true.
documentation('BeginNodeFn', "A &%UnaryFunction that maps a &%GraphPath 
to the &%GraphNode that is the beginning of the &%GraphPath.  Note that, 
unlike &%InitialNodeFn (which relates a &%GraphArc to a &%GraphNode),  
&%BeginNodeFn is a total function - every &%GraphPath has a beginning."):-true.
instance('EndNodeFn', 'UnaryFunction'):-true.
domain('EndNodeFn', 1, 'GraphPath'):-true.
range('EndNodeFn', 'GraphNode'):-true.
'relatedInternalConcept'('EndNodeFn', 'TerminalNodeFn'):-true.
documentation('EndNodeFn', "A &%UnaryFunction that maps a &%GraphPath 
to the &%GraphNode that is the end of the &%GraphPath.  Note that, unlike 
&%TerminalNodeFn (which relates a &%GraphArc to a &%GraphNode),  
&%EndNodeFn is a total function - every &%GraphPath has a end."):-true.
instance('arcWeight', 'BinaryPredicate'):-true.
domain('arcWeight', 1, 'GraphArc'):-true.
domain('arcWeight', 2, 'RealNumber'):-true.
documentation('arcWeight', "This predicate indicates the value of a 
&%GraphArc in a &%Graph.  This could map to the length of a road in 
a road network or the flow rate of a pipe in a plumbing system."):-true.
instance('PathWeightFn', 'UnaryFunction'):-true.
domain('PathWeightFn', 1, 'GraphPath'):-true.
range('PathWeightFn', 'RealNumber'):-true.
documentation('PathWeightFn', "A &%UnaryFunction that maps a 
&%GraphPath to the sum of the &%arcWeights on the &%GraphArcs in 
the &%GraphPath."):-true.
=>([and, [equal, ['PathWeightFn', '?PATH'], '?SUM'], ['subGraph', '?SUBPATH', '?PATH'], ['graphPart', '?ARC1', '?PATH'], ['arcWeight', '?ARC1', '?NUMBER1'], [forall, ['?ARC2'], [=>, ['graphPart', '?ARC2', '?PATH'], [or, ['graphPart', '?ARC2', '?SUBPATH'], [equal, '?ARC2', '?ARC1']]]]], [equal, '?SUM', ['AdditionFn', ['PathWeightFn', '?SUBPATH'], '?NUMBER1']]):-true.
=>([and, [equal, ['PathWeightFn', '?PATH'], '?SUM'], ['graphPart', '?ARC1', '?PATH'], ['graphPart', '?ARC2', '?PATH'], ['arcWeight', '?ARC1', '?NUMBER1'], ['arcWeight', '?ARC2', '?NUMBER2'], [forall, ['?ARC3'], [=>, ['graphPart', '?ARC3', '?PATH'], [or, [equal, '?ARC3', '?ARC1'], [equal, '?ARC3', '?ARC2']]]]], [equal, ['PathWeightFn', '?PATH'], ['AdditionFn', '?NUMBER1', '?NUMBER2']]):-true.
instance('MinimalWeightedPathFn', 'BinaryFunction'):-true.
domain('MinimalWeightedPathFn', 1, 'GraphNode'):-true.
domain('MinimalWeightedPathFn', 2, 'GraphNode'):-true.
range('MinimalWeightedPathFn', 'GraphPath'):-true.
documentation('MinimalWeightedPathFn', "This &%BinaryFunction assigns two 
&%GraphNodes to the &%GraphPath with the smallest sum of weighted arcs 
between the two &%GraphNodes."):-true.
=>([equal, ['MinimalWeightedPathFn', '?NODE1', '?NODE2'], '?PATH'], [instance, '?PATH', ['GraphPathFn', '?NODE1', '?NODE2']]):-true.
=>([and, [equal, ['MinimalWeightedPathFn', '?NODE1', '?NODE2'], '?PATH'], [equal, ['PathWeightFn', '?PATH'], '?NUMBER']], [forall, ['?PATH2'], [=>, [and, [instance, '?PATH2', ['GraphPathFn', '?NODE1', '?NODE2']], [equal, ['PathWeightFn', '?PATH2'], '?NUMBER2']], ['greaterThanOrEqualTo', '?NUMBER2', '?NUMBER1']]]):-true.
instance('MaximalWeightedPathFn', 'BinaryFunction'):-true.
domain('MaximalWeightedPathFn', 1, 'GraphNode'):-true.
domain('MaximalWeightedPathFn', 2, 'GraphNode'):-true.
range('MaximalWeightedPathFn', 'GraphPath'):-true.
documentation('MaximalWeightedPathFn', "This &%BinaryFunction assigns two 
&%GraphNodes to the &%GraphPath with the largest sum of weighted arcs 
between the two &%GraphNodes."):-true.
=>([equal, ['MaximalWeightedPathFn', '?NODE1', '?NODE2'], '?PATH'], [instance, '?PATH', ['GraphPathFn', '?NODE1', '?NODE2']]):-true.
=>([and, [equal, ['MaximalWeightedPathFn', '?NODE1', '?NODE2'], '?PATH'], [equal, ['PathWeightFn', '?PATH'], '?NUMBER']], [forall, ['?PATH2'], [=>, [and, [instance, '?PATH2', ['GraphPathFn', '?NODE1', '?NODE2']], [equal, ['PathWeightFn', '?PATH2'], '?NUMBER2']], ['lessThanOrEqualTo', '?NUMBER2', '?NUMBER1']]]):-true.
instance('GraphPathFn', 'BinaryFunction'):-true.
domain('GraphPathFn', 1, 'GraphNode'):-true.
domain('GraphPathFn', 2, 'GraphNode'):-true.
'rangeSubclass'('GraphPathFn', 'GraphPath'):-true.
documentation('GraphPathFn', "A &%BinaryFunction that maps two &%GraphNodes 
to the &%Class of &%GraphPaths between those two nodes.  Note that the two 
&%GraphNodes must belong to the same &%Graph."):-true.
=>([and, ['graphPart', '?PATH', '?GRAPH'], [not, [instance, '?GRAPH', 'DirectedGraph']]], [<=>, [equal, ['GraphPathFn', '?NODE1', '?NODE2'], '?PATH'], [equal, ['GraphPathFn', '?NODE2', '?NODE1'], '?PATH']]):-true.
instance('CutSetFn', 'UnaryFunction'):-true.
domain('CutSetFn', 1, 'Graph'):-true.
'rangeSubclass'('CutSetFn', 'GraphPath'):-true.
documentation('CutSetFn', "A &%UnaryFunction that assigns a &%Graph the 
&%Class of &%GraphPaths that partition the graph into two separate 
graphs if cut.  There may be more than one cutset for a given graph."):-true.
instance('MinimalCutSetFn', 'UnaryFunction'):-true.
domain('MinimalCutSetFn', 1, 'Graph'):-true.
'rangeSubclass'('MinimalCutSetFn', 'GraphPath'):-true.
'relatedInternalConcept'('MinimalCutSetFn', 'CutSetFn'):-true.
documentation('MinimalCutSetFn', "A &%UnaryFunction that assigns a &%Graph 
the &%Class of &%GraphPaths which comprise cutsets for the &%Graph and 
which have the least number of &%GraphArcs."):-true.
subclass(['MinimalCutSetFn', '?GRAPH'], ['CutSetFn', '?GRAPH']):-true.
=>([equal, ['MinimalCutSetFn', '?GRAPH'], '?PATHCLASS'], [exists, ['?NUMBER'], [forall, ['?PATH'], [=>, [instance, '?PATH', '?PATHCLASS'], ['pathLength', '?PATH', '?NUMBER']]]]):-true.
is_not([exists, ['?PATH1', '?PATH2'], [and, [instance, '?PATH1', ['CutSetFn', '?GRAPH']], [instance, '?PATH2', ['MinimalCutSetFn', '?GRAPH']], ['pathLength', '?PATH1', '?NUMBER1'], ['pathLength', '?PATH2', '?NUMBER2'], ['lessThan', '?NUMBER1', '?NUMBER2']]]):-true.
subclass('UnitOfMeasure', 'PhysicalQuantity'):-true.
documentation('UnitOfMeasure', "A standard of measurement for some dimension. 
For example, the &%Meter is a &%UnitOfMeasure for the dimension of length, 
as is the &%Inch.  There is no intrinsic property of a &%UnitOfMeasure that 
makes it primitive or fundamental; rather, a system of units (e.g. 
&%SystemeInternationalUnit) defines a set of orthogonal dimensions and 
assigns units for each."):-true.
subclass('SystemeInternationalUnit', 'UnitOfMeasure'):-true.
documentation('SystemeInternationalUnit', "The &%Class of Systeme 
International (SI) units."):-true.
subclass('LengthMeasure', 'ConstantQuantity'):-true.
documentation('LengthMeasure', "The &%Class of &%ConstantQuantities relating 
to length."):-true.
subclass('MassMeasure', 'ConstantQuantity'):-true.
documentation('MassMeasure', "The &%Class of &%ConstantQuantities relating 
to the amount of matter in an &%Object."):-true.
subclass('AreaMeasure', 'ConstantQuantity'):-true.
documentation('AreaMeasure', "Measures of the amount of space in two 
dimensions."):-true.
subclass('VolumeMeasure', 'ConstantQuantity'):-true.
documentation('VolumeMeasure', "Measures of the amount of space in three 
dimensions."):-true.
subclass('TemperatureMeasure', 'ConstantQuantity'):-true.
documentation('TemperatureMeasure', "Measures of temperature.  
In scientific circles, the temperature of something is understood as the 
average velocity of the atoms or molecules that make up the thing."):-true.
subclass('CurrencyMeasure', 'ConstantQuantity'):-true.
documentation('CurrencyMeasure', "Includes all standard measures of monetary 
value, including &%UnitedStatesDollar, &%UnitedStatesCent, Lire, Yen, etc."):-true.
subclass('AngleMeasure', 'ConstantQuantity'):-true.
documentation('AngleMeasure', "The value of an angle in a plane or in a 
solid."):-true.
subclass('PlaneAngleMeasure', 'AngleMeasure'):-true.
documentation('PlaneAngleMeasure', "The value of an angle in a plane."):-true.
subclass('SolidAngleMeasure', 'AngleMeasure'):-true.
disjoint('SolidAngleMeasure', 'PlaneAngleMeasure'):-true.
documentation('SolidAngleMeasure', "The value of an angle in a solid."):-true.
instance('MeasureFn', 'BinaryFunction'):-true.
domain('MeasureFn', 1, 'RealNumber'):-true.
domain('MeasureFn', 2, 'UnitOfMeasure'):-true.
range('MeasureFn', 'ConstantQuantity'):-true.
documentation('MeasureFn', "This &%BinaryFunction maps a &%RealNumber and 
a &%UnitOfMeasure to that &%Number of units.  It is used for expressing 
&%ConstantQuantities.  For example, the concept of three meters is 
represented as (&%MeasureFn 3 &%Meter)."):-true.
=>([and, [equal, ['MeasureFn', '?NUMBER', '?UNIT'], '?QUANT'], [instance, '?UNIT', '?QUANTTYPE'], [not, [equal, '?QUANTTYPE', 'UnitOfMeasure']]], [instance, '?QUANT', '?QUANTTYPE']):-true.
=>([and, [instance, '?FUNCTION', 'RelationExtendedToQuantities'], [instance, '?FUNCTION', 'BinaryFunction'], [instance, '?NUMBER1', 'RealNumber'], [instance, '?NUMBER2', 'RealNumber'], [equal, ['AssignmentFn', '?FUNCTION', '?NUMBER1', '?NUMBER2'], '?VALUE']], [forall, ['?UNIT'], [=>, [instance, '?UNIT', 'UnitOfMeasure'], [equal, ['AssignmentFn', '?FUNCTION', ['MeasureFn', '?NUMBER1', '?UNIT'], ['MeasureFn', '?NUMBER2', '?UNIT']], ['MeasureFn', '?VALUE', '?UNIT']]]]):-true.
=>([and, [instance, '?REL', 'RelationExtendedToQuantities'], [instance, '?REL', 'BinaryRelation'], [instance, '?NUMBER1', 'RealNumber'], [instance, '?NUMBER2', 'RealNumber'], [holds, '?REL', '?NUMBER1', '?NUMBER2']], [forall, ['?UNIT'], [=>, [instance, '?UNIT', 'UnitOfMeasure'], [holds, '?REL', ['MeasureFn', '?NUMBER1', '?UNIT'], ['MeasureFn', '?NUMBER2', '?UNIT']]]]):-true.
instance('KiloFn', 'UnaryFunction'):-true.
domain('KiloFn', 1, 'UnitOfMeasure'):-true.
range('KiloFn', 'UnitOfMeasure'):-true.
documentation('KiloFn', "A &%UnaryFunction that maps a &%UnitOfMeasure into 
a &%UnitOfMeasure that is equal to 1,000 units of the original &%UnitOfMeasure.  
For example, (&%KiloFn &%Gram) is 1,000 &%Grams."):-true.
equal(['KiloFn', '?UNIT'], ['MeasureFn', 1000, '?UNIT']):-true.
instance('MegaFn', 'UnaryFunction'):-true.
domain('MegaFn', 1, 'UnitOfMeasure'):-true.
range('MegaFn', 'UnitOfMeasure'):-true.
documentation('MegaFn', "A &%UnaryFunction that maps a &%UnitOfMeasure into 
a &%UnitOfMeasure that is equal to 1,000,000 units of the original 
&%UnitOfMeasure.  For example, (&%KiloFn &%Hertz) is 1,000,000 &%Hertz."):-true.
equal(['MegaFn', '?UNIT'], ['MeasureFn', 1000000, '?UNIT']):-true.
instance('GigaFn', 'UnaryFunction'):-true.
domain('GigaFn', 1, 'UnitOfMeasure'):-true.
range('GigaFn', 'UnitOfMeasure'):-true.
documentation('GigaFn', "A &%UnaryFunction that maps a &%UnitOfMeasure into 
a &%UnitOfMeasure that is equal to 1,000,000,000 units of the original 
&%UnitOfMeasure.  For example, (&%GigaFn &%Hertz) is 1,000,000,000 &%Hertz."):-true.
equal(['GigaFn', '?UNIT'], ['MeasureFn', 1000000000, '?UNIT']):-true.
instance('TeraFn', 'UnaryFunction'):-true.
domain('TeraFn', 1, 'UnitOfMeasure'):-true.
range('TeraFn', 'UnitOfMeasure'):-true.
documentation('TeraFn', "A &%UnaryFunction that maps a &%UnitOfMeasure 
into a &%UnitOfMeasure that is equal to 1,000,000,000,000 units of the original 
&%UnitOfMeasure.  For example, (&%TeraFn &%Hertz) is 1,000,000,000,000 &%Hertz."):-true.
equal(['TeraFn', '?UNIT'], ['MeasureFn', 1e+012, '?UNIT']):-true.
instance('MilliFn', 'UnaryFunction'):-true.
domain('MilliFn', 1, 'UnitOfMeasure'):-true.
range('MilliFn', 'UnitOfMeasure'):-true.
documentation('MilliFn', "A &%UnaryFunction that maps a &%UnitOfMeasure into 
a &%UnitOfMeasure that is equal to .001 units of the original &%UnitOfMeasure.  
For example, (&%MilliFn &%Ampere) is .001 &%Amperes."):-true.
equal(['MilliFn', '?UNIT'], ['MeasureFn', 0.001, '?UNIT']):-true.
instance('MicroFn', 'UnaryFunction'):-true.
domain('MicroFn', 1, 'UnitOfMeasure'):-true.
range('MicroFn', 'UnitOfMeasure'):-true.
documentation('MicroFn', "A &%UnaryFunction that maps a &%UnitOfMeasure into 
a &%UnitOfMeasure that is equal to .000001 units of the original &%UnitOfMeasure.  
For example, (&%MicroFn &%Meter) is .000001 &%Meters."):-true.
equal(['MicroFn', '?UNIT'], ['MeasureFn', 1e-006, '?UNIT']):-true.
instance('NanoFn', 'UnaryFunction'):-true.
domain('NanoFn', 1, 'UnitOfMeasure'):-true.
range('NanoFn', 'UnitOfMeasure'):-true.
documentation('NanoFn', "A &%UnaryFunction that maps a &%UnitOfMeasure into 
a &%UnitOfMeasure that is equal to .000000001 units of the original 
&%UnitOfMeasure.  For example, (&%MicroFn &%SecondDuration) is .000000001 
&%SecondDurations."):-true.
equal(['NanoFn', '?UNIT'], ['MeasureFn', 1e-009, '?UNIT']):-true.
instance('PicoFn', 'UnaryFunction'):-true.
domain('PicoFn', 1, 'UnitOfMeasure'):-true.
range('PicoFn', 'UnitOfMeasure'):-true.
documentation('PicoFn', "A &%UnaryFunction that maps a &%UnitOfMeasure into 
a &%UnitOfMeasure that is equal to .000000000001 units of the original 
&%UnitOfMeasure.  For example, (&%PicoFn &%Ampere) is .000000000001 
&%Amperes."):-true.
equal(['PicoFn', '?UNIT'], ['MeasureFn', 1e-012, '?UNIT']):-true.
instance('IntervalFn', 'BinaryFunction'):-true.
domain('IntervalFn', 1, 'ConstantQuantity'):-true.
domain('IntervalFn', 2, 'ConstantQuantity'):-true.
'rangeSubclass'('IntervalFn', 'ConstantQuantity'):-true.
'relatedInternalConcept'('IntervalFn', 'RecurrentTimeIntervalFn'):-true.
documentation('IntervalFn', "A &%BinaryFunction that maps two &%ConstantQuantities 
to the &%Class of &%ConstantQuantities that comprise the interval from the first 
&%ConstantQuantity to the second &%ConstantQuantity.  For example, (&%IntervalFn 
(&%MeasureFn 8 &%Meter) (&%MeasureFn 14 &%Meter)) would return the &%Class of 
&%ConstantQuantities between 8 and 14 meters in length."):-true.
=>([instance, '?QUANTITY', ['IntervalFn', '?FROM', '?TO']], [and, ['greaterThanOrEqualTo', '?QUANTITY', '?FROM'], ['lessThanOrEqualTo', '?QUANTITY', '?TO']]):-true.
instance('MagnitudeFn', 'UnaryFunction'):-true.
domain('MagnitudeFn', 1, 'ConstantQuantity'):-true.
range('MagnitudeFn', 'RealNumber'):-true.
documentation('MagnitudeFn', "The magnitude of a &%ConstantQuantity is the 
numeric value for the quantity.  In other words, &%MagnitudeFn converts 
a &%ConstantQuantity with an associated &%UnitOfMeasure into an ordinary 
&%RealNumber.  For example, the magnitude of the &%ConstantQuantity 2 
&%Kilometers is the &%RealNumber 2.   Note that the magnitude of a 
quantity in a given unit times that unit is equal to the original 
quantity."):-true.
equal(['MagnitudeFn', ['MeasureFn', '?NUMBER', '?UNIT']], '?NUMBER'):-true.
instance('DensityFn', 'BinaryFunction'):-true.
domain('DensityFn', 1, 'MassMeasure'):-true.
domain('DensityFn', 2, 'VolumeMeasure'):-true.
range('DensityFn', 'FunctionQuantity'):-true.
documentation('DensityFn', "&%DensityFn maps an instance of &%MassMeasure 
and an instance of &%VolumeMeasure to the density represented by this 
proportion of mass and volume.  For example, (&%DensityFn (&%MeasureFn 3 &%Gram) 
(&%MeasureFn 1 &%Liter)) represents the density of 3 grams per liter."):-true.
instance('SpeedFn', 'BinaryFunction'):-true.
domain('SpeedFn', 1, 'LengthMeasure'):-true.
domain('SpeedFn', 2, 'TimeDuration'):-true.
range('SpeedFn', 'FunctionQuantity'):-true.
documentation('SpeedFn', "Maps an instance of &%LengthMeasure and an instance of 
&%TimeDuration to the speed represented by this proportion of distance and time.  
For example, (&%SpeedFn (&%MeasureFn 55 &%Mile)(&%MeasureFn 1 &%HourDuration)) 
represents the velocity of 55 miles per hour."):-true.
instance('VelocityFn', 'QuaternaryFunction'):-true.
domain('VelocityFn', 1, 'LengthMeasure'):-true.
domain('VelocityFn', 2, 'TimeDuration'):-true.
domain('VelocityFn', 3, 'Region'):-true.
domain('VelocityFn', 4, 'DirectionalAttribute'):-true.
range('VelocityFn', 'FunctionQuantity'):-true.
documentation('VelocityFn', "Specifies the velocity of an object, i.e. the speed 
and the direction of the speed.  For example (&%VelocityFn (&%MeasureFn 55 &%Mile) 
(&%MeasureFn 2 &%HourDuration) ?REFERENCE &%North) denotes the velocity of 55 miles 
per hour North of the given reference point ?REFERENCE."):-true.
=>([measure, '?OBJECT', ['VelocityFn', '?DISTANCE', '?TIME', '?REF', '?DIRECTION']], [measure, '?OBJECT', ['SpeedFn', '?DISTANCE', '?TIME']]):-true.
subclass('Meter', 'LengthMeasure'):-true.
instance('Meter', 'SystemeInternationalUnit'):-true.
documentation('Meter', "SI &%LengthMeasure.  Symbol:  m. It is one of the
base units in SI, and it is currently defined as follows: the &%Meter 
is the length of the path traveled by light in a vacuum during a time 
interval of 1/299792458 of a &%SecondDuration."):-true.
subclass('Gram', 'MassMeasure'):-true.
instance('Gram', 'SystemeInternationalUnit'):-true.
documentation('Gram', "Submultiple of kilogram.  Symbol: g. 
1 kilogram = 1000 &%Grams."):-true.
subclass('SecondDuration', 'TimeDuration'):-true.
instance('SecondDuration', 'SystemeInternationalUnit'):-true.
documentation('SecondDuration', "SI &%TimeDuration.  Symbol: s. 
It is one of the base units in SI, and it is currently defined as 
follows: the &%SecondDuration is the duration of 9192631770 periods of 
the radiation corresponding to the transition between the two hyperfine 
levels of the ground state of the cesium 133 atom."):-true.
subclass('Ampere', 'FunctionQuantity'):-true.
instance('Ampere', 'SystemeInternationalUnit'):-true.
documentation('Ampere', "SI electric current measure.  Symbol: A. It is 
one of the base units in SI. It is defined as follows: the &%Ampere is 
that constant current which, if maintained in two straight parallel 
conductors of infinite length, of negligible circular cross-section, and 
placed 1 &%Meter apart in a vacuum, would produce between these conductors 
a force equal to 2*10^(-7) &%Newton per &%Meter of length."):-true.
subclass('KelvinDegree', 'TemperatureMeasure'):-true.
instance('KelvinDegree', 'SystemeInternationalUnit'):-true.
documentation('KelvinDegree', "SI &%TemperatureMeasure.  Symbol: K. 
It is one of the base units in SI (it is also a unit in the ITS system). 
Kelvin differs from the Celsius scale in that the triple point of water 
is defined to be 273.16 &%KelvinDegrees while it is 0 &%CelsiusDegrees.  
The magnitudes of intervals in the two scales are the same.  By definition 
the conversion constant is 273.15."):-true.
subclass('Mole', 'MassMeasure'):-true.
instance('Mole', 'SystemeInternationalUnit'):-true.
documentation('Mole', "SI amount of substance unit. symbol: mol. It is one 
of the base units in SI. It is defined as follows:  the &%Mole is the 
amount of substance of a system which contains as many elementary entities 
as there are atoms in 0.012 &%Kilograms of carbon 12.  Note that, when this 
&%UnitOfMeasure is used, the elementary entities must be specified - they 
may be atoms, molecules, ions, electrons, etc. or groups of such 
particles."):-true.
subclass('Candela', 'FunctionQuantity'):-true.
instance('Candela', 'SystemeInternationalUnit'):-true.
documentation('Candela', "SI luminosity intensity measure.  Symbol: cd. 
It is one of the base units in SI, and it is currently defined as 
follows:  the &%Candela is the luminous intensity, in a given direction, 
of a source that emits monochromatic radiation of frequency 540*10^12 
&%Hertz and that has a radiant intensity in that direction of 1/683 
&%Watt per &%Steradian."):-true.
subclass('Liter', 'VolumeMeasure'):-true.
instance('Liter', 'UnitOfMeasure'):-true.
documentation('Liter', "Unit of volume in the metric system.  It is currently 
defined to be equal to one cubic decimeter (0.001 cubic meter).  Symbol: l."):-true.
subclass('Centimeter', 'LengthMeasure'):-true.
instance('Centimeter', 'UnitOfMeasure'):-true.
documentation('Centimeter', "Submultiple of &%Meter.  Symbol: cm. It is 
the 100th part of a &%Meter"):-true.
equal(['MeasureFn', '?NUMBER', 'Centimeter'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 0.01], 'Meter']):-true.
subclass('Radian', 'PlaneAngleMeasure'):-true.
instance('Radian', 'SystemeInternationalUnit'):-true.
documentation('Radian', "SI plane angle measure.  Symbol: rad.  It is the 
angle of a circle subtended by an arc equal in length to the circle's 
radius. Another definition is:  the plane angle between two radii of a 
circle which cut off on the circumference an arc equal in length to the 
radius.  &%Radian = m/m = 1."):-true.
subclass('Steradian', 'SolidAngleMeasure'):-true.
instance('Steradian', 'SystemeInternationalUnit'):-true.
documentation('Steradian', "SI solid angle measure.  Symbol: sr.  It is 
the solid angle of a sphere subtended by a portion of the surface whose 
area is equal to the square of the sphere's radius.  Another definition 
is: the solid angle which, having its vertex in the center of the sphere, 
cuts off an area of the surface of the sphere equal to that of a square 
with sides of length equal to the radius of the sphere.  &%Steradian = 
m^2/m^2 = 1."):-true.
subclass('Hertz', 'TimeDependentQuantity'):-true.
instance('Hertz', 'SystemeInternationalUnit'):-true.
documentation('Hertz', "SI frequency measure.  Symbol: Hz. It is the 
number of cycles per second.  &%Hertz = s^(-1).  Note that &%Hertz 
does not have a conversion function."):-true.
subclass('Newton', 'FunctionQuantity'):-true.
instance('Newton', 'SystemeInternationalUnit'):-true.
documentation('Newton', "SI force measure.  Symbol: N. It is that force 
which gives to a mass of 1 kilogram an acceleration of 1 &%Meter per 
&%SecondDuration.  &%Newton = m*kg*s^(-2)."):-true.
subclass('Pascal', 'FunctionQuantity'):-true.
instance('Pascal', 'SystemeInternationalUnit'):-true.
documentation('Pascal', "SI pressure measure.  Symbol:Pa. It is the 
pressure of one &%Newton per square &%Meter.  &%Pascal = N/m^2 
= m^(-1)*kg*s^(-2)."):-true.
subclass('Joule', 'FunctionQuantity'):-true.
instance('Joule', 'SystemeInternationalUnit'):-true.
documentation('Joule', "SI energy measure.  Symbol: J.  It is the work 
done when the point of application of 1 &%Newton is displaced a distance 
of 1 &%Meter in the direction of the force.  &%Joule = N*m = 
m^2*kg*s^(-2)."):-true.
subclass('Watt', 'FunctionQuantity'):-true.
instance('Watt', 'SystemeInternationalUnit'):-true.
documentation('Watt', "SI power measure.  Symbol: W.  A &%UnitOfMeasure 
that measures power, i.e. energy produced or expended divided by 
&%TimeDuration. It is the power which gives rise to the production 
of energy (or work) at the rate of one &%Joule per &%SecondDuration.  
&%Watt = J/s = m^2*kg*s^(-3)."):-true.
subclass('Coulomb', 'TimeDependentQuantity'):-true.
instance('Coulomb', 'SystemeInternationalUnit'):-true.
documentation('Coulomb', "SI electric charge measure.  Symbol: C. It is 
the quantity of electric charge transported through a cross section of 
a conductor in an electric circuit during each &%SecondDuration by a 
current of 1 &%Ampere.  Coulomb = s*A."):-true.
subclass('Volt', 'FunctionQuantity'):-true.
instance('Volt', 'SystemeInternationalUnit'):-true.
documentation('Volt', "SI electric potential measure.  Symbol: V.  It is 
the difference of electric potential between two points of a conducting 
wire carrying a constant current of 1 &%Ampere, when the power dissipated 
between these points is equal to 1 &%Watt.  &%Volt = W/A = 
m^2*kg*s^(-3)*A^(-1)."):-true.
subclass('Farad', 'FunctionQuantity'):-true.
instance('Farad', 'SystemeInternationalUnit'):-true.
documentation('Farad', "SI capacitance measure.  Symbol: F.  It is the 
capacitance of a capacitator between the plates of which there appears 
a difference of potential of 1 &%Volt when it is charged by a quantity 
of electricity equal to 1 Coulomb.  &%Farad = C/V = 
m^(-2)*kg(-1)*s^4*A^2."):-true.
subclass('Ohm', 'FunctionQuantity'):-true.
instance('Ohm', 'SystemeInternationalUnit'):-true.
documentation('Ohm', "SI electric resistance measure. It is the electric
resistance between two points of a conductor when a constant difference 
of potential of 1 &%Volt, applied between these two points,
produces in this conductor a current of 1 &%Ampere, this conductor not
being the force of any electromotive force.  &%Ohm = V/A = 
m^2*kg*s^(-3)*A^(-2)."):-true.
subclass('Siemens', 'FunctionQuantity'):-true.
instance('Siemens', 'SystemeInternationalUnit'):-true.
documentation('Siemens', "SI electric conductance measure.  Symbol:  S.  
In the case of direct current, the conductance in &%Siemens is the 
reciprocal of the resistance in &%Ohms; in the case of alternating current, 
it is the reciprocal of the impedance in ohms.  siemens = A/V = 
m^(-2)*kg(-1)*s^(3)*A^2."):-true.
subclass('Weber', 'FunctionQuantity'):-true.
instance('Weber', 'SystemeInternationalUnit'):-true.
documentation('Weber', "SI magnetic flux measure.  Symbol: Wb. It is the 
magnetic flux which, linking a circuit of one turn, produces in it an
electromotive force of 1 &%Volt as it is reduced to zero at a uniform
rate in 1 &%SecondDuration.  &%Weber = V*s = m^2*kg*s^(-2)*A^(-1)."):-true.
subclass('Tesla', 'FunctionQuantity'):-true.
instance('Tesla', 'SystemeInternationalUnit'):-true.
documentation('Tesla', "SI magnetic flux density measure.  Symbol:  T.
One &%Tesla equals one &%Weber per square &%Meter.  &%Tesla = Wb/m^2 = 
kg*s^(-2)*A^(-1)."):-true.
subclass('Henry', 'FunctionQuantity'):-true.
instance('Henry', 'SystemeInternationalUnit'):-true.
documentation('Henry', "SI inductance measure.  Symbol: H.  One &%Henry 
is equivalent to one &%Volt divided by one &%Ampere per &%SecondDuration. 
If a current changing at the rate of one &%Ampere per &%SecondDuration 
induces an electromotive force of one &%Volt, the circuit has an 
inductance of one &%Henry.  &%Henry = Wb/A = m^2*kg*s^(-2)*A^(-2)."):-true.
subclass('CelsiusDegree', 'TemperatureMeasure'):-true.
instance('CelsiusDegree', 'SystemeInternationalUnit'):-true.
documentation('CelsiusDegree', "A &%TemperatureMeasure.  The freezing point 
and the boiling point of water are, respectively, 0 &%CelsiusDegrees and 100 
&%CelsiusDegrees."):-true.
equal(['MeasureFn', '?NUMBER', 'CelsiusDegree'], ['MeasureFn', ['SubtractionFn', '?NUMBER', 273.15], 'KelvinDegree']):-true.
equal(['MeasureFn', '?NUMBER', 'CelsiusDegree'], ['MeasureFn', ['DivisionFn', ['SubtractionFn', '?NUMBER', 32], 1.8], 'FahrenheitDegree']):-true.
subclass('Lumen', 'FunctionQuantity'):-true.
instance('Lumen', 'SystemeInternationalUnit'):-true.
documentation('Lumen', "SI luminous flux measure.  Symbol: lm.  It is the 
amount streaming outward through one solid angle of 1 &%Steradian from a 
uniform point source having an intensity of one &%Candela.  &%Lumen = 
cd*sr = cd * 1."):-true.
subclass('Lux', 'FunctionQuantity'):-true.
instance('Lux', 'SystemeInternationalUnit'):-true.
documentation('Lux', "SI illuminance measure.  Symbol: lx.  It is the 
amount of illumination provided when one &%Lumen is evenly distributed 
over an area of 1 square &%Meter. This is also equivalent to the 
illumination that would exist on a surface all points of which are one 
&%Meter from a point source of one &%Candela.  &%Lux = lm/m^2 = 
m^(-2)*cd."):-true.
subclass('Becquerel', 'TimeDependentQuantity'):-true.
instance('Becquerel', 'SystemeInternationalUnit'):-true.
documentation('Becquerel', "SI activity measure.  Symbol: Bq.  It measures 
the amount of radioactivity contained in a given sample of matter. It is 
that quantity of a radioactive element in which there is one atomic 
disintegration per &%SecondDuration.  &%Becquerel = s^(-1)."):-true.
subclass('Gray', 'FunctionQuantity'):-true.
instance('Gray', 'SystemeInternationalUnit'):-true.
documentation('Gray', "SI absorbed dose measure.  Symbol: Gy.  It measures 
the dose of radiation absorbed in living tissue. It is equal approximately 
to the absorbed dose delivered when the energy per unit mass imparted to
matter by ionizing radiation is 1 &%Joule per kilogram.  &%Gray = J/kg 
= m^2*s^(-2)."):-true.
subclass('Sievert', 'FunctionQuantity'):-true.
instance('Sievert', 'SystemeInternationalUnit'):-true.
documentation('Sievert', "SI dose equivalent measure.  Symbol: Sv.  It is 
a unit of biologic dose of ionizing radiation.  The &%Sievert makes it 
possible to normalize doses of different types of radiation. It takes 
into account the relative biologic effectiveness of ionizing radiation, 
since each form of such radiation--e.g., X rays, gamma rays, neutrons--
has a slightly different effect on living tissue for a given absorbed 
dose. The dose equivalent of a given type of radiation (in &%Sievert) is 
the dose of the radiation in &%Gray multiplied by a quality factor that 
is based on the relative biologic effectiveness of the radiation. 
Accordingly, one &%Sievert is generally defined as the amount of radiation 
roughly equivalent in biologic effectiveness to one &%Gray of gamma 
radiation.  &%Sievert = J/kg = m^2*s^(-2)"):-true.
subclass('DayDuration', 'TimeDuration'):-true.
instance('DayDuration', 'UnitOfMeasure'):-true.
documentation('DayDuration', "Time unit. 1 day = 24 hours."):-true.
equal(['MeasureFn', '?NUMBER', 'DayDuration'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 24], 'HourDuration']):-true.
subclass('HourDuration', 'TimeDuration'):-true.
instance('HourDuration', 'UnitOfMeasure'):-true.
documentation('HourDuration', "Time unit. 1 hour = 60 minutes."):-true.
equal(['MeasureFn', '?NUMBER', 'HourDuration'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 60], 'MinuteDuration']):-true.
subclass('MinuteDuration', 'TimeDuration'):-true.
instance('MinuteDuration', 'UnitOfMeasure'):-true.
documentation('MinuteDuration', "Time unit. 1 minute = 60 seconds. "):-true.
equal(['MeasureFn', '?NUMBER', 'MinuteDuration'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 60], 'SecondDuration']):-true.
subclass('WeekDuration', 'TimeDuration'):-true.
instance('WeekDuration', 'UnitOfMeasure'):-true.
documentation('WeekDuration', "Time unit.  A week's duration is seven days."):-true.
equal(['MeasureFn', '?NUMBER', 'WeekDuration'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 7], 'DayDuration']):-true.
subclass('YearDuration', 'TimeDuration'):-true.
instance('YearDuration', 'UnitOfMeasure'):-true.
documentation('YearDuration', "Time unit. one calendar year. 1 year =
365 days = 31536000 seconds."):-true.
equal(['MeasureFn', '?NUMBER', 'YearDuration'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 365], 'DayDuration']):-true.
subclass('Amu', 'MassMeasure'):-true.
instance('Amu', 'UnitOfMeasure'):-true.
documentation('Amu', "Atomic mass unit.  Symbol: u. It is the mass of 
the twelfth part of an atom of the Carbon 12 isotope."):-true.
equal(['MeasureFn', '?NUMBER', 'Amu'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', '1.6605402E-24'], 'Gram']):-true.
subclass('ElectronVolt', 'FunctionQuantity'):-true.
instance('ElectronVolt', 'UnitOfMeasure'):-true.
documentation('ElectronVolt', "The &%ElectronVolt is an energy measure.  
Symbol: eV.  It is the kinetic energy acquired by an electron in passing 
through a potential difference of 1 &%Volt in a vacuum."):-true.
equal(['MeasureFn', '?NUMBER', 'ElectronVolt'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', '1.60217733E-19'], 'Joule']):-true.
subclass('Angstrom', 'LengthMeasure'):-true.
instance('Angstrom', 'UnitOfMeasure'):-true.
documentation('Angstrom', "The &%Angstrom is a &%LengthMeasure. 
1 &%Angstrom = 10^(-10) m"):-true.
equal(['MeasureFn', '?NUMBER', 'Angstrom'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', '1.0E-10'], 'Meter']):-true.
subclass('Foot', 'LengthMeasure'):-true.
instance('Foot', 'UnitOfMeasure'):-true.
documentation('Foot', "English length unit of feet."):-true.
equal(['MeasureFn', '?NUMBER', 'Foot'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 0.3048], 'Meter']):-true.
subclass('Inch', 'LengthMeasure'):-true.
instance('Inch', 'UnitOfMeasure'):-true.
documentation('Inch', "English length unit of inches."):-true.
equal(['MeasureFn', '?NUMBER', 'Inch'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 0.0254], 'Meter']):-true.
subclass('Mile', 'LengthMeasure'):-true.
instance('Mile', 'UnitOfMeasure'):-true.
documentation('Mile', "English length unit of miles."):-true.
equal(['MeasureFn', '?NUMBER', 'Mile'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 1609.34], 'Meter']):-true.
subclass('UnitedStatesGallon', 'VolumeMeasure'):-true.
instance('UnitedStatesGallon', 'UnitOfMeasure'):-true.
'relatedInternalConcept'('UnitedStatesGallon', 'UnitedKingdomGallon'):-true.
documentation('UnitedStatesGallon', "Unit of volume commonly used in the 
United States."):-true.
equal(['MeasureFn', '?NUMBER', 'UnitedStatesGallon'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 3.78541], 'Liter']):-true.
subclass('Quart', 'VolumeMeasure'):-true.
instance('Quart', 'UnitOfMeasure'):-true.
documentation('Quart', "English unit of volume equal to 1/4 of a 
&%UnitedStatesGallon."):-true.
equal(['MeasureFn', '?NUMBER', 'Quart'], ['MeasureFn', ['DivisionFn', '?NUMBER', 4], 'UnitedStatesGallon']):-true.
subclass('Pint', 'VolumeMeasure'):-true.
instance('Pint', 'UnitOfMeasure'):-true.
documentation('Pint', "English unit of volume equal to 1/2 of a 
&%Quart."):-true.
equal(['MeasureFn', '?NUMBER', 'Pint'], ['MeasureFn', ['DivisionFn', '?NUMBER', 2], 'Quart']):-true.
subclass('Cup', 'VolumeMeasure'):-true.
instance('Cup', 'UnitOfMeasure'):-true.
documentation('Cup', "English unit of volume equal to 1/2 of a 
&%Pint."):-true.
equal(['MeasureFn', '?NUMBER', 'Cup'], ['MeasureFn', ['DivisionFn', '?NUMBER', 2], 'Pint']):-true.
subclass('Ounce', 'VolumeMeasure'):-true.
instance('Ounce', 'UnitOfMeasure'):-true.
documentation('Ounce', "English unit of volume equal to 1/8 of a 
&%Cup."):-true.
equal(['MeasureFn', '?NUMBER', 'Ounce'], ['MeasureFn', ['DivisionFn', '?NUMBER', 8], 'Cup']):-true.
subclass('UnitedKingdomGallon', 'VolumeMeasure'):-true.
instance('UnitedKingdomGallon', 'UnitOfMeasure'):-true.
documentation('UnitedKingdomGallon', "Unit of volume commonly used in the 
United Kingdom."):-true.
equal(['MeasureFn', '?NUMBER', 'UnitedKingdomGallon'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 4.54609], 'Liter']):-true.
subclass('AtomGram', 'MassMeasure'):-true.
instance('AtomGram', 'UnitOfMeasure'):-true.
documentation('AtomGram', "&%MassMeasure that is also known as the gram-atom.  
Defined as the mass in grams of 1 &%Mole of pure substance.  For example, 
1 &%AtomGram of Carbon 12 will be 12 &%Grams of pure Carbon 12.  2 &%AtomGrams 
of the same substance will be 24 &%Grams of it.  This is an unusual unit in 
that it is essentially 1 &%Mole of 'stuff' measured in grams, so that the 
actual value (i.e. mass) depends on the type of substance."):-true.
subclass('PoundMass', 'MassMeasure'):-true.
instance('PoundMass', 'UnitOfMeasure'):-true.
documentation('PoundMass', "English mass unit of pounds."):-true.
equal(['MeasureFn', '?NUMBER', 'PoundMass'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 453.592], 'Gram']):-true.
subclass('Slug', 'MassMeasure'):-true.
instance('Slug', 'UnitOfMeasure'):-true.
documentation('Slug', "English mass unit of slugs."):-true.
equal(['MeasureFn', '?NUMBER', 'Slug'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 14593.9], 'Gram']):-true.
subclass('RankineDegree', 'TemperatureMeasure'):-true.
instance('RankineDegree', 'UnitOfMeasure'):-true.
documentation('RankineDegree', "A &%TemperatureMeasure.  Note 
that 0 &%RankineDegrees is the same as the absolute zero (i.e. 0 
&%KelvinDegrees)."):-true.
equal(['MeasureFn', '?NUMBER', 'Rankine'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 1.8], 'Kelvin']):-true.
subclass('FahrenheitDegree', 'TemperatureMeasure'):-true.
instance('FahrenheitDegree', 'UnitOfMeasure'):-true.
documentation('FahrenheitDegree', "A &%TemperatureMeasure that is commonly 
used in the United States.  On the Fahrenheit scale, the freezing point 
of water is 32 &%FahrenheitDegrees, and the boiling point of water is 
212 &%FahrenheitDegrees."):-true.
subclass('PoundForce', 'FunctionQuantity'):-true.
instance('PoundForce', 'UnitOfMeasure'):-true.
documentation('PoundForce', "English pound of force. The conversion
factor depends on the local value of the acceleration of free fall. A
mean value is used in the conversion axiom associated with this 
constant."):-true.
equal(['MeasureFn', '?NUMBER', 'PoundForce'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 4.44822], 'Newton']):-true.
subclass('Calorie', 'FunctionQuantity'):-true.
instance('Calorie', 'UnitOfMeasure'):-true.
documentation('Calorie', "A &%Calorie is an energy measure."):-true.
equal(['MeasureFn', '?NUMBER', 'Calorie'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 4.1868], 'Joule']):-true.
subclass('BritishThermalUnit', 'FunctionQuantity'):-true.
instance('BritishThermalUnit', 'UnitOfMeasure'):-true.
documentation('BritishThermalUnit', "An energy measure."):-true.
equal(['MeasureFn', '?NUMBER', 'BritishThermalUnit'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 1055.06], 'Joule']):-true.
subclass('AngularDegree', 'PlaneAngleMeasure'):-true.
instance('AngularDegree', 'UnitOfMeasure'):-true.
documentation('AngularDegree', "A plane angle measure."):-true.
equal(['MeasureFn', '?NUMBER', 'AngularDegree'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', ['DivisionFn', 'Pi', 180]], 'Radian']):-true.
subclass('UnitedStatesDollar', 'CurrencyMeasure'):-true.
instance('UnitedStatesDollar', 'UnitOfMeasure'):-true.
documentation('UnitedStatesDollar', "A currency measure."):-true.
subclass('UnitedStatesCent', 'CurrencyMeasure'):-true.
instance('UnitedStatesCent', 'UnitOfMeasure'):-true.
documentation('UnitedStatesCent', "A currency measure.  1 &%UnitedStatesCent is 
equal to .01 &%UnitedStatesDollars."):-true.
equal(['MeasureFn', '?NUMBER', 'UnitedStatesCent'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 0.01], 'UnitedStatesDollar']):-true.
subclass('EuroDollar', 'CurrencyMeasure'):-true.
instance('EuroDollar', 'UnitOfMeasure'):-true.
documentation('EuroDollar', "A currency measure of most European Union countries.  
It is based on the &%UnitedStatesDollar."):-true.
subclass('EuroCent', 'CurrencyMeasure'):-true.
instance('EuroCent', 'UnitOfMeasure'):-true.
documentation('EuroCent', "A currency measure.  1 &%EuroCent is equal to .01 
&%EuroDollars."):-true.
equal(['MeasureFn', '?NUMBER', 'EuroCent'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 0.01], 'EuroDollar']):-true.
subclass('InformationMeasure', 'ConstantQuantity'):-true.
documentation('InformationMeasure', "Measures of the amount of information.  
Includes &%Bit, &%Byte, and multiples of these, e.g. &%KiloByte and 
&%MegaByte."):-true.
subclass('Bit', 'InformationMeasure'):-true.
instance('Bit', 'UnitOfMeasure'):-true.
documentation('Bit', "One &%Bit of information.  A one or a zero."):-true.
subclass('Byte', 'InformationMeasure'):-true.
instance('Byte', 'UnitOfMeasure'):-true.
documentation('Byte', "One &%Byte of information.  A &%Byte is eight 
&%Bits."):-true.
equal(['MeasureFn', '?NUMBER', 'Byte'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 8], 'Bit']):-true.
subclass('KiloByte', 'InformationMeasure'):-true.
instance('KiloByte', 'UnitOfMeasure'):-true.
documentation('KiloByte', "One &%KiloByte (KB) of information.  One 
&%KiloByte is 1024 &%Bytes.  Note that this sense of 'kilo' is 
different from the one accepted in the SI system."):-true.
equal(['MeasureFn', '?NUMBER', 'KiloByte'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 1024], 'Byte']):-true.
subclass('MegaByte', 'InformationMeasure'):-true.
instance('MegaByte', 'UnitOfMeasure'):-true.
documentation('MegaByte', "One &%MegaByte (MB) of information.  One 
&%MegaByte is 1024 &%KiloBytes.  Note that this sense of 'mega' is 
different from the one accepted in the SI system."):-true.
equal(['MeasureFn', '?NUMBER', 'MegaByte'], ['MeasureFn', ['MultiplicationFn', '?NUMBER', 1024], 'KiloByte']):-true.
instance(measure, 'BinaryPredicate'):-true.
instance(measure, 'AsymmetricRelation'):-true.
domain(measure, 1, 'Object'):-true.
domain(measure, 2, 'PhysicalQuantity'):-true.
documentation(measure, "A very general &%Predicate for asserting that a 
particular &%Object is measured by a particular &%ConstantQuantity.  
In general, the second argument of this &%Predicate will be an instance 
of the &%Function &%MeasureFn."):-true.
instance(age, 'SingleValuedRelation'):-true.
subrelation(age, measure):-true.
domain(age, 2, 'TimeDuration'):-true.
documentation(age, "Simply relates an &%Object to a &%ConstantQuantity 
specifying the age of the &%Object."):-true.
subrelation(length, measure):-true.
domain(length, 2, 'LengthMeasure'):-true.
documentation(length, "&%BinaryPredicate that is used to state the measure 
of an &%Object from one point to another point along its surface."):-true.
instance(width, 'SingleValuedRelation'):-true.
subrelation(width, length):-true.
documentation(width, "&%BinaryPredicate that is used to state the measure 
of an &%Object from side to side at its widest span."):-true.
subrelation(diameter, width):-true.
documentation(diameter, "&%BinaryPredicate that is used to state the 
measure of a circular &%Object from side to side."):-true.
instance(altitude, 'SingleValuedRelation'):-true.
subrelation(altitude, length):-true.
documentation(altitude, "&%BinaryPredicate that is used to state the &%length
between an &%Object from its highest point to sea level.  Note that this 
&%Predicate can be used to specify, for example, the height of geographic 
features, e.g. mountains, the altitude of aircraft, and the orbit of satellites 
around the Earth."):-true.
instance(distance, 'SingleValuedRelation'):-true.
instance(distance, 'SpatialRelation'):-true.
instance(distance, 'TernaryPredicate'):-true.
domain(distance, 1, 'Physical'):-true.
domain(distance, 2, 'Physical'):-true.
domain(distance, 3, 'LengthMeasure'):-true.
documentation(distance, "(&%distance ?OBJ1 ?OBJ2 ?QUANT) means that the 
shortest distance between the two objects ?OBJ1 and ?OBJ2 is ?QUANT."):-true.
instance(larger, 'BinaryPredicate'):-true.
instance(larger, 'SpatialRelation'):-true.
instance(larger, 'TransitiveRelation'):-true.
instance(larger, 'IrreflexiveRelation'):-true.
domain(larger, 1, 'Object'):-true.
domain(larger, 2, 'Object'):-true.
documentation(larger, "(&%larger ?OBJ1 ?OBJ2) simply means that ?OBJ1 is 
larger, with respect to all &%LengthMeasures, than ?OBJ2."):-true.
=>([larger, '?OBJ1', '?OBJ2'], [forall, ['?QUANT1', '?QUANT2'], [=>, [and, [measure, '?OBJ1', ['MeasureFn', '?QUANT1', 'LengthMeasure']], [measure, '?OBJ2', ['MeasureFn', '?QUANT2', 'LengthMeasure']]], ['greaterThan', '?QUANT1', '?QUANT2']]]):-true.
instance(smaller, 'BinaryPredicate'):-true.
instance(smaller, 'SpatialRelation'):-true.
instance(smaller, 'TransitiveRelation'):-true.
instance(smaller, 'IrreflexiveRelation'):-true.
domain(smaller, 1, 'Object'):-true.
domain(smaller, 2, 'Object'):-true.
inverse(smaller, larger):-true.
documentation(smaller, "(&%smaller ?OBJ1 ?OBJ2) simply means that ?OBJ1 
is smaller, with respect to all &%LengthMeasures, than ?OBJ2."):-true.
instance('monetaryValue', 'SingleValuedRelation'):-true.
subrelation('monetaryValue', measure):-true.
domain('monetaryValue', 1, 'Object'):-true.
domain('monetaryValue', 2, 'CurrencyMeasure'):-true.
documentation('monetaryValue', "A &%BinaryPredicate that associates an 
&%Object with its value expressed as an instance of &%CurrencyMeasure."):-true.
instance('WealthFn', 'UnaryFunction'):-true.
domain('WealthFn', 1, 'Agent'):-true.
range('WealthFn', 'CurrencyMeasure'):-true.
documentation('WealthFn', "A &%UnaryFunction that maps an &%Agent to a &%CurrencyMeasure specifying the value of the property owned by the &%Agent.  Note that this &%Function is generally used in conjunction with the &%Function &%PropertyFn, e.g. (&%WealthFn (&%PropertyFn BillGates)) would return the monetary value of the sum of Bill Gates' holdings."):-true.
<=>([equal, ['WealthFn', '?PERSON'], '?AMOUNT'], ['monetaryValue', ['PropertyFn', '?PERSON'], '?AMOUNT']):-true.
instance('PositiveInfinity', 'TimePoint'):-true.
documentation('PositiveInfinity', "The &%TimePoint that is after 
all other &%TimePoints."):-true.
=>([and, [instance, '?POINT', 'TimePoint'], [not, [equal, '?POINT', 'PositiveInfinity']]], [before, '?POINT', 'PositiveInfinity']):-true.
=>([and, [instance, '?POINT', 'TimePoint'], [not, [equal, '?POINT', 'PositiveInfinity']]], [exists, ['?OTHERPOINT'], ['temporallyBetween', '?POINT', '?OTHERPOINT', 'PositiveInfinity']]):-true.
instance('NegativeInfinity', 'TimePoint'):-true.
documentation('NegativeInfinity', "The &%TimePoint that is before 
all other &%TimePoints."):-true.
=>([and, [instance, '?POINT', 'TimePoint'], [not, [equal, '?POINT', 'NegativeInfinity']]], [before, 'NegativeInfinity', '?POINT']):-true.
=>([and, [instance, '?POINT', 'TimePoint'], [not, [equal, '?POINT', 'NegativeInfinity']]], [exists, ['?OTHERPOINT'], ['temporallyBetween', 'NegativeInfinity', '?OTHERPOINT', '?POINT']]):-true.
instance(duration, 'BinaryPredicate'):-true.
instance(duration, 'AsymmetricRelation'):-true.
instance(duration, 'TotalValuedRelation'):-true.
domain(duration, 1, 'TimeInterval'):-true.
domain(duration, 2, 'TimeDuration'):-true.
documentation(duration, "(&%duration ?POS ?TIME) means that the 
duration of the &%TimePosition ?POS is ?TIME.  Note that this 
&%Predicate can be used in conjunction with the &%Function &%WhenFn 
to specify the duration of any instance of &%Physical."):-true.
instance(frequency, 'BinaryPredicate'):-true.
instance(frequency, 'AsymmetricRelation'):-true.
'domainSubclass'(frequency, 1, 'Process'):-true.
domain(frequency, 2, 'TimeDuration'):-true.
documentation(frequency, "(frequency ?PROC ?TIME) means that the 
&%Process type of ?PROC recurs after every interval of ?TIME."):-true.
=>([frequency, '?PROC', '?TIME1'], [forall, ['?TIME2'], [=>, [duration, '?TIME2', '?TIME1'], [exists, ['?POSITION'], [and, ['temporalPart', '?POSITION', '?TIME2'], ['holdsDuring', '?POSITION', [exists, ['?INST'], [instance, '?INST', '?PROC']]]]]]]):-true.
instance('temporalPart', 'BinaryPredicate'):-true.
instance('temporalPart', 'AsymmetricRelation'):-true.
domain('temporalPart', 1, 'TimePosition'):-true.
domain('temporalPart', 2, 'TimePosition'):-true.
documentation('temporalPart', "(&%temporalPart ?POS1 ?POS2) 
means that &%TimePosition ?POS1 is part of &%TimePosition ?POS2."):-true.
=>([instance, '?POINT', 'TimePoint'], [exists, ['?INTERVAL'], [and, [instance, '?INTERVAL', 'TimeInterval'], ['temporalPart', '?POINT', '?INTERVAL']]]):-true.
=>([instance, '?INTERVAL', 'TimeInterval'], [exists, ['?POINT'], [and, [instance, '?POINT', 'TimePoint'], ['temporalPart', '?POINT', '?INTERVAL']]]):-true.
=>([and, ['holdsDuring', '?TIME1', '?SITUATION'], ['temporalPart', '?TIME2', '?TIME1']], ['holdsDuring', '?TIME2', '?SITUATION']):-true.
=>([and, ['holdsDuring', '?INTERVAL', [holds, '?REL', '?INST1', '?INST2']], [instance, '?INST1', 'Physical'], [instance, '?INST2', 'Physical']], [and, [time, '?INST1', '?INTERVAL'], [time, '?INST2', '?INTERVAL']]):-true.
<=>(['temporalPart', '?POS', ['WhenFn', '?THING']], [time, '?THING', '?POS']):-true.
instance('BeginFn', 'TemporalRelation'):-true.
instance('BeginFn', 'UnaryFunction'):-true.
domain('BeginFn', 1, 'TimeInterval'):-true.
range('BeginFn', 'TimePoint'):-true.
documentation('BeginFn', "A &%UnaryFunction that maps a &%TimeInterval to 
the &%TimePoint at which the interval begins."):-true.
=>([equal, ['BeginFn', '?INTERVAL'], '?POINT'], [forall, ['?OTHERPOINT'], [=>, [and, ['temporalPart', '?OTHERPOINT', '?INTERVAL'], [not, [equal, '?OTHERPOINT', '?POINT']]], [before, '?POINT', '?OTHERPOINT']]]):-true.
=>([origin, '?PROCESS', '?OBJ'], [located, ['WhereFn', '?PROCESS', ['BeginFn', ['WhenFn', '?PROCESS']]], ['WhereFn', '?OBJ', ['BeginFn', ['WhenFn', '?OBJ']]]]):-true.
instance('EndFn', 'TemporalRelation'):-true.
instance('EndFn', 'UnaryFunction'):-true.
domain('EndFn', 1, 'TimeInterval'):-true.
range('EndFn', 'TimePoint'):-true.
documentation('EndFn', "A &%UnaryFunction that maps a &%TimeInterval to 
the &%TimePoint at which the interval ends."):-true.
=>([equal, ['EndFn', '?INTERVAL'], '?POINT'], [forall, ['?OTHERPOINT'], [=>, [and, ['temporalPart', '?OTHERPOINT', '?INTERVAL'], [not, [equal, '?OTHERPOINT', '?POINT']]], [before, '?OTHERPOINT', '?POINT']]]):-true.
instance(starts, 'BinaryPredicate'):-true.
instance(starts, 'TemporalRelation'):-true.
instance(starts, 'TransitiveRelation'):-true.
instance(starts, 'IrreflexiveRelation'):-true.
domain(starts, 1, 'TimeInterval'):-true.
domain(starts, 2, 'TimeInterval'):-true.
documentation(starts, "(&%starts ?INTERVAL1 ?INTERVAL2) means that 
?INTERVAL1 and ?INTERVAL2 are both &%TimeIntervals that have the same 
initial &%TimePoint and that ?INTERVAL1 ends before ?INTERVAL2."):-true.
<=>([starts, '?INTERVAL1', '?INTERVAL2'], [and, [equal, ['BeginFn', '?INTERVAL1'], ['BeginFn', '?INTERVAL2']], [before, ['EndFn', '?INTERVAL1'], ['EndFn', '?INTERVAL2']]]):-true.
instance(finishes, 'BinaryPredicate'):-true.
instance(finishes, 'TemporalRelation'):-true.
instance(finishes, 'TransitiveRelation'):-true.
instance(finishes, 'IrreflexiveRelation'):-true.
domain(finishes, 1, 'TimeInterval'):-true.
domain(finishes, 2, 'TimeInterval'):-true.
documentation(finishes, "(&%finishes ?INTERVAL1 ?INTERVAL2) means that 
?INTERVAL1 and ?INTERVAL2 are both &%TimeIntervals that have the same 
ending &%TimePoint and that ?INTERVAL2 begins before ?INTERVAL1."):-true.
<=>([finishes, '?INTERVAL1', '?INTERVAL2'], [and, [before, ['BeginFn', '?INTERVAL2'], ['BeginFn', '?INTERVAL1']], [equal, ['EndFn', '?INTERVAL2'], ['EndFn', '?INTERVAL1']]]):-true.
instance(before, 'TemporalRelation'):-true.
instance(before, 'IrreflexiveRelation'):-true.
instance(before, 'TransitiveRelation'):-true.
subrelation(before, 'beforeOrEqual'):-true.
'relatedInternalConcept'(before, earlier):-true.
domain(before, 1, 'TimePoint'):-true.
domain(before, 2, 'TimePoint'):-true.
documentation(before, "(&%before ?POINT1 ?POINT2) means that ?POINT1 
precedes ?POINT2 on the universal timeline."):-true.
=>([instance, '?OBJ', 'Object'], [exists, ['?TIME1', '?TIME2'], [and, [instance, '?TIME1', 'TimePoint'], [instance, '?TIME2', 'TimePoint'], [before, '?TIME1', '?TIME2'], [forall, ['?TIME'], [=>, [and, ['beforeOrEqual', '?TIME1', '?TIME'], ['beforeOrEqual', '?TIME', '?TIME2']], [time, '?OBJ', '?TIME']]]]]):-true.
=>([result, '?PROC', '?OBJ'], [forall, ['?TIME'], [=>, [before, '?TIME', ['BeginFn', ['WhenFn', '?PROC']]], [not, [time, '?OBJ', '?TIME']]]]):-true.
instance('beforeOrEqual', 'BinaryPredicate'):-true.
instance('beforeOrEqual', 'PartialOrderingRelation'):-true.
domain('beforeOrEqual', 1, 'TimePoint'):-true.
domain('beforeOrEqual', 2, 'TimePoint'):-true.
documentation('beforeOrEqual', "(&%beforeOrEqual ?POINT1 ?POINT2) means that ?POINT1 
is identical with ?POINT2 or occurs before it on the universal timeline."):-true.
=>(['beforeOrEqual', '?POINT1', '?POINT2'], [or, [before, '?POINT1', '?POINT2'], [equal, '?POINT1', '?POINT2']]):-true.
instance('temporallyBetween', 'TemporalRelation'):-true.
instance('temporallyBetween', 'TernaryPredicate'):-true.
subrelation('temporallyBetween', 'temporallyBetweenOrEqual'):-true.
domain('temporallyBetween', 1, 'TimePoint'):-true.
domain('temporallyBetween', 2, 'TimePoint'):-true.
domain('temporallyBetween', 3, 'TimePoint'):-true.
documentation('temporallyBetween', "(&%temporallyBetween ?POINT1 ?POINT2 
?POINT3) means that the &%TimePoint ?POINT2 is between the &%TimePoints 
?POINT1 and ?POINT3, i.e. ?POINT1 is before ?POINT2 and ?POINT2 is before 
?POINT3."):-true.
<=>(['temporallyBetween', '?POINT1', '?POINT2', '?POINT3'], [and, [before, '?POINT1', '?POINT2'], [before, '?POINT2', '?POINT3']]):-true.
instance('temporallyBetweenOrEqual', 'TemporalRelation'):-true.
instance('temporallyBetweenOrEqual', 'TernaryPredicate'):-true.
domain('temporallyBetweenOrEqual', 1, 'TimePoint'):-true.
domain('temporallyBetweenOrEqual', 2, 'TimePoint'):-true.
domain('temporallyBetweenOrEqual', 3, 'TimePoint'):-true.
documentation('temporallyBetweenOrEqual', "(&%temporallyBetweenOrEqual ?POINT1 ?POINT2 
?POINT3) means that the &%TimePoint ?POINT1 is before or equal to the 
&%TimePoint ?POINT2 and ?POINT2 is before or equal to the &%TimePoint 
?POINT3."):-true.
<=>(['temporallyBetweenOrEqual', '?POINT1', '?POINT2', '?POINT3'], [and, ['beforeOrEqual', '?POINT1', '?POINT2'], ['beforeOrEqual', '?POINT2', '?POINT3']]):-true.
<=>([and, [time, '?PHYS', '?TIME'], [instance, '?TIME', 'TimePoint']], ['temporallyBetweenOrEqual', ['BeginFn', ['WhenFn', '?PHYS']], '?TIME', ['EndFn', ['WhenFn', '?PHYS']]]):-true.
instance('overlapsTemporally', 'BinaryPredicate'):-true.
instance('overlapsTemporally', 'TemporalRelation'):-true.
instance('overlapsTemporally', 'PartialOrderingRelation'):-true.
domain('overlapsTemporally', 1, 'TimeInterval'):-true.
domain('overlapsTemporally', 2, 'TimeInterval'):-true.
documentation('overlapsTemporally', "(&%overlapsTemporally ?INTERVAL1 
?INTERVAL2) means that the &%TimeInterval ?INTERVAL1 subsumes the 
&%TimeInterval ?INTERVAL2, i.e. the starting point of ?INTERVAL2 is 
equal to or after the starting point of ?INTERVAL1 and the ending 
point of ?INTERVAL2 is equal to or before the ending point of ?INTERVAL1."):-true.
<=>(['overlapsTemporally', '?INTERVAL1', '?INTERVAL2'], [or, [equal, '?INTERVAL1', '?INTERVAL2'], [during, '?INTERVAL1', '?INTERVAL2'], [starts, '?INTERVAL1', '?INTERVAL2'], [finishes, '?INTERVAL1', '?INTERVAL2']]):-true.
=>([and, [instance, '?REL', 'SpatialRelation'], [holds, '?REL', '?OBJ1', '?OBJ2']], ['overlapsTemporally', ['WhenFn', '?OBJ1'], ['WhenFn', '?OBJ2']]):-true.
instance(during, 'TemporalRelation'):-true.
instance(during, 'TransitiveRelation'):-true.
instance(during, 'IrreflexiveRelation'):-true.
subrelation(during, 'overlapsTemporally'):-true.
domain(during, 1, 'TimeInterval'):-true.
domain(during, 2, 'TimeInterval'):-true.
documentation(during, "(&%during ?INTERVAL1 ?INTERVAL2) means that 
?INTERVAL1 starts after and ends before ?INTERVAL2."):-true.
=>([during, '?INTERVAL1', '?INTERVAL2'], [and, [before, ['EndFn', '?INTERVAL1'], ['EndFn', '?INTERVAL2']], [before, ['BeginFn', '?INTERVAL2'], ['BeginFn', '?INTERVAL1']]]):-true.
instance('meetsTemporally', 'BinaryPredicate'):-true.
instance('meetsTemporally', 'TemporalRelation'):-true.
instance('meetsTemporally', 'AsymmetricRelation'):-true.
instance('meetsTemporally', 'IntransitiveRelation'):-true.
domain('meetsTemporally', 1, 'TimeInterval'):-true.
domain('meetsTemporally', 2, 'TimeInterval'):-true.
documentation('meetsTemporally', "(&%meetsTemporally ?INTERVAL1 ?INTERVAL2) 
means that the terminal point of the &%TimeInterval ?INTERVAL1 is the 
initial point of the &%TimeInterval ?INTERVAL2."):-true.
<=>(['meetsTemporally', '?INTERVAL1', '?INTERVAL2'], [equal, ['EndFn', '?INTERVAL1'], ['BeginFn', '?INTERVAL2']]):-true.
=>([and, [equal, ['BeginFn', '?INTERVAL1'], ['BeginFn', '?INTERVAL2']], [equal, ['EndFn', '?INTERVAL1'], ['EndFn', '?INTERVAL2']]], [equal, '?INTERVAL1', '?INTERVAL2']):-true.
instance(earlier, 'BinaryPredicate'):-true.
instance(earlier, 'TemporalRelation'):-true.
instance(earlier, 'TransitiveRelation'):-true.
instance(earlier, 'IrreflexiveRelation'):-true.
domain(earlier, 1, 'TimeInterval'):-true.
domain(earlier, 2, 'TimeInterval'):-true.
documentation(earlier, "(earlier INTERVAL1 INTERVAL2) means that 
INTERVAL1 ends before INTERVAL2 begins."):-true.
<=>([earlier, '?INTERVAL1', '?INTERVAL2'], [before, ['EndFn', '?INTERVAL1'], ['BeginFn', '?INTERVAL2']]):-true.
instance(cooccur, 'BinaryPredicate'):-true.
instance(cooccur, 'TemporalRelation'):-true.
instance(cooccur, 'EquivalenceRelation'):-true.
domain(cooccur, 1, 'Physical'):-true.
domain(cooccur, 2, 'Physical'):-true.
documentation(cooccur, "(&%cooccur ?THING1 ?THING2) means that the 
&%Object or &%Process ?THING1 occurs at the same time as, together with, 
or jointly with the &%Object or &%Process ?THING2.  This covers the 
following temporal relations:  is co-incident with, is concurrent with, 
is contemporaneous with, and is concomitant with."):-true.
<=>([cooccur, '?PHYS1', '?PHYS2'], [equal, ['WhenFn', '?PHYS1'], ['WhenFn', '?PHYS2']]):-true.
instance('TimeIntervalFn', 'BinaryFunction'):-true.
instance('TimeIntervalFn', 'TemporalRelation'):-true.
domain('TimeIntervalFn', 1, 'TimePoint'):-true.
domain('TimeIntervalFn', 2, 'TimePoint'):-true.
range('TimeIntervalFn', 'TimeInterval'):-true.
documentation('TimeIntervalFn', "A &%BinaryFunction that takes two &%TimePoints 
as arguments and returns the &%TimeInterval defined by these two &%TimePoints.  
Note that the first &%TimePoint must occur earlier than the second &%TimePoint."):-true.
=>([equal, ['TimeIntervalFn', '?POINT1', '?POINT2'], '?INTERVAL'], [and, [equal, ['BeginFn', '?INTERVAL'], '?POINT1'], [equal, ['EndFn', '?INTERVAL'], '?POINT2']]):-true.
=>([equal, ['TimeIntervalFn', '?POINT1', '?POINT2'], '?INTERVAL'], [forall, ['?POINT'], [<=>, ['temporallyBetweenOrEqual', '?POINT1', '?POINT', '?POINT2'], ['temporalPart', '?POINT', '?INTERVAL']]]):-true.
instance('RecurrentTimeIntervalFn', 'TemporalRelation'):-true.
instance('RecurrentTimeIntervalFn', 'BinaryFunction'):-true.
'domainSubclass'('RecurrentTimeIntervalFn', 1, 'TimeInterval'):-true.
'domainSubclass'('RecurrentTimeIntervalFn', 2, 'TimeInterval'):-true.
'rangeSubclass'('RecurrentTimeIntervalFn', 'TimeInterval'):-true.
documentation('RecurrentTimeIntervalFn', "A function that is useful for generating 
recurring time intervals.  For example, (&%RecurrentTimeIntervalFn (&%HourFn 6 &%Day)  
(&%HourFn 12 &%Day)) returns the &%Class of &%TimeIntervals beginning at 6 in the 
morning and ending at 12 noon.  For another example, (&%RecurrentTimeInterval 
&%Saturday &%Sunday) returns the &%Class of all weekends.  For still another example, 
(&%RecurrentTimeInterval &%June &%August) returns the &%Class containing the academic 
summer period."):-true.
=>([instance, '?INTERVAL', ['RecurrentTimeIntervalFn', '?TIMECLASS1', '?TIMECLASS2']], [exists, ['?TIME1', '?TIME2'], [and, [instance, '?TIME1', '?TIMECLASS1'], [instance, '?TIME2', '?TIMECLASS2'], [starts, '?TIME1', '?INTERVAL'], [finishes, '?TIME2', '?INTERVAL']]]):-true.
instance('WhenFn', 'TemporalRelation'):-true.
instance('WhenFn', 'UnaryFunction'):-true.
domain('WhenFn', 1, 'Physical'):-true.
range('WhenFn', 'TimeInterval'):-true.
documentation('WhenFn', "A &%UnaryFunction that maps an &%Object or 
&%Process to the exact &%TimeInterval during which it exists.  Note 
that, for every &%TimePoint ?TIME outside of the &%TimeInterval 
(WhenFn ?THING), (time ?THING ?TIME) does not hold."):-true.
before(['BeginFn', ['WhenFn', '?THING']], ['EndFn', ['WhenFn', '?THING']]):-true.
instance('PastFn', 'TemporalRelation'):-true.
instance('PastFn', 'UnaryFunction'):-true.
domain('PastFn', 1, 'TimePosition'):-true.
range('PastFn', 'TimeInterval'):-true.
documentation('PastFn', "A &%UnaryFunction that maps a &%TimePosition 
to the &%TimeInterval that meets it and that begins at 
&%NegativeInfinity."):-true.
'meetsTemporally'(['PastFn', ['WhenFn', '?THING']], ['WhenFn', '?THING']):-true.
equal(['PastFn', ['WhenFn', '?PROCESS']], ['TimeIntervalFn', 'NegativeInfinity', ['BeginFn', ['WhenFn', '?PROCESS']]]):-true.
instance('ImmediatePastFn', 'TemporalRelation'):-true.
instance('ImmediatePastFn', 'UnaryFunction'):-true.
domain('ImmediatePastFn', 1, 'TimePosition'):-true.
range('ImmediatePastFn', 'TimeInterval'):-true.
documentation('ImmediatePastFn', "A &%UnaryFunction that maps a 
&%TimePosition to a short, indeterminate &%TimeInterval that 
immediately precedes the &%TimePosition."):-true.
finishes(['ImmediatePastFn', ['WhenFn', '?THING']], ['PastFn', ['WhenFn', '?THING']]):-true.
=>([and, [resource, '?PROC', '?OBJ'], ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?PROC']], [measure, '?OBJ', '?QUANT1']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?PROC']], [measure, '?OBJ', '?QUANT2']]], ['greaterThan', '?QUANT1', '?QUANT2']):-true.
instance('FutureFn', 'TemporalRelation'):-true.
instance('FutureFn', 'UnaryFunction'):-true.
domain('FutureFn', 1, 'TimePosition'):-true.
range('FutureFn', 'TimeInterval'):-true.
documentation('FutureFn', "A &%UnaryFunction that maps a &%TimePosition 
to the &%TimeInterval which it meets and which ends at 
&%PositiveInfinity."):-true.
'meetsTemporally'(['WhenFn', '?THING'], ['FutureFn', ['WhenFn', '?THING']]):-true.
equal(['FutureFn', ['WhenFn', '?PROCESS']], ['TimeIntervalFn', ['EndFn', ['WhenFn', '?PROCESS']], 'PositiveInfinity']):-true.
instance('ImmediateFutureFn', 'TemporalRelation'):-true.
instance('ImmediateFutureFn', 'UnaryFunction'):-true.
domain('ImmediateFutureFn', 1, 'TimePosition'):-true.
range('ImmediateFutureFn', 'TimeInterval'):-true.
documentation('ImmediateFutureFn', "A &%UnaryFunction that maps a 
&%TimePosition to a short, indeterminate &%TimeInterval that 
immediately follows the &%TimePosition."):-true.
starts(['ImmediateFutureFn', ['WhenFn', '?THING']], ['FutureFn', ['WhenFn', '?THING']]):-true.
instance(date, 'BinaryPredicate'):-true.
instance(date, 'SingleValuedRelation'):-true.
instance(date, 'AsymmetricRelation'):-true.
domain(date, 1, 'Physical'):-true.
domain(date, 2, 'Day'):-true.
subrelation(date, time):-true.
documentation(date, "A &%BinaryPredicate that specifies a 
&%TimePosition in absolute calendar time, at the resolution 
of one day, for a particular &%Object or &%Process."):-true.
instance('YearFn', 'TemporalRelation'):-true.
instance('YearFn', 'UnaryFunction'):-true.
domain('YearFn', 1, 'Integer'):-true.
'rangeSubclass'('YearFn', 'Year'):-true.
documentation('YearFn', "A &%UnaryFunction that maps a number to the corresponding calendar 
&%Year.  For example, (&%YearFn 1912) returns the &%Class containing just one instance, 
the year of 1912.  As might be expected, positive integers return years in the Common Era, 
while negative integers return years in B.C.E.  Note that this function returns a &%Class 
as a value.  The reason for this is that the related functions, viz. &%MonthFn, &%DayFn, 
&%HourFn, &%MinuteFn, and &%SecondFn, are used to generate both specific &%TimeIntervals 
and recurrent intervals, and the only way to do this is to make the domains and ranges of 
these functions classes rather than individuals."):-true.
instance('MonthFn', 'TemporalRelation'):-true.
instance('MonthFn', 'BinaryFunction'):-true.
'domainSubclass'('MonthFn', 1, 'Month'):-true.
'domainSubclass'('MonthFn', 2, 'Year'):-true.
'rangeSubclass'('MonthFn', 'Month'):-true.
documentation('MonthFn', "A &%BinaryFunction that maps a subclass of &%Month and a 
subclass of &%Year to the class containing the &%Months corresponding to thos &%Years. 
For example (&%MonthFn &%January (&%YearFn 1912)) is the class containing the eighth 
&%Month, i.e. August, of the &%Year 1912.  For another example, (&%MonthFn &%August 
&%Year) is equal to &%August, the class of all months of August.  Note that this function 
returns a &%Class as a value.  The reason for this is that the related functions, viz. 
DayFn, HourFn, MinuteFn, and SecondFn, are used to generate both specific &%TimeIntervals 
and recurrent intervals, and the only way to do this is to make the domains and ranges of 
these functions classes rather than individuals."):-true.
instance('DayFn', 'TemporalRelation'):-true.
instance('DayFn', 'BinaryFunction'):-true.
domain('DayFn', 1, 'PositiveRealNumber'):-true.
'domainSubclass'('DayFn', 2, 'Month'):-true.
'rangeSubclass'('DayFn', 'Day'):-true.
documentation('DayFn', "A &%BinaryFunction that assigns a &%PositiveRealNumber and 
a subclass of &%Months to the &%Days within each &%Month corresponding to that 
&%PositiveRealNumber.  For example, (&%DayFn 16 &%August) is the &%Class of all 
sixteenth days of August.  For another example, (&%DayFn 9 &%Month) would return 
the class of all ninth days of any month.  For still another example, (&%DayFn 18 
(&%MonthFn 8 (YearFn 1912))) denotes the 18th day of August 1912."):-true.
=>([instance, '?DAY', ['DayFn', '?NUMBER', '?MONTH']], ['lessThanOrEqualTo', '?NUMBER', 31]):-true.
=>([and, [instance, '?DAY1', ['DayFn', '?NUMBER1', '?MONTH']], [instance, '?DAY2', ['DayFn', '?NUMBER2', '?MONTH']], [equal, ['SubtractionFn', '?NUMBER2', '?NUMBER1'], 1]], ['meetsTemporally', '?DAY1', '?DAY2']):-true.
instance('HourFn', 'TemporalRelation'):-true.
instance('HourFn', 'BinaryFunction'):-true.
domain('HourFn', 1, 'PositiveRealNumber'):-true.
'domainSubclass'('HourFn', 2, 'Day'):-true.
'rangeSubclass'('HourFn', 'Hour'):-true.
documentation('HourFn', "A &%BinaryFunction that assigns a &%PositiveRealNumber and 
a subclass of &%Days to the &%Hours within each &%Day corresponding to that 
&%PositiveRealNumber.  For example, (&%HourFn 12 &%Thursday) is the &%Class of all 
instances of noon Thursday.  For another example, (&%HourFn 24 &%Day) would return 
the class of all instances of midnight.  For still another example, (&%HourFn 14 
(&%DayFn 18 (&%MonthFn 8 (YearFn 1912)))) denotes 2 PM on the 18th day of August 
1912."):-true.
=>([instance, '?HOUR', ['HourFn', '?NUMBER', '?DAY']], ['lessThan', '?NUMBER', 24]):-true.
=>([and, [instance, '?HOUR1', ['HourFn', '?NUMBER1', '?DAY']], [instance, '?HOUR2', ['HourFn', '?NUMBER2', '?DAY']], [equal, ['SubtractionFn', '?NUMBER2', '?NUMBER1'], 1]], ['meetsTemporally', '?HOUR1', '?HOUR2']):-true.
instance('MinuteFn', 'TemporalRelation'):-true.
instance('MinuteFn', 'BinaryFunction'):-true.
domain('MinuteFn', 1, 'PositiveRealNumber'):-true.
'domainSubclass'('MinuteFn', 2, 'Hour'):-true.
'rangeSubclass'('MinuteFn', 'Minute'):-true.
documentation('MinuteFn', "A &%BinaryFunction that assigns a &%PositiveRealNumber and 
a subclass of &%Hours to the &%Minutes within each &%Hour corresponding to that 
&%PositiveRealNumber.  For example, (&%MinuteFn 30 (&%HourFn 17 &%Day)) is the &%Class 
of all 5:30's in the afternoon.  For another example, (&%MinuteFn 15 &%Hour) would return 
the class of all instances of quarter past the hour.  For still another example, 
(&%MinuteFn 15 (&%HourFn 14 (&%DayFn 18 (&%MonthFn 8 (YearFn 1912))))) denotes 15 
minutes after 2 PM on the 18th day of August 1912."):-true.
=>([instance, '?MINUTE', ['MinuteFn', '?NUMBER', '?HOUR']], ['lessThan', '?NUMBER', 60]):-true.
=>([and, [instance, '?MINUTE1', ['MinuteFn', '?NUMBER1', '?HOUR']], [instance, '?MINUTE2', ['MinuteFn', '?NUMBER2', '?HOUR']], [equal, ['SubtractionFn', '?NUMBER2', '?NUMBER1'], 1]], ['meetsTemporally', '?MINUTE1', '?MINUTE2']):-true.
instance('SecondFn', 'TemporalRelation'):-true.
instance('SecondFn', 'BinaryFunction'):-true.
domain('SecondFn', 1, 'PositiveRealNumber'):-true.
'domainSubclass'('SecondFn', 2, 'Minute'):-true.
'rangeSubclass'('SecondFn', 'Second'):-true.
documentation('SecondFn', "A &%BinaryFunction that assigns a &%PositiveRealNumber and a 
subclass of &%Minutes to the &%Seconds within each &%Minute corresponding to that 
&%PositiveRealNumber.  For example, (&%SecondFn 4 (&%MinuteFn 5 &%Hour)) is the &%Class 
of all fourth &%Seconds of every fifth &%Minute of every hour.  For another example, 
(&%SecondFn 8 &%Minute) would return the eighth second of every minute.  For still 
another example, (&%SecondFn 9 (&%MinuteFn 15 (&%HourFn 14 (&%DayFn 18 (&%MonthFn 8 
(YearFn 1912)))))) denotes 9 seconds and 15 minutes after 2 PM on the 18th day of 
August 1912."):-true.
=>([instance, '?SECOND', ['SecondFn', '?NUMBER', '?MINUTE']], ['lessThan', '?NUMBER', 60]):-true.
=>([and, [instance, '?SECOND1', ['SecondFn', '?NUMBER1', '?MINUTE']], [instance, '?SECOND2', ['SecondFn', '?NUMBER2', '?MINUTE']], [equal, ['SubtractionFn', '?NUMBER2', '?NUMBER1'], 1]], ['meetsTemporally', '?SECOND1', '?SECOND2']):-true.
subclass('Year', 'TimeInterval'):-true.
'relatedInternalConcept'('Year', 'YearFn'):-true.
'relatedInternalConcept'('Year', 'YearDuration'):-true.
documentation('Year', "The &%Class of all calendar &%Years."):-true.
=>([instance, '?YEAR', 'Year'], [duration, '?YEAR', ['MeasureFn', 1, 'YearDuration']]):-true.
=>([and, [instance, '?YEAR1', 'Year'], [instance, '?YEAR2', 'Year'], [equal, ['SubtractionFn', '?YEAR2', '?YEAR1'], 1]], ['meetsTemporally', '?YEAR1', '?YEAR2']):-true.
subclass('LeapYear', 'Year'):-true.
documentation('LeapYear', "The &%Class of all leap years.  These are years 
which are either (i.) evenly divisible by 4 and not by 100 or (ii.) evenly 
divisible by 400 (this latter case is known as a leap century)."):-true.
=>([and, [instance, '?LEAP', 'LeapYear'], [equal, '?LEAP', ['MeasureFn', '?NUMBER', 'Year']]], [or, [and, [equal, ['RemainderFn', '?NUMBER', 4], 0], [not, [equal, ['RemainderFn', '?NUMBER', 100], 0]]], [equal, ['RemainderFn', '?NUMBER', 400], 0]]):-true.
subclass('Month', 'TimeInterval'):-true.
'relatedInternalConcept'('Month', 'MonthFn'):-true.
documentation('Month', "The &%Class of all calendar &%Months."):-true.
subclass('January', 'Month'):-true.
documentation('January', "The &%Class of all &%Months which are January."):-true.
=>([instance, '?MONTH', 'January'], [duration, '?MONTH', ['MeasureFn', 31, 'DayDuration']]):-true.
=>([and, [equal, '?MONTH1', ['MonthFn', 'January', '?YEAR']], [equal, '?MONTH2', ['MonthFn', 'February', '?YEAR']]], ['meetsTemporally', '?MONTH1', '?MONTH2']):-true.
subclass('February', 'Month'):-true.
documentation('February', "The &%Class of all &%Months which are February."):-true.
=>([and, [equal, ['MonthFn', 'February', '?YEAR'], '?MONTH'], [not, [instance, '?YEAR', 'LeapYear']]], [duration, '?MONTH', ['MeasureFn', 28, 'DayDuration']]):-true.
=>([and, [equal, ['MonthFn', 'February', '?YEAR'], '?MONTH'], [instance, '?YEAR', 'LeapYear']], [duration, '?MONTH', ['MeasureFn', 29, 'DayDuration']]):-true.
=>([and, [equal, '?MONTH1', ['MonthFn', 'February', '?YEAR']], [equal, '?MONTH2', ['MonthFn', 'March', '?YEAR']]], ['meetsTemporally', '?MONTH1', '?MONTH2']):-true.
subclass('March', 'Month'):-true.
documentation('March', "The &%Class of all &%Months which are March."):-true.
=>([instance, '?MONTH', 'March'], [duration, '?MONTH', ['MeasureFn', 31, 'DayDuration']]):-true.
=>([and, [equal, '?MONTH1', ['MonthFn', 'March', '?YEAR']], [equal, '?MONTH2', ['MonthFn', 'April', '?YEAR']]], ['meetsTemporally', '?MONTH1', '?MONTH2']):-true.
subclass('April', 'Month'):-true.
documentation('April', "The &%Class of all &%Months which are April."):-true.
=>([instance, '?MONTH', 'April'], [duration, '?MONTH', ['MeasureFn', 30, 'DayDuration']]):-true.
=>([and, [equal, '?MONTH1', ['MonthFn', 'April', '?YEAR']], [equal, '?MONTH2', ['MonthFn', 'May', '?YEAR']]], ['meetsTemporally', '?MONTH1', '?MONTH2']):-true.
subclass('May', 'Month'):-true.
documentation('May', "The &%Class of all &%Months which are May."):-true.
=>([instance, '?MONTH', 'May'], [duration, '?MONTH', ['MeasureFn', 31, 'DayDuration']]):-true.
=>([and, [equal, '?MONTH1', ['MonthFn', 'May', '?YEAR']], [equal, '?MONTH2', ['MonthFn', 'June', '?YEAR']]], ['meetsTemporally', '?MONTH1', '?MONTH2']):-true.
subclass('June', 'Month'):-true.
documentation('June', "The &%Class of all &%Months which are June."):-true.
=>([instance, '?MONTH', 'June'], [duration, '?MONTH', ['MeasureFn', 30, 'DayDuration']]):-true.
=>([and, [equal, '?MONTH1', ['MonthFn', 'June', '?YEAR']], [equal, '?MONTH2', ['MonthFn', 'July', '?YEAR']]], ['meetsTemporally', '?MONTH1', '?MONTH2']):-true.
subclass('July', 'Month'):-true.
documentation('July', "The &%Class of all &%Months which are July."):-true.
=>([instance, '?MONTH', 'July'], [duration, '?MONTH', ['MeasureFn', 31, 'DayDuration']]):-true.
=>([and, [equal, '?MONTH1', ['MonthFn', 'July', '?YEAR']], [equal, '?MONTH2', ['MonthFn', 'August', '?YEAR']]], ['meetsTemporally', '?MONTH1', '?MONTH2']):-true.
subclass('August', 'Month'):-true.
documentation('August', "The &%Class of all &%Months which are August."):-true.
=>([instance, '?MONTH', 'August'], [duration, '?MONTH', ['MeasureFn', 31, 'DayDuration']]):-true.
=>([and, [equal, '?MONTH1', ['MonthFn', 'August', '?YEAR']], [equal, '?MONTH2', ['MonthFn', 'September', '?YEAR']]], ['meetsTemporally', '?MONTH1', '?MONTH2']):-true.
subclass('September', 'Month'):-true.
documentation('September', "The &%Class of all &%Months which are September."):-true.
=>([instance, '?MONTH', 'September'], [duration, '?MONTH', ['MeasureFn', 30, 'DayDuration']]):-true.
=>([and, [equal, '?MONTH1', ['MonthFn', 'September', '?YEAR']], [equal, '?MONTH2', ['MonthFn', 'October', '?YEAR']]], ['meetsTemporally', '?MONTH1', '?MONTH2']):-true.
subclass('October', 'Month'):-true.
documentation('October', "The &%Class of all &%Months which are October."):-true.
=>([instance, '?MONTH', 'October'], [duration, '?MONTH', ['MeasureFn', 31, 'DayDuration']]):-true.
=>([and, [equal, '?MONTH1', ['MonthFn', 'October', '?YEAR']], [equal, '?MONTH2', ['MonthFn', 'November', '?YEAR']]], ['meetsTemporally', '?MONTH1', '?MONTH2']):-true.
subclass('November', 'Month'):-true.
documentation('November', "The &%Class of all &%Months which are November."):-true.
=>([instance, '?MONTH', 'November'], [duration, '?MONTH', ['MeasureFn', 30, 'DayDuration']]):-true.
=>([and, [equal, '?MONTH1', ['MonthFn', 'November', '?YEAR']], [equal, '?MONTH2', ['MonthFn', 'December', '?YEAR']]], ['meetsTemporally', '?MONTH1', '?MONTH2']):-true.
subclass('December', 'Month'):-true.
documentation('December', "The &%Class of all &%Months which are December."):-true.
=>([instance, '?MONTH', 'December'], [duration, '?MONTH', ['MeasureFn', 31, 'DayDuration']]):-true.
=>([and, [equal, '?MONTH1', ['MonthFn', 'December', '?YEAR1']], [equal, '?MONTH2', ['MonthFn', 'January', '?YEAR2']], ['meetsTemporally', '?YEAR1', '?YEAR2']], ['meetsTemporally', '?MONTH1', '?MONTH2']):-true.
subclass('Day', 'TimeInterval'):-true.
'relatedInternalConcept'('Day', 'DayFn'):-true.
'relatedInternalConcept'('Day', 'DayDuration'):-true.
documentation('Day', "The &%Class of all calendar &%Days."):-true.
=>([instance, '?DAY', 'Day'], [duration, '?DAY', ['MeasureFn', 1, 'DayDuration']]):-true.
subclass('Monday', 'Day'):-true.
documentation('Monday', "The &%Class of all calendar Mondays."):-true.
subclass('Tuesday', 'Day'):-true.
documentation('Tuesday', "The &%Class of all calendar Tuesdays."):-true.
=>([and, [instance, '?DAY1', 'Monday'], [instance, '?DAY2', 'Tuesday'], [instance, '?WEEK', 'Week'], ['temporalPart', '?DAY1', '?WEEK'], ['temporalPart', '?DAY2', '?WEEK']], ['meetsTemporally', '?DAY1', '?DAY2']):-true.
subclass('Wednesday', 'Day'):-true.
documentation('Wednesday', "The &%Class of all calendar Wednesdays."):-true.
=>([and, [instance, '?DAY1', 'Tuesday'], [instance, '?DAY2', 'Wednesday'], [instance, '?WEEK', 'Week'], ['temporalPart', '?DAY1', '?WEEK'], ['temporalPart', '?DAY2', '?WEEK']], ['meetsTemporally', '?DAY1', '?DAY2']):-true.
subclass('Thursday', 'Day'):-true.
documentation('Thursday', "The &%Class of all calendar Thursdays."):-true.
=>([and, [instance, '?DAY1', 'Wednesday'], [instance, '?DAY2', 'Thursday'], [instance, '?WEEK', 'Week'], ['temporalPart', '?DAY1', '?WEEK'], ['temporalPart', '?DAY2', '?WEEK']], ['meetsTemporally', '?DAY1', '?DAY2']):-true.
subclass('Friday', 'Day'):-true.
documentation('Friday', "The &%Class of all calendar Fridays."):-true.
=>([and, [instance, '?DAY1', 'Thursday'], [instance, '?DAY2', 'Friday'], [instance, '?WEEK', 'Week'], ['temporalPart', '?DAY1', '?WEEK'], ['temporalPart', '?DAY2', '?WEEK']], ['meetsTemporally', '?DAY1', '?DAY2']):-true.
subclass('Saturday', 'Day'):-true.
documentation('Saturday', "The &%Class of all calendar Saturdays."):-true.
=>([and, [instance, '?DAY1', 'Friday'], [instance, '?DAY2', 'Saturday'], [instance, '?WEEK', 'Week'], ['temporalPart', '?DAY1', '?WEEK'], ['temporalPart', '?DAY2', '?WEEK']], ['meetsTemporally', '?DAY1', '?DAY2']):-true.
subclass('Sunday', 'Day'):-true.
documentation('Sunday', "The &%Class of all calendar Sundays."):-true.
=>([and, [instance, '?DAY1', 'Saturday'], [instance, '?DAY2', 'Sunday'], [instance, '?WEEK', 'Week'], ['temporalPart', '?DAY1', '?WEEK'], ['temporalPart', '?DAY2', '?WEEK']], ['meetsTemporally', '?DAY1', '?DAY2']):-true.
=>([and, [instance, '?DAY1', 'Sunday'], [instance, '?DAY2', 'Monday'], [instance, '?WEEK1', 'Week'], [instance, '?WEEK2', 'Week'], ['temporalPart', '?DAY1', '?WEEK1'], ['temporalPart', '?DAY2', '?WEEK2'], ['meetsTemporally', '?WEEK1', '?WEEK2']], ['meetsTemporally', '?DAY1', '?DAY2']):-true.
subclass('Week', 'TimeInterval'):-true.
documentation('Week', "The &%Class of all calendar weeks."):-true.
=>([instance, '?WEEK', 'Week'], [duration, '?WEEK', ['MeasureFn', 1, 'WeekDuration']]):-true.
subclass('Hour', 'TimeInterval'):-true.
'relatedInternalConcept'('Hour', 'HourFn'):-true.
'relatedInternalConcept'('Hour', 'HourDuration'):-true.
documentation('Hour', "The &%Class of all clock &%Hours."):-true.
=>([instance, '?HOUR', 'Hour'], [duration, '?HOUR', ['MeasureFn', 1, 'HourDuration']]):-true.
subclass('Minute', 'TimeInterval'):-true.
'relatedInternalConcept'('Minute', 'MinuteFn'):-true.
'relatedInternalConcept'('Minute', 'MinuteDuration'):-true.
documentation('Minute', "The &%Class of all clock &%Minutes."):-true.
=>([instance, '?MINUTE', 'Minute'], [duration, '?MINUTE', ['MeasureFn', 1, 'MinuteDuration']]):-true.
subclass('Second', 'TimeInterval'):-true.
'relatedInternalConcept'('Second', 'SecondDuration'):-true.
'relatedInternalConcept'('Second', 'SecondFn'):-true.
documentation('Second', "The &%Class of all clock &%Seconds."):-true.
=>([instance, '?SECOND', 'Second'], [duration, '?SECOND', ['MeasureFn', 1, 'SecondDuration']]):-true.
instance('TemporalCompositionFn', 'TemporalRelation'):-true.
instance('TemporalCompositionFn', 'BinaryFunction'):-true.
domain('TemporalCompositionFn', 1, 'TimeInterval'):-true.
'domainSubclass'('TemporalCompositionFn', 2, 'TimeInterval'):-true.
'rangeSubclass'('TemporalCompositionFn', 'TimeInterval'):-true.
documentation('TemporalCompositionFn', "The basic &%Function for expressing 
the composition of larger &%TimeIntervals out of smaller &%TimeIntervals.  
For example, if &%ThisSeptember is an &%instance of &%September, 
(&%TemporalCompositionFn &%ThisSeptember &%Day) denotes the &%Class of 
consecutive days that make up &%ThisSeptember.  Note that one can obtain 
the number of instances of this &%Class by using the function &%CardinalityFn."):-true.
=>([equal, ['TemporalCompositionFn', '?INTERVAL', '?INTERVAL-TYPE'], '?CLASS'], [forall, ['?TIME1', '?TIME2'], [=>, [and, [instance, '?TIME1', '?INTERVAL-TYPE'], [instance, '?TIME2', '?CLASS']], [exists, ['?DURATION'], [and, [duration, '?TIME1', '?DURATION'], [duration, '?TIME2', '?DURATION']]]]]):-true.
=>([equal, ['TemporalCompositionFn', '?INTERVAL', '?INTERVAL-TYPE'], '?CLASS'], [forall, ['?TIME1', '?TIME2'], [=>, [and, [instance, '?TIME1', '?CLASS'], [instance, '?TIME2', '?CLASS'], [not, [equal, '?TIME1', '?TIME2']]], [or, ['meetsTemporally', '?TIME1', '?TIME2'], ['meetsTemporally', '?TIME2', '?TIME1'], [earlier, '?TIME1', '?TIME2'], [earlier, '?TIME2', '?TIME1']]]]):-true.
=>([equal, ['TemporalCompositionFn', '?INTERVAL', '?INTERVAL-TYPE'], '?CLASS'], [exists, ['?TIME'], [and, [instance, '?TIME', '?CLASS'], [starts, '?TIME', '?INTERVAL']]]):-true.
=>([equal, ['TemporalCompositionFn', '?INTERVAL', '?INTERVAL-TYPE'], '?CLASS'], [exists, ['?TIME'], [and, [instance, '?TIME', '?CLASS'], [finishes, '?TIME', '?INTERVAL']]]):-true.
=>([equal, ['TemporalCompositionFn', '?INTERVAL', '?INTERVAL-TYPE'], '?CLASS'], [forall, ['?TIME1'], [=>, [and, [instance, '?TIME1', '?CLASS'], [not, [finishes, '?TIME1', '?INTERVAL']]], [exists, ['?TIME2'], [and, [instance, '?TIME2', '?CLASS'], ['meetsTemporally', '?TIME1', '?TIME2']]]]]):-true.
=>([equal, ['TemporalCompositionFn', '?INTERVAL', '?INTERVAL-TYPE'], '?CLASS'], [forall, ['?TIME1'], [=>, [and, [instance, '?TIME1', '?CLASS'], [not, [starts, '?TIME1', '?INTERVAL']]], [exists, ['?TIME2'], [and, [instance, '?TIME2', '?CLASS'], ['meetsTemporally', '?TIME2', '?TIME1']]]]]):-true.
=>([equal, ['TemporalCompositionFn', '?INTERVAL', '?INTERVAL-TYPE'], '?CLASS'], [forall, ['?TIME'], [=>, [and, [instance, '?TIME', 'TimePoint'], ['temporalPart', '?TIME', '?INTERVAL']], [exists, ['?INSTANCE'], [and, [instance, '?INSTANCE', '?CLASS'], ['temporalPart', '?TIME', '?INSTANCE']]]]]):-true.
=>([instance, '?YEAR', 'Year'], [equal, ['CardinalityFn', ['TemporalCompositionFn', '?YEAR', 'Month']], 12]):-true.
=>([and, [instance, '?MONTH', 'Month'], [duration, '?MONTH', ['MeasureFn', '?NUMBER', 'DayDuration']]], [equal, ['CardinalityFn', ['TemporalCompositionFn', '?MONTH', 'Day']], '?NUMBER']):-true.
=>([instance, '?WEEK', 'Week'], [equal, ['CardinalityFn', ['TemporalCompositionFn', '?WEEK', 'Day']], 7]):-true.
=>([instance, '?DAY', 'Day'], [equal, ['CardinalityFn', ['TemporalCompositionFn', '?DAY', 'Hour']], 24]):-true.
=>([instance, '?HOUR', 'Hour'], [equal, ['CardinalityFn', ['TemporalCompositionFn', '?HOUR', 'Minute']], 60]):-true.
=>([instance, '?MINUTE', 'Minute'], [equal, ['CardinalityFn', ['TemporalCompositionFn', '?MINUTE', 'Second']], 60]):-true.
instance(connected, 'BinaryPredicate'):-true.
instance(connected, 'SpatialRelation'):-true.
instance(connected, 'ReflexiveRelation'):-true.
instance(connected, 'SymmetricRelation'):-true.
domain(connected, 1, 'Object'):-true.
domain(connected, 2, 'Object'):-true.
documentation(connected, "(connected ?OBJ1 ?OBJ2) means that ?OBJ1 
&%meetsSpatially ?OBJ2 or that ?OBJ1 &%overlapsSpatially ?OBJ2."):-true.
=>([connected, '?OBJ1', '?OBJ2'], [or, ['meetsSpatially', '?OBJ1', '?OBJ2'], ['overlapsSpatially', '?OBJ1', '?OBJ2']]):-true.
<=>([instance, '?OBJ', 'SelfConnectedObject'], [forall, ['?PART1', '?PART2'], [=>, [equal, '?OBJ', ['MereologicalSumFn', '?PART1', '?PART2']], [connected, '?PART1', '?PART2']]]):-true.
instance(connects, 'SpatialRelation'):-true.
instance(connects, 'TernaryPredicate'):-true.
domain(connects, 1, 'SelfConnectedObject'):-true.
domain(connects, 2, 'SelfConnectedObject'):-true.
domain(connects, 3, 'SelfConnectedObject'):-true.
documentation(connects, "The relationship between three things, when one of 
the three things connects the other two.  More formally, (&%connects ?OBJ1 
?OBJ2 ?OBJ3) means that (&%connected ?OBJ1 ?OBJ2) and (&%connected ?OBJ1 ?OBJ3) 
and not (&%connected ?OBJ2 ?OBJ3)."):-true.
<=>([connects, '?OBJ1', '?OBJ2', '?OBJ3'], [and, [connected, '?OBJ1', '?OBJ2'], [connected, '?OBJ1', '?OBJ3'], [not, [connected, '?OBJ2', '?OBJ3']]]):-true.
subrelation('meetsSpatially', connected):-true.
instance('meetsSpatially', 'IrreflexiveRelation'):-true.
instance('meetsSpatially', 'SymmetricRelation'):-true.
'disjointRelation'('meetsSpatially', 'overlapsSpatially'):-true.
documentation('meetsSpatially', "(&%meetsSpatially ?OBJ1 ?OBJ2) means that 
?OBJ1 and ?OBJ2 are &%connected but that neither ?OBJ1 nor ?OBJ2 
&%overlapsSpatially the other."):-true.
subrelation('overlapsSpatially', connected):-true.
instance('overlapsSpatially', 'ReflexiveRelation'):-true.
instance('overlapsSpatially', 'SymmetricRelation'):-true.
documentation('overlapsSpatially', "(&%overlapsSpatially ?OBJ1 ?OBJ2) means 
that the &%Objects ?OBJ1 and ?OBJ2 have some parts in common.  This is a 
reflexive and symmetric (but not transitive) relation."):-true.
<=>(['overlapsSpatially', '?OBJ1', '?OBJ2'], [exists, ['?OBJ3'], [and, [part, '?OBJ3', '?OBJ1'], [part, '?OBJ3', '?OBJ2']]]):-true.
=>(['partlyLocated', '?OBJ', '?REGION'], ['overlapsSpatially', '?OBJ', '?REGION']):-true.
=>([and, [member, '?OBJ1', '?COLL'], [member, '?OBJ2', '?COLL']], [not, ['overlapsSpatially', '?OBJ1', '?OBJ2']]):-true.
=>([and, [instance, '?REL', 'CaseRole'], [holds, '?REL', '?PROCESS', '?OBJ']], [exists, ['?TIME'], ['overlapsSpatially', ['WhereFn', '?PROCESS', '?TIME'], '?OBJ']]):-true.
instance('overlapsPartially', 'SymmetricRelation'):-true.
instance('overlapsPartially', 'IrreflexiveRelation'):-true.
subrelation('overlapsPartially', 'overlapsSpatially'):-true.
documentation('overlapsPartially', "(&%overlapsPartially ?OBJ1 ?OBJ2) means 
that ?OBJ1 and ?OBJ2 have part(s) in common, but neither ?OBJ1 nor ?OBJ2 
is a &%part of the other."):-true.
<=>(['overlapsPartially', '?OBJ1', '?OBJ2'], [and, [not, [part, '?OBJ1', '?OBJ2']], [not, [part, '?OBJ2', '?OBJ1']], [exists, ['?OBJ3'], [and, [part, '?OBJ3', '?OBJ1'], [part, '?OBJ3', '?OBJ2']]]]):-true.
subrelation('superficialPart', part):-true.
instance('superficialPart', 'IrreflexiveRelation'):-true.
instance('superficialPart', 'TransitiveRelation'):-true.
documentation('superficialPart', "(&%superficialPart ?OBJ1 ?OBJ2) 
means that ?OBJ1 is a part of ?OBJ2 that has no interior parts of its own 
(or, intuitively, that only overlaps those parts of ?OBJ2 that are 
externally connected with the mereological complement of ?OBJ2). This too 
is a transitive relation closed under &%MereologicalSumFn and 
&%MereologicalProductFn."):-true.
=>(['superficialPart', '?OBJ1', '?OBJ2'], [and, [not, ['interiorPart', '?OBJ1', '?OBJ2']], [not, [exists, ['?OBJ3'], ['interiorPart', '?OBJ3', '?OBJ1']]]]):-true.
instance(surface, 'AsymmetricRelation'):-true.
subrelation(surface, 'superficialPart'):-true.
domain(surface, 1, 'SelfConnectedObject'):-true.
domain(surface, 2, 'SelfConnectedObject'):-true.
documentation(surface, "(&%surface ?OBJ1 ?OBJ2) means that ?OBJ1 
is a maximally connected &%superficialPart of ?OBJ2.  Note that some 
&%SelfConnectedObjects have more than one surface, e.g. a hollow 
object like a tennis ball has both an inner and an outer surface."):-true.
=>([surface, '?OBJ1', '?OBJ2'], [forall, ['?OBJ3'], [=>, ['superficialPart', '?OBJ3', '?OBJ2'], [part, '?OBJ3', '?OBJ1']]]):-true.
subrelation('interiorPart', part):-true.
instance('interiorPart', 'IrreflexiveRelation'):-true.
instance('interiorPart', 'TransitiveRelation'):-true.
documentation('interiorPart', "(&%interiorPart ?OBJ1 ?OBJ2) means 
that ?OBJ1 is part ?OBJ2 and there is no overlap between ?OBJ1 and 
any &%superficialPart ?OBJ2."):-true.
<=>(['interiorPart', '?OBJ1', '?OBJ2'], [forall, ['?PART'], [=>, ['superficialPart', '?PART', '?OBJ2'], [not, ['overlapsSpatially', '?OBJ1', '?PART']]]]):-true.
subrelation(bottom, 'superficialPart'):-true.
domain(bottom, 1, 'SelfConnectedObject'):-true.
domain(bottom, 2, 'SelfConnectedObject'):-true.
documentation(bottom, "(&%bottom ?BOTTOM ?OBJECT) holds if ?BOTTOM is the 
lowest or deepest maximal superficial part of ?OBJECT."):-true.
=>([and, [bottom, '?BOTTOM', '?OBJECT'], [part, '?PART', '?OBJECT'], [not, [connected, '?PART', '?BOTTOM']]], [orientation, '?PART', '?BOTTOM', 'Above']):-true.
subrelation(top, 'superficialPart'):-true.
domain(top, 1, 'SelfConnectedObject'):-true.
domain(top, 2, 'SelfConnectedObject'):-true.
documentation(top, "(&%top ?TOP ?OBJECT) means that ?TOP is the highest maximal 
superficial part of ?OBJECT."):-true.
=>([and, [top, '?TOP', '?OBJECT'], [part, '?PART', '?OBJECT'], [not, [connected, '?PART', '?TOP']]], [orientation, '?PART', '?TOP', 'Below']):-true.
subrelation(side, 'superficialPart'):-true.
domain(side, 1, 'SelfConnectedObject'):-true.
domain(side, 2, 'SelfConnectedObject'):-true.
documentation(side, "(&%side ?SIDE ?OBJECT) means that ?SIDE is a side of the object, 
as opposed to the &%top or &%bottom."):-true.
=>([and, [side, '?SIDE', '?OBJECT'], [part, '?PART', '?OBJECT'], [not, [connected, '?PART', '?SIDE']]], [or, [orientation, '?SIDE', '?PART', 'Left'], [orientation, '?SIDE', '?PART', 'Right']]):-true.
<=>([width, '?OBJECT', '?WIDTH'], [exists, ['?SIDE1', '?SIDE2'], [and, [side, '?SIDE1', '?OBJECT'], [side, '?SIDE2', '?OBJECT'], [distance, '?SIDE1', '?SIDE2', '?WIDTH']]]):-true.
subrelation(height, length):-true.
domain(height, 1, 'SelfConnectedObject'):-true.
domain(height, 2, 'LengthMeasure'):-true.
documentation(height, "The height of an &%Object is the distance between 
its &%top and its &%bottom."):-true.
=>([and, [height, '?OBJECT', '?HEIGHT'], [top, '?TOP', '?OBJECT'], [bottom, '?BOTTOM', '?OBJECT']], [distance, '?TOP', '?BOTTOM', '?HEIGHT']):-true.
instance('MereologicalSumFn', 'SpatialRelation'):-true.
instance('MereologicalSumFn', 'BinaryFunction'):-true.
domain('MereologicalSumFn', 1, 'Object'):-true.
domain('MereologicalSumFn', 2, 'Object'):-true.
range('MereologicalSumFn', 'Object'):-true.
'relatedInternalConcept'('MereologicalSumFn', 'MereologicalProductFn'):-true.
'relatedInternalConcept'('MereologicalSumFn', 'MereologicalDifferenceFn'):-true.
documentation('MereologicalSumFn', "(&%MereologicalSumFn ?OBJ1 ?OBJ2) 
denotes the &%Object consisting of the parts which belong to either 
?OBJ1 or ?OBJ2."):-true.
=>([equal, '?OBJ3', ['MereologicalSumFn', '?OBJ1', '?OBJ2']], [forall, ['?PART'], [<=>, [part, '?PART', '?OBJ3'], [or, [part, '?PART', '?OBJ1'], [part, '?PART', '?OBJ2']]]]):-true.
instance('MereologicalProductFn', 'SpatialRelation'):-true.
instance('MereologicalProductFn', 'BinaryFunction'):-true.
domain('MereologicalProductFn', 1, 'Object'):-true.
domain('MereologicalProductFn', 2, 'Object'):-true.
range('MereologicalProductFn', 'Object'):-true.
'relatedInternalConcept'('MereologicalProductFn', 'MereologicalDifferenceFn'):-true.
documentation('MereologicalProductFn', "(&%MereologicalProductFn ?OBJ1 ?OBJ2) 
denotes the &%Object consisting of the parts which belong to both ?OBJ1 
and ?OBJ2."):-true.
=>([equal, '?OBJ3', ['MereologicalProductFn', '?OBJ1', '?OBJ2']], [forall, ['?PART'], [<=>, [part, '?PART', '?OBJ3'], [and, [part, '?PART', '?OBJ1'], [part, '?PART', '?OBJ2']]]]):-true.
instance('MereologicalDifferenceFn', 'SpatialRelation'):-true.
instance('MereologicalDifferenceFn', 'BinaryFunction'):-true.
domain('MereologicalDifferenceFn', 1, 'Object'):-true.
domain('MereologicalDifferenceFn', 2, 'Object'):-true.
range('MereologicalDifferenceFn', 'Object'):-true.
documentation('MereologicalDifferenceFn', "(&%MereologicalDifferenceFn ?OBJ1 
?OBJ2) denotes the &%Object consisting of the parts which belong to ?OBJ1 
and not to ?OBJ2."):-true.
=>([equal, '?OBJ3', ['MereologicalDifferenceFn', '?OBJ1', '?OBJ2']], [forall, ['?PART'], [<=>, [part, '?PART', '?OBJ3'], [and, [part, '?PART', '?OBJ1'], [not, [part, '?PART', '?OBJ2']]]]]):-true.
instance(hole, 'BinaryPredicate'):-true.
instance(hole, 'SpatialRelation'):-true.
instance(hole, 'AsymmetricRelation'):-true.
domain(hole, 1, 'Hole'):-true.
domain(hole, 2, 'SelfConnectedObject'):-true.
documentation(hole, "(&%hole ?HOLE ?OBJ) means that ?HOLE is a 
&%Hole in ?OBJ.  A &%Hole is a fillable body located at the 
&%surface an &%Object."):-true.
subclass('Hole', 'Region'):-true.
documentation('Hole', "A hole is an immaterial body located at the surface 
of an &%Object.  Since every &%Hole is ontologically dependent on its host 
(i.e., the object in which it is a hole), being a &%Hole is defined as 
being a &%hole in something.  Note that two &%Holes may occupy the same 
region, or part of the same region, without sharing any parts."):-true.
<=>([instance, '?HOLE', 'Hole'], [exists, ['?OBJ'], [hole, '?HOLE', '?OBJ']]):-true.
=>([hole, '?HOLE', '?OBJ'], [not, [instance, '?OBJ', 'Hole']]):-true.
=>([hole, '?HOLE', '?OBJ'], [not, ['overlapsSpatially', '?HOLE', '?OBJ']]):-true.
=>([and, [hole, '?HOLE', '?OBJ1'], [hole, '?HOLE', '?OBJ2']], [exists, ['?OBJ3'], [and, ['properPart', '?OBJ3', ['MereologicalProductFn', '?OBJ1', '?OBJ2']], [hole, '?HOLE', '?OBJ3']]]):-true.
=>([and, [hole, '?HOLE1', '?OBJ'], [hole, '?HOLE2', '?OBJ']], [forall, ['?HOLE3'], [=>, [part, '?HOLE3', ['MereologicalSumFn', '?HOLE1', '?HOLE2']], [hole, '?HOLE3', '?OBJ']]]):-true.
=>([and, [hole, '?HOLE', '?OBJ1'], [part, '?OBJ1', '?OBJ2']], [or, ['overlapsSpatially', '?HOLE', '?OBJ2'], [hole, '?HOLE', '?OBJ2']]):-true.
=>([and, [hole, '?HOLE1', '?OBJ1'], [hole, '?HOLE2', '?OBJ2'], ['overlapsSpatially', '?HOLE1', '?HOLE2']], ['overlapsSpatially', '?OBJ1', '?OBJ2']):-true.
=>([instance, '?HOLE1', 'Hole'], [exists, ['?HOLE2'], ['properPart', '?HOLE2', '?HOLE1']]):-true.
instance('PrincipalHostFn', 'SpatialRelation'):-true.
instance('PrincipalHostFn', 'UnaryFunction'):-true.
instance('PrincipalHostFn', 'AsymmetricRelation'):-true.
domain('PrincipalHostFn', 1, 'Hole'):-true.
range('PrincipalHostFn', 'Object'):-true.
documentation('PrincipalHostFn', "A &%UnaryFunction that maps a &%Hole to 
the &%Object which is its principal host.  The principle host of a &%Hole 
is its maximally connected host (a notion taken here to be defined only 
when the argument is a hole)."):-true.
=>([equal, '?OBJ1', ['PrincipalHostFn', '?HOLE']], [forall, ['?OBJ2'], [<=>, ['overlapsSpatially', '?OBJ2', '?OBJ1'], [exists, ['?OBJ3'], [and, [hole, '?HOLE', '?OBJ3'], ['overlapsSpatially', '?OBJ2', '?OBJ3']]]]]):-true.
=>([hole, '?HOLE', '?OBJ'], [connected, '?HOLE', '?OBJ']):-true.
=>([and, [instance, '?HOLE1', 'Hole'], ['properPart', '?HOLE2', '?HOLE1']], [exists, ['?OBJ'], [and, ['meetsSpatially', '?HOLE1', '?OBJ'], [not, ['meetsSpatially', '?HOLE2', '?OBJ']]]]):-true.
instance('Fillable', 'ShapeProperty'):-true.
documentation('Fillable', "Something is &%Fillable if it can be filled by 
something else.  Note that 'filled' here means perfectly filled."):-true.
=>([exists, ['?TIME'], ['holdsDuring', '?TIME', [fills, '?OBJ', '?HOLE']]], [attribute, '?HOLE', 'Fillable']):-true.
<=>([attribute, '?HOLE1', 'Fillable'], [exists, ['?HOLE2'], [and, [instance, '?HOLE2', 'Hole'], [part, '?HOLE1', '?HOLE2']]]):-true.
subrelation('partiallyFills', located):-true.
instance('partiallyFills', 'SpatialRelation'):-true.
instance('partiallyFills', 'AsymmetricRelation'):-true.
domain('partiallyFills', 1, 'Object'):-true.
domain('partiallyFills', 2, 'Hole'):-true.
documentation('partiallyFills', "(&%partiallyFills ?OBJ ?HOLE) means that 
?OBJ &%completelyFills some part of ?HOLE. Note that if (&%partiallyFills 
?OBJ1 ?HOLE) and (&%part ?OBJ1 ?OBJ2), then (&%partiallyFills ?OBJ2 ?HOLE). 
Note too that a partial filler need not be wholly inside a hole (it may 
stick out), which means that every complete filler also qualifies as 
(is a limit case of) a partial one."):-true.
=>(['partiallyFills', '?OBJ', '?HOLE1'], [exists, ['?HOLE2'], [and, [part, '?HOLE2', '?HOLE1'], ['completelyFills', '?OBJ', '?HOLE2']]]):-true.
instance('properlyFills', 'AsymmetricRelation'):-true.
subrelation('properlyFills', 'partiallyFills'):-true.
domain('properlyFills', 1, 'Object'):-true.
domain('properlyFills', 2, 'Hole'):-true.
documentation('properlyFills', "(&%properlyFills ?OBJ ?HOLE) 
means that ?HOLE is properly (though perhaps incompletely) filled by 
?OBJ, i.e. some part of ?HOLE is perfectly filled by ?OBJ.  Note that 
&%properlyFills is the dual of &%completelyFills, and is so 
related to &%partiallyFills that ?OBJ &%properlyFills ?HOLE just in 
case ?OBJ &%partiallyFills every part of ?HOLE.  (Thus, every perfect 
filler is both complete and proper in this sense)."):-true.
=>(['properlyFills', '?OBJ', '?HOLE1'], [exists, ['?HOLE2'], [and, [part, '?HOLE2', '?HOLE1'], [fills, '?OBJ', '?HOLE2']]]):-true.
instance('completelyFills', 'AsymmetricRelation'):-true.
subrelation('completelyFills', 'partiallyFills'):-true.
documentation('completelyFills', "(&%completelyFills ?OBJ ?HOLE) 
means that some &%part of the &%Object ?OBJ fills the &%Hole ?HOLE.  
Note that if (&%completelyFills ?OBJ1 ?HOLE) and (&%part 
?OBJ1 ?OBJ2), then (&%completelyFills ?OBJ2 ?HOLE)."):-true.
=>(['completelyFills', '?OBJ1', '?HOLE'], [exists, ['?OBJ2'], [and, [part, '?OBJ2', '?OBJ1'], [fills, '?OBJ2', '?HOLE']]]):-true.
instance(fills, 'AsymmetricRelation'):-true.
subrelation(fills, 'completelyFills'):-true.
subrelation(fills, 'properlyFills'):-true.
domain(fills, 1, 'Object'):-true.
domain(fills, 2, 'Hole'):-true.
'relatedInternalConcept'(fills, 'Fillable'):-true.
documentation(fills, "Holes can be filled.  (&%fills ?OBJ ?HOLE) 
means that the &%Object ?OBJ fills the &%Hole ?HOLE.  Note that 
&%fills here means perfectly filled."):-true.
=>([and, [fills, '?OBJ1', '?HOLE'], [attribute, '?OBJ2', 'Fillable']], [not, ['overlapsSpatially', '?OBJ1', '?OBJ2']]):-true.
=>(['completelyFills', '?OBJ1', '?HOLE'], [forall, ['?OBJ2'], [=>, [connected, '?OBJ2', '?HOLE'], [connected, '?OBJ2', '?OBJ1']]]):-true.
=>([and, ['properlyFills', '?OBJ1', '?HOLE'], [connected, '?OBJ2', '?OBJ1']], [connected, '?HOLE', '?OBJ2']):-true.
=>([and, [fills, '?OBJ', '?HOLE1'], ['properPart', '?HOLE2', '?HOLE1']], ['completelyFills', '?OBJ', '?HOLE2']):-true.
=>([and, [fills, '?OBJ1', '?HOLE'], ['properPart', '?OBJ2', '?OBJ1']], ['properlyFills', '?OBJ2', '?HOLE']):-true.
instance('SkinFn', 'SpatialRelation'):-true.
instance('SkinFn', 'UnaryFunction'):-true.
instance('SkinFn', 'AsymmetricRelation'):-true.
domain('SkinFn', 1, 'Hole'):-true.
range('SkinFn', 'Object'):-true.
documentation('SkinFn', "A &%UnaryFunction that maps a &%Hole to the skin 
of the &%Hole.  The skin of a &%Hole is the fusion of those superficial 
parts (see &%superficialPart) of the &%Hole's principal host (see 
&%PrincipalHostFn) with which the &%Hole is externally connected."):-true.
=>([equal, '?OBJ1', ['SkinFn', '?HOLE']], [forall, ['?OBJ2'], [<=>, ['overlapsSpatially', '?OBJ2', '?OBJ1'], [exists, ['?OBJ3'], [and, ['superficialPart', '?OBJ3', ['PrincipalHostFn', '?HOLE']], ['meetsSpatially', '?HOLE', '?OBJ3'], ['overlapsSpatially', '?OBJ2', '?OBJ3']]]]]):-true.
instance('subProcess', 'BinaryPredicate'):-true.
instance('subProcess', 'PartialOrderingRelation'):-true.
domain('subProcess', 1, 'Process'):-true.
domain('subProcess', 2, 'Process'):-true.
documentation('subProcess', "(&%subProcess ?SUBPROC ?PROC) means that ?SUBPROC 
is a subprocess of ?PROC.  A subprocess is here understood as a temporally 
distinguished part (proper or not) of a &%Process."):-true.
=>(['subProcess', '?SUBPROC', '?PROC'], [or, [equal, ['WhenFn', '?SUBPROC'], ['WhenFn', '?PROC']], [during, ['WhenFn', '?SUBPROC'], ['WhenFn', '?PROC']]]):-true.
=>(['subProcess', '?SUBPROC', '?PROC'], [forall, ['?REGION'], [=>, [located, '?PROC', '?REGION'], [located, '?SUBPROC', '?REGION']]]):-true.
=>([and, [instance, '?PROC', 'Process'], ['subProcess', '?SUBPROC', '?PROC']], [exists, ['?TIME'], [time, '?SUBPROC', '?TIME']]):-true.
subclass('BiologicalProcess', 'InternalChange'):-true.
documentation('BiologicalProcess', "A &%Process embodied in an &%Organism."):-true.
=>([instance, '?PROC', 'BiologicalProcess'], [exists, ['?OBJ'], [and, [instance, '?OBJ', 'Organism'], [located, '?PROC', '?OBJ']]]):-true.
subclass('PhysiologicProcess', 'BiologicalProcess'):-true.
documentation('PhysiologicProcess', "A normal process of an &%Organism 
or part of an &%Organism."):-true.
subclass('OrganismProcess', 'PhysiologicProcess'):-true.
documentation('OrganismProcess', "A physiologic function of the 
&%Organism as a whole, of multiple organ systems or of multiple 
&%Organs or &%Tissues."):-true.
subclass('Birth', 'OrganismProcess'):-true.
documentation('Birth', "The &%Process of being born."):-true.
=>([and, [instance, '?BIRTH', 'Birth'], [experiencer, '?BIRTH', '?AGENT']], [exists, ['?DEATH'], [and, [instance, '?DEATH', 'Death'], [experiencer, '?DEATH', '?AGENT']]]):-true.
subclass('Death', 'OrganismProcess'):-true.
documentation('Death', "The &%Process of dying."):-true.
=>([and, [instance, '?DEATH', 'Death'], [experiencer, '?DEATH', '?AGENT']], ['holdsDuring', ['FutureFn', ['WhenFn', '?DEATH']], [attribute, '?AGENT', 'Dead']]):-true.
=>([and, [instance, '?DEATH', 'Death'], [instance, '?BIRTH', 'Birth'], [experiencer, '?DEATH', '?AGENT'], [experiencer, '?BIRTH', '?AGENT']], [exists, ['?TIME'], [and, ['meetsTemporally', ['WhenFn', '?BIRTH'], '?TIME'], ['meetsTemporally', '?TIME', ['WhenFn', '?DEATH']], ['holdsDuring', '?TIME', [attribute, '?AGENT', 'Living']]]]):-true.
subclass('Breathing', 'OrganismProcess'):-true.
documentation('Breathing', "The &%Process of respiration, by which oxygen 
is made available to an &%Animal.  This covers processes of inhalation, 
exhalation, and alternations between the two."):-true.
subclass('Ingesting', 'OrganismProcess'):-true.
documentation('Ingesting', "The &%Process by which &%Food is 
incorporated into an &%Animal."):-true.
=>([and, [instance, '?ACT', 'Ingesting'], [patient, '?ACT', '?FOOD']], [instance, '?FOOD', 'Food']):-true.
subclass('Eating', 'Ingesting'):-true.
documentation('Eating', "The &%Process by which solid &%Food is 
incorporated into an &%Animal."):-true.
=>([and, [instance, '?ACT', 'Eating'], [patient, '?ACT', '?FOOD']], [attribute, '?FOOD', 'Solid']):-true.
subclass('Drinking', 'Ingesting'):-true.
documentation('Drinking', "The &%Process by which liquid &%Food, i.e. 
&%Beverages, are incorporated into an &%Animal."):-true.
subclass('Growth', 'OrganismProcess'):-true.
documentation('Growth', "The &%Process of biological development in which 
an &%Organism or part of an &%Organism changes its form or its size."):-true.
subclass('Replication', 'OrganismProcess'):-true.
documentation('Replication', "The &%Process of biological reproduction.  
This can be either a sexual or an asexual process."):-true.
=>([and, [instance, '?REP', 'Replication'], [agent, '?REP', '?PARENT'], [result, '?REP', '?CHILD']], [parent, '?CHILD', '?PARENT']):-true.
subclass('SexualReproduction', 'Replication'):-true.
disjoint('SexualReproduction', 'AsexualReproduction'):-true.
documentation('SexualReproduction', "Sexual &%Processes of biological 
reproduction."):-true.
=>([and, [instance, '?REP', 'SexualReproduction'], [result, '?REP', '?ORGANISM']], [not, [exists, ['?MOTHER', '?FATHER'], [and, [mother, '?ORGANISM', '?MOTHER'], [father, '?ORGANISM', '?FATHER']]]]):-true.
subclass('AsexualReproduction', 'Replication'):-true.
documentation('AsexualReproduction', "Asexual &%Processes of biological 
reproduction."):-true.
=>([and, [instance, '?REP', 'AsexualReproduction'], [result, '?REP', '?ORGANISM']], [not, [exists, ['?PARENT1', '?PARENT2'], [and, [parent, '?ORGANISM', '?PARENT1'], [parent, '?ORGANISM', '?PARENT2'], [not, [equal, '?PARENT1', '?PARENT2']]]]]):-true.
subclass('PsychologicalProcess', 'BiologicalProcess'):-true.
documentation('PsychologicalProcess', "A &%BiologicalProcess which takes place in 
the mind or brain of an &%Organism and which may be manifested in the behavior 
of the &%Organism."):-true.
=>([instance, '?PROCESS', 'PsychologicalProcess'], [exists, ['?ANIMAL'], [and, [instance, '?ANIMAL', 'Animal'], [experiencer, '?PROCESS', '?ANIMAL']]]):-true.
subclass('OrganOrTissueProcess', 'PhysiologicProcess'):-true.
disjoint('OrganOrTissueProcess', 'OrganismProcess'):-true.
documentation('OrganOrTissueProcess', "A &%PhysiologicProcess of a 
particular &%Organ or &%Tissue."):-true.
=>([instance, '?PROC', 'OrganOrTissueProcess'], [exists, ['?THING'], [and, [located, '?PROC', '?THING'], [or, [instance, '?THING', 'Organ'], [instance, '?THING', 'Tissue']]]]):-true.
subclass('PathologicProcess', 'BiologicalProcess'):-true.
disjoint('PathologicProcess', 'PhysiologicProcess'):-true.
documentation('PathologicProcess', "A disordered process, activity, or 
state of the &%Organism as a whole, of a body system or systems, or of 
multiple &%Organs or &%Tissues. Included here are normal responses to a 
negative stimulus as well as patholologic conditions or states that are 
less specific than a disease. Pathologic functions frequently have 
systemic effects."):-true.
subclass('Injuring', 'PathologicProcess'):-true.
subclass('Injuring', 'Damaging'):-true.
documentation('Injuring', "The process of creating a traumatic wound or 
injury.  Since &%Injuring is not possible without some biologic function 
of the organism being injured, it is a subclass of &%BiologicalProcess."):-true.
=>([instance, '?INJ', 'Injuring'], [exists, ['?STRUCT'], [and, [instance, '?STRUCT', 'AnatomicalStructure'], [patient, '?INJ', '?STRUCT']]]):-true.
<=>([instance, '?INJ', 'Injuring'], [and, [instance, '?INJ', 'Damaging'], [patient, '?INJ', 'Organism']]):-true.
subclass('Poisoning', 'Injuring'):-true.
documentation('Poisoning', "A &%Poisoning is caused by an external 
substance.  Since &%Poisoning is not possible without some biologic 
function which affects the &%Organism being injured, it is a subclass 
of &%BiologicalProcess."):-true.
=>([instance, '?POISON', 'Poisoning'], [exists, ['?THING'], [and, [patient, '?POISON', '?THING'], [or, [instance, '?THING', 'Organism'], [instance, '?THING', 'AnatomicalStructure']]]]):-true.
=>([instance, '?POISON', 'Poisoning'], [exists, ['?SUBSTANCE'], [and, [instance, '?SUBSTANCE', 'BiologicallyActiveSubstance'], [instrument, '?POISON', '?SUBSTANCE']]]):-true.
subclass('IntentionalProcess', 'Process'):-true.
documentation('IntentionalProcess', "A &%Process that is 
deliberately set in motion by a &%CognitiveAgent."):-true.
=>([and, [instance, '?PROC', 'IntentionalProcess'], [agent, '?PROC', '?AGENT']], [and, [instance, '?AGENT', 'CognitiveAgent'], [exists, ['?PURP'], ['hasPurposeForAgent', '?PROC', '?PURP', '?AGENT']]]):-true.
=>([instance, '?PROC', 'IntentionalProcess'], [exists, ['?AGENT'], [and, [instance, '?AGENT', 'CognitiveAgent'], [agent, '?PROC', '?AGENT']]]):-true.
subclass('IntentionalPsychologicalProcess', 'IntentionalProcess'):-true.
subclass('IntentionalPsychologicalProcess', 'PsychologicalProcess'):-true.
documentation('IntentionalPsychologicalProcess', "An &%IntentionalProcess that 
can be realized entirely within the mind or brain of an &%Organism.  Thus, 
for example, &%Reasoning is a subclass of &%IntentionalPsychologicalProcess, 
because one can reason simply by exercising one's mind/brain.  On the other 
hand, &%RecreationOrExercise is not a subclass of &%IntentionalPsychologicalProcess,
because many instances of &%RecreationOrExercise necessarily have &%subProcesses 
of &%BodyMotion."):-true.
subclass('RecreationOrExercise', 'IntentionalProcess'):-true.
documentation('RecreationOrExercise', "A &%Process that is carried out for 
the purpose of recreation or exercise."):-true.
subclass('OrganizationalProcess', 'IntentionalProcess'):-true.
documentation('OrganizationalProcess', "An &%IntentionalProcess that 
is carried out within or by an &%Organization."):-true.
=>([and, [instance, '?ACT', 'OrganizationalProcess'], [agent, '?ACT', '?AGENT']], [or, [instance, '?AGENT', 'Organization'], [exists, ['?ORG'], [and, [instance, '?ORG', 'Organization'], [member, '?AGENT', '?ORG']]]]):-true.
subclass('EducationalProcess', 'OrganizationalProcess'):-true.
subclass('EducationalProcess', 'Guiding'):-true.
documentation('EducationalProcess', "Any &%Process which occurs in an 
organizational setting and which is intended to result in &%Learning."):-true.
=>([and, [instance, '?EDUCATION', 'EducationalProcess'], [patient, '?EDUCATION', '?PERSON']], ['hasPurpose', '?EDUCATION', [exists, ['?LEARN'], [and, [instance, '?LEARN', 'Learning'], [patient, '?LEARN', '?PERSON']]]]):-true.
subclass('ReligiousProcess', 'OrganizationalProcess'):-true.
documentation('ReligiousProcess', "An &%OrganizationalProcess that is 
carried out within or by a &%ReligiousOrganization."):-true.
=>([and, [instance, '?ACT', 'ReligiousProcess'], [agent, '?ACT', '?AGENT']], [or, [instance, '?AGENT', 'ReligiousOrganization'], [exists, ['?ORG'], [and, [member, '?AGENT', '?ORG'], [instance, '?ORG', 'ReligiousOrganization']]]]):-true.
subclass('JoiningAnOrganization', 'OrganizationalProcess'):-true.
documentation('JoiningAnOrganization', "The &%OrganizationalProcess of 
becoming a &%member of an &%Organization."):-true.
=>([and, [instance, '?JOIN', 'JoiningAnOrganization'], [instance, '?ORG', 'Organization'], [agent, '?JOIN', '?ORG'], [patient, '?JOIN', '?PERSON']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?JOIN']], [member, '?PERSON', '?ORG']]):-true.
subclass('LeavingAnOrganization', 'OrganizationalProcess'):-true.
disjoint('LeavingAnOrganization', 'JoiningAnOrganization'):-true.
documentation('LeavingAnOrganization', "The &%OrganizationalProcess of 
leaving an &%Organization, whether voluntarily or involuntarily."):-true.
=>([and, [instance, '?LEAVE', 'LeavingAnOrganization'], [instance, '?ORG', 'Organization'], [agent, '?LEAVE', '?ORG'], [patient, '?LEAVE', '?PERSON']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?LEAVE']], [not, [member, '?PERSON', '?ORG']]]):-true.
subclass('Graduation', 'LeavingAnOrganization'):-true.
documentation('Graduation', "The &%OrganizationalProcess of graduating 
from an &%EducationalOrganization."):-true.
=>([and, [instance, '?GRAD', 'Graduation'], [agent, '?GRAD', '?ORG'], [patient, '?GRAD', '?PERSON']], [instance, '?ORG', 'EducationalOrganization']):-true.
subclass('Matriculation', 'JoiningAnOrganization'):-true.
documentation('Matriculation', "The &%OrganizationalProcess of joining an 
&%EducationalOrganization as a student."):-true.
=>([and, [instance, '?MAT', 'Matriculation'], [agent, '?MAT', '?ORG'], [patient, '?MAT', '?PERSON']], [instance, '?ORG', 'EducationalOrganization']):-true.
subclass('Hiring', 'JoiningAnOrganization'):-true.
documentation('Hiring', "&%OrganizationalProcesses where someone is made an 
employee of an &%Organization."):-true.
=>([and, [instance, '?HIRE', 'Hiring'], [instance, '?ORG', 'Organization'], [agent, '?HIRE', '?ORG'], [patient, '?HIRE', '?PERSON']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?HIRE']], [employs, '?ORG', '?PERSON']]):-true.
subclass('TerminatingEmployment', 'LeavingAnOrganization'):-true.
documentation('TerminatingEmployment', "&%OrganizationalProcesses where someone 
ceases to be an employee of an &%Organization.  Note that this covers being 
laid off, being fired, and voluntarily leaving a job."):-true.
=>([and, [instance, '?FIRE', 'TerminatingEmployment'], [instance, '?ORG', 'Organization'], [agent, '?FIRE', '?ORG'], [patient, '?FIRE', '?PERSON']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?FIRE']], [not, [employs, '?ORG', '?PERSON']]]):-true.
subclass('PoliticalProcess', 'OrganizationalProcess'):-true.
documentation('PoliticalProcess', "An &%OrganizationalProcess carried 
out by officially constituted governments or with the aim of overthrowing 
or modifying such a government."):-true.
=>([instance, '?PROC', 'PoliticalProcess'], [exists, ['?GOV'], [and, [instance, '?GOV', 'Government'], [or, [agent, '?PROC', '?GOV'], [patient, '?PROC', '?GOV']]]]):-true.
subclass('MilitaryProcess', 'PoliticalProcess'):-true.
documentation('MilitaryProcess', "Any &%Process that is carried out by a 
military organization.  Note that this class covers &%Processes, e.g. 
military operations, that are the result of careful planning, as well as 
those which are unscripted."):-true.
subclass('RegulatoryProcess', 'OrganizationalProcess'):-true.
documentation('RegulatoryProcess', "an &%OrganizationalProcess whose aim is 
the enforcement of rules or regulations."):-true.
subclass('Managing', 'OrganizationalProcess'):-true.
subclass('Managing', 'Guiding'):-true.
documentation('Managing', "&%OrganizationalProcesses that involve overseeing 
the activities of others."):-true.
subclass('Planning', 'IntentionalPsychologicalProcess'):-true.
documentation('Planning', "Specifying a set of actions in order to meet a 
set of goals or objectives."):-true.
subclass('QuantityChange', 'InternalChange'):-true.
partition('QuantityChange', 'Increasing', 'Decreasing'):-true.
documentation('QuantityChange', "Any &%InternalChange where a &%PhysicalQuantity 
associated with the &%patient is altered."):-true.
subclass('Increasing', 'QuantityChange'):-true.
'relatedInternalConcept'('Increasing', 'Putting'):-true.
documentation('Increasing', "Any &%QuantityChange where the &%PhysicalQuantity 
is increased."):-true.
=>([and, [instance, '?INCREASE', 'Increasing'], [patient, '?INCREASE', '?OBJ']], [exists, ['?UNIT', '?QUANT1', '?QUANT2'], [and, ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?INCREASE']], [equal, ['MeasureFn', '?OBJ', '?UNIT'], '?QUANT1']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?INCREASE']], [equal, ['MeasureFn', '?OBJ', '?UNIT'], '?QUANT2']], ['greaterThan', '?QUANT2', '?QUANT1']]]):-true.
subclass('Heating', 'Increasing'):-true.
disjoint('Heating', 'Cooling'):-true.
documentation('Heating', "Any &%Increasing &%Process where the &%PhysicalQuantity 
increased is a &%TemperatureMeasure."):-true.
=>([and, [instance, '?HEAT', 'Heating'], [patient, '?HEAT', '?OBJ']], [exists, ['?UNIT', '?QUANT1', '?QUANT2'], [and, [instance, '?UNIT', 'TemperatureMeasure'], ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?HEAT']], [equal, ['MeasureFn', '?OBJ', '?UNIT'], '?QUANT1']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?HEAT']], [equal, ['MeasureFn', '?OBJ', '?UNIT'], '?QUANT2']], ['greaterThan', '?QUANT2', '?QUANT1']]]):-true.
subclass('Decreasing', 'QuantityChange'):-true.
'relatedInternalConcept'('Decreasing', 'Removing'):-true.
documentation('Decreasing', "Any &%QuantityChange where the &%PhysicalQuantity 
is decreased."):-true.
=>([and, [instance, '?DECREASE', 'Decreasing'], [patient, '?DECREASE', '?OBJ']], [exists, ['?UNIT', '?QUANT1', '?QUANT2'], [and, ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?DECREASE']], [equal, ['MeasureFn', '?OBJ', '?UNIT'], '?QUANT1']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?DECREASE']], [equal, ['MeasureFn', '?OBJ', '?UNIT'], '?QUANT2']], ['lessThan', '?QUANT2', '?QUANT1']]]):-true.
subclass('Cooling', 'Decreasing'):-true.
documentation('Cooling', "Any &%Decreasing &%Process where the &%PhysicalQuantity 
decreased is a &%TemperatureMeasure."):-true.
=>([and, [instance, '?COOL', 'Cooling'], [patient, '?COOL', '?OBJ']], [exists, ['?UNIT', '?QUANT1', '?QUANT2'], [and, [instance, '?UNIT', 'TemperatureMeasure'], ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?COOL']], [equal, ['MeasureFn', '?OBJ', '?UNIT'], '?QUANT1']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?COOL']], [equal, ['MeasureFn', '?OBJ', '?UNIT'], '?QUANT2']], ['lessThan', '?QUANT2', '?QUANT1']]]):-true.
subclass('Motion', 'Process'):-true.
documentation('Motion', "Any &%Process of movement."):-true.
=>([and, [instance, '?MOTION', 'Motion'], [patient, '?MOTION', '?OBJ'], [origin, '?MOTION', '?PLACE']], ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?MOTION']], [located, '?OBJ', '?PLACE']]):-true.
=>([and, [instance, '?MOTION', 'Motion'], [patient, '?MOTION', '?OBJ'], [destination, '?MOTION', '?PLACE']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?MOTION']], [located, '?OBJ', '?PLACE']]):-true.
instance(path, 'CaseRole'):-true.
domain(path, 1, 'Motion'):-true.
domain(path, 2, 'Region'):-true.
documentation(path, "(&%path ?MOTION ?PATH) means that ?PATH is a route 
along which ?MOTION occurs.  For example, Highway 101 is the path in the 
following proposition:  the car drove up Highway 101."):-true.
=>([and, [path, '?PROCESS', '?PATH1'], [origin, '?PROCESS', '?SOURCE'], [destination, '?PROCESS', '?DEST'], [length, '?PATH1', '?MEASURE1'], [not, [exists, ['?PATH2', '?MEASURE2'], [and, [path, '?PROCESS', '?PATH2'], [origin, '?PROCESS', '?ORIGIN'], [destination, '?PROCESS', '?DEST'], [length, '?PATH2', '?MEASURE2'], ['lessThan', '?MEASURE2', '?MEASURE1']]]]], [forall, ['?OBJ'], [=>, [part, '?OBJ', '?PATH1'], [between, '?SOURCE', '?OBJ', '?DEST']]]):-true.
subclass('BodyMotion', 'Motion'):-true.
documentation('BodyMotion', "Any &%Motion where the &%agent is an &%Organism 
and the &%patient is a &%BodyPart."):-true.
=>([instance, '?MOTION', 'BodyMotion'], [exists, ['?OBJ', '?AGENT'], [and, [instance, '?OBJ', 'BodyPart'], [patient, '?MOTION', '?OBJ'], [instance, '?AGENT', 'Organism'], [agent, '?MOTION', '?AGENT']]]):-true.
subclass('Walking', 'BodyMotion'):-true.
documentation('Walking', "Any &%BodyMotion which is accomplished by 
means of the legs of an &%Organism on land for the purpose of moving 
from one point to another."):-true.
=>([and, [instance, '?WALK', 'Walking'], [agent, '?WALK', '?AGENT']], [exists, ['?AREA'], [and, [instance, '?AREA', 'LandArea'], [located, '?AGENT', '?AREA']]]):-true.
subclass('Swimming', 'BodyMotion'):-true.
documentation('Swimming', "Any deliberate and controlled &%BodyMotion 
through water that is accomplished by an &%Organism."):-true.
=>([and, [instance, '?SWIM', 'Swimming'], [agent, '?SWIM', '?AGENT']], [exists, ['?AREA'], [and, [instance, '?AREA', 'WaterArea'], [located, '?AGENT', '?AREA']]]):-true.
subclass('Dancing', 'BodyMotion'):-true.
documentation('Dancing', "Any &%BodyMotion of &%Humans which is 
deliberately coordinated with music."):-true.
subclass('DirectionChange', 'Motion'):-true.
documentation('DirectionChange', "The act of changing the direction in 
which the &%patient of the act is oriented."):-true.
=>([instance, '?PROC', 'DirectionChange'], [exists, ['?ATTR'], [and, [instance, '?ATTR', 'DirectionalAttribute'], [or, [and, ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?PROC']], [attribute, '?PROC', '?ATTR']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?PROC']], [not, [attribute, '?PROC', '?ATTR']]]], [and, ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?PROC']], [attribute, '?PROC', '?ATTR']], ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?PROC']], [not, [attribute, '?PROC', '?ATTR']]]]]]]):-true.
subclass('Transfer', 'Motion'):-true.
documentation('Transfer', "Any &%instance of &%Motion where the &%agent 
of the &%Motion and the &%patient of the &%Motion are not the same 
thing."):-true.
=>([and, [instance, '?TRANSFER', 'Transfer'], [agent, '?TRANSFER', '?AGENT'], [patient, '?TRANSFER', '?PATIENT']], [not, [equal, '?AGENT', '?PATIENT']]):-true.
subclass('Removing', 'Transfer'):-true.
documentation('Removing', "The &%Class of &%Processes where something is 
taken away from a location.  Note that the thing removed and the location 
are specified with the &%CaseRoles &%patient and &%origin, respectively."):-true.
=>([and, [instance, '?REMOVE', 'Removing'], [origin, '?REMOVE', '?PLACE'], [patient, '?REMOVE', '?OBJ']], [and, ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?REMOVE']], [located, '?OBJ', '?PLACE']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?REMOVE']], [not, [located, '?OBJ', '?PLACE']]]]):-true.
subclass('Uncovering', 'Removing'):-true.
disjoint('Uncovering', 'Covering'):-true.
documentation('Uncovering', "The &%Class of &%Removing processes where the &%agent 
uncovers the &%patient, either completely or only partially."):-true.
subclass('Putting', 'Transfer'):-true.
documentation('Putting', "The &%Class of &%Processes where something is put 
in a location.  Note that the location is specified with the &%CaseRole 
&%destination."):-true.
=>([and, [instance, '?PUT', 'Putting'], [destination, '?PUT', '?PLACE'], [patient, '?PUT', '?OBJ']], [and, ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?PUT']], [not, [located, '?OBJ', '?PLACE']]], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?PUT']], [located, '?OBJ', '?PLACE']]]):-true.
subclass('Covering', 'Putting'):-true.
documentation('Covering', "The &%Class of &%Putting processes where the &%agent 
covers the &%patient, either completely or only partially, with something 
else."):-true.
subclass('Substituting', 'Transfer'):-true.
subclass('Substituting', 'DualObjectProcess'):-true.
documentation('Substituting', "The &%Class of &%Transfers where one thing is 
replaced with something else."):-true.
=>([instance, '?SUB', 'Substituting'], [exists, ['?PUT', '?REMOVE', '?OBJ1', '?OBJ2', '?PLACE'], [and, [instance, '?PUT', 'Putting'], [instance, '?REMOVE', 'Removing'], ['subProcess', '?PUT', '?SUB'], ['subProcess', '?REMOVE', '?SUB'], [patient, '?REMOVE', '?OBJ1'], [origin, '?REMOVE', '?PLACE'], [patient, '?PUT', '?OBJ2'], [destination, '?PUT', '?PLACE'], [not, [equal, '?OBJ1', '?OBJ2']]]]):-true.
subclass('Impelling', 'Transfer'):-true.
documentation('Impelling', "The &%subclass of &%Transfer where the &%patient 
travels through space by means of a sudden, forceful event.  Some examples 
would be shooting, throwing, tossing, etc."):-true.
subclass('Shooting', 'Impelling'):-true.
documentation('Shooting', "The &%subclass of &%Impelling where the &%patient 
is a projectile that is fired through the air by means of some sort of 
&%Device."):-true.
subclass('Touching', 'Transfer'):-true.
documentation('Touching', "Any &%Transfer where two &%Objects are 
brought into immediate physical contact with one another."):-true.
=>([and, [instance, '?TOUCH', 'Touching'], [agent, '?TOUCH', '?OBJ1'], [patient, '?TOUCH', '?OBJ2']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?TOUCH']], [connected, '?OBJ1', '?OBJ2']]):-true.
subclass('Impacting', 'Touching'):-true.
documentation('Impacting', "Any &%Touching where something comes into 
sudden, forceful, physical contact with something else.  Some examples 
would be striking, knocking, whipping etc."):-true.
=>([and, [instance, '?IMPACT', 'Impacting'], [patient, '?IMPACT', '?OBJ']], [exists, ['?IMPEL'], [and, [instance, '?IMPEL', 'Impelling'], [patient, '?IMPEL', '?OBJ'], [earlier, ['WhenFn', '?IMPEL'], ['WhenFn', '?IMPACT']]]]):-true.
subclass('Transportation', 'Motion'):-true.
'relatedInternalConcept'('Transportation', 'TransportationDevice'):-true.
documentation('Transportation', "&%Motion from one point to another by means 
of a &%TransportationDevice."):-true.
=>([instance, '?TRANS', 'Transportation'], [exists, ['?DEVICE'], [and, [instance, '?DEVICE', 'TransportationDevice'], [instrument, '?TRANS', '?DEVICE']]]):-true.
subclass('Guiding', 'IntentionalProcess'):-true.
documentation('Guiding', "Any &%IntentionalProcess where the &%agent tries to 
direct the movements of another &%Object, whether an &%Agent or not."):-true.
subclass('Steering', 'Guiding'):-true.
documentation('Steering', "Controlling the direction and/or speed of a 
&%TransportationDevice.  This includes navigating a ship, driving a car 
or truck, operating a train, etc."):-true.
=>([instance, '?STEER', 'Steering'], [exists, ['?VEHICLE'], [and, [instance, '?VEHICLE', 'TransportationDevice'], [patient, '?STEER', '?VEHICLE']]]):-true.
subclass('ChangeOfPossession', 'SocialInteraction'):-true.
'relatedInternalConcept'('ChangeOfPossession', possesses):-true.
documentation('ChangeOfPossession', "The &%Class of &%Processes where 
ownership of something is transferred from one &%Agent to another."):-true.
=>([and, [instance, '?CHANGE', 'ChangeOfPossession'], [patient, '?CHANGE', '?OBJ'], ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?CHANGE']], [possesses, '?AGENT1', '?OBJ']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?CHANGE']], [possesses, '?AGENT2', '?OBJ']]], [not, [equal, '?AGENT1', '?AGENT2']]):-true.
subclass('Giving', 'ChangeOfPossession'):-true.
documentation('Giving', "The &%subclass of &%ChangeOfPossession where the 
&%agent gives the &%destination something."):-true.
=>([and, [instance, '?GIVE', 'Giving'], [agent, '?GIVE', '?AGENT1'], [destination, '?GIVE', '?AGENT2'], [instance, '?AGENT2', 'Agent'], [patient, '?GIVE', '?OBJ']], [and, ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?GIVE']], [possesses, '?AGENT1', '?OBJ']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?GIVE']], [possesses, '?AGENT2', '?OBJ']]]):-true.
=>([and, [instance, '?GIVE', 'Giving'], [agent, '?GIVE', '?AGENT1'], [destination, '?GIVE', '?AGENT2'], [instance, '?AGENT2', 'Agent'], [patient, '?GIVE', '?OBJ']], [exists, ['?GET'], [and, [instance, '?GET', 'Getting'], [agent, '?GET', '?AGENT2'], [origin, '?GET', '?AGENT1'], [patient, '?GET', '?OBJ']]]):-true.
subclass('UnilateralGiving', 'Giving'):-true.
documentation('UnilateralGiving', "Any instance of &%Giving that is not part 
of a &%Transaction.  In other words, any instance of &%Giving where nothing 
is received in return.  Some examples of &%UnilateralGiving are:  honorary 
awards, gifts, and financial grants."):-true.
=>([instance, '?GIVE', 'UnilateralGiving'], [not, [exists, ['?TRANS'], [and, [instance, '?TRANS', 'Transaction'], ['subProcess', '?GIVE', '?TRANS']]]]):-true.
subclass('Lending', 'Giving'):-true.
documentation('Lending', "The &%subclass of &%Giving &%Processes where 
the &%agent gives the &%destination something for a limited period of 
time with the expectation that it will be returned later (perhaps with 
interest)."):-true.
<=>([and, [instance, '?BORROW', 'Borrowing'], [agent, '?BORROW', '?AGENT1'], [origin, '?BORROW', '?AGENT2'], [patient, '?BORROW', '?OBJECT']], [and, [instance, '?LEND', 'Lending'], [agent, '?LEND', '?AGENT2'], [destination, '?LEND', '?AGENT1'], [patient, '?LEND', '?OBJECT']]):-true.
subclass('Getting', 'ChangeOfPossession'):-true.
documentation('Getting', "The &%subclass of &%ChangeOfPossession where the
&%agent gets something.  Note that the source from which something is 
obtained is specified with the &%origin &%CaseRole."):-true.
subclass('UnilateralGetting', 'Getting'):-true.
'relatedInternalConcept'('UnilateralGetting', 'UnilateralGiving'):-true.
documentation('UnilateralGetting', "Any instance of &%Getting that is not part 
of a &%Transaction.  In other words, any instance of &%Getting where nothing 
is given in return.  Some examples of &%UnilateralGetting are:  appropriating, 
commandeering, stealing, etc."):-true.
=>([instance, '?GET', 'UnilateralGetting'], [not, [exists, ['?TRANS'], [and, [instance, '?TRANS', 'Transaction'], ['subProcess', '?GET', '?TRANS']]]]):-true.
subclass('Borrowing', 'Getting'):-true.
documentation('Borrowing', "The &%subclass of &%Getting &%Processes where 
the &%agent gets something for a limited period of time with the expectation 
that it will be returned later (perhaps with interest)."):-true.
subclass('Transaction', 'ChangeOfPossession'):-true.
subclass('Transaction', 'DualObjectProcess'):-true.
documentation('Transaction', "The &%subclass of &%ChangeOfPossession where 
something is exchanged for something else."):-true.
=>([instance, '?TRANS', 'Transaction'], [exists, ['?AGENT1', '?AGENT2', '?GIVE1', '?GIVE2', '?OBJ1', '?OBJ2'], [and, [instance, '?GIVE1', 'Giving'], [instance, '?GIVE2', 'Giving'], ['subProcess', '?GIVE1', '?TRANS'], ['subProcess', '?GIVE2', '?TRANS'], [agent, '?GIVE1', '?AGENT1'], [agent, '?GIVE2', '?AGENT2'], [patient, '?GIVE1', '?OBJ1'], [patient, '?GIVE2', '?OBJ2'], [destination, '?GIVE1', '?AGENT2'], [destination, '?GIVE2', '?AGENT1'], [not, [equal, '?AGENT1', '?AGENT2']], [not, [equal, '?OBJ1', '?OBJ2']]]]):-true.
subclass('FinancialTransaction', 'Transaction'):-true.
documentation('FinancialTransaction', "A &%Transaction where an instance 
of &%CurrencyMeasure is exchanged for something else."):-true.
=>([instance, '?TRANS', 'FinancialTransaction'], [exists, ['?OBJ'], [and, [patient, '?TRANS', '?OBJ'], [instance, '?OBJ', 'Currency']]]):-true.
subclass('Betting', 'FinancialTransaction'):-true.
documentation('Betting', "A &%FinancialTransaction where an instance of 
&%CurrencyMeasure is exchanged for the possibility of winning a larger 
instance of &%CurrencyMeasure within the context of some sort of 
&%Game."):-true.
subclass('Buying', 'FinancialTransaction'):-true.
'relatedInternalConcept'('Buying', 'Selling'):-true.
documentation('Buying', "A &%FinancialTransaction in which an instance of 
&%CurrencyMeasure is exchanged for an instance of &%Physical."):-true.
subclass('Selling', 'FinancialTransaction'):-true.
documentation('Selling', "A &%FinancialTransaction in which an instance of 
&%Physical is exchanged for an instance of &%CurrencyMeasure."):-true.
<=>([and, [instance, '?BUY', 'Buying'], [agent, '?BUY', '?AGENT1'], [origin, '?BUY', '?AGENT2'], [patient, '?BUY', '?OBJECT']], [and, [instance, '?SELL', 'Selling'], [agent, '?SELL', '?AGENT2'], [destination, '?SELL', '?AGENT1'], [patient, '?SELL', '?OBJECT']]):-true.
subclass('Learning', 'IntentionalPsychologicalProcess'):-true.
documentation('Learning', "The &%Class of &%Processes which relate to the 
acquisition of information."):-true.
=>([and, [instance, '?LEARN', 'Learning'], [agent, '?LEARN', '?AGENT']], [instance, '?AGENT', 'CognitiveAgent']):-true.
=>(['holdsDuring', '?TIME', [exists, ['?LEARN'], [and, [instance, '?LEARN', 'Learning'], [agent, '?LEARN', '?AGENT'], [patient, '?LEARN', '?PROP']]]], ['holdsDuring', ['ImmediateFutureFn', '?TIME'], [believes, '?AGENT', '?PROP']]):-true.
subclass('Classifying', 'IntentionalPsychologicalProcess'):-true.
documentation('Classifying', "The &%Class of &%IntentionalPsychologicalProcesses 
which involve attaching a name or category to a thing or set of things.  
Note that &%Classifying is distinguished from &%Learning by the fact 
that the latter covers the acquisition by a &%CognitiveAgent of any 
&%Proposition, while the former involves the assignment of a label 
or category."):-true.
subclass('Reasoning', 'IntentionalPsychologicalProcess'):-true.
documentation('Reasoning', "The &%Class of &%IntentionalPsychologicalProcesses 
which involve concluding, on the basis of either deductive or inductive 
evidence, that a particular &%Proposition or &%Sentence is true."):-true.
=>([instance, '?AGENT', 'CognitiveAgent'], [capability, 'Reasoning', agent, '?AGENT']):-true.
subclass('Selecting', 'IntentionalPsychologicalProcess'):-true.
documentation('Selecting', "The &%Class of &%IntentionalPsychologicalProcesses 
which involve opting for one or more &%Entity out of a larger set of &%Entities."):-true.
subclass('Comparing', 'IntentionalPsychologicalProcess'):-true.
subclass('Comparing', 'DualObjectProcess'):-true.
documentation('Comparing', "The &%Class of &%IntentionalPsychologicalProcesses 
which involve comparing, relating, contrasting, etc. the properties of 
two or more &%Entities."):-true.
subclass('Calculating', 'IntentionalPsychologicalProcess'):-true.
documentation('Calculating', "&%IntentionalPsychologicalProcesses which involve 
the consideration and/or manipulation of instances of &%Quantity."):-true.
subclass('Measuring', 'Calculating'):-true.
documentation('Measuring', "The &%Class of &%Calculating &%Processes where 
the aim is to determine the &%PhysicalQuantity of some aspect of the &%patient."):-true.
=>([and, [instance, '?MEAS', 'Measuring'], [agent, '?MEAS', '?AGENT'], [patient, '?MEAS', '?OBJ']], [exists, ['?QUANT', '?UNIT'], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?MEAS']], [knows, '?AGENT', [measure, '?OBJ', ['MeasureFn', '?QUANT', '?UNIT']]]]]):-true.
subclass('Counting', 'Calculating'):-true.
documentation('Counting', "Enumerating something.  The &%Class of &%Calculating 
&%Processes where the aim is to determine the &%Number corresponding to the 
&%patient."):-true.
=>([and, [instance, '?COUNT', 'Counting'], [agent, '?COUNT', '?AGENT'], [patient, '?COUNT', '?ENTITY']], [exists, ['?NUMBER'], [knows, '?AGENT', [equal, ['CardinalityFn', '?ENTITY'], '?NUMBER']]]):-true.
subclass('Predicting', 'IntentionalPsychologicalProcess'):-true.
documentation('Predicting', "The &%Class of &%IntentionalPsychologicalProcesses 
which involve the formulation of a &%Proposition about a state of affairs 
which might be realized in the future."):-true.
=>([and, [instance, '?PREDICT', 'Predicting'], [patient, '?PREDICT', '?FORMULA']], [exists, ['?TIME'], [and, ['holdsDuring', '?TIME', '?FORMULA'], [or, [before, '?TIME', ['WhenFn', '?PREDICT']], [earlier, '?TIME', ['WhenFn', '?PREDICT']]]]]):-true.
subclass('Remembering', 'PsychologicalProcess'):-true.
documentation('Remembering', "The &%Class of &%PsychologicalProcesses which 
involve the recollection of prior experiences and/or of knowledge 
which was previously acquired."):-true.
=>([and, [instance, '?REMEMBER', 'Remembering'], [patient, '?REMEMBER', '?FORMULA']], [exists, ['?TIME'], [and, ['holdsDuring', '?TIME', '?FORMULA'], [or, [before, ['WhenFn', '?REMEMBER'], '?TIME'], [earlier, ['WhenFn', '?REMEMBER'], '?TIME']]]]):-true.
subclass('Keeping', 'IntentionalProcess'):-true.
documentation('Keeping', "The &%Class of &%Processes where the &%agent 
keeps something in a particular location for an extended period of time."):-true.
=>([and, [instance, '?KEEP', 'Keeping'], [agent, '?KEEP', '?AGENT'], [patient, '?KEEP', '?OBJ']], [exists, ['?PUT'], [and, [instance, '?PUT', 'Putting'], [agent, '?PUT', '?AGENT'], [patient, '?PUT', '?OBJ'], [earlier, ['WhenFn', '?PUT'], ['WhenFn', '?KEEP']]]]):-true.
=>([and, [instance, '?KEEP', 'Keeping'], [patient, '?KEEP', '?OBJ']], [exists, ['?PLACE'], [forall, ['?TIME'], [=>, ['temporalPart', '?TIME', ['WhenFn', '?KEEP']], ['holdsDuring', '?TIME', [located, '?OBJ', '?PLACE']]]]]):-true.
subclass('Confining', 'Keeping'):-true.
documentation('Confining', "The &%Class of &%Keeping &%Processes where the 
&%patient is &%Human and is kept against his/her will.  This covers 
imprisonment, being jailed, held in custody, etc."):-true.
=>([instance, '?CONFINE', 'Confining'], [exists, ['?HUMAN'], [and, [instance, '?HUMAN', 'Human'], [patient, '?CONFINE', '?HUMAN']]]):-true.
=>([and, [instance, '?CONFINE', 'Confining'], [patient, '?CONFINE', '?PERSON']], [not, [desires, '?PERSON', [patient, '?CONFINE', '?PERSON']]]):-true.
subclass('Maintaining', 'IntentionalProcess'):-true.
documentation('Maintaining', "The &%Class of &%Processes where the &%agent 
cares for or maintains the &%Object."):-true.
subclass('Repairing', 'IntentionalProcess'):-true.
'relatedInternalConcept'('Repairing', 'Maintaining'):-true.
documentation('Repairing', "The &%Class of &%Processes where the &%agent 
makes a modification or series of modifications to an &%Object that is not 
functioning as intended so that it works properly."):-true.
=>([and, [instance, '?REPAIR', 'Repairing'], [patient, '?REPAIR', '?OBJ']], [exists, ['?DAMAGE'], [and, [instance, '?DAMAGE', 'Damaging'], [patient, '?DAMAGE', '?OBJ'], [earlier, ['WhenFn', '?DAMAGE'], ['WhenFn', '?REPAIR']]]]):-true.
subclass('TherapeuticProcess', 'Repairing'):-true.
documentation('TherapeuticProcess', "A &%Process that is carried out 
for the purpose of curing, improving or reducing the pain associated 
with a &%DiseaseOrSyndrome."):-true.
=>([and, [instance, '?PROC', 'TherapeuticProcess'], [patient, '?PROC', '?BIO']], [or, [instance, '?BIO', 'Organism'], [exists, ['?ORG'], [and, [instance, '?ORG', 'Organism'], [part, '?BIO', '?ORG']]]]):-true.
subclass('Surgery', 'TherapeuticProcess'):-true.
documentation('Surgery', "Any &%TherapeuticProcess that involves making an 
incision in the &%Animal that is the &%patient of the &%TherapeuticProcess."):-true.
=>([and, [instance, '?ACT', 'Surgery'], [patient, '?ACT', '?ANIMAL']], [exists, ['?SUBACT'], [and, [instance, '?SUBACT', 'Cutting'], [instance, '?ANIMAL', 'Animal'], [patient, '?ANIMAL', '?CUTTING'], ['subProcess', '?SUBACT', '?ACT']]]):-true.
subclass('Damaging', 'InternalChange'):-true.
disjoint('Damaging', 'Repairing'):-true.
documentation('Damaging', "The &%Class of &%Processes where the &%agent 
brings about a situation where the &%patient no longer functions normally 
or as intended."):-true.
subclass('Destruction', 'Damaging'):-true.
documentation('Destruction', "The &%subclass of &%Damagings in which 
the &%patient (or an essential element of the &%patient) is destroyed.  
Note that the difference between this concept and its superclass is solely 
one of extent."):-true.
<=>([instance, '?PROCESS', 'Destruction'], [exists, ['?PATIENT'], [and, [patient, '?PROCESS', '?PATIENT'], [time, '?PATIENT', ['ImmediatePastFn', ['WhenFn', '?PROCESS']]], [not, [time, '?PATIENT', ['ImmediateFutureFn', ['WhenFn', '?PROCESS']]]]]]):-true.
subclass('Killing', 'Destruction'):-true.
documentation('Killing', "The &%subclass of &%Destruction in which the 
death of an &%Organism is caused by an &%Organism.  Note that in cases 
of suicide the &%Organism would be the same in both cases."):-true.
=>([and, [instance, '?KILL', 'Killing'], [agent, '?KILL', '?AGENT'], [patient, '?KILL', '?PATIENT']], [and, [instance, '?AGENT', 'Organism'], [instance, '?PATIENT', 'Organism']]):-true.
=>([and, [instance, '?KILL', 'Killing'], [patient, '?KILL', '?PATIENT']], [and, ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?KILL']], [attribute, '?PATIENT', 'Living']], ['holdsDuring', ['FutureFn', ['WhenFn', '?KILL']], [attribute, '?PATIENT', 'Dead']]]):-true.
subclass('Poking', 'IntentionalProcess'):-true.
documentation('Poking', "The &%Class of &%Processes where the &%agent
pierces the surface of the &%Object with an &%instrument."):-true.
=>([and, [instance, '?POKE', 'Poking'], [agent, '?POKE', '?AGENT'], [patient, '?POKE', '?OBJ'], [instrument, '?POKE', '?INST']], ['holdsDuring', ['WhenFn', '?POKE'], [connects, '?INST', '?AGENT', '?OBJ']]):-true.
subclass('Cutting', 'Poking'):-true.
documentation('Cutting', "The &%subclass of &%Poking &%Processes which 
involve a sharp &%instrument."):-true.
subclass('Attaching', 'DualObjectProcess'):-true.
disjoint('Attaching', 'Detaching'):-true.
'relatedInternalConcept'('Attaching', 'Putting'):-true.
documentation('Attaching', "A &%Process where one &%Object becomes attached 
to another &%Object.  Note that this differs from &%Putting in that two 
things which are attached may already be in the same location."):-true.
=>([and, [instance, '?ATTACH', 'Attaching'], [patient, '?ATTACH', '?OBJ1'], [patient, '?ATTACH', '?OBJ2']], [and, ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?ATTACH']], [not, [connected, '?OBJ1', '?OBJ2']]], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?ATTACH']], [connected, '?OBJ1', '?OBJ2']]]):-true.
subclass('Detaching', 'DualObjectProcess'):-true.
documentation('Detaching', "A &%Process where the &%agent detaches one thing 
from something else.  Note that this is different from &%Removing in that 
neither of the two things which are detached may be removed from the location 
where it was attached."):-true.
=>([and, [instance, '?DETACH', 'Detaching'], [patient, '?DETACH', '?OBJ1'], [patient, '?DETACH', '?OBJ2']], [and, ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?DETACH']], [connected, '?OBJ1', '?OBJ2']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?DETACH']], [not, [connected, '?OBJ1', '?OBJ2']]]]):-true.
subclass('Combining', 'DualObjectProcess'):-true.
documentation('Combining', "A &%Process where two or more things are combined 
into a single thing."):-true.
<=>([and, [instance, '?COMBINE', 'Combining'], [resource, '?COMBINE', '?OBJ1'], [result, '?COMBINE', '?OBJ2']], [and, ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?COMBINE']], [not, [part, '?OBJ1', '?OBJ2']]], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?COMBINE']], [part, '?OBJ1', '?OBJ2']]]):-true.
subclass('Separating', 'DualObjectProcess'):-true.
disjoint('Separating', 'Combining'):-true.
documentation('Separating', "A &%Process where something is disassembled 
into (some of) its parts."):-true.
subclass('ChemicalProcess', 'InternalChange'):-true.
documentation('ChemicalProcess', "A &%ChemicalProcess occurs whenever 
chemical compounds (&%CompoundSubstances) are formed or decomposed.  
For example, reactants disappear as chemical change occurs, and products 
appear as chemical change occurs.  In a chemical change a chemical 
reaction takes place.  Catalysts in a &%ChemicalProcess may speed up the 
reaction, but aren't themselves produced or consumed.  Examples: rusting of 
iron and the decomposition of water, induced by an electric current, to 
gaseous hydrogen and gaseous oxygen."):-true.
=>([and, [instance, '?PROC', 'ChemicalProcess'], [or, [resource, '?PROC', '?STUFF'], [result, '?PROC', '?STUFF']]], [instance, '?STUFF', 'PureSubstance']):-true.
subclass('ChemicalSynthesis', 'ChemicalProcess'):-true.
subclass('ChemicalSynthesis', 'Combining'):-true.
documentation('ChemicalSynthesis', "The &%Class of &%ChemicalProcesses in 
which a &%CompoundSubstance is formed from simpler reactants."):-true.
=>([and, [resource, '?PROC', '?SUBSTANCE1'], [result, '?PROC', '?SUBSTANCE2'], [instance, '?SUBSTANCE1', 'ElementalSubstance'], [instance, '?SUBSTANCE2', 'CompoundSubstance']], [instance, '?PROC', 'ChemicalSynthesis']):-true.
<=>([instance, '?COMPOUND', 'CompoundSubstance'], [exists, ['?ELEMENT1', '?ELEMENT2', '?PROCESS'], [and, [instance, '?ELEMENT1', 'ElementalSubstance'], [instance, '?ELEMENT2', 'ElementalSubstance'], [not, [equal, '?ELEMENT1', '?ELEMENT2']], [instance, '?PROCESS', 'ChemicalSynthesis'], [resource, '?PROCESS', '?ELEMENT1'], [resource, '?PROCESS', '?ELEMENT2'], [result, '?PROCESS', '?COMPOUND']]]):-true.
subclass('ChemicalDecomposition', 'ChemicalProcess'):-true.
subclass('ChemicalDecomposition', 'Separating'):-true.
documentation('ChemicalDecomposition', "The &%Class of &%ChemicalProcesses 
in which a &%CompoundSubstance breaks down into simpler products."):-true.
=>([and, [resource, '?PROC', '?SUBSTANCE1'], [result, '?PROC', '?SUBSTANCE2'], [instance, '?SUBSTANCE1', 'CompoundSubstance'], [instance, '?SUBSTANCE2', 'ElementalSubstance']], [instance, '?PROC', 'ChemicalDecomposition']):-true.
subclass('Combustion', 'ChemicalDecomposition'):-true.
documentation('Combustion', "The &%Class of &%ChemicalProcesses in which an &%Object 
reacts with oxygen and gives off heat.  This includes all &%Processes in which 
something is burning."):-true.
=>([instance, '?COMBUSTION', 'Combustion'], [exists, ['?HEAT', '?LIGHT'], [and, [instance, '?HEAT', 'Heating'], [instance, '?LIGHT', 'RadiatingLight'], ['subProcess', '?HEAT', '?COMBUSTION'], ['subProcess', '?LIGHT', '?COMBUSTION']]]):-true.
subclass('InternalChange', 'Process'):-true.
documentation('InternalChange', "&%Processes which involve altering an internal 
property of an &%Object, e.g. the shape of the &%Object, its coloring, its 
structure, etc.  &%Processes that are not instances of this class include 
changes that only affect the relationship to other objects, e.g. changes in 
spatial or temporal location."):-true.
=>([and, [instance, '?CHANGE', 'InternalChange'], [patient, '?CHANGE', '?OBJ']], [exists, ['?PROPERTY'], [or, [and, ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?CHANGE']], [attribute, '?OBJ', '?PROPERTY']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?CHANGE']], [not, [attribute, '?OBJ', '?PROPERTY']]]], [and, ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?CHANGE']], [not, [attribute, '?OBJ', '?PROPERTY']]], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?CHANGE']], [attribute, '?OBJ', '?PROPERTY']]]]]):-true.
subclass('SurfaceChange', 'InternalChange'):-true.
documentation('SurfaceChange', "&%Processes which involve altering 
the properties that apply to the surface of an &%Object."):-true.
=>([and, [instance, '?ALT', 'SurfaceChange'], [patient, '?ALT', '?OBJ']], [exists, ['?PART', '?PROPERTY'], [and, ['superficialPart', '?PART', '?OBJ'], ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?ALT']], [attribute, '?PART', '?PROPERTY']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?ALT']], [not, [attribute, '?PART', '?PROPERTY']]]]]):-true.
subclass('ShapeChange', 'Process'):-true.
documentation('ShapeChange', "The &%Process of changing the shape of an &%Object."):-true.
=>([and, [instance, '?ALT', 'ShapeChange'], [patient, '?ALT', '?OBJ']], [exists, ['?PROPERTY'], [and, [instance, '?PROPERTY', 'ShapeAttribute'], [or, [and, ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?ALT']], [attribute, '?OBJ', '?PROPERTY']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?ALT']], [not, [attribute, '?OBJ', '?PROPERTY']]]], [and, ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?ALT']], [not, [attribute, '?OBJ', '?PROPERTY']]], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?ALT']], [attribute, '?OBJ', '?PROPERTY']]]]]]):-true.
subclass('Coloring', 'SurfaceChange'):-true.
documentation('Coloring', "The &%subclass of &%SurfaceChange where a 
&%ColorAttribute of the &%patient is altered."):-true.
=>([and, [instance, '?COLORING', 'Coloring'], [patient, '?COLORING', '?OBJ']], [exists, ['?PROPERTY'], [and, [instance, '?PROPERTY', 'ColorAttribute'], ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?COLORING']], [attribute, '?OBJ', '?PROPERTY']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?COLORING']], [not, [attribute, '?OBJ', '?PROPERTY']]]]]):-true.
subclass('ContentDevelopment', 'IntentionalProcess'):-true.
documentation('ContentDevelopment', "A &%subclass of &%IntentionalProcess in 
which content is modified, its form is altered or it is created anew."):-true.
=>([instance, '?DEVELOP', 'ContentDevelopment'], [exists, ['?OBJ'], [and, [instance, '?OBJ', 'ContentBearingObject'], [patient, '?DEVELOP', '?OBJ']]]):-true.
subclass('Reading', 'ContentDevelopment'):-true.
documentation('Reading', "A &%subclass of &%ContentDevelopment in which 
content is converted from a written form into a spoken or mental representation."):-true.
=>([instance, '?READ', 'Reading'], [exists, ['?TEXT', '?PROP'], [and, [instance, '?TEXT', 'Text'], ['containsInformation', '?TEXT', '?PROP'], [realization, '?READ', '?PROP']]]):-true.
subclass('Writing', 'ContentDevelopment'):-true.
documentation('Writing', "A &%subclass of &%ContentDevelopment in which 
content is converted from one form (e.g. uttered, written or represented 
mentally) into a written form.  Note that this class covers both 
transcription and original creation of written &%Texts."):-true.
subclass('Encoding', 'Writing'):-true.
documentation('Encoding', "Converting a document or message into a formal 
language or into a code that can be understood only by a relatively small 
body of &%Agents.  Generally speaking, this hinders wide dissemination of 
the content in the original document or message."):-true.
subclass('Decoding', 'Writing'):-true.
disjoint('Decoding', 'Encoding'):-true.
documentation('Decoding', "Converting a document or message that has previously 
been encoded (see &%Encoding) into a &%Language that can be understood by a 
relatively large number of speakers."):-true.
=>([and, [instance, '?DECODE', 'Decoding'], [patient, '?DECODE', '?DOC1']], [exists, ['?ENCODE', '?DOC2', '?TIME'], [and, ['containsInformation', '?DOC2', '?PROP'], ['containsInformation', '?DOC1', '?PROP'], ['temporalPart', '?TIME', ['PastFn', ['WhenFn', '?DECODE']]], ['holdsDuring', '?TIME', [and, [instance, '?ENCODE', 'Encoding'], [patient, '?ENCODE', '?DOC2']]]]]):-true.
subclass('Wetting', 'Combining'):-true.
documentation('Wetting', "The &%Class of &%Processes where a &%Liquid is 
added to an &%Object."):-true.
=>([and, [instance, '?WET', 'Wetting'], [patient, '?WET', '?OBJ']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?WET']], [or, [attribute, '?OBJ', 'Wet'], [attribute, '?OBJ', 'Damp']]]):-true.
=>([instance, '?WET', 'Wetting'], [exists, ['?OBJ'], [and, [attribute, '?OBJ', 'Liquid'], [patient, '?WET', '?OBJ']]]):-true.
subclass('Drying', 'Separating'):-true.
documentation('Drying', "The &%Class of &%Processes where a &%Liquid is removed 
from an &%Object."):-true.
=>([and, [instance, '?DRY', 'Drying'], [patient, '?DRY', '?OBJ']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?DRY']], [attribute, '?OBJ', 'Dry']]):-true.
subclass('Creation', 'InternalChange'):-true.
'relatedInternalConcept'('Creation', 'Destruction'):-true.
documentation('Creation', "The &%subclass of &%Process in which 
something is created.  Note that the thing created is specified 
with the &%result &%CaseRole."):-true.
=>([instance, '?ACTION', 'Creation'], [exists, ['?RESULT'], [result, '?ACTION', '?RESULT']]):-true.
<=>([instance, '?PROCESS', 'Creation'], [exists, ['?PATIENT'], [and, [patient, '?PROCESS', '?PATIENT'], [time, '?PATIENT', ['ImmediateFutureFn', ['WhenFn', '?PROCESS']]], [not, [time, '?PATIENT', ['ImmediatePastFn', ['WhenFn', '?PROCESS']]]]]]):-true.
subclass('Making', 'Creation'):-true.
subclass('Making', 'IntentionalProcess'):-true.
documentation('Making', "The &%subclass of &%Creation in which an individual 
&%Artifact or a type of &%Artifact is made."):-true.
=>([instance, '?MAKE', 'Making'], [exists, ['?ARTIFACT'], [and, [instance, '?ARTIFACT', 'Artifact'], [result, '?MAKE', '?ARTIFACT']]]):-true.
subclass('Constructing', 'Making'):-true.
documentation('Constructing', "The &%subclass of &%Making in which a 
&%StationaryArtifact is built."):-true.
<=>([exists, ['?BUILD'], [and, [instance, '?BUILD', 'Constructing'], [result, '?BUILD', '?ARTIFACT']]], [instance, '?ARTIFACT', 'StationaryArtifact']):-true.
subclass('Manufacture', 'Making'):-true.
documentation('Manufacture', "The &%Making of &%Artifacts on a mass 
scale."):-true.
subclass('Publication', 'Manufacture'):-true.
documentation('Publication', "The &%Manufacture of &%Texts.  Note that 
there is no implication that the &%Texts are distributed.  Such 
distribution, when it occurs, is an instance of &%Dissemination."):-true.
=>([and, [instance, '?PUB', 'Publication'], [patient, '?PUB', '?TEXT']], [subclass, '?TEXT', 'Text']):-true.
subclass('Cooking', 'Making'):-true.
documentation('Cooking', "The &%Making of an &%instance of &%Food."):-true.
=>([instance, '?COOK', 'Cooking'], [exists, ['?FOOD'], [and, [instance, '?FOOD', 'Food'], [result, '?COOK', '?FOOD']]]):-true.
subclass('Searching', 'IntentionalProcess'):-true.
documentation('Searching', "Any &%Process where the aim is to find 
something, whether it be information (i.e. a &%Proposition) or an 
&%Object."):-true.
=>([and, [instance, '?SEARCH', 'Searching'], [agent, '?SEARCH', '?AGENT'], [patient, '?SEARCH', '?ENTITY']], ['inScopeOfInterest', '?AGENT', '?ENTITY']):-true.
subclass('Pursuing', 'Searching'):-true.
documentation('Pursuing', "The &%subclass of &%Searching where the thing 
sought is an &%Object.  Some examples would be hunting, shopping, 
trawling, and stalking."):-true.
=>([and, [instance, '?PURSUE', 'Pursuing'], [agent, '?PURSUE', '?AGENT'], [patient, '?PURSUE', '?OBJ']], ['holdsDuring', '?PURSUE', [wants, '?AGENT', '?OBJ']]):-true.
=>([and, [instance, '?PURSUE', 'Pursuing'], [agent, '?PURSUE', '?AGENT'], [patient, '?PURSUE', '?OBJ']], ['holdsDuring', '?PURSUE', [not, [possesses, '?AGENT', '?OBJ']]]):-true.
subclass('Investigating', 'Searching'):-true.
documentation('Investigating', "The &%subclass of &%Searching where the 
thing sought is a piece of information (i.e. a &%Proposition denoted by 
a &%Formula)."):-true.
=>([and, [instance, '?INVESTIGATE', 'Investigating'], [patient, '?INVESTIGATE', '?PROP']], [instance, '?PROP', 'Formula']):-true.
=>([and, [instance, '?INVESTIGATE', 'Investigating'], [agent, '?INVESTIGATE', '?AGENT'], [patient, '?INVESTIGATE', '?PROP']], ['holdsDuring', ['WhenFn', '?INVESTIGATE'], [not, [knows, '?AGENT', '?PROP']]]):-true.
subclass('DiagnosticProcess', 'Investigating'):-true.
documentation('DiagnosticProcess', "A &%Process that is carried out for 
the purpose of determining the nature of a &%DiseaseOrSyndrome."):-true.
=>([and, [instance, '?PROC', 'DiagnosticProcess'], [agent, '?PROC', '?AGENT']], [exists, ['?CAUSE'], ['hasPurposeForAgent', '?PROC', [knows, '?AGENT', [causes, '?CAUSE', '?PROC']], '?AGENT']]):-true.
subclass('SocialInteraction', 'IntentionalProcess'):-true.
documentation('SocialInteraction', "The &%subclass of 
&%IntentionalProcess that involves interactions between 
&%CognitiveAgents."):-true.
=>([instance, '?INTERACTION', 'SocialInteraction'], [exists, ['?AGENT1', '?AGENT2'], [and, [agent, '?INTERACTION', '?AGENT1'], [agent, '?INTERACTION', '?AGENT2'], [not, [equal, '?AGENT1', '?AGENT2']]]]):-true.
subclass('Pretending', 'SocialInteraction'):-true.
documentation('Pretending', "Any &%SocialInteraction where a 
&%CognitiveAgent or &%Group of &%CognitiveAgents attempts to make 
another &%CognitiveAgent or &%Group of &%CognitiveAgents believe 
something that is false.  This covers deceit, affectation, 
impersonation, and entertainment productions, to give just a few 
examples."):-true.
=>([instance, '?PRETEND', 'Pretending'], [exists, ['?PERSON', '?PROP'], [and, ['hasPurpose', '?PRETEND', [believes, '?PERSON', '?PROP']], [true, '?PROP', 'True']]]):-true.
subclass('Communication', 'SocialInteraction'):-true.
'relatedInternalConcept'('Communication', 'ContentDevelopment'):-true.
documentation('Communication', "A &%SocialInteraction that involves 
the transfer of information between two or more &%CognitiveAgents 
via a &%ContentBearingObject.  Note that &%Communication is closely 
related to, but essentially different from, &%ContentDevelopment.  
The latter involves the creation or modification of a &%ContentBearingObject, 
while &%Communication is the transfer of such an object for the purpose 
of conveying a message."):-true.
=>([instance, '?COMMUNICATE', 'Communication'], [exists, ['?OBJ', '?AGENT1', '?AGENT2'], [and, [instance, '?OBJ', 'ContentBearingObject'], [patient, '?COMMUNICATE', '?OBJ'], [instance, '?AGENT1', 'CognitiveAgent'], [agent, '?COMMUNICATE', '?AGENT1'], [instance, '?AGENT2', 'CognitiveAgent'], [destination, '?COMMUNICATE', '?AGENT2']]]):-true.
subclass('Disseminating', 'Communication'):-true.
documentation('Disseminating', "Any &%Communication that involves a 
single &%agent and many &%destinations.  This covers the release 
of a published book, broadcasting, a theatrical performance, giving 
orders to assembled troops, delivering a public lecture, etc."):-true.
=>([instance, '?DISSEMINATE', 'Disseminating'], [exists, ['?AGENT1', '?AGENT2'], [and, [destination, '?DISSEMINATE', '?AGENT1'], [instance, '?AGENT1', 'CognitiveAgent'], [destination, '?DISSEMINATE', '?AGENT2'], [instance, '?AGENT2', 'CognitiveAgent'], [not, [equal, '?AGENT1', '?AGENT2']]]]):-true.
subclass('Advertising', 'Disseminating'):-true.
documentation('Advertising', "A &%Disseminating whose purpose is to 
promote the sale of an &%Object represented in a &%Text or &%Icon 
(the advertisement)."):-true.
=>([instance, '?ADVERT', 'Advertising'], [exists, ['?OBJ'], [and, [refers, '?ADVERT', '?OBJ'], ['hasPurpose', '?ADVERT', [exists, ['?SALE'], [and, [instance, '?SALE', 'Selling'], [patient, '?SALE', '?OBJ']]]]]]):-true.
subclass('Stating', 'Communication'):-true.
documentation('Stating', "Instances of this &%Class commit a sender to some truth. 
Example: John concluded that Peter was not present, and predicted that he would 
arrive soon."):-true.
=>([and, [instance, '?STATE', 'Stating'], [agent, '?STATE', '?AGENT'], [patient, '?STATE', '?FORMULA'], [instance, '?FORMULA', 'Formula']], ['holdsDuring', ['WhenFn', '?STATE'], [believes, '?AGENT', '?FORMULA']]):-true.
subclass('Directing', 'Communication'):-true.
documentation('Directing', "Instances of this &%Class urge some further action 
among the receivers.  Example: The 5th Battalion requested air support from 
the 3rd Bomber Group."):-true.
subclass('Ordering', 'Directing'):-true.
documentation('Ordering', "A &%Directing in which the receiver is 
commanded to realize the content of a &%ContentBearingObject.  Orders 
are injunctions, the disobedience of which involves sanctions, or 
which express an obligation upon the part of the orderee."):-true.
=>([and, [instance, '?ORDER', 'Ordering'], [patient, '?ORDER', '?FORMULA']], ['modalAttribute', '?FORMULA', 'Obligation']):-true.
subclass('Requesting', 'Directing'):-true.
documentation('Requesting', "A request expresses a desire by the &%agent of the 
request that the &%destination of the request perform some future action."):-true.
=>([and, [instance, '?REQUEST', 'Requesting'], [agent, '?REQUEST', '?AGENT'], [patient, '?REQUEST', '?FORMULA'], [instance, '?FORMULA', 'Formula']], [desires, '?AGENT', '?FORMULA']):-true.
subclass('Questioning', 'Directing'):-true.
documentation('Questioning', "A request for information."):-true.
=>([and, [instance, '?QUESTION', 'Questioning'], [agent, '?QUESTION', '?AGENT'], [patient, '?QUESTION', '?FORMULA'], [instance, '?FORMULA', 'Formula']], ['holdsDuring', ['WhenFn', '?QUESTION'], [not, [knows, '?AGENT', '?FORMULA']]]):-true.
subclass('Committing', 'Communication'):-true.
documentation('Committing', "Instances of this &%Class commit the sender to some 
future course. Example: Bob promised Susan that he would be home by 11pm."):-true.
=>([and, [instance, '?COMMIT', 'Committing'], [patient, '?COMMIT', '?FORMULA'], [instance, '?FORMULA', 'Formula']], ['modalAttribute', '?FORMULA', 'Promise']):-true.
subclass('Expressing', 'Communication'):-true.
documentation('Expressing', "Instances of this &%Class express a state of the sender. 
Example: Jane thanked Barbara for the present she had given her."):-true.
=>([and, [instance, '?EXPRESS', 'Expressing'], [agent, '?EXPRESS', '?AGENT']], [exists, ['?STATE'], [and, [instance, '?STATE', 'StateOfMind'], [attribute, '?AGENT', '?STATE'], [represents, '?EXPRESS', '?STATE']]]):-true.
subclass('Declaring', 'Communication'):-true.
documentation('Declaring', "The &%Class of &%Communications that effect an 
institutional alteration when performed by competent authority.  Some 
examples are nominating, marrying, and excommunicating."):-true.
=>([and, [instance, '?DECLARE', 'Declaring'], [agent, '?DECLARE', '?AGENT1']], [exists, ['?PROC', '?AGENT2'], [or, ['confersRight', '?PROC', '?DECLARE', '?AGENT2'], ['confersObligation', '?PROC', '?DECLARE', '?AGENT2']]]):-true.
subclass('Cooperation', 'SocialInteraction'):-true.
documentation('Cooperation', "The &%subclass of &%SocialInteraction where 
the participants involved work together for the achievement of a common 
goal."):-true.
=>([instance, '?COOPERATE', 'Cooperation'], [exists, ['?PURP'], [forall, ['?AGENT'], [=>, [agent, '?COOPERATE', '?AGENT'], ['hasPurposeForAgent', '?COOPERATE', '?PURP', '?AGENT']]]]):-true.
subclass('Meeting', 'SocialInteraction'):-true.
documentation('Meeting', "The coming together of two or more 
&%CognitiveAgents for the purpose of &%Communication.  This covers informal 
meetings, e.g. visits with family members, and formal meetings, e.g. a board 
of directors meeting."):-true.
=>([and, [instance, '?MEET', 'Meeting'], [agent, '?MEET', '?AGENT1'], [agent, '?MEET', '?AGENT2']], ['holdsDuring', ['WhenFn', '?MEET'], [orientation, '?AGENT1', '?AGENT2', 'Near']]):-true.
=>([instance, '?MEET', 'Meeting'], [exists, ['?AGENT1', '?AGENT2'], [and, [agent, '?MEET', '?AGENT1'], [agent, '?MEET', '?AGENT2'], ['hasPurpose', '?MEET', [exists, ['?COMM'], [and, [instance, '?COMM', 'Communication'], [agent, '?COMM', '?AGENT1'], [agent, '?COMM', '?AGENT2']]]]]]):-true.
subclass('Contest', 'SocialInteraction'):-true.
documentation('Contest', "A &%SocialInteraction where the &%agent and 
&%patient are &%CognitiveAgents who are trying to defeat one another.  
Note that this concept is often applied in a metaphorical sense in natural 
language, when we speak, e.g., of the struggle of plants for space or 
sunlight, or of bacteria for food resources in some environment."):-true.
=>([instance, '?CONTEST', 'Contest'], [exists, ['?AGENT1', '?AGENT2', '?PURP1', '?PURP2'], [and, [agent, '?CONTEST', '?AGENT1'], [agent, '?CONTEST', '?AGENT2'], ['hasPurposeForAgent', '?CONTEST', '?PURP1', '?AGENT1'], ['hasPurposeForAgent', '?CONTEST', '?PURP2', '?AGENT2'], [not, [equal, '?AGENT1', '?AGENT2']], [not, [equal, '?PURP1', '?PURP2']]]]):-true.
subclass('ViolentContest', 'Contest'):-true.
documentation('ViolentContest', "A &%Contest where one participant attempts to 
physically injure another participant."):-true.
subclass('War', 'ViolentContest'):-true.
documentation('War', "A military confrontation between two or more 
&%Nations or &%Organizations whose members are &%Nations.  As the 
corresponding axiom specifies, a &%War is made up of &%Battles."):-true.
=>([instance, '?WAR', 'War'], [exists, ['?BATTLE'], [and, [instance, '?BATTLE', 'Battle'], ['subProcess', '?BATTLE', '?WAR']]]):-true.
=>([and, [instance, '?WAR', 'War'], [agent, '?WAR', '?AGENT']], [or, [instance, '?AGENT', 'Nation'], [and, [instance, '?AGENT', 'Organization'], [forall, ['?MEMBER'], [=>, [member, '?MEMBER', '?AGENT'], [instance, '?MEMBER', 'Nation']]]]]):-true.
subclass('Battle', 'ViolentContest'):-true.
documentation('Battle', "A &%ViolentContest between two or more military 
units within the context of a war.  Note that this does not cover the 
metaphorical sense of 'battle', which simply means a struggle of some 
sort.  This sense should be represented with the more general concept of 
&%Contest."):-true.
=>([instance, '?BATTLE', 'Battle'], [exists, ['?WAR'], [and, [instance, '?WAR', 'War'], ['subProcess', '?BATTLE', '?WAR']]]):-true.
=>([instance, '?BATTLE', 'Battle'], [exists, ['?ATTACK'], [and, [instance, '?ATTACK', 'ViolentContest'], ['subProcess', '?ATTACK', '?BATTLE']]]):-true.
subclass('Game', 'Contest'):-true.
subclass('Game', 'RecreationOrExercise'):-true.
documentation('Game', "A &%Contest whose purpose is the 
enjoyment/stimulation of the participants or spectators of the &%Game."):-true.
subclass('Sport', 'Game'):-true.
documentation('Sport', "A &%Game which requires some degree of physical 
exercion from the participants of the game."):-true.
subclass('LegalAction', 'Contest'):-true.
documentation('LegalAction', "Any &%Process where a &%CognitiveAgent seeks 
to obtain something from another &%CognitiveAgent through a court of law."):-true.
subclass('Maneuver', 'IntentionalProcess'):-true.
documentation('Maneuver', "An intentional move or play within a &%Contest.  
In many cases, a &%Maneuver is part of a strategy for winning the 
larger &%Contest of which the &%Maneuver is a part."):-true.
=>([instance, '?MOVE', 'Maneuver'], [exists, ['?CONTEST'], [and, [instance, '?CONTEST', 'Contest'], ['subProcess', '?MOVE', '?CONTEST']]]):-true.
subclass('Perception', 'PsychologicalProcess'):-true.
documentation('Perception', "Sensing some aspect of the material world.  
Note that the &%agent of this sensing is assumed to be an &%Animal."):-true.
=>([and, [instance, '?PERCEPT', 'Perception'], [agent, '?PERCEPT', '?AGENT']], [instance, '?AGENT', 'Animal']):-true.
=>([and, [instance, '?PERCEPT', 'Perception'], [agent, '?PERCEPT', '?AGENT'], [patient, '?PERCEPT', '?OBJECT']], [notices, '?AGENT', '?OBJECT']):-true.
=>([instance, '?AGENT', 'SentientAgent'], [capability, 'Perception', experiencer, '?AGENT']):-true.
subclass('Seeing', 'Perception'):-true.
documentation('Seeing', "The &%subclass of &%Perception in which the 
sensing is done by an ocular &%Organ."):-true.
=>([and, [instance, '?SEE', 'Seeing'], [agent, '?SEE', '?AGENT'], [patient, '?SEE', '?OBJ']], [and, [attribute, '?OBJ', 'Illuminated'], [exists, ['?PROP'], [and, [instance, '?PROP', 'ColorAttribute'], [knows, '?AGENT', [attribute, '?OBJ', '?PROP']]]]]):-true.
subclass('Smelling', 'Perception'):-true.
documentation('Smelling', "The &%subclass of &%Perception in which the 
sensing is done by an olefactory &%Organ."):-true.
=>([and, [instance, '?SMELL', 'Smelling'], [patient, '?SMELL', '?OBJ']], [exists, ['?PROP'], [and, [instance, '?PROP', 'OlfactoryAttribute'], [attribute, '?OBJ', '?PROP']]]):-true.
subclass('Tasting', 'Perception'):-true.
documentation('Tasting', "The &%subclass of &%Perception in which the 
sensing is done by of an &%Organ which can discriminate various tastes."):-true.
=>([and, [instance, '?TASTE', 'Tasting'], [patient, '?TASTE', '?OBJ']], [exists, ['?PROP'], [and, [instance, '?PROP', 'TasteAttribute'], [attribute, '?OBJ', '?PROP']]]):-true.
subclass('Hearing', 'Perception'):-true.
documentation('Hearing', "The &%subclass of &%Perception in which the 
sensing is done by an auditory &%Organ."):-true.
=>([and, [instance, '?HEAR', 'Hearing'], [patient, '?HEAR', '?OBJ']], [exists, ['?PROP'], [and, [instance, '?PROP', 'SoundAttribute'], [attribute, '?OBJ', '?PROP']]]):-true.
subclass('TactilePerception', 'Perception'):-true.
documentation('TactilePerception', "The &%subclass of &%Perception in which 
the sensing is done by &%Touching.  Note that &%Touching need not involve 
&%TactilePerception.  For example, a person who has lost all sensation in 
both of his legs would have no &%TactilePerception of anything his legs 
were &%Touching."):-true.
=>([instance, '?TACTILE', 'TactilePerception'], [exists, ['?TOUCH'], [and, [instance, '?TOUCH', 'Touching'], ['subProcess', '?TOUCH', '?TACTILE']]]):-true.
subclass('Radiating', 'Motion'):-true.
documentation('Radiating', "Processes in which some form of radiation, 
e.g. radio waves, light waves, electrical energy, etc., is given off 
or absorbed by something else."):-true.
subclass('RadiatingLight', 'Radiating'):-true.
documentation('RadiatingLight', "The &%subclass of &%Radiating in which 
light is given off or absorbed.  Some examples include blinking, flashing, 
and glittering."):-true.
<=>([exists, ['?EMIT'], [and, [instance, '?EMIT', 'RadiatingLight'], [patient, '?EMIT', '?REGION'], [instance, '?REGION', 'Region']]], [attribute, '?REGION', 'Illuminated']):-true.
subclass('RadiatingSound', 'Radiating'):-true.
documentation('RadiatingSound', "The &%subclass of &%Radiating in which 
sound waves are given off or absorbed.  Some examples include creaking, 
roaring, and whistling."):-true.
=>([and, [instance, '?EMIT', 'RadiatingSound'], [agent, '?EMIT', '?SOUND']], [exists, ['?PROP'], [and, [instance, '?PROP', 'SoundAttribute'], [attribute, '?SOUND', '?PROP']]]):-true.
subclass('Music', 'RadiatingSound'):-true.
documentation('Music', "The &%subclass of &%RadiatingSound where the 
sound is intended to be melodic and is produced deliberately."):-true.
subclass('StateChange', 'InternalChange'):-true.
documentation('StateChange', "Any &%Process where the &%PhysicalState 
of &%part of the &%patient of the &%Process changes."):-true.
=>([and, [instance, '?PROCESS', 'StateChange'], [patient, '?PROCESS', '?OBJ']], [exists, ['?PART', '?STATE1', '?STATE2'], [and, [part, '?PART', '?OBJ'], [instance, '?STATE1', 'PhysicalState'], [instance, '?STATE2', 'PhysicalState'], [not, [equal, '?STATE1', '?STATE2']], ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?PROCESS']], [attribute, '?PART', '?STATE1']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?FREEZE']], [attribute, '?PART', '?STATE2']]]]):-true.
subclass('Melting', 'StateChange'):-true.
documentation('Melting', "The &%Class of &%Processes where an &%Object is 
heated and converted from a &%Solid to a &%Liquid."):-true.
=>([instance, '?MELT', 'Melting'], [exists, ['?HEAT'], [and, [instance, '?HEAT', 'Heating'], ['subProcess', '?HEAT', '?MELT']]]):-true.
=>([and, [instance, '?MELT', 'Melting'], [patient, '?MELT', '?OBJ']], [exists, ['?PART'], [and, [part, '?PART', '?OBJ'], ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?MELT']], [attribute, '?PART', 'Solid']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?MELT']], [attribute, '?PART', 'Liquid']]]]):-true.
subclass('Boiling', 'StateChange'):-true.
documentation('Boiling', "The &%Class of &%Processes where an &%Object is 
heated and converted from a &%Liquid to a &%Gas."):-true.
=>([instance, '?BOIL', 'Boiling'], [exists, ['?HEAT'], [and, [instance, '?HEAT', 'Heating'], ['subProcess', '?HEAT', '?BOIL']]]):-true.
=>([and, [instance, '?BOIL', 'Boiling'], [patient, '?BOIL', '?OBJ']], [exists, ['?PART'], [and, [part, '?PART', '?OBJ'], ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?BOIL']], [attribute, '?PART', 'Liquid']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?BOIL']], [attribute, '?PART', 'Gas']]]]):-true.
subclass('Condensing', 'StateChange'):-true.
documentation('Condensing', "The &%Class of &%Processes where an &%Object is 
cooled and converted from a &%Gas to a &%Liquid."):-true.
=>([instance, '?COND', 'Condensing'], [exists, ['?COOL'], [and, [instance, '?COOL', 'Cooling'], ['subProcess', '?COOL', '?COND']]]):-true.
=>([and, [instance, '?COND', 'Condensing'], [patient, '?COND', '?OBJ']], [exists, ['?PART'], [and, [part, '?PART', '?OBJ'], ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?COND']], [attribute, '?PART', 'Gas']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?COND']], [attribute, '?PART', 'Liquid']]]]):-true.
subclass('Freezing', 'StateChange'):-true.
documentation('Freezing', "The &%Class of &%Processes where an &%Object is 
cooled and converted from a &%Liquid to a &%Solid."):-true.
=>([instance, '?FREEZE', 'Freezing'], [exists, ['?COOL'], [and, [instance, '?COOL', 'Cooling'], ['subProcess', '?COOL', '?FREEZE']]]):-true.
=>([and, [instance, '?FREEZE', 'Freezing'], [patient, '?FREEZE', '?OBJ']], [exists, ['?PART'], [and, [part, '?PART', '?OBJ'], ['holdsDuring', ['ImmediatePastFn', ['WhenFn', '?FREEZE']], [attribute, '?PART', 'Liquid']], ['holdsDuring', ['ImmediateFutureFn', ['WhenFn', '?FREEZE']], [attribute, '?PART', 'Solid']]]]):-true.
subclass('AstronomicalBody', 'Region'):-true.
disjoint('AstronomicalBody', 'GeographicArea'):-true.
documentation('AstronomicalBody', "The &%Class of all astronomical 
objects of significant size.  It includes &%SelfConnectedObjects 
like planets, stars, and asteroids, as well as &%Collections like 
nebulae, galaxies, and constellations.  Note that the planet Earth 
is an &%AstronomicalBody, but every &%Region of Earth is a 
&%GeographicArea."):-true.
subclass('GeographicArea', 'Region'):-true.
partition('GeographicArea', 'WaterArea', 'LandArea'):-true.
documentation('GeographicArea', "A geographic location, generally having 
definite boundaries.  Note that this differs from its immediate superclass 
&%Region in that a &%GeographicArea is a three-dimensional &%Region of the 
earth.  Accordingly, all astronomical objects other than earth and all 
one-dimensional and two-dimensional &%Regions are not classed under 
&%GeographicArea."):-true.
subclass('GeopoliticalArea', 'GeographicArea'):-true.
subclass('GeopoliticalArea', 'Agent'):-true.
documentation('GeopoliticalArea', "Any &%GeographicArea which is associated 
with some sort of political structure.  This class includes &%Lands, 
&%Cities, districts of cities, counties, etc.  Note that the identity 
of a &%GeopoliticalArea may remain constant after a change in borders."):-true.
subclass('WaterArea', 'GeographicArea'):-true.
documentation('WaterArea', "A body which is made up predominantly of water, 
e.g. rivers, lakes, oceans, etc."):-true.
=>([instance, '?AREA', 'WaterArea'], [exists, ['?BED', '?HOLE', '?WATER'], [and, [equal, ['PrincipalHostFn', '?HOLE'], '?BED'], [instance, '?WATER', 'Water'], ['properlyFills', '?WATER', '?HOLE'], [equal, ['MereologicalSumFn', '?BED', '?WATER'], '?AREA']]]):-true.
subclass('SaltWaterArea', 'WaterArea'):-true.
disjoint('SaltWaterArea', 'FreshWaterArea'):-true.
documentation('SaltWaterArea', "A &%WaterArea whose &%Water is saline, e.g. 
oceans and seas."):-true.
subclass('FreshWaterArea', 'WaterArea'):-true.
documentation('FreshWaterArea', "A &%WaterArea whose &%Water is not saline, 
e.g. most rivers and lakes."):-true.
subclass('StreamWaterArea', 'WaterArea'):-true.
disjoint('StreamWaterArea', 'StaticWaterArea'):-true.
documentation('StreamWaterArea', "A relatively narrow &%WaterArea where the 
water flows constantly and in the same direction, e.g. a river, a stream, 
etc."):-true.
subclass('StaticWaterArea', 'WaterArea'):-true.
documentation('StaticWaterArea', "A &%WaterArea in which water does not flow 
constantly or in the same direction, e.g. most lakes and ponds."):-true.
subclass('LandArea', 'GeographicArea'):-true.
documentation('LandArea', "An area which is predominantly solid ground, 
e.g. a &%Nation, a mountain, a desert, etc.  Note that a &%LandArea may 
contain some relatively small &%WaterAreas.  For example, Australia is 
a &%LandArea even though it contains various rivers and lakes."):-true.
=>([instance, '?LAND1', 'LandArea'], [exists, ['?LAND2'], [and, [part, '?LAND1', '?LAND2'], [or, [instance, '?LAND2', 'Continent'], [instance, '?LAND2', 'Island']]]]):-true.
subclass('Continent', 'LandArea'):-true.
documentation('Continent', "One of the seven largest &%LandAreas on earth."):-true.
equal(['CardinalityFn', 'Continent'], 7):-true.
subclass('Island', 'LandArea'):-true.
documentation('Island', "A &%LandArea that is completely surrounded by a &%WaterArea."):-true.
=>([instance, '?ISLAND', 'Island'], [not, [exists, ['?AREA', '?PART1', '?PART2'], [and, [instance, '?AREA', 'LandArea'], [part, '?PART1', '?ISLAND'], [part, '?PART2', '?AREA'], [not, [part, '?ISLAND', '?AREA']], [not, [part, '?AREA', '?ISLAND']], [connected, '?PART1', '?PART2']]]]):-true.
subclass('Nation', 'GeopoliticalArea'):-true.
subclass('Nation', 'LandArea'):-true.
documentation('Nation', "The broadest &%GeopoliticalArea, i.e. &%Nations are 
&%GeopoliticalAreas that are not part of any other overarching and 
comprehensive governance structure (excepting commonwealths and other sorts 
of loose international organizations."):-true.
subclass('StateOrProvince', 'GeopoliticalArea'):-true.
subclass('StateOrProvince', 'LandArea'):-true.
documentation('StateOrProvince', "Administrative subdivisions of a 
&%Nation that are broader than any other political subdivisions that 
may exist.  This &%Class includes the states of the United States, as 
well as the provinces of Canada and European countries."):-true.
=>([instance, '?STATE', 'StateOrProvince'], [exists, ['?LAND'], [and, [instance, '?LAND', 'Nation'], ['properPart', '?STATE', '?LAND']]]):-true.
subclass('Address', 'LandArea'):-true.
documentation('Address', "A &%LandArea of relatively small size.  This 
concept represents the state of 'being at an address'."):-true.
subclass('City', 'GeopoliticalArea'):-true.
subclass('City', 'LandArea'):-true.
documentation('City', "A &%LandArea of relatively small size, inhabited 
by a community of people, and having some sort of political structure.  
Note that this class includes both large cities and small settlements 
like towns, villages, hamlets, etc."):-true.
subclass('Water', 'CompoundSubstance'):-true.
documentation('Water', "The &%Class of samples of the compound H20.  Note 
that this &%Class covers both pure and impure &%Water."):-true.
subclass('Mineral', 'Mixture'):-true.
documentation('Mineral', " Any of various naturally occurring homogeneous substances (such as stone, coal, salt, sulfur, sand, petroleum), or synthetic substances having the chemical composition and crystalline form and properties of a naturally occurring mineral."):-true.
instance('developmentalForm', 'BinaryPredicate'):-true.
instance('developmentalForm', 'AsymmetricRelation'):-true.
instance('developmentalForm', 'TransitiveRelation'):-true.
subrelation('developmentalForm', attribute):-true.
domain('developmentalForm', 1, 'OrganicObject'):-true.
domain('developmentalForm', 2, 'DevelopmentalAttribute'):-true.
documentation('developmentalForm', "(&%developmentalForm ?OBJECT ?FORM) 
means that ?FORM is an earlier stage in the individual maturation of 
?OBJECT.  For example, tadpole and caterpillar are &%developmentalForms 
of frogs and butterflies, respectively."):-true.
=>([and, ['holdsDuring', '?TIME1', ['developmentalForm', '?OBJ', '?ATTR1']], ['successorAttributeClosure', '?ATTR2', '?ATTR1']], [exists, ['?TIME2'], [and, [earlier, '?TIME2', '?TIME1'], ['holdsDuring', '?TIME2', ['developmentalForm', '?OBJ', '?ATTR2']]]]):-true.
subclass('OrganicObject', 'CorpuscularObject'):-true.
partition('OrganicObject', 'Organism', 'AnatomicalStructure'):-true.
documentation('OrganicObject', "This class encompasses &%Organisms, 
&%CorpuscularObjects that are parts of &%Organisms, i.e. &%BodyParts, 
and &%CorpuscularObjects that are nonintentionally produced by 
&%Organisms, e.g. &%ReproductiveBodies."):-true.
subclass('Organism', 'OrganicObject'):-true.
subclass('Organism', 'Agent'):-true.
partition('Organism', 'Animal', 'Plant', 'Microorganism'):-true.
documentation('Organism', "Generally, a living individual, including all 
&%Plants and &%Animals."):-true.
=>([instance, '?ORGANISM', 'Organism'], [exists, ['?BIRTH'], [and, [instance, '?BIRTH', 'Birth'], [experiencer, '?BIRTH', '?ORGANISM']]]):-true.
instance(inhabits, 'BinaryPredicate'):-true.
instance(inhabits, 'AsymmetricRelation'):-true.
domain(inhabits, 1, 'Organism'):-true.
domain(inhabits, 2, 'Object'):-true.
documentation(inhabits, "A very basic notion of living within something 
else.  (&%inhabits ?ORGANISM ?OBJECT) means that ?OBJECT is the residence, 
nest, home, etc. of ?ORGANISM."):-true.
=>([inhabits, '?ORGANISM', '?OBJ'], [exists, ['?TIME'], ['holdsDuring', '?TIME', [located, '?ORGANISM', '?OBJ']]]):-true.
instance(parent, 'BinaryPredicate'):-true.
subrelation(parent, 'familyRelation'):-true.
instance(parent, 'AsymmetricRelation'):-true.
instance(parent, 'IntransitiveRelation'):-true.
domain(parent, 1, 'Organism'):-true.
domain(parent, 2, 'Organism'):-true.
documentation(parent, "The general relationship of parenthood.  
(&%parent ?CHILD ?PARENT) means that ?PARENT is a biological parent 
of ?CHILD."):-true.
=>([parent, '?CHILD', '?PARENT'], [before, ['BeginFn', ['WhenFn', '?PARENT']], ['BeginFn', ['WhenFn', '?CHILD']]]):-true.
=>([and, [parent, '?CHILD', '?PARENT'], [subclass, '?CLASS', 'Organism'], [instance, '?PARENT', '?CLASS']], [instance, '?CHILD', '?CLASS']):-true.
=>([parent, '?CHILD', '?PARENT'], [or, [mother, '?CHILD', '?PARENT'], [father, '?CHILD', '?PARENT']]):-true.
=>([instance, '?ORGANISM', 'Organism'], [exists, ['?PARENT'], [parent, '?ORGANISM', '?PARENT']]):-true.
instance(mother, 'SingleValuedRelation'):-true.
subrelation(mother, parent):-true.
domain(mother, 1, 'Organism'):-true.
domain(mother, 2, 'Organism'):-true.
documentation(mother, "The general relationship of motherhood.  
(&%mother ?CHILD ?MOTHER) means that ?MOTHER is the biological mother 
of ?CHILD."):-true.
=>([mother, '?CHILD', '?MOTHER'], [attribute, '?MOTHER', 'Female']):-true.
instance(father, 'SingleValuedRelation'):-true.
subrelation(father, parent):-true.
domain(father, 1, 'Organism'):-true.
domain(father, 2, 'Organism'):-true.
documentation(father, "The general relationship of fatherhood.  
(&%father ?CHILD ?FATHER) means that ?FATHER is the biological father
of ?CHILD."):-true.
=>([father, '?CHILD', '?FATHER'], [attribute, '?FATHER', 'Male']):-true.
instance(sibling, 'BinaryPredicate'):-true.
subrelation(sibling, 'familyRelation'):-true.
instance(sibling, 'SymmetricRelation'):-true.
instance(sibling, 'IrreflexiveRelation'):-true.
domain(sibling, 1, 'Organism'):-true.
domain(sibling, 2, 'Organism'):-true.
documentation(sibling, "The relationship between two &%Organisms that 
have the same &%mother and &%father.  Note that this relationship does 
not hold between half-brothers, half-sisters, etc."):-true.
<=>([sibling, '?ANIMAL1', '?ANIMAL2'], [exists, ['?FATHER', '?MOTHER'], [and, [father, '?ANIMAL1', '?FATHER'], [father, '?ANIMAL2', '?FATHER'], [mother, '?ANIMAL1', '?MOTHER'], [mother, '?ANIMAL2', '?MOTHER']]]):-true.
subclass('Plant', 'Organism'):-true.
documentation('Plant', "An &%Organism having cellulose cell walls, growing 
by synthesis of &%Substances, generally distinguished by the presence of 
chlorophyll, and lacking the power of locomotion."):-true.
subclass('FloweringPlant', 'Plant'):-true.
documentation('FloweringPlant', "A &%Plant that produces seeds and flowers.  
This class includes trees, shrubs, herbs, and flowers."):-true.
subclass('NonFloweringPlant', 'Plant'):-true.
disjoint('NonFloweringPlant', 'FloweringPlant'):-true.
'disjointDecomposition'('NonFloweringPlant', 'Alga', 'Fern', 'Fungus', 'Moss'):-true.
documentation('NonFloweringPlant', "A &%Plant that reproduces with spores and 
does not produce flowers."):-true.
subclass('Alga', 'NonFloweringPlant'):-true.
documentation('Alga', "A chiefly aquatic plant that contains chlorophyll, 
but does not form embryos during development and lacks vascular tissue."):-true.
=>([instance, '?ALGA', 'Alga'], [exists, ['?WATER'], [and, [inhabits, '?ALGA', '?WATER'], [instance, '?WATER', 'Water']]]):-true.
subclass('Fungus', 'NonFloweringPlant'):-true.
documentation('Fungus', "A eukaryotic &%Organism characterized by the 
absence of chlorophyll and the presence of rigid cell walls. Included 
here are both slime molds and true fungi such as yeasts, molds, mildews, 
and mushrooms."):-true.
=>([and, [instance, '?FUNGUS', 'Fungus'], [inhabits, '?FUNGUS', '?OBJ']], [instance, '?OBJ', 'Organism']):-true.
subclass('Moss', 'NonFloweringPlant'):-true.
documentation('Moss', "A &%NonFloweringPlant without true roots and little 
if any vascular tissue."):-true.
subclass('Fern', 'NonFloweringPlant'):-true.
documentation('Fern', "A &%NonFloweringPlant that contains vascular tissue.  
This class includes true ferns, as well as horsetails, club mosses, and 
whisk ferns."):-true.
subclass('Animal', 'Organism'):-true.
documentation('Animal', "An &%Organism with eukaryotic &%Cells, and lacking 
stiff cell walls, plastids, and photosynthetic pigments."):-true.
subclass('Microorganism', 'Organism'):-true.
documentation('Microorganism', "An &%Organism that can be seen only with 
the aid of a microscope."):-true.
subclass('Bacterium', 'Microorganism'):-true.
documentation('Bacterium', "A small, typically one-celled, prokaryotic 
&%Microorganism."):-true.
=>([instance, '?BACTERIUM', 'Bacterium'], [exists, ['?CELL1'], [and, [component, '?CELL1', '?BACTERIUM'], [instance, '?CELL1', 'Cell'], [forall, ['?CELL2'], [=>, [and, [component, '?CELL2', '?BACTERIUM'], [instance, '?CELL2', 'Cell']], [equal, '?CELL1', '?CELL2']]]]]):-true.
=>([and, [instance, '?BACTERIUM', 'Bacterium'], [inhabits, '?BACTERIUM', '?OBJ']], [instance, '?OBJ', 'Organism']):-true.
subclass('Virus', 'Microorganism'):-true.
documentation('Virus', "An &%Organism consisting of a core of a single 
nucleic acid enclosed in a protective coat of protein. A virus may replicate 
only inside a host living cell. A virus exhibits some but not all of the 
usual characteristics of living things."):-true.
=>([instance, '?VIRUS', 'Virus'], [exists, ['?MOL1'], [and, [component, '?MOL1', '?VIRUS'], [instance, '?MOL1', 'Molecule'], [forall, ['?MOL2'], [=>, [and, [component, '?MOL2', '?VIRUS'], [instance, '?MOL2', 'Molecule']], [equal, '?MOL1', '?MOL2']]]]]):-true.
=>([and, [instance, '?VIRUS', 'Virus'], [inhabits, '?VIRUS', '?OBJ']], [instance, '?OBJ', 'Organism']):-true.
=>([and, [instance, '?VIRUS', 'Virus'], [instance, '?PROC', 'Replication'], [agent, '?PROC', '?VIRUS']], [exists, ['?CELL'], [and, [located, '?PROC', '?CELL'], [instance, '?CELL', 'Cell']]]):-true.
subclass('Vertebrate', 'Animal'):-true.
documentation('Vertebrate', "An &%Animal which has a spinal column."):-true.
subclass('Invertebrate', 'Animal'):-true.
disjoint('Invertebrate', 'Vertebrate'):-true.
'disjointDecomposition'('Invertebrate', 'Worm', 'Mollusk', 'Arthropod'):-true.
documentation('Invertebrate', "An &%Animal which has no spinal column."):-true.
subclass('Worm', 'Invertebrate'):-true.
documentation('Worm', "Long, narrow, soft-bodied &%Invertebrates."):-true.
subclass('Mollusk', 'Invertebrate'):-true.
documentation('Mollusk', "Soft-bodied &%Invertebrate that is usually 
contained in a shell.  Includes oysters, clams, mussels, snails, slugs, 
octopi, and squid."):-true.
subclass('Arthropod', 'Invertebrate'):-true.
'disjointDecomposition'('Arthropod', 'Arachnid', 'Myriapod', 'Insect', 'Crustacean'):-true.
documentation('Arthropod', "A &%Class of &%Invertebrate that includes 
&%Arachnids and &%Insects."):-true.
subclass('Arachnid', 'Arthropod'):-true.
documentation('Arachnid', "A &%Class of &%Arthropods that includes 
ticks and spiders."):-true.
subclass('Myriapod', 'Arthropod'):-true.
documentation('Myriapod', "A &%Class of &%Arthropods that includes 
centipedes and millipedes."):-true.
subclass('Insect', 'Arthropod'):-true.
documentation('Insect', "A &%Class of small &%Arthropods that are 
air-breathing and that are distinguished by appearance."):-true.
subclass('Crustacean', 'Arthropod'):-true.
documentation('Crustacean', "A &%Class of &%Arthropods that mainly dwells 
in water and has a segmented body and a chitinous exoskeleton.  Includes 
lobsters, crabs, shrimp, and barnacles."):-true.
subclass('ColdBloodedVertebrate', 'Vertebrate'):-true.
'disjointDecomposition'('ColdBloodedVertebrate', 'Amphibian', 'Fish', 'Reptile'):-true.
documentation('ColdBloodedVertebrate', "&%Vertebrates whose body temperature 
is not internally regulated."):-true.
subclass('WarmBloodedVertebrate', 'Vertebrate'):-true.
disjoint('WarmBloodedVertebrate', 'ColdBloodedVertebrate'):-true.
documentation('WarmBloodedVertebrate', "&%Vertebrates whose body temperature 
is internally regulated."):-true.
subclass('Amphibian', 'ColdBloodedVertebrate'):-true.
documentation('Amphibian', "A cold-blooded, smooth-skinned &%Vertebrate 
which characteristically hatches as an aquatic larva, breathing by 
gills.  When mature, the &%Amphibian breathes with &%Lungs."):-true.
subclass('Bird', 'WarmBloodedVertebrate'):-true.
disjoint('Bird', 'Mammal'):-true.
documentation('Bird', "A &%Vertebrate having a constant body temperature 
and characterized by the presence of feathers."):-true.
subclass('Fish', 'ColdBloodedVertebrate'):-true.
documentation('Fish', "A cold-blooded aquatic &%Vertebrate characterized by 
fins and breathing by gills. Included here are &%Fish having either a bony 
skeleton, such as a perch, or a cartilaginous skeleton, such as a shark.  
Also included are those &%Fish lacking a jaw, such as a lamprey or 
hagfish."):-true.
=>([instance, '?FISH', 'Fish'], [exists, ['?WATER'], [and, [inhabits, '?FISH', '?WATER'], [instance, '?WATER', 'Water']]]):-true.
subclass('Mammal', 'WarmBloodedVertebrate'):-true.
'disjointDecomposition'('Mammal', 'AquaticMammal', 'HoofedMammal', 'Marsupial', 'Rodent', 'Primate'):-true.
documentation('Mammal', "A &%Vertebrate having a constant body temperature 
and characterized by the presence of hair, mammary glands, and sweat 
glands."):-true.
subclass('AquaticMammal', 'Mammal'):-true.
documentation('AquaticMammal', "The &%Class of &%Mammals that dwell chiefly 
in the water.  Includes whales, dolphins, manatees, seals, and walruses."):-true.
subclass('HoofedMammal', 'Mammal'):-true.
documentation('HoofedMammal', "The &%Class of &%Mammals with hooves.  
Includes horses, cows, sheep, pigs, antelope, etc."):-true.
subclass('Marsupial', 'Mammal'):-true.
documentation('Marsupial', "The &%Class of &%Mammals which have a pouch for 
their young."):-true.
subclass('Carnivore', 'Mammal'):-true.
documentation('Carnivore', "The &%Class of flesh-eating &%Mammals.  Members 
of this &%Class typically have four or five claws on each paw.  Includes 
cats, dogs, bears, racoons, and skunks."):-true.
subclass('Canine', 'Carnivore'):-true.
disjoint('Canine', 'Feline'):-true.
documentation('Canine', "The &%Class of &%Carnivores with completely 
separable toes, nonretractable claws, and long muzzles."):-true.
subclass('Feline', 'Carnivore'):-true.
documentation('Feline', "The &%Class of &%Carnivores with completely
separable toes, nonretractable claws, slim bodies, and rounded heads."):-true.
subclass('Rodent', 'Mammal'):-true.
documentation('Rodent', "The &%Class of &%Mammals with one or two pairs 
of incisors for gnawing.  Includes rats, mice, guinea pigs, and 
rabbits."):-true.
subclass('Primate', 'Mammal'):-true.
'disjointDecomposition'('Primate', 'Ape', 'Monkey', 'Hominid'):-true.
documentation('Primate', "The &%Class of &%Mammals which are 
&%Primates."):-true.
subclass('Ape', 'Primate'):-true.
documentation('Ape', "Various &%Primates with no tails or only short 
tails."):-true.
subclass('Monkey', 'Primate'):-true.
documentation('Monkey', "Various &%Primates with relatively long 
tails."):-true.
subclass('Hominid', 'Primate'):-true.
documentation('Hominid', "Includes &%Humans and relatively recent 
ancestors of &%Humans."):-true.
subclass('Human', 'Hominid'):-true.
subclass('Human', 'CognitiveAgent'):-true.
documentation('Human', "Modern man, the only remaining species of the Homo 
genus."):-true.
subclass('Reptile', 'ColdBloodedVertebrate'):-true.
documentation('Reptile', "A &%ColdBloodedVertebrate having an external 
covering of scales or horny plates.  &%Reptiles breathe by means of 
&%Lungs and generally lay eggs."):-true.
subclass('BiologicallyActiveSubstance', 'Substance'):-true.
documentation('BiologicallyActiveSubstance', "A &%Substance that is 
capable of inducing a change in the structure or functioning of an 
&%Organism.  This &%Class includes &%Substances used in the treatment, 
diagnosis, prevention or analysis of normal and abnormal body function.  
This &%Class also includes &%Substances that occur naturally in the body 
and are administered therapeutically.  Finally, &%BiologicallyActiveSubstance 
includes &%Nutrients, most drugs of abuse, and agents that require special 
handling because of their toxicity."):-true.
subclass('Nutrient', 'BiologicallyActiveSubstance'):-true.
subclass('Nutrient', 'CompoundSubstance'):-true.
'disjointDecomposition'('Nutrient', 'Protein', 'Carbohydrate', 'Vitamin'):-true.
documentation('Nutrient', "A &%BiologicallyActiveSubstance produced or 
required by an &%Organism, of primary interest because of its role in the 
biologic functioning of the &%Organism."):-true.
subclass('Protein', 'Nutrient'):-true.
documentation('Protein', "A &%Nutrient made up of amino acids joined by 
peptide bonds."):-true.
subclass('Enzyme', 'Protein'):-true.
documentation('Enzyme', "A complex &%Protein that is produced by living 
cells and which catalyzes specific biochemical reactions. There are six 
main types of enzymes:  oxidoreductases, transferases, hydrolases, 
lyases, isomerases, and ligases."):-true.
subclass('Carbohydrate', 'Nutrient'):-true.
documentation('Carbohydrate', "An element of living cells and a source of 
energy for &%Animals.  This class includes both simple &%Carbohydrates, 
i.e. sugars, and complex &%Carbohydrates, i.e. starches."):-true.
subclass('Vitamin', 'Nutrient'):-true.
documentation('Vitamin', "A &%Nutrient present in natural products or made 
synthetically, which is essential in the diet of &%Humans and other higher 
&%Animals.  Included here are &%Vitamin precursors and provitamins."):-true.
subclass('BodySubstance', 'Mixture'):-true.
documentation('BodySubstance', "Extracellular material and mixtures of 
cells and extracellular material that are produced, excreted or accreted 
by an &%Organism.  Included here are &%Substances such as saliva, dental 
enamel, sweat, and gastric acid."):-true.
subclass('Hormone', 'BodySubstance'):-true.
subclass('Hormone', 'BiologicallyActiveSubstance'):-true.
documentation('Hormone', "In &%Animals, a chemical secreted by an 
endocrine gland whose products are released into the circulating fluid.  
&%Plant hormones or synthetic hormones which are used only to alter or 
control various physiologic processes, e.g., reproductive control agents, 
are assigned to the &%Class &%BiologicallyActiveSubstance. &%Hormones act as 
chemical messengers and regulate various physiologic processes such as 
growth, reproduction, metabolism, etc.  They usually fall into two broad 
categories, viz. steroid hormones and peptide hormones."):-true.
subclass('Blood', 'BodySubstance'):-true.
documentation('Blood', "A fluid present in &%Animals that transports 
&%Nutrients to and waste products away from various &%BodyParts."):-true.
subclass('ToxicOrganism', 'Organism'):-true.
documentation('ToxicOrganism', "The &%Class of &%Organisms which are 
poisonous to other &%Organisms."):-true.
=>([instance, '?ORGANISM', 'ToxicOrganism'], [exists, ['?SUBSTANCE'], [and, [instance, '?SUBSTANCE', 'BiologicallyActiveSubstance'], [part, '?SUBSTANCE', '?ORGANISM']]]):-true.
subclass('Food', 'SelfConnectedObject'):-true.
'disjointDecomposition'('Food', 'Meat', 'FruitOrVegetable', 'Beverage'):-true.
documentation('Food', "Any &%SelfConnectedObject containing &%Nutrients, 
such as carbohydrates, proteins, and fats, that can be ingested by a 
living &%Animal and metabolized into energy and body tissue."):-true.
=>([instance, '?FOOD', 'Food'], [exists, ['?NUTRIENT'], [and, [instance, '?NUTRIENT', 'Nutrient'], [part, '?NUTRIENT', '?FOOD']]]):-true.
=>([instance, '?FOOD', 'Food'], [forall, ['?PART1'], [=>, [part, '?PART1', '?FOOD'], [exists, ['?PART2', '?ANIMAL'], [and, [part, '?PART1', '?PART2'], [part, '?PART2', '?ANIMAL'], [instance, '?ANIMAL', 'Animal']]]]]):-true.
subclass('Meat', 'Food'):-true.
documentation('Meat', "Any &%Food which was originally part of an 
&%Animal and is not ingested by drinking, including eggs and animal 
blood that is eaten as food.  Note that this class covers both raw 
meat and meat that has been prepared in some way, e.g. by cooking.  
Note too that preparations involving &%Meat and &%FruitOrVegetable 
are classed directly under &%Food."):-true.
=>([instance, '?MEAT', 'Meat'], [forall, ['?PART'], [=>, [part, '?PART', '?MEAT'], [exists, ['?SUBPART', '?TIME', '?ANIMAL'], [and, [part, '?SUBPART', '?PART'], ['holdsDuring', '?TIME', [and, [instance, '?ANIMAL', 'Animal'], [part, '?SUBPART', '?ANIMAL']]]]]]]):-true.
subclass('FruitOrVegetable', 'Food'):-true.
subclass('FruitOrVegetable', 'BodyPart'):-true.
documentation('FruitOrVegetable', "Any fruit or vegetable, i.e. a 
&%Food that was originally part of a &%Plant and is not ingested 
by drinking, including nuts, grains, beans, etc.  Note that 
preparations involving more than one &%FruitOrVegetable or 
something that is not a &%FruitOrVegetable are not classed under 
&%FruitOrVegetable.  For example, a salad is not considered to be 
a &%FruitOrVegetable because most salads contain more than one kind 
of vegetable, and zucchini cooked in lard would not be a &%FruitOrVegetable 
because it contains an &%Animal product."):-true.
=>([instance, '?VEG', 'FruitOrVegetable'], [forall, ['?PART'], [=>, [part, '?PART', '?VEG'], [exists, ['?SUBPART', '?TIME', '?PLANT'], [and, [part, '?SUBPART', '?PART'], ['holdsDuring', '?TIME', [and, [instance, '?PLANT', 'Plant'], [part, '?SUBPART', '?PLANT']]]]]]]):-true.
subclass('Beverage', 'Food'):-true.
documentation('Beverage', "Any &%Food that is ingested by &%Drinking.  
Note that this class is disjoint with the other subclasses of &%Food, 
i.e. &%Meat and &%FruitOrVegetable."):-true.
=>([instance, '?BEV', 'Beverage'], [attribute, '?BEV', 'Liquid']):-true.
=>([and, [instance, '?DRINK', 'Drinking'], [patient, '?DRINK', '?BEV']], [instance, '?BEV', 'Beverage']):-true.
subclass('AnatomicalStructure', 'OrganicObject'):-true.
disjoint('AnatomicalStructure', 'Organism'):-true.
documentation('AnatomicalStructure', "A normal or pathological part 
of the anatomy or structural organization of an &%Organism.  This 
class covers &%BodyParts, as well as structures that are given off 
by &%Organisms, e.g. &%ReproductiveBodies."):-true.
=>([instance, '?ANAT', 'AnatomicalStructure'], [exists, ['?ORGANISM'], [and, [instance, '?ORGANISM', 'Organism'], [part, '?ANAT', '?ORGANISM']]]):-true.
=>([instance, '?PART', 'AnatomicalStructure'], [exists, ['?CELL'], [and, [instance, '?CELL', 'Cell'], [part, '?CELL', '?PART']]]):-true.
subclass('ReproductiveBody', 'AnatomicalStructure'):-true.
documentation('ReproductiveBody', "Reproductive structure of &%Organisms.  
Consists of an &%Embryonic &%Object and a nutritive/protective envelope.  
Note that this class includes seeds, spores, and &%FruitOrVegetables, as 
well as the eggs produced by &%Animals."):-true.
subclass('BodyPart', 'AnatomicalStructure'):-true.
documentation('BodyPart', "A collection of &%Cells and &%Tissues which 
are localized to a specific area of an &%Organism and which are not 
pathological. The instances of this &%Class range from gross structures 
to small components of complex &%Organs."):-true.
=>([instance, '?PART', 'BodyPart'], [not, [exists, ['?PROC'], [and, [instance, '?PROC', 'PathologicProcess'], [result, '?PROC', '?PART']]]]):-true.
subclass('BodyCovering', 'BodyPart'):-true.
documentation('BodyCovering', "Any &%BodyPart which is a covering of another 
&%BodyPart or of an entire &%Organism.  This would include the rinds of 
&%FruitOrVegetables and the skins of &%Animals."):-true.
=>([instance, '?COVER', 'BodyCovering'], [exists, ['?BODY'], [and, ['superficialPart', '?COVER', '?BODY'], [or, [instance, '?BODY', 'Organism'], [instance, '?BODY', 'BodyPart']]]]):-true.
subclass('BodyJunction', 'BodyPart'):-true.
documentation('BodyJunction', "The place where two &%BodyParts 
meet or connect."):-true.
=>([instance, '?JUNCT', 'BodyJunction'], [exists, ['?STRUCT'], [and, [instance, '?STRUCT', 'BodyPart'], [component, '?JUNCT', '?STRUCT']]]):-true.
=>([instance, '?JUNCT', 'BodyJunction'], [exists, ['?STRUCT1', '?STRUCT2'], [and, [connected, '?JUNCT', '?STRUCT1'], [connected, '?JUNCT', '?STRUCT2'], [instance, '?STRUCT1', 'BodyPart'], [instance, '?STRUCT2', 'BodyPart'], [not, [equal, '?STRUCT1', '?STRUCT2']]]]):-true.
subclass('BodyVessel', 'BodyPart'):-true.
documentation('BodyVessel', "Any tube-like structure which occurs naturally in 
an &%Organism and through which a &%BodySubstance can circulate."):-true.
subclass('Cell', 'BodyPart'):-true.
documentation('Cell', "The fundamental structural and functional unit of 
living &%Organisms."):-true.
subclass('Organ', 'BodyPart'):-true.
documentation('Organ', "A somewhat independent &%BodyPart that performs a 
specialized function."):-true.
=>([instance, '?ORGAN', 'Organ'], [exists, ['?PURP'], ['hasPurpose', '?ORGAN', '?PURP']]):-true.
subclass('Gland', 'Organ'):-true.
documentation('Gland', "An &%Organ that removes &%Substances from the &%Blood, 
alters them in some way, and then releases them."):-true.
subclass('Tissue', 'BodySubstance'):-true.
'disjointDecomposition'('Tissue', 'Bone', 'Muscle', 'FatTissue'):-true.
documentation('Tissue', "An aggregation of similarly specialized &%Cells 
and the associated intercellular substance. &%Tissues are relatively 
non-localized in comparison to &%BodyParts, &%Organs or &%Organ components.  
The main features of &%Tissues are self-connectivity (see 
&%SelfConnectedObject) and being a homogeneous mass (all parts in the 
same granularity are instances of &%Tissue as well)."):-true.
=>([instance, '?STUFF', 'Tissue'], [exists, ['?PART'], [and, [instance, '?PART', 'Cell'], [part, '?PART', '?STUFF']]]):-true.
=>([instance, '?STUFF', 'Tissue'], [exists, ['?ORGANISM'], [and, [instance, '?ORGANISM', 'Organism'], [part, '?STUFF', '?ORGANISM']]]):-true.
subclass('Bone', 'Tissue'):-true.
documentation('Bone', "Rigid &%Tissue composed largely of calcium that makes up 
the skeleton of &%Vertebrates.  Note that this &%Class also includes teeth."):-true.
=>([instance, '?BONE', 'Bone'], [exists, ['?VERT'], [and, [instance, '?VERT', 'Vertebrate'], [part, '?BONE', '?VERT']]]):-true.
subclass('Muscle', 'Tissue'):-true.
documentation('Muscle', "Nonrigid &%Tissue appearing only in &%Animals and 
composed largely of contractile cells."):-true.
subclass('FatTissue', 'Tissue'):-true.
documentation('FatTissue', "Nonrigid &%Tissue that is composed largely of 
fat cells."):-true.
subclass('Noun', 'Word'):-true.
partition('Word', 'Noun', 'Verb', 'Adjective', 'Adverb', 'Particle'):-true.
documentation('Noun', "One of the parts of speech.  The &%Class of &%Words 
that conventionally denote &%Objects."):-true.
subclass('Verb', 'Word'):-true.
documentation('Verb', "One of the parts of speech.  The &%Class of &%Words 
that conventionally denote &%Processes."):-true.
subclass('Adjective', 'Word'):-true.
documentation('Adjective', "One of the parts of speech.  The &%Class of 
&%Words that conventionally denote &%Attributes of &%Objects."):-true.
subclass('Adverb', 'Word'):-true.
documentation('Adverb', "One of the parts of speech.  The &%Class of &%Words 
that conventionally denote &%Attributes of &%Processes."):-true.
subclass('Particle', 'Word'):-true.
documentation('Particle', "An umbrella &%Class for any &%Word that does not 
fit into the other subclasses of &%Word.  A &%Particle is generally a small 
term that serves a grammatical or logical function, e.g. 'and', 'of', 
'since', etc.  At some point, this class might be broken up into the 
subclasses 'Connective', 'Preposition', etc."):-true.
subclass('Morpheme', 'LinguisticExpression'):-true.
documentation('Morpheme', "Part of a &%Word which cannot be subdivided 
and which expresses a meaning."):-true.
=>([instance, '?MORPH', 'Morpheme'], [not, [exists, ['?OTHERMORPH'], [and, [instance, '?OTHERMORPH', 'Morpheme'], [part, '?OTHERMORPH', '?MORPH'], [not, [equal, '?OTHERMORPH', '?MORPH']]]]]):-true.
=>([instance, '?MORPH', 'Morpheme'], [exists, ['?WORD'], [and, [instance, '?WORD', 'Word'], [part, '?MORPH', '?WORD']]]):-true.
=>([instance, '?WORD', 'Word'], [exists, ['?PART'], [and, [part, '?PART', '?WORD'], [instance, '?PART', 'Morpheme']]]):-true.
subclass('Phrase', 'LinguisticExpression'):-true.
'disjointDecomposition'('Phrase', 'VerbPhrase', 'NounPhrase', 'PrepositionalPhrase'):-true.
documentation('Phrase', "A set of &%Words in a &%Language which form a unit, 
i.e. express a meaning in the &%Language."):-true.
=>([instance, '?PHRASE', 'Phrase'], [exists, ['?PART1', '?PART2'], [and, [part, '?PART1', '?PHRASE'], [part, '?PART2', '?PHRASE'], [instance, '?PART1', 'Word'], [instance, '?PART2', 'Word'], [not, [equal, '?PART1', '?PART2']]]]):-true.
subclass('VerbPhrase', 'Phrase'):-true.
documentation('VerbPhrase', "A &%Phrase that has the same function as a 
&%Verb."):-true.
subclass('NounPhrase', 'Phrase'):-true.
disjoint('NounPhrase', 'VerbPhrase'):-true.
documentation('NounPhrase', "A &%Phrase that has the same function as a 
&%Noun."):-true.
=>([instance, '?SENTENCE', 'Sentence'], [exists, ['?PHRASE1', '?PHRASE2'], [and, [instance, '?PHRASE1', 'NounPhrase'], [instance, '?PHRASE2', 'VerbPhrase'], [part, '?PHRASE1', '?SENTENCE'], [part, '?PHRASE2', '?SENTENCE']]]):-true.
subclass('PrepositionalPhrase', 'Phrase'):-true.
documentation('PrepositionalPhrase', "A &%Phrase that begins with a 
preposition and that functions as an &%Adjective or an &%Adverb."):-true.
subclass('Text', 'LinguisticExpression'):-true.
subclass('Text', 'Artifact'):-true.
documentation('Text', "A &%Sentence or set of &%Sentences that perform 
a specific function related to &%Communication, e.g. express a discourse 
about a particular topic."):-true.
=>([instance, '?TEXT', 'Text'], [exists, ['?PROP'], [and, [instance, '?PROP', 'Proposition'], ['containsInformation', '?TEXT', '?PROP']]]):-true.
=>([instance, '?TEXT', 'Text'], [exists, ['?PART'], [and, [part, '?PART', '?TEXT'], [instance, '?PART', 'Sentence']]]):-true.
=>([instance, '?TEXT', 'Text'], [exists, ['?WRITE'], [and, [instance, '?WRITE', 'Writing'], [result, '?WRITE', '?TEXT']]]):-true.
subclass('Sentence', 'Text'):-true.
documentation('Sentence', "A syntactically well-formed formula of a 
&%Language.  It includes, at minimum, a predicate and a subject (which 
may be explicit or implicit), and it expresses a &%Proposition."):-true.
=>([instance, '?SENT', 'Sentence'], [exists, ['?NOUN', '?VERB'], [and, [instance, '?NOUN', 'NounPhrase'], [instance, '?VERB', 'VerbPhrase'], [part, '?NOUN', '?SENT'], [part, '?VERB', '?SENT']]]):-true.
instance(authors, 'BinaryPredicate'):-true.
instance(authors, 'AsymmetricRelation'):-true.
domain(authors, 1, 'Agent'):-true.
'domainSubclass'(authors, 2, 'Text'):-true.
documentation(authors, "(&%authors ?AGENT ?TEXT) means that ?AGENT is 
creatively responsible for ?TEXT.  For example, Agatha Christie is 
author of Murder_on_the_Orient_Express."):-true.
=>([authors, '?AGENT', '?TEXT'], [exists, ['?PROCESS', '?INSTANCE'], [and, [agent, '?PROCESS', '?AGENT'], [instance, '?INSTANCE', '?TEXT'], [result, '?PROCESS', '?TEXT']]]):-true.
instance(editor, 'BinaryPredicate'):-true.
instance(editor, 'AsymmetricRelation'):-true.
domain(editor, 1, 'Agent'):-true.
'domainSubclass'(editor, 2, 'Text'):-true.
documentation(editor, "(&%editor ?AGENT ?TEXT) means that ?AGENT is 
an editor of ?TEXT."):-true.
instance(publishes, 'BinaryPredicate'):-true.
instance(publishes, 'AsymmetricRelation'):-true.
domain(publishes, 1, 'Organization'):-true.
'domainSubclass'(publishes, 2, 'Text'):-true.
documentation(publishes, "(&%publishes ?ORG ?TEXT) means that ?ORG 
publishes ?TEXT.  For example, Bantam Books publishes Agatha Christie's 
Murder_on_the_Orient_Express."):-true.
<=>([publishes, '?ORG', '?TEXT'], [exists, ['?PUB'], [and, [instance, '?PUB', 'Publication'], [agent, '?PUB', '?ORG'], [patient, '?PUB', '?TEXT']]]):-true.
instance('EditionFn', 'BinaryFunction'):-true.
'domainSubclass'('EditionFn', 1, 'ContentBearingObject'):-true.
domain('EditionFn', 2, 'PositiveInteger'):-true.
'rangeSubclass'('EditionFn', 'ContentBearingObject'):-true.
documentation('EditionFn', "A &%BinaryFunction that maps a type of text 
(e.g. Agatha Christie's Murder_on_the_Orient_Express) and a number 
to the edition of the text type corresponding to the number."):-true.
=>([and, [equal, ['EditionFn', '?TEXT', '?INT1'], '?EDITION1'], [equal, ['EditionFn', '?TEXT', '?INT2'], '?EDITION2'], ['greaterThan', '?INT2', '?INT1'], [instance, '?PUB1', 'Publication'], [instance, '?PUB2', 'Publication'], [patient, '?PUB1', '?EDITION1'], [patient, '?PUB2', '?EDITION2'], [date, '?PUB1', '?DATE1'], [date, '?PUB2', '?DATE2']], [before, ['EndFn', '?DATE1'], ['EndFn', '?DATE2']]):-true.
=>([equal, ['EditionFn', '?TEXT1', '?NUMBER'], '?TEXT2'], ['subsumesContentClass', '?TEXT1', '?TEXT2']):-true.
instance('SeriesVolumeFn', 'BinaryFunction'):-true.
'domainSubclass'('SeriesVolumeFn', 1, 'Series'):-true.
domain('SeriesVolumeFn', 2, 'PositiveInteger'):-true.
'rangeSubclass'('SeriesVolumeFn', 'Text'):-true.
documentation('SeriesVolumeFn', "A &%BinaryFunction that maps a type of &%Series 
(e.g. the Encyclopedia_Britannica or the Popular_Mechanics periodical) and a 
number to the volumes of the text type designated by the number."):-true.
=>([and, [subclass, '?TEXT', 'Periodical'], [equal, ['SeriesVolumeFn', '?TEXT', '?INT1'], '?VOLUME1'], [equal, ['SeriesVolumeFn', '?TEXT', '?INT2'], '?VOLUME2'], ['greaterThan', '?INT2', '?INT1'], [instance, '?PUB1', 'Publication'], [instance, '?PUB2', 'Publication'], [patient, '?PUB1', '?VOLUME1'], [patient, '?PUB2', '?VOLUME2'], [date, '?PUB1', '?DATE1'], [date, '?PUB2', '?DATE2']], [before, ['EndFn', '?DATE1'], ['EndFn', '?DATE2']]):-true.
=>([equal, ['SeriesVolumeFn', '?SERIES', '?NUMBER'], '?VOLUME'], ['subsumesContentClass', '?SERIES', '?VOLUME']):-true.
instance('PeriodicalIssueFn', 'BinaryFunction'):-true.
'domainSubclass'('PeriodicalIssueFn', 1, 'Periodical'):-true.
domain('PeriodicalIssueFn', 2, 'PositiveInteger'):-true.
'rangeSubclass'('PeriodicalIssueFn', 'Periodical'):-true.
documentation('PeriodicalIssueFn', "A &%BinaryFunction that maps a subclass of 
&%Periodical and a number to all of the issues of the &%Periodical corresponding 
to the number."):-true.
=>([equal, ['PeriodicalIssueFn', '?PERIODICAL', '?NUMBER'], '?ISSUE'], ['subsumesContentClass', '?PERIODICAL', '?ISSUE']):-true.
subclass('Book', 'Text'):-true.
documentation('Book', "A &%Text that has pages and is bound."):-true.
subclass('Summary', 'Text'):-true.
documentation('Summary', "A short &%Text that is a summary of another, 
longer &%Text."):-true.
=>([instance, '?TEXT', 'Summary'], [exists, ['?TEXT2'], [and, [instance, '?TEXT2', 'Text'], ['subsumesContentInstance', '?TEXT2', '?TEXT']]]):-true.
subclass('Series', 'ContentBearingObject'):-true.
documentation('Series', "A &%ContentBearingObject consisting of multiple 
self-contained units.  Some examples are an encyclopedia containing a couple 
dozen volumes, a television series made up of many dozens of episodes, a 
film serial, etc."):-true.
=>([instance, '?SERIES', 'Series'], [exists, ['?BOOK1', '?BOOK2'], [and, [instance, '?BOOK1', 'Book'], [instance, '?BOOK2', 'Book'], ['subsumesContentInstance', '?SERIES', '?BOOK1'], ['subsumesContentInstance', '?SERIES', '?BOOK2'], [not, [equal, '?BOOK1', '?BOOK2']]]]):-true.
subclass('Periodical', 'Series'):-true.
documentation('Periodical', "A &%Series whose elements are published separately 
and on a periodic basis."):-true.
subclass('Article', 'Text'):-true.
documentation('Article', "An article in a &%Book or &%Series."):-true.
=>([instance, '?ARTICLE', 'Article'], [exists, ['?BOOK'], [and, [instance, '?BOOK', 'Book'], ['subsumesContentInstance', '?BOOK', '?ARTICLE']]]):-true.
subclass('Certificate', 'Text'):-true.
documentation('Certificate', "A &%Text that confers a right or obligation 
on the holder of the &%Certificate.  Note that the right or obligation 
need not be a legal one, as in the case of an academic diploma that grants 
certain privileges in the professional world."):-true.
=>([and, [instance, '?DOC', 'Certificate'], [possesses, '?AGENT', '?DOC']], [exists, ['?PROC'], [or, ['confersRight', '?PROC', '?DOC', '?AGENT'], ['confersObligation', '?PROC', '?DOC', '?AGENT']]]):-true.
subclass('Patent', 'Certificate'):-true.
documentation('Patent', "A &%Certificate that expresses the content of an 
invention that has been accorded legal protection by a governemental 
entity."):-true.
subclass('Molecule', 'CompoundSubstance'):-true.
documentation('Molecule', "A molecule is the smallest unit of matter of a 
&%CompoundSubstance that retains all the physical and chemical properties 
of that substance, e.g., Ne, H2, H2O.  A molecule is two or more &%Atoms 
linked by a chemical bond."):-true.
=>([instance, '?MOLE', 'Molecule'], [exists, ['?ATOM1', '?ATOM2'], [and, [instance, '?ATOM1', 'Atom'], [instance, '?ATOM2', 'Atom'], [part, '?ATOM1', '?MOLE'], [part, '?ATOM2', '?MOLE'], [not, [equal, '?ATOM1', '?ATOM2']]]]):-true.
subclass('Artifact', 'CorpuscularObject'):-true.
documentation('Artifact', "A &%CorpuscularObject that is the product of an 
&%IntentionalProcess."):-true.
=>([instance, '?ARTIFACT', 'Artifact'], [exists, ['?MAKING'], [and, [instance, '?PROCESS', 'Making'], [result, '?MAKING', '?ARTIFACT']]]):-true.
subclass('Product', 'Artifact'):-true.
documentation('Product', "An &%Artifact that is produced by &%Manufacture and 
that is intended to be sold."):-true.
=>([instance, '?PRODUCT', 'Product'], [exists, ['?MANUFACTURE'], [and, [instance, '?MANUFACTURE', 'Manufacture'], [result, '?MANUFACTURE', '?PRODUCT']]]):-true.
instance(version, 'BinaryPredicate'):-true.
instance(version, 'AsymmetricRelation'):-true.
instance(version, 'TransitiveRelation'):-true.
'domainSubclass'(version, 1, 'Artifact'):-true.
'domainSubclass'(version, 2, 'Artifact'):-true.
documentation(version, "Some &%Artifacts have a life cycle with discrete 
stages or versions.  (&%version ARTIFACT1 ARTIFACT2) means that ARTIFACT1 
is a version of ARTIFACT2.  Note that this &%Predicate relates subclasses of 
&%Artifact and not instances."):-true.
=>([version, '?ARTIFACT1', '?ARTIFACT2'], [subclass, '?ARTIFACT1', '?ARTIFACT2']):-true.
subclass('StationaryArtifact', 'Artifact'):-true.
documentation('StationaryArtifact', "A &%StationaryArtifact is an &%Artifact 
that has a fixed spatial location.  Most instances of this &%Class are 
architectural works, e.g. the Eiffel Tower, the Great Pyramids, office towers, 
single-family houses, etc."):-true.
=>([instance, '?ARTIFACT', 'StationaryArtifact'], [exists, ['?PLACE'], [forall, ['?TIME'], [=>, [and, ['beforeOrEqual', '?TIME', ['EndFn', ['WhenFn', '?ARTIFACT']]], ['beforeOrEqual', ['BeginFn', ['WhenFn', '?ARTIFACT']], '?TIME']], [equal, ['WhereFn', '?ARTIFACT', '?TIME'], '?PLACE']]]]):-true.
subclass('Building', 'StationaryArtifact'):-true.
documentation('Building', "The Class of &%StationaryArtifacts which are 
intended to house &%Humans and their activities."):-true.
=>([instance, '?BUILDING', 'Building'], [exists, ['?HUMAN'], [and, [instance, '?HUMAN', 'Human'], [or, [inhabits, '?HUMAN', '?BUILDING'], [exists, ['?ACT'], [and, [agent, '?ACT', '?HUMAN'], [located, '?ACT', '?BUILDING']]]]]]):-true.
subclass('Room', 'StationaryArtifact'):-true.
disjoint('Room', 'Building'):-true.
documentation('Room', "A &%properPart of a &%Building which is separated from 
the exterior of the &%Building and/or other &%Rooms of the &%Building by walls. 
Some &%Rooms may have a specific purpose, e.g. sleeping, bathing, cooking, 
entertainment, etc."):-true.
=>([instance, '?ROOM', 'Room'], [exists, ['?BUILD'], [and, [instance, '?BUILD', 'Building'], ['properPart', '?ROOM', '?BUILD']]]):-true.
subclass('ArtWork', 'Artifact'):-true.
documentation('ArtWork', "&%Artifacts that are created primarily for 
aesthetic appreciation.  Note that this &%Class does not include 
most examples of architecture, which belong under &%StationaryArtifact."):-true.
subclass('Fabric', 'Artifact'):-true.
disjoint('Fabric', 'StationaryArtifact'):-true.
documentation('Fabric', "&%Artifacts that are created by weaving together 
natural or synthetic fibers or by treating the skins of certain sorts of 
&%Animals.  Note that this &%Class includes articles that are created by 
stitching together various types of fabrics, e.g. bedspreads.  On the other 
hand, &%Clothing is not a &%subclass of &%Fabric, because many clothing items 
contain elements that are not fabrics."):-true.
subclass('Clothing', 'Artifact'):-true.
disjoint('Clothing', 'StationaryArtifact'):-true.
documentation('Clothing', "&%Artifact made out of fabrics and possibly other 
materials that are used to cover the bodies of &%Humans."):-true.
=>([instance, '?CLOTHING', 'Clothing'], [exists, ['?FABRIC'], [and, [instance, '?FABRIC', 'Fabric'], [part, '?FABRIC', '?CLOTHING']]]):-true.
subclass('Device', 'Artifact'):-true.
documentation('Device', "A &%Device is an &%Artifact whose purpose is to 
serve as an &%instrument in a specific type of task."):-true.
=>([instance, '?DEVICE', 'Device'], [exists, ['?PROC'], [and, [subclass, '?PROC', 'Process'], [capability, '?PROC', instrument, '?DEVICE']]]):-true.
=>([instance, '?DEVICE', 'Device'], [exists, ['?PROC'], [and, [subclass, '?PROC', 'Process'], ['hasPurpose', '?DEVICE', [capability, '?PROC', instrument, '?DEVICE']]]]):-true.
subclass('MusicalInstrument', 'Device'):-true.
documentation('MusicalInstrument', "A &%Device which is manipulated by a &%Human 
and whose purpose is to produce &%Music."):-true.
=>([instance, '?INSTRUMENT', 'MusicalInstrument'], [capability, 'Music', instrument, '?DEVICE']):-true.
subclass('TransportationDevice', 'Device'):-true.
documentation('TransportationDevice', "A &%TransportationDevice is a &%Device 
which serves as the &%instrument in a &%Transportation &%Process which carries 
the &%patient of the &%Process from one point to another."):-true.
=>([instance, '?DEVICE', 'TransportationDevice'], [capability, 'Transportation', instrument, '?DEVICE']):-true.
subclass('Weapon', 'Device'):-true.
documentation('Weapon', "The &%Class of &%Devices that are designed 
primarily to damage or destroy &%Humans/&%Animals, &%StationaryArtifacts or 
the places inhabited by &%Humans/&%Animals."):-true.
=>([instance, '?WEAPON', 'Weapon'], [capability, 'Damaging', instrument, '?WEAPON']):-true.
=>([instance, '?WEAPON', 'Weapon'], ['hasPurpose', '?WEAPON', [exists, ['?DEST', '?PATIENT'], [and, [instance, '?DEST', 'Damaging'], [patient, '?DEST', '?PATIENT'], [or, [instance, '?PATIENT', 'StationaryArtifact'], [instance, '?PATIENT', 'Animal'], [exists, ['?ANIMAL'], [and, [instance, '?ANIMAL', 'Animal'], [inhabits, '?ANIMAL', '?PATIENT']]]]]]]):-true.
subclass('Machine', 'Device'):-true.
documentation('Machine', "&%Machines are &%Devices that that have a 
well-defined &%resource and &%result and that automatically convert 
the &%resource into the &%result."):-true.
=>([instance, '?MACHINE', 'Machine'], [forall, ['?PROC'], [=>, [instrument, '?PROC', '?MACHINE'], [exists, ['?RESOURCE', '?RESULT'], [and, [resource, '?PROC', '?RESOURCE'], [result, '?PROC', '?RESULT']]]]]):-true.
subclass('EngineeringComponent', 'Device'):-true.
documentation('EngineeringComponent', "A fundamental concept that applies 
in many engineering domains.  An &%EngineeringComponent is an element of 
a &%Device that is a physically whole object, such as one might 
see listed as standard parts in a catalog.  The main difference betweeen 
&%EngineeringComponents and arbitrary globs of matter is that 
&%EngineeringComponents are object-like in a modeling sense.  Thus, an 
&%EngineeringComponent is not an arbtrary subregion, but a part of a 
system with a stable identity."):-true.
=>([instance, '?COMP', 'EngineeringComponent'], [exists, ['?DEVICE'], [and, [instance, '?DEVICE', 'Device'], [component, '?COMP', '?DEVICE']]]):-true.
subrelation('engineeringSubcomponent', 'properPart'):-true.
domain('engineeringSubcomponent', 1, 'EngineeringComponent'):-true.
domain('engineeringSubcomponent', 2, 'EngineeringComponent'):-true.
documentation('engineeringSubcomponent', "(&%engineeringSubcomponent ?SUB 
?SUPER) means that the &%EngineeringComponent ?SUB is structurally a 
&%properPart ?SUPER.  This relation is an &%AsymmetricRelation, since 
two &%EngineeringComponents cannot be subcomponents of each other."):-true.
instance('connectedEngineeringComponents', 'SymmetricRelation'):-true.
instance('connectedEngineeringComponents', 'IrreflexiveRelation'):-true.
subrelation('connectedEngineeringComponents', connected):-true.
domain('connectedEngineeringComponents', 1, 'EngineeringComponent'):-true.
domain('connectedEngineeringComponents', 2, 'EngineeringComponent'):-true.
documentation('connectedEngineeringComponents', "This is the most general 
connection relation between &%EngineeringComponents.  If 
(&%connectedEngineeringComponents ?COMP1 ?COMP2), then neither ?COMP1 nor 
?COMP2 can be an &%engineeringSubcomponent of the other.  The relation 
&%connectedEngineeringComponents is a &%SymmetricRelation; there is no 
information in the direction of connection between two components.  It is 
also an &%IrreflexiveRelation; no &%EngineeringComponent bears this relation 
to itself.  Note that this relation does not associate a name or type 
with the connection."):-true.
=>(['connectedEngineeringComponents', '?COMP1', '?COMP2'], [and, [not, ['engineeringSubcomponent', '?COMP1', '?COMP2']], [not, ['engineeringSubcomponent', '?COMP2', '?COMP1']]]):-true.
=>(['connectedEngineeringComponents', '?COMP1', '?COMP2'], [not, [or, [instance, '?COMP1', 'EngineeringConnection'], [instance, '?COMP2', 'EngineeringConnection']]]):-true.
<=>(['connectedEngineeringComponents', '?COMP1', '?COMP2'], [exists, ['?CONNECTION'], ['connectsEngineeringComponents', '?CONNECTION', '?COMP1', '?COMP2']]):-true.
subclass('EngineeringConnection', 'EngineeringComponent'):-true.
documentation('EngineeringConnection', "An &%EngineeringConnection is an 
&%EngineeringComponent that represents a connection relationship between 
two other &%EngineeringComponents.  It is a reification of the 
&%Predicate &%connectedEngineeringComponents.  That means that whenever 
this &%Predicate holds between two &%EngineeringComponents, there exists an 
&%EngineeringConnection.  The practical reason for reifying a relationship 
is to be able to attach other information about it. For example, one
might want to say that a particular connection is associated with some 
shared parameters, or that it is of a particular type.  
&%EngineeringConnections are &%EngineeringComponents and can therefore be 
an &%engineeringSubcomponent of other &%EngineeringComponents.  However, 
to provide for modular regularity in component systems, 
&%EngineeringConnections cannot be connected.  For each pair of 
&%EngineeringComponents related by &%connectedEngineeringComponents, there 
exists at least one &%EngineeringConnection.  However, that object may not 
be unique, and the same &%EngineeringConnection may be associated with 
several pairs of &%EngineeringComponents."):-true.
=>([instance, '?CONNECTION', 'EngineeringConnection'], [exists, ['?COMP1', '?COMP2'], ['connectsEngineeringComponents', '?CONNECTION', '?COMP1', '?COMP2']]):-true.
subrelation('connectsEngineeringComponents', connects):-true.
domain('connectsEngineeringComponents', 1, 'EngineeringConnection'):-true.
domain('connectsEngineeringComponents', 2, 'EngineeringComponent'):-true.
domain('connectsEngineeringComponents', 3, 'EngineeringComponent'):-true.
documentation('connectsEngineeringComponents', "&%connectsEngineeringComponents 
is a &%TernaryPredicate that maps from an &%EngineeringConnection to the 
&%EngineeringComponents it connects.  Since &%EngineeringComponents cannot 
be connected to themselves and there cannot be an &%EngineeringConnection 
without a &%connectedEngineeringComponents &%Predicate, the second and third 
arguments of any &%connectsEngineeringComponents relationship will always be 
distinct for any given first argument."):-true.
subclass('Group', 'Collection'):-true.
subclass('Group', 'Agent'):-true.
documentation('Group', "A &%Collection of &%Agents, e.g. a flock 
of sheep, a herd of goats, or the local Boy Scout troop."):-true.
=>([and, [instance, '?GROUP', 'Group'], [member, '?MEMB', '?GROUP']], [instance, '?MEMB', 'Agent']):-true.
subclass('AgeGroup', 'Group'):-true.
documentation('AgeGroup', "A &%Group whose &%members all have the same &%age."):-true.
=>([instance, '?GROUP', 'AgeGroup'], [forall, ['?MEMB1', '?MEMB2', '?AGE1', '?AGE2'], [=>, [and, [member, '?MEMB1', '?GROUP'], [member, '?MEMB2', '?GROUP'], [age, '?MEMB1', '?AGE1'], [age, '?MEMB2', '?AGE2']], [equal, '?AGE1', '?AGE2']]]):-true.
subclass('FamilyGroup', 'Group'):-true.
documentation('FamilyGroup', "A &%Group whose &%members bear 
&%familyRelations to one another."):-true.
=>([instance, '?GROUP', 'FamilyGroup'], [forall, ['?MEMB1', '?MEMB2'], [=>, [and, [member, '?MEMB1', '?GROUP'], [member, '?MEMB2', '?GROUP']], ['familyRelation', '?MEMB1', '?MEMB2']]]):-true.
instance('familyRelation', 'BinaryPredicate'):-true.
instance('familyRelation', 'EquivalenceRelation'):-true.
domain('familyRelation', 1, 'Organism'):-true.
domain('familyRelation', 2, 'Organism'):-true.
documentation('familyRelation', "A very general &%Predicate for biological 
relationships. (&%familyRelation ?ORGANISM1 ?ORGANISM2) means that 
?ORGANISM1 and ?ORGANISM2 are biologically derived from a common ancestor."):-true.
=>(['familyRelation', '?ORGANISM1', '?ORGANISM2'], [exists, ['?ORGANISM3'], [and, ['familyRelation', '?ORGANISM3', '?ORGANISM1'], ['familyRelation', '?ORGANISM3', '?ORGANISM2']]]):-true.
subclass('EthnicGroup', 'Group'):-true.
documentation('EthnicGroup', "A &%Group whose &%members originate from 
the same &%GeographicArea or share the same &%Language and/or cultural 
practices."):-true.
subclass('OrganizationUnit', 'CognitiveAgent'):-true.
documentation('OrganizationUnit', "An organization, division or department 
within an organization. For example, the Shell Corporation, the accounting 
department at Shell, etc. would all be instances of &%OrganizationUnit. 
The continued existence of an &%OrganizationUnit is not dependent on any of 
its members, locations, or facilities. Note that, unlike &%Organization, an 
&%OrganizationUnit may have no members."):-true.
subclass('Organization', 'Group'):-true.
documentation('Organization', "An &%Organization is a corporate or similar 
institution, distinguished from other &%Agents. The &%members of an 
&%Organization typically have a common purpose or function. Note that 
parts of &%Organizations should not be included here, unless they are 
&%subOrganizations of an &%Organization."):-true.
instance('OrganizationFn', 'UnaryFunction'):-true.
domain('OrganizationFn', 1, 'OrganizationUnit'):-true.
range('OrganizationFn', 'Organization'):-true.
documentation('OrganizationFn', "A &%UnaryFunction that maps an &%OrganizationUnit, 
the notion of an organization which is independent of its members, locations, 
and facilities, to the corresponding &%Organization, i.e. the &%Group of &%Humans 
who are &%members of the &%Organization.  Note that this is a partial function, 
since there are many cases of organizations which are legal entities but which 
lack any members."):-true.
=>([and, [equal, ['OrganizationFn', '?UNIT'], '?ORG'], [instance, '?ATTR', 'NormativeAttribute']], [<=>, ['modalAttribute', '?UNIT', '?ATTR'], ['modalAttribute', '?ORG', '?ATTR']]):-true.
instance(employs, 'BinaryPredicate'):-true.
domain(employs, 1, 'Organization'):-true.
domain(employs, 2, 'CognitiveAgent'):-true.
documentation(employs, "(&%employs ?ORG ?PERSON) means that ?ORG has 
hired ?PERSON and currently retains ?PERSON, on a salaried or 
contractual basis, to provide services in exchange for monetary 
compensation."):-true.
=>([employs, '?ORG', '?PERSON'], [member, '?PERSON', '?ORG']):-true.
subclass('Corporation', 'Organization'):-true.
documentation('Corporation', "An &%Organization that provides products and/or 
services for a fee with the aim of making a profit."):-true.
subclass('PoliticalOrganization', 'Organization'):-true.
documentation('PoliticalOrganization', "An &%Organization that is a &%Government, 
a &%subOrganizations of a &%Government, or an &%Organization that is attempting 
to bring about some sort of political change."):-true.
=>([instance, '?POL', 'PoliticalOrganization'], [exists, ['?PROC'], [and, [instance, '?PROC', 'PoliticalProcess'], [agent, '?PROC', '?POL']]]):-true.
subclass('Government', 'PoliticalOrganization'):-true.
documentation('Government', "The ruling body of a &%Nation or one of the 
&%subOrganizations of the ruling body of a &%Nation."):-true.
subclass('EducationalOrganization', 'Organization'):-true.
documentation('EducationalOrganization', "A &%EducationalOrganization is 
an institution of learning. Some examples are public and private K-12 
schools, and colleges and universities."):-true.
subclass('ReligiousOrganization', 'Organization'):-true.
subclass('ReligiousOrganization', 'BeliefGroup'):-true.
documentation('ReligiousOrganization', "An &%Organization whose members 
share a set of religious beliefs."):-true.
subrelation('subOrganizations', 'subCollection'):-true.
instance('subOrganizations', 'IrreflexiveRelation'):-true.
instance('subOrganizations', 'TransitiveRelation'):-true.
domain('subOrganizations', 1, 'Organization'):-true.
domain('subOrganizations', 2, 'Organization'):-true.
documentation('subOrganizations', "(&%subOrganizations ?ORG1 ?ORG2) means 
that ?ORG1 is an &%Organization which is a proper part of the 
&%Organization ?ORG2."):-true.
instance(citizen, 'BinaryPredicate'):-true.
instance(citizen, 'AsymmetricRelation'):-true.
domain(citizen, 1, 'Human'):-true.
domain(citizen, 2, 'Nation'):-true.
documentation(citizen, "(&%citizen ?PERSON ?NATION) means that the 
&%Human ?PERSON is a citizen of &%Nation ?NATION."):-true.
subclass('FieldOfStudy', 'Proposition'):-true.
documentation('FieldOfStudy', "An academic or applied discipline with 
recognized experts and with a core of accepted theory or practice.  Note 
that &%FieldOfStudy is a &%subclass of &%Proposition, because a 
&%FieldOfStudy is understood to be a body of abstract, informational 
content, with varying degrees of certainty attached to each element of 
this content."):-true.
subclass('Procedure', 'Proposition'):-true.
documentation('Procedure', "A sequence-dependent specification.  Some 
examples are &%ComputerPrograms, finite-state machines, cooking recipes, 
musical scores, conference schedules, driving directions, and the scripts 
of plays and movies."):-true.
subclass('ComputerProgram', 'Procedure'):-true.
documentation('ComputerProgram', "A set of instructions in a computer 
programming language that can be executed by a computer."):-true.
subclass('Plan', 'Procedure'):-true.
documentation('Plan', "A specification of a sequence of &%Processes which 
is intended to satisfy a specified purpose at some future time."):-true.
=>([and, [instance, '?PLAN', 'Plan'], [instance, '?OBJ', 'ContentBearingObject'], ['containsInformation', '?OBJ', '?PLAN']], [exists, ['?PLANNING'], [and, [instance, '?PLANNING', 'Planning'], [result, '?PLANNING', '?OBJ']]]):-true.
=>([instance, '?PLAN', 'Plan'], [exists, ['?PURP'], ['hasPurpose', '?PLAN', '?PURP']]):-true.
instance(orientation, 'SpatialRelation'):-true.
instance(orientation, 'TernaryPredicate'):-true.
domain(orientation, 1, 'Object'):-true.
domain(orientation, 2, 'Object'):-true.
domain(orientation, 3, 'PositionalAttribute'):-true.
documentation(orientation, "A general &%Predicate for indicating how two 
&%Objects are oriented with respect to one another.  For example, 
(orientation ?OBJ1 ?OBJ2 North) means that ?OBJ1 is north of ?OBJ2, and 
(orientation ?OBJ1 ?OBJ2 Vertical) means that ?OBJ1 is positioned 
vertically with respect to ?OBJ2."):-true.
<=>([between, '?OBJ1', '?OBJ2', '?OBJ3'], [and, [orientation, '?OBJ2', '?OBJ1', 'Left'], [orientation, '?OBJ1', '?OBJ3', 'Left']]):-true.
=>([and, [orientation, '?OBJ1', '?OBJ2', '?ATTR1'], ['contraryAttribute', '?ATTR1', '?ATTR2']], [not, [orientation, '?OBJ1', '?OBJ2', '?ATTR2']]):-true.
instance(direction, 'CaseRole'):-true.
domain(direction, 1, 'Process'):-true.
domain(direction, 2, 'DirectionalAttribute'):-true.
documentation(direction, "(&%direction ?PROC ?ATTR) means that the 
&%Process ?PROC is moving in the direction ?ATTR.  For example, one 
would use this &%Predicate to represent the fact that Max is moving 
&%North."):-true.
=>(['holdsDuring', '?TIME', [direction, '?PROC', '?ATTR1']], [forall, ['?ATTR2'], [=>, ['holdsDuring', '?TIME', [direction, '?PROC', '?ATTR2']], [equal, '?ATTR2', '?ATTR1']]]):-true.
instance(faces, 'BinaryPredicate'):-true.
domain(faces, 1, 'Object'):-true.
domain(faces, 2, 'DirectionalAttribute'):-true.
documentation(faces, "(&%faces ?OBJ ?DIRECTION) means that the front of 
?OBJ (see &%FrontFn) is positioned towards the compass direction ?DIRECTION.  
More precisely, it means that if a line were extended from the center of 
?DIRECTION, the line would intersect with the front of ?OBJ before it 
intersected with its back (see &%BackFn)."):-true.
=>(['holdsDuring', '?TIME', [faces, '?PROC', '?ATTR1']], [forall, ['?ATTR2'], [=>, ['holdsDuring', '?TIME', [faces, '?PROC', '?ATTR2']], [equal, '?ATTR2', '?ATTR1']]]):-true.
subclass('TruthValue', 'RelationalAttribute'):-true.
documentation('TruthValue', "The &%Class of truth values, e.g. &%True and 
&%False.  These are &%Attributes of &%Sentences."):-true.
instance('True', 'TruthValue'):-true.
documentation('True', "The &%TruthValue of being true."):-true.
instance('False', 'TruthValue'):-true.
'contraryAttribute'('False', 'True'):-true.
documentation('False', "The &%TruthValue of being false."):-true.
instance('Likely', 'TruthValue'):-true.
'contraryAttribute'('Likely', 'Unlikely'):-true.
documentation('Likely', "The &%TruthValue of being probable, i.e. more likely than 
not to be &%True."):-true.
=>([attribute, '?FORMULA', 'Likely'], ['greaterThan', ['ProbabilityFn', [true, '?FORMULA', 'True']], ['ProbabilityFn', [true, '?FORMULA', 'False']]]):-true.
instance('Unlikely', 'TruthValue'):-true.
documentation('Unlikely', "The &%TruthValue of being improbable, i.e. more likely 
than not to be &%False."):-true.
=>([attribute, '?FORMULA', 'Unlikely'], ['greaterThan', ['ProbabilityFn', [true, '?FORMULA', 'False']], ['ProbabilityFn', [true, '?FORMULA', 'True']]]):-true.
subrelation(true, attribute):-true.
'relatedInternalConcept'(true, 'TruthFn'):-true.
domain(true, 1, 'Sentence'):-true.
domain(true, 2, 'TruthValue'):-true.
documentation(true, "The &%BinaryPredicate that relates a &%Sentence 
to its &%TruthValue."):-true.
subclass('PositionalAttribute', 'RelationalAttribute'):-true.
documentation('PositionalAttribute', "&%Attributes characterizing the 
orientation of an &%Object, e.g. &%Vertical versus &%Horizontal, &%Left 
versus &%Right etc."):-true.
subclass('DirectionalAttribute', 'PositionalAttribute'):-true.
documentation('DirectionalAttribute', "The subclass of &%PositionalAttributes 
that concern compass directions."):-true.
=>([and, [orientation, '?OBJ1', '?OBJ2', '?ATTR1'], [instance, '?ATTR1', 'DirectionalAttribute'], [instance, '?ATTR2', 'DirectionalAttribute'], [not, [equal, '?ATTR1', '?ATTR2']]], [not, [orientation, '?OBJ1', '?OBJ2', '?ATTR2']]):-true.
instance('North', 'DirectionalAttribute'):-true.
'contraryAttribute'('North', 'South'):-true.
documentation('North', "The compass direction of &%North."):-true.
instance('South', 'DirectionalAttribute'):-true.
documentation('South', "The compass direction of &%South."):-true.
<=>([orientation, '?OBJ1', '?OBJ2', 'North'], [orientation, '?OBJ2', '?OBJ1', 'South']):-true.
instance('East', 'DirectionalAttribute'):-true.
'contraryAttribute'('East', 'West'):-true.
documentation('East', "The compass direction of &%East."):-true.
instance('West', 'DirectionalAttribute'):-true.
documentation('West', "The compass direction of &%West."):-true.
<=>([orientation, '?OBJ1', '?OBJ2', 'East'], [orientation, '?OBJ2', '?OBJ1', 'West']):-true.
instance('Vertical', 'PositionalAttribute'):-true.
documentation('Vertical', "Attribute used to indicate that an &%Object 
is positioned height-wise with respect to another &%Object."):-true.
instance('Horizontal', 'PositionalAttribute'):-true.
'contraryAttribute'('Horizontal', 'Vertical'):-true.
documentation('Horizontal', "Attribute used to indicate that an &%Object 
is positioned width-wise with respect to another &%Object."):-true.
<=>([orientation, '?OBJ1', '?OBJ2', 'Vertical'], [orientation, '?OBJ2', '?OBJ1', 'Horizontal']):-true.
instance('Above', 'PositionalAttribute'):-true.
'contraryAttribute'('Above', 'Below'):-true.
documentation('Above', "This is a &%PositionalAttribute derived from the 
up/down schema and not involving contact.  Note that this means directly 
above, i.e., if one object is &%Above another object, then the projections 
of the two objects overlap."):-true.
<=>([orientation, '?OBJ1', '?OBJ2', 'Above'], [orientation, '?OBJ2', '?OBJ1', 'Below']):-true.
=>([orientation, '?OBJ1', '?OBJ2', 'Above'], [not, [connected, '?OBJ1', '?OBJ2']]):-true.
instance('Below', 'PositionalAttribute'):-true.
documentation('Below', "This &%PositionalAttribute is derived from the 
up/down schema and may or may not involve contact.  Note that this means 
directly below, i.e., if one object is &%Below another object, then the 
projections of the two objects overlap."):-true.
=>([orientation, '?OBJ1', '?OBJ2', 'Below'], [or, [orientation, '?OBJ2', '?OBJ1', 'On'], [orientation, '?OBJ2', '?OBJ1', 'Above']]):-true.
instance('Adjacent', 'PositionalAttribute'):-true.
documentation('Adjacent', "Used to assert that an object ?OBJ1 is close 
to, near or abutting ?OBJ2.  This &%PositionalAttribute covers the 
following common sense notions:  adjoins, abuts, is contiguous to, 
is juxtaposed, and is close to."):-true.
=>([orientation, '?OBJ1', '?OBJ2', 'Adjacent'], [or, [orientation, '?OBJ1', '?OBJ2', 'Near'], [connected, '?OBJ1', '?OBJ2']]):-true.
instance('Left', 'PositionalAttribute'):-true.
documentation('Left', "This &%PositionalAttribute is derived from the 
left/right schema.  Note that this means directly to the left, so that, 
if one object is to the left of another, then the projections of the 
two objects overlap."):-true.
instance('Right', 'PositionalAttribute'):-true.
'contraryAttribute'('Right', 'Left'):-true.
documentation('Right', "This &%PositionalAttribute is derived from the 
left/right schema.  Note that this means directly to the right, so that, 
if one object is to the right of another, then the projections of the 
two objects overlap."):-true.
<=>([orientation, '?OBJ1', '?OBJ2', 'Right'], [orientation, '?OBJ2', '?OBJ1', 'Left']):-true.
instance('Near', 'PositionalAttribute'):-true.
documentation('Near', "The relation of common sense adjacency.  Note that, if 
an object is &%Near another object, then the objects are not &%connected."):-true.
=>([orientation, '?OBJ1', '?OBJ2', 'Near'], [not, [connected, '?OBJ1', '?OBJ2']]):-true.
=>([orientation, '?OBJ1', '?OBJ2', 'Near'], [orientation, '?OBJ2', '?OBJ1', 'Near']):-true.
instance('On', 'PositionalAttribute'):-true.
documentation('On', "This is used to assert that an object is on top of 
another object, and it is derived from the up/down schema and involves 
contact."):-true.
=>([orientation, '?OBJ1', '?OBJ2', 'On'], [connected, '?OBJ1', '?OBJ2']):-true.
=>([orientation, '?OBJ1', '?OBJ2', 'On'], [located, '?OBJ1', '?OBJ2']):-true.
=>([orientation, '?OBJ1', '?OBJ2', 'On'], [not, [orientation, '?OBJ2', '?OBJ1', 'On']]):-true.
subclass('TimeZone', 'RelationalAttribute'):-true.
documentation('TimeZone', "An &%Attribute which is used to specify coordinates 
in which time measures are uniform, i.e. all time devices are synchronized to 
the same &%TimePositions."):-true.
instance('CoordinatedUniversalTimeZone', 'TimeZone'):-true.
documentation('CoordinatedUniversalTimeZone', "A &%TimeZone which functions 
as the standard time zone.  It is also known as Zulu time (in the military), 
Greenwich Mean Time, and the Western European time zone.  Note that whenever 
a &%TimeZone is not specified, the &%TimePosition is understood to be with 
respect to the &%CoordinatedUniversalTimeZone."):-true.
instance('PacificTimeZone', 'TimeZone'):-true.
documentation('PacificTimeZone', "A &%TimeZone that covers much of the 
western part of the United States."):-true.
=>([equal, ['RelativeTimeFn', '?TIME1', 'PacificTimeZone'], '?TIME2'], [equal, '?TIME2', ['AdditionFn', '?TIME1', 8]]):-true.
instance('MountainTimeZone', 'TimeZone'):-true.
documentation('MountainTimeZone', "A &%TimeZone that covers much of the 
Rocky Mountain region of the United States."):-true.
=>([equal, ['RelativeTimeFn', '?TIME1', 'MountainTimeZone'], '?TIME2'], [equal, '?TIME2', ['AdditionFn', '?TIME1', 7]]):-true.
instance('CentralTimeZone', 'TimeZone'):-true.
documentation('CentralTimeZone', "A &%TimeZone that covers much of the 
midwestern United States."):-true.
=>([equal, ['RelativeTimeFn', '?TIME1', 'CentralTimeZone'], '?TIME2'], [equal, '?TIME2', ['AdditionFn', '?TIME1', 6]]):-true.
instance('EasternTimeZone', 'TimeZone'):-true.
documentation('EasternTimeZone', "A &%TimeZone that covers much of the 
eastern United States."):-true.
=>([equal, ['RelativeTimeFn', '?TIME1', 'EasternTimeZone'], '?TIME2'], [equal, '?TIME2', ['AdditionFn', '?TIME1', 5]]):-true.
instance('RelativeTimeFn', 'BinaryFunction'):-true.
instance('RelativeTimeFn', 'TemporalRelation'):-true.
domain('RelativeTimeFn', 1, 'TimePosition'):-true.
domain('RelativeTimeFn', 2, 'TimeZone'):-true.
range('RelativeTimeFn', 'TimePosition'):-true.
documentation('RelativeTimeFn', "A means of converting &%TimePositions 
between different &%TimeZones.  (&%RelativeTimeFn ?TIME ?ZONE) 
denotes the &%TimePosition in &%CoordinatedUniversalTime that is 
contemporaneous with the &%TimePosition ?TIME in &%TimeZone ?ZONE.  
For example, (&%RelativeTimeFn (&%MeasureFn 14 &%Hour) &%EasternTimeZone) 
would return the value (&%MeasureFn 19 &%Hour)."):-true.
subclass('SocialRole', 'RelationalAttribute'):-true.
documentation('SocialRole', "The &%Class of all &%Attributes that 
specify the position or status of a &%CognitiveAgent within an 
&%Organization or other &%Group."):-true.
=>([and, [attribute, '?PERSON', '?ATTRIBUTE'], [instance, '?ATTRIBUTE', 'SocialRole']], [instance, '?PERSON', 'Human']):-true.
subclass('OccupationalRole', 'SocialRole'):-true.
documentation('OccupationalRole', "The &%Class of all &%Attributes that 
specify an occupational role of a &%CognitiveAgent."):-true.
instance('Unemployed', 'OccupationalRole'):-true.
documentation('Unemployed', "The &%Attribute of a &%CognitiveAgent when 
he/she is unemployed."):-true.
<=>([attribute, '?PERSON', 'Unemployed'], [not, [exists, ['?ORG'], [employs, '?ORG', '?PERSON']]]):-true.
subclass('Position', 'OccupationalRole'):-true.
documentation('Position', "A formal position of reponsibility within an 
&%Organization. Examples of &%Positions include president, laboratory 
director, senior researcher, sales representative, etc."):-true.
instance('occupiesPosition', 'TernaryPredicate'):-true.
domain('occupiesPosition', 1, 'Human'):-true.
domain('occupiesPosition', 2, 'Position'):-true.
domain('occupiesPosition', 3, 'Organization'):-true.
documentation('occupiesPosition', "(&%occupiesPosition ?PERSON ?POSITION ?ORG) 
means that ?PERSON holds the &%Position ?POSITION at &%Organization ?ORG. 
For example, (&%occupiesPosition &%TomSmith &%ResearchDirector 
&%AcmeLaboratory) means that &%TomSmith is a research director at Acme Labs."):-true.
=>([employs, '?ORG', '?PERSON'], [exists, ['?POSITION'], ['occupiesPosition', '?PERSON', '?POSITION', '?ORG']]):-true.
subclass('NormativeAttribute', 'RelationalAttribute'):-true.
documentation('NormativeAttribute', "A &%Class containing all of the 
&%Attributes that are specific to judgements about morality, legality, 
aesthetics, etiquette, etc.  In general, these judgements express that 
something ought to be the case."):-true.
instance('modalAttribute', 'BinaryPredicate'):-true.
instance('modalAttribute', 'AsymmetricRelation'):-true.
instance('modalAttribute', 'IrreflexiveRelation'):-true.
subrelation('modalAttribute', property):-true.
domain('modalAttribute', 1, 'Formula'):-true.
domain('modalAttribute', 2, 'NormativeAttribute'):-true.
documentation('modalAttribute', "A &%BinaryRelation that is used to state the 
normative force of a &%Proposition.  (&%modalAttribute ?FORMULA ?PROP) means 
that the &%Proposition expressed by ?FORMULA has the &%NormativeAttribute 
?PROP.  For example, (&%modalAttribute (&%exists (?ACT ?OBJ) (&%and 
(&%instance ?ACT &%Giving) (&%agent ?ACT John) (&%patient ?ACT ?OBJ) 
(&%destination ?ACT Tom))) &%Obligation) means that John is obligated to give 
Tom something."):-true.
=>([and, ['modalAttribute', '?FORMULA1', '?PROP'], [entails, '?FORMULA1', '?FORMULA2']], ['modalAttribute', '?FORMULA2', '?PROP']):-true.
=>(['holdsObligation', '?PROCESS', '?AGENT'], ['modalAttribute', [exists, ['?INSTANCE'], [and, [instance, '?INSTANCE', '?PROCESS'], [agent, '?INSTANCE', '?AGENT']]], 'Obligation']):-true.
=>(['holdsRight', '?PROCESS', '?AGENT'], ['modalAttribute', [exists, ['?INSTANCE'], [and, [instance, '?INSTANCE', '?PROCESS'], [agent, '?INSTANCE', '?AGENT']]], 'Permission']):-true.
subclass('SubjectiveAssessmentAttribute', 'NormativeAttribute'):-true.
disjoint('SubjectiveAssessmentAttribute', 'ObjectiveNorm'):-true.
documentation('SubjectiveAssessmentAttribute', "The &%Class of &%NormativeAttributes
which lack an objective criterion for their attribution, i.e. the attribution of 
these &%Attributes varies from subject to subject and even with respect to the 
same subject over time.  This &%Class is, generally speaking, only used when 
mapping external knowledge sources to the SUMO.  If a term from such a knowledge 
source seems to lack objective criteria for its attribution, it is assigned to 
this &%Class."):-true.
subclass('ObjectiveNorm', 'NormativeAttribute'):-true.
documentation('ObjectiveNorm', "The &%Class of &%NormativeAttributes that are 
associated with an objective criterion for their attribution, i.e. there is 
broad consensus about the cases where these attributes are applicable."):-true.
subclass('ContestAttribute', 'ObjectiveNorm'):-true.
documentation('ContestAttribute', "A &%Class containing all of the 
&%Attributes that are specific to participants in a &%Contest.  Some 
of these &%Attributes are winning, losing, won, lost, etc."):-true.
=>([and, [attribute, '?ENTITY', '?ATTR'], [instance, '?ATTR', 'ContestAttribute']], [exists, ['?CONTEST'], [and, [instance, '?CONTEST', 'Contest'], [or, [agent, '?CONTEST', '?ENTITY'], [patient, '?CONTEST', '?ENTITY']]]]):-true.
subclass('AlethicAttribute', 'ObjectiveNorm'):-true.
documentation('AlethicAttribute', "A &%Class containing all of the &%Attributes 
relating to the notions of possibility and necessity."):-true.
instance('Possibility', 'AlethicAttribute'):-true.
documentation('Possibility', "Attribute that applies to &%Propositions that are 
possible, i.e. true in at least one possible world."):-true.
instance('Necessity', 'AlethicAttribute'):-true.
documentation('Necessity', "Attribute that applies to &%Propositions that are 
necessary, i.e. true in every possible world."):-true.
<=>(['modalAttribute', '?FORMULA', 'Necessity'], [not, ['modalAttribute', [not, '?FORMULA'], 'Possibility']]):-true.
=>(['modalAttribute', '?FORMULA', 'Necessity'], ['modalAttribute', '?FORMULA', 'Possibility']):-true.
subclass('DeonticAttribute', 'ObjectiveNorm'):-true.
documentation('DeonticAttribute', "A &%Class containing all of the &%Attributes 
relating to the notions of permission, obligation, and prohibition."):-true.
instance('Permission', 'DeonticAttribute'):-true.
documentation('Permission', "&%Attribute that applies to &%Propositions that an 
&%Agent is permitted, by some authority, to make true."):-true.
instance('Obligation', 'DeonticAttribute'):-true.
documentation('Obligation', "&%Attribute that applies to &%Propositions that an 
&%Agent is required, by some authority, to make true."):-true.
<=>(['modalAttribute', '?FORMULA', 'Obligation'], [not, ['modalAttribute', [not, '?FORMULA'], 'Permission']]):-true.
=>(['modalAttribute', '?FORMULA', 'Obligation'], ['modalAttribute', '?FORMULA', 'Permission']):-true.
'subAttribute'('Law', 'Obligation'):-true.
documentation('Law', "&%Attribute that applies to &%Propositions that are 
required by a government or a branch of the government and that are enforced 
with penalties for noncompliance.  These &%Propositions may be codified as 
legislation or they may be more informal, as in the case of government policy."):-true.
instance('Promise', 'Obligation'):-true.
documentation('Promise', "&%Attribute that applies to &%Propositions that 
an &%Agent promises to make true.  &%Promises may be implicit or explicit.  
They may be expressed in a written or verbal or gestural manner."):-true.
'subAttribute'('Contract', 'Promise'):-true.
documentation('Contract', "&%Attribute that applies to &%Propositions where 
something is promised in return, i.e. a reciprocal promise."):-true.
'subAttribute'('PurchaseContract', 'Contract'):-true.
documentation('PurchaseContract', "A &%Contract between two &%Agents in 
which one &%Agent agrees to render the other some good or service in 
exchange for currency."):-true.
'subAttribute'('ServiceContract', 'Contract'):-true.
documentation('ServiceContract', "A &%Contract where an &%Agent agrees to 
perform a service for another &%Agent (usually for a price)."):-true.
'subAttribute'('Warranty', 'ServiceContract'):-true.
documentation('Warranty', "A &%Contract that states the cirumstances 
under which defects in the product will be corrected for no charge.  
A &%Warranty is usually limited to a length of time that is specified 
in the &%Warranty itself.  A &%Warranty also includes information about 
what is not covered and actions that invalidate the &%Warranty."):-true.
'subAttribute'('NakedPromise', 'Promise'):-true.
'contraryAttribute'('NakedPromise', 'Contract'):-true.
documentation('NakedPromise', "A &%Promise where nothing is promised in return, 
i.e. a nudum pactum."):-true.
subclass('PhysicalState', 'InternalAttribute'):-true.
documentation('PhysicalState', "The physical state of an &%Object.  There 
are three instances of this &%Class:  &%Solid, &%Liquid, and &%Gas.  
Physical changes are not characterized by the transformation of one 
substance into another, but rather by the change of the form (physical 
states) of a given substance. The bending of a piece of copper wire fails 
to change the copper into another substance; crushing a block of ice leaves 
only crushed ice; melting an iron nail yields a substance still called 
iron: these are all accepted as physical changes."):-true.
instance('Solid', 'PhysicalState'):-true.
documentation('Solid', "An &%Object has the &%Attribute of &%Solid if it 
has a fixed shape and a fixed volume."):-true.
instance('Liquid', 'PhysicalState'):-true.
documentation('Liquid', "An &%Object has the &%Attribute of &%Liquid if 
it has a fixed volume but not a fixed shape."):-true.
=>([instance, '?X', 'Solution'], [attribute, '?X', 'Liquid']):-true.
instance('Gas', 'PhysicalState'):-true.
documentation('Gas', "An &%Object has the &%Attribute of &%Gas if it has 
neither a fixed volume nor a fixed shape."):-true.
=>([and, [instance, '?STATE', 'PhysicalState'], [part, '?PART', '?OBJ'], ['holdsDuring', '?TIME', [attribute, '?OBJ', '?STATE']]], [not, [exists, ['?OTHERSTATE'], [and, [instance, '?OTHERSTATE', 'PhysicalState'], ['holdsDuring', '?TIME', [attribute, '?PART', '?OTHERSTATE']], [not, [equal, '?STATE', '?OTHERSTATE']]]]]):-true.
subclass('PerceptualAttribute', 'InternalAttribute'):-true.
documentation('PerceptualAttribute', "Any &%Attribute whose presence is detected 
by an act of &%Perception."):-true.
=>([and, [instance, '?PERCEPTION', 'Perception'], [patient, '?PERCEPTION', '?OBJ']], [exists, ['?PROP'], [and, [instance, '?PROP', 'PerceptualAttribute'], [attribute, '?OBJ', '?PROP']]]):-true.
subclass('TasteAttribute', 'PerceptualAttribute'):-true.
documentation('TasteAttribute', "The &%Class of &%Attributes relating to 
the taste of &%Objects."):-true.
=>([instance, '?OBJ', 'Food'], [exists, ['?ATTR'], [and, [instance, '?ATTR', 'TasteAttribute'], [attribute, '?OBJ', '?ATTR']]]):-true.
instance('Sweet', 'TasteAttribute'):-true.
'contraryAttribute'('Sweet', 'Bitter'):-true.
documentation('Sweet', "The &%Attribute of &%Objects that are 
sweet-tasting."):-true.
instance('Bitter', 'TasteAttribute'):-true.
documentation('Bitter', "The &%Attribute of &%Objects that are 
bitter-tasting."):-true.
subclass('OlfactoryAttribute', 'PerceptualAttribute'):-true.
documentation('OlfactoryAttribute', "The &%Class of properties that are 
detectable by smell."):-true.
subclass('VisualAttribute', 'PerceptualAttribute'):-true.
documentation('VisualAttribute', "The &%Class of visually discernible 
properties."):-true.
instance('Illuminated', 'VisualAttribute'):-true.
documentation('Illuminated', "The &%Attribute of &%Regions that are 
illuminated to some degree, i.e. in which some shapes are visually 
discernable."):-true.
instance('Unilluminated', 'VisualAttribute'):-true.
'contraryAttribute'('Unilluminated', 'Illuminated'):-true.
documentation('Unilluminated', "The &%Attribute of &%Regions that are 
unilluminated, i.e in which no shapes are visually discernable."):-true.
subclass('ColorAttribute', 'VisualAttribute'):-true.
documentation('ColorAttribute', "The &%Class of &%VisualAttributes 
relating to the color of &%Objects."):-true.
subclass('PrimaryColor', 'ColorAttribute'):-true.
documentation('PrimaryColor', "Colors which can be blended to form any 
color and which cannot be derived from any other colors."):-true.
instance('Red', 'PrimaryColor'):-true.
documentation('Red', "The &%Attribute of redness."):-true.
instance('Blue', 'PrimaryColor'):-true.
documentation('Blue', "The &%Attribute of being blue in color."):-true.
instance('Yellow', 'PrimaryColor'):-true.
documentation('Yellow', "The &%Attribute of being yellow in color."):-true.
instance('White', 'PrimaryColor'):-true.
documentation('White', "The &%Attribute of being white in color."):-true.
instance('Black', 'PrimaryColor'):-true.
documentation('Black', "The &%Attribute of being black in color."):-true.
instance('Monochromatic', 'ColorAttribute'):-true.
documentation('Monochromatic', "An &%Object with this &%Attribute has 
the same color on every part of its surface."):-true.
=>([and, [attribute, '?OBJ', 'Monochromatic'], ['superficialPart', '?PART', '?OBJ'], [attribute, '?PART', '?COLOR'], [instance, '?COLOR', 'PrimaryColor']], [forall, ['?ELEMENT'], [=>, ['superficialPart', '?ELEMENT', '?OBJ'], [attribute, '?ELEMENT', '?COLOR']]]):-true.
instance('Polychromatic', 'ColorAttribute'):-true.
'contraryAttribute'('Polychromatic', 'Monochromatic'):-true.
documentation('Polychromatic', "An &%Object with this &%Attribute has 
different colors on different parts of its surface."):-true.
=>([attribute, '?OBJ', 'Polychromatic'], [exists, ['?PART1', '?PART2', '?COLOR1', '?COLOR2'], [and, ['superficialPart', '?PART1', '?OBJ'], ['superficialPart', '?PART2', '?OBJ'], [attribute, '?PART1', '?COLOR1'], [attribute, '?PART2', '?COLOR2'], [instance, '?COLOR1', 'ColorAttribute'], [instance, '?COLOR2', 'ColorAttribute'], [not, [equal, '?COLOR1', '?COLOR2']]]]):-true.
subclass('ShapeAttribute', 'InternalAttribute'):-true.
documentation('ShapeAttribute', "Any &%Attribute that relates to the 
shape of an &%Object."):-true.
instance('Pliable', 'ShapeAttribute'):-true.
documentation('Pliable', "The shape of an &%Object with this &%Attribute 
can be altered."):-true.
instance('Rigid', 'ShapeAttribute'):-true.
'contraryAttribute'('Rigid', 'Pliable'):-true.
documentation('Rigid', "The shape of an &%Object with this &%Attribute 
cannot be easily altered."):-true.
subclass('TextureAttribute', 'PerceptualAttribute'):-true.
documentation('TextureAttribute', "Any &%Attribute that characterizes the 
texture of an &%Object."):-true.
=>([and, [instance, '?ATTRIBUTE', 'TextureAttribute'], [attribute, '?OBJ', '?ATTRIBUTE'], [surface, '?SURFACE', '?OBJ']], [attribute, '?SURFACE', '?ATTRIBUTE']):-true.
instance('Smooth', 'TextureAttribute'):-true.
documentation('Smooth', "An &%Object with this &%Attribute has a smooth 
surface."):-true.
instance('Rough', 'TextureAttribute'):-true.
'contraryAttribute'('Smooth', 'Rough'):-true.
documentation('Rough', "An &%Object with this &%Attribute has a rough 
surface."):-true.
subclass('SoundAttribute', 'PerceptualAttribute'):-true.
documentation('SoundAttribute', "Any &%Attribute that characterizes the 
sound made by an &%Object."):-true.
subclass('SaturationAttribute', 'InternalAttribute'):-true.
documentation('SaturationAttribute', "A &%Class of &%Attributes that specify 
whether there is any &%Liquid in a particular &%Object."):-true.
instance('Dry', 'SaturationAttribute'):-true.
'contraryAttribute'('Dry', 'Damp'):-true.
'contraryAttribute'('Dry', 'Wet'):-true.
documentation('Dry', "An &%Attribute which indicates that the associated 
&%Object contains no &%Liquid."):-true.
=>([attribute, '?OBJ', 'Dry'], [not, [exists, ['?SUBOBJ'], [and, [part, '?SUBOBJ', '?OBJ'], [attribute, '?SUBOBJ', 'Liquid']]]]):-true.
instance('Damp', 'SaturationAttribute'):-true.
documentation('Damp', "An &%Attribute which indicates that the associated 
&%Object contains some &%Liquid."):-true.
instance('Wet', 'SaturationAttribute'):-true.
'subAttribute'('Wet', 'Damp'):-true.
documentation('Wet', "An &%Attribute which indicates that the 
associated &%Object is fully saturated with a &%Liquid, i.e. 
every part of the &%Object has a subpart which is a &%Liquid."):-true.
=>([attribute, '?OBJ', 'Wet'], [forall, ['?PART'], [=>, [part, '?PART', '?OBJ'], [exists, ['?SUBPART'], [and, [part, '?SUBPART', '?PART'], [attribute, '?SUBPART', 'Liquid']]]]]):-true.
subclass('BreakabilityAttribute', 'InternalAttribute'):-true.
documentation('BreakabilityAttribute', "A &%subclass of &%Attributes for 
characterizing the breakability of &%CorpuscularObjects."):-true.
instance('Fragile', 'BreakabilityAttribute'):-true.
documentation('Fragile', "An &%Attribute which indicates that the 
associated &%Object is very breakable."):-true.
instance('Unbreakable', 'BreakabilityAttribute'):-true.
'contraryAttribute'('Unbreakable', 'Fragile'):-true.
documentation('Unbreakable', "An &%Attribute which indicates that the 
associated &%Object cannot be broken under ordinary circumstances."):-true.
subclass('BiologicalAttribute', 'InternalAttribute'):-true.
documentation('BiologicalAttribute', "&%Attributes that apply specifically 
to instances of &%Organism or &%parts of an &%Organism."):-true.
=>([and, [attribute, '?ORG', '?ATT'], [instance, '?ATT', 'BiologicalAttribute']], [instance, '?ORG', 'Organism']):-true.
subclass('AnimacyAttribute', 'BiologicalAttribute'):-true.
documentation('AnimacyAttribute', "&%Attributes that indicate whether an 
&%Organism is alive or not."):-true.
instance('Living', 'AnimacyAttribute'):-true.
documentation('Living', "This &%Attribute applies to &%Organisms that are 
alive."):-true.
=>([and, [instance, '?ORGANISM', 'Organism'], [agent, '?PROCESS', '?ORGANISM']], ['holdsDuring', ['WhenFn', '?PROCESS'], [attribute, '?ORGANISM', 'Living']]):-true.
instance('Dead', 'AnimacyAttribute'):-true.
'contraryAttribute'('Dead', 'Living'):-true.
documentation('Dead', "This &%Attribute applies to &%Organisms that are 
not alive."):-true.
subclass('SexAttribute', 'BiologicalAttribute'):-true.
documentation('SexAttribute', "&%Attributes that indicate the sex of an 
&%Organism."):-true.
instance('Female', 'SexAttribute'):-true.
documentation('Female', "An &%Attribute indicating that an &%Organism is 
female in nature."):-true.
instance('Male', 'SexAttribute'):-true.
'contraryAttribute'('Male', 'Female'):-true.
documentation('Male', "An &%Attribute indicating that an &%Organism is 
male in nature."):-true.
subclass('DevelopmentalAttribute', 'BiologicalAttribute'):-true.
documentation('DevelopmentalAttribute', "&%Attributes that indicate the 
stage of development of an &%Organism."):-true.
instance('FullyFormed', 'DevelopmentalAttribute'):-true.
documentation('FullyFormed', "The stage of an &%Organism or an 
&%AnatomicalStructure when it has reached the end of its growth 
phase."):-true.
=>([attribute, '?OBJ', 'FullyFormed'], [exists, ['?GROWTH'], [and, [instance, '?GROWTH', 'Growth'], [experiencer, '?GROWTH', '?OBJ'], ['holdsDuring', ['BeginFn', ['WhenFn', '?OBJ']], [attribute, '?OBJ', 'NonFullyFormed']]]]):-true.
instance('NonFullyFormed', 'DevelopmentalAttribute'):-true.
'contraryAttribute'('NonFullyFormed', 'FullyFormed'):-true.
'successorAttribute'('NonFullyFormed', 'FullyFormed'):-true.
documentation('NonFullyFormed', "The stage of an &%Animal before it is 
&%FullyFormed."):-true.
instance('Larval', 'DevelopmentalAttribute'):-true.
'subAttribute'('Larval', 'NonFullyFormed'):-true.
documentation('Larval', "Form of most &%Invertebrates, &%Amphibians, and 
&%Fish immediately after they hatch.  This form is fundamentally unlike 
the adult form, and metamorphosis is required to reach the latter form."):-true.
instance('Embryonic', 'DevelopmentalAttribute'):-true.
'subAttribute'('Embryonic', 'NonFullyFormed'):-true.
'successorAttribute'('Embryonic', 'Fetal'):-true.
documentation('Embryonic', "The stage of an &%Organism or an 
&%AnatomicalStructure that exists only before the &%Organism is fully 
formed.  &%Mammals, for example, have this &%Attribute only prior to 
their birth."):-true.
instance('Fetal', 'DevelopmentalAttribute'):-true.
'subAttribute'('Fetal', 'NonFullyFormed'):-true.
'contraryAttribute'('Fetal', 'Embryonic'):-true.
documentation('Fetal', "A &%Vertebrate in the later stage of development before 
its birth has this &%Attribute, which indicates that the main features of the 
&%FullyFormed animal are recognizable."):-true.
subclass('DiseaseOrSyndrome', 'BiologicalAttribute'):-true.
documentation('DiseaseOrSyndrome', "A &%BiologicalAttribute which qualifies 
something that alters or interferes with a normal process, state or activity 
of an &%Organism.  It is usually characterized by the abnormal functioning of 
one or more of the host's systems, parts, or &%Organs."):-true.
subclass('PsychologicalAttribute', 'BiologicalAttribute'):-true.
partition('PsychologicalAttribute', 'StateOfMind', 'TraitAttribute'):-true.
documentation('PsychologicalAttribute', "&%Attributes that characterize the mental 
or behavioral life of an &%Organism (and perhaps other &%SentientAgents)."):-true.
=>([instance, '?PROPERTY', 'PsychologicalAttribute'], [=>, ['holdsDuring', '?TIME', [attribute, '?ORGANISM', '?PROPERTY']], ['holdsDuring', '?TIME', [attribute, '?ORGANISM', 'Living']]]):-true.
subclass('StateOfMind', 'PsychologicalAttribute'):-true.
documentation('StateOfMind', "The class &%StateOfMind is distinguished from 
its complement &%TraitAttribute by the fact that instances of the former are 
transient while instances of the latter are persistent features of a creature's behavioral/psychological make-up."):-true.
subclass('EmotionalState', 'StateOfMind'):-true.
documentation('EmotionalState', "The &%Class of &%Attributes that denote emotional 
states of &%Organisms (and perhaps other &%Agents).  ."):-true.
subclass('ConsciousnessAttribute', 'StateOfMind'):-true.
documentation('ConsciousnessAttribute', "&%Attributes that indicate whether 
an &%Organism is conscious or the qualitative degree of consciousness of 
an &%Organism."):-true.
subclass('Asleep', 'ConsciousnessAttribute'):-true.
'contraryAttribute'('Asleep', 'Unconscious'):-true.
'contraryAttribute'('Asleep', 'Awake'):-true.
documentation('Asleep', "This &%Attribute applies to &%Organisms that are 
sleeping."):-true.
subclass('Unconscious', 'ConsciousnessAttribute'):-true.
'contraryAttribute'('Unconscious', 'Awake'):-true.
documentation('Unconscious', "This &%Attribute applies to &%Organisms that 
are unconscious because of a non-natural process, e.g. a blow to the head, 
a drug, etc.  Note that the &%Attribute &%Asleep is used to describe the 
&%ConsciousnessAttribute of an &%Organism that is asleep."):-true.
subclass('Awake', 'ConsciousnessAttribute'):-true.
documentation('Awake', "This &%Attribute applies to &%Organisms that are 
neither &%Unconscious nor &%Asleep."):-true.
subclass('TraitAttribute', 'PsychologicalAttribute'):-true.
documentation('TraitAttribute', "&%Attributes that indicate the the 
behavior/personality traits of an &%Organism."):-true.
instance('Aggressive', 'TraitAttribute'):-true.
'contraryAttribute'('Aggressive', 'Docile'):-true.
documentation('Aggressive', "The &%Attribute of having an aggressive 
disposition."):-true.
instance('Docile', 'TraitAttribute'):-true.
documentation('Docile', "The &%Attribute of having a docile disposition."):-true.
subclass('PsychologicalDysfunction', 'PsychologicalAttribute'):-true.
subclass('PsychologicalDysfunction', 'DiseaseOrSyndrome'):-true.
documentation('PsychologicalDysfunction', "A clinically significant 
dysfunction whose major manifestation is behavioral or psychological.  
These dysfunctions may have identified or presumed biological etiologies 
or manifestations."):-true.
=>([and, [instance, '?DYSFUNCTION', 'PsychologicalDysfunction'], [attribute, '?AGENT', '?DYSFUNCTION']], [instance, '?AGENT', 'SentientAgent']):-true.
%End of file
