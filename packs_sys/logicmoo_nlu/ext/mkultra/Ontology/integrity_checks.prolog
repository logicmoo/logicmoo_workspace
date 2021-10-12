known_object(Object) :-
   for_all_unique(Object,
		  declare_kind(Object, _),
		  true).

known_type(number).
known_type(string).
known_type(List) :-
   list(List).
known_type(kind_of(Kind)) :-
   kind(Kind).
known_type(subkind_of(Kind)) :-
   kind(Kind).
known_type(Kind) :-
   kind(Kind).

known_property(Property) :-
   for_all_unique(Property, property_type(Property, _, _)).

known_relation(Relation) :-
   for_all_unique(Relation, relation_type(Relation, _, _)).

test(integrity(property_declarations_well_formed),
     [ problem_list("The following properties specify unknown types in the spreadsheet",
		    Malformed) ]) :-
   all(Property,
       ( property_type(Property, ObjectType, ValueType),
	 \+ ( known_type(ObjectType),
	      known_type(ValueType) ) ),
       Malformed).

test(integrity(properties_have_types_declared),
     [ problem_list("The following properties have no type listed in the spreadsheet",
		    UndeclaredProperties) ]) :-
   all(Property,
       ( declare_value(_, Property, _),
	 \+ property_type(Property, _, _) ),
       UndeclaredProperties).

test(integrity(valid_property_types),
     [ problem_list("The following objects have properties with invalid types",
		    InvalidValues) ]) :-
   all(Object.Property=Value,
       ( declare_value(Object, Property, Value),
	 property_type(Property, ObjectType, ValueType),
	 \+ ( is_type(Object, ObjectType),
	      is_type(Value, ValueType) ) ),
       InvalidValues).

test(integrity(relation_declarations_well_formed),
     [ problem_list("The following relations have invalid types in the spreadsheet",
		    Malformed) ]) :-
   all(Relation,
       ( relation_type(Relation, ObjectType, ValueType),
	 \+ ( known_type(ObjectType),
	      known_type(ValueType) ) ),
       Malformed).

test(integrity(relations_declared),
     [ problem_list("The following relations are not declared in the spreadsheet",
		    UndeclaredRelations) ]) :-
   all(Relation,
       ( declare_related(_, Relation, _),
	 \+ relation_type(Relation, _, _) ),
       UndeclaredRelations).

test(integrity(inverse_relations_must_be_declared_in_their_canonical_form),
     [ problem_list("The following objects declare relations that should be declared through their inverses instead",
		    Malformed) ]) :-
   all(related(X, R, Y),
       ( declare_related(X, R, Y),
	 inverse_relation(R, _) ),
       Malformed).

test(integrity(valid_relation_types),
     [ problem_list("The following objects declare relations with the wrong types",
		    InvalidValues) ]) :-
   all(Object:Relation:Value,
       ( declare_related(Object, Relation, Value),
	 relation_type(Relation, ObjectType, ValueType),
	 \+ ( is_type(Object, ObjectType),
	      is_type(Value, ValueType) ) ),
       InvalidValues).

test(integrity(implied_relations_must_be_defined),
     [ problem_list("The following relations are undeclared",
		    UndeclaredRelations) ]) :-
   all(S,
       ( implies_relation(_R, S),
	 \+ relation_type(S, _, _) ),
       UndeclaredRelations).

test(integrity(implied_relations_must_be_type_consistent),
     [ problem_list("Relation is declared to have a generalization with incompatible types",
		    UndeclaredRelations) ]) :-
   all((R -> S),
       ( implies_relation(R, S),
	 \+ ( relation_type(R, RTO, RTR),
	      relation_type(S, STO, STR),
	      kind_of(RTO, STO),
	      kind_of(RTR, STR) ) ),
       UndeclaredRelations).

test(integrity(intransitive_verb_semantics_defined),
     [problem_list("The following undefined predicates or attributes appear in word definitions",
		    UndefinedPredicates) ]) :-
   all(Spec:Phrase,
       ( iv(past_participle, _, Semantics, _, _, Phrase, [ ]),
	 lambda_contains_undefined_predicate(Semantics, Spec) ),
       UndefinedPredicates).

test(integrity(transitive_verb_semantics_defined),
     [ problem_list("The following undefined predicates or attributes appear in word definitions",
		    UndefinedPredicates) ]) :-
   all(Spec:Phrase,
       ( tv(past_participle, _, Semantics, _, _, Phrase, [ ]),
	 lambda_contains_undefined_predicate(Semantics, Spec) ),
       UndefinedPredicates).

test(integrity(ditransitive_verb_semantics_defined),
     [problem_list("The following undefined predicates or attributes appear in word definitions",
		    UndefinedPredicates) ]) :-
   all(Spec:Phrase,
       ( dtv(past_participle, _, Semantics, _, _, Phrase, [ ]),
	 lambda_contains_undefined_predicate(Semantics, Spec) ),
       UndefinedPredicates).

test(integrity(adjective_semantics_defined),
     [ problem_list("The following undefined predicates or attributes appear in word definitions",
		    UndefinedPredicates) ]) :-
   all(Spec:Word,
       ( adjective(Word, Semantics),
	 lambda_contains_undefined_predicate(Semantics, Spec) ),
       UndefinedPredicates).

test(entities_have_required_properties,
     [ problem_list("The following entities are missing values for required properties",
		    ProblematicEntities) ]) :-
   all(Entity:MissingProperty,
       ( kind(Kind),
	 property_value(Kind, required_properties, Required),
	 is_a(Entity, Kind),
	 member(MissingProperty, Required),
	 \+ property_value(Entity, MissingProperty, _) ),
       ProblematicEntities).

test(entities_have_required_relations,
     [ problem_list("The following entities are missing required relations",
		    ProblematicEntities) ]) :-
   all(Entity:MissingRelation,
       ( kind(Kind),
	 property_value(Kind, required_relation, Required),
	 is_a(Entity, Kind),
	 member(MissingRelation, Required),
	 \+ related(Entity, MissingRelation, _) ),
       ProblematicEntities).

lambda_contains_undefined_predicate(_^P, Spec) :-
   !,
   lambda_contains_undefined_predicate(P, Spec).
lambda_contains_undefined_predicate(related(_, Relation, _),
				    relation(Relation)) :-
   !,
   \+ relation_type(Relation, _, _).
lambda_contains_undefined_predicate(property_value(_, Property, _),
				    property(Property)) :-
   !,
   \+ property_type(Property, _, _).
lambda_contains_undefined_predicate(P,Name/Arity) :-
   functor(P, Name, Arity),
   functor(Copy, Name, Arity),
   \+ predicate_type(_, Copy).