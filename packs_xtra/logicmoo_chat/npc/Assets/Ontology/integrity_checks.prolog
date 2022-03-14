

%=autodoc
%% known_object( ?Object) is semidet.
%
% Known Object.
%
known_object(Object) :-
   for_all_unique(Object,
		  declared_kind(Object, _),
		  true).



%=autodoc
%% known_type( ?String1) is semidet.
%
% Known Type.
%
known_type(number).
known_type(string).
known_type(List) :-
   is_list(List).
known_type(kind_of(Kind)) :-
   kind(Kind).
known_type(subkind_of(Kind)) :-
   kind(Kind).
known_type(Kind) :-
   kind(Kind).



%=autodoc
%% known_property( ?Property) is semidet.
%
% Known Property.
%
known_property(Property) :-
   for_all_unique(Property, property_type(Property, _, _)).



%=autodoc
%% known_relation( ?Relation) is semidet.
%
% Known Relation.
%
known_relation(Relation) :-  
  for_all_unique(Relation, mpred_argtypes(t(Relation, _, _))).



%=autodoc
%% test( ?Property_declarations_well_formed, ?The following properties specify unknown types in the spreadsheet) is semidet.
%
% Test.
%
test(integrity(property_declarations_well_formed),
     [ problem_list("The following properties specify unknown types in the spreadsheet",
		    Malformed) ]) :-
   all(Property,
       ( property_type(Property, ObjectType, ValueType),
	 \+ ( known_type(ObjectType),
	      known_type(ValueType) ) ),
       Malformed).

test( integrity(properties_have_types_declared), [
  problem_list("The following properties have no type listed in the spreadsheet", UndeclaredProperties)]) :-  
   all(Property,
    (t(Property, _, _), \+property_type(Property, _, _)), 
       UndeclaredProperties).

test( integrity(valid_property_types), [
  problem_list("The following objects have properties with invalid types", InvalidValues)]) :-  
  all( 
     o_p_v(ObjectType:Object, Property, ValueType:Value), 
     ( t(Property, Object, Value)  ,
	 property_type(Property, ObjectType, ValueType),
       \+ (is_type(Object, ObjectType), is_type(Value, ValueType))), 
       InvalidValues).

test( integrity(relation_declarations_well_formed), [
  problem_list("The following relations have invalid types in the spreadsheet", Malformed)]) :-  
  all( Relation, 
    ( mpred_argtypes(t(Relation, ObjectType, ValueType)), 
      \+ (known_type(ObjectType), known_type(ValueType))), 
    Malformed).

test( integrity(relations_declared), [
  problem_list("The following relations are not declared in the spreadsheet", UndeclaredRelations)]) :-  
  all( Relation, 
    ( asserted_t(Relation, _, _),
      \+mpred_argtypes(t(Relation, _, _))), 
    UndeclaredRelations).

test( integrity(inverse_relations_must_be_declared_in_their_canonical_form), [
  problem_list( "The following objects declare relations that should be declared through their inverses instead", 
    Malformed)]) :-  
  all( t(R, X, Y), 
    (asserted_t(R, X, Y), inverse_relation(R, _)), 
    Malformed).

test( integrity(valid_relation_types), [
  problem_list("The following objects declare relations with the wrong types", InvalidValues)]) :- 
  DR=t(Relation, Object, Value), 
  all( DR, 
    ( DR  , \+ valid_relation_data(Relation, Object, Value) ), 
    InvalidValues).

valid_relation_data(Relation, Object, Value):-
    mpred_argtypes(t(Relation, ObjectType, ValueType)), 
    is_type(Object, ObjectType), is_type(Value, ValueType).
valid_relation_data(Relation, _Object, _Value):- \+ mpred_argtypes(t(Relation, _, _)) , !.

test( integrity(implied_relations_must_be_defined), [
  problem_list("The following relations are undeclared", UndeclaredRelations)]) :-  
  all( S, 
    (implies_relation(_R, S), \+mpred_argtypes(t(S, _, _))), 
    UndeclaredRelations).

test( integrity(implied_relations_must_be_type_consistent), [
  problem_list("Relation is declared to have a generalization with incompatible types", UndeclaredRelations)]) :-  
  all( implies_relation(R, S), 
    ( implies_relation(R, S), 
      \+( ( mpred_argtypes(t(R, RTO, RTR))  ,
            mpred_argtypes(t(S, STO, STR)), 
            kind_of(RTO, STO), 
            kind_of(RTR, STR)))), 
    UndeclaredRelations).

test(integrity(intransitive_verb_semantics_defined),
     [problem_list("The following undefined predicates or attributes appear in word definitions",
		    UndefinedPredicates) ]) :-
   all(Spec:Semantics:Phrase,
       ( iv(past_participle, _, Semantics, _, _, Phrase, [ ]),
	 lambda_contains_undefined_predicate(Semantics, Spec) ),
       UndefinedPredicates).

test(integrity(transitive_verb_semantics_defined),
     [ problem_list("The following undefined predicates or attributes appear in word definitions",
		    UndefinedPredicates) ]) :-
   all(Spec:Semantics:Phrase,
       ( tv(past_participle, _, Semantics, _, _, Phrase, [ ]),
	 lambda_contains_undefined_predicate(Semantics, Spec) ),
       UndefinedPredicates).

test(integrity(ditransitive_verb_semantics_defined),
     [problem_list("The following undefined predicates or attributes appear in word definitions",
		    UndefinedPredicates) ]) :-
   all(Spec:Semantics:Phrase,
       ( dtv(past_participle, _, Semantics, _, _, Phrase, [ ]),
	 lambda_contains_undefined_predicate(Semantics, Spec) ),
       UndefinedPredicates).

test(integrity(adjective_semantics_defined),
     [ problem_list("The following undefined predicates or attributes appear in word definitions",
		    UndefinedPredicates) ]) :-
   all(Spec+adjective(Semantics, Phrase, []),
       ( adjective(Semantics, Phrase, []),
	 lambda_contains_undefined_predicate(Semantics, Spec) ),
       UndefinedPredicates).

test( entities_have_required_properties, [
  problem_list("The following entities are missing values for required properties", ProblematicEntities)]) :-  
  all( Entity:MissingProperty, 
    ( kind(Kind)  ,
      t(required_properties, Kind, Required), 
      iz_a(Entity, Kind), 
      member(MissingProperty, Required), 
      \+t(MissingProperty, Entity, _)), 
    ProblematicEntities).

test( entities_have_required_relations, [
  problem_list("The following entities are missing required relations", ProblematicEntities)]) :-  
  all( Entity:Required:MissingRelation, 
    ( kind(Kind)  ,
      t(required_relation, Kind, Required), 
      iz_a(Entity, Kind), 
      member(MissingRelation, Required), 
      \+t(MissingRelation, Entity, _)), 
    ProblematicEntities).



%=autodoc
%% lambda_contains_undefined_predicate( ?P, -O) is semidet.
%
% Lambda Contains Undefined Predicate.
%
lambda_contains_undefined_predicate([P],O) :- !, lambda_contains_undefined_predicate(P,O).
lambda_contains_undefined_predicate(P,O) :- !, is_list(P), maplist(lambda_contains_undefined_predicate,P,O).

lambda_contains_undefined_predicate(_^P, Spec) :-
   !,
   lambda_contains_undefined_predicate(P, Spec).
lambda_contains_undefined_predicate(t(Relation, _, _), relation(Relation)) :-  !, 
  \+mpred_argtypes(t(Relation, _, _)).
lambda_contains_undefined_predicate(t(Property, _, _), property(Property)) :-  !, 
  \+property_type(Property, _, _).
lambda_contains_undefined_predicate(P,Name/Arity) :-
   functor(P, Name, Arity),
   functor(Copy, Name, Arity),
   \+ predicate_type(_, Copy).

