%%
%% Rules for loading the KB from spreadsheets
%% These start as sheets of KB.xslm and get exported as separate .csv files.
%%

%%
%% Kinds
%%

load_special_csv_row(RowNumber,
		     kinds(Kind, Parents,
			   Description,
			   SingularSpec, PluralSpec,
			   DefaultProperties,
			   DefaultRelations,
			   ClassProperties,
			   ClassRelations)) :-
   begin(define_kind(RowNumber, Kind, Parents),
	 assert_default_description(Kind, Description),
	 decode_kind_name(SingularSpec, [Kind], Singular),
	 decode_kind_name(PluralSpec, Singular, Plural),
	 assert_kind_noun(Kind, Singular, Plural),
	 assert(declare_kind(Kind, kind)),
	 parse_list(Prop=Value, DefaultProperties,
		    assert(default_value(Kind, Prop, Value)),
		    BadElement,
		    kind_declaration_syntax_error(Kind, row:RowNumber,
						  default_property_list:BadElement)),
	 parse_list(Relation:Relatum, DefaultRelations,
		    assert(default_related(Kind, Relation, Relatum)),
		    BadElement,
		    kind_declaration_syntax_error(Kind, row:RowNumber,
						  default_relation_list:BadElement)),
	 parse_list(Prop=Value, ClassProperties,
		    assert(declare_value(Kind, Prop, Value)),
		    BadElement,
		    kind_declaration_syntax_error(Kind, row:RowNumber,
						  class_property_list:BadElement)),
	 parse_list(Relation:Relatum, ClassRelations,
		    assert(declare_related(Kind, Relation, Relatum)),
		    BadElement,
		    kind_declaration_syntax_error(Kind, row:RowNumber,
						  class_relation_list:BadElement))).

assert_default_description(_, null).
assert_default_description(Kind, Description) :-
   assert(default_value(Kind, description, Description)).

decode_kind_name([-], _, []).
decode_kind_name([], Default, Default).
decode_kind_name(Name, _, Name).

assert_kind_noun(_, _, []).
assert_kind_noun(Kind, Singular, Plural) :-
   assert_phrase_rule(kind_noun(Kind, singular), Singular),
   assert_phrase_rule(kind_noun(Kind, plural), Plural).

define_kind(RowNumber, Kind, _) :-
   kind(Kind),
   throw(error(row:RowNumber:kind_already_defined:Kind)).
define_kind(RowNumber, Kind, [ ]) :-
   Kind \= entity,
   throw(error(row:RowNumber:kind_has_no_parents:Kind)).
define_kind(_, Kind, Parents) :-
   assert(kind(Kind)),
   forall(member(P, Parents),
	  assert(immediate_kind_of(Kind, P))).

end_csv_loading(kinds) :-
   % Find all the leaf kinds
   forall((kind(K), \+ immediate_kind_of(_, K)),
	   assert(leaf_kind(K))).

end_csv_loading(predicate_type) :-
   forall(predicate_type(Type, ArgTypes),
	  check_predicate_signature(Type, ArgTypes)).

check_predicate_signature(Type, ArgTypes) :-
   \+ kind(Type),
   log(bad_declared_type(ArgTypes, Type)).
check_predicate_signature(_Type, ArgTypes) :-
   ArgTypes =.. [_Functor | Types],
   forall(member(AType, Types),
	  ((kind(AType),!) ; log(bad_declared_argument_type(AType, ArgTypes)))).

%%
%% Properties
%%

load_special_csv_row(_RowNumber, properties(Name, Visibility,
					    SurfaceForm,
					    ObjectType, ValueType)) :-
   assert(declare_kind(Name, property)),
   assert(visibility(Name, Visibility)),
   assert(property_type(Name, ObjectType, ValueType)),
   assert_phrase_rule(property_name(Name), SurfaceForm).

%%
%% Relations
%%

load_special_csv_row(_RowNumber,
		     relations(Name, Visibility,
			       ObjectType, ValueType,
			       CopularForm,
			       SingularForm,
			       PluralForm,
			       Generalizations,
			       Inverse)) :-
   assert(declare_kind(Name, relation)),
   assert(visibility(Name, Visibility)),
   assert(relation_type(Name, ObjectType, ValueType)),
   assert_copular_form(Name, CopularForm),
   assert_genitive_form(Name, singular, SingularForm),
   assert_genitive_form(Name, plural, PluralForm),
   forall(member(Gen, Generalizations),
	  assert(implies_relation(Name, Gen))),
   (Inverse \= null -> assert(inverse_relation(Name, Inverse)) ; true).

assert_copular_form(_Name, [ ]).
assert_copular_form(Name, [be | CopularForm]) :-
   assert_phrase_rule(copular_relation(Name), CopularForm).
assert_copular_form(Name, CopularForm) :-
   % Copular forms must start with the word "be"
   % (the English copula is the verb "to be").
   log(malformed_copular_form_of_relation(Name, CopularForm)).

assert_genitive_form(_Name, _Number, []).
assert_genitive_form(Name, Number, Phrase) :-
   assert_phrase_rule(genitive_form_of_relation(Name, Number), Phrase).

%%
%% Entities
%%

load_special_csv_row(_RowNumber,
		     entities(EntityName, KindList,
			      Description,
			      ProperName, GramaticalNumber,
			      PropertyList, RelationList)) :-
   assert_description(EntityName, Description),
   forall(member(Kind, KindList),
	  assert(declare_kind(EntityName, Kind))),
   assert_proper_name(EntityName, ProperName, GramaticalNumber),
   forall(member(PropertyName=Value, PropertyList),
	  assert(declare_value(EntityName, PropertyName, Value))),
   forall(member(RelationName:Relatum, RelationList),
	  assert(declare_related(EntityName, RelationName, Relatum))).

assert_description(_, null).
assert_description(Entity, Description) :-
   assert(declare_value(Entity, description, Description)).

parse_list(Pattern, List, Goal, ListElement, ErrorMessage) :-
   forall(member(ListElement, List),
	  ( ListElement=Pattern ->
	       Goal
	       ;
	       throw(error(ErrorMessage)) )).
