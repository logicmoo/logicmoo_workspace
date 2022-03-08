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
	 decode_kind_names(SingularSpec, [Kind], Singular),
	 %(([DefPlural | _] = Singular) ; DefPlural = []),
	 decode_kind_names(PluralSpec, [], Plural),
   log(assert_kind_nouns(Kind, Singular, Plural)),
	 assert_kind_nouns(Kind, Singular, Plural),
	 assert_if_unew(declare_kind(Kind, kind)),
	 parse_list(Prop=Value, DefaultProperties,
		    assert_if_unew(default_value(Kind, Prop, Value)),
		    BadElement,
		    kind_declaration_syntax_error(Kind, row:RowNumber,
						  default_property_list:BadElement)),
	 parse_list(Relation:Relatum, DefaultRelations,
		    assert_if_unew(default_related(Kind, Relation, Relatum)),
		    BadElement,
		    kind_declaration_syntax_error(Kind, row:RowNumber,
						  default_relation_list:BadElement)),
	 parse_list(Prop=Value, ClassProperties,
		    assert_if_unew(declare_value(Kind, Prop, Value)),
		    BadElement,
		    kind_declaration_syntax_error(Kind, row:RowNumber,
						  class_property_list:BadElement)),
	 parse_list(Relation:Relatum, ClassRelations,
		    assert_if_unew(declare_related(Kind, Relation, Relatum)),
		    BadElement,
		    kind_declaration_syntax_error(Kind, row:RowNumber,
						  class_relation_list:BadElement))).

assert_default_description(_, null).
assert_default_description(Kind, Description) :-
   assert_if_unew(default_value(Kind, description, Description)).

decode_kind_names([[-]], _, []).
decode_kind_names([[]], Default, [Default]).
decode_kind_names([], Default, [Default]).
decode_kind_names(Names, _, Names).

atom_len_gt2(R): atom_length(R,L),L>2.
pluralize([B|S],[B|P]):- S\==[], !, pluralize(S,P).
pluralize([S],[P]):- !, pluralize(S,P).
pluralize(S,P):- atom(S),plur(SF,L,PF),atom_concat(R,SF,S),atom_length(R,AL),AL>L,!,atom_concat(R,PF,P).
pluralize(S,P):- atom(P),plur(SF,L,PF),atom_concat(R,PF,P),atom_length(R,AL),AL>L,!,atom_concat(R,SF,S).
plur('sis',2,'ses'). plur('xis',0,'xes').   plur('us',2,'i').    plur('ss',0,'sses'). plur('s',0,'s').
plur('fe',0,'ves').  plur('f',0,'ves').     plur('o',0,'oes').   plur('x',0,'xes').   plur('um',2,'a').
plur('on',2,'a').    plur('elf',0,'elves'). plur('y',0,'ies',P). plur('h',0,'hes',P). plur('',2,'s').

assert_kind_nouns(Kind, [Nil], Pls):-  Nil == [], !, assert_kind_nouns(Kind, [], Pls).
assert_kind_nouns(Kind, Sings, [Nil]):- Nil == [], !,  assert_kind_nouns(Kind, Sings, []).
assert_kind_nouns(Kind, [], Pls):- !, 
  assert_kind_nouns(Kind, [Kind], Pls).
assert_kind_nouns(Kind, Singulars, []):- % Singulars\==[],
  maplist(pluralize,Singulars,Plurals), !,
  assert_kind_nouns(Kind, Singulars, Plurals).
assert_kind_nouns(Kind, Singulars, Plurals) :-
   begin(forall(member(Phrase, Singulars),
		assert_phrase_rule(kind_noun(Kind, singular), Phrase)),
	 forall(member(Phrase, Plurals),
		assert_phrase_rule(kind_noun(Kind, plural), Phrase))).

define_kind(RowNumber, Kind, _) :-
   kind(Kind),
   log(warn(throw(error(row:RowNumber:kind_already_defined:Kind)))),fail.
define_kind(RowNumber, Kind, [ ]) :-
   Kind \= entity,
   throw(error(row:RowNumber:kind_has_no_parents:Kind)).
define_kind(_, Kind, Parents) :-
   assert_if_unew(kind(Kind)),
   forall(member(P, Parents),
	  assert_if_unew(immediate_kind_of(Kind, P))).

end_csv_loading(kinds) :-
   % Find all the leaf kinds
   forall((kind(K), \+ immediate_kind_of(_, K)),
	   assert_if_unew(leaf_kind(K))).

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
:- dynamic(property_type/3).

load_special_csv_row(_RowNumber, properties(Name, Visibility,
					    SurfaceForm,
					    ObjectType, ValueType)) :-
   assert_if_unew(declare_kind(Name, property)),
   assert_if_unew(visibility(Name, Visibility)),
   assert_if_unew(property_type(Name, ObjectType, ValueType)),
   assert_phrase_rule(property_name(Name), SurfaceForm).

%%
%% Relations
%%
:- dynamic(symmetric/1).

load_special_csv_row(_RowNumber,
		     relations(Name, Visibility,
			       ObjectType, ValueType,
			       CopularForm,
			       SingularForm,
			       PluralForm,
			       Generalizations,
			       Inverse,
			       Symmetric)) :-
   begin(assert_if_unew(declare_kind(Name, relation)),
	 assert_if_unew(visibility(Name, Visibility)),
	 assert_if_unew(relation_type(Name, ObjectType, ValueType)),
	 assert_copular_form(Name, CopularForm),
	 assert_genitive_form(Name, singular, SingularForm),
	 assert_genitive_form(Name, plural, PluralForm),
	 forall(member(Gen, Generalizations),
		assert_if_unew(implies_relation(Name, Gen))),
	 (Inverse \= null -> assert_if_unew(inverse_relation(Name, Inverse)) ; true),
	 (Symmetric \= null -> assert_if_unew(symmetric(Name)) ; true)).

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
member_pn(E,[H|T]):- is_list(H),!,member(E,[H|T]).
member_pn(E,E):- E\==[], is_list(E).

load_special_csv_row(_RowNumber,
		     entities(EntityName, KindList,
			      Description,
			      ProperNames, GramaticalNumber,
			      PropertyList, RelationList)) :-
   assert_description(EntityName, Description),
   forall(member(Kind, KindList),
	  assert_if_unew(declare_kind(EntityName, Kind))),
   forall(member_pn(ProperName, ProperNames),
	  assert_proper_name(EntityName, ProperName, GramaticalNumber)),
   forall(member(PropertyName=Value, PropertyList),
	  assert_if_unew(declare_value(EntityName, PropertyName, Value))),
   forall(member(RelationName:Relatum, RelationList),
	  assert_if_unew(declare_related(EntityName, RelationName, Relatum))).

assert_description(_, null).
assert_description(Entity, Description) :-
   assert_if_unew(declare_value(Entity, description, Description)).

parse_list(Pattern, List, Goal, ListElement, ErrorMessage) :-
   forall(member(ListElement, List),
	  ( ListElement=Pattern ->
	       Goal
	       ;
	       throw(error(ErrorMessage)) )).
