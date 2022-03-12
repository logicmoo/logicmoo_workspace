%%
%% Rules for loading the KB from spreadsheets
%% These start as sheets of KB.xslm and get exported as separate .csv files.
%%

%%
%% Kinds
%%



%=autodoc
%% load_special_csv_row( ?RowNumber, ?Base) is semidet.
%
% Load Special Csv Row.
%
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



%=autodoc
%% assert_default_description( ?ARG1, ?ARG2) is semidet.
%
% Assert Default Description.
%
assert_default_description(_, null).
assert_default_description(Kind, Description) :-
   assert_if_unew(default_value(Kind, description, Description)).



%=autodoc
%% decode_kind_names( ?ARG1, ?ARG2, ?ARG3) is semidet.
%
% Decode Kind Names.
%
decode_kind_names([[-]], _, []).
decode_kind_names([[]], Default, [Default]).
decode_kind_names([], Default, [Default]).
decode_kind_names(Names, _, Names).



%=autodoc
%% pluralize( ?B, ?B) is semidet.
%
% Pluralize.
%
pluralize([B|S],[B|P]):- S\==[], !, pluralize(S,P).
pluralize([S],[P]):- !, pluralize(S,P).
pluralize(die,dice).    pluralize(ox,oxen). 
pluralize(S,P):- atom(S),plur(SF,L,PF),atom_concat(R,SF,S),atom_length(R,AL),AL>=L,!,atom_concat(R,PF,P).
pluralize(S,P):- atom(P),plur(SF,L,PF),atom_concat(R,PF,P),atom_length(R,AL),AL>=L,!,atom_concat(R,SF,S).


%=autodoc
%% plur( ?ATOM1, :GoalGOAL2, ?ATOM3) is semidet.
%
% Plur.
%
plur(foot,0,feet).   plur(tooth,0,teeth).   plur(goose,0,geese).   plur(child,0,children).
plur(woman,0,women). plur(mouse,0,mice).    plur(index,0,indices). plur(man,0,men).    
plur('sis',2,'ses'). plur('xis',1,'xes').   plur('us',2,'i').      plur('ss',1,'sses').
plur('ef',2,'efs').  plur('ff',2,'ffs').    plur('ife',1,'ives').  plur('if',1,'ives'). plur('elf',1,'elves'). 
plur('o',1,'oes').   plur('x',1,'xes').     plur('um',2,'a').      plur('on',2,'a').    
plur('y',1,'ies').   plur('h',3,'hes').     plur('z',2,'zes').     plur('s',2,'ses'). 
plur('',3,'s').      plur('s',0,'s').       plur('',0,'s').



%=autodoc
%% assert_kind_nouns( +Kind, ?Nil, ?Pls) is semidet.
%
% Assert Kind Nouns.
%
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



%=autodoc
%% define_kind( ?RowNumber, +Kind, ?ARG3) is semidet.
%
% Define Kind.
%
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



%=autodoc
%% end_csv_loading( ?Intransitive_verb) is semidet.
%
% End Csv Loading.
%
end_csv_loading(kinds) :-
   % Find all the leaf kinds
   forall((kind(K), \+ immediate_kind_of(_, K)),
	   assert_if_unew(leaf_kind(K))).

end_csv_loading(predicate_type) :-
   forall(predicate_type(Type, ArgTypes),
	  check_predicate_signature(Type, ArgTypes)).



%=autodoc
%% check_predicate_signature( ?Type, ?ArgTypes) is semidet.
%
% Check Predicate Signature.
%
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

load_special_csv_row( _RowNumber, 
  relations( Name, 
    Visibility, ObjectType, ValueType, 
    CopularForm, SingularForm, 
    PluralForm, Generalizations, 
    Inverse, Symmetric)) :-  
  begin( assert_if_unew(declare_kind(Name, relation)), 
    assert_if_unew(visibility(Name, Visibility)), 
    assert_if_unew(mpred_argtypes(t(Name, ObjectType, ValueType))), 
    assert_copular_form(Name, CopularForm), 
    assert_genitive_form(Name, singular, SingularForm), 
    assert_genitive_form(Name, plural, PluralForm), 
    forall( member(Gen, Generalizations), 
      assert_if_unew(implies_relation(Name, Gen))), 
    Inverse\=null->assert_if_unew(inverse_relation(Name, Inverse));true, 
    Symmetric\=null->assert_if_unew(symmetric(Name));true).



%=autodoc
%% assert_copular_form( ?Name, ?[]) is semidet.
%
% Assert Copular Form.
%
assert_copular_form(_Name, [ ]).
assert_copular_form(Name, [be | CopularForm]) :-
   assert_phrase_rule(copular_relation(Name), CopularForm).
assert_copular_form(Name, CopularForm) :-
   % Copular forms must start with the word "be"
   % (the English copula is the verb "to be").
   log(malformed_copular_form_of_relation(Name, CopularForm)).



%=autodoc
%% assert_genitive_form( ?Name, ?Number, ?[]) is semidet.
%
% Assert Genitive Form.
%
assert_genitive_form(_Name, _Number, []).
assert_genitive_form(Name, Number, Phrase) :-
   assert_phrase_rule(genitive_form_of_relation(Name, Number), Phrase).

%%
%% Entities
%%
member_pn(E,[H|T]):- is_list(H),!,member(E,[H|T]).

%=autodoc
%% member_pn( ?E, ?H) is semidet.
%
% Member Pn.
%

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
  forall(member(PropertyName:Value, PropertyList),
   assert_if_unew(declare_value(EntityName, PropertyName, Value))),
  forall(member(RelationName=Relatum, RelationList),
   assert_if_unew(declare_related(EntityName, RelationName, Relatum))),
   forall(member(RelationName:Relatum, RelationList),
	  assert_if_unew(declare_related(EntityName, RelationName, Relatum))).



%=autodoc
%% assert_description( ?ARG1, ?ARG2) is semidet.
%
% Assert Description.
%
assert_description(_, null).
assert_description(Entity, Description) :-
   assert_if_unew(declare_value(Entity, description, Description)).



%=autodoc
%% parse_list( ?Pattern, ?List, :GoalGoal, ?ListElement, ?ErrorMessage) is semidet.
%
% Parse List.
%
parse_list(Pattern, List, Goal, ListElement, ErrorMessage) :-
   forall(member(ListElement, List),
	  ( ListElement=Pattern ->
	       Goal
	       ;
	       throw(error(ErrorMessage)) )).
