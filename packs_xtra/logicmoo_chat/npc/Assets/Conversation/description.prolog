%%
%% Describing objects
%%



%=autodoc
%% normalize_task( ?Status, ?Task) is semidet.
%
% Normalize Task.
%
normalize_task(describe(X),
	       describe(X, general, null)).

strategy(describe(Object, Purpose, NullContinuation),
	 begin(preface_description(Object),
	       describe_attributes(Object, Attributes, NullContinuation))) :-
   all(Attribute,
       interesting_attribute($addressee, Purpose, Object, Attribute),
       AllAttributes),
   remove_redundant_attributes(AllAttributes, Attributes).

default_strategy( preface_description(Object), 
  if( t(description, Object, Description), 
    monolog(Description), 
    describe_type(Object))).

strategy(describe_type(Object),
	 let(base_kind(Object, Kind),
	     say_answer(iz_a(Object, Kind)))).

%%
%% Describing lists of attributes
%%

strategy(describe_attributes(Object, Attributes, NullContinuation),
	 if(Attributes=[ ],
	    NullContinuation,
	    generate_list(Attributes, attribute_of(Object)))).

strategy(generate_next(Property:Value, attribute_of(Object)),
	 describe_property("", Object, Property, Value, ", ...")).
strategy(generate_last(Property:Value, attribute_of(Object)),
	 describe_property("and", Object, Property, Value, ".")).

strategy(generate_next(Property/Value, attribute_of(Object)),
	 describe_relation("", Object, Property, Value, ", ...")).
strategy(generate_last(Property/Value, attribute_of(Object)),
	 describe_relation("and", Object, Property, Value, ".")).

strategy( 
   describe_property(Linkage, Object, Property, Value, Termination), 
   speech([Linkage, Surface, Termination])) :- 
  surface_form(t(Property, Object, Value), Surface), 
  tell(/mentioned_to/ $addressee/Object/Property:Value).

strategy( 
   describe_relation(Linkage, Object, Relation, Relatum, Termination), 
   speech([Linkage, Surface, Termination])) :- 
  surface_form(t(Relation, Object, Relatum), Surface), 
  forall( ancestor_relation(A, Relation), 
    tell(/mentioned_to/ $addressee/Object/A/Relatum)).



%% surface_form( ?Property, ?Form) is semidet.
%
% Surface Form.
%
surface_form( t(Property, Object, Value), 
  question_answer(t(Property, Object, Value))).

surface_form( t(Relation, Object, Relatum), 
  question_answer(t(Relation, Object, Relatum))).

%%
%% Determining lists of relevant attributes
%%



%=autodoc
%% remove_redundant_attributes( ?ARG1, ?ARG2) is semidet.
%
% Remove Redundant Attributes.
%
remove_redundant_attributes([ ], [ ]).
remove_redundant_attributes([Relation/Relatum | Rest], RestRemoved) :-
   decendant_relation(Antecedant, Relation),
   member(Antecedant/Relatum, Rest),
   !,
   remove_redundant_attributes(Rest, RestRemoved).
remove_redundant_attributes([X | Rest], [X | Final]) :-
   remove_implicants(X, Rest, WithoutImplicants),
   remove_redundant_attributes(WithoutImplicants, Final).



%=autodoc
%% remove_implicants( ?ARG1, ?ARG2, ?ARG3) is semidet.
%
% Remove Implicants.
%
remove_implicants(_, [ ], [ ]).
remove_implicants(Rel/Object, [Implicant/Object | Rest], Rest) :-
   ancestor_relation(Implicant, Rel),
   !.
remove_implicants(Attribute, [X | Rest] , [X | RestRemoved]) :-
   remove_implicants(Attribute, Rest, RestRemoved).



%=autodoc
%% interesting_attribute( ?Listener, ?Purpose, ?Object, ?Attribute) is semidet.
%
% Interesting Attribute.
%
interesting_attribute(Listener, Purpose, Object, Attribute) :-
   interesting_property(Listener, Purpose, Object, Attribute)
   ;
   interesting_relation(Listener, Purpose, Object, Attribute).

:- dynamic(visibility/2).



%=autodoc
%% interesting_property( ?Listener, ?Purpose, ?Object, +Prop) is semidet.
%
% Interesting Property.
%
interesting_property(Listener, Purpose, Object, Prop:Value) :- 
  property_nondefault_value(Object, Prop, Value), 
  \+ /mentioned_to/ $addressee/Object/Prop:Value, 
  \+visibility(Prop, internal), 
  property_relevant_to_purpose(Purpose, Object, Prop, Value), 
  admitted_truth_value(Listener, t(Prop, Object, Value), true).



%=autodoc
%% interesting_relation( ?Listener, ?Purpose, ?Object, ?Relation) is semidet.
%
% Interesting Relation.
%
interesting_relation(Listener, Purpose, Object, Relation/Relatum) :- 
  related_nondefault(Object, Relation, Relatum), 
  \+ /mentioned_to/ $addressee/Object/Relation/Relatum, 
  \+visibility(Relation, internal), 
  relation_relevant_to_purpose(Purpose, Object, Relation, Relatum), 
  admitted_truth_value(Listener, t(Relation, Object, Relatum), true).


