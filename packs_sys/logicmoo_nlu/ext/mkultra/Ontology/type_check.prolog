%% possibile_types_given_constraint(-Var, :Expression, -Types)
%  Gives all the possible types for Var within Expression.
possible_types_given_constraint(Var, Expression, Types) :-
   all(Type,
       variable_type_given_constraint(Var, Expression, Type),
       Types).

%% possibile_types_given_constraint(-Var, :Expression, -Types)
%  Gives all the possible types for Var within Expression.
variable_type_given_constraint(Object, _, Type) :-
   nonvar(Object),
   is_a(Object, Type).
variable_type_given_constraint(Var, Expression, Type) :-
   well_typed(Expression, _, Bindings),
   lookup_variable_type(Var, Type, Bindings).

well_typed(Expression, Kind) :-
   well_typed(Expression, Kind, _).

%% well_typed(=Expression, ?Kind, -Bindings)
%  Expression is well typed as of type Kind.
%  Types of any free variables of it are given by Bindings.
well_typed(Object, Kind, Bindings) :-
   well_typed(Object, Kind, [ ], Bindings).

%% well_typed(=Expression, ?Kind, +BindingsIn, -BindingsOut)
%  Expression is well typed and of type Kind
%  given the variable:type bindings given in BindingsIn.
%  Further, BindingsOut gives the complete list of bindings
%  for free variables in Expression.
well_typed(Var, Kind, BIn, BOut) :-
   var(Var),
   !,
   variable_well_typed(Var, Kind, BIn, BOut).

well_typed(Atom, Type, Bindings, Bindings) :-
   atomic(Atom),
   !,
   is_type(Atom, Type).

well_typed((Expression, is_a(Var, VKind)), Kind, BIn, BOut) :-
   well_typed(Expression, Kind, [Var:VKind | BIn], BOut).

well_typed(related(Object, Relation, Relatum), condition, BIn, BOut) :-
   nonvar(Relation),
   relation_type(Relation, ObjectType, RelatumType),
   well_typed(Object, ObjectType, BIn, BIntermediate),
   well_typed(Relatum, RelatumType, BIntermediate, BOut).

well_typed(property_value(Object, Property, Value), condition, BIn, BOut) :-
   nonvar(Property),
   property_type(Property, ObjectType, ValueType),
   well_typed(Object, ObjectType, BIn, BIntermediate),
   well_typed(Value, ValueType, BIntermediate, BOut).

well_typed(Event, Kind, BindingsIn, BindingsOut) :-
   Event =.. [Functor | ActualArgs],
   copy_list_as_variables(ActualArgs, ArgTypes),
   TypeDecl =.. [Functor | ArgTypes],
   predicate_type(Kind, TypeDecl),
   well_typed_arguments(ActualArgs, ArgTypes, BindingsIn, BindingsOut).

well_typed_arguments([], [], Bindings, Bindings).
well_typed_arguments([Arg | Args], [Type | Types], BIn, BOut) :-
   well_typed(Arg, Type, BIn, BIntermediate),
   well_typed_arguments(Args, Types, BIntermediate, BOut).

copy_list_as_variables([], []).
copy_list_as_variables([_ | T1], [_ | T2]) :-
   copy_list_as_variables(T1, T2).

variable_well_typed(V, Kind, BIn, BOut) :-
   lookup_variable_type(V, PreviousKind, BIn),
   !,
   variable_well_typed(V, Kind, PreviousKind, BIn, BOut).
variable_well_typed(V, Kind, B, [V:Kind | B]).  % haven't seen this var before.

variable_well_typed(V, Kind, entity, B, [V:Kind | B]) :-
   !.
variable_well_typed(_V, Kind, PreviousKind, B, B) :-
   kind_of(PreviousKind, Kind),  % We already have a type that's at least as specific.
   !.
variable_well_typed(V, Kind, PreviousKind, B, [V:Kind | B]) :-
   kind_of(Kind, PreviousKind),  % Kind is a more specific type. 
   !.

lookup_variable_type(Var, Type, [Var:Type | _]) :-
   !.
lookup_variable_type(Var, Type, [_ | Tail]) :-
   lookup_variable_type(Var, Type, Tail).
