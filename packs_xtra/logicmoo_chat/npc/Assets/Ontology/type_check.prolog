%% possibile_types_given_constraint(-Var, :Expression, -Types)
% Gives all the possible types for Var within Expression.
possible_types_given_constraint(Var, Expression, Types) :-
  all(Type,
    variable_type_given_constraint(Var, Expression, Type),
    Types).

%% possibile_types_given_constraint(-Var, :Expression, -Types)
% Gives all the possible types for Var within Expression.
variable_type_given_constraint(Object, _, Type) :-
  nonvar(Object),
  iz_a(Object, Type).
variable_type_given_constraint(Var, Expression, Type) :-
  well_typed(Expression, _, Bindings),
  lookup_variable_type(Var, Type, Bindings).

well_typed(Expression, Kind) :-
  well_typed(Expression, Kind, _).

%% well_typed(=Expression, ?Kind, -Bindings)
% Expression is well typed as of type Kind.
% Types of any free variables of it are given by Bindings.
well_typed(Object, Kind, Bindings) :-
  well_typed(Object, Kind, [ ], Bindings).

%% well_typed(=Expression, ?Kind, +BindingsIn, -BindingsOut)
% Expression is well typed and of type Kind
% given the variable:type bindings given in BindingsIn.
% Further, BindingsOut gives the complete list of bindings
% for free variables in Expression.
well_typed(Var, Kind, BIn, BOut) :-
  var(Var),
  !,
  variable_well_typed(Var, Kind, BIn, BOut).

well_typed(Atom, Type, Bindings, Bindings) :-
  atomic(Atom),
  !,
  is_type(Atom, Type).

well_typed((Expression, iz_a(Var, VKind)), Kind, BIn, BOut) :-
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
variable_well_typed(V, Kind, B, [V:Kind | B]). % haven't seen this var before.

variable_well_typed(V, Kind, entity, B, [V:Kind | B]) :-
  !.
variable_well_typed(_V, Kind, PreviousKind, B, B) :-
  kind_of(PreviousKind, Kind),  % We already have a type that's at least as specific.
  !.
variable_well_typed(V, Kind, PreviousKind, B, [V:Kind | B]) :-
  kind_of(Kind, PreviousKind),  % Kind is a more specific type.
  !.

lookup_variable_type(Var, Type, [V:Type | _]) :-
  V==Var,
  !.
lookup_variable_type(Var, Type, [_ | Tail]) :-
  lookup_variable_type(Var, Type, Tail).



enforce_args(S):- var(S), !, freeze(S, enforce_args(S)).
enforce_args(_:S):- enforce_args(S).
enforce_args(S):- \+ compound(S), !.
enforce_args(S):-
  compound_name_arity(S, F, A),
  compound_name_arity(STypes, F, A),
  (attempt_predicate_type(_, STypes)-> enforce_args(S, STypes) ; true).

enforce_args(S, SType):-
  atom(SType),!,
  enforce_args(S).

enforce_args(S, SType):-
  share_functor(SType, S, F ),
  compound_name_arguments(S, F, Args),
  compound_name_arguments(SType, F, TArgs),
  enforce_args(F, TArgs, Args).

enforce_args(_, _, []):-!.
enforce_args(F, [TArg|TArgs], [Arg|Args]):-
  enforce_arg(Arg, TArg),
  enforce_args(F, TArgs, Args).

iz_c(Arg, TArg):- iz_a(Arg, TArg).
iz_c(Arg, TArg):- attempt_predicate_type(TArg, STypes),
  (compound(STypes) -> (compound_name_arity(STypes, F, A), functor(Arg, F, A));true),
  enforce_args(Arg, STypes).

enforce_arg(Arg, entity):- !, nop((iz_c(Arg, entity);Arg=_)).
enforce_arg( _, TArg):- \+ iz_c(_, TArg), !.
enforce_arg(Arg, TArg):- var(Arg), !, freeze(Arg, enforce_targ(Arg, TArg)).
enforce_arg(Arg, TArg):- enforce_targ(Arg, TArg).

enforce_targ(Arg, TArg):- compound(Arg), !, enforce_args(Arg, TArg).
enforce_targ(Arg, TArg):- iz_c(Arg, TArg).

:- multifile(predicate_type/2).
:- dynamic(predicate_type/2).

attempt_predicate_type(PType, STypes):- predicate_type(PType, STypes).
attempt_predicate_type(PType, STypes):- typed_clauses(PType, STypes).

typed_clauses(Type, P) :- clt(Type, P, Head), clause(Head, _), nonvar(P).

clt(T, P, Head):- cltf(T, F, A, N), functor(Head, F, A), arg(N, Head, P).
cltf(event, ss, 7, 1):- fail.


predicate_type(action, do(actor, action)).
predicate_type(action, be(actor)).
predicate_type(action, drink(actor, beverage)).
predicate_type(action, eat(person, food)).
predicate_type(action, end_game(entity, entity)).
predicate_type(action, give(actor, actor, physical_object)).
predicate_type(action, go(actor, physical_object)).
predicate_type(action, halt(actor)).
predicate_type(action, deactivate(actor, living_thing)).
predicate_type(action, move(person, physical_object, container, container)).
predicate_type(action, move(person, physical_object, container)).
predicate_type(action, put(actor, physical_object, container)).
predicate_type(action, press(actor, button)).
predicate_type(action, get(actor, physical_object)).
predicate_type(action, find(actor, physical_object)).
predicate_type(action, take(actor, physical_object, container)).
predicate_type(action, talk(actor, actor, entity)).
predicate_type(action, tell(actor, actor, action)).
predicate_type(action, tell(actor, actor, condition)).
predicate_type(action, tell_value(actor, actor, condition)).
predicate_type(action, tell_about(actor, actor, entity)).
predicate_type(action, ask_about(actor, actor, entity)).
predicate_type(action, ask(actor, actor, action)).
predicate_type(action, ask(actor, actor, condition)).
predicate_type(action, ask_value(actor, actor, condition)).
predicate_type(action, comm(keystrokes, actor, action)).
predicate_type(action, comm(keystrokes, actor, assertion)).
predicate_type(action, comm(keystrokes, actor, question)).
predicate_type(action, bring(person, physical_object, physical_object)).
predicate_type(action, search_for(actor, physical_object, physical_object)).
predicate_type(action, look_for(actor, physical_object)).
predicate_type(action, sleep(actor, container)).
predicate_type(action, examine(actor, physical_object)).
predicate_type(action, read(actor, document)).
predicate_type(action, switch(actor, appliance, entity)).
predicate_type(action, believes(actor, condition)).
predicate_type(action, watch(actor, physical_object)).
predicate_type(action, leave(actor, container)).
predicate_type(action, flee(actor)).
predicate_type(action, use(actor, device)).
predicate_type(action, operate(actor, device)).
predicate_type(action, thinks(actor, condition)).

predicate_type(condition, intend(actor, action)).
predicate_type(condition, informed_about(actor, condition)).
predicate_type(condition, inferrable_postcondition(condition)).

predicate_type(condition, before(action, action)).
predicate_type(condition, after(action, action)).
predicate_type(condition, strategy(action, action)).
predicate_type(condition, precondition(action, condition)).
predicate_type(condition, postcondition(action, condition)).
predicate_type(condition, plot_goal(condition)).
predicate_type(condition, personal_strategy(action, action)).
predicate_type(condition, permissible(action)).
predicate_type(condition, on_enter_state(taskstate, task, qud)).
predicate_type(condition, on_event(action, eventtype, character, condition)).
predicate_type(condition, on_event(action, eventtype, character, action)).
predicate_type(condition, objectives_achieved(number)).
predicate_type(condition, objective_description(condition, string)).
predicate_type(condition, normalize_task(action, task)).
predicate_type(condition, normalize_dialog_act(action, action)).
predicate_type(condition, menu_dialog_act(character, action)).
predicate_type(condition, menu_dialog(character, action)).
predicate_type(condition, menu_action(character, action)).
predicate_type(condition, default_strategy(action, action)).
predicate_type(condition, unique_answer(entity, condition)).

predicate_type(condition, pretend_truth_value(conversant, condition, truthvalue)).
predicate_type(condition, inverted_sentence(condition, entity, entity, entity, entity, entity)).
predicate_type(condition, ss(condition, entity, entity, entity, entity, entity, entity)).

predicate_type(condition, well_formed_dialog_act(dialog_act)).
predicate_type(condition, stock_phrase(dialog_act)).
predicate_type(condition, notebook_entry(list)).
predicate_type(condition, nocondition).
predicate_type(condition, normalize_precondition_for_graph(dialog_act,dialog_act)).
predicate_type(condition, normalize_dialog_act(dialog_act, dialog_act)).
predicate_type(condition, da_normal_form(dialog_act, dialog_act)).
predicate_type(condition, conversation_idle_task(character, dialog_task)).
predicate_type(condition, conversation_handler_task(character, dialog_task)).
predicate_type(condition, construe(dialog_act, event)).
predicate_type(condition, beat_task_name(beat_task, string)).
predicate_type(condition, beat_start_task(beat_task, conversant, action)).
predicate_type(condition, beat_requirement(beat_task, condition)).
predicate_type(condition, beat_priority(beat_task, number)).
predicate_type(condition, beat_menu_question(beat_task, conversant, condition)).
predicate_type(condition, beat_menu_command(beat_task, conversant, action)).
predicate_type(condition, beat_menu_automa_command(beat_task, conversant, condition)).
predicate_type(condition, beat_leads_to_event(beat_task, conversant, action)).
predicate_type(condition, beat_is_character_reaction(beat_task, conversant, action)).
predicate_type(condition, beat_info(text)).
predicate_type(condition, beat_includes_markup(beat_task, dialog_act)).
predicate_type(condition, beat_graph_subgraph(beat_task, list)).
predicate_type(condition, beat_graph_relation(beat_task, conversant, list)).
predicate_type(condition, beat_graph_node(beat_task, list)).
predicate_type(condition, beat_graph_attributes(beat_task, list)).
predicate_type(condition, beat_dialog(beat_task, conversant, conversant, list)).
predicate_type(condition, beat_delay(beat_task, number)).
predicate_type(condition, beat_declaration_assertions(beat_task, conversant, list)).
predicate_type(condition, current_priority(number)).
predicate_type(condition, respond_to_quip_markup(dialog_act)).
predicate_type(condition, respond_to_dialog_act(dialog_act)).

predicate_type(condition, respond_to_increment(conversant, conversant, dialog_act)).
predicate_type(condition, respond_to_assertion(conversant, condition, yn_answer)).
predicate_type(condition, normalized_assertion(conversant, conversant, condition, condition)).
predicate_type(condition, normalized_assertion(conversant, conversant, action, condition)).

predicate_type(condition, score_action(action, task, character, number)).
predicate_type(condition, propose_action(action, task, character)).
predicate_type(condition, in_room(physical_object, room) ).
predicate_type(condition, incompatible_cl(condition,condition)).


predicate_type(condition, entity:action).
predicate_type(condition, entity:condition).

predicate_type(condition,  (condition, condition)).
predicate_type(condition,  (action, condition)).


predicate_type(condition, would(condition)).
predicate_type(condition, would(action)).
predicate_type(condition, wants(actor, condition)).
predicate_type(condition, wants(actor, action)).
predicate_type(condition, needs(actor, condition)).
predicate_type(condition, needs(actor, action)).
predicate_type(condition, likes(actor, condition)).
predicate_type(condition, likes(actor, action)).
predicate_type(condition, thinks(actor, condition)).
predicate_type(condition, should(action)).
predicate_type(condition, shall(action)).
predicate_type(condition, okay(person)).
predicate_type(condition, may(action)).
predicate_type(condition, can(action)).
predicate_type(condition, believes(actor, condition)).
predicate_type(condition, knows_value(actor, condition)).
predicate_type(condition, know(actor, condition)).
predicate_type(condition, manner(action, manner)).

predicate_type(condition, toggled_state(appliance, entity, entity)).
predicate_type(condition, related(entity, relation, entity)).
predicate_type(condition, location(physical_object, container)).
predicate_type(condition, contained_in(physical_object, container)).

predicate_type(condition, away(living_thing)).
predicate_type(condition, present(physical_object)).
predicate_type(condition, here(living_thing)).

predicate_type(condition, good(person)).
predicate_type(condition, fine(person)).
predicate_type(condition, okay(person)).
predicate_type(condition, hungry(person)).
predicate_type(condition, thirsty(person)).

predicate_type(condition, kind_of(kind, kind)).
predicate_type(condition, iz_a(entity, kind)).

predicate_type(condition, be(entity, entity)).
predicate_type(condition, be(entity)).


