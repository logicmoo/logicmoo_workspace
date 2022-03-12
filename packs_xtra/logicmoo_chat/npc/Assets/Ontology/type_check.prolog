
:- multifile test/2, test/1, test_options/2, test_file/2.


%% possibile_types_given_constraint(-Var, :Expression, -Types)
% Gives all the possible types for Var within Expression.
possible_types_given_constraint(Var, Expression, Types) :-
  all(Type,
    variable_type_given_constraint(Var, Expression, Type),
    Types).

%=autodoc
%% possible_types_given_constraint( ?Var, ?Expression, ?Types) is semidet.
%
% Possible  Types Given Constraint.
%


%% possibile_types_given_constraint(-Var, :Expression, -Types)
% Gives all the possible types for Var within Expression.
variable_type_given_constraint(Object, _, Type) :-
  nonvar(Object),
  iz_a(Object, Type).

%=autodoc
%% variable_type_given_constraint( ?Object, ?ARG2, ?Type) is semidet.
%
% Variable Type Given Constraint.
%

variable_type_given_constraint(Var, Expression, Type) :-
  well_typed(Expression, _, Bindings),
  lookup_variable_type(Var, Type, Bindings).



%=autodoc
%% well_typed( ?Expression, +Kind) is semidet.
%
% Well Typed.
%
well_typed(Expression, Kind) :-
  well_typed(Expression, Kind, _).

%% well_typed(=Expression, ?Kind, -Bindings)
% Expression is well typed as of type Kind.
% Types of any free variables of it are given by Bindings.
well_typed(Object, Kind, Bindings) :-
  well_typed(Object, Kind, [ ], Bindings).

%=autodoc
%% well_typed( ?Object, +Kind, +Bindings) is semidet.
%
% Well Typed.
%


%% well_typed(=Expression, ?Kind, +BindingsIn, -BindingsOut)
% Expression is well typed and of type Kind
% given the variable:type bindings given in BindingsIn.
% Further, BindingsOut gives the complete list of bindings
% for free variables in Expression.
well_typed(Var, Kind, BIn, BOut) :-
  var(Var),
  !,
  variable_well_typed(Var, Kind, BIn, BOut).

%=autodoc
%% well_typed( ?Var, ?Kind, +BIn, -BOut) is semidet.
%
% Well Typed.
%


well_typed(Atom, Type, Bindings, Bindings) :-
  atomic(Atom),
  !,
  is_type(Atom, Type).

well_typed((Expression, iz_a(Var, VKind)), Kind, BIn, BOut) :-
  well_typed(Expression, Kind, [Var:VKind | BIn], BOut).

well_typed(t(Relation, Object, Relatum), condition, BIn, BOut) :- 
  nonvar(Relation), 
  mpred_argtypes(t(Relation, ObjectType, RelatumType)), 
  well_typed(Object, ObjectType, BIn, BIntermediate), 
  well_typed(Relatum, RelatumType, BIntermediate, BOut).

well_typed(t(Property, Object, Value), condition, BIn, BOut) :- 
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



%=autodoc
%% well_typed_arguments( ?ARG1, ?ARG2, ?ARG3, +Bindings) is semidet.
%
% Well Typed Arguments.
%
well_typed_arguments([], [], Bindings, Bindings).
well_typed_arguments([Arg | Args], [Type | Types], BIn, BOut) :-
  well_typed(Arg, Type, BIn, BIntermediate),
  well_typed_arguments(Args, Types, BIntermediate, BOut).



%=autodoc
%% copy_list_as_variables( ?ARG1, ?ARG2) is semidet.
%
% Copy List Converted To Variables.
%
copy_list_as_variables([], []).
copy_list_as_variables([_ | T1], [_ | T2]) :-
  copy_list_as_variables(T1, T2).



%=autodoc
%% variable_well_typed( ?V, +Kind, +BIn, +BOut) is semidet.
%
% Variable Well Typed.
%
variable_well_typed(V, Kind, BIn, BOut) :-
  lookup_variable_type(V, PreviousKind, BIn),
  !,
  variable_well_typed(V, Kind, PreviousKind, BIn, BOut).
variable_well_typed(V, Kind, B, [V:Kind | B]). % haven't seen this var before.



%=autodoc
%% variable_well_typed( ?V, +Kind, ?Entity, ?B, ?V) is semidet.
%
% Variable Well Typed.
%
variable_well_typed(V, Kind, entity, B, [V:Kind | B]) :-
  !.
variable_well_typed(_V, Kind, PreviousKind, B, B) :-
  kind_of(PreviousKind, Kind),  % We already have a type that's at least as specific.
  !.
variable_well_typed(V, Kind, PreviousKind, B, [V:Kind | B]) :-
  kind_of(Kind, PreviousKind),  % Kind is a more specific type.
  !.



%=autodoc
%% lookup_variable_type( ?Var, ?Type, ?V) is semidet.
%
% Lookup Variable Type.
%
lookup_variable_type(Var, Type, [V:Type | _]) :-
  V==Var,
  !.
lookup_variable_type(Var, Type, [_ | Tail]) :-
  lookup_variable_type(Var, Type, Tail).





%=autodoc
%% enforce_args( ?S) is semidet.
%
% Enforce Arguments.
%
enforce_args(S):- var(S), !, freeze(S, enforce_args(S)).
enforce_args(_:S):- enforce_args(S).
enforce_args(S):- \+ compound(S), !.
enforce_args(S):-
  compound_name_arity(S, F, A),
  compound_name_arity(STypes, F, A),
  (attempt_predicate_type(_, STypes)-> enforce_args(S, STypes) ; true).



%=autodoc
%% enforce_args( ?S, ?SType) is semidet.
%
% Enforce Arguments.
%
enforce_args(S, SType):-
  atom(SType),!,
  enforce_args(S).

enforce_args(S, SType):-
  share_functor(SType, S, F ),
  compound_name_arguments(S, F, Args),
  compound_name_arguments(SType, F, TArgs),
  enforce_args(F, TArgs, Args).



%=autodoc
%% enforce_args( ?ARG1, ?ARG2, ?ARG3) is semidet.
%
% Enforce Arguments.
%
enforce_args(_, _, []):-!.
enforce_args(F, [TArg|TArgs], [Arg|Args]):-
  enforce_arg(Arg, TArg),
  enforce_args(F, TArgs, Args).



%=autodoc
%% iz_c( ?Arg, ?TArg) is semidet.
%
% Iz Class.
%
iz_c(Arg, TArg):- iz_a(Arg, TArg).
iz_c(Arg, TArg):- attempt_predicate_type(TArg, STypes),
  (compound(STypes) -> (compound_name_arity(STypes, F, A), functor(Arg, F, A));true),
  enforce_args(Arg, STypes).



%=autodoc
%% enforce_arg( ?Arg, ?Entity) is semidet.
%
% Enforce Argument.
%
enforce_arg(Arg, entity):- !, nop((iz_c(Arg, entity);Arg=_)).
enforce_arg( _, TArg):- \+ iz_c(_, TArg), !.
enforce_arg(Arg, TArg):- var(Arg), !, freeze(Arg, enforce_targ(Arg, TArg)).
enforce_arg(Arg, TArg):- enforce_targ(Arg, TArg).



%=autodoc
%% enforce_targ( ?Arg, ?TArg) is semidet.
%
% Enforce Targ.
%
enforce_targ(Arg, TArg):- compound(Arg), !, enforce_args(Arg, TArg).
enforce_targ(Arg, TArg):- iz_c(Arg, TArg).

:- multifile(predicate_type/2).
:- dynamic(predicate_type/2).



%=autodoc
%% attempt_predicate_type( ?PType, ?STypes) is semidet.
%
% Attempt Predicate Type.
%
attempt_predicate_type(PType, STypes):- predicate_type(PType, STypes).
attempt_predicate_type(PType, STypes):- typed_clauses(PType, STypes).



%=autodoc
%% typed_clauses( ?Type, ?P) is semidet.
%
% Typed Clauses.
%
typed_clauses(Type, P) :- clt(Type, P, Head), clause(Head, _), nonvar(P).



%=autodoc
%% clt( ?T, ?P, ?Head) is semidet.
%
% Clt.
%
clt(T, P, Head):- cltf(T, F, A, N), functor(Head, F, A), arg(N, Head, P).


%=autodoc
%% cltf( ?Event1, ?Ss2, :PRED7PRED73, :PRED1PRED14) is semidet.
%
% Cltf.
%
cltf(event, ss, 7, 1):- fail.




%=autodoc
%% predicate_type( ?Condition1, ?Entity2) is semidet.
%
% Predicate Type.
%
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
predicate_type(condition, t(relation, entity, entity)).
predicate_type(condition, t(location, physical_object, container)).
predicate_type(condition, t(contained_in, physical_object, container)).

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

end_of_file.

% discovered by system

decl_asap(result_isa(~(_), condition)).
decl_asap(result_isa({_}, action)).
decl_asap(result_isa(work_on_everyday_life_task(_), action)).
decl_asap(result_isa(with_status_text(_, _), action)).
decl_asap(result_isa(with_child_task(_, _, _, _), action)).
decl_asap(result_isa(with_child_task(_, _, _), action)).
decl_asap(result_isa(when(_, _, _, _, _, _), action)).
decl_asap(result_isa(when(_, _, _, _, _), action)).
decl_asap(result_isa(when(_, _, _), action)).
decl_asap(result_isa(when(_, _), action)).
decl_asap(result_isa(wait_for_child(_), action)).
decl_asap(result_isa(wait_event_with_timeout(_, _), action)).
decl_asap(result_isa(wait_event_completed(_, _), condition)).
decl_asap(result_isa(wait_event(_, _), task)).
decl_asap(result_isa(wait_condition(_), task)).
decl_asap(result_isa(wait_condition(_), action)).
decl_asap(result_isa(uslash(top, /(perception/nobody_speaking)), condition)).
decl_asap(result_isa(unless(_, _, _, _, _, _), action)).
decl_asap(result_isa(unless(_, _, _, _, _), action)).
decl_asap(result_isa(unless(_, _, _), action)).
decl_asap(result_isa(unless(_, _), action)).
decl_asap(result_isa(tell(_), action)).
decl_asap(result_isa(stop_qud(_), condition)).
decl_asap(result_isa(stop_qud(_), action)).
decl_asap(result_isa(stop(_), action)).
decl_asap(result_isa(speech(_), action)).
decl_asap(result_isa(spawn_child_task(_, _, _), task)).
decl_asap(result_isa(spawn_child_task(_), task)).
decl_asap(result_isa(spawn(_, _, _), action)).
decl_asap(result_isa(spawn(_), action)).
decl_asap(result_isa(show_status(_, _, _), dialog_act)).
decl_asap(result_isa(show_status(_, _, _), action)).
decl_asap(result_isa(show_status(_), task)).
decl_asap(result_isa(show_status(_), action)).
decl_asap(result_isa(set_status(_), action)).
decl_asap(result_isa(set_qud_status(_, _), task)).
decl_asap(result_isa(search_object(_, _, _, _), action)).
decl_asap(result_isa(script_update(_, _), condition)).
decl_asap(result_isa(schedule_satisfaction(_, _, _, _), condition)).
decl_asap(result_isa(say_string(_), task)).
decl_asap(result_isa(say_string(_), action)).
decl_asap(result_isa(say_rest(_, _, _), action)).
decl_asap(result_isa(say_object(_), action)).
decl_asap(result_isa(say_next(_), action)).
decl_asap(result_isa(say_list(_, _, _), action)).
decl_asap(result_isa(say_last(_, _, _), action)).
decl_asap(result_isa(say_first(_, _), action)).
decl_asap(result_isa(say_answer(_), action)).
decl_asap(result_isa(say(_), action)).
decl_asap(result_isa(s(_), dialog_act)).
decl_asap(result_isa(run_quip(_), beat_task)).
decl_asap(result_isa(run_quip(_), action)).
decl_asap(result_isa(reveal(_), dialog_act)).
decl_asap(result_isa(restart(_), action)).
decl_asap(result_isa(resolve_match_failure(_), action)).
decl_asap(result_isa(resolve_conflict(_, _), action)).
decl_asap(result_isa(rejection(_, _, _, _), dialog_act)).
decl_asap(result_isa(ready_to_hand(_), condition)).
decl_asap(result_isa(reaction_to(_, _), conversant)).
decl_asap(result_isa(question_answer(_, _, _, _, _), action)).
decl_asap(result_isa(question_answer(_, _, _), dialog_act)).
decl_asap(result_isa(question_answer(_), dialog_act)).
decl_asap(result_isa(question(_, _, _, _, _), dialog_act)).
decl_asap(result_isa(question(_, _, _, _, _), action)).
decl_asap(result_isa(question(_), entity)).
decl_asap(result_isa(putdown(_, _), action)).
decl_asap(result_isa(t(_, _, _), condition)).
decl_asap(result_isa(prompt_player(_, _), dialog_act)).
decl_asap(result_isa(prompt_player(_, _), action)).
decl_asap(result_isa(preface_description(_), action)).
decl_asap(result_isa(possession(_, _), condition)).
decl_asap(result_isa(player_input(_), action)).
decl_asap(result_isa(pickup(_), task)).
decl_asap(result_isa(pickup(_), action)).
decl_asap(result_isa(pick_randomly(_), action)).
decl_asap(result_isa(pick_preferred(_, _), action)).
decl_asap(result_isa(pause(_), action)).
decl_asap(result_isa(parting(_, _), dialog_act)).
decl_asap(result_isa(parting(_, _), action)).
decl_asap(result_isa(operate(_), action)).
decl_asap(result_isa(on_behalf_of(_, _), action)).
decl_asap(result_isa(offer(_, _, _, _), dialog_act)).
decl_asap(result_isa(offer(_, _, _, _), action)).
decl_asap(result_isa(objective_query(_, _), dialog_act)).
decl_asap(result_isa(objective_query(_, _), action)).
decl_asap(result_isa(monolog(_), task)).
decl_asap(result_isa(monolog(_), action)).
decl_asap(result_isa(method(_, _), condition)).
decl_asap(result_isa(mention(_), event)).
decl_asap(result_isa(mental_monolog(_), action)).
decl_asap(result_isa(maybe_remember_event(_), action)).
decl_asap(result_isa(let(_, _), task)).
decl_asap(result_isa(let(_, _), action)).
decl_asap(result_isa(leads_to(_, _), conversant)).
decl_asap(result_isa(launch_conversation(_, _, _), condition)).
decl_asap(result_isa(iz(uslash), condition)).
decl_asap(result_isa(iz(condition), task)).
decl_asap(result_isa(iz(condition), condition)).
decl_asap(result_isa(iz(condition), action)).
decl_asap(result_isa(invoke_continuation(_), action)).
decl_asap(result_isa(introduce_question(_, _), dialog_act)).
decl_asap(result_isa(introduce_person(_), action)).
decl_asap(result_isa(introduce_goal(_, _), dialog_act)).
decl_asap(result_isa(injunction(_, _, _), action)).
decl_asap(result_isa(ingest(_), action)).
decl_asap(result_isa(inaction(_, _, _, _), dialog_act)).
decl_asap(result_isa(inaction(_, _, _, _), action)).
decl_asap(result_isa(ignore(_), condition)).
decl_asap(result_isa(if_navigation_command(_, _, _), dialog_act)).
decl_asap(result_isa(if_navigation_command(_, _, _), action)).
decl_asap(result_isa(if_navigation_command(_), task)).
decl_asap(result_isa(if_navigation_command(_), action)).
decl_asap(result_isa(if(_, _, _), action)).
decl_asap(result_isa(how_do_i(_, _, _), dialog_act)).
decl_asap(result_isa(how_do_i(_, _, _), action)).
decl_asap(result_isa(how_do_i(_), task)).
decl_asap(result_isa(how_do_i(_), action)).
decl_asap(result_isa(handle_discovery(_), action)).
decl_asap(result_isa(greet(_, _), dialog_act)).
decl_asap(result_isa(greet(_, _), action)).
decl_asap(result_isa(goto_internal(_), action)).
decl_asap(result_isa(goto(_), task)).
decl_asap(result_isa(goto(_), action)).
decl_asap(result_isa(give_name(_), action)).
decl_asap(result_isa(generate_unique_answer(_, _, _, _), action)).
decl_asap(result_isa(generate_singleton(_, _), action)).
decl_asap(result_isa(generate_rest(_, _), action)).
decl_asap(result_isa(generate_next(_, _), action)).
decl_asap(result_isa(generate_list(_, _), action)).
decl_asap(result_isa(generate_last(_, _), action)).
decl_asap(result_isa(generate_first(_, _), action)).
decl_asap(result_isa(generate_empty(_), action)).
decl_asap(result_isa(generate_answer(_, _), action)).
decl_asap(result_isa(general_help(_, _), dialog_act)).
decl_asap(result_isa(general_help(_, _), action)).
decl_asap(result_isa(force_examine(_), action)).
decl_asap(result_isa(follow_command(_, _, _), action)).
decl_asap(result_isa(failed_because(_), action)).
decl_asap(result_isa(explanation(_, _), condition)).
decl_asap(result_isa(explain_failure(_), action)).
decl_asap(result_isa(exit_conversational_space(_), action)).
decl_asap(result_isa(excuse_self(_, _), dialog_act)).
decl_asap(result_isa(excuse_self(_, _), action)).
decl_asap(result_isa(enumerate_answers(_, _, _, _), action)).
decl_asap(result_isa(enter_social_space(_), action)).
decl_asap(result_isa(engage_in_conversation(_), action)).
decl_asap(result_isa(end_game(_), action)).
decl_asap(result_isa(emote(_), task)).
decl_asap(result_isa(emote(_), action)).
decl_asap(result_isa(docked_with(_), condition)).
decl_asap(result_isa(do_not_understand(_, _, _), dialog_act)).
decl_asap(result_isa(do_not_understand(_, _, _), action)).
decl_asap(result_isa(do_beat_dialog(_), dialog_task)).
decl_asap(result_isa(do_beat_dialog(_), action)).
decl_asap(result_isa(do_automatically_believe(_), action)).
decl_asap(result_isa(discourse_increment(_, _, _), dialog_act)).
decl_asap(result_isa(discourse_increment(_, _, _), action)).
decl_asap(result_isa(describe_type(_), action)).
decl_asap(result_isa(describe_relation(_, _, _, _, _), action)).
decl_asap(result_isa(describe_property(_, _, _, _, _), action)).
decl_asap(result_isa(describe_attributes(_, _, _), action)).
decl_asap(result_isa(describe(_, _, _), task)).
decl_asap(result_isa(describe(_, _, _), action)).
decl_asap(result_isa(describe(_), action)).
decl_asap(result_isa(departed(_), action)).
decl_asap(result_isa(deactivate_prop(_), action)).
decl_asap(result_isa(command(_, _, _), dialog_act)).
decl_asap(result_isa(command(_, _, _), action)).
decl_asap(result_isa(color_query(_, _, _), dialog_act)).
decl_asap(result_isa(color_query(_, _, _), action)).
decl_asap(result_isa(color_query(_), task)).
decl_asap(result_isa(color_query(_), action)).
decl_asap(result_isa(color(_, _), text)).
decl_asap(result_isa(cobegin(_, _, _, _, _, _), action)).
decl_asap(result_isa(cobegin(_, _, _, _, _), action)).
decl_asap(result_isa(cobegin(_, _, _, _), action)).
decl_asap(result_isa(cobegin(_, _, _), action)).
decl_asap(result_isa(cobegin(_), action)).
decl_asap(result_isa(clue(_, _), dialog_act)).
decl_asap(result_isa(clue(_), dialog_act)).
decl_asap(result_isa(character_remembers_recently(_, _), condition)).
decl_asap(result_isa(cases(_), action)).
decl_asap(result_isa(begin(_, _, _, _, _, _, _), action)).
decl_asap(result_isa(begin(_, _, _, _, _, _), action)).
decl_asap(result_isa(begin(_, _, _, _, _), action)).
decl_asap(result_isa(begin(_, _, _, _), action)).
decl_asap(result_isa(begin(_, _, _), action)).
decl_asap(result_isa(begin(_, _), task)).
decl_asap(result_isa(begin(_, _), condition)).
decl_asap(result_isa(begin(_, _), action)).
decl_asap(result_isa(beat_state(_, _), condition)).
decl_asap(result_isa(automa_command(_, _, _, _, _), dialog_act)).
decl_asap(result_isa(automa_command(_, _, _, _, _), action)).
decl_asap(result_isa(assertion(_, _, _, _, _), dialog_act)).
decl_asap(result_isa(assertion(_, _, _, _, _), action)).
decl_asap(result_isa(arrived_at(_), action)).
decl_asap(result_isa(apology(_, _), dialog_act)).
decl_asap(result_isa(apology(_, _), action)).
decl_asap(result_isa(answered(_), dialog_act)).
decl_asap(result_isa(answer_yes_no(_, _), action)).
decl_asap(result_isa(answer_with_list(_, _, _, _), action)).
decl_asap(result_isa(answer_wh(_, _, _, _), action)).
decl_asap(result_isa(answer_can_wh(_, _, _, _), action)).
decl_asap(result_isa(agree(_, _, _, _), dialog_act)).
decl_asap(result_isa(agree(_, _, _, _), action)).
decl_asap(result_isa(affective_reaction(_, _, _, _), condition)).
decl_asap(result_isa(add_pending_task(_), action)).
decl_asap(result_isa(add_conversation_topic(_, _), action)).
decl_asap(result_isa(add_conversation_task(_, _), action)).
decl_asap(result_isa(activate_prop(_), action)).
decl_asap(result_isa(achieve_precondition(_, _), action)).
decl_asap(result_isa(achieve(_), action)).
decl_asap(result_isa(acceptance(_, _, _, _), dialog_act)).
decl_asap(result_isa(acceptance(_, _, _, _), action)).
decl_asap(result_isa(abort_and_then(_), action)).
decl_asap(result_isa([_|_], list)).
decl_asap(result_isa([_|_], dialog_act)).
decl_asap(result_isa((table _), text)).
decl_asap(arg_isa(wait_event_completed(_, _), condition)).
decl_asap(arg_isa(tell(_), action)).
decl_asap(arg_isa(speech(_), action)).
decl_asap(arg_isa(script_update(_, _), condition)).
decl_asap(arg_isa(schedule_satisfaction(_, _, _, _), condition)).
decl_asap(arg_isa(run_quip(_), action)).
decl_asap(arg_isa(resolve_match_failure(_), action)).
decl_asap(arg_isa(player_input(_), action)).
decl_asap(arg_isa(pick_randomly(_), action)).
decl_asap(arg_isa(pick_preferred(_, _), action)).
decl_asap(arg_isa(iz(uslash), condition)).
decl_asap(arg_isa(iz(condition), task)).
decl_asap(arg_isa(iz(condition), condition)).
decl_asap(arg_isa(iz(condition), action)).
decl_asap(arg_isa(if(_, _, _), action)).
decl_asap(arg_isa(greet(_, _), action)).
decl_asap(arg_isa(goto(_), action)).
decl_asap(arg_isa(give_name(_), action)).
decl_asap(arg_isa(engage_in_conversation(_), action)).
decl_asap(arg_isa(emote(_), action)).
decl_asap(arg_isa(do_beat_dialog(_), action)).
decl_asap(arg_isa(begin(_, _, _), action)).
decl_asap(arg_isa(begin(_, _), action)).
decl_asap(arg_isa(affective_reaction(_, _, _, _), condition)).
decl_asap(arg_isa(achieve_precondition(_, _), action)).
decl_asap(arg_isa(achieve(_), action)).

