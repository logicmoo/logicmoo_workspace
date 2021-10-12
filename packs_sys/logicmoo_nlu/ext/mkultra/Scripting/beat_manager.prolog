%%%
%%% Simple Beat system
%%% Doesn't handle joint dialog behaviors, since problem solver doesn't support
%%% joint behaviors in general
%%%

%%
%% Since the beat system operates at the story level rather than the
%% character level, its state information is stored in
%% $global_root/beats rather than in a specific character.  However,
%% there is no global drama manager object, so beat selection code
%% runs in whatever character happens to run code that needs to select
%% a new beat.
%%

:- external beat/1, beat_priority/2, beat_precondition/2, beat_completion_condition/2,
   beat_dialog/4, beat_monolog/3,
   beat_start_task/3, beat_idle_task/3, beat_sequel/2, beat_follows/2, beat_delay/2,
   good_ending/1, bad_ending/1, player_achieves_task_during_beat/2,
   beat_is_character_reaction/3,
   beat_leads_to_event/3, plot_event/1.
:- external plot_goal_idle_task/2, plot_goal_achieves/2.
:- external beat_expected_during/2, beat_excursion/2.
:- external plot_relevant_assertion/4.
:- higher_order beat_precondition(0, 1).
:- external plot_goal/1, plot_subgoal/2.
:- higher_order plot_goal(1).
:- higher_order plot_subgoal(1,1).
:- public dialog_task_advances_current_beat/1, my_beat_idle_task/1.

:- external plot_question_introduced/1, plot_question_flavor_text/2,
   plot_question_answered/1,
   revealed/1,
   plot_goal/1, plot_goal_flavor_text/2,
   clue/1, clue_flavor_text/2, objective_description/2.

%%%
%%% Foreground/background relations on characters
%%%
%%% Currently a character is foreground iff they have dialog/monolog.
%%%

%% foreground_character(?Beat, ?Character)
%  Character is involved in the action of Beat.
foreground_character(Beat, Character) :-
   beat_dialog(Beat, Character, _, _).
foreground_character(Beat, Character) :-
   beat_monolog(Beat, Character, _).

%% background_character(?Beat, ?Character)
%  Character is not involved in the action of Beat.
background_character(Beat, Character) :-
   \+ foreground_character(Beat, Character).

:- public foreground_character_in_current_beat/0, background_character_in_current_beat/0.
%% foreground_character_in_current_beat
%  Current character ($me) is involved in the action of current beat.
foreground_character_in_current_beat :-
   current_beat(Beat),
   once(foreground_character(Beat, $me)).
foreground_character_in_current_beat :-
   in_conversation_with($pc).
   
%% background_character_in_current_beat
%  Current character ($me) is not involved in the action of current beat.
background_character_in_current_beat :-
   current_beat(Beat),
   \+ in_conversation_with($pc),
   background_character(Beat, $me).

%%%
%%% Task generation based on beat
%%%
%%% This code gets called by characters when they're idle to find out
%%% they can do to advance the plot.
%%%
%%% There are two different entrypoints, one for dialog tasks and
%%% one for non-dialog tasks.
%%%

dialog_task_with_partner_advances_current_beat(Beat, Partner, Canon) :-
   \+ $global_root/configuration/inhibit_beat_system,
   beat_dialog_with(Beat, Partner, TaskList),
   ( incomplete_beat_task_from_list(Beat, TaskList, T) ->
     (can_perform_beat_task(T, Task), canonicalize_beat_dialog_task(Task, Canon))
        ;
     (Task=null, check_beat_completion) ).

canonicalize_beat_dialog_task(String, run_quip(String)) :-
   string(String),
   !.
canonicalize_beat_dialog_task(String:Markup, run_quip(String:Markup)) :-
   string(String),
   !.
canonicalize_beat_dialog_task(Task, Task).

beat_task_name(run_quip(String:_Markup), String) :-
   !.
beat_task_name(run_quip(String), String) :-
   !.
beat_task_name(X, X).

% Used for debugging display.
potential_beat_dialog(Task) :-
   current_log_character(Beat),
   in_conversation_with(Partner),
   dialog_task_with_partner_advances_current_beat(Beat, Partner, Task).

can_perform_beat_task(Who::Task, Task) :-
   !,
   Who = $me.
can_perform_beat_task(Task, Task) :-
   have_strategy(Task).

incomplete_beat_task_from_list(Beat, TaskList, Task) :-
   member(Task, TaskList),
   \+ beat_task_already_executed(Beat, Task).

beat_task_already_executed(Beat, _Character :: Whatever) :-
   !,
   beat_task_already_executed(Beat, Whatever).
beat_task_already_executed(Beat, String:_Markup) :-
   !,
   $global_root/beats/Beat/completed_tasks/String.
beat_task_already_executed(Beat, Task) :-
   atomic(Task) ->
        % The / operator only does a pointer comparison, not unification.
        % So this only works when Task is atomic.
        ($global_root/beats/Beat/completed_tasks/Task)
        ;
        % This is the harder version - look at each T and check if it's Task.
        (($global_root/beats/Beat/completed_tasks/T), T=Task).

beat_dialog_with(Beat, Partner, TaskList) :-
   beat_dialog(Beat, $me, Partner, TaskList).
beat_dialog_with(Beat, Partner, TaskList) :-
   beat_dialog(Beat, Partner, $me, TaskList).

%%%
%%% Beat background tasks
%%%

todo(BeatIdleTask, 0.5) :-
   my_beat_idle_task(BeatIdleTask).

%% my_beat_idle_task(-Task)
%  Task is the thing I should do to advance the current beat if
%  I'm not already involved in dialog.
my_beat_idle_task(Task) :-
   beat_is_idle,
   current_beat(Beat),
   ( next_beat_monolog_task(Beat, Task)
     ;
     beat_idle_task(Beat, $me, Task) ).

beat_is_idle :-
   \+ $global_root/configuration/inhibit_beat_system,
   \+ in_conversation_with(_), % we're not idle if we aren't in conversation
   current_beat(B),
   \+ beat_dialog(B, $me, _, _),
   \+ beat_waiting_for_timeout.

%%%
%%% Plot goal idle tasks
%%%

todo(PlotGoalIdleTask, 0) :-
   player_character,
   beat_is_idle,
   plot_goal(G),
   \+ G,
   plot_goal_idle_task(G, PlotGoalIdleTask).


%%%
%%% Beat monologs
%%%

next_beat_monolog_task(Beat, T) :-
   beat_monolog(Beat, $me, TaskList),
   (incomplete_beat_task_from_list(Beat, TaskList, Task) ->
      monolog_task(Beat, Task, T)
      ;
      ( T = null, check_beat_completion )).

monolog_task(Beat,
	     String,
	     begin(run_quip(String),
		   assert($global_root/beats/Beat/completed_tasks/String)) ) :-
   string(String),
   !.
monolog_task(Beat,
	     String:Markup,
	     begin(run_quip(String),
		   respond_to_quip_markup(Markup),
		   assert($global_root/beats/Beat/completed_tasks/String)) ) :-
   string(String),
   !.

monolog_task(Beat,
	     Task,
	     begin(Task,
		   assert($global_root/beats/Beat/completed_tasks/Task))).

beat_waiting_for_timeout :-
   current_beat(Beat),
   beat_delay(Beat, Time),
   (\+ ( beat_running_for_at_least(Beat, Time),
	 player_idle_for_at_least(Time) )).

%%%
%%% Beat state
%%%

%% current_beat(?Beat)
%  Beat is the beat we're currently working on.  If none had been
%  previously selected, this will force it to select a new one.
current_beat(Beat) :-
   $global_root/beats/current:Beat,
   !.
current_beat(Beat) :-
   var(Beat), % need this or calling this will a bound variable
              % will force the selection of a new beat.
   select_new_beat(Beat),
   !.

set_current_beat(Beat) :-
   tell($global_root/beats/current:Beat),
   set_beat_state(Beat, started).

%% beat_state(?Beat, ?State)
%  Beat is in the specified State.
beat_state(Beat, State) :-
   $global_root/beats/Beat/state:State.
set_beat_state(Beat, State) :-
   tell($global_root/beats/Beat/state:State).

beat_running_time(Beat, Time) :-
   $global_root/beats/Beat/start_time:T,
   Time is $now-T.

beat_running_for_at_least(Beat, Time) :-
   beat_running_time(Beat, T),
   T >= Time.

%%%
%%% Beat selection
%%%

%% best_next_beat(-Beat)
%  Beat is the best beat to run next.
best_next_beat(Beat) :-
   arg_max(Beat,
	   Score,
	   ( available_beat(Beat),
	     beat_score(Beat, Score) )).

%% select_new_beat(-Beat)
%  Forces reselection of the next beat.
select_new_beat(Beat) :-
   best_next_beat(Beat),
   set_current_beat(Beat),
   start_beat(Beat).

%% available_beat(?Beat)
%  Beat is a beat that hasn't finished and whose preconditions are satisfied.
available_beat(Beat) :-
   beat(Beat),
   \+ beat_state(Beat, completed),
   runnable_beat(Beat).

%% runnable_beat(+Beat)
%  Beat has no unsatisfied preconditions
runnable_beat(Beat) :-
   forall(beat_requirement(Beat, P),
	  P).

beat_requirement(Beat, beat_state(Predecessor, completed)) :-
   beat_follows(Beat, Predecessor).
beat_requirement(Beat, $global_root/beats/previous:ImmediatePredecessor) :-
   beat_sequel(Beat, ImmediatePredecessor).
beat_requirement(Beat, Precondition) :-
   beat_precondition(Beat, Precondition).
beat_requirement(Beat, character_remembers_recently(Character, Event)) :-
   beat_is_character_reaction(Beat, Character, Event).

%% beat_score(+Beat, -Score)
%  Beat has the specified score.
beat_score(Beat, Score) :-
   beat_priority(Beat, Score) -> true ; (Score = 0).

start_beat(Beat) :-
   \+ $global_root/configuration/inhibit_beat_system,
   tell($global_root/beats/Beat/start_time: $now),
   forall(beat_start_task(Beat, Who, Task),
	  Who::add_pending_task(Task)).

%% interrupt_beat(+Beat)
%  Called when Beat is to be interrupted.
interrupt_beat(_).  % currently does nothing.

%% check_beat_completion
%  Called upon completion of beat dialog.  Ends beat if any additional
%  completion conditions are achieved.
check_beat_completion :-
   current_beat(Beat),
   (beat_completion_condition(Beat, C) -> C ; true),
   end_beat.

end_beat :-
   current_beat(Beat),
   set_beat_state(Beat, completed),
   assert($global_root/beats/previous:Beat),
   retract($global_root/beats/current).

test_file(problem_solver(_),
	  "Scripting/beat_task_crossrefs").

on_memorable_event(Event) :-
   plot_event(Event) -> maybe_interrupt_current_beat.

when_added(Assertion, maybe_interrupt_current_beat) :-
   beat_precondition(_, Assertion).

when_added(Assertion, maybe_interrupt_current_beat) :-
   beat_completion_condition(_, Assertion).

maybe_interrupt_current_beat :-
   begin(current_beat(Current),
	 beat_score(Current, CurrentScore),
	 best_next_beat(Winner),
	 beat_score(Winner, WinnerScore)),
   WinnerScore > CurrentScore,
   begin(interrupt_beat(Current),
	 set_current_beat(Winner),
	 start_beat(Winner)).
maybe_interrupt_current_beat.

%%%
%%% Context menu support
%%%

:- external beat_menu_action/3,
   beat_menu_assertion/3, beat_menu_command/3, beat_menu_question/3, beat_menu_hypno_command/3.
menu_action(Object, DialogAct) :-
   current_beat(Beat),
   beat_menu_action(Beat, Object, DialogAct).

menu_question(Object, DialogAct) :-
   current_beat(Beat),
   beat_menu_question(Beat, Object, DialogAct).

menu_assertion(Object, DialogAct) :-
   current_beat(Beat),
   beat_menu_assertion(Beat, Object, DialogAct).

menu_command(Object, DialogAct) :-
   current_beat(Beat),
   beat_menu_command(Beat, Object, DialogAct).

menu_hypno_command(Object, DialogAct) :-
   current_beat(Beat),
   beat_menu_hypno_command(Beat, Object, DialogAct).

%%%
%%% Beat declaration language
%%%

:- public beat/2.

initialization :-
   beat(BeatName, { Declarations }),
   assert($global::beat(BeatName)),
   forall(( member_of_comma_separated_list(Declaration, Declarations),
	    once(beat_declaration_assertions(BeatName, Declaration, Assertions)),
	    member(Assertion, Assertions) ),
	  assert($global::Assertion)).

member_of_comma_separated_list(Member, (X, Y)) :-
   !,
   ( member_of_comma_separated_list(Member, X)
   ;
     member_of_comma_separated_list(Member, Y) ).
member_of_comma_separated_list(Member, Member).

beat_declaration_assertions(BeatName,
			   start(Character):Task,
			   [beat_start_task(BeatName, Character, Task)]).
beat_declaration_assertions(BeatName,
			   (Character1+Character2):Dialog,
			   [beat_dialog(BeatName, Character1, Character2, Dialog)]).
beat_declaration_assertions(BeatName,
			   Character: Monolog,
			   [beat_monolog(BeatName, Character, Monolog)]) :-
   character(Character).
beat_declaration_assertions(BeatName,
			    idle_task(Character): Task,
			   [beat_idle_task(BeatName, Character, Task)]) :-
   character(Character).
beat_declaration_assertions(BeatName,
			   sequel_to: Beat,
			   [beat_sequel(BeatName, Beat)]).
beat_declaration_assertions(BeatName,
			   start_delay: Time,
			   [beat_delay(BeatName, Time)]).
beat_declaration_assertions(BeatName,
			   follows: Beat,
			   [beat_follows(BeatName, Beat)]).
beat_declaration_assertions(BeatName,
			   completed_when: Condition,
			   [beat_completion_condition(BeatName, Condition)]).
beat_declaration_assertions(BeatName,
			   priority: Priority,
			   [beat_priority(BeatName, Priority)]).
beat_declaration_assertions(BeatName,
			   precondition: Condition,
			   [beat_precondition(BeatName, Condition)]).
beat_declaration_assertions(BeatName,
			   excursion_of: Predecessor,
			   [beat_excursion(BeatName, Predecessor)]).
beat_declaration_assertions(BeatName,
			   expected_during: Predecessor,
			   [beat_expected_during(BeatName, Predecessor)]).
beat_declaration_assertions(BeatName,
			    leads_to(Character, Event),
			    [beat_leads_to_event(BeatName, Character, Event)]).
beat_declaration_assertions(BeatName,
			    reaction_to(Character, Event),
			    [ beat_is_character_reaction(BeatName, Character, Event)
			      | Memorability ]) :-
   plot_event(Event) ->
   (Memorability=[])
   ;
   (Memorability=[plot_event(Event)]).

memorable_event(E) :-
   plot_event(E).

beat_declaration_assertions(BeatName,
			   good_ending,
			   [good_ending(BeatName)]).
beat_declaration_assertions(BeatName,
			   bad_ending,
			   [bad_ending(BeatName)]).

beat_declaration_assertions(BeatName,
			   player_achieves:Task,
			    [player_achieves_task_during_beat(BeatName, Task)]).

beat_declaration_assertions(BeatName,
			   menu_action(Object):DialogAct,
			   [beat_menu_action(BeatName, Object, DialogAct)]).
beat_declaration_assertions(BeatName,
			   menu_command(Character):DialogAct,
			   [beat_menu_command(BeatName, Character, DialogAct)]).
beat_declaration_assertions(BeatName,
			   menu_assertion(Character):DialogAct,
			   [beat_menu_assertion(BeatName, Character, DialogAct)]).
beat_declaration_assertions(BeatName,
			   menu_hypno_command(Character):DialogAct,
			   [beat_menu_hypno_command(BeatName, Character, DialogAct)]).
beat_declaration_assertions(BeatName,
			   menu_question(Character):DialogAct,
			   [beat_menu_question(BeatName, Character, DialogAct)]).

beat_declaration_assertions(BeatName, Declaration, []) :-
   log(BeatName:unknown_beat_declaration(Declaration)).

%%%
%%% Derived beat properties
%%%

terminal_beat(B) :-
   good_ending(B).
terminal_beat(B) :-
   bad_ending(B).

%%%
%%% Objectives
%%%

objective_achieved(Objective) :-
   objective_description(Objective, _),
   Objective.

unachieved_objective(Objective) :-
   objective_description(Objective, _),
   \+ Objective.

%%%
%%% Debugging display
%%%

fkey_command(alt-delete, "Skip current beat") :-
   current_beat(Beat),
   log(force_beat_completion:Beat),
   end_beat.

fkey_command(alt-b, "Show beat status") :-
   generate_unsorted_overlay("Beat status",
			     beat_info(I),
			     I).

beat_info(color("red", line("Beat system disabled."))) :-
   $global_root/configuration/inhibit_beat_system,
   !.
beat_info(table([[bold("Beat"), bold("Score"), bold("State"), bold("Waiting for")]
		| BeatList])) :-
   findall([color(Color, Beat), Score, State, term(WaitList)],
	   beat_table_entry(Beat, Score, State, WaitList, Color),
	   BeatList).

beat_table_entry(Beat, Score, State, WaitList, Color) :-
   current_beat(Current),
   beat(Beat),
   beat_score(Beat, Score),
   (beat_state(Beat, State) -> true ; State=" "),
   all(Precondition,
       unsatisfied_beat_precondition(Beat, Precondition),
       WaitList),
   once(beat_display_color(Beat, Current, WaitList, State, Color)).

unsatisfied_beat_precondition(Beat, P) :-
   beat_precondition(Beat, P),
   \+ P.

beat_display_color(Current, Current, _,   _,         lime) :-
   \+ beat_waiting_for_timeout.
beat_display_color(Current, Current, _,   _,         yellow).  % if waiting for timeout
beat_display_color(_,       _,       _,   completed, grey).
beat_display_color(_,       _,       [ ], _,         white).
beat_display_color(_,       _,       _,   _,         red).

character_debug_display(Character, line("Idle task:\t", Task, "\t", beat:Beat)) :-
   current_beat(Beat),
   (Character::beat_idle_task(Beat, Character, Task) -> true ; (Task=none)).
character_debug_display(Character, line("Beat task:\t", Task)) :-
   Character::potential_beat_dialog(Task).

%%%
%%% Graphviz interface
%%%

:- public make_plot_graph/0, beat_graph_node/2, beat_graph_relation/3,
   beat_graph_subgraph/2.

make_plot_graph :-
   draw_diggraph(beat_graph_node, beat_graph_relation, beat_graph_subgraph).

beat_graph_node(Beat, [shape=box | Attributes]) :-
   beat(Beat),
   beat_graph_attributes(Beat, Attributes).

beat_graph_attributes(Beat, [style=filled, fillcolor=green]) :-
   good_ending(Beat),
   !.
beat_graph_attributes(Beat, [style=filled, fillcolor=red]) :-
   bad_ending(Beat),
   !.
beat_graph_attributes(_Beat, []).

beat_graph_relation(B1, B2, [label="sequel"]) :-
   beat_sequel(B2, B1).

beat_graph_relation(B1, B2, [label="precedes"]) :-
   beat_follows(B2, B1).

% beat_graph_relation(B1, B2, [style=dotted]) :-
%    beat_expected_during(B2, B1).

% beat_graph_relation(B1, B2, [label="excursion", style=dotted]) :-
%    beat_excursion(B2, B1).

beat_graph_relation(B, G, [label="introduces goal"]) :-
   beat(B),
   beat_includes_markup(B, introduce_goal(G,_)).

beat_graph_relation(B, Q, [label="introduces question"]) :-
   beat(B),
   beat_includes_markup(B, introduce_question(Q,_)).

beat_graph_relation(Q, B, [label="answered by"]) :-
   beat(B),
   beat_includes_markup(B, answered(Q)).

beat_graph_relation(B, N, [label="completion requires"]) :-
   beat_completion_condition(B, Condition),
   member_of_comma_separated_list(C, Condition),
   normalize_precondition_for_graph(C, N).

beat_graph_relation(N, B, [label="precondition"]) :-
   beat_precondition(B, P),
   normalize_precondition_for_graph(P, N).

%beat_graph_relation(G, S, [label="subgoal"]) :-
%   plot_subgoal(S, G).

beat_graph_relation(G, C, [label="consequence"]) :-
   plot_goal_achieves(G, C).
beat_graph_relation(B, T, [label="player achieves"]) :-
   player_achieves_task_during_beat(B, T).

beat_graph_node(Event, [shape=octagon]) :-
   beat_is_character_reaction(_, _, Event).

beat_graph_relation(Event, Beat, [label="reaction to"]) :-
   beat_is_character_reaction(Beat, _, Event).

beat_graph_relation(Event, Condition, [label="achieves"]) :-
   beat_is_character_reaction(_, Character, Event),
   postcondition(Event, Condition),
   (plot_relevant_condition(Condition) ; plot_relevant_condition(Character::Condition)).

plot_relevant_condition(Condition) :-
   beat_precondition(_, Condition).
plot_relevant_condition(Condition) :-
   beat_completion_condition(_, Condition).
plot_relevant_condition(Condition) :-
   beat_completion_condition(_, Condition)
   ;    beat_completion_condition(_, (Condition, _))
   ;    beat_completion_condition(_, (_, Condition)).


beat_graph_relation(Beat, Event, [label="leads to"]) :-
   beat_leads_to_event(Beat, _, Event).

beat_graph_node(N, [shape=ellipse]) :-
   beat_precondition(_,P),
   once((normalize_precondition_for_graph(P, N) ; P=N)).
normalize_precondition_for_graph($pc::P, P).
normalize_precondition_for_graph(plot_goal(P), P).

beat_graph_node(G, [shape=ellipse, style=filled, fillcolor=green]) :-
   plot_goal(G).
beat_graph_node(G, [shape=ellipse]) :-
   beat(B),
   beat_includes_markup(B, introduce_goal(G, _)).
beat_graph_node(Q, [shape=ellipse]) :-
   beat(B),
   beat_includes_markup(B, introduce_question(Q, _)).

beat_graph_node(C, [shape=ellipse]) :-
   beat(B),
   beat_includes_markup(B, clue(C)).

beat_includes_markup(Beat, Markup) :-
   (beat_monolog(Beat, _, List) ; beat_dialog(Beat, _, _, List)),
   member(Line, List),
   line_includes_markup(Line, Markup).

line_includes_markup((_:LineMarkup), Markup) :-
   markup_matches(LineMarkup, Markup).
line_includes_markup((_::(_:LineMarkup)), Markup) :-
   markup_matches(LineMarkup, Markup).

markup_matches(M, M) :-
   !.
markup_matches(List, M) :-
   list(List),
   member(M, List).

beat_graph_subgraph(TerminalBeats, [rank=sink]) :-
   all(Beat,
       terminal_beat(Beat),
       TerminalBeats).
