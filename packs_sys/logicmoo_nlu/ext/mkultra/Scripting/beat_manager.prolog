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
            beat_start_task/3, beat_idle_task/3, beat_sequel/2, beat_follows/2, beat_delay/2.
:- external plot_relevant_assertion/4.
:- higher_order beat_precondition(0, 1).
:- public dialog_task_advances_current_beat/1, my_beat_idle_task/1.

%%%
%%% Task generation based on beat
%%%
%%% This code gets called by characters when they're idle to find out
%%% they can do to advance the plot.
%%%
%%% There are two different entrypoints, one for dialog tasks and
%%% one for non-dialog tasks.
%%%

dialog_task_with_partner_advances_current_beat(Beat, Partner, Task) :-
   \+ $global_root/configuration/inhibit_beat_system,
   beat_dialog_with(Beat, Partner, TaskList),
   ( incomplete_beat_task_from_list(Beat, TaskList, T) ->
     can_perform_beat_task(T, Task)
        ;
        (Task=null, check_beat_completion) ).

% Used for debugging display.
potential_beat_dialog(Task) :-
   current_log_character(Beat),
   in_conversation_with(Partner),
   dialog_task_with_partner_advances_current_beat(Beat, Partner, Task).

can_perform_beat_task(Who:Task, Task) :-
   !,
   Who = $me.
can_perform_beat_task(Task, Task) :-
   have_strategy(Task).

incomplete_beat_task_from_list(Beat, TaskList, Task) :-
   member(Task, TaskList),
   ( atomic(Task) ->
        % The / operator only does a pointer comparison, not unification.
        % So this only works when Task is atomic.
        (\+ $global_root/beats/Beat/completed_tasks/Task)
        ;
        % This is the harder version - look at each T and check if it's Task.
        \+ ($global_root/beats/Beat/completed_tasks/T, T=Task) ).

beat_dialog_with(Beat, Partner, TaskList) :-
   beat_dialog(Beat, $me, Partner, TaskList).
beat_dialog_with(Beat, Partner, TaskList) :-
   beat_dialog(Beat, Partner, $me, TaskList).

%% my_beat_idle_task(-Task)
%  Task is the thing I should do to advance the current beat if
%  I'm not already involved in dialog.
my_beat_idle_task(sleep(1)) :-
   beat_waiting_for_timeout.
my_beat_idle_task(Task) :-
   \+ $global_root/configuration/inhibit_beat_system,
   \+ in_conversation_with(_),  % we're not idle if we aren't in conversation
   current_beat(Beat),
   ( next_beat_monolog_task(Beat, Task)
     ;
     beat_idle_task(Beat, $me, Task) ).

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
	     Task,
	     begin(Task,
		   assert($global_root/beats/Beat/completed_tasks/Task))).

beat_waiting_for_timeout :-
   current_beat(Beat),
   beat_delay(Beat, Time),
   ( (\+ beat_running_for_at_least(Beat, Time))
     ;
     (\+ player_idle_for_at_least(Time)) ).

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
   assert($global_root/beats/current:Beat),
   set_beat_state(Beat, started).

%% beat_state(?Beat, ?State)
%  Beat is in the specified State.
beat_state(Beat, State) :-
   $global_root/beats/Beat/state:State.
set_beat_state(Beat, State) :-
   assert($global_root/beats/Beat/state:State).

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

%% beat_score(+Beat, -Score)
%  Beat has the specified score.
beat_score(Beat, Score) :-
   beat_priority(Beat, Score) -> true ; (Score = 0).

start_beat(Beat) :-
   \+ $global_root/configuration/inhibit_beat_system,
   assert($global_root/beats/Beat/start_time: $now),
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

%%%
%%% Monitoring plot-relevant events
%%%

standard_concern(plot_event_monitor, 1).

on_event(pickup(X),
	 plot_event_monitor,
	 _,
	 react_to_plot_event(pickup(X))) :-
   is_a(X, key_item).

on_event(ingest(X),
	 plot_event_monitor,
	 _,
	 react_to_plot_event(ingest(X))) :-
   character(X).

on_event(assertion(Speaker, $me, LF, Tense, Aspect),
	 plot_event_monitor,
	 _,
	 react_to_plot_event(learns_that($me, PlotPoint))) :-
   modalized(LF, Tense, Aspect, Modal),
   plot_relevant_assertion(Speaker, $me, Modal, PlotPoint).

plot_point(learns_that(Character, LF),
	   $global_root/plot_points/Character/LF).
plot_point(ingest(Character),
	   $global_root/plot_points/killed/ $me/Character).
plot_point(ingest(Character),
	   $global_root/plot_points/ate/ $me/Character).

react_to_plot_event(Event) :-
   forall(plot_point(Event, PlotPoint),
	  assert(PlotPoint)),
   maybe_interrupt_current_beat.

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
beat_info(color(Color,
		[ line(Beat, ": "),
		  line("\tScore: ", Score, "\tState: ", State)
		  | WaitList ])) :-
   current_beat(Current),
   beat(Beat),
   beat_score(Beat, Score),
   (beat_state(Beat, State) -> true ; State=null),
   all(line("\tWaiting for: ", Precondition),
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