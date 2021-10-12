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
            beat_start_task/3, beat_idle_task/3.
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

%% dialog_task_advances_current_beat(-Task) is det
%  Task is the thing I should run to try to move the beat forward.
%  If I'm not a participant for this beat or if there's nothing for me
%  to do right now, this will fail.
dialog_task_advances_current_beat(begin(Task,
				 assert($global_root/beats/Beat/completed_tasks/Task))) :-
   current_beat(Beat),
   dialog_task_advances_beat(Beat, Task).

%% dialog_task_advances_beat(+Beat, -Task)
%  Task is a task I can do that would advance Beat.
dialog_task_advances_beat(Beat, Task) :-
   $task/partner/Partner,
   beat_dialog_with(Beat, Partner, TaskList),
   ( incomplete_beat_task_from_list(Beat, TaskList, T) ->
        can_perform_beat_task(T, Task)
        ;
        (Task=null, check_beat_completion) ).

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
my_beat_idle_task(Task) :-
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

%%%
%%% Beat selection
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
   log(beat:Beat),
   assert($global_root/beats/current:Beat),
   set_beat_state(Beat, started).

%% beat_state(?Beat, ?State)
%  Beat is in the specified State.
beat_state(Beat, State) :-
   $global_root/beats/Beat/state:State.
set_beat_state(Beat, State) :-
   assert($global_root/beats/Beat/state:State).

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
   forall(beat_precondition(Beat, P),
	  P).

%% beat_score(+Beat, -Score)
%  Beat has the specified score.
beat_score(Beat, Score) :-
   beat_priority(Beat, Score) -> true ; (Score = 0).

start_beat(Beat) :-
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
   log(Beat:completed),
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

on_event(assertion(Speaker, $me, LF, Tense, Aspect),
	 plot_event_monitor,
	 _,
	 react_to_plot_event(learns_that($me, PlotPoint))) :-
   modalized(LF, Tense, Aspect, Modal),
   plot_relevant_assertion(Speaker, $me, Modal, PlotPoint).

react_to_plot_event(learns_that(Character, LF)) :-
   assert($global_root/plot_points/Character/LF),
   fail.
react_to_plot_event(_) :-
   maybe_interrupt_current_beat.

maybe_interrupt_current_beat :-
   begin(current_beat(Current),
	 beat_score(Current, CurrentScore),
	 best_next_beat(Winner),
	 beat_score(Winner, WinnerScore)),
   log(got:Winner:WinnerScore),
   WinnerScore > CurrentScore,
   log(interrupting:Current),
   begin(interrupt_beat(Current),
	 set_current_beat(Winner),
	 start_beat(Winner)).
maybe_interrupt_current_beat.
