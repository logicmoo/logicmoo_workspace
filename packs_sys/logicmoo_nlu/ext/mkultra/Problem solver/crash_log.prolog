%%
%% Saving of crash logs and other logs of problem solver tasks
%%

%% maybe_log_task(+Task)
% Adds Task to current task concern's log, provided it's "worth" logging.
% Doesn't bother to log the first level of match failure or comma expressions.
maybe_log_task( (_, _) ) :- !.
maybe_log_task(resolve_match_failure(X)) :-
   X \= resolve_match_failure(_),
   !.
maybe_log_task(Task) :-
   assert($task/log/Task).

%% save_task_logs(+Character, +TopLevelGoal)
% Tasks for Character that involve TopLevelGoal should be logged.
:- external save_task_logs/2.

%% maybe_save_log(+TaskConcern)
% Save log of TaskConcern, but only if it has been
% marked for saving with save_task_logs/3.
maybe_save_log(TaskConcern) :-
   begin(TaskConcern/type:task:Task,
	 when(save_task_logs($me, Task),
	      save_log(TaskConcern, "task completed  normally"))).

save_task_logs(_,_).

%% save_log(+TaskConcern, +ExitStatus)
% Saves the log of TaskConcern in /logs.
save_log(TaskConcern, ExitStatus) :-
   begin(TaskConcern/type:task:Task,
	 property(TaskConcern, "Key", Key),
	 assert($global_root/logs/ $me/Key/task:Task),
	 assert($global_root/logs/ $me/Key/exit_status:ExitStatus),
	 forall(TaskConcern/log/T,
		assert($global_root/logs/ $me/Key/log/T))).

fkey_command(alt-l, "Display crash log") :-
   update_crash_log_display.

update_crash_log_display :-
   begin(current_log_character(C),
	 if(( current_log_uid(UID),
	      ((/logs/C/UID)>>Log) ),
	    generate_unsorted_overlay("Logs",
				      task_log_line(C, UID, Log, Line),
				      Line),
	    generate_unsorted_overlay("Logs",
				      member(Line,[ line("Character:\t", C),
						    line("No logs saved so far") ]),
				      Line))).

task_log_line(Character, _UID, _Log, color(Color, line("Who:\t", Character))) :-
   (Character= $pc) -> Color="lime" ; Color="yellow".
task_log_line(_Character, UID, _Log, line("UID:\t", UID)).
task_log_line(_Character, _UID, Log, line("Task:\t", Task)) :-
   Log/task:Task.
task_log_line(_Character, _UID, Log, color(Color, line("Exit:\t", Status))) :-
   Log/exit_status:Status,
   ((Status="task completed  normally") -> Color="lime" ; Color="red").
task_log_line(_, _, _, line("")).
task_log_line(_Character, _UID, Log, line(T)) :-
   Log/log/T.		     

fkey_command(alt-rightarrow, "Show logs for next character") :-
   current_log_character(C),
   all(X, character(X), Characters),
   adjacent_in_list_circular(C, New, Characters),
   assert($global_root/gui_state/current_log_character:New),
   retract($global_root/gui_state/current_log_uid),
   update_crash_log_display.
fkey_command(alt-leftarrow, "Show logs for previous character") :-
   current_log_character(C),
   all(X, character(X), Characters),
   adjacent_in_list_circular(New, C, Characters),
   assert($global_root/gui_state/current_log_character:New),
   retract($global_root/gui_state/current_log_uid),
   update_crash_log_display.

current_log_character(C) :-
   $global_root/gui_state/current_log_character:C,
   !.
current_log_character(C) :-
   character(C),
   assert($global_root/gui_state/current_log_character:C).

fkey_command(alt-downarrow, "Show next log for this character") :-
   current_log_character(C),
   current_log_uid(CU),
   all(U, $global_root/logs/C/U, UIDs),
   adjacent_in_list_circular(CU, New, UIDs),
   assert($global_root/gui_state/current_log_uid:New),
   update_crash_log_display.
fkey_command(alt-uparrow, "Show previous log for this character") :-
   current_log_character(C),
   current_log_uid(CU),
   all(U, $global_root/logs/C/U, UIDs),
   adjacent_in_list_circular(New, CU, UIDs),
   assert($global_root/gui_state/current_log_uid:New),
   update_crash_log_display.

current_log_uid(U) :-
   $global_root/gui_state/current_log_uid:U,
   !.
current_log_uid(U) :-
   current_log_character(C),
   $global_root/logs/C/U,
   assert($global_root/gui_state/current_log_uid:U).

adjacent_in_list_circular(Before, After, [After | Tail]) :-
   last_element(Before, Tail).
adjacent_in_list_circular(Before, After, List) :-
   adjacent_in_list(Before, After, List).
adjacent_in_list(Before, After, [Before, After | _]).
adjacent_in_list(Before, After, [_ | Tail]) :-
   adjacent_in_list(Before, After, Tail).

last_element(X, [X]).
last_element(X, [_ | Tail]) :-
   last_element(X, Tail).
