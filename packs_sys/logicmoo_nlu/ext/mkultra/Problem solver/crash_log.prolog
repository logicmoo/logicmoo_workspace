%%
%% Saving of crash logs and other logs of problem solver tasks
%%

%% save_task_logs(+Character, +TopLevelGoal)
% Tasks for Character that involve TopLevelGoal should be logged.
:- external save_task_logs/2.

%% maybe_save_log(+TaskConcern)
% Save log of TaskConcern, but only if it has been
% marked for saving with save_task_logs/3.
maybe_save_log(TaskConcern) :-
   begin(TaskConcern/type:task:Task,
	 when(save_task_logs_of_concern($me, Task),
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
   ((/logs/_Key)>>Log) ->
       generate_unsorted_overlay("Logs",
				 task_log_line(Log, Line),
				 Line)
       ;
       generate_unsorted_overlay("Logs",
				 Line=line("No logs saved so far"),
				 Line).

task_log_line(Log, line("Task:\t", Task)) :-
   Log/task:Task.
task_log_line(Log, line("UID:\t", UID)) :-
   property(Log, "Key", UID).
task_log_line(Log, line("Exit status:\t", Status)) :-
   Log/exit_status:Status.
task_log_line(Log, line(T)) :-
   Log/log/T.		     