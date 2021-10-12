display_task_debugger :-
   pause_game,
   generate_unsorted_overlay("Breakpoint",
			     debugger_line(Line),
			     Line).
debugger_line(line("Who:\t", $me)).
debugger_line(line("Task:\t", Task)) :-
   $task/type:task:Task.
debugger_line(line("Current:\t", Task)) :-
   $task/current:Task.
debugger_line(line("Next:\t", Task)) :-
   $task/continuation:Task.
debugger_line(line("")).
debugger_line(line(Step)) :-
   $task/log/Step.

fkey_command(alt-c, "Continue from problem solver breakpoint.") :-
   unpause_game,
   character(C),
   C::continue_from_breakpoint.

continue_from_breakpoint :-
   concern(C, task),
   C/current:breakpoint,
   % Can't call step completed directly because this is called by the UI code,
   % Note from within the character itself.
   % So we kluge it by rewritting the current step to be a wait that will continue
   % immediately.
   assert(C/current:wait_condition(true)),
   hide_overlay.