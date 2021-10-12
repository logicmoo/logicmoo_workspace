display_task_debugger :-
   pause_game,
   generate_unsorted_overlay("Task debugger",
			     debugger_line(Line),
			     Line).