%%
%% The "everyday life" task.
%%
%% Periodically searches for stuff to do.
%%

character_initialization :-
   \+ $global_root/configuration/inhibit_concern_initialization,
   start_task($root, everyday_life, 1, T, [T/repeating_task]).

character_debug_display(Character, line("Pending:\t", Task)) :-
   Character::(/goals/pending_tasks/Task).

character_debug_display(Character, line("Topics:\t", Person:Topic)) :-
   Character::(/pending_conversation_topics/Person/Topic).

normalize_task(work_on_everyday_life_task(T),
	       begin(call(set_concern_status($task, T)),
		     % Spawn it as a subtask and wait for it.
		     % Spawning it means that if it crashes, it doesn't take the
		     % parent task with it, and that the task gets its own separate
		     % crash log for debugging.
		     cobegin(T))).

everyday_life_task(TaskConcern) :-
   concern(TaskConcern, task),
   TaskConcern/type:task:everyday_life.

restart_everyday_life_task :-
   everyday_life_task(C),
   restart_task(C).

strategy(everyday_life,
	 begin(retract(Node), work_on_everyday_life_task(T))) :-
   /goals/pending_tasks/T>>Node.

add_pending_task(Task) :-
   assert(/goals/pending_tasks/Task).

strategy(everyday_life,
	 work_on_everyday_life_task(achieve(P)) ) :-
   unsatisfied_maintenance_goal(P),
   % Make sure that P isn't obviously unachievable.
   have_strategy(achieve(P)).

unsatisfied_maintenance_goal(P) :-
   maintenance_goal(P),
   \+ P.

strategy(everyday_life,
	 work_on_everyday_life_task(engage_in_conversation(Person))) :-
   /pending_conversation_topics/Person/_,
   \+ currently_in_conversation.

default_strategy(everyday_life,
		 if(my_beat_idle_task(Task),
		    work_on_everyday_life_task(Task),
		    begin(call(set_concern_status($task, idle)),
			  wait_event_with_timeout(_, PollTime)))) :-
   everyday_life_polling_time(PollTime).

everyday_life_polling_time(T) :-
   /parameters/poll_time:T -> true ; T = 60.

maintenance_goal(P) :-
   /goals/maintain/P.

maintenance_goal(~hungry($me)).
hungry($me) :- /physiological_states/hungry.
~hungry(X) :- \+ hungry(X).

maintenance_goal(~thirsty($me)).
thirsty($me) :- /physiological_states/thirsty.
~thirsty(X) :- \+ thirsty(X).

maintenance_goal(~tired($me)).
tired($me) :- /physiological_states/tired.
~tired(X) :- \+ tired(X).

maintenance_goal(~dirty($me)).
dirty($me) :- /physiological_states/dirty.
~dirty(X) :- \+ dirty(X).

maintenance_goal(~full_bladder($me)).
full_bladder($me) :- /physiological_states/full_bladder.
~full_bladder(X) :- \+ full_bladder(X).
