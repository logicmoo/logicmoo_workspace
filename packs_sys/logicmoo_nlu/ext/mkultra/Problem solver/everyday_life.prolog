%%
%% The "everyday life" task.
%%
%% Periodically searches for stuff to do.
%%

character_initialization :-
   \+ $global_root/configuration/inhibit_concern_initialization,
   start_task($root, everyday_life, 1, T, [T/repeating_task, T/status:idle]),
   assert(everyday_life_task(T)).

character_debug_display(Character, line("Pending:\t", Task)) :-
   Character::(/goals/pending_tasks/Task).

character_debug_display(Character, line("Topics:\t", Person:Topic)) :-
   Character::(/pending_conversation_topics/Person/Topic).

%% todo(-Task/-Preamble, -Priority)
% Character wants/need to do Task with specified Priority.
% When executing this task, it should first run Preamble
% Preamble is used for housekeeping, e.g. to remove Task
% from a queue of pending tasks. 

todo(Task, Priority) :-
   personal_todo(Task, Priority).

%% personal_todo(-Task/-Preamble, -Priority)
% Like todo/2, but is specific to one character's KB.
% Character wants/need to do Task with specified Priority.
% When executing this task, it should first run Preamble
% Preamble is used for housekeeping, e.g. to remove Task
% from a queue of pending tasks. 

:- external personal_todo/2.

strategy(everyday_life,
	 work_on_everyday_life_task(Task)) :-
   arg_max(Task, Priority, todo(Task, Priority)).
default_strategy(everyday_life, yield).

default_strategy(work_on_everyday_life_task(T),
		 begin(set_status(Task),
		       % Spawn it as a subtask and wait for it.
		       % Spawning it means that if it crashes, it doesn't take the
		       % parent task with it, and that the task gets its own separate
		       % crash log for debugging.
		       Preamble,
		       cobegin(Task),
		       set_status(idle))) :-
   unpack_preamble(T, Task, Preamble).

unpack_preamble(Task/Preamble, Task, Preamble) :- !.
unpack_preamble(Task, Task, null).

%%%
%%% Control of the everyday_life task
%%%

%% everyday_life_task(-TaskConcern)
% Returns the everyday_life task of the current character.
% This means the top-level task called everyday_life, not whatever
% todo list item it happens to be running right now.

% Defined when the everyday_life concern is created. 
:- external everyday_life_task/1.

everyday_life_task_busy :-
   everyday_life_task(C),
   C/concerns/_.

%% restart_everday_life_task
% Restarts the everyday_life task
restart_everyday_life_task :-
   everyday_life_task(C),
   restart_task(C).

%% kill_current_everyday_life_task
% Stops whatever the everyday_life task is currently trying to do.
:- public kill_current_everyday_life_task/0.
kill_current_everyday_life_task :-
   everyday_life_task(T),
   kill_children(T).


%%%
%%% Pending task queue
%%%

todo(T/retract(Node), P) :-
   /goals/pending_tasks/T>>Node,
   Node:P.

add_pending_task(Task) :-
   add_pending_task(Task, 1).
add_pending_task(Task, Priority) :-
   current_priority(P),
   Multiplied is Priority*P,
   assert(/goals/pending_tasks/Task:Multiplied).

%%%
%%% Maintenance goals
%%%

todo(achieve(P), 1) :-
   unsatisfied_maintenance_goal(P),
   % Make sure that P isn't obviously unachievable.
   have_strategy(achieve(P)).

unsatisfied_maintenance_goal(P) :-
   maintenance_goal(P),
   \+ P.

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
