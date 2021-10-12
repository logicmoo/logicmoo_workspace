%%
%% Driver code - called from action_selection.prolog
%%

%% poll_tasks
%  Polls all tasks of all concerns.
poll_tasks :-
   forall(concern(Task, task),
	  poll_task(Task)).

%% poll_task(+Task)
%  Attempts to make forward progress on Task's current step.
poll_task(T) :-
   (T/current:A)>>ActionNode,
   ((ActionNode:action) ->
      poll_action(T, A)
      ;
      poll_builtin(T, A)).

poll_action(T, A) :-
   % Make sure it's still runnable
   runnable(A) ; interrupt_step(T, achieve(runnable(A))).

poll_builtin(T, wait_condition(Condition)) :-
   !,
   (Condition -> step_completed(T) ; true).
poll_builtin(_, wait_event(_)).   % nothing to do.
poll_builtin(T, wait_event(_, Timeout)) :-
   ($now > Timeout) ->
      step_completed(T) ; true.

%%
%%  Interface to mundane action selection
%%

propose_action(A, task, T) :-
   T/current:A:action.

score_action(A, task, T, Score) :-
   T/current:X:action,
   A=X,
   T/priority:Score.

on_event(E, task, T, step_completed(T)) :-
   T/current:X,
   (X=E ; X=wait_event(E) ; X=wait_event(E,_)).
