%%
%% Simple problem solver in the general tradition of NASL
%%

% The problem solver can be instantiated inside an concern by adding a task
% to Concern/tasks/Task.
%
% Problem solver state is stored in:
%   Concern/task/Task1/current:Task2              The step its working on
%   Concern/task/Task/continuation:Task           What to do after current

:- indexical task=null.

%%
%% Interface to external code
%% Task creation
%%

:- public start_task/2.

%% start_task(+Concern, +Task, +Priority) is det
%  Adds a task to Concern's list of running tasks.  Priority is
%  The score to be given by the task to any actions it attempts
%  to perform.
start_task(C, T, Priority) :-
   assert(C/task/T),
   assert(C/task/T/priority:Priority).

%%
%% General strategies
%%

strategy(achieve(runnable(Action)), Strategy) :-
   % If there's an unsatisfied precondition, try to satisfy it.
   % Otherwise, it's already runnable
   blocking(Action, Blocker) ->
      (Strategy = achieve(Blocker))
      ; (Strategy = null).

strategy(achieve(P), wait_condition(P)) :-
   self_achieving(P).

self_achieving(/perception/nobody_speaking).

%%
%% Task update
%%

%% task(-Task)
%  Task is a task of some concern.
task(T) :-
   concern(C),
   C/task/T.

%% tick_tasks
%  Ticks all tasks of all concerns.
tick_tasks :-
   forall(task(T),
	  begin(tick_task(T))).

%% tick_task(+Task)
%  Attempts to make forward progress on Task's current step.
tick_task(T) :-
   T/current:A:action,
   % It's an action; we have to wait for it to be executed.
   !,
   (runnable(A) ; interrupt_step(achieve(runnable(A)))).

tick_task(T) :-
   % This only runs if Current isn't an action.
   bind(task,T),
   T/current:Current,
   dispatch(Current).

%% strategy(+Step, -NewStep)
%  NewStep is a way of trying to achieve Step.
:- external strategy/2.

%% dispatch(+Current)
%  Attempts to make progress on compound task Current, which should be the current step
%  of $task.
dispatch(wait_condition(Condition)) :-
   !,
   (Condition -> step_completed ; true).
dispatch(Current) :-
   all(S,
       strategy(Current, S),
       Strategies),
   dispatch_from_strategy_list(Current, Strategies).

%% dispatch_from_strategy_list(+Step, StrategyList)
%  If StrategyList is a singleton, it runs it, else subgoals
%  to a metastrategy.
dispatch_from_strategy_list(_, [S]) :-
   !,
   apply_strategy(S).

dispatch_from_strategy_list(Current, []) :-
   !,
   dispatch(no_matches(Current)).

dispatch_from_strategy_list(Current, Strategies) :-
   dispatch(multiple_matches(Current, Strategies)).

%% apply_strategy(+Strategy)
%  Makes Strategy the current step.
apply_strategy(wait_condition(Condition)) :-
   continue(wait_condition(Condition)).

apply_strategy((First, Rest)) :-
   !,
   $task/continuation:K,
   continue(First, (K, Rest) ).

apply_strategy(null) :-
   step_completed.

apply_strategy(S) :-
   action(S) -> assert($task/current:S:action)
                ;
		assert($task/current:S).   

%%
%% Interrupts
%%

%% interrupt_step(+InterruptingTask)
%  Executes InterruptingTask, then returns to previous step.
interrupt_step(InterruptingTask) :-
   $task/current:C,
   $task/continuation:K,
   continue(InterruptingTask, (C, K)).

%%
%% Continuation invocation
%%

step_completed(T) :-
   bind(task, T),
   step_completed.

step_completed :-
   $task/continuation:K,
   continue(K).

continue(null) :-
   !,
   retract($task).

continue((First, Rest) ) :-
   !,
   continue(First, Rest).

continue(FinalTask) :-
   continue(FinalTask, null).

continue(Current, Continuation) :-
   ( action(Current) -> assert($task/current:Current:action)
                        ; assert($task/current:Current) ),
   assert($task/continuation:Continuation).

%%
%%  Interface to mundane action selection
%%

propose_action(A, _, C) :-
   C/tasks/_/current:A:action.

score_action(A, _, C, Score) :-
   C/tasks/T/current:A:action,
   C/tasks/T/priority:Score.

on_event(A, _, C, step_completed(T)) :-
   C/tasks/T/current:A:action.
