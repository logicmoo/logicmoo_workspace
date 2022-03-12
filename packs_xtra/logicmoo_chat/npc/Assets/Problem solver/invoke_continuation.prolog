%% step_completed
%  Current task has completed its current step; run continuation.
step_completed :-
   $task/continuation:K,
   invoke_continuation(K).

%% step_completed(+TaskQud)
%  TaskQud has completed its current step; tell it to run
%  its comtinuation.
step_completed(TaskQud) :-
   within_task(TaskQud, step_completed).

%% invoke_continuation(+Task)
%  Switch to current task's continuation, which is Task.
invoke_continuation((First, Rest)) :-  !, 
  begin(assert($task/continuation:Rest), switch_to_task(First)).
invoke_continuation(K) :-  
  begin(assert($task/continuation:done), switch_to_task(K)).



%=autodoc
%% invoke_continuation( ?TaskQud, ?K) is semidet.
%
% Invoke Continuation.
%
invoke_continuation(TaskQud, K) :-
   within_task(TaskQud, invoke_continuation(K)).



%=autodoc
%% restart_or_stop_task is semidet.
%
% Restart Or Stop Task.
%
restart_or_stop_task :-
   % Poor being's flow: task achievement -> positive affect
   affective_reaction(0.2, 0.05, 0, 0),
   ($task/repeating_task ->
      begin($task/type:task:Goal,
	    ignore(retract($task/log)),
	    invoke_continuation(Goal))
      ;
      stop_task($task)).



%=autodoc
%% stop_task( ?T) is semidet.
%
% Stop Task.
%
stop_task(T) :-  
  begin(assert(T/current:exiting), maybe_save_log(T), stop_qud(T)).

%% restart_task(+TaskQud)
%  Restarts a repeating task
restart_task(TaskQud) :-  
  begin( perform_restart_retractions(TaskQud), 
    assertion(TaskQud/repeating_task, "Attempt to restart a non-repeating task"), 
    assert(TaskQud/current:restarting), 
    TaskQud/type:task:Goal, 
    invoke_continuation(TaskQud, Goal)).



%=autodoc
%% perform_restart_retractions( ?Task) is semidet.
%
% Perform Restart Retractions.
%
perform_restart_retractions(Task) :-  
  forall(retract_on_restart(Task, Assertion), ignore(retract(Assertion))).



%=autodoc
%% retract_on_restart( ?Task, ?Task) is semidet.
%
% Retract Whenever Restart.
%
retract_on_restart(Task, Task/location_bids).
retract_on_restart(Task, Task/monitor).

%%
%% Interrupts
%%

%% interrupt_step(TaskQud, +InterruptingTask)
%  Executes InterruptingTask, then returns to previous step.
interrupt_step(TaskQud, InterruptingTask) :-  
  within_task( TaskQud, 
    begin( [ TaskQud/current:C, 
             TaskQud/continuation:K, 
             assert(TaskQud/continuation:(C, K)), 
             switch_to_task(InterruptingTask)])).
