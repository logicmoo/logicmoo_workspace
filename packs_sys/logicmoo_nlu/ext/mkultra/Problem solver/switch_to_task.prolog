:- external trace_task/2.

%% switch_to_task(+Task)
%  Stop running current step and instead run Task followed by our continuation.
%  If Task decomposes to a (,) expression, this will update both current and
%  continuation, otherwise just current.

switch_to_task(Task) :-
   % This clause just logs Task and fails over to the next clause.
   maybe_log_task(Task),
   emit_grain("task", 10),
   trace_task($me, Task),
   ($task/current:CurrentStep ->
      ($task/continuation:K ->
           log($me:(CurrentStep -> (Task, K)))
           ;
           log($me:(CurrentStep -> Task)))
      ;
      log($me:(null->Task))),
   fail.
% Check for immediate builtins
switch_to_task(done) :-
   !,
   restart_or_kill_task.
switch_to_task(null) :-
   !,
   step_completed.
switch_to_task(call(PrologCode)) :-
   begin(PrologCode,
	 step_completed).
switch_to_task(assert(Fact)) :-
   begin(assert(Fact),
	 step_completed).
switch_to_task(retract(Fact)) :-
   begin(retract(Fact),
	 step_completed).
switch_to_task(invoke_continuation(K)) :-
   !,
   invoke_continuation(K).

% Non-immediates that can be taken care of now.
switch_to_task(wait_condition(Condition)) :-
   Condition,
   !,
   step_completed.
switch_to_task( (First, Rest) ) :-
   begin($task/continuation:K,
	 assert($task/continuation:(Rest,K)),
	 switch_to_task(First)).
switch_to_task(let(BindingCode, Task)) :-
   !,
   BindingCode ->
      switch_to_task(Task)
      ;
      throw(let_failed(let(BindingCode, Task))).
switch_to_task(breakpoint) :-
   !,
   assert($task/current:breakpoint),
   display_task_debugger.
% All other primitive tasks
switch_to_task(B) :-
   polled_builtin(B),
   !,
   assert($task/current:B).
switch_to_task(A) :-
   action(A),
   !,
   (blocking(A, Precondition) ->
      % Oops, can't run action yet because of blocked precondition.
      switch_to_task( (achieve(Precondition),A) )
      ;
      % It's an action and it's ready to run.
      assert($task/current:A:action)).

:- external failed_task/2.
%% We have a task we don't know what to do with.
switch_to_task(resolve_match_failure(resolve_match_failure(resolve_match_failure(FailedTask)))) :-
   !,
   fail_task("repeated match failure", FailedTask).

fail_task(Why, FailedTask) :-
   begin($task/type:task:TopLevelTask,
	 asserta($global::failed_task($me, (TopLevelTask-> FailedTask))),
	 emit_grain("task fail", 100),
	 restart_or_kill_task,
	 throw(task_failed($me, Why, (TopLevelTask->FailedTask)))).

% Compound task, so decompose it.
switch_to_task(T) :-
   begin(canonical_form_of_task(T, Task),
	 switch_to_canonical_task(Task)).

switch_to_canonical_task(Task) :-
   unsatisfied_task_precondition(Task, Precondition),
   switch_to_task((achieve_precondition(Task,
					Precondition),
		   Task)).
switch_to_canonical_task(Task) :-
   begin(task_reduction(Task, Reduced),
	 switch_to_task(Reduced)).

%%
%% Task preconditions
%%

unsatisfied_task_precondition(Task, P) :-
   precondition(Task, P),
   \+ task_precondition_satisfied(P).

task_precondition_satisfied(know(_:Condition)) :-
   !,
   ($task/on_behalf_of:Beneficiary) ->
      admitted_truth_value(Beneficiary, Condition, true)
      ;
      truth_value(Condition, true).
task_precondition_satisfied(Condition) :-
   ($task/on_behalf_of:Beneficiary) ->
      admitted_truth_value(Beneficiary, Condition, true)
      ;
      truth_value(Condition, true).
