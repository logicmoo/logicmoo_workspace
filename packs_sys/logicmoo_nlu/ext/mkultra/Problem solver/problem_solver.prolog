%%
%% Simple problem solver in the general tradition of NASL
%%

test_file(problem_solver(_), "Problem solver/integrity_checks").
test_file(problem_solver(_), "Problem solver/ps_tests").

%%%
%%% Interface to external code
%%% Task creation, strategy specification
%%%

:- public start_task/5, start_task/3, start_task/2.

%% start_task(+Parent, +Task, +Priority, TaskConcern, +Assertions) is det
%  Adds a task to Parent's subconcerns.  Priority is
%  The score to be given by the task to any actions it attempts
%  to perform.
start_task(Parent, Task, Priority, TaskConcern, Assertions) :-
   begin_child_concern(Parent, task, Priority, TaskConcern,
   		       [TaskConcern/type:task:Task,
			TaskConcern/current:start,
   			TaskConcern/continuation:done]),
   forall(member(A, Assertions),
   	  assert(A)).
%   within_task(TaskConcern, switch_to_task(Task)).

start_task(Parent, Task, Priority) :-
   start_task(Parent, Task, Priority, _, [ ]).
start_task(Task, Priority) :-
   start_task($root, Task, Priority, _, [ ]).

% Problem solver state is stored in:
%   TaskConcern/type:task:TopLevelTask         
%   TaskConcern/current:CurrentStep     (always an action or polled_builtin)
%   TaskConcern/continuation:Task       (any task)

:- indexical task=null.

:- external veto_strategy/1, personal_strategy/2, before/2, after/2.

%% within_task(+TaskConcern, :Code)
%  Runs Code within the task TaskConcern.
within_task(TaskConcern, Code) :-
   bind(task, TaskConcern),
   (TaskConcern/partner/P -> bind(addressee, P) ; true),
   Code.

primitive_task(T) :-
   builtin_task(T) ; action(T).
builtin_task(T) :-
   immediate_builtin(T) ; polled_builtin(T).
immediate_builtin(null).
immediate_builtin(done).
immediate_builtin(call(_)).
immediate_builtin(assert(_)).
immediate_builtin(retract(_)).
immediate_builtin(invoke_continuation(_)).
immediate_builtin((_,_)).
immediate_builtin(let(_,_)).
polled_builtin(wait_condition(_)).
polled_builtin(wait_event(_)).
polled_builtin(wait_event(_,_)).
polled_builtin(breakpoint).

%% strategy(+Task, -CandidateStrategy)
%  CandidateStrategy is a possible way to solve Task.
%  CandidateStrategy may be another task, null, or a sequence of tasks
%  constructed using ,/2.

%% personaly_strategy(+Task, -CandidateStrategy)
%  CandidateStrategy is a possible way to solve Task.
%  CandidateStrategy may be another task, null, or a sequence of tasks
%  constructed using ,/2.

%% have_strategy(+Task)
%  True when we have at least some candidate reduction for this task.
have_strategy(Task) :-
   task_reduction(Task, Reduct),
   !,
   Reduct \= resolve_match_failure(_).
   