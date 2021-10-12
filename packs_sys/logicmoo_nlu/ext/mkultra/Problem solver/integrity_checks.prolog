%% known_task(+Task)
%  Task has a declared reduction.
known_task(Task) :-
   polled_builtin(Task),
   !.
known_task(Task) :-
   once(clause(strategy(Task, _), _)).