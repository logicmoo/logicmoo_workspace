%%
%% Conflict strategies
%% Preference relations and random selection.
%%

default_strategy(resolve_conflict(T, L), S) :-
   % Pick preferred strategy if there's a preference relation defined for this task.
   clause(prefer_strategy(T, _, _),_) ->
	S = pick_preferred(T, L)
	;
	S = pick_randomly(L).

%%
%% Null strategies
%%

default_strategy(resolve_match_failure(T), restart(T)) :-
   \+ ($task/failed/PastFailure, PastFailure=T) .

strategy(restart(T),
	 ( assert($task/failed/T),
	   invoke_continuation(Goal) )) :-
   $task/type:task:Goal.

strategy(pick_randomly(List), X) :-
   % Pick randomly; need once/1, or it just regenerates the whole list.
   once(random_member(X, List)).

strategy(pick_preferred(Task, List), Preferred) :-
   preferred_strategy(Task, List, Preferred).

:- external prefer_strategy/3.

preferred_strategy(Task, [First | Rest], Preferred) :-
   max_preference(Task, First, Rest, Preferred).

max_preference(_Task, Default, [], Default).
max_preference(Task, Default, [First | Rest], Max) :-
   prefer_strategy(Task, First, Default),
   !,
   max_preference(Task, First, Rest, Max).
max_preference(Task, Default, [_ | Rest], Max) :-
   max_preference(Task, Default, Rest, Max).

%% prefer_strategy(+Task, +PreferredStrategy, +DispreferredStrategy)
%  PreferredStrategy is better for solving Task than DispreferredStrategy.
