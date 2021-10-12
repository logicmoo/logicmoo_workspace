%%%
%%% Actions and tasks
%%%

%% precondition(?Action, ?P)
%  P is a precondition of Action.

:- external precondition/2.

%% achieves(?Action, ?Effect)
%  Action can be expected to achieve Effect

:- external achieves/2.

%% primitive(?Task)
%  True if Task is a primitive task, i.e. an action.

:- external primitive/2.

%% runnable(+Action) is det
%  True if Action can be executed now.
runnable(Action) :-
   \+ blocking(Action, _).

%% blocking(+Action, ?Precondition) is det
%  Action cannot be run because Precondition is an unsatisfied precondition of P.
blocking(Action, P) :-
   precondition(Action, P),
   \+ P.
