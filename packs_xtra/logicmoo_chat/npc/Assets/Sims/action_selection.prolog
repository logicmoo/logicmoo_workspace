%%%
%%% ACTION SELECTION
%%%
%%% Main idea:
%%% - SimController calls next_action/1
%%% - Quds propose actions using propose_action/3.
%%% - Quds can score and action or one its construals through score_action/4.
%%% - Score for an action is sum of all scores by all quds of all construals
%%% - Select action with maximal score.
%%%

:- public actions/0, next_action/1, score_action/4.

%% propose_action(-Action, +Type, +Qud)
%  Qud, which is of type Type, proposes Action.
:- external propose_action/3.

%% score_action(+Action, +Type, +Qud, -Score)
%  Qud (of type Type) assigns Score to Action.
:- external score_action/4.

%% next_action(-Action) is det
%  Action is the highest rated action available, or pause if no available
%  actions.
%  Called by SimController component's Update routine.
next_action(Action) :-
   begin(run_delayed_operations,
	 poll_tasks,
	 update_halo,
	 update_character_status),
   best_action(Action).
next_action(pause(1)).

best_action(Action) :-
   ignore(retract(/action_state/candidates)),
   arg_max(Action,
	   Score,
	   (  generate_unique(Action, available_action(Action)),
	      runnable(Action),
	      action_score(Action, Score),
	      assert(/action_state/candidates/Action:Score) )).

available_action(Action) :-
   qud(Qud, Type),
   propose_action(Action, Type, Qud).

action_score(Action, TotalScore) :-
   sumall(Score,
	  ( generate_unique(Construal, construal(Action, Construal)),
	    qud(Qud, Type),
	    score_action(Action, Type, Qud, Score) ),
	  TotalScore).

% Debugging tool
% Prints proposed actions and their scores
actions :-
   findall(S-A,
	   ( available_action(A), 
	     action_score(A, S) ),
	   Unsorted),
   keysort(Unsorted, Sorted),
   reverse(Sorted, Reversed),
   forall(member(Score-Action, Reversed),
	  begin(write(Action),
		write("\t"),
		writeln(Score))).
