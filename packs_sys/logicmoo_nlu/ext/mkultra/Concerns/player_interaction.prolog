%%%
%%% Driver for conversation between player and player character
%%%

on_event(player_input(X),
	 player_interaction,
	 C,
	 Response) :-
   normalize_dialog_act(X, Normalized),
   (agent(X,player) ->
      % This is the player talking to the PC.
      Response = player_input_task(C, respond_to_dialog_act(Normalized))
      ;
      % This is the player proposing a PC action.
      Response = assert(C/propose_action:X)).

on_event(DialogAct,
	 player_interaction,
	 C,
	 retract(C/propose_action)) :-
   C/propose_action:A,
   A=DialogAct.

propose_action(A, player_interaction, C) :-
   C/propose_action:A.

:- public player_input_task/2.

player_input_task(Concern, Input) :-
   kill_children(Concern),
   start_task(Concern, Input, 100, T, [T/partner/player]),
   restart_everyday_life_task.

%%
%% Question answering KB
%%
	 
:- public manner/2, be/2, okay/1, can/1, type/2.

okay($pc).
be(player, $pc).

declare_kind(player, actor).
