%%%
%%% Driver for conversation between player and player character
%%%

on_event(player_input(DialogAct),
	 player_interaction,
	 C,
	 Response) :-
   once(player_input_response(DialogAct, C, Response)).


player_input_response(DialogAct, C, player_input_task(C, respond_to_dialog_act(Normalized))) :-
   normalize_dialog_act(DialogAct, Normalized),
   Normalized \= hypno_command(_, $pc, _, _, _),
   agent(Normalized, player).
player_input_response(DA, C,
		      player_input_task(C, say_string(Feedback))) :-
   normalize_dialog_act(DA, Normalized),
   reject_player_dialog_act(Normalized, Feedback).

reject_player_dialog_act(hypno_command($pc, X, wants(X, _), _, _),
			 "I wish I could just tell people to want things, but I can't.").
reject_player_dialog_act(hypno_command(_, _, P, _, _),
			 "Sorry.  That's just too meta for my little head.") :-
   recursive_modal(P).
reject_player_dialog_act(hypno_command(_, $pc, _, _, _),
			 "I wish I could hypnotize myself, but I can't.").
reject_player_dialog_act(command($pc, Partner, Command),
			 "You're in conversation with them; just say it directly.") :-
   in_conversation_with(Partner),
   misdirected_ask_or_tell_command(Partner, Command).

misdirected_ask_or_tell_command(Partner, ask_about(Partner, Partner, _)).
misdirected_ask_or_tell_command(Partner, ask_value(Partner, Partner, _)).
misdirected_ask_or_tell_command(Partner, tell_about(Partner, Partner, _)).

modal_payload(wants(_, P), P).
modal_payload(needs(_, P), P).
modal_payload(likes(_, P), P).
modal_payload(believes(_, P), P).
modal_payload(knows(_, P), P).
modal_payload(thinks(_, P), P).

recursive_modal(P) :-
   modal_payload(P, Q),
   modal_payload(Q, _).

player_input_response(X, C, assert(C/propose_action:X)).

da_normal_form(assertion($pc, NPC, knows(NPC, Proposition), present, simple),
	       hypno_command($pc, NPC, Proposition, present, simple)).
da_normal_form(assertion($pc, NPC, believes(NPC, Proposition), present, simple),
	       hypno_command($pc, NPC, Proposition, present, simple)).
da_normal_form(assertion($pc, NPC, thinks(NPC, Proposition), present, simple),
	       hypno_command($pc, NPC, Proposition, present, simple)).
da_normal_form(command($pc, NPC, knows(NPC, Proposition)),
	       hypno_command($pc, NPC, Proposition, present, simple)).
da_normal_form(command($pc, NPC, believes(NPC, Proposition)),
	       hypno_command($pc, NPC, Proposition, present, simple)).
da_normal_form(command($pc, NPC, thinks(NPC, Proposition)),
	       hypno_command($pc, NPC, Proposition, present, simple)).

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
   kill_current_everyday_life_task,
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
