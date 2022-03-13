%%%
%%% Driver for conversation between $user and $user character
%%%

on_event(player_input(DialogAct),
	 player_interaction,
	 C,
	 Response) :-
   once(player_input_response(DialogAct, C, Response)).




%=autodoc
%% player_input_response( ?DialogAct, ?C, ?C) is semidet.
%
% User Input Response.
%
player_input_response(DialogAct, C, player_input_task(C, respond_to_dialog_act(Normalized))) :-
   normalize_dialog_act(DialogAct, Normalized),
   Normalized \= automa_command(_, $pc, _, _, _),
   agent(Normalized, $user).
player_input_response(DA, C,
		      player_input_task(C, say_string(Feedback))) :-
   normalize_dialog_act(DA, Normalized),
   reject_player_dialog_act(Normalized, Feedback).



%=autodoc
%% reject_player_dialog_act( ?X, ?STRING2) is semidet.
%
% Reject User Dialog Single Doer Action.
%
reject_player_dialog_act(automa_command($pc, X, wants(X, _), _, _),
			 "I wish I could just tell people to want things, but I can't.").
reject_player_dialog_act(automa_command(_, _, P, _, _),
			 "Sorry.  That's just too meta for my little head.") :-
   recursive_modal(P).
reject_player_dialog_act(automa_command(_, $pc, _, _, _),
			 "I wish I could automatize myself, but I can't.").
reject_player_dialog_act(command($pc, Partner, Command),
			 "You're in conversation with them; just say it directly.") :-
   in_conversation_with(Partner),
   misdirected_ask_or_tell_command(Partner, Command).



%=autodoc
%% misdirected_ask_or_tell_command( ?ARG1, ?Partner) is semidet.
%
% Misdirected Complete Inference Or Canonicalize And Store Command.
%
misdirected_ask_or_tell_command(Partner, ask_about(Partner, Partner, _)).
misdirected_ask_or_tell_command(Partner, ask_value(Partner, Partner, _)).
misdirected_ask_or_tell_command(Partner, tell_about(Partner, Partner, _)).



%=autodoc
%% modal_payload( ?UPARAM1, ?P) is semidet.
%
% Modal Payload.
%
modal_payload(wants(_, P), P).
modal_payload(needs(_, P), P).
modal_payload(likes(_, P), P).
modal_payload(believes(_, P), P).
modal_payload(know(_, P), P).
modal_payload(thinks(_, P), P).



%=autodoc
%% recursive_modal( ?P) is semidet.
%
% Recursive Modal.
%
recursive_modal(P) :-
   modal_payload(P, Q),
   modal_payload(Q, _).

player_input_response(X, C, assert(C/propose_action:X)).

:- multifile(da_normal_form/2).


%=autodoc
%% da_normal_form( ?Pc, ?$pc) is semidet.
%
% Da Normal Form.
%
da_normal_form(assertion($pc, NPC, know(NPC, Proposition), present, simple),
	       automa_command($pc, NPC, Proposition, present, simple)).
da_normal_form(assertion($pc, NPC, believes(NPC, Proposition), present, simple),
	       automa_command($pc, NPC, Proposition, present, simple)).
da_normal_form(assertion($pc, NPC, thinks(NPC, Proposition), present, simple),
	       automa_command($pc, NPC, Proposition, present, simple)).
da_normal_form(command($pc, NPC, know(NPC, Proposition)),
	       automa_command($pc, NPC, Proposition, present, simple)).
da_normal_form(command($pc, NPC, believes(NPC, Proposition)),
	       automa_command($pc, NPC, Proposition, present, simple)).
da_normal_form(command($pc, NPC, thinks(NPC, Proposition)),
	       automa_command($pc, NPC, Proposition, present, simple)).

on_event(DialogAct, player_interaction, C, retract(C/propose_action)) :- 
  C/propose_action:A, 
  A=DialogAct.

propose_action(A, player_interaction, C) :-
   C/propose_action:A.

:- public player_input_task/2.



%=autodoc
%% player_input_task( ?Qud, +Input) is semidet.
%
% User Input Task.
%
player_input_task(Qud, Input) :-
   stop_current_everyday_life_task,
   stop_children(Qud),
   start_task(Qud, Input, 100, T, [T/partner/ $user]),
   restart_everyday_life_task.

%%
%% Question answering KB
%%
	 
:- public manner/2, be/2, okay/1, can/1, type/2.



%=autodoc
%% okay( ?ARG1) is semidet.
%
% Okay.
%
okay($pc).


%=autodoc
%% be( ?User1, ?ARG2) is semidet.
%
% Be.
%
be($user, $pc).



%=autodoc
%% declare_kind( ?ARG1, ?Research_program2) is semidet.
%
% Declare Kind.
%
declare_kind($user, actor).
