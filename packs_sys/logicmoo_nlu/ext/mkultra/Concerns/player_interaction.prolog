%%%
%%% Driver for conversation between player and player character
%%%

on_enter_state(start, player_interaction, C) :-
   begin_child_concern(C, conversation, Child,
		       [ Child/partner/$me,
			 