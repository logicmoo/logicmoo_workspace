normalize_task(respond_to_dialog_act(player, $me, general_help),
	       general_help).
normalize_task(respond_to_dialog_act(player, $me, how_do_i(Question)),
	       how_do_i(Question)).


player_question(play) --> [play].
player_question(control_minds) --> [control, people, '\'', s, minds].
player_question(read_minds) --> [read, minds].
player_question(
