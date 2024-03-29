%%%
%%% Initializations for the player character
%%%

$pc/player_character.

% Monitor goals quickly
$pc/parameters/poll_time:1.

:- ((unless(proper_name($pc, _, _X, []),
	  assert_proper_name($pc, ['Betsy'], singular)),
   unless(declare_value($pc, gender, _),
	  assert($global::declare_value($pc, gender, female))),
   unless(declare_value($pc, given_name, _),
	  assert($global::declare_value($pc, given_name, "Betsy"))),
   unless(declare_value($pc, surname, _),
	  assert($global::declare_value($pc, surname, "Quatermass"))))).


$pc/goals/player_objective_monolog:["I need to get my macguffin back.",
				 "I'm sure Kavi stole it,",
				 "but I don't know where he hid it."].

:- assert(($global::fkey_command(alt-p, "Display player character's status") :-
   generate_character_debug_overlay($pc))).
