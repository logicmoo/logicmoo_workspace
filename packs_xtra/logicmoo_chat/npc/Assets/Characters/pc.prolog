%%%
%%% Initializations for the $player character
%%%


$pc/player_character.

% Monitor goals quickly
$pc/parameters/poll_time:1.

:-( ( unless(proper_name($pc, _, _X, []), assert_proper_name($pc, ['Betsy'], singular))  ,
      unless(t(gender, $pc, _), assert($global::t(gender, $pc, female))), 
      unless(t(given_name, $pc, _), assert($global::t(given_name, $pc, "Betsy"))), 
      unless(t(surname, $pc, _), assert($global::t(surname, $pc, "Quatermass"))))).


$pc/goals/player_objective_monolog:["I need to get my macguffin back.",
				 "I'm sure Kavi stole it,",
				 "but I don't know where he hid it."].

:- assert(($global::fkey_command(alt-p, "Display $player character's status") :-
   generate_character_debug_overlay($pc))).
