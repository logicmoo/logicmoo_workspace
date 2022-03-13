%%%
%%% Initializations for the $user character
%%%


$pc/player_character.

% Monitor goals quickly
$pc/parameters/poll_time:1.

:-( ( unless(proper_name($pc, _, _X, []), assert_proper_name($pc, ['User'], singular))  ,
      unless(t(gender, $pc, _), assert($global::t(gender, $pc, female))), 
      unless(t(given_name, $pc, _), assert($global::t(given_name, $pc, "User"))), 
      unless(t(surname, $pc, _), assert($global::t(surname, $pc, "Quatermass"))))).


$pc/goals/player_objective_monolog:["I need to get my novel_idea back.",
				 "I'm sure Sophia could remember it,",
				 "but I don't know where she storing it."].

:- assert(($global::fkey_command(alt-p, "Display $user character's status") :-
   generate_character_debug_overlay($pc))).
