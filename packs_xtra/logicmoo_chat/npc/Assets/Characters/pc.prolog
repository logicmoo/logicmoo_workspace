%%%
%%% Initializations for the $user character
%%%


$pc/player_character.

% Monitor goals quickly
$pc/parameters/poll_time:1.


unless_assigned(Prop,Obj,Value):- unless(t(Prop,Obj,_), assert($global::t(Prop,Obj,Value))).

:-( ( unless(proper_name($pc, _, _X, []), assert_proper_name($pc, ['Douglas'], singular))  ,
      unless_assigned(gender, $pc, male), 
      unless_assigned(given_name, $pc, "Douglas"), 
      unless(t(surname, $pc, _), assert($global::t(surname, $pc, "Quatermass"))))).


$pc/goals/player_objective_monolog:["I need to get my novel idea back.",
				 "I'm sure Sophia could remember it,",
				 "but I don't know where she storing it."].

:- assert(($global::fkey_command(alt-p, "Display $user character's status") :-
   generate_character_debug_overlay($pc))).
