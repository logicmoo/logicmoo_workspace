:- module(mendeleev_npc, []).
/** <module> Behavior tree definition for Mendeleev scenario
 *
 * */

:- behavior_tree(npc).

npc is   % define a label
  [dead, % priority execution is []
   vacuum,
   light
  ].

vacuum is [
	   vacuum_exposure,
	   vacuum_no_air,
	   vacuum_consume_air
       ].

dead is
   body(dead) >> 30 turns =>  % continuous
     [[   % actions in parallel
       ! godlike_drop_all,
       ! godlike_remove_all,
       ! delete_me
	       ]] ;  % set but not at continuous
     ! lay_quietly
   .

%+ vacuum exposure without protection
vacuum_exposure is [
  env(vacuum) =>
  \+ wearing(space_suit) =>
  blood_boil,
  env(vacuum) =>
  wearing(space_suit) =>
  setting(visor, up) =>
  blood_boil].

blood_boil is
  [
      body(blood_boiling) >> 3 frames =>
      [[
      ! char_say('blackness mercifully frees you from the agony.. you are dead'),
      die
	   ]] ;
      [[
          ! char_say('Your blood is boiling from your eyes and mouth, you are rapidly losing consciousness'),
	  setonce body(blood_boiling)
      ]]
  ].

% what if you have no air tank?
% handle in hard coding, current_air_tank will be zero
vacuum_no_air is
% you can die in a suit without being in vacuum
   wearing(space_suit) =>
   setting(visor, down) =>
   amount(current_air_tank) == 0 >> 3 turns =>
      die ;
      continue(char_say('YOU CAN\'T BREATH!')).  % same as fail, but implies 'do more stuff'

% abstracted because we might want to do lots of stuff here
die is
   set body(dead).

vacuum_consume_air is
   wearing(space_suit) =>
   setting(visor, down) =>
   amount(current_air_tank) > 0 =>
   continue(decrease(current_air_tank, 1)).

% dark handling needs to be in the environment
% still not happy with this
light is
    env(dark) =>
    char_say('It is dark, you cannot see').


:- end_behavior_tree.
