% Monitor goals quickly

$bina48/parameters/poll_time:3.

:-assert(($global::fkey_command(alt-z, "Display bina48's status") :-
   generate_character_debug_overlay($bina48))).



%=autodoc
%%  ?Sophia:: ?Patrol_researchis semidet.
%
% ::.
%
$bina48::personally_affective_event(enter_social_space($pc), 1, 0.5, 0, 0).
$bina48::personally_affective_event(exit_social_space($pc), 0, 0, 1, 0.5).
