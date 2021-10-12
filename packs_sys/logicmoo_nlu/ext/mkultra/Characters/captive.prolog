% Monitor goals quickly
/parameters/poll_time:3.

$global::fkey_command(alt-z, "Display captive's status") :-
   generate_character_debug_overlay($captive).

personally_affective_event(enter_social_space($pc), 1, 0.5, 0, 0).
personally_affective_event(exit_social_space($pc), 0, 0, 1, 0.5).