$'Kavi'::hot_button_topic($macguffin).
$'Kavi'::hot_button_topic($bedroom).
$'Kavi'::hot_button_topic($bookshelf).
$'Kavi'::hot_button_topic(theclub).

% Keep intruders out of bedroom
$'Kavi'/goals/maintain/bedroom_empty.

% I know the secret location of the macguffin
$'Kavi'/perception/location/ $macguffin : $bookshelf.

% Monitor goals quickly
$'Kavi'/parameters/poll_time:3.

% Don't admit you know where the macguffin is to anyone
% but other theclub members
$'Kavi'::pretend_truth_value(Asker,
		    location($macguffin, Loc),
		    T) :-
   \+ related(Asker, member_of, theclub),
   (var(Loc) -> T = unknown ; T = false).
$'Kavi'::pretend_truth_value(Asker,
		    contained_in($macguffin, Loc),
		    T) :-
   \+ related(Asker, member_of, theclub),
   (var(Loc) -> T = unknown ; T = false).

% Don't admit to being an theclub member to non-members
$'Kavi'::pretend_truth_value(Asker,
		    related($me, member_of, theclub),
		    false) :-
   \+ related(Asker, member_of, theclub).
   
% Don't admit to knowing who's in the theclub
$'Kavi'::pretend_truth_value(Asker,
		    related(X, member_of, theclub),
		    unknown) :-
   var(X),
   \+ related(Asker, member_of, theclub).
   
:- public bedroom_empty/0.
$'Kavi'::bedroom_empty :-
   % Kluge, but I don't have time to refactor this stuff to be nice.
   $global_root/configuration/inhibit_beat_system
   ;
   \+ intruder(_Intruder, $bedroom).

% An intruder is a person who isn't an theclub member
$'Kavi'::intruder(Intruder, Room) :-
   location(Intruder, Room),
   iz_a(Intruder, person),
   \+ related(Intruder, member_of, theclub).

% Eat all intruders
$'Kavi'::personal_strategy(achieve(bedroom_empty),
		  ingest(Intruder)) :-
   intruder(Intruder, $bedroom).

:- assert($global::fkey_command(alt-k, "Display Kavi's status") :-
   generate_character_debug_overlay($'Kavi')).
