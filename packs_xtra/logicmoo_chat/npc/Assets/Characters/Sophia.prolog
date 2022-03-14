

%%  ?Sophia:: ?Patrol_researchis semidet.
%
% ::.
%
$'Sophia'::hot_button_topic($novel_idea).
$'Sophia'::hot_button_topic($buggy_module).
$'Sophia'::hot_button_topic($bookshelf).
$'Sophia'::hot_button_topic(theclub).

% Keep user_ofs out of buggy_module
$'Sophia'/goals/maintain/buggy_module_empty.

% I know the secret location of the novel idea
$'Sophia'/perception/location/ $novel_idea : $bookshelf.

% Monitor goals quickly
$'Sophia'/parameters/poll_time:3.

% Don't admit you know where the novel idea is to anyone
% but other theclub members
$'Sophia' ::  
  pretend_truth_value(Asker, t(location, $novel_idea, Loc), T) :- 
  \+t(member_of, Asker, theclub), 
  var(Loc)->T=unknown;T=false.
$'Sophia' ::  
  pretend_truth_value(Asker, t(contained_in, $novel_idea, Loc), T) :- 
  \+t(member_of, Asker, theclub), 
  var(Loc)->T=unknown;T=false.

% Don't admit to being an theclub member to non-members
$'Sophia' ::  
  pretend_truth_value(Asker, t(member_of, $me, theclub), false) :-  
  \+t(member_of, Asker, theclub).
   
% Don't admit to knowing who's in the theclub
$'Sophia' ::  
  pretend_truth_value(Asker, t(member_of, X, theclub), unknown) :- 
   var(X),
  \+t(member_of, Asker, theclub).
   
:- public buggy_module_empty/0.
$'Sophia'::buggy_module_empty :-
   % Kluge, but I don't have time to refactor this stuff to be nice.
   $global_root/configuration/inhibit_beat_system
   ;
   \+ user_of(_User_of, $buggy_module).

% An user_of is a person who isn't an theclub member
$'Sophia'::user_of(User_of, Module) :-
  t(location, User_of, Module), 
   iz_a(User_of, person),
  \+t(member_of, User_of, theclub).

% Eat all user_ofs
$'Sophia'::personal_strategy(achieve(buggy_module_empty),
		  ingest(User_of)) :-
   user_of(User_of, $buggy_module).

:-( assert(($global::fkey_command(alt-k, "Display Sophia's status"):-generate_character_debug_overlay($'Sophia')))).
