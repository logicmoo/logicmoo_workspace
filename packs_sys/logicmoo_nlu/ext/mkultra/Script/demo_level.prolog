%%%
%%% Exposition beat
%%%

beat(exposition).
beat_start_task(exposition,
		$kavi,
		goto($pc)).
beat_dialog(exposition,
	    $pc, $kavi,
	    [ mention_macguffin,
	      mention_keepout ]).

$kavi::quip(mention_macguffin,
	    ["Sorry to hear your macguffin was stolen.",
	     "Make yourself at home."]).
$kavi::quip(mention_keepout,
	    ["By the way,",
	     "Stay out of my bedroom",
	     "It's a personal thing."]).

%%%
%%% Pc reacts to Kavi's speech
%%%

beat(pc_reacts).
beat_sequel(pc_reacts, exposition).
beat_start_task(pc_reacts, $kavi, goto($'kitchen sink')).
beat_monolog(pc_reacts,
	     $pc,
	     [ sleep(3),
	       "I'm sure Kavi stole my macguffin.",
	       "It must be here someplace.",
	       "He's a member of the illuminati.",
	       "I need to search the house." ]).

%%%
%%% Pc explores the house
%%%

beat(pc_explores_the_house).
beat_delay(pc_explores_the_house, 5).
beat_follows(pc_explores_the_house, pc_reacts).
beat_completion_condition(pc_explores_the_house,
			  ( $pc::contained_in($macguffin, $pc),
			    $pc::contained_in($report, $pc) )).
beat_idle_task(pc_explores_the_house,
	       $pc,
	       search_object(kavis_house,
			     X^previously_hidden(X),
			     Y^pickup(Y),
			     mental_monologue(["Nothing seems to be hidden."]))).

after(pickup($report),
      describe($report)).

%%%
%%% Pc finds the report
%%%

beat(pc_finds_the_report).
beat_priority(pc_finds_the_report, 1).
beat_precondition(pc_finds_the_report,
		  $pc::contained_in($report, $pc)).
beat_monolog(pc_finds_the_report,
	     $pc,
	     ["What's this?",
	      "It's a report on project MKSPARSE.",
	      "I've never heard of it."]).

%%%
%%% Pc finds the macguffin
%%%

beat(pc_finds_the_macguffin).
beat_priority(pc_finds_the_macguffin, 1).
beat_precondition(pc_finds_the_macguffin,
		  $pc::contained_in($macguffin, $pc)).
beat_monolog(pc_finds_the_macguffin,
	     $pc,
	     ["Got it!",
	      "I knew he stole it."]).

%%%
%%% Kavi eats Pc
%%%

beat(kavi_eats_pc).
beat_priority(kavi_eats_pc, 2).
beat_precondition(kavi_eats_pc,
		  $global_root/plot_points/ate/ $kavi/ $pc).
beat_monolog(kavi_eats_pc,
	     $kavi,
	     ["Sorry, old chap,",
	      "but I can't let you investigate my house.",
	      "I know it's horribly rude to eat you,",
	      "But you see, I don't have a gun.",
	      "So there's really no alternative."]).