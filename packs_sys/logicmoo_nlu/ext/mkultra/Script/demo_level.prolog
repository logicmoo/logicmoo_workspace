
%%%
%%% Exposition beat
%%%

beat(exposition).
beat_start_task(exposition,
		$kavi,
		goto($bruce)).
beat_dialog(exposition,
	    $bruce, $kavi,
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
%%% Bruce reacts to Kavi's speech
%%%

beat(bruce_reacts).
beat_start_task(bruce_reacts, $kavi, goto($'kitchen sink')).
beat_monolog(bruce_reacts,
	     $bruce,
	     [ sleep(3),
	       "I'm sure Kavi stole my macguffin.",
	       "It must be here someplace.",
	       "Kavi's a member of the illuminati.",
	       "He'd kill me if he knew I knew.",
	       "Or that I'm an agent of WWW.",
	       "I need to search the house." ]).

%%%
%%% Bruce explores the house
%%%

beat(bruce_explores_the_house).
beat_completion_condition(bruce_explores_the_house,
			  ( $bruce::contained_in($macguffin, $bruce),
			    $bruce::contained_in($report, $bruce) )).
beat_idle_task(bruce_explores_the_house,
	       $bruce,
	       search_for($bruce, kavis_house, _)).

%%%
%%% Bruce finds the report
%%%

beat(bruce_finds_the_report).
beat_priority(bruce_finds_the_report, 1).
beat_precondition(bruce_finds_the_report,
		  $bruce::contained_in($report, $bruce)).
beat_monolog(bruce_finds_the_report,
	     $bruce,
	     ["What's this?",
	      "I need to find out what MKSPARSE is."]).

%%%
%%% Bruce finds the macguffin
%%%

beat(bruce_finds_the_macguffin).
beat_priority(bruce_finds_the_macguffin, 1).
beat_precondition(bruce_finds_the_macguffin,
		  $bruce::contained_in($macguffin, $bruce)).
beat_monolog(bruce_finds_the_macguffin,
	     $bruce,
	     ["Got it!",
	      "I knew he stole it."]).
