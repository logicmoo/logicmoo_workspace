
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

% Load grammar
:- regulus_batch('$REGULUS/Examples/Toy1/scripts/toy1.cfg', 
		 ["ECHO_ON", "LOAD"]).

% Load dialogue files

:- regulus_batch('$REGULUS/Examples/Toy1/scripts/toy1_dialogue.cfg', 
		 ["LOAD_DIALOGUE"]).

% Load translation files
:- regulus_batch('$REGULUS/Examples/Toy1/scripts/toy1_slt.cfg', 
		 ["LOAD_TRANSLATE"]).
	
% Test dialogue
:- regulus_batch('$REGULUS/Examples/Toy1/scripts/toy1_dialogue.cfg', 
		 ["DIALOGUE",
		  
		  "switch on the light in the kitchen",
		  "is the light switched on",
		  "switch on the fan",
		  "switch off the light"]).

% Test translation
:- regulus_batch('$REGULUS/Examples/Toy1/scripts/toy1_slt.cfg', 
		 ["TRANSLATE",
		  
		  "switch on the light in the kitchen",
		  "is the light switched on",
		  "switch on the fan",
		  "switch off the light"]).

:- halt.


