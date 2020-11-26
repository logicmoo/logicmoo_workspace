
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

% Load grammar
:- regulus_batch('$REGULUS/Examples/Toy1/scripts/toy1_dialogue_batch.cfg', 
		 ["ECHO_ON", "LOAD",

% Load dialogue files

		  "LOAD_DIALOGUE",

% Test dialogue on speech files

		  "BATCH_DIALOGUE_SPEECH"]).

:- halt.


