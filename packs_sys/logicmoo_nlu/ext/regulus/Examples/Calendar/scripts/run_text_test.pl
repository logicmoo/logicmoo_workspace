
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

% Load stuff, split corpus, run batch text test
:- regulus_batch('$REGULUS/Examples/Calendar/scripts/calendar.cfg',
		 [%"NUANCE_PARSER",
		  "EBL_LOAD",
		  "LOAD_DIALOGUE",
		  "BATCH_DIALOGUE"]).

:- halt.


