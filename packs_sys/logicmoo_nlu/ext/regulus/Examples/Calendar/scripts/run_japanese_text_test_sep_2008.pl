
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

% Load stuff, split corpus, run batch text test
:- regulus_batch('$REGULUS/Examples/Calendar/scripts/japanese_calendar.cfg',
		 ["LOAD",
		  "LOAD_DIALOGUE",
		  "BATCH_DIALOGUE sep2008_speech"]).

:- halt.


