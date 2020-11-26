
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

% Load stuff, split corpus, run batch speech test on in-coverage data
:- regulus_batch('$REGULUS/Examples/Calendar/scripts/calendar.cfg',
		 ["NUANCE_PARSER",
		  "EBL_LOAD",
		  "LOAD_DIALOGUE",
		  "DUMP_NBEST_TRAINING_DATA_ON",
		  "BATCH_DIALOGUE_SPEECH"]).

:- halt.


