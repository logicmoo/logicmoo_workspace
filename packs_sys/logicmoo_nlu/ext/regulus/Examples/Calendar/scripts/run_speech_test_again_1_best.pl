
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

% Load stuff, split corpus, run batch speech test on in-coverage data
:- regulus_batch('$REGULUS/Examples/Calendar/scripts/calendar.cfg',
		 ["EBL_LOAD",
		  "LOAD_DIALOGUE",
		  "SET_NBEST_N 1",
		  "DUMP_NBEST_TRAINING_DATA_ON",
		  "BATCH_DIALOGUE_SPEECH_AGAIN in_coverage"]).

:- halt.


