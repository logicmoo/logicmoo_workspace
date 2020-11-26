
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

% Load stuff, split corpus, run batch speech test on in-coverage data
:- regulus_batch('$REGULUS/Examples/Calendar/scripts/calendar_slm.cfg',
		 ["EBL_LOAD",
		  "LOAD_DIALOGUE",
		  "SPLIT_SPEECH_CORPUS .MAIN in_coverage out_of_coverage",
		  "DUMP_NBEST_TRAINING_DATA_ON",
		  "BATCH_DIALOGUE_SPEECH in_coverage"]).

:- halt.


