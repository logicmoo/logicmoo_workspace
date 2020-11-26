
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

% Load stuff, split corpus, run batch speech test on in-coverage data
:- regulus_batch('$REGULUS/Examples/Calendar/scripts/japanese_calendar.cfg',
		 ["NUANCE_PARSER",
		  "SPLIT_SPEECH_CORPUS .MAIN in_coverage out_of_coverage"]).

% Load stuff, run batch speech test on in-coverage data
:- regulus_batch('$REGULUS/Examples/Calendar/scripts/japanese_calendar_no_sortal_feats.cfg',
		 ["NUANCE_PARSER",
		  "LOAD_DIALOGUE",
		  "DUMP_NBEST_TRAINING_DATA_ON",
		  "BATCH_DIALOGUE_SPEECH in_coverage"]).

:- halt.


