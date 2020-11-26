
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

% Load stuff, split corpus, run batch speech test on in-coverage data
:- regulus_batch('$REGULUS/Examples/Calendar/scripts/calendar.cfg',
		 ["EBL_LOAD",
		  "LOAD_DIALOGUE",
		  "BATCH_DIALOGUE_SPEECH_AGAIN in_coverage_test"]).

:- halt.


