
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

% Load stuff, split corpus, run batch speech test on in-coverage PDA data
:- regulus_batch('$REGULUS/Examples/Calendar/scripts/calendar.cfg',
		 ["EBL_LOAD",
		  "LOAD_DIALOGUE",
		  "SPLIT_SPEECH_CORPUS .MAIN pda in_coverage_pda out_of_coverage_pda",
		  "BATCH_DIALOGUE_SPEECH in_coverage_pda"]).

:- halt.


