
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

% Load stuff, split corpus, run batch text test
:- regulus_batch('$REGULUS/Examples/Calendar/scripts/calendar.cfg',
		 ["EBL_LOAD",
		  "COMPILE_HELP"]).

:- halt.


