
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

% Compile generation grammar
:- regulus_batch('$REGULUS/Examples/Calendar/scripts/paraphrase_recogniser.cfg',
		 ["NUANCE",
		  %"NUANCE_COMPILE_WITH_PCFG"
		  "NUANCE_COMPILE"
		  ]).

:- halt.

