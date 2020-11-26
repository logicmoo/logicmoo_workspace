
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

% Compile Regulus grammar to Nuance
:- regulus_batch('$REGULUS/Examples/Calendar/scripts/calendar.cfg',
		 ["LOAD", "EBL_TREEBANK", "EBL_TRAIN", "EBL_POSTPROCESS", "EBL_NUANCE", "EBL_GRAMMAR_PROBS"]).

:- halt.


