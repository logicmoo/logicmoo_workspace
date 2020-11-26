
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

:- regulus_batch('$REGULUS/Examples/Calendar/scripts/calendar.cfg',
		 ["UPDATE_DIALOGUE_JUDGEMENTS"]).

:- halt.


