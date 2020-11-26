
% Compile the main Regulus code
:- compile('$REGULUS/Prolog/load').

:- regulus_batch('$REGULUS/Examples/Calendar/scripts/japanese_calendar.cfg',
		 ["UPDATE_DIALOGUE_JUDGEMENTS sep2008_speech_no_datastructures"]).

:- halt.


