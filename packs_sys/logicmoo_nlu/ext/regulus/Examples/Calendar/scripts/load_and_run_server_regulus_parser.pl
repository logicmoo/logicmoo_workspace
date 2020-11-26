
:- compile('$REGULUS/Prolog/dialogue_server').

:- use_module('$REGULUS/Examples/Calendar/Prolog/database1').

%:- server(1985, '$REGULUS/Examples/Calendar/scripts/calendar.cfg').
:- server(1985,
	  '$REGULUS/Examples/Calendar/scripts/calendar.cfg',
	  ["EBL_LOAD", "LOAD_DIALOGUE", "LOAD_HELP"],
	  '$REGULUS/logfiles').

:- halt.

