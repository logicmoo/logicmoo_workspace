
:- compile('$REGULUS/Prolog/dialogue_server').

:- use_module('$REGULUS/Examples/Calendar/Prolog/database1').

:- server(1985, '$REGULUS/Examples/Calendar/scripts/calendar.cfg').

:- halt.

