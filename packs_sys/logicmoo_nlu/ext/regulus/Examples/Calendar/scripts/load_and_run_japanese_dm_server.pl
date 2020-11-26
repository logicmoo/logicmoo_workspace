
:- compile('$REGULUS/Prolog/dialogue_server').

:- use_module('$REGULUS/Examples/Calendar/Prolog/japanese_database').

:- server(1985, '$REGULUS/Examples/Calendar/scripts/japanese_calendar.cfg').

:- halt.

