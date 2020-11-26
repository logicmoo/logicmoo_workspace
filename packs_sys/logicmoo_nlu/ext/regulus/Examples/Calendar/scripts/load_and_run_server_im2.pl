
:- compile('$REGULUS/Examples/Calendar/Prolog/dialogue_server').

:- use_module('$REGULUS/Examples/Calendar/Prolog/database_im2').

:- server(1985, '$REGULUS/Examples/Calendar/scripts/calendar_im2.cfg').

:- halt.

