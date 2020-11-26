
:- compile('$REGULUS/Examples/Calendar/Prolog/toy1_app_using_server').

:- assertz(regulus_config(wavfile_directory, '$REGULUS/Examples/Calendar/japanese_wavfiles')).

:- set_debug_mode.

:- toy1_app(1975, 1985, '$REGULUS/Examples/Calendar/Generated/japanese_recogniser').

:- halt.


